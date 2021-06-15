(* Copyright (C) 2015--2021  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Cmdliner
open Logging
open Lwt.Infix
open Unprime_list
open Unprime_string
open Utils

module Int_map = Map.Make (struct type t = int let compare = compare end)

let uri_parser s =
  try `Ok (Uri.of_string s)
  with Invalid_argument msg -> `Error msg
let uri_printer fmtr uri = Format.pp_print_string fmtr (Uri.to_string uri)
let uri_conv = uri_parser, uri_printer

let up_parser s =
  match String.cut_affix ":" s with
  | None -> `Error "Expecting <user>:<pass>."
  | Some up -> `Ok up
let up_printer fmtr (u, _) = Format.pp_print_string fmtr (u ^ ":********")
let up_conv = up_parser, up_printer

type filter_action = [`Commit | `Skip | `Fail]

let action_of_string = function
  | "c" | "commit" -> `Commit
  | "s" | "skip" -> `Skip
  | "f" | "fail" -> `Fail
  | _ -> invalid_arg "action_of_string"

let string_of_action = function
  | `Commit -> "commit"
  | `Skip -> "skip"
  | `Fail -> "fail"

let default_actions = Int_map.singleton 0 `Commit, `Fail

let actions_parser s =
  let push s (am, ad) =
    match String.cut_affix "=" s with
    | None -> invalid_arg "Missing equal sign."
    | Some ("else", a) -> am, action_of_string a
    | Some (c, a) ->
      Int_map.add (int_of_string c) (action_of_string a) am, ad in
  try `Ok (List.fold push (String.chop_affix "," s) default_actions)
  with Invalid_argument msg -> `Error msg

let actions_printer fmtr (am, ad) =
  Int_map.iter (fun c a -> Format.fprintf fmtr "%d=%s," c (string_of_action a))
               am;
  Format.pp_print_string fmtr "else=";
  Format.pp_print_string fmtr (string_of_action ad)

let actions_conv = actions_parser, actions_printer

type filter_config = {
  fc_command : string * string array;
  fc_actions : filter_action Int_map.t * filter_action;
}

let filter_text ~fc text =
  let res = ref "" in
  Lwt_process.with_process_full fc.fc_command begin fun process ->
    let writer =
      Lwt_io.write process#stdin text >>= fun () ->
      Lwt_io.close process#stdin in
    let reader = Lwt_io.read process#stdout >|= fun s -> res := s in
    let logger =
      Lwt_stream.iter_s (Lwt_io.eprintlf "<stderr> %s")
                        (Lwt_io.read_lines process#stderr) in
    Lwt.join [writer; reader; logger] >>= fun () ->
    match%lwt process#status with
    | Unix.WEXITED ec ->
      begin match
        try Int_map.find ec (fst fc.fc_actions)
        with Not_found -> snd fc.fc_actions
      with
      | `Commit -> Lwt.return (`Replace !res)
      | `Skip -> Lwt.return `Skip
      | `Fail -> fail_f "Subprocess exited with %d" ec
      end
    | Unix.WSIGNALED sn | Unix.WSTOPPED sn ->
      fail_f "Subprocess received signal %d" sn
  end

let filter_page ~fc ~page mw =
  let%lwt text = Mwapi_lwt.call Mwapi_parse.(parse ~page wikitext) mw in
  match%lwt filter_text ~fc text with
  | `Skip ->
    Log.info (fun f -> f "Skipping due to exit code.")
  | `Replace text' when text' = text ->
    Log.info (fun f -> f "No changes to write back.")
  | `Replace _ as op ->
    let%lwt token = Utils.get_edit_token mw in
    Utils.call_edit
      Mwapi_edit.(edit ~token ~page ~create:`May_not ~recreate:`May_not ~op ())
      mw

let main api login page_title actions cmd persist_cookies =
  let page = `Title page_title in
  match cmd with
  | [] -> exit 0
  | (prog :: _) ->
    let fc = {
      fc_command = prog, Array.of_list cmd;
      fc_actions = actions;
    } in
    Lwt_main.run begin
      let%lwt mw = Mwapi_lwt.open_api ~load_cookies:persist_cookies api in
      begin match login with
      | None -> Lwt.return_unit
      | Some (_ as name, password) -> Utils.login ~name ~password mw
      end >>= fun () ->
      filter_page ~fc ~page mw >>= fun () ->
      Mwapi_lwt.close_api ~save_cookies:persist_cookies mw
    end

let () =
  setup_logging ();
  let api_t =
    Arg.(required & opt (some uri_conv) None &
         info ~docs:"CONNECTION OPTIONS"
              ~docv:"URI" ~doc:"Location of api.php" ["api"]) in
  let login_t =
    Arg.(value & opt (some up_conv) None &
         info ~docs:"CONNECTION OPTIONS"
              ~docv:"USER:PASS"
              ~doc:"Login with the given credentials." ["login"]) in
  let page_title_t =
    Arg.(required & opt (some string) None &
         info ~docs:"PAGE SELECTION OPTIONS"
              ~docv:"TITLE" ~doc:"Title of the page to edit."
              ["P"; "page-title"]) in
  let actions_t =
    Arg.(value & opt actions_conv default_actions &
         info ~docv:"N=ACTION,...,N=ACTION[,else=ACTION]"
              ~doc:"For exit code N, proceed with ACTION, which can be
                    one of: \
                    $(b,commit) to write back the modified page, \
                    $(b,skip) to silently leave the page unchanged, \
                    $(b,fail) to abort the program with an error."
              ["actions"]) in
  let cmd_t =
    Arg.(value & pos_all string [] & info ~docv:"COMMAND" []) in
  let persist_cookies_t =
    let doc = "Load and save non-session cookies." in
    Arg.(value & flag & info ~doc ["persistent-cookies"]) in
  let main_t = Term.(pure main $ api_t $ login_t $ page_title_t $
                     actions_t $ cmd_t $ persist_cookies_t) in
  let doc = "filter a wiki page though a command and write it back" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) filters a wiki page though an external command and saves \
        the result back to the same wiki page. E.g.";
    `P "$(tname) ... -P 'Main Page' -- sed -f script.sed";
    `P "would run the main page though a sed script.";
  ] in
  match Term.(eval (main_t, info ~doc ~man "mw-filter")) with
  | `Error `Parse -> exit 64
  | `Error `Term -> exit 69
  | `Error `Exn -> exit 70
  | `Ok () -> exit 0
  | `Version | `Help -> exit 0
