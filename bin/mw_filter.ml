(* Copyright (C) 2015--2023  Petter A. Urkedal <paurkedal@gmail.com>
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
open Lwt.Syntax
open Unprime_list
open Unprime_string
open Utils

module Int_map = Map.Make (struct type t = int let compare = compare end)

let uri_conv =
  let parser s =
    try Ok (Uri.of_string s)
    with Invalid_argument msg -> Error msg
  in
  Arg.Conv.make ~docv:"URI" ~parser ~pp:Uri.pp ()

let login_conv =
  let parser s =
    (match String.cut_affix ":" s with
     | None -> Error "Expecting <user>:<pass>."
     | Some up -> Ok up)
  in
  let pp ppf (u, _) = Format.pp_print_string ppf (u ^ ":********") in
  Arg.Conv.make ~docv:"LOGIN" ~parser ~pp ()

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

let actions_conv =
  let parser s =
    let push s (am, ad) =
      (match String.cut_affix "=" s with
       | None -> invalid_arg "Missing equal sign."
       | Some ("else", a) -> am, action_of_string a
       | Some (c, a) ->
          Int_map.add (int_of_string c) (action_of_string a) am, ad)
    in
    try Ok (List.fold push (String.chop_affix "," s) default_actions)
    with Invalid_argument msg -> Error msg
  in
  let pp ppf (am, ad) =
    Int_map.iter
      (fun c a -> Format.fprintf ppf "%d=%s," c (string_of_action a)) am;
    Format.pp_print_string ppf "else=";
    Format.pp_print_string ppf (string_of_action ad)
  in
  Arg.Conv.make ~docv:"ACTIONS" ~parser ~pp ()

type filter_config = {
  fc_command : string * string array;
  fc_actions : filter_action Int_map.t * filter_action;
}

let find_filter_action ec fc =
  try Int_map.find ec (fst fc.fc_actions)
  with Not_found -> snd fc.fc_actions

let filter_text ~fc text =
  let res = ref "" in
  Lwt_process.with_process_full fc.fc_command begin fun process ->
    let writer =
      let* () = Lwt_io.write process#stdin text in
      Lwt_io.close process#stdin
    in
    let reader =
      let+ s = Lwt_io.read process#stdout in
      res := s
    in
    let logger =
      Lwt_stream.iter_s
        (Lwt_io.eprintlf "<stderr> %s")
        (Lwt_io.read_lines process#stderr)
    in
    let* () = Lwt.join [writer; reader; logger] in
    process#status >|= function
     | Unix.WEXITED ec ->
        (match find_filter_action ec fc with
         | `Commit -> Ok (`Replace !res)
         | `Skip -> Ok `Skip
         | `Fail -> Fmt.error_msg "Subprocess exited with %d" ec)
     | Unix.WSIGNALED sn | Unix.WSTOPPED sn ->
        Fmt.error_msg "Subprocess received signal %d" sn
  end

let filter_page ~fc ~page mw =
  let*? text = Mwapi_lwt.call Mwapi_parse.(parse ~page wikitext) mw in
  filter_text ~fc text >>=? function
   | `Skip ->
      Log.info (fun f -> f "Skipping due to exit code.") >|= Result.ok
   | `Replace text' when text' = text ->
      Log.info (fun f -> f "No changes to write back.") >|= Result.ok
   | `Replace _ as op ->
      let*? token = Utils.get_edit_token mw in
      Utils.call_edit
        Mwapi_edit.(edit ~token ~page ~create:`May_not ~recreate:`May_not ~op ())
        mw

let report_error = function
 | Ok () -> Lwt.return 0
 | Error err -> let+ () = Log.err (fun f -> f "%a" Mwapi.pp_error err) in 69

let main api login page_title actions cmd persist_cookies =
  let page = `Title page_title in
  (match cmd with
   | [] ->
      Lwt.return_ok ()
   | (prog :: _) ->
      let fc = {
        fc_command = prog, Array.of_list cmd;
        fc_actions = actions;
      } in
      let*? mw = Mwapi_lwt.open_api ~load_cookies:persist_cookies api in
      let*? () =
        (match login with
         | None -> Lwt.return_ok ()
         | Some (_ as name, password) -> Utils.login ~name ~password mw)
      in
      let*? () = filter_page ~fc ~page mw in
      Mwapi_lwt.close_api ~save_cookies:persist_cookies mw)
  >>= report_error |> Lwt_main.run

let () =
  setup_logging ();
  let api_t =
    Arg.(required & opt (some uri_conv) None &
         info ~docs:"CONNECTION OPTIONS"
              ~docv:"URI" ~doc:"Location of api.php" ["api"]) in
  let login_t =
    Arg.(value & opt (some login_conv) None &
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
  let main_t = Term.(const main $ api_t $ login_t $ page_title_t $
                     actions_t $ cmd_t $ persist_cookies_t) in
  let doc = "filter a wiki page though a command and write it back" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) filters a wiki page though an external command and saves \
        the result back to the same wiki page. E.g.";
    `P "$(tname) ... -P 'Main Page' -- sed -f script.sed";
    `P "would run the main page though a sed script.";
  ] in
  let cmd = Cmd.v (Cmd.info ~doc ~man "mw-filter") main_t in
  exit (Cmd.eval' cmd)
