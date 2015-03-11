(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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
open Unprime_string
open Utils

let uri_parser s =
  try `Ok (Uri.of_string s)
  with Invalid_argument msg -> `Error msg
let uri_printer fmtr uri = Format.pp_print_string fmtr (Uri.to_string uri)
let uri_conv = uri_parser, uri_printer

let up_parser s =
  match String.cut_affix ":" s with
  | None -> `Error "Expecting <user>:<pass>."
  | Some up -> `Ok up
let up_printer fmtr (u, p) = Format.pp_print_string fmtr (u ^ ":********")
let up_conv = up_parser, up_printer

let filter_text cmd text =
  let res = ref "" in
  Lwt_process.with_process_full cmd begin fun process ->
    let writer =
      Lwt_io.write process#stdin text >>
      Lwt_io.close process#stdin in
    let reader = Lwt_io.read process#stdout >|= fun s -> res := s in
    let logger =
      Lwt_stream.iter_s (Lwt_io.eprintlf "<stderr> %s")
			(Lwt_io.read_lines process#stderr) in
    Lwt.join [writer; reader; logger]
  end >|= fun () -> !res

let filter_page mw ~page cmd =
  lwt text = Mwapi_lwt.call Mwapi_parse.(parse ~page wikitext) mw in
  lwt text' = filter_text cmd text in
  lwt token = Utils.get_edit_token ~page mw in
  if text = text' then
    Lwt_log.info "No changes."
  else
    Utils.call_edit
      Mwapi_edit.(edit ~token ~page ~create:`May_not ~recreate:`May_not
		       ~op:(`Replace text') ()) mw

let main api login page_title cmd =
  let page = `Title page_title in
  match cmd with
  | [] -> exit 0
  | (prog :: _) ->
    Lwt_main.run begin
      lwt mw = Mwapi_lwt.open_api api in
      begin match login with
      | None -> Lwt.return_unit
      | Some (_ as name, password) -> Utils.login ~name ~password mw
      end >>
      filter_page mw ~page (prog, Array.of_list cmd) >>
      Mwapi_lwt.close_api mw
    end

let () =
  let api_t =
    Arg.(required & opt (some uri_conv) None &
	 info ~docv:"URI" ~doc:"Location of api.php" ["api"]) in
  let login_t =
    Arg.(value & opt (some up_conv) None &
	 info ~docv:"USER:PASS"
	      ~doc:"Login with the given credentials." ["login"]) in
  let page_title_t =
    Arg.(required & opt (some string) None &
	 info ~docv:"TITLE" ~doc:"Title of the page to edit."
	      ["P"; "page-title"]) in
  let cmd_t =
    Arg.(value & pos_all string [] & info ~docv:"COMMAND" []) in
  let main_t = Term.(pure main $ api_t $ login_t $ page_title_t $ cmd_t) in
  match Term.(eval (main_t, info "mw-filter")) with
  | `Error `Parse -> exit 64
  | `Error `Term -> exit 69
  | `Error `Exn -> exit 70
  | `Ok () -> exit 0
  | `Version | `Help -> exit 0
