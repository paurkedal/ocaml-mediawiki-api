(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
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

open Cohttp_lwt_unix
open Kojson_pattern
open Mwapi
open Printf
open Unprime
open Unprime_option

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let section = Lwt_log.Section.make "mwapi"

module Cookiejar = Mwapi_cookiejar.Make (Cohttp_lwt_unix_io)

let use_certificate cert key =
  Ssl.init ();
  Ssl.use_certificate Cohttp_lwt_unix_net.Ssl_client.sslctx cert key

type client = {
  endpoint : Uri.t;
  cookiejar : Cookiejar.t;
  logger : Lwt_log.logger;
}

let rec mkdir_rec dir =
  if dir = "/" || dir = "." then Lwt.return_unit else
  Lwt.catch
    (fun () -> Lwt_unix.stat dir >|= ignore)
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
	mkdir_rec (Filename.dirname dir) >>
	Lwt_unix.mkdir dir 0o755
      | xc -> Lwt.fail xc)

let open_api ?(logger = !Lwt_log.default) endpoint =
  (match Uri.scheme endpoint with Some "https" -> Ssl.init () | _ -> ());
  let cookiejar = Cookiejar.create () in
  begin match Uri.host endpoint with
  | None -> Lwt.return_unit
  | Some domain ->
    let fp = Mwapi_cookiejar.xdg_cookie_path ~domain in
    Lwt.catch
      (fun () ->
	Lwt_io.with_file Lwt_io.input fp
	  (fun ic -> Cookiejar.read ic cookiejar))
      (function _ -> Lwt.return_unit)
  end >>
  Lwt.return {endpoint; cookiejar; logger}

let close_api {endpoint; cookiejar} =
  match Uri.host endpoint with
  | None -> Lwt.return_unit
  | Some domain ->
    (* TODO: Expire cookies *)
    let fp = Mwapi_cookiejar.xdg_cookie_path ~domain in
    mkdir_rec (Filename.dirname fp) >>
    Lwt_io.with_file Lwt_io.output fp
      (fun oc -> Cookiejar.write oc cookiejar)

let decode_star =
  K.assoc begin
    "*"^: K.string *> fun text ->
    Ka.stop text
  end

let decode_warnings =
  K.assoc (Ka.map (fun m -> decode_star *> fun msg -> (m, msg)))

let decode_json logger resp body =
  lwt body_str = Cohttp_lwt_body.string_of_body body in

  let json_warnings = Lwt_sequence.create () in
  let warn p msg = ignore (Lwt_sequence.add_r (p, msg) json_warnings) in
  let rec emit_json_warnings () =
    try
      let (p, msg) = Lwt_sequence.take_l json_warnings in
      Lwt_log.warning_f ~logger "In response below %s: %s"
			(Kojson.string_of_path p) msg >>
      emit_json_warnings ()
    with Lwt_sequence.Empty -> Lwt.return_unit in

  match Response.status resp with
  | `OK ->
    Yojson.Basic.from_string body_str |> Kojson.jin_of_json ~warn |>
    K.assoc begin
      Ka.first [
	begin
	  "error"^:
	    K.assoc begin
	      "code"^: K.string *> fun wiki_error_code ->
	      "info"^: K.string *> fun wiki_error_info ->
	      "*"^?: Option.map K.string *> fun wiki_error_details ->
	      Ka.stop {wiki_error_code; wiki_error_info; wiki_error_details}
	    end *> fun err ->
	  Ka.stop (emit_json_warnings () >> Lwt.fail (Wiki_error err))
	end;
	begin
	  "warnings"^?: Option.map decode_warnings *> fun wiki_warnings ->
	  fun rest ->
	    let result = `Assoc (Ka.any rest) in
	    emit_json_warnings () >>
	    Lwt_list.iter_s
	      (fun (m, msg) ->
		Lwt_log.warning_f ~logger "From wiki %s: %s" m msg)
	      (Option.get_or [] wiki_warnings) >>
	    Lwt.return result
	end;
      ]
    end
  | st ->
    Lwt.fail (Http_error {http_error_code = Cohttp.Code.code_of_status st;
			  http_error_info = body_str})

let fail_with_no_response = Lwt.fail
  (Http_error {http_error_code = 0; http_error_info = "No response."})

let get_json {endpoint; cookiejar; logger} params =
  let params = ("format", "json") :: params in
  let params = List.map (fun (k, v) -> (k, [v])) params in
  let uri = Uri.with_query endpoint params in
  Lwt_log.debug_f ~logger ~section "GETting %s." (Uri.to_string uri) >>
  match_lwt Client.get uri with
  | None -> fail_with_no_response
  | Some (resp, body) -> decode_json logger resp body

let post_json {endpoint; cookiejar; logger} params =
  let params = ("format", "json") :: params in
  let params = List.map (fun (k, v) -> (k, [v])) params in
  let postdata = Uri.encoded_of_query params in
  let body = Cohttp_lwt_body.body_of_string postdata in
  let headers = Cohttp.Header.of_list
	["Content-Type", "application/x-www-form-urlencoded"] in
  Lwt_log.debug_f ~logger ~section "POSTing to %s: %s"
		  (Uri.to_string endpoint) postdata >>
  match_lwt Client.post ~headers ?body endpoint with
  | None -> fail_with_no_response
  | Some (resp, body) -> decode_json logger resp body

let call mw {request_method; request_params; request_decode} =
  begin match request_method with
  | `GET -> get_json
  | `POST -> post_json
  end mw request_params >|=
  request_decode
