(* Copyright (C) 2013--2017  Petter A. Urkedal <paurkedal@gmail.com>
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
open Lwt.Infix
open Mwapi
open Printf
open Unprime
open Unprime_option

let section = Lwt_log.Section.make "mwapi"

module Cookiejar_io = Mwapi_cookiejar.Make (Cohttp_lwt_unix_io)

type client = {
  ctx : Cohttp_lwt_unix.Client.ctx option;
  endpoint : Uri.t;
  cookiejar : Mwapi_cookiejar.t;
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

let open_api ?cert ?certkey ?(logger = !Lwt_log.default) endpoint =
  (match Uri.scheme endpoint with Some "https" -> Ssl.init () | _ -> ());
  let make_context cert certkey =
    (* FIXME: TLS client authentication is not cleanly supported in Conduit yet,
     * but this works when using OpenSSL: *)
    Ssl.use_certificate Conduit_lwt_unix_ssl.Client.default_ctx cert certkey;
    None
  in
  let ctx =
    (match cert, certkey with
     | None, None -> None
     | None, Some cert | Some cert, None -> make_context cert cert
     | Some cert, Some certkey -> make_context cert certkey)
  in
  (match Uri.host endpoint with
   | None -> Lwt.fail (Failure "Missing domain on MWAPI URL.")
   | Some domain ->
      let cookiejar = Mwapi_cookiejar.create () in
      let origin = Mwapi_cookiejar.uri_origin endpoint in
      let fp = Mwapi_cookiejar.persistence_path ~origin () in
      Lwt.catch
        (fun () ->
          Lwt_io.with_file Lwt_io.input fp
            (fun ic -> Cookiejar_io.read ~origin ic cookiejar))
        (function _ -> Lwt.return_unit) >>
      Lwt.return {ctx; endpoint; cookiejar; logger})

let close_api {endpoint; cookiejar} =
  match Uri.host endpoint with
  | None -> Lwt.return_unit
  | Some domain ->
    (* TODO: Expire cookies *)
    let origin = Mwapi_cookiejar.uri_origin endpoint in
    let fp = Mwapi_cookiejar.persistence_path ~origin () in
    mkdir_rec (Filename.dirname fp) >>
    Lwt_io.with_file Lwt_io.output fp
      (fun oc -> Cookiejar_io.write ~origin oc cookiejar)

let decode_star =
  K.assoc begin
    "*"^: K.string %> fun text ->
    Ka.stop text
  end

let decode_warnings =
  K.assoc_or_null (Ka.map (fun m -> decode_star %> fun msg -> (m, msg)))

let make_json_warning_buffer () =
  let json_warnings = Lwt_sequence.create () in
  let warn p msg = ignore (Lwt_sequence.add_r (p, msg) json_warnings) in
  let rec emit logger =
    try
      let (p, msg) = Lwt_sequence.take_l json_warnings in
      Lwt_log.warning_f ~logger "In response below %s: %s"
                        (Kojson.string_of_path p) msg >>
      emit logger
    with Lwt_sequence.Empty -> Lwt.return_unit in
  (warn, emit)

let decode_json logger resp body =
  let%lwt body_str = Cohttp_lwt_body.to_string body in
  let warn, emit_json_warnings = make_json_warning_buffer () in

  match Response.status resp with
  | `OK ->
    Yojson.Basic.from_string body_str |> Kojson.jin_of_json ~warn |>
    K.assoc_or_null begin
      Ka.first [
        begin
          "error"^:
            K.assoc begin
              "code"^: K.string %> fun wiki_error_code ->
              "info"^: K.string %> fun wiki_error_info ->
              "*"^?: Option.map K.string %> fun wiki_error_details ->
              Ka.stop {wiki_error_code; wiki_error_info; wiki_error_details}
            end %> fun err ->
          Ka.stop (emit_json_warnings logger >> Lwt.fail (Wiki_error err))
        end;
        begin
          "warnings"^?: Option.map decode_warnings %> fun wiki_warnings ->
          fun rest ->
            let result = `Assoc (Ka.any rest) in
            emit_json_warnings logger >>
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

let log_headers logger headers =
  Lwt_list.iter_s
    (fun ln -> Lwt_log.debug_f ~logger ~section "Header: %s" ln)
    (Cohttp.Header.to_lines headers)

let get_json params {ctx; endpoint; cookiejar; logger} =
  let params = ("format", "json") :: params in
  let params = List.map (fun (k, v) -> (k, [v])) params in
  let uri = Uri.with_query endpoint params in
  let cookies_hdr = Mwapi_cookiejar.header endpoint cookiejar in
  let headers = Cohttp.Header.of_list [cookies_hdr] in
  log_headers logger headers >>
  Lwt_log.debug_f ~logger ~section "GETting %s." (Uri.to_string uri) >>
  let%lwt resp, body = Client.get ?ctx ~headers uri in
  Mwapi_cookiejar.extract endpoint (Cohttp.Response.headers resp) cookiejar;
  decode_json logger resp body

let post_json params {ctx; endpoint; cookiejar; logger} =
  let params = ("format", "json") :: params in
  let params = List.map (fun (k, v) -> (k, [v])) params in
  let postdata = Uri.encoded_of_query params in
  let body = Cohttp_lwt_body.of_string postdata in
  let cookies_hdr = Mwapi_cookiejar.header endpoint cookiejar in
  let headers = Cohttp.Header.of_list
        ["Content-Type", "application/x-www-form-urlencoded"; cookies_hdr] in
  log_headers logger headers >>
  Lwt_log.debug_f ~logger ~section "POSTing to %s: %s"
                  (Uri.to_string endpoint) postdata >>
  let%lwt resp, body = Client.post ?ctx ~headers ~body endpoint in
  Mwapi_cookiejar.extract endpoint (Cohttp.Response.headers resp) cookiejar;
  decode_json logger resp body

let call {request_method; request_params; request_decode} mw =
  begin match request_method with
  | `GET -> get_json
  | `POST -> post_json
  end request_params mw >>= fun json ->
  let warn, emit_json_warnings = make_json_warning_buffer () in
  let result = Kojson.jin_of_json ~warn json
            |> K.assoc_or_null (request_decode %> uncurry Ka.stop) in
  emit_json_warnings mw.logger >> Lwt.return result
