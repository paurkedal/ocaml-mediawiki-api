(* Copyright (C) 2013--2023  Petter A. Urkedal <paurkedal@gmail.com>
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
open Lwt.Syntax
open Mwapi
open Unprime
open Unprime_option

let ( let*? ) = Lwt_result.Syntax.( let* )

let log_src = Logs.Src.create "mwapi"
module Log = (val Logs_lwt.src_log log_src)

let decode_log_src = Logs.Src.create "mwapi.kojson"
module Decode_log = (val Logs_lwt.src_log decode_log_src)

type client = {
  ctx: Cohttp_lwt_unix.Client.ctx option;
  endpoint: Uri.t;
  cookiejar: Mwapi_cookiejar.t;
  cookiejar_path: string;
}

let rec mkdir_rec dir =
  if dir = "/" || dir = "." then Lwt.return_unit else
  Lwt.catch
    (fun () -> Lwt_unix.stat dir >|= ignore)
    (function
     | Unix.Unix_error (Unix.ENOENT, _, _) ->
        mkdir_rec (Filename.dirname dir) >>= fun () ->
        Lwt_unix.mkdir dir 0o755
     | xc -> Lwt.fail xc)

let () =
  Sys.getenv_opt "MWAPI_TLS_LIBRARY" |> Option.iter begin function
   | "native" -> Conduit_lwt_unix.(tls_library := Native)
   | "openssl" | "OpenSSL" -> Conduit_lwt_unix.(tls_library := OpenSSL)
   | str -> Fmt.failwith "Invalid value %s for $MWAPI_TLS_LIBRARY." str
  end

let open_api_exn ?cert ?certkey ?(load_cookies = false) endpoint =
  let xdg = Xdg.create ~env:Sys.getenv_opt () in
  let make_context cert certkey =
    let tls_own_key =
      `TLS (`Crt_file_path cert, `Key_file_path certkey, `No_password)
    in
    let+ ctx = Conduit_lwt_unix.init ~tls_own_key () in
    Cohttp_lwt_unix.Net.init ~ctx ()
  in
  let* ctx =
    (match cert, certkey with
     | None, None ->
        Lwt.return_none
     | None, Some cert | Some cert, None ->
        Option.some =|< make_context cert cert
     | Some cert, Some certkey ->
        Option.some =|< make_context cert certkey)
  in
  let* origin = Lwt.wrap1 Mwapi_cookiejar.uri_origin endpoint in
  let cookiejar = Mwapi_cookiejar.create () in
  let cookiejar_path = Mwapi_cookiejar.persistence_path ~xdg ~origin () in
  begin
    if not load_cookies then Lwt.return_unit else
    Lwt.catch
      (fun () ->
        Lwt_io.with_file ~mode:Lwt_io.input cookiejar_path
          (fun ic -> Mwapi_cookiejar_lwt.read ~origin ic cookiejar))
      (function _ -> Log.warn (fun f -> f "Contaminated cookie jar."))
  end >>= fun () ->
  Lwt.return {ctx; endpoint; cookiejar; cookiejar_path}

let open_api ?cert ?certkey ?load_cookies endpoint =
  (* TODO: Sort out exceptions. *)
  Lwt.catch
    (fun () ->
      open_api_exn ?cert ?certkey ?load_cookies endpoint >|= Result.ok)
    (function
     | Failure msg -> Lwt.return_error (`Msg msg)
     | exn -> Lwt.fail exn)

let close_api_exn
      ?(save_cookies = false) {endpoint; cookiejar; cookiejar_path; _} =
  (* TODO: Expire cookies *)
  let* origin = Lwt.wrap1 Mwapi_cookiejar.uri_origin endpoint in
  if not save_cookies then Lwt.return_unit else
  let* () = mkdir_rec (Filename.dirname cookiejar_path) in
  Lwt_io.with_file ~mode:Lwt_io.output cookiejar_path
    (fun oc -> Mwapi_cookiejar_lwt.write ~origin oc cookiejar)

let close_api ?save_cookies mw =
  (* TODO: Sort out exceptions. *)
  Lwt.catch
    (fun () -> close_api_exn ?save_cookies mw >|= Result.ok)
    (function
     | Failure msg -> Lwt.return_error (`Msg msg)
     | exn -> Lwt.fail exn)

let with_api
      ?cert ?certkey
      ?(load_cookies = false) ?(save_cookies = load_cookies)
      endpoint f =
  let*? mw = open_api ?cert ?certkey ~load_cookies endpoint in
  Lwt.finalize
    (fun () -> f mw)
    (fun () -> close_api ~save_cookies mw >|= ignore)

let decode_star =
  K.assoc begin
    "*"^: K.string %> fun text ->
    Ka.stop text
  end

let decode_warnings =
  K.assoc_or_null (Ka.map (fun m -> decode_star %> fun msg -> (m, msg)))

let make_json_warning_buffer () =
  let buf = ref [] in
  let warn p msg = buf := (p, msg) :: !buf in
  let emit () =
    let aux (p, msg) =
      Decode_log.debug (fun f ->
        f "In response below %s: %s" (Kojson.string_of_path p) msg) in
    let warnings = List.rev !buf in
    buf := [];
    Lwt_list.iter_s aux warnings
  in
  (warn, emit)

let decode_json resp body =
  let* body_str = Cohttp_lwt.Body.to_string body in
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
          Ka.stop begin
            emit_json_warnings () >>= fun () ->
            Lwt.fail (Wiki_error err)
          end
        end;
        begin
          "warnings"^?: Option.map decode_warnings %> fun wiki_warnings ->
          fun rest ->
            let result = `Assoc (Ka.any rest) in
            emit_json_warnings () >>= fun () ->
            Lwt_list.iter_s
              (fun (m, msg) -> Log.warn (fun f -> f "From wiki %s: %s" m msg))
              (Option.get_or [] wiki_warnings) >>= fun () ->
            Lwt.return result
        end;
      ]
    end
  | st ->
    Lwt.fail (Http_error {http_error_code = Cohttp.Code.code_of_status st;
                          http_error_info = body_str})

let log_headers headers =
  Lwt_list.iter_s
    (fun ln -> Log.debug (fun f -> f "Header: %s" ln))
    (Cohttp.Header.to_lines headers)

let get_json params {ctx; endpoint; cookiejar; _} =
  let params = ("format", "json") :: params in
  let params = List.map (fun (k, v) -> (k, [v])) params in
  let uri = Uri.with_query endpoint params in
  let cookies_hdr = Mwapi_cookiejar.header endpoint cookiejar in
  let headers = Cohttp.Header.of_list [cookies_hdr] in
  log_headers headers >>= fun () ->
  Log.debug (fun f -> f "GETting %a." Uri.pp uri) >>= fun () ->
  let* resp, body = Client.get ?ctx ~headers uri in
  Mwapi_cookiejar.extract endpoint (Cohttp.Response.headers resp) cookiejar;
  decode_json resp body

let post_json params {ctx; endpoint; cookiejar; _} =
  let params = ("format", "json") :: params in
  let params = List.map (fun (k, v) -> (k, [v])) params in
  let postdata = Uri.encoded_of_query params in
  let body = Cohttp_lwt.Body.of_string postdata in
  let cookies_hdr = Mwapi_cookiejar.header endpoint cookiejar in
  let headers = Cohttp.Header.of_list [
    "Content-Type", "application/x-www-form-urlencoded";
    cookies_hdr
  ] in
  log_headers headers >>= fun () ->
  Log.debug (fun f -> f "POSTing to %a: %s" Uri.pp endpoint postdata)
    >>= fun () ->
  let* resp, body = Client.post ?ctx ~headers ~body endpoint in
  Mwapi_cookiejar.extract endpoint (Cohttp.Response.headers resp) cookiejar;
  decode_json resp body

let call_exn {request_method; request_params; request_decode} mw =
  (match request_method with
   | `GET -> get_json
   | `POST -> post_json) request_params mw >>= fun json ->
  let warn, emit_json_warnings = make_json_warning_buffer () in
  let result = Kojson.jin_of_json ~warn json
            |> K.assoc_or_null (request_decode %> uncurry Ka.stop) in
  emit_json_warnings () >>= fun () -> Lwt.return result

let call op mw =
  Lwt.catch (fun () -> call_exn op mw >|= Result.ok)
    (function
     | Http_error err -> Lwt.return_error (`Http_error err)
     | Wiki_error err -> Lwt.return_error (`Wiki_error err)
     | exn -> Lwt.fail exn)
