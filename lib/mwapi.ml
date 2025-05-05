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

type json = Yojson.Basic.t

type http_error = {
  http_error_code: int;
  http_error_info: string;
}

let pp_http_error =
  let open Fmt in
  const string "HTTP " ++
  using (fun e -> e.http_error_code) int ++ sp ++
  using (fun e -> e.http_error_info) string

type wiki_error = {
  wiki_error_code: string;
  wiki_error_info: string;
  wiki_error_details: string option
}

let pp_wiki_error =
  let open Fmt in
  using (fun e -> e.wiki_error_code) string ++ sp ++
  using (fun e -> e.wiki_error_info) string ++
  using (fun e -> e.wiki_error_details)
    (option (const string ":" ++ sp ++ string))

type 'a request = {
  request_method: [`GET | `POST];
  request_params: (string * string) list;
  request_decode: Kojson.jain -> 'a * Kojson.jain;
}

type error = [
  | `Http_error of http_error
  | `Wiki_error of wiki_error
  | `Json_error of string
  | `Msg of string
]

let pp_error ppf = function
 | `Http_error err -> pp_http_error ppf err
 | `Wiki_error err -> pp_wiki_error ppf err
 | `Json_error err -> Fmt.(const string "Invalid JSON: %s" ++ string) ppf err
 | `Msg msg -> Fmt.string ppf msg

exception Http_error of http_error
exception Wiki_error of wiki_error
