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

open Kojson_pattern
open Mwapi
open Mwapi_utils
open Unprime
open Unprime_option

type login_status =
  [ `Success
  | `Illegal
  | `Not_exists
  | `Empty_pass
  | `Wrong_pass
  | `Wrong_plugin_pass
  | `Created_blocked
  | `Throttled
  | `Blocked
  | `Need_token of string ]

let string_of_login_status = function
  | `Success -> "Success"
  | `Illegal -> "Illegal"
  | `Not_exists -> "NotExists"
  | `Empty_pass -> "EmptyPass"
  | `Wrong_pass -> "WrongPass"
  | `Wrong_plugin_pass -> "WrongPluginPass"
  | `Created_blocked -> "CreateBlocked"
  | `Throttled -> "Throttled"
  | `Blocked -> "Blocked"
  | `Need_token s -> "NeedToken[" ^ s ^ "]"

let login_status_of_string' ?(token : string option) = function
  | "Success" -> `Success
  | "Illegal" -> `Illegal
  | "NotExists" -> `Not_exists
  | "EmptyPass" -> `Empty_pass
  | "WrongPass" -> `Wrong_pass
  | "WrongPluginPass" -> `Wrong_plugin_pass
  | "CreateBlocked" -> `Created_blocked
  | "Throttled" -> `Throttled
  | "Blocked" -> `Blocked
  | "NeedToken" ->
    begin match token with
    | None -> failwith "Got \"NeedToken\" from MW login, but no token."
    | Some token -> `Need_token token
    end
  | "NoName" ->
    failwith "Unexpected \"NoName\" status from MW login, we passed lgname."
  | "mustbeposted" ->
    failwith "Unexpected \"mustbeposted\" status from MW login, we did POST."
  | s ->
    failwith_f "Unknown \"%s\" status received from MW login." s

let login_status_of_json' ?token = function
  | `String s -> login_status_of_string' ?token s
  | _ -> failwith "Expecting a string."

let decode_login =
  "login"^:
    K.assoc begin
      "token"^?: Option.map K.string *> fun token ->
      "result"^: K.convert "login_status" (login_status_of_json' ?token) *>
      Ka.stop
    end *> pair

let login ~name ~password ?domain ?token () =
  let params = ["action", "login"; "lgname", name; "lgpassword", password]
    |> pass_opt ident "lgdomain" domain
    |> pass_opt ident "lgtoken" token in
  { request_method = `POST;
    request_params = params;
    request_decode = decode_login; }

let logout =
  { request_method = `POST;
    request_params = ["action", "logout"];
    request_decode = fun jain -> ((), jain) }
