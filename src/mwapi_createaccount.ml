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

let decode_result = function
  | `String "needtoken" -> `Needtoken
  | `String "success" -> `Success
  | `String "warning" -> `Warning
  | res -> failwith "Unrecognized result from createacconut."

let request_decode =
  "createaccount"^:
    K.assoc begin
      "token"^: K.string *> fun token ->
      "result"^: K.convert "createaccount_result" decode_result *> fun result ->
      "username"^?: Option.map K.string *> fun username ->
      "userid"^?: Option.map K.int *> fun userid ->
      Ka.stop
	begin match result, username, userid with
	| (`Success | `Warning), Some username, Some userid ->
	  `Success (username, userid)
	| `Needtoken, None, None ->
	  `Needtoken token
	| _ -> failwith "Unrecognized response from createaccount."
	end
    end *> pair

let createaccount ~name ?password ?domain ?token ?email ?realname
		  ?(mailpassword = false) ?reason ?language () =
  let request_params = ["action", "createaccount"; "name", name]
    |> pass_opt ident "password" password
    |> pass_opt ident "domain" domain
    |> pass_opt ident "token" token
    |> pass_opt ident "email" email
    |> pass_opt ident "realname" realname
    |> pass_if "mailpassword" mailpassword
    |> pass_opt ident "reason" reason
    |> pass_opt ident "language" language in
  {request_method = `POST; request_params; request_decode}
