(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Mwapi_common
open Mwapi_query

type token_type =
  [ `Csrf
  | `Watch
  | `Patrol
  | `Rollback
  | `Userrights
  | `Login
  | `Createaccount ]

let string_of_token_type = function
  | `Csrf -> "csrf"
  | `Watch -> "watch"
  | `Patrol -> "patrol"
  | `Rollback -> "rollback"
  | `Userrights -> "userrights"
  | `Login -> "login"
  | `Createaccount -> "createaccount"

let tokens tokens =
  let mq_params = Qparams.empty
    |> Qparams.add "meta" "tokens"
    |> Qparams.add "type"
        (String.concat "|"
          (List.map string_of_token_type (Nlist.to_list tokens))) in
  let extract m =
    Nlist.map
      (fun token -> String_map.find (string_of_token_type token ^ "token") m)
      tokens in
  let mq_decode =
    let open Kojson_pattern in
    let aux k v = String_map.add k (K.string v) in
    "tokens"^:
      K.assoc (fun ka -> extract (Ka.fold aux ka String_map.empty)) %> fun m ->
    fun jain -> (m, jain)
  in
  {mq_params; mq_decode}
