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

open Mwapi

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

val string_of_login_status : login_status -> string

val login : name: string -> password: string -> ?domain: string ->
	    ?token: string -> unit -> login_status request

val logout : unit request
