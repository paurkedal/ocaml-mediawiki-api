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

(** Meta queries ([action=query&meta=*]). *)

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

val tokens : ([< token_type], 'n) Nlist.t -> (string, 'n) Nlist.t meta_query
