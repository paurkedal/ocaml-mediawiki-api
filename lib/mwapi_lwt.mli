(* Copyright (C) 2013--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

type client

val open_api :
  ?cert: string -> ?certkey: string ->
  ?load_cookies: bool ->
  Uri.t -> client Lwt.t

val close_api :
  ?save_cookies: bool ->
  client -> unit Lwt.t

val with_api :
  ?cert: string -> ?certkey: string ->
  ?load_cookies: bool ->
  ?save_cookies: bool ->
  Uri.t -> (client -> 'a Lwt.t) -> 'a Lwt.t

val get_json : (string * string) list -> client -> json Lwt.t
val post_json : (string * string) list -> client -> json Lwt.t
val call : 'a request -> client -> 'a Lwt.t
