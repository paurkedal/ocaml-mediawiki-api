(* Copyright (C) 2013--2014  Petter Urkedal <paurkedal@gmail.com>
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

open Cohttp.Cookie

type origin = string * int
(** A host and port used to identify the cookie origin. *)

val uri_origin : Uri.t -> origin

type cookie = Set_cookie_hdr.t

type t

val create : unit -> t

val get : Uri.t -> t -> cookie list
(** [get uri jar] is the list of cookies to send when contacting [uri]. *)

val add : Uri.t -> cookie -> t -> unit
(** [add uri cookie jar] conditionally adds [cookie] to [jar] at the location
    determined from [uri] and the domain and path of the cookie.  If the
    cookie sets a domain it is verified to be a subdomain of the [uri], but
    the cookie is stored under the exact domain of [uri]. *)

val header : Uri.t -> t -> string * string
(** [header uri jar] formats a cookie header for [uri] based on cookies in
    [jar]. *)

val extract : Uri.t -> Cohttp.Header.t -> t -> unit
(** [extract uri hdr jar] calls [add uri cookie jar] for each [cookie] in
    [hdr]. *)

val fold : ?origin: origin -> (cookie -> 'a -> 'a) -> t -> 'a -> 'a
val iter : ?origin: origin -> (cookie -> unit) -> t -> unit

val persistence_path :
  xdg: Xdg.t -> ?ext: string -> origin: origin -> unit -> string

(**/**)
type 'a sink = Sink of ('a -> unit)
val populate : origin -> t -> cookie sink
