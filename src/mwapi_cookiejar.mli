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

val xdg_cookie_path : domain: string -> string

module type S = sig
  module IO : Cohttp.IO.S

  type t

  val create : unit -> t
  val extract : Cohttp.Header.t -> t -> unit
  val write : IO.oc -> t -> unit IO.t
  val read : IO.ic -> t -> unit IO.t
end

module Make (IO : Cohttp.IO.S) : S with module IO = IO
