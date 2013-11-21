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

type 'a prop

val (&) : 'a prop -> 'b prop -> ('a * 'b) prop

val parse :
  ?disablepp: bool ->
  ?redirects: bool ->
  ?contentformat: string ->
  ?contentmodel: string ->
  page: [< `Title of string | `Id of int | `Rev of int] ->
  ?section: int ->
  'a prop -> 'a request

val title : string prop

val revid : int prop

type section = {
  section_toclevel : int;
  section_level : int;
  section_line : string;
  section_number : string;
  section_index : string; (* Templates have a "T-" prefix. *)
  section_fromtitle : string;
  section_byteoffset : int;
  section_anchor : string;
}
val sections : section list prop

(* [1] Should these really be strings, as suggested by the JSON data? *)
