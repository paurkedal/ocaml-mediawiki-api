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

open Printf

module String_map = Prime_map.Make (String)

let failwith_f fmt = ksprintf failwith fmt

let pair x y = x, y

type params = (string * string) list

let pass conv label x params = (label, conv x) :: params

let pass_opt conv label xo params =
  match xo with
  | None -> params
  | Some x -> (label, conv x) :: params

let pass_list conv label xs params =
  match xs with
  | [] -> params
  | _ -> (label, String.concat "|" (List.map conv xs)) :: params

let pass_if_true label cond params =
  if cond then (label, "") :: params else params
