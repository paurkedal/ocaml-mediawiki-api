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
open Unprime
open Unprime_list

module String_map = Prime_map.Make (String)
module String_set = Set.Make (String)

let failwith_f fmt = ksprintf failwith fmt

let pair x y = x, y

type params = (string * string) list

let pass conv label ?default x params =
  if default = Some x then params else (label, conv x) :: params

let pass_opt conv label xo params =
  match xo with
  | None -> params
  | Some x -> (label, conv x) :: params

let pass_list conv label xs params =
  match xs with
  | [] -> params
  | _ -> (label, String.concat "|" (List.map conv xs)) :: params

let pass_if label cond params =
  if cond then (label, "") :: params else params

module Qparams = struct
  type t = String_set.t String_map.t

  let empty = String_map.empty

  let singleton k v = String_map.singleton k (String_set.singleton v)

  let add k v q =
    try String_map.add k (String_set.add v (String_map.find k q)) q
    with Not_found -> String_map.add k (String_set.singleton v) q

  let of_list xs = List.fold (uncurry add) xs String_map.empty

  let to_params q =
    String_map.fold
      (fun k xs acc -> (k, String.concat "|" (String_set.elements xs)) :: acc)
      q []

  let merge =
    String_map.merge
      (fun k xs ys ->
	match xs, ys with
	| Some xs, None -> Some xs
	| None, Some ys -> Some ys
	| Some xs, Some ys -> Some (String_set.union xs ys)
	| None, None -> None)
end

module K_repair = struct
  open Kojson_pattern

  let int =
    K.convert "int"
      begin function
      | `Int i -> i
      | `String s -> int_of_string s
      | _ -> failwith "A string or, preferably, integer expected."
      end
end
