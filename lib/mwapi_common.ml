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

open Unprime
open Unprime_list

module String_map = Prime_map.Make (String)
module String_set = Set.Make (String)

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
      (fun _ xs ys ->
        match xs, ys with
        | Some xs, None -> Some xs
        | None, Some ys -> Some ys
        | Some xs, Some ys -> Some (String_set.union xs ys)
        | None, None -> None)
end

module Nlist = struct
  type (_, _) t =
    | [] : ('a, [`Z]) t
    | (::) : 'a * ('a, 'n) t -> ('a, [`S of 'n]) t

  let rec map : type a b n. (a -> b) -> (a, n) t -> (b, n) t = fun f -> function
   | [] -> []
   | x :: xs -> f x :: map f xs

  let rec to_list : type a n. (a, n) t -> a list = function
   | [] -> ([] : _ list)
   | x :: xs -> ((x :: to_list xs) : _ list)
end
