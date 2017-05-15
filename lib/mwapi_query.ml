(* Copyright (C) 2013--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Kojson_pattern
open Mwapi
open Mwapi_utils
open Unprime
open Unprime_list
open Unprime_option

(* The continue field may contain a mixture of at least strings and integers,
 * but they must be flattened to strings when passed back anyway, so just save
 * them as such. *)
type continue = (string * string) list

let continue_value_of_json = function
  | `Null -> ""                     (* Probably unused. *)
  | `Bool x -> string_of_bool x     (* Probably unused. *)
  | `Int x -> string_of_int x
  | `Float x -> string_of_float x   (* Probably unused. *)
  | `String x -> x
  | `Assoc _ | `List _ -> failwith "Scalar expected."

type 'm meta_query = {
  mq_params : Qparams.t;
  mq_decode : Kojson.jain -> 'm * Kojson.jain;
}

type ('l, 'k) list_query = {
  lq_params : Qparams.t;
  lq_decode : Kojson.jain -> 'l * Kojson.jain;
}

type ('a, 'am) page =
  [ `Present of string * int * int * 'a
  | `Missing of string * int * 'am
  | `Invalid of string ]

type ('a, 'am, 'k) page_query = {
  pq_params : Qparams.t;
  pq_decode : Kojson.jain -> ('a, 'am) page list * Kojson.jain;
}

type ('m, 'l, 'a, 'am) query = {
  query_meta : 'm;
  query_list : 'l;
  query_pages : ('a, 'am) page list;
  query_continue : continue option;
}

let no_meta = {mq_params = Qparams.empty; mq_decode = fun jain -> ((), jain)}
let no_list = {lq_params = Qparams.empty; lq_decode = fun jain -> ((), jain)}
let no_pages = {pq_params = Qparams.empty; pq_decode = fun jain -> ([], jain)}

let decode_continue =
  Option.map @@ K.assoc @@ Ka.map @@ fun k ->
  K.convert "continuation parameter" continue_value_of_json *> fun v -> (k, v)

let combine ?continue mq lq pq =
  let params =
    Qparams.merge (Qparams.merge mq.mq_params pq.pq_params) lq.lq_params in
  let cont_params =
    match continue with
    | None -> ["continue", ""]
    | Some params -> params in
  let request_params =
    ("action", "query") :: (cont_params @ Qparams.to_params params) in
  let request_decode =
    "continue"^?: decode_continue *> fun query_continue ->
    "query"^: K.assoc begin fun jain ->
      let query_meta, jain = mq.mq_decode jain in
      let query_list, jain = lq.lq_decode jain in
      let query_pages, jain = pq.pq_decode jain in
      Ka.stop {query_meta; query_list; query_pages; query_continue} jain
    end *> pair in
  {request_method = `GET; request_params; request_decode}

let only_meta ?continue mq = combine ?continue mq no_list no_pages
let only_list ?continue lq = combine ?continue no_meta lq no_pages
let only_pages ?continue pq = combine ?continue no_meta no_list pq
