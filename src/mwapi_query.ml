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

open Kojson_pattern
open Mwapi
open Mwapi_utils
open Unprime
open Unprime_list
open Unprime_option

type 'm meta_query = {
  mq_params : Qparams.t;
  mq_decode : Kojson.jain -> 'm * Kojson.jain;
}

type ('l, 'k) list_query = {
  lq_params : Qparams.t;
  lq_decode : Kojson.jain -> 'l * Kojson.jain;
}

type 'p page =
  [ `Present of string * int * int * 'p
  | `Missing of string * int
  | `Invalid of string ]

type ('p, 'k) page_query = {
  pq_params : Qparams.t;
  pq_decode : Kojson.jain -> 'p page list * Kojson.jain;
}

type ('m, 'l, 'p) query = {
  query_meta : 'm;
  query_list : 'l;
  query_pages : 'p page list;
  query_continue : (string * string) list;
}

let no_meta = {mq_params = Qparams.empty; mq_decode = fun jain -> ((), jain)}
let no_list = {lq_params = Qparams.empty; lq_decode = fun jain -> ((), jain)}
let no_pages = {pq_params = Qparams.empty; pq_decode = fun jain -> ([], jain)}

let decode_continue =
  "continue"^?:
    Option.map (K.assoc (Ka.map (fun k -> K.string *> fun v -> (k, v))))
      *> fun continue jain ->
  (Option.get_or [] continue, jain)

let combine mq lq pq =
  let params =
    Qparams.merge (Qparams.merge mq.mq_params pq.pq_params) lq.lq_params in
  let request_params =
    ("action", "query") :: ("continue", "") :: Qparams.to_params params in
  let request_decode =
    "query"^: K.assoc begin fun jain ->
      let query_meta, jain = mq.mq_decode jain in
      let query_list, jain = lq.lq_decode jain in
      let query_pages, jain = pq.pq_decode jain in
      let query_continue, jain = decode_continue jain in
      Ka.stop {query_meta; query_list; query_pages; query_continue} jain
    end *> pair in
  {request_method = `GET; request_params; request_decode}

let only_meta mq = combine mq no_list no_pages
let only_list lq = combine no_meta lq no_pages
let only_pages pq = combine no_meta no_list pq
