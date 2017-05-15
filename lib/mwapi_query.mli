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

open Mwapi
open Mwapi_utils

type continue = (string * string) list

type 'a meta_query = {
  mq_params : Qparams.t;
  mq_decode : Kojson.jain -> 'a * Kojson.jain;
}

type ('a, 'k) list_query = {
  lq_params : Qparams.t;
  lq_decode : Kojson.jain -> 'a * Kojson.jain;
}

type ('a, 'am) page =
  [ `Present of string * int * int * 'a (* title, ns, pageid, prop *)
  | `Missing of string * int * 'am      (* title, ns, prop *)
  | `Invalid of string ]                (* title *)

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

val no_meta : unit meta_query
val no_list : (unit, [`N]) list_query
val no_pages : (unit, unit, [`N]) page_query

val combine : ?continue: continue ->
              'm meta_query ->
              ('l, 'lk) list_query ->
              ('a, 'am, 'ak) page_query -> ('m, 'l, 'a, 'am) query request

val only_meta : ?continue: continue -> 'm meta_query ->
                ('m, unit, unit, unit) query request
val only_list : ?continue: continue -> ('l, 'lk) list_query ->
                (unit, 'l, unit, unit) query request
val only_pages : ?continue: continue -> ('a, 'am, 'ak) page_query ->
                 (unit, unit, 'a, 'am) query request
