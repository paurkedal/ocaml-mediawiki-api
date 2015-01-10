(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

(** List Queries. *)

open Mwapi
open Mwapi_query

(** {2 list=allcategories} *)

type acsize = {
  acsize_size : int;
  acsize_pages : int;
  acsize_files : int;
  acsize_subcats : int;
}

module Acprop : sig
  type 'a t
  val (&) : 'a t -> 'b t -> ('a * 'b) t
  val none : unit t
  val size : acsize t
  val hidden : bool t
end

type 'a allcategory = {
  ac_title : string;
  ac_prop : 'a;
}

val allcategories :
  ?start: string ->
  ?continue: string ->
  ?stop: string ->
  ?prefix: string ->
  ?dir: [`Ascending | `Descending] ->
  ?min: int ->
  ?max: int ->
  ?limit: int ->
  'a Acprop.t -> ('a allcategory list, [`N]) list_query

(** {2 list=allpages} *)

type allpage = {
  ap_pageid : int;
  ap_ns : int;
  ap_title : string;
}

val allpages :
  ?start: string ->
  ?continue: string ->
  ?stop: string ->
  ?prefix: string ->
  ?namespace: int ->
  ?filterredir: [`All | `Redirects | `Nonredirects] ->
  ?minsize: int ->
  ?maxsize: int ->
  ?prtype: [`Edit | `Move | `Upload] list ->
  ?prlevel: [`Autoconfirmed | `Sysop] list ->
  ?prfiltercascade: [`Cascading | `Noncascading | `All] ->
  ?limit: int ->
  ?dir: [`Ascending | `Descending] ->
  ?filterlanglinks: [`Withlanglinks | `Withoutlanglinks | `All] ->
  ?prexpiry: [`Indefinite | `Definite | `All] ->
  unit -> (allpage list, [`G]) list_query
