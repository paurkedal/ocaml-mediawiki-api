(* Copyright (C) 2015--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

(** {2 list=allimages} *)

type aisize = {aisize_size : int; aisize_width : int; aisize_height : int}
type aiurl = {aiurl_url : string; aiurl_descriptionurl : string}

module Aiprop : sig
  type 'a t

  val (&) : 'a t -> 'b t -> ('a * 'b) t

  val timestamp : CalendarLib.Calendar.t t
  val user : string t
  val userid : int t
  val comment : string t
  val parsedcomment : string t
  val canonicaltitle : string t
  val url : aiurl t
  val size : aisize t
  val sha1 : string t
  val mime : string t
  val mediatype : string t
  (* TODO: metadata, commonmetadata, extmetadata *)
  val bitdepth : int t
end

type 'a allimage = {
  ai_name : string;
  ai_title : string;
  ai_ns : int;
  ai_prop : 'a;
}

val allimages :
        ?sort: [`Name | `Timestamp] ->
        ?dir: [`Ascending | `Descending | `Newer | `Older] ->
        ?start: string ->
        ?continue: string ->
        ?stop: string ->
        ?tstart: CalendarLib.Calendar.t ->
        ?tstop: CalendarLib.Calendar.t ->
        ?prefix: string ->
        ?minsize: int ->
        ?maxsize: int ->
        ?sha1: [`Hex of string | `Base36 of string] ->
        ?user: string ->
        ?filterbots: [`All | `Bots | `Nobots] ->
        ?mime: string ->
        ?limit: int ->
        'a Aiprop.t -> ('a allimage list, [`G]) list_query

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
