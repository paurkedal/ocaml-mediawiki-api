(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Mwapi_common

module Rvprop : sig

  module type SCHEME = sig

    type 'a mode

    type ('ids, 'flags, 'timestamp, 'user, 'userid, 'size, 'sha1,
          'contentmodel, 'comment, 'parsedcomment, 'content, 'tags) t =
    {
      ids: 'ids mode;
      flags: 'flags mode;
      timestamp: 'timestamp mode;
      user: 'user mode;
      userid: 'userid mode;
      size: 'size mode;
      sha1: 'sha1 mode;
      contentmodel: 'contentmodel mode;
      comment: 'comment mode;
      parsedcomment: 'parsedcomment mode;
      content: 'content mode;
      tags: 'tags mode;
    }
  end

  module Reply : sig
    type ids = {revid: int; parentid: int}
    type flags = {minor: bool}
    type content = {contentformat: string; contentbody: string}
    type slots = content String_map.t
    include SCHEME with type 'a mode := 'a ident
  end

  module Request : sig
    type 'a requested
    include SCHEME with type 'a mode := 'a requested

    val none : (unit, unit, unit, unit, unit, unit,
                unit, unit, unit, unit, unit, unit) t
    val ids : Reply.ids requested
    val flags : Reply.flags requested
    val timestamp : CalendarLib.Calendar.t requested
    val user : string requested
    val userid : int requested
    val size : int requested
    val sha1 : string requested
    val contentmodel : string requested
    val comment : string requested
    val parsedcomment : string requested
    val content : Reply.slots requested
    val tags : string list requested
  end

end

val revisions :
  ?limit: int ->
  ?startid: int ->
  ?stopid: int ->
  ?tstart: CalendarLib.Calendar.t ->
  ?tstop: CalendarLib.Calendar.t ->
  ?dir: [`Newer | `Older] ->
  ?user: string ->
  ?excludeuser: string ->
  ?tag: string ->
  ?expandtemplates: bool ->
  ?generatexml: bool ->
  ?parse: bool ->
  ?section: int ->
  ?continue: string ->
  ?diffto: [`Id of int | `Prev | `Next | `Cur | `Text of string] ->
  ?slots: string list ->
  ?contentformat: string ->
  ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l) Rvprop.Request.t ->
  (('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l) Rvprop.Reply.t list,
   unit) Mwapi_query_prop.nprop
