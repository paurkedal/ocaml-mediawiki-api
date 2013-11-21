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

type change = {
  change_oldrevid : int;
  change_newrevid : int;
  change_newtimestamp : string;
}

type edit = {
  edit_pageid : int;
  edit_title : string;
  edit_change : change option;
}

type watchlist = [`Watch | `Unwatch | `Preferences | `Nochange]

val edit :
  ?token: string ->
  ?summary: string ->
  ?minor: bool ->
  ?bot: bool ->
  ?basetimestamp: string ->
  ?starttimestamp: string ->
  ?recreate: [`May | `May_not] ->
  ?create: [`May | `Must | `May_not] ->
  ?watchlist: watchlist ->
  ?md5: string ->
  ?redirect: bool ->
  page: [< `Title of string | `Id of int] ->
  ?section: [< `No of int | `New of string] ->
  op: [< `Replace of string | `Prepend of string | `Append of string
       | `Bracket of string * string
       | `Undo of int | `Undo_upto of int * int ] ->
  unit -> edit request
