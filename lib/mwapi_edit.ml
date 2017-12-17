(* Copyright (C) 2013--2017  Petter A. Urkedal <paurkedal@gmail.com>
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
open Mwapi_prereq
open Unprime

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

let string_of_watchlist = function
  | `Watch -> "watch"
  | `Unwatch -> "unwatch"
  | `Preferences -> "preferences"
  | `Nochange -> "nochange"

let request_decode =
  "edit"^:
    K.assoc begin
      "result"^: K.literal (`String "Success") %> fun () ->
      "pageid"^: K.int %> fun edit_pageid ->
      "title"^: K.string %> fun edit_title ->
      begin
        "nochange"^?: function
        | Some _ -> Ka.stop None
        | None ->
          "oldrevid"^: K.int %> fun change_oldrevid ->
          "newrevid"^: K.int %> fun change_newrevid ->
          "newtimestamp"^: K.string %> fun change_newtimestamp ->
          Ka.stop (Some {change_oldrevid; change_newrevid;
                         change_newtimestamp})
      end %> fun edit_change ->
      {edit_pageid; edit_title; edit_change}
    end %> pair

let edit ?token ?summary ?(minor = false) ?(bot = true)
         ?basetimestamp ?starttimestamp ?(recreate = `May_not) ?(create = `May)
         ?(watchlist : watchlist = `Preferences) ?md5 ?(redirect = false)
         ~page ?section ~op () =
  let page_param =
    match page with
    | `Title title -> ("title", title)
    | `Id pageid -> ("pageid", string_of_int pageid) in
  let request_params = ["action", "edit"; page_param]
    |> pass_opt (function `No i -> string_of_int i | `New _ -> "new")
                "section" section
    |> pass_opt ident "sectiontitle"
        (match section with Some (`No _) | None -> None
                          | Some (`New s) -> Some s)
    |> pass_opt ident "token" token
    |> pass_opt ident "summary" summary
    |> pass_if "minor" minor
    |> pass_if "bot" bot
    |> pass_opt ident "basetimestamp" basetimestamp
    |> pass_opt ident "starttimestamp" starttimestamp
    |> pass_if "recreate" (recreate = `May)
    |> pass_if "createonly" (create = `Must)
    |> pass_if "nocreate" (create = `May_not)
    |> pass string_of_watchlist "watchlist" ~default:`Preferences watchlist
    |> pass_opt ident "md5" md5
    |> pass_if "redirect" redirect
    |> (match op with
        | `Replace s -> pass ident "text" s
        | `Append s -> pass ident "appendtext" s
        | `Prepend s -> pass ident "prependtext" s
        | `Bracket (s0, s1) ->
          pass ident "prependtext" s0 %> pass ident "appendtext" s1
        | `Undo i -> pass string_of_int "undo" i
        | `Undo_upto (i, j) ->
          pass string_of_int "undo" i %> pass string_of_int "undo_after" j) in
  {request_method = `POST; request_params; request_decode}
