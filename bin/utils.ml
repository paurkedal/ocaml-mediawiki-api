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

open Printf
open Unprime_list
open Unprime_option

let (@@) f x = f x
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
let fail_f fmt = ksprintf (fun s -> Lwt.fail (Failure s)) fmt

let log_section = Lwt_log.Section.make "mw-tools"

let login ~name ~password mw =
  match_lwt
    match_lwt Mwapi_lwt.call (Mwapi_login.login ~name ~password ()) mw with
    | `Need_token token ->
      Mwapi_lwt.call (Mwapi_login.login ~name ~password ~token ()) mw
    | status -> Lwt.return status
  with
  | `Success -> Lwt.return_unit
  | status ->
    fail_f "Login failed: %s" (Mwapi_login.string_of_login_status status)

let get_edit_token ~page mw =
  let for_pages =
    match page with `Title tt -> Mwapi_query_prop.for_titles [tt]
                  | `Id id -> Mwapi_query_prop.for_pageids [id] in
  let req = Mwapi_query.only_pages (for_pages Mwapi_query_prop.intoken_edit) in
  lwt res = Mwapi_lwt.call req mw in
  match res.Mwapi_query.query_pages with
  | [`Present (_, _, _, token)] | [`Missing (_, _, token)] ->
    Lwt.return token
  | [`Invalid title] ->
    fail_f "Unexpected invalid-response to edit-token request for \"%s\"." title
  | rs ->
    fail_f "Expected single edit-token response, got %d." (List.length rs)

let call_edit op mw =
  let open Mwapi_edit in
  lwt r = Mwapi_lwt.call op mw in
  match r.edit_change with
  | None ->
    Lwt_log.info_f ~section:log_section
      "No changes to page # %d \"%s\"." r.edit_pageid r.edit_title
  | Some change ->
    Lwt_log.info_f ~section:log_section
      "Updated page # %d \"%s\" rev %d -> %d at %s." r.edit_pageid r.edit_title
      change.change_oldrevid change.change_newrevid change.change_newtimestamp
