(* Copyright (C) 2013--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

open Logging
open Printf
open Mwapi_common

let fail_f fmt = ksprintf (fun s -> Lwt.fail (Failure s)) fmt

let login ~name ~password mw =
  match%lwt
    match%lwt Mwapi_lwt.call (Mwapi_login.login ~name ~password ()) mw with
    | `Need_token token ->
      Mwapi_lwt.call (Mwapi_login.login ~name ~password ~token ()) mw
    | status -> Lwt.return status
  with
  | `Success -> Lwt.return_unit
  | status ->
    fail_f "Login failed: %s" (Mwapi_login.string_of_login_status status)

let get_edit_token mw =
  let req = Mwapi_query.only_meta (Mwapi_query_meta.tokens Nlist.[`Csrf]) in
  let%lwt res = Mwapi_lwt.call req mw in
  (match res.Mwapi_query.query_meta with
   | Nlist.[token] -> Lwt.return token)

let call_edit op mw =
  let open Mwapi_edit in
  let%lwt r = Mwapi_lwt.call op mw in
  match r.edit_change with
  | None ->
    Log.info (fun f ->
      f "No changes to page # %d %S." r.edit_pageid r.edit_title)
  | Some change ->
    Log.info (fun f ->
      f "Updated page # %d %S rev %d -> %d at %s." r.edit_pageid r.edit_title
      change.change_oldrevid change.change_newrevid change.change_newtimestamp)
