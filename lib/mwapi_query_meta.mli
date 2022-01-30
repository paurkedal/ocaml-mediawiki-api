(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Meta queries ([action=query&meta=*]). *)

open Mwapi_common
open Mwapi_query

type token_type =
  [ `Csrf
  | `Watch
  | `Patrol
  | `Rollback
  | `Userrights
  | `Login
  | `Createaccount ]

val tokens : ([< token_type], 'n) Nlist.t -> (string, 'n) Nlist.t meta_query

(* Needs more work using siprop,
 * See https://www.mediawiki.org/wiki/API:Siteinfo *)

type namespace = {
  id: int;
  case: [`First_letter | `Case_sensitive];
  subpages: bool;
  canonical: string option;
  defaultcontentmodel: string option;
  name: string;
}

type namespacealias = {
  id: int;
  name: string;
}

type usergroup = {
  name: string;
  rights: string list;
}

type extension = {
  type_: string;
  name: string;
  namemsg: string;
  version: string option;
  descriptionmsg: string;
  author: string;
  url: string option;
  license_name: string option;
  license: string option;
}

module Siprop : sig
  type 'a t
  val (&) : 'a t -> 'b t -> ('a * 'b) t
  val namespaces : namespace list t
  val namespacealiases : namespacealias list t
  val usergroups : usergroup list t
  val extensions : extension list t
end

val siteinfo : 'a Siprop.t -> 'a meta_query
