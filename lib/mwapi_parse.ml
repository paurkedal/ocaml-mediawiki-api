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

type 'a prop = {
  prop_names : string;
  prop_decode : Kojson.jain -> 'a * Kojson.jain;
}

let (&) a b =
  let prop_decode jain =
    let x, jain = a.prop_decode jain in
    let y, jain = b.prop_decode jain in
    ((x, y), jain) in
  let prop_names =
    match a.prop_names, b.prop_names with
    | "", pn | pn, "" -> pn
    | pna, pnb -> pna ^ "|" ^ pnb in
  { prop_names; prop_decode }

let prim_parse params {prop_names; prop_decode} =
  let request_params = ("action", "parse") :: ("prop", prop_names) :: params in
  let request_decode =
    "parse"^:
      K.assoc begin
        prop_decode %> fun (x, rest) -> Ka.stop x rest
      end %> pair in
  {request_method = `GET; request_params; request_decode}

let parse ?(disablepp = false) ?(redirects = false) ?contentformat ?contentmodel
          ~page ?section prop =
  let params = []
    |> pass_if "disablepp" disablepp
    |> pass_if "redirects" redirects
    |> pass_opt string_of_int "section" section
    |> pass_opt ident "contentformat" contentformat
    |> pass_opt ident "contentmodel" contentmodel
    |> (match page with
        | `Title s -> pass ident "page" s
        | `Id i -> pass string_of_int "pageid" i
        | `Rev i -> pass string_of_int "oldid" i) in
  prim_parse params prop

let title =
  let prop_decode = "title"^: K.string %> fun title rest -> (title, rest) in
  {prop_names = ""; prop_decode}

let revid =
  let prop_decode = "revid"^: K.int %> fun revid rest -> (revid, rest) in
  {prop_names = "revid"; prop_decode}

let text =
  let prop_decode = "text"^: K.assoc ("*"^: K.string %> Ka.stop) %> pair in
  {prop_names = "text"; prop_decode}

let wikitext =
  let prop_decode = "wikitext"^: K.assoc ("*"^: K.string %> Ka.stop) %> pair in
  {prop_names = "wikitext"; prop_decode}

type section = {
  section_toclevel : int;
  section_level : int;
  section_line : string;
  section_number : string;
  section_index : string;
  section_fromtitle : string;
  section_byteoffset : int;
  section_anchor : string;
}
let sections =
  let prop_decode =
    "sections"^:
      K.list (K.assoc
        begin
          "toclevel"^: K.int %> fun section_toclevel ->
          "level"^: K_repair.int %> fun section_level ->
          "line"^: K.string %> fun section_line ->
          "number"^: K.string %> fun section_number ->
          "index"^: K.string %> fun section_index ->
          "fromtitle"^: K.string %> fun section_fromtitle ->
          "byteoffset"^: K.int %> fun section_byteoffset ->
          "anchor"^: K.string %> fun section_anchor ->
          Ka.stop {
            section_toclevel; section_level; section_line; section_number;
            section_index; section_fromtitle;
            section_byteoffset; section_anchor
          }
        end) %> fun sections rest -> (sections, rest) in
  {prop_names = "sections"; prop_decode}
