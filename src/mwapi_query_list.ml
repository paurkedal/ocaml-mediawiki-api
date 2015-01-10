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

open Mwapi_query
open Mwapi_utils
open Unprime
open Unprime_option

(* list=allcategories *)

type acsize = {
  acsize_size : int;
  acsize_pages : int;
  acsize_files : int;
  acsize_subcats : int;
}

module Acprop = struct

  type 'a t = {
    keys : string;
    decode : Kojson.jain -> 'a * Kojson.jain;
  }

  let (&) p0 p1 =
    let decode jain =
      let x0, jain = p0.decode jain in
      let x1, jain = p1.decode jain in
      ((x0, x1), jain) in
    {keys = p0.keys ^ "|" ^ p1.keys; decode}

  let none = {
    keys = "";
    decode = pair ();
  }

  let size = {
    keys = "size";
    decode =
      let open Kojson_pattern in
      "size"^: K.int *> fun acsize_size ->
      "pages"^: K.int *> fun acsize_pages ->
      "files"^: K.int *> fun acsize_files ->
      "subcats"^: K.int *> fun acsize_subcats ->
      pair {acsize_size; acsize_pages; acsize_files; acsize_subcats};
  }

  let hidden = {
    keys = "hidden";
    decode =
      let open Kojson_pattern in
      "hidden"^?: Option.map K.string *> fun hidden ->
      pair (hidden <> None)
  }
end

type 'a allcategory = {
  ac_title : string;
  ac_prop : 'a;
}

let allcategories
	?start ?continue ?stop
	?prefix
	?(dir = `Ascending)
	?min
	?max
	?limit
	prop =
  let lq_params = Qparams.singleton "list" "allcategories"
    |> Option.fold (Qparams.add "acfrom") start
    |> Option.fold (Qparams.add "accontinune") continue
    |> Option.fold (Qparams.add "acto") stop
    |> Option.fold (Qparams.add "acprefix") prefix
    |> (match dir with
	| `Ascending -> ident
	| `Descending -> Qparams.add "acdir" "descending")
    |> Option.fold (Qparams.add "acmin" *< string_of_int) min
    |> Option.fold (Qparams.add "acmax" *< string_of_int) max
    |> Option.fold (Qparams.add "aclimit" *< string_of_int) limit
    |> (if prop.Acprop.keys <> ""
	then Qparams.add "acprop" prop.Acprop.keys
	else ident) in
  let lq_decode =
    let open Kojson_pattern in
    "allcategories"^:
      K.list begin
	K.assoc begin
	  "*"^: K.string *> fun ac_title ->
	  prop.Acprop.decode *> fun (ac_prop, jain) ->
	  Ka.stop {ac_title; ac_prop} jain
	end
      end *> pair in
  {lq_params; lq_decode}

(* list=allpages *)

type allpage = {
  ap_pageid : int;
  ap_ns : int;
  ap_title : string;
}

let lq_decode =
  let open Kojson_pattern in
  "allpages"^:
    K.list begin
      K.assoc begin
	"pageid"^: K.int *> fun ap_pageid ->
	"ns"^: K.int *> fun ap_ns ->
	"title"^: K.string *> fun ap_title ->
	Ka.stop {ap_pageid; ap_ns; ap_title}
      end
    end *> pair

let allpages
	?start ?continue ?stop
	?prefix
	?namespace
	?(filterredir = `All)
	?minsize
	?maxsize
	?(prtype = [])
	?(prlevel = [])
	?(prfiltercascade = `All)
	?limit
	?(dir = `Ascending)
	?(filterlanglinks = `All)
	?(prexpiry = `All) () =
  let lq_params = Qparams.singleton "list" "allpages"
    |> Option.fold (Qparams.add "apfrom") start
    |> Option.fold (Qparams.add "apcontinue") continue
    |> Option.fold (Qparams.add "apto") stop
    |> Option.fold (Qparams.add "apprefix") prefix
    |> Option.fold (Qparams.add "apnamespace" *< string_of_int) namespace
    |> Option.fold (Qparams.add "apfilterredir")
	(match filterredir with
	  | `All -> None
	  | `Redirects -> Some "redirects"
	  | `Nonredirects -> Some "nonredirects")
    |> Option.fold (Qparams.add "apminsize" *< string_of_int) minsize
    |> Option.fold (Qparams.add "apmaxsize" *< string_of_int) maxsize
    |> (match prtype with
	| [] -> ident
	| xs -> Qparams.add "prtype" @@ String.concat "|" @@
		List.map (function `Edit -> "edit" | `Move -> "move"
				 | `Upload -> "upload") xs)
    |> (match prlevel with
	| [] -> ident
	| xs -> Qparams.add "prlevel" @@ String.concat "|" @@
		List.map (function `Autoconfirmed -> "autoconfirmed"
				 | `Sysop -> "sysop") xs)
    |> (match prfiltercascade with
	| `All -> ident
	| `Cascading -> Qparams.add "prfiltercascade" "cascading"
	| `Noncascading -> Qparams.add "prfiltercascade" "noncascading")
    |> Option.fold (Qparams.add "aplimit" *< string_of_int) limit
    |> (match dir with
	| `Ascending -> ident
	| `Descending -> Qparams.add "apdir" "descending")
    |> (match filterlanglinks with
	| `All -> ident
	| `Withlanglinks -> Qparams.add "apfilterlanglinks" "withlanglinks"
	| `Withoutlanglinks ->
	  Qparams.add "apfilterlanglinks" "withoutlanglinks")
    |> (match prexpiry with
	| `All -> ident
	| `Indefinite -> Qparams.add "apprexpiry" "indefinite"
	| `Definite -> Qparams.add "apprexpiry" "definite") in
  {lq_params; lq_decode}
