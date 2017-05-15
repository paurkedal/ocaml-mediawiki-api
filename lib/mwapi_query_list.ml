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
      "size"^: K.int %> fun acsize_size ->
      "pages"^: K.int %> fun acsize_pages ->
      "files"^: K.int %> fun acsize_files ->
      "subcats"^: K.int %> fun acsize_subcats ->
      pair {acsize_size; acsize_pages; acsize_files; acsize_subcats};
  }

  let hidden = {
    keys = "hidden";
    decode =
      let open Kojson_pattern in
      "hidden"^?: Option.map K.string %> fun hidden ->
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
    |> Option.fold (Qparams.add "acmin" % string_of_int) min
    |> Option.fold (Qparams.add "acmax" % string_of_int) max
    |> Option.fold (Qparams.add "aclimit" % string_of_int) limit
    |> (if prop.Acprop.keys <> ""
        then Qparams.add "acprop" prop.Acprop.keys
        else ident) in
  let lq_decode =
    let open Kojson_pattern in
    "allcategories"^:
      K.list begin
        K.assoc begin
          "*"^: K.string %> fun ac_title ->
          prop.Acprop.decode %> fun (ac_prop, jain) ->
          Ka.stop {ac_title; ac_prop} jain
        end
      end %> pair in
  {lq_params; lq_decode}

(* list=allimages *)

type aisize = {aisize_size : int; aisize_width : int; aisize_height : int}
type aiurl = {aiurl_url : string; aiurl_descriptionurl : string}

module Aiprop = struct
  type 'a t = {
    keys : string;
    decode : Kojson.jain -> 'a * Kojson.jain;
  }

  let (&) a b =
    let decode jain =
      let x, jain = a.decode jain in
      let y, jain = b.decode jain in
      ((x, y), jain) in
    {keys = a.keys ^ "|" ^ b.keys; decode}

  open Kojson_pattern

  let timestamp =
    { keys = "timestamp";
      decode = "timestamp"^: K.convert_string "time" caltime_of_string %> pair }
  let user = {keys = "user"; decode = "user"^: K.string %> pair}
  let userid = {keys = "userid"; decode = "userid"^: K_repair.int %> pair}
  let comment = {keys = "comment"; decode = "comment"^: K.string %> pair}
  let parsedcomment =
    {keys = "parsedcomment"; decode = "parsedcomment"^: K.string %> pair}
  let canonicaltitle =
    {keys = "canonicaltitle"; decode = "canonicaltitle"^: K.string %> pair}
  let url =
    let decode =
      "url"^: K.string %> fun aiurl_url ->
      "descriptionurl"^: K.string %> fun aiurl_descriptionurl ->
      pair {aiurl_url; aiurl_descriptionurl} in
    {keys = "url"; decode}
  let size =
    let decode =
      "size"^: K.int %> fun aisize_size ->
      "width"^: K.int %> fun aisize_width ->
      "height"^: K.int %> fun aisize_height ->
      pair {aisize_size; aisize_width; aisize_height} in
    {keys = "size"; decode}
  let sha1 = {keys = "sha1"; decode = "sha1"^: K.string %> pair}
  let mime = {keys = "mime"; decode = "mime"^: K.string %> pair}
  let mediatype = {keys = "mediatype"; decode = "mediatype"^: K.string %> pair}
  let bitdepth = {keys = "bitdepth"; decode = "bitdepth"^: K_repair.int %> pair}
end

type 'a allimage = {
  ai_name : string;
  ai_title : string;
  ai_ns : int;
  ai_prop : 'a;
}

let allimages
        ?(sort = `Name)
        ?(dir = `Ascending)
        ?start ?continue ?stop
        ?tstart ?tstop
        ?prefix
        ?minsize ?maxsize
        ?sha1
        ?user
        ?(filterbots = `All)
        ?mime
        ?limit
        prop =
  let lq_params = Qparams.singleton "list" "allimages"
    |> (if prop.Aiprop.keys = ""
        then ident
        else Qparams.add "aiprop" prop.Aiprop.keys)
    |> (match sort with
        | `Name -> ident
        | `Timestamp -> Qparams.add "aisort" "timestamp")
    |> (match dir with
        | `Ascending -> ident
        | `Descending -> Qparams.add "aidir" "descending"
        | `Newer -> Qparams.add "aidir" "newer"
        | `Older -> Qparams.add "aidir" "older")
    |> Option.fold (Qparams.add "aifrom") start
    |> Option.fold (Qparams.add "aistop") stop
    |> Option.fold (Qparams.add "aicontinue") continue
    |> Option.fold (Qparams.add "aistart" % string_of_caltime) tstart
    |> Option.fold (Qparams.add "aiend" % string_of_caltime) tstop
    |> Option.fold (Qparams.add "aiprefix") prefix
    |> Option.fold (Qparams.add "aiminsize" % string_of_int) minsize
    |> Option.fold (Qparams.add "aimaxsize" % string_of_int) maxsize
    |> Option.fold (function `Hex s -> Qparams.add "aisha1" s
                           | `Base36 s -> Qparams.add "aisha1base36" s) sha1
    |> Option.fold (Qparams.add "aiuser") user
    |> (match filterbots with
        | `All -> ident
        | `Bots -> Qparams.add "aifilterbots" "bots"
        | `Nobots -> Qparams.add "aifilterbots" "nobots")
    |> Option.fold (Qparams.add "aimime") mime
    |> Option.fold (Qparams.add "ailimit" % string_of_int) limit in
  let lq_decode =
    let open Kojson_pattern in
    "allimages"^:
      K.list begin
        K.assoc begin
          "name"^: K.string %> fun ai_name ->
          "ns"^: K.int %> fun ai_ns ->
          "title"^: K.string %> fun ai_title ->
          prop.Aiprop.decode %> fun (ai_prop, jain) ->
          Ka.stop {ai_name; ai_ns; ai_title; ai_prop} jain
        end
      end %> pair in
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
        "pageid"^: K.int %> fun ap_pageid ->
        "ns"^: K.int %> fun ap_ns ->
        "title"^: K.string %> fun ap_title ->
        Ka.stop {ap_pageid; ap_ns; ap_title}
      end
    end %> pair

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
    |> Option.fold (Qparams.add "apnamespace" % string_of_int) namespace
    |> Option.fold (Qparams.add "apfilterredir")
        (match filterredir with
          | `All -> None
          | `Redirects -> Some "redirects"
          | `Nonredirects -> Some "nonredirects")
    |> Option.fold (Qparams.add "apminsize" % string_of_int) minsize
    |> Option.fold (Qparams.add "apmaxsize" % string_of_int) maxsize
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
    |> Option.fold (Qparams.add "aplimit" % string_of_int) limit
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
