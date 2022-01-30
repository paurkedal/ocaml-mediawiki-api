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

open Mwapi_common
open Mwapi_prereq
open Mwapi_query

type token_type =
  [ `Csrf
  | `Watch
  | `Patrol
  | `Rollback
  | `Userrights
  | `Login
  | `Createaccount ]

let string_of_token_type = function
  | `Csrf -> "csrf"
  | `Watch -> "watch"
  | `Patrol -> "patrol"
  | `Rollback -> "rollback"
  | `Userrights -> "userrights"
  | `Login -> "login"
  | `Createaccount -> "createaccount"

let tokens tokens =
  let mq_params = Qparams.empty
    |> Qparams.add "meta" "tokens"
    |> Qparams.add "type"
        (String.concat "|"
          (List.map string_of_token_type (Nlist.to_list tokens))) in
  let extract m =
    Nlist.map
      (fun token -> String_map.find (string_of_token_type token ^ "token") m)
      tokens in
  let mq_decode =
    let open Kojson_pattern in
    let aux k v = String_map.add k (K.string v) in
    "tokens"^:
      K.assoc (fun ka -> extract (Ka.fold aux ka String_map.empty)) %> fun m ->
    fun jain -> (m, jain)
  in
  {mq_params; mq_decode}

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

module Siprop = struct
  type 'a t = {
    keys: String_set.t;
    decode: Kojson.jain -> 'a * Kojson.jain;
  }

  let (&) p1 p2 =
    let decode jain =
      let x1, jain = p1.decode jain in
      let x2, jain = p2.decode jain in
      ((x1, x2), jain)
    in
    {keys = String_set.union p1.keys p2.keys; decode}

  let namespaces =
    let decode =
      let open Kojson_pattern in
      let case = K.string_enum [
        "first-letter", `First_letter;
        "case-sensitive", `Case_sensitive
      ] in
      "namespaces"^: K.assoc @@ Ka.map begin fun _id ->
        K.assoc begin
          "id"^: K.int %> fun id ->
          "case"^: case %> fun case ->
          "subpages"^?: fun subpages ->
          "canonical"^?: Option.map K.string %> fun canonical ->
          "defaultcontentmodel"^?:
            Option.map K.string %> fun defaultcontentmodel ->
          "*"^: K.string %> fun name ->
          let subpages = subpages <> None in
          Ka.stop {id; case; subpages; canonical; defaultcontentmodel; name}
        end
      end %> pair
    in
    {keys = String_set.singleton "namespaces"; decode}

  let namespacealiases =
    let decode =
      let open Kojson_pattern in
      "namespacealiases"^: K.list begin
        K.assoc begin
          "id"^: K.int %> fun id ->
          "*"^: K.string %> fun name ->
          Ka.stop {id; name}
        end
      end %> pair
    in
    {keys = String_set.singleton "namespacealiases"; decode}

  let usergroups =
    let decode =
      let open Kojson_pattern in
      "usergroups"^: K.list begin
        K.assoc begin
          "name"^: K.string %> fun name ->
          "rights"^: K.list K.string %> fun rights ->
          Ka.stop {name; rights}
        end
      end %> pair
    in
    {keys = String_set.singleton "usergroups"; decode}

  let extensions =
    let decode =
      let open Kojson_pattern in
      "extensions"^: K.list begin
        K.assoc begin
          "type"^: K.string %> fun type_ ->
          "name"^: K.string %> fun name ->
          "namemsg"^: K.string %> fun namemsg ->
          "version"^?: Option.map K.string %> fun version ->
          "descriptionmsg"^: K.string %> fun descriptionmsg ->
          "author"^: K.string %> fun author ->
          "url"^?: Option.map K.string %> fun url ->
          "license_name"^?: Option.map K.string %> fun license_name ->
          "license"^?: Option.map K.string %> fun license ->
          Ka.stop {
            type_; name; namemsg; version; descriptionmsg; author; url;
            license_name; license;
          }
        end
      end %> pair
    in
    {keys = String_set.singleton "extensions"; decode}
end

let siteinfo prop =
  let mq_params = Qparams.singleton "meta" "siteinfo"
    |> Qparams.add "siprop"
        (String.concat "|" (String_set.elements prop.Siprop.keys))
  in
  {mq_params; mq_decode = prop.Siprop.decode}
