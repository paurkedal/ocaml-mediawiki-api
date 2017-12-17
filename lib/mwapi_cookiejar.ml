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

open Cohttp.Cookie
open Mwapi_utils
open Printf
open Unprime
open Unprime_char
open Unprime_list
open Unprime_option
open Unprime_string

(* Helpers *)

let is_subdomain sfx dom =
  let n = String.length sfx in
  let i = String.length dom - n in
  n = 0 || String.has_suffix sfx dom
            && (i = 0 || sfx.[0] = '.' || dom.[i - 1] = '.')

let is_subpath pfx path =
  let n = String.length pfx in
  n = 0 || String.has_prefix pfx path
            && (n = String.length path || path.[n - 1] = '/' || path.[n] = '/')

let uri_origin uri =
  let host =
    (match Uri.host uri with
     | Some host -> host
     | None -> failwith "The host part of the URL is required.") in
  let port =
    (match Uri.port uri with
     | Some port -> port
     | None ->
        (match Uri.scheme uri with
         | Some "http" -> 80
         | Some "https" -> 443
         | _ -> failwith "Unimplemented protocol for storing cookies.")) in
  (host, port)

type uri_info = {
  uri_origin : string * int;
  uri_is_http : bool;
  uri_is_secure : bool;
}

let uri_info uri =
  let uri_is_http, uri_is_secure =
    match Uri.scheme uri with
    | Some "http" -> true, false
    | Some "https" -> true, true
    | _ -> false, false in
  {uri_origin = uri_origin uri; uri_is_http; uri_is_secure}


(* The Works *)

type origin = string * int

type cookie = Set_cookie_hdr.t

type scope = {
  mutable is_modified : bool;
  mutable cookies : cookie String_map.t;
}

type t = (origin, (string, scope) Hashtbl.t) Hashtbl.t

let create () : t = Hashtbl.create 7

let rec get' ~is_http ~is_secure path by_path acc =
  let cookies =
    try (Hashtbl.find by_path path).cookies
    with Not_found -> String_map.empty in
  let cookies =
    if is_http && is_secure then cookies else
    String_map.filter
      (* TODO: Add is_http check when Set_cookie_hdr supports it. *)
      (fun _ cookie -> is_secure || not (Set_cookie_hdr.secure cookie))
      cookies in
  let acc' = String_map.left_union acc cookies in
  if path = "/" then acc' else
  get' ~is_http ~is_secure (Filename.dirname path) by_path acc'

let get uri by_origin =
  let {uri_origin = origin; uri_is_http = is_http; uri_is_secure = is_secure} =
    uri_info uri in
  try
    let by_path = Hashtbl.find by_origin origin in
    let cookies = get' ~is_http ~is_secure (Uri.path uri)
                       by_path String_map.empty in
    String_map.fold (fun _ cookie acc -> cookie :: acc) cookies []
  with Not_found -> []

let tread_origin origin by_origin =
  try Hashtbl.find by_origin origin
  with Not_found ->
    let by_path = Hashtbl.create 7 in
    Hashtbl.replace by_origin origin by_path;
    by_path

let tread_path path by_path =
  try Hashtbl.find by_path path
  with Not_found ->
    let scope = {is_modified = false; cookies = String_map.empty} in
    Hashtbl.replace by_path path scope;
    scope

let add uri cookie by_origin =
  let origin = uri_origin uri in
  let host = fst origin in
  let cookie_path = Option.get_or "/" (Set_cookie_hdr.path cookie) in
  let cookie_domain = Set_cookie_hdr.domain cookie in
  if Option.for_all (fun dom -> is_subdomain dom host) cookie_domain
        && is_subpath cookie_path (Uri.path uri) then begin
    let by_path = tread_origin origin by_origin in
    let scope = tread_path cookie_path by_path in
    let name, _ = Set_cookie_hdr.cookie cookie in
    scope.is_modified <- true;
    scope.cookies <- String_map.add name cookie scope.cookies
  end

let extract uri hdr by_origin =
  List.iter (fun (_, cookie) -> add uri cookie by_origin)
            (Set_cookie_hdr.extract hdr)

let header uri by_origin =
  let cookies = get uri by_origin in
  let comps =
    List.fold
      (fun cookie acc ->
        let k, v = Set_cookie_hdr.cookie cookie in
        (k ^ "=" ^ v) :: acc)
      cookies [] in
  ("Cookie", String.concat "; " comps)

let fold ?origin f by_origin =
  let fold_by_path =
    Hashtbl.fold
      (fun _ cj -> String_map.fold (fun _ cookie -> f cookie) cj.cookies) in
  match origin with
  | None ->
    Hashtbl.fold (fun _ -> fold_by_path) by_origin
  | Some origin ->
    try fold_by_path (Hashtbl.find by_origin origin)
    with Not_found -> ident

let iter ?origin f by_origin =
  let iter_by_path =
    Hashtbl.iter
      (fun _ cj -> String_map.iter (fun _ cookie -> f cookie) cj.cookies) in
  match origin with
  | None ->
    Hashtbl.iter (fun _ -> iter_by_path) by_origin
  | Some origin ->
    try iter_by_path (Hashtbl.find by_origin origin)
    with Not_found -> ()

module type S = sig
  module IO : Cohttp.S.IO

  val write : origin: origin -> IO.oc -> t -> unit IO.t
  val read : origin: origin -> IO.ic -> t -> unit IO.t
end

module Make (IO : Cohttp.S.IO) = struct

  module IO = IO

  let write ~origin oc by_origin =
    let open IO in
    fold ~origin
      (fun c acc ->
        acc >>= fun () ->
        write oc (snd (Set_cookie_hdr.serialize c)) >>= fun () ->
        write oc "\n")
      by_origin (return ())

  let read ~origin ic by_origin =
    let by_path = tread_origin origin by_origin in
    let open IO in
    let rec loop acc =
      read_line ic >>= function
      | Some line -> loop (("set-cookie", line) :: acc)
      | None -> return acc in
    loop [] >>= fun hdrlines ->
    List.iter
      (fun (name, cookie) ->
        let path = Option.get_or "/" (Set_cookie_hdr.path cookie) in
        let scope = tread_path path by_path in
        scope.cookies <- String_map.add name cookie scope.cookies)
      (Set_cookie_hdr.extract (Cohttp.Header.of_list hdrlines));
    return ()

end

let persistence_path ?(ext = "cookies") ~origin:(domain, port) () =
  let is_safechar c = Char.is_alnum c || c = '.' || Char.code c >= 128 in
  if domain = ".." || not (String.for_all is_safechar domain) then
    failwith_f "Unsafe character in domain \"%s\"." domain;
  let cache_dir = XDGBaseDir.Cache.user_dir () in
  let fn = sprintf "%s:%d.%s" domain port ext in
  List.fold_right Filename.concat [cache_dir; "ocaml-mediawiki-api"] fn
