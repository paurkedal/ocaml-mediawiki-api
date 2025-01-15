(* Copyright (C) 2025  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

open Cohttp.Cookie
open Lwt.Infix
module Sexp = Sexplib.Sexp

let rec write_lines oc = function
 | [] -> Lwt.return ()
 | line :: lines ->
    Lwt_io.write oc (line ^ "\n") >>= fun () ->
    write_lines oc lines

(* Should store time and check Max_age here, but MW probably only use session
 * cookies for the API.  To do this right, customize the cookie type above to
 * include absolute time of expiration or creation, and maybe remove some
 * redundant fields. *)

let write ~origin oc by_origin =
  let aux sch =
    (match sch.Set_cookie_hdr.expiration with
     | `Session -> Fun.id
     | `Max_age _t -> List.cons (Sexp.to_string (Set_cookie_hdr.sexp_of_t sch)))
  in
  write_lines oc (Mwapi_cookiejar.fold ~origin aux by_origin [])

let read ~origin ic by_origin =
  let Sink add_cookie = Mwapi_cookiejar.populate origin by_origin in
  let add_cookie_line line =
    let sch = Set_cookie_hdr.t_of_sexp (Sexp.of_string line) in
    (match sch.Set_cookie_hdr.expiration with
     | `Session -> ()
     | `Max_age _t -> add_cookie sch)
  in
  Lwt_stream.iter add_cookie_line (Lwt_io.read_lines ic)
