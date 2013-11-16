(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
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
open Unprime_list

let xdg_cookie_path ~domain =
  let cache_dir = XDGBaseDir.Cache.user_dir () in
  let fn = domain ^ ".cookies" in
  List.fold_right Filename.concat [cache_dir; "ocaml-mediawikiapi"] fn

module type S = sig
  module IO : Cohttp.IO.S

  type t

  val create : unit -> t
  val extract : Cohttp.Header.t -> t -> unit
  val write : IO.oc -> t -> unit IO.t
  val read : IO.ic -> t -> unit IO.t
end

module Make (IO : Cohttp.IO.S) = struct

  module IO = IO

  type t = (string, Set_cookie_hdr.t) Hashtbl.t

  let create () = Hashtbl.create 7

  let extract h jar =
    List.iter (fun (k, c) -> Hashtbl.replace jar k c) (Set_cookie_hdr.extract h)

  let write oc jar =
    let open IO in
    Hashtbl.fold
      (fun _ c acc ->
	acc >>
	write oc (snd (Set_cookie_hdr.serialize c)) >>
	write oc "\n")
      jar (return ())

  let read ic jar =
    let open IO in
    let rec loop acc =
      read_line ic >>= function
      | Some line -> loop (("set-cookie", line) :: acc)
      | None -> return acc in
    loop [] >>= fun hdrlines ->
    List.iter (fun (k, c) -> Hashtbl.replace jar k c)
	      (Set_cookie_hdr.extract (Cohttp.Header.of_list hdrlines));
    return ()

end
