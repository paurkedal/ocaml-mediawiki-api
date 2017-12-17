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

open Mwapi_query
open Mwapi_common

type ('a, 'am, 'k) prop = {
  prop_params : Qparams.t;
  prop_decode : Kojson.jain -> 'a * Kojson.jain;
  prop_decode_missing : Kojson.jain -> 'am * Kojson.jain;
}

type ('a, 'am) gprop = ('a, 'am, [`G]) prop
(** A property which can be used as a generator. *)

type ('a, 'am) nprop = ('a, 'am, [`N]) prop
(** A property which cannot be used as a generator. *)

val (&) : ('a, 'am, 'ak) prop ->
          ('b, 'bm, 'bk) prop -> ('a * 'b, 'am * 'bm) nprop

val for_titles : string list -> ('a, 'am, 'k) prop -> ('a, 'am, 'k) page_query
val for_pageids : int list -> ('a, 'am, 'k) prop -> ('a, 'am, 'k) page_query
val for_revids : int list -> ('a, 'am, 'k) prop -> ('a, 'am, 'k) page_query
(*
val for_list : ('b, 'bm, [< `G]) list_query -> ('a, 'am, 'k) prop ->
               ('a, 'am, [`N]) page_query
val for_prop : ('b, 'bm, [< `G]) page_query -> ('a, 'am, 'k) prop ->
               ('a, 'am, [`N]) page_query
*)


(** {2 prop=info} *)

type info = {
  in_touched : string;
  in_lastrevid : int;
  in_counter : int;
  in_length : int;
  in_redirect : bool;
  in_new : bool;
}
val info : (info, unit) nprop

(** {3 inprop} *)

type protection = {
  protection_type : [`Edit | `Move];
  protection_level : [`Autoconfirmed | `Sysop];
  protection_expiry : string;
}
val inprop_protection : (protection, unit) nprop
val inprop_talkid : (int, unit) nprop
type urls = {
  fullurl : string;
  editurl : string;
}
val inprop_url : (urls, unit) nprop

(** {3 intoken} *)

val intoken_edit : (string, string) nprop
val intoken_delete : (string, unit) nprop
val intoken_protect : (string, unit) nprop
val intoken_move : (string, unit) nprop
val intoken_block : (string, unit) nprop
val intoken_unblock : (string, unit) nprop
val intoken_email : (string, unit) nprop
val intoken_import : (string, unit) nprop
val intoken_watch : (string, unit) nprop
