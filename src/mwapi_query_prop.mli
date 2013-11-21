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

open Mwapi_query
open Mwapi_utils

type ('a, 'k) prop = {
  prop_params : Qparams.t;
  prop_decode : Kojson.jain -> 'a * Kojson.jain;
}

type 'a gprop = ('a, [`G]) prop
(** A property which can be used as a generator. *)

type 'a nprop = ('a, [`N]) prop
(** A property which cannot be used as a generator. *)

val (&) : ('a, 'ak) prop -> ('b, 'bk) prop -> ('a * 'b) nprop

val for_titles : string list -> ('a, 'k) prop -> ('a, 'k) page_query
val for_pageids : int list -> ('a, 'k) prop -> ('a, 'k) page_query
(*
val for_revids : int list -> ('a, 'k) prop -> ('a, 'k) page_query
val for_list : ('b, [< `G]) list_query -> ('a, 'k) prop ->
	       ('a, [`N]) page_query
val for_prop : ('b, [< `G]) page_query -> ('a, 'k) prop ->
	       ('a, [`N]) page_query
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
val info : info nprop

(** {3 inprop} *)

type protection = {
  protection_type : [`Edit | `Move];
  protection_level : [`Autoconfirmed | `Sysop];
  protection_expiry : string;
}
val inprop_protection : protection nprop
val inprop_talkid : int nprop
type urls = {
  fullurl : string;
  editurl : string;
}
val inprop_url : urls nprop

(** {3 intoken} *)

val intoken_edit : string nprop
val intoken_delete : string nprop
val intoken_protect : string nprop
val intoken_move : string nprop
val intoken_block : string nprop
val intoken_unblock : string nprop
val intoken_email : string nprop
val intoken_import : string nprop
val intoken_watch : string nprop
