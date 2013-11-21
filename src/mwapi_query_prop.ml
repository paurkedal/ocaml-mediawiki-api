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

open Kojson_pattern
open Mwapi_query
open Mwapi_utils
open Unprime
open Unprime_list
open Unprime_option

type ('a, 'k) prop = {
  prop_params : Qparams.t;
  prop_decode : Kojson.jain -> 'a * Kojson.jain;
}
type 'a nprop = ('a, [`N]) prop
type 'a gprop = ('a, [`G]) prop

let (&) a b =
  let prop_params = Qparams.merge a.prop_params b.prop_params in
  let prop_decode jain =
    let x, jain = a.prop_decode jain in
    let y, jain = b.prop_decode jain in
    ((x, y), jain) in
  {prop_params; prop_decode}

let possibly_redundant_labels = [
  (* prop=info is implied by inprop_- and intoken_-queries. *)
  "touched"; "starttimestamp"; "length"; "lastrevid"; "counter";
  "redirect"; "new";
]

let decode_pages decode_prop =
  "pages"^:
    K.assoc (Ka.map begin fun _ ->
      K.assoc begin
	"title"^: K.string *> fun title ->
	"invalid"^?:
	  begin function
	  | None ->
	    "ns"^: K.int *> fun ns ->
	    "missing"^?:
	      begin function
	      | None ->
		"pageid"^: K.int *> fun pageid ->
		decode_prop *> fun (prop, rest) ->
		Ka.stop (`Present (title, ns, pageid, prop))
			(Ka.drop possibly_redundant_labels rest)
	      | Some _ ->
		Ka.stop (`Missing (title, ns))
	      end
	  | Some _ -> Ka.stop (`Invalid title)
	  end
      end
    end) *> pair

let for_titles titles prop =
  let pq_params = List.fold (Qparams.add "titles") titles prop.prop_params in
  {pq_params; pq_decode = decode_pages prop.prop_decode}

let for_pageids pageids prop =
  let pq_params =
    List.fold (Qparams.add "pageids") (List.map string_of_int pageids)
	      prop.prop_params in
  {pq_params; pq_decode = decode_pages prop.prop_decode}


(* prop=info *)

type info = {
  in_touched : string;
  in_lastrevid : int;
  in_counter : int;
  in_length : int;
  in_redirect : bool;
  in_new : bool;
}
let info =
  let prop_decode =
    "touched"^: K.string *> fun in_touched ->
    "lastrevid"^: K.int *> fun in_lastrevid ->
    "counter"^: K.int *> fun in_counter ->
    "length"^: K.int *> fun in_length ->
    "redirect"^?: fun in_redirect ->
    "new"^?: fun in_new ->
    let in_redirect = in_redirect <> None in
    let in_new = in_new <> None in
    pair {in_touched; in_lastrevid; in_counter; in_length;
	  in_redirect; in_new} in
  {prop_params = Qparams.singleton "prop" "info"; prop_decode}

let make_inprop inprop decode = {
  prop_params = Qparams.of_list ["prop", "info"; "inprop", inprop];
  prop_decode = inprop^: decode *> pair;
}

type protection = {
  protection_type : [`Edit | `Move];
  protection_level : [`Autoconfirmed | `Sysop];
  protection_expiry : string;
}
let decode_protection =
  K.assoc begin
    "type"^: K.string_enum ["edit", `Edit; "move", `Move]
			*> fun protection_type ->
    "level"^: K.string_enum ["sysop", `Sysop; "autoconfirmed", `Autoconfirmed]
			*> fun protection_level ->
    "expiry"^: K.string *> fun protection_expiry ->
    Ka.stop {protection_type; protection_level; protection_expiry}
  end
let inprop_protection = make_inprop "protection" decode_protection
let inprop_talkid = make_inprop "talkid" K.int

type urls = {
  fullurl : string;
  editurl : string;
}
let inprop_url =
  let prop_decode =
    "fullurl"^: K.string *> fun fullurl ->
    "editurl"^: K.string *> fun editurl ->
    pair {fullurl; editurl} in
  {prop_params = Qparams.of_list ["prop", "info"; "inprop", "url"]; prop_decode}

let make_intoken which = {
  prop_params = Qparams.of_list ["prop", "info"; "intoken", which];
  prop_decode = (which ^ "token")^: K.string *> pair;
}
let intoken_edit = make_intoken "edit"
let intoken_delete = make_intoken "delete"
let intoken_protect = make_intoken "protect"
let intoken_move = make_intoken "move"
let intoken_block = make_intoken "block"
let intoken_unblock = make_intoken "unblock"
let intoken_email = make_intoken "email"
let intoken_import = make_intoken "import"
let intoken_watch = make_intoken "watch"
