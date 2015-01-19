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

open Kojson_pattern
open Mwapi_utils
open Unprime
open Unprime_option

module Rvprop = struct

  module Scheme (T : sig type 'a t end) = struct
    type ('ids, 'flags, 'timestamp, 'user, 'userid, 'size, 'sha1,
	  'contentmodel, 'comment, 'parsedcomment, 'content, 'tags) t =
    {
      ids: 'ids T.t;
      flags: 'flags T.t;
      timestamp: 'timestamp T.t;
      user: 'user T.t;
      userid: 'userid T.t;
      size: 'size T.t;
      sha1: 'sha1 T.t;
      contentmodel: 'contentmodel T.t;
      comment: 'comment T.t;
      parsedcomment: 'parsedcomment T.t;
      content: 'content T.t;
      tags: 'tags T.t;
    }
  end

  module type SCHEME = sig
    type 'a mode
    include module type of Scheme (struct type 'a t = 'a mode end)
  end

  module Reply = struct
    type ids = {revid: int; parentid: int}
    type flags = {minor: bool}
    type content = {contentformat: string; contentbody: string}
    include Scheme (struct type 'a t = 'a end)
  end

  module Request = struct
    open Reply

    type 'a requested = bool * (Kojson.jain -> 'a * Kojson.jain)
    include Scheme (struct type 'a t = 'a requested end)

    let decode_unit jain = (), jain

    let none = {
      ids = false, decode_unit;
      flags = false, decode_unit;
      timestamp = false, decode_unit;
      user = false, decode_unit;
      userid = false, decode_unit;
      size = false, decode_unit;
      sha1 = false, decode_unit;
      contentmodel = false, decode_unit;
      comment = false, decode_unit;
      parsedcomment = false, decode_unit;
      content = false, decode_unit;
      tags = false, decode_unit;
    }

    let decode f name = name^: f *> pair

    let decode_ids =
      "revid"^: K.int *> fun revid ->
      "parentid"^: K.int *> fun parentid ->
      pair {revid; parentid}

    let decode_flags = "minor"^?: fun o -> pair {minor = o <> None}

    let decode_content =
      "contentformat"^: K.string *> fun contentformat ->
      "*"^: K.string *> fun contentbody ->
      pair {contentformat; contentbody}

    let ids = true, decode_ids
    let flags = true, decode_flags
    let timestamp = true, decode (K.convert_string "time" caltime_of_string)
				 "timestamp"
    let user = true, decode K.string "user"
    let userid = true, decode K.int "userid"
    let size = true, decode K.int "size"
    let sha1 = true, decode K.string "sha1"
    let contentmodel = true, decode K.string "contentmodel"
    let comment = true, decode K.string "comment"
    let parsedcomment = true, decode K.string "parsedcomment"
    let content = true, decode_content
    let tags = true, decode (K.list K.string) "tags"

    let to_string r =
      let buf = Buffer.create 256 in
      let add s = Buffer.add_string buf s; Buffer.add_char buf '|' in
      if fst r.ids then add "ids";
      if fst r.flags then add "flags";
      if fst r.timestamp then add "timestamp";
      if fst r.user then add "user";
      if fst r.userid then add "userid";
      if fst r.size then add "size";
      if fst r.sha1 then add "sha1";
      if fst r.contentmodel then add "contentmodel";
      if fst r.comment then add "comment";
      if fst r.parsedcomment then add "parsedcomment";
      if fst r.content then add "content";
      if fst r.tags then add "tags";
      Buffer.sub buf 0 (Buffer.length buf - 1)

    let decode r jain =
      let ids, jain = snd r.ids jain in
      let flags, jain = snd r.flags jain in
      let timestamp, jain = snd r.timestamp jain in
      let user, jain = snd r.user jain in
      let userid, jain = snd r.userid jain in
      let size, jain = snd r.size jain in
      let sha1, jain = snd r.sha1 jain in
      let contentmodel, jain = snd r.contentmodel jain in
      let comment, jain = snd r.comment jain in
      let parsedcomment, jain = snd r.parsedcomment jain in
      let content, jain = snd r.content jain in
      let tags, jain = snd r.tags jain in
      let open Reply in
      { ids; flags; timestamp; user; userid; size; sha1;
	contentmodel; comment; parsedcomment; content; tags }, jain
  end
end

let revisions
	?limit
	?startid
	?stopid
	?tstart
	?tstop
	?(dir = `Older)
	?user
	?excludeuser
	?tag
	?(expandtemplates = false)
	?(generatexml = false)
	?(parse = false)
	?section
	?continue
	?diffto
	?contentformat
	rvprop =
  let prop_params = Qparams.singleton "prop" "revisions"
    |> Qparams.add "rvprop" (Rvprop.Request.to_string rvprop)
    |> Option.fold (Qparams.add "rvlimit" *< string_of_int) limit
    |> Option.fold (Qparams.add "rvstartid" *< string_of_int) startid
    |> Option.fold (Qparams.add "rvendid" *< string_of_int) stopid
    |> Option.fold (Qparams.add "rvstart" *< string_of_caltime) tstart
    |> Option.fold (Qparams.add "rvend" *< string_of_caltime) tstop
    |> (match dir with `Newer -> Qparams.add "rvdir" "newer" | `Older -> ident)
    |> Option.fold (Qparams.add "rvuser") user
    |> Option.fold (Qparams.add "rvexcludeuser") excludeuser
    |> Option.fold (Qparams.add "rvtag") tag
    |> (if expandtemplates then Qparams.add "rvexpandtemplates" "" else ident)
    |> (if generatexml then Qparams.add "rvgeneratexml" "" else ident)
    |> (if parse then Qparams.add "rvparse" "" else ident)
    |> Option.fold (Qparams.add "rvsection") section
    |> Option.fold (Qparams.add "rvcontinue") continue
    |> Option.fold
	(function
	  | `Id id -> Qparams.add "rvdiffto" (string_of_int id)
	  | `Prev -> Qparams.add "rvdiffto" "prev"
	  | `Next -> Qparams.add "rvdiffto" "next"
	  | `Cur -> Qparams.add "rvdiffto" "cur"
	  | `Text s -> Qparams.add "rvdifftotext" s) diffto
    |> Option.fold (Qparams.add "rvcontentformat") contentformat in
  let prop_decode =
    "revisions"^:
      K.list begin
	K.assoc begin fun jain ->
	  let reply, jain = Rvprop.Request.decode rvprop jain in
	  (* Request for content implies contentmodel. *)
	  Ka.stop reply (Ka.drop ["contentmodel"] jain)
	end
      end *> pair in
  let prop_decode_missing jain = (), jain in
  Mwapi_query_prop.({prop_params; prop_decode; prop_decode_missing})
