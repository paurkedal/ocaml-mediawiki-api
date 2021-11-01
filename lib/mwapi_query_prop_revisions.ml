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

open Kojson_pattern
open Mwapi_common
open Mwapi_prereq
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
    type slots = content String_map.t
    include Scheme (struct type 'a t = 'a end)
  end

  module Request = struct
    open Reply

    type 'a requested = string option * (Kojson.jain -> 'a * Kojson.jain)
    include Scheme (struct type 'a t = 'a requested end)

    let decode_unit jain = (), jain

    let none = {
      ids = None, decode_unit;
      flags = None, decode_unit;
      timestamp = None, decode_unit;
      user = None, decode_unit;
      userid = None, decode_unit;
      size = None, decode_unit;
      sha1 = None, decode_unit;
      contentmodel = None, decode_unit;
      comment = None, decode_unit;
      parsedcomment = None, decode_unit;
      content = None, decode_unit;
      tags = None, decode_unit;
    }

    let decode f name = name^: f %> pair

    let decode_ids =
      "revid"^: K.int %> fun revid ->
      "parentid"^: K.int %> fun parentid ->
      pair {revid; parentid}

    let decode_flags = "minor"^?: fun o -> pair {minor = o <> None}

    let decode_content_slot =
      K.assoc begin
        "contentformat"^: K.string %> fun contentformat ->
        "*"^: K.string %> fun contentbody ->
        Ka.drop ["contentmodel"] %> Ka.stop {contentformat; contentbody}
      end

    let decode_content =
      "slots"^:
        K.assoc begin fun jain ->
          Ka.fold
            (fun k jin -> String_map.add k (decode_content_slot jin))
            jain String_map.empty
        end %> fun slots ->
      pair slots

    let ids = Some "ids", decode_ids
    let flags = Some "flags", decode_flags
    let timestamp = Some "timestamp",
      decode (K.convert_string "time" caltime_of_string) "timestamp"
    let user = Some "user", decode K.string "user"
    let userid = Some "userid", decode K.int "userid"
    let size = Some "size", decode K.int "size"
    let sha1 = Some "sha1", decode K.string "sha1"
    let contentmodel = Some "contentmodel", decode K.string "contentmodel"
    let comment = Some "comment", decode K.string "comment"
    let parsedcomment = Some "parsedcomment", decode K.string "parsedcomment"
    let content = Some "content", decode_content
    let tags = Some "tags", decode (K.list K.string) "tags"

    let to_string r =
      let buf = Buffer.create 256 in
      let add s = Buffer.add_string buf s; Buffer.add_char buf '|' in
      Option.iter add (fst r.ids);
      Option.iter add (fst r.flags);
      Option.iter add (fst r.timestamp);
      Option.iter add (fst r.user);
      Option.iter add (fst r.userid);
      Option.iter add (fst r.size);
      Option.iter add (fst r.sha1);
      Option.iter add (fst r.contentmodel);
      Option.iter add (fst r.comment);
      Option.iter add (fst r.parsedcomment);
      Option.iter add (fst r.content);
      Option.iter add (fst r.tags);
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
        ?(slots = ["*"])
        ?contentformat
        rvprop =
  let prop_params = Qparams.singleton "prop" "revisions"
    |> Qparams.add "rvprop" (Rvprop.Request.to_string rvprop)
    |> Qparams.add "rvslots" (String.concat "|" slots)
    |> Option.fold (Qparams.add "rvlimit" % string_of_int) limit
    |> Option.fold (Qparams.add "rvstartid" % string_of_int) startid
    |> Option.fold (Qparams.add "rvendid" % string_of_int) stopid
    |> Option.fold (Qparams.add "rvstart" % string_of_caltime) tstart
    |> Option.fold (Qparams.add "rvend" % string_of_caltime) tstop
    |> (match dir with `Newer -> Qparams.add "rvdir" "newer" | `Older -> ident)
    |> Option.fold (Qparams.add "rvuser") user
    |> Option.fold (Qparams.add "rvexcludeuser") excludeuser
    |> Option.fold (Qparams.add "rvtag") tag
    |> (if expandtemplates then Qparams.add "rvexpandtemplates" "" else ident)
    |> (if generatexml then Qparams.add "rvgeneratexml" "" else ident)
    |> (if parse then Qparams.add "rvparse" "" else ident)
    |> Option.fold (Qparams.add "rvsection" % string_of_int) section
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
      end %> pair in
  let prop_decode_missing jain = (), jain in
  Mwapi_query_prop.({prop_params; prop_decode; prop_decode_missing})
