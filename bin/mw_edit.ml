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

open Mwapi_utils
open Printf
open Unprime
open Unprime_list
open Unprime_option
open Unprime_string
open Utils

let find_section level tt =
  let open Mwapi_parse in
  List.search
    begin fun {section_level; section_line; section_index} ->
      if section_line <> tt || section_level <> level then None
      else Some (int_of_string section_index)
    end

let new_section level title =
  let markup = String.make level '=' in
  [Template.Text (sprintf "\n\n%s %s %s\n\n" markup title markup);
   Template.Stencil ("init", None)]

let find_insertion_point level title ipt sections_result =
  match ipt with
  | `Before tt ->
    Option.map
      (fun i -> i, new_section level title, (fun s -> `Prepend s))
      (find_section level tt sections_result)
  | `After tt ->
    Option.map
      (fun i -> i, new_section level title, (fun s -> `Append s))
      (find_section level tt sections_result)

let edit ~page ~level ~section ?(insertion_points = []) subst mw =
  lwt r = Mwapi_lwt.call Mwapi_parse.(parse ~page sections) mw in
  lwt i, tmpl, make_op =
    match find_section level section r with
    | Some i ->
      Mwapi_lwt.call Mwapi_parse.(parse ~page ~section:i wikitext) mw
	>|= fun s ->
      (i, Template.of_string s, (fun s -> `Replace s))
    | None ->
      begin match
	List.search (fun ipt -> find_insertion_point level section ipt r)
		    insertion_points
      with
      | Some iop -> Lwt.return iop
      | None ->
	Lwt.fail (Failure "Could not locate section to edit or place to \
			   insert it.")
      end in
  let tmpl = Template.subst_map subst tmpl in
  lwt token = Utils.get_edit_token ~page mw in
  let op = make_op (Template.to_string tmpl) in
  Utils.call_edit Mwapi_edit.(edit ~token ~page ~section:(`No i) ~op ()) mw

let subst_of_arg s =
  match String.cut_affix "=" s with
  | None -> raise (Arg.Bad "Missing equality sign in -s argument")
  | Some (x, s) -> (x, Template.of_string s)

let uri_of_arg s =
  try Uri.of_string s
  with Invalid_argument _ -> raise (Arg.Bad "Malformed URI")

let set_option opt arg = opt := Some arg

let bsnl_rex = Pcre.regexp "\\\\\n"

let load_template dirs x =
  let fn = x ^ ".tmpl" in
  let rec loop = function
    | [] -> fail_f "Cannot find template %s." x
    | dir :: dirs ->
      let fp = Filename.concat dir fn in
      begin
	try_lwt
	  lwt st = Lwt_unix.stat fp in
	  Lwt_io.with_file Lwt_io.input fp
	    (fun ic ->
	      let s = String.create st.Unix.st_size in
	      Lwt_io.read_into_exactly ic s 0 st.Unix.st_size >>
	      Lwt.return
		(Template.of_string (Pcre.replace ~rex:bsnl_rex ~templ:"" s)))
	with Unix.Unix_error (Unix.ENOENT, _, _) -> loop dirs
      end in
  loop dirs

let () =
  let opt_api = ref None in
  let opt_cert = ref None in
  let opt_certkey = ref None in
  let opt_page = ref None in
  let opt_level = ref 2 in
  let opt_section = ref None in
  let opt_ipts = ref [] in
  let opt_subst = ref [] in
  let opt_includes = ref [] in
  let arg_usage = "mw-edit <options> text" in
  let arg_specs = Arg.align [
    "-api", Arg.String (fun s -> opt_api := Some (uri_of_arg s)),
      "<url> URL of the MediaWiki API.";
    "-cert", Arg.String (set_option opt_cert), "<file> Client certificate.";
    "-certkey", Arg.String (set_option opt_certkey), "<file> Private key.";
    "-page", Arg.String (set_option opt_page),
      "<title> The title of the page to edit.";
    "-level", Arg.Set_int opt_level,
      "<level> The level of the target section specified as the number of \
	       equality signs in the corresponding MediaWiki delimiters. \
	       The default level is 2, e.g. the normal section top-level. \
	       This applies to -section, -add-after, and -add-before.";
    "-section", Arg.String (set_option opt_section),
      "<title> The title of the target section.";
    "-add-after", Arg.String (fun tt -> opt_ipts := `After tt :: !opt_ipts),
      "<title> If the target section is not found, add it after <title>.";
    "-add-before", Arg.String (fun tt -> opt_ipts := `Before tt :: !opt_ipts),
      "<title> If the target section is not found, add it before <title>.";
    "-s", Arg.String (fun s -> opt_subst := `Set(subst_of_arg s) :: !opt_subst),
      "<x>=<tmpl> Substitute <tmpl> for <x>.";
    "-l", Arg.String (fun s -> opt_subst := `Load s :: !opt_subst),
      "<x> Load <x>.tmpl from the include path and substitute it for <x>.";
    "-I", Arg.String (fun s -> opt_includes := s :: !opt_includes),
      "<dir> Add <dir> to the include path.";
  ] in
  let misuse msg = eprintf "%s\n" msg; exit 64 in
  let mandatory name opt =
    match !opt with
    | None -> misuse (sprintf "The %s option is mandatory." name)
    | Some section -> section in
  Arg.parse arg_specs
	    (fun _ -> misuse "Not expecting positional arguments.") arg_usage;
  let api = mandatory "-api" opt_api in
  let page = mandatory "-page" opt_page in
  let section = mandatory "-section" opt_section in

  Option.iter (uncurry Mwapi_lwt.use_certificate)
    begin match !opt_cert, !opt_certkey with
    | None, None -> None
    | Some cert, Some certkey -> Some (cert, certkey)
    | Some certkey, None -> Some (certkey, certkey)
    | None, Some _ -> misuse "-certkey is required when using -cert"
    end;

  Lwt_main.run begin
    lwt mw = Mwapi_lwt.open_api api in
    lwt subst =
      Lwt_list.fold_left_s
	(fun subst ->
	  function
	  | `Load x ->
	    load_template !opt_includes x >|= fun tmpl ->
	    String_map.add x (Template.subst_map subst tmpl) subst
	  | `Set (x, tmpl) ->
	    Lwt.return (String_map.add x (Template.subst_map subst tmpl) subst))
	String_map.empty !opt_subst in
    edit ~page:(`Title page)
	 ~level:!opt_level ~section ~insertion_points:(List.rev !opt_ipts)
	 subst mw >>
    Mwapi_lwt.close_api mw
  end
