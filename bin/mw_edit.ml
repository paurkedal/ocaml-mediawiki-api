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

open Lwt.Infix
open Mwapi
open Mwapi_common
open Printf
open Unprime_list
open Unprime_option
open Unprime_string
open Utils

type insertion_point =
  | Before_section of string
  | After_section of string
  | Bottom_of_section of string

type edit_target =
  | Edit_page
  | Edit_section of int * string * insertion_point list

let find_section level tt =
  let open Mwapi_parse in
  List.search
    begin fun {section_level; section_line; section_index; _} ->
      if section_line <> tt || section_level <> level then None
      else Some (int_of_string section_index)
    end

let new_page = [Template.Stencil ("init", None)]
let new_section level title =
  let markup = String.make level '=' in
  [Template.Text (sprintf "%s %s %s\n\n" markup title markup);
   Template.Stencil ("init", None)]

let break_after s =
  if String.length s = 0 || String.has_suffix "\n\n" s then s else
  if String.has_suffix "\n" s then s ^ "\n" else s ^ "\n\n"

let find_insertion_point level ipt sections_result =
  match ipt with
  | Before_section tt ->
    Option.map (fun i -> i, (fun s -> `Prepend (break_after s)))
               (find_section level tt sections_result)
  | After_section tt ->
    Option.map (fun i -> i, (fun s -> `Append ("\n\n" ^ s)))
               (find_section level tt sections_result)
  | Bottom_of_section tt ->
    Option.map (fun i -> i, (fun s -> `Append ("\n\n" ^ s)))
               (find_section (level - 1) tt sections_result)

let edit ?(do_replace = false) ~page ~target subst mw =
  let%lwt create, section, tmpl, make_op =
    match target with
    | Edit_section (secn_level, secn_title, ipts) ->
      let%lwt r = Mwapi_lwt.call Mwapi_parse.(parse ~page sections) mw in
      begin match find_section secn_level secn_title r with
      | Some i ->
        Mwapi_lwt.call Mwapi_parse.(parse ~page ~section:i wikitext) mw
          >|= fun s ->
        let tmpl = if do_replace then new_section secn_level secn_title
                                 else Template.of_string s in
        (`May_not, Some (`No i), tmpl, (fun s -> `Replace s))
      | None ->
        begin match
          List.search (fun ipt -> find_insertion_point secn_level ipt r) ipts
        with
        | Some (i, op) ->
          let tmpl = new_section secn_level secn_title in
          Lwt.return (`May_not, Some (`No i), tmpl, op)
        | None ->
          Lwt.fail (Failure "Could not locate section to edit or place to \
                             insert it.")
        end
      end
    | Edit_page ->
      try%lwt
        if do_replace then
          Lwt.return (`May, None, new_page, (fun s -> `Replace s))
        else
          Mwapi_lwt.call Mwapi_parse.(parse ~page wikitext) mw >|= fun s ->
          (`May_not, None, Template.of_string s, (fun s -> `Replace s))
      with Wiki_error {wiki_error_code = "missingtitle"; _} ->
        Lwt.return (`Must, None, new_page, (fun s -> `Replace s)) in
  let tmpl = Template.subst_map subst tmpl in
  let%lwt token = Utils.get_edit_token mw in
  let op = make_op (Template.to_string tmpl) in
  Utils.call_edit Mwapi_edit.(edit ~token ~page ~create ?section ~op ()) mw

let subst_of_arg s =
  match String.cut_affix "=" s with
  | None -> raise (Arg.Bad "Missing equality sign in -s argument")
  | Some (x, s) -> (x, Template.of_string s)

let uri_of_arg s =
  try Uri.of_string s
  with Invalid_argument _ -> raise (Arg.Bad "Malformed URI")

let set_option opt arg = opt := Some arg

let set_login opt arg =
  match String.cut_affix ":" arg with
  | Some p -> opt := Some p
  | _ -> raise (Arg.Bad "Expecting <username>:<password>.")

let bsnl_rex = Pcre.regexp "\\\\\n"
let template_of_string s =
  Template.of_string (Pcre.replace ~rex:bsnl_rex ~templ:"" s)

let load_template_stdin () = Lwt_io.read Lwt_io.stdin >|= template_of_string

let load_template_from fp =
  let%lwt st = Lwt_unix.stat fp in
  Lwt_io.with_file ~mode:Lwt_io.input fp
    (fun ic ->
      let s = Bytes.create st.Unix.st_size in
      Lwt_io.read_into_exactly ic s 0 st.Unix.st_size >>
      Lwt.return (template_of_string (Bytes.to_string s)))

let load_template dirs x =
  let fn = x ^ ".tmpl" in
  let rec loop = function
    | [] -> fail_f "Cannot find template %s." x
    | dir :: dirs ->
      let fp = Filename.concat dir fn in
      begin
        try%lwt load_template_from fp
        with Unix.Unix_error (Unix.ENOENT, _, _) -> loop dirs
      end in
  loop dirs

let loadspec_of_arg arg =
  match String.cut_affix "=" arg with
  | None -> `Load arg
  | Some (x, fp) -> `Load_from (x, fp)

let () =
  let opt_api = ref None in
  let opt_cert = ref None in
  let opt_certkey = ref None in
  let opt_login = ref None in
  let opt_page = ref None in
  let opt_level = ref None in
  let opt_section = ref None in
  let opt_replace = ref false in
  let opt_ipts = ref [] in
  let opt_subst = ref [] in
  let opt_includes = ref [] in
  let arg_usage = "mw-edit <options> text" in
  let arg_specs = Arg.align [
    "-api", Arg.String (fun s -> opt_api := Some (uri_of_arg s)),
      "<url> URL of the MediaWiki API.";
    "-cert", Arg.String (set_option opt_cert), "<file> Client certificate.";
    "-certkey", Arg.String (set_option opt_certkey), "<file> Private key.";
    "-login", Arg.String (set_login opt_login),
      "<user>:<pass> Login first of all.";
    "-page", Arg.String (set_option opt_page),
      "<title> The title of the page to edit.";
    "-level", Arg.Int (set_option opt_level),
      "<level> The level of the target section specified as the number of \
               equality signs in the corresponding MediaWiki delimiters. \
               The default level is 2, e.g. the normal section top-level. \
               This applies to -section, -add-after, and -add-before.";
    "-section", Arg.String (set_option opt_section),
      "<title> The title of the target section.";
    "-add-after",
      Arg.String (fun tt -> opt_ipts := After_section tt :: !opt_ipts),
      "<title> If the target section is not found, add it after <title>.";
    "-add-before",
      Arg.String (fun tt -> opt_ipts := Before_section tt :: !opt_ipts),
      "<title> If the target section is not found, add it before <title>.";
    "-add-bottom",
      Arg.String (fun tt -> opt_ipts := Bottom_of_section tt :: !opt_ipts),
      "<title> If the target section is not found, add it at the bottom of \
       <title>, which is one level above the target section.";
    "-replace", Arg.Unit (fun () -> opt_replace := true),
      " Replace the given section or page if it exists. Be careful!";
    "-s", Arg.String (fun s -> opt_subst := `Set(subst_of_arg s) :: !opt_subst),
      "<x>=<tmpl> Substitute <tmpl> for <x>.";
    "-i", Arg.String (fun s -> opt_subst := `Load_stdin s :: !opt_subst),
      "<x> Load <x> from the standard input.";
    "-l", Arg.String (fun s -> opt_subst := loadspec_of_arg s :: !opt_subst),
      "<x>(=<file>)? Load a template from <file> if specified, otherwise from \
                     the first match of <x>.tmpl under the include path, and \
                     substitute it for <x>.";
    "-I", Arg.String (fun s -> opt_includes := s :: !opt_includes),
      "<dir> Add <dir> to the include path.";
  ] in
  let misuse msg = eprintf "%s\n" msg; exit 64 in
  let mandatory name opt =
    match !opt with
    | None -> misuse (sprintf "The %s option is mandatory." name)
    | Some arg -> arg in
  Arg.parse arg_specs
            (fun _ -> misuse "Not expecting positional arguments.") arg_usage;
  let api = mandatory "-api" opt_api in
  let page = mandatory "-page" opt_page in
  let target =
    match !opt_level, !opt_section, !opt_ipts with
    | Some level, Some title, ipts -> Edit_section (level, title, List.rev ipts)
    | None, None, [] -> Edit_page
    | None, None, _ ->
      misuse "-add-* options are only relevant when targeting a section"
    | None, Some _, _ ->
      misuse "The -level option is required when targeting a section."
    | Some _, None, _ ->
      misuse "The -section option is required when targeting a section." in
  if List.count (function `Load_stdin _ -> true | _ -> false)
                !opt_subst > 1 then
    misuse "Only one mapping can be loaded from standard input.";

  Lwt_main.run begin
    let%lwt mw = Mwapi_lwt.open_api ?cert:!opt_cert ?certkey:!opt_certkey api in
    let%lwt subst =
      Lwt_list.fold_left_s
        (fun subst ->
          function
          | `Load x ->
            load_template !opt_includes x >|= fun tmpl ->
            String_map.add x (Template.subst_map subst tmpl) subst
          | `Load_from (x, fp) ->
            load_template_from fp >|= fun tmpl ->
            String_map.add x (Template.subst_map subst tmpl) subst
          | `Load_stdin x ->
            load_template_stdin () >|= fun tmpl ->
            String_map.add x (Template.subst_map subst tmpl) subst
          | `Set (x, tmpl) ->
            Lwt.return (String_map.add x (Template.subst_map subst tmpl) subst))
        String_map.empty !opt_subst in
    try%lwt
      begin match !opt_login with
      | None -> Lwt.return_unit
      | Some (_ as name, password) -> Utils.login ~name ~password mw
      end >>
      edit ~do_replace:!opt_replace ~page:(`Title page) ~target subst mw >>
      Mwapi_lwt.close_api mw
    with
    | Wiki_error {wiki_error_code; wiki_error_info; _} ->
      Lwt_log.error_f "%s - %s" wiki_error_code wiki_error_info
    | Http_error {http_error_code; http_error_info; _} ->
      Lwt_log.error_f "HTTP %d - %s" http_error_code http_error_info
  end
