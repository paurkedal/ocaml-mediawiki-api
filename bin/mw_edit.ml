(* Copyright (C) 2013--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

open Logging
open Lwt.Infix
open Lwt.Syntax
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

type edit_mode =
  | Create_if_missing
  | Create_or_modify
  | Create_or_replace

let find_section level tt =
  let open Mwapi_parse in
  List.find_map
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

let resolve_insertion_point level sections = function
 | Before_section tt ->
    find_section level tt sections
      |> Option.map (fun i -> i, (fun s -> `Prepend (break_after s)))
 | After_section tt ->
    find_section level tt sections
      |> Option.map (fun i -> i, (fun s -> `Append ("\n\n" ^ s)))
 | Bottom_of_section tt ->
    find_section (level - 1) tt sections
      |> Option.map (fun i -> i, (fun s -> `Append ("\n\n" ^ s)))

let edit_page ~edit_mode ~page mw =
  (match edit_mode with
   | Create_if_missing ->
      Lwt.return_ok (`Must, None, new_page, (fun s -> `Replace s))
   | Create_or_replace ->
      Lwt.return_ok (`May, None, new_page, (fun s -> `Replace s))
   | Create_or_modify ->
      Mwapi_lwt.call Mwapi_parse.(parse ~page wikitext) mw >|=
      (function
       | Ok content ->
          let tmpl = Template.of_string content in
          Ok (`May_not, None, tmpl, (fun s -> `Replace s))
       | Error (`Wiki_error {wiki_error_code = "missingtitle"; _}) ->
          Ok (`Must, None, new_page, (fun s -> `Replace s))
       | Error _ as res -> res))

let edit_section ~edit_mode ~page ~secn_level ~secn_title ~ipts mw =
  let*? sections = Mwapi_lwt.call Mwapi_parse.(parse ~page sections) mw in
  (match find_section secn_level secn_title sections with
   | Some secn_no ->
      let*? section_content =
        Mwapi_lwt.call
          Mwapi_parse.(parse ~page ~section:secn_no wikitext) mw
      in
      (match edit_mode with
       | Create_if_missing ->
          let* () = Log.info (fun f -> f "Section already exists.") in
          Lwt.return_error `Section_exists
       | Create_or_replace ->
          let* () = Log.info (fun f -> f "Replacing existing section.") in
          let tmpl = new_section secn_level secn_title in
          Lwt.return_ok
            (`May_not, Some (`No secn_no), tmpl, (fun s -> `Replace s))
       | Create_or_modify ->
          let* () = Log.info (fun f -> f "Modifying existing section.") in
          let tmpl = Template.of_string section_content in
          Lwt.return_ok
            (`May_not, Some (`No secn_no), tmpl, (fun s -> `Replace s)))
   | None ->
      (match
        List.find_map (resolve_insertion_point secn_level sections) ipts
       with
       | Some (secn_no, op) ->
          let+ () = Log.info (fun f -> f "Adding section.") in
          let tmpl = new_section secn_level secn_title in
          Ok (`May_not, Some (`No secn_no), tmpl, op)
       | None ->
          Lwt.return_error `Section_missing))

let edit ?(edit_mode = Create_or_modify) ~page ~target subst mw =
  let*? token = Utils.get_edit_token mw in
  let*? create, section, tmpl, make_op =
    (match target with
     | Edit_section (secn_level, secn_title, ipts) ->
        edit_section ~edit_mode ~page ~secn_level ~secn_title ~ipts mw
     | Edit_page ->
        edit_page ~edit_mode ~page mw)
  in
  let tmpl = Template.subst_map subst tmpl in
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

let bsnl_re = Re.compile Re.(str "\\\n")
let template_of_string s =
  Template.of_string (Re.replace_string bsnl_re ~by:"" s)

let load_template_stdin () = Lwt_io.read Lwt_io.stdin >|= template_of_string

let load_template_from fp =
  let* st = Lwt_unix.stat fp in
  Lwt_io.with_file ~mode:Lwt_io.input fp
    (fun ic ->
      let s = Bytes.create st.Unix.st_size in
      Lwt_io.read_into_exactly ic s 0 st.Unix.st_size >>= fun () ->
      Lwt.return (template_of_string (Bytes.to_string s)))

let load_template dirs x =
  let fn = x ^ ".tmpl" in
  let rec loop = function
    | [] -> fail_f "Cannot find template %s." x
    | dir :: dirs ->
      let fp = Filename.concat dir fn in
      Lwt.catch
        (fun () -> load_template_from fp)
        (function
         | Unix.Unix_error (Unix.ENOENT, _, _) -> loop dirs
         | exn -> Lwt.fail exn)
  in
  loop dirs

let loadspec_of_arg arg =
  match String.cut_affix "=" arg with
  | None -> `Load arg
  | Some (x, fp) -> `Load_from (x, fp)

let report_error ~edit_mode = function
 | Ok () | Error `Section_exists | Error `Page_exists ->
    Lwt.return 0
 | Error `Section_missing ->
    Log.err (fun f -> f "Cannot find section to edit or place to insert it.")
      >|= fun () -> 66
 | Error (`Wiki_error {wiki_error_code = "articleexists"; _})
   when edit_mode = Create_if_missing ->
    Log.info (fun f -> f "Page already exists.")
      >|= fun () -> 0
 | Error (#Mwapi.error as err) ->
    Log.err (fun f -> f "%a" Mwapi.pp_error err)
      >|= fun () -> 69

let () =
  setup_logging ();
  let opt_api = ref None in
  let opt_cert = ref None in
  let opt_certkey = ref None in
  let opt_login = ref None in
  let opt_page = ref None in
  let opt_level = ref None in
  let opt_section = ref None in
  let opt_edit_mode = ref Create_or_modify in
  let opt_ipts = ref [] in
  let opt_subst = ref [] in
  let opt_includes = ref [] in
  let opt_persist_cookies = ref false in
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
    "-create", Arg.Unit (fun () -> opt_edit_mode := Create_if_missing),
      " Create a new page or section if it does not exist, \
        otherwise do nothing. This is incompatible with -replace.";
    "-replace", Arg.Unit (fun () -> opt_edit_mode := Create_or_replace),
      " Replace the given section or page if it exists. Be careful! \
        This is incompatible with -create.";
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
    "-persistent-cookies", Arg.Unit (fun () -> opt_persist_cookies := true),
      " Load and save non-session cookies if any.";
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

  begin
    let*? mw =
      Mwapi_lwt.open_api
        ?cert:!opt_cert ?certkey:!opt_certkey
        ~load_cookies:!opt_persist_cookies api
    in
    let* subst =
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
        String_map.empty !opt_subst
    in
    let*? () =
      (match !opt_login with
       | None -> Lwt.return_ok ()
       | Some (_ as name, password) -> Utils.login ~name ~password mw)
    in
    let*? () =
      edit ~edit_mode:!opt_edit_mode ~page:(`Title page) ~target subst mw
    in
    Mwapi_lwt.close_api ~save_cookies:!opt_persist_cookies mw
  end >>= report_error ~edit_mode:!opt_edit_mode |> Lwt_main.run |> exit
