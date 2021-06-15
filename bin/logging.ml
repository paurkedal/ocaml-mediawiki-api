(* Copyright (C) 2019--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

open Unprime_list
open Unprime_string

let log_src = Logs.Src.create "mwapi.tools"
module Log = (val Logs_lwt.src_log log_src)

let make_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
       | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
       | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.async (fun () -> Lwt.finalize write unblock);
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }

module String_map = Map.Make (String)

let set_log_levels rules =
  let add_src src = String_map.add (Logs.Src.name src) src in
  let sources = List.fold add_src (Logs.Src.list ()) String_map.empty in
  String.split_on_char ';' rules |> List.iter begin fun rule ->
    (match String.cut_affix "->" rule with
     | None -> failwith "Syntax error in logging rule."
     | Some (src_name, level_name) ->
        let src_name = String.trim src_name in
        let level_name = String.trim level_name in
        (match Logs.level_of_string level_name with
         | Error (`Msg msg) -> failwith ("Invalid log level: " ^ msg)
         | Ok level ->
            (match String_map.find_opt src_name sources with
             | None -> failwith ("Invalid log source " ^ src_name ^ ".")
             | Some src -> Logs.Src.set_level src level)))
  end

let setup_logging () =
  Logs.set_reporter (make_reporter ());
  (match Sys.getenv "MWAPI_LOG" with
   | exception Not_found ->
      (match Sys.getenv "LWT_LOG" with
       | exception Not_found -> ()
       | rules -> set_log_levels rules)
   | rules -> set_log_levels rules)
