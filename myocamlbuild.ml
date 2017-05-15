(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Ocamlbuild_plugin

let () = dispatch @@ function
 | After_rules ->
    (match Sys.getenv "TERM" with
     | exception Not_found -> ()
     | "" | "dumb" -> ()
     | _ -> flag ["ocaml"; "compile"] (S [A"-color"; A"always"]));
    rule "%.mli & %.idem -> %.ml"
      ~deps:["%.mli"; "%.idem"] ~prod:"%.ml"
      begin fun env build ->
        let src = env "%.mli" and dst = env "%.ml" in
        cp src dst
      end
 | _ -> ()
