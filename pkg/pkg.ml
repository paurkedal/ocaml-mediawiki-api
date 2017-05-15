#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let () = Pkg.describe ~licenses "mediawiki-api" @@ fun c ->
  Ok [
    Pkg.mllib "lib/mediawiki-api.mllib";
    Pkg.bin ~dst:"mw-edit" "bin/mw_edit";
    Pkg.bin ~dst:"mw-filter" "bin/mw_filter";
  ]
