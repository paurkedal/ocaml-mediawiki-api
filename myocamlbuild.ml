(* OASIS_START *)
(* OASIS_STOP *)

let () = dispatch begin function

  | Before_options as e ->
    Options.use_ocamlfind := true;
    dispatch_default e

  | After_rules as e ->
    flag ["doc"; "ocaml"; "extension:html"] &
      S[A"-charset"; A"utf8"; A"-colorize-code";
        A"-t"; A"MediaWiki API"];
    dispatch_default e

  | e ->
    dispatch_default e

end
