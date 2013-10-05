
(* http://pleac.sourceforge.net/pleac_ocaml/strings.html#AEN68 *)
let eval text =
  let lexbuf = (Lexing.from_string text) in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  ignore (Toploop.execute_phrase false Format.std_formatter phrase)

let () =
  (* interactive deactivate *)
  Sys.interactive := false;
  (* enable toplevel environment *)
  Toploop.initialize_toplevel_env ();

  eval "Printf.printf \"%d\" (1 + 1)"
