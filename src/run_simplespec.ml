
(* http://pleac.sourceforge.net/pleac_ocaml/strings.html#AEN68 *)
let eval text =
  let lexbuf = (Lexing.from_string text) in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  ignore (Toploop.execute_phrase false Format.std_formatter phrase)

let parse_argv () =
  let format = "text" in
  let files = ref [] in
  let anon s = files := s :: !files in
  Arg.parse [] anon "Usage : simplespec <file> <file> ...";
  (format, List.rev !files)


let run_spec format file =
  ignore (Toploop.use_silently Format.std_formatter file);
  eval "module Fmt = SpecFormat.Text;;";
  eval "List.iter (fun spec ->  Fmt.format Format.std_formatter spec) (Spec.run_specs ()); Spec.cleanup ();;"

let rec run_files format = function
  | [] -> ()
  | [file] -> run_spec format file
  | file :: files ->
    begin
      run_spec format file;
      run_files format files
    end

let load_object_files files =
  let package = Findlib.package_directory "simplespec" in
  let load_obj file = Topdirs.dir_load Format.std_formatter (package ^ "/" ^ file) in
  Topdirs.dir_directory package;
  List.iter load_obj files

let () =
  (* interactive deactivate *)
  Sys.interactive := false;
  (* enable toplevel environment *)
  Toploop.initialize_toplevel_env ();

  let format, files = parse_argv () in
  load_object_files ["spec.cmo"; "specFormat.cmo"];

  run_files format files
