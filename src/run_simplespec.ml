
(* http://pleac.sourceforge.net/pleac_ocaml/strings.html#AEN68 *)
let eval text =
  let lexbuf = (Lexing.from_string text) in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  ignore (Toploop.execute_phrase false Format.std_formatter phrase)

let parse_argv () =
  let format = "text" in
  let files = ref [] in
  let packages = ref [] in
  let anon s = files := s :: !files in
  let split_packages packs =
    let regexp = Str.regexp "," in
    packages := Str.split regexp packs in
  Arg.parse [("-package", Arg.String split_packages, "packages to be using in the spec files.")]
    anon "Usage : simplespec -package p1,p2,... <file> <file> ...";
  (format, List.rev !files, !packages)

let run_spec format file =
  ignore (Toploop.use_silently Format.std_formatter file);
  eval "open Simplespec;;";
  eval "module Fmt = SpecFormat.Text;;";
  eval "List.iter (fun spec ->  Fmt.format Format.std_formatter spec) (Spec.run_specs ()); Spec.cleanup ();;"

let rec run_files format = function
  | [] -> ()
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

let rec load_package_byte_files = function
  | [] -> ()
  | package :: rest ->
    let path = Findlib.package_directory package in
    let archive = Findlib.package_property ["byte"] package "archive" in
    let load_obj () = Topdirs.dir_load Format.std_formatter (path ^ "/" ^ archive) in
    Topdirs.dir_directory path;
    load_obj ();
    load_object_files rest

let () =
  (* interactive deactivate *)
  Sys.interactive := false;
  (* enable toplevel environment *)
  Toploop.initialize_toplevel_env ();

  let format, files, packages = parse_argv () in
  load_object_files ["simplespec.cma"];
  load_package_byte_files packages;

  run_files format files
