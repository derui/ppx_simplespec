
(* http://pleac.sourceforge.net/pleac_ocaml/strings.html#AEN68 *)
let eval text =
  let lexbuf = (Lexing.from_string text) in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  ignore (Toploop.execute_phrase false Format.std_formatter phrase)

let parse_argv () =
  let format = "text" in
  let files = ref [] in
  let packages = ref [] in
  let objs = ref [] in
  let anon s = files := s :: !files in
  let split_args r = fun args ->
    let regexp = Str.regexp "," in
    r := Str.split regexp args in
  Arg.parse [("-package", Arg.String (split_args packages), "packages to be using in the spec files.");
             ("-objs", Arg.String (split_args objs), "objects to be using in the spec files.")]
            anon "Usage : simplespec -package p1,p2,... <file> <file> ...";
  (format, List.rev !files, !packages, !objs)

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

let load_package_files package files =
  let package = Findlib.package_directory package in
  let load_obj file = Topdirs.dir_load Format.std_formatter (package ^ "/" ^ file) in
  Topdirs.dir_directory package;
  List.iter load_obj files

let load_object_files objs =
  List.iter (fun obj ->
    Topdirs.dir_load Format.std_formatter obj
  ) objs

let get_package_property param package property =
  try
    Some (Findlib.package_property param package property)
  with Not_found -> None

let rec load_package_byte_files = function
  | [] -> ()
  | package :: rest ->
     let path =
       try
         Findlib.package_directory package
       with Not_found -> "" in

     let archive = get_package_property ["byte"] package "archive" in
     let requires = get_package_property [] package "requires" in
     let load_obj () =
       match (path, archive) with
       | _, None -> ()
       | path, Some(archive) -> Topdirs.dir_load Format.std_formatter (path ^ "/" ^ archive) in

     begin
       match requires with
       | Some(r) when r <> "" -> load_package_byte_files (Str.split (Str.regexp " +") r)
       | _ -> ()
     end;

     Topdirs.dir_directory path;
     load_obj ();
     load_package_byte_files rest

let () =
  (* interactive deactivate *)
  Sys.interactive := false;
  (* enable toplevel environment *)
  Toploop.initialize_toplevel_env ();

  let format, files, packages,objs = parse_argv () in
  load_package_files "simplespec" ["simplespec.cma"];
  load_package_files "camlp4" ["camlp4of.cma"];
  load_package_files "simplespec" ["pa_spec.cmo"];
  load_package_byte_files packages;
  load_object_files objs;

  run_files format files
