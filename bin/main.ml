open Core

module Config = struct
  type dirs = {
    input : string; [@default "src"]
    output : string; [@default "dist"]
    templates : string; [@default "src/templates"]
    static : string; [@default "src/static"]
    (* TODO: copy static? *)
    link_static : bool; [@default false]
    create_output : bool; [@default true]
  }
  [@@deriving of_sexp]

  type meta = { name : string; description : string } [@@deriving of_sexp]
  type config = { dirs : dirs; meta : meta } [@@deriving of_sexp]
end
[@@deriving of_sexp]

let config_filepath =
  Option.value (Sys.getenv "STAML_CONFIG") ~default:"config.sexp"

let config =
  try Config.(config_of_sexp) @@ Sexp.load_sexp config_filepath
  with e ->
    Format.eprintf
      "Failed to read/parse config (defaults to config.sexp, set with \
       STAML_CONFIG env var): %s\n"
      (Exn.to_string e);
    exit 1

let meta_json =
  `O
    [
      ("name", `String config.meta.name);
      ("description", `String config.meta.description);
    ]

let rec input_filepaths dir =
  match Core_unix.readdir_opt dir with
  | Some fn -> (
      let acc = input_filepaths dir in
      match Filename.split_extension fn with
      | _, Some ext when String.(ext = "md") ->
          Filename.concat config.dirs.input fn :: acc
      | _ -> acc)
  | None -> []

let input_to_output_filepaths =
  List.map ~f:(fun fn ->
      Filename.concat config.dirs.output
        (Filename.basename @@ Filename.chop_extension fn ^ ".html"))

let root_tmpl =
  try
    Mustache.of_string
    @@ In_channel.read_all
         (Filename.concat config.dirs.templates "root.mustache")
  with
  | Mustache.Parse_error e ->
      Format.eprintf "%a@." Mustache.pp_template_parse_error e;
      exit 1
  | e ->
      Format.eprintf "Failed to read root.mustache: %s\n" (Exn.to_string e);
      exit 1

let compile_from_file filename =
  let ic = In_channel.create filename in
  let compiled = Omd.to_html (Omd.of_channel ic) in
  In_channel.close ic;
  compiled

let render_to_file filename markdown json =
  let oc = Out_channel.create filename in
  let fmt = Format.formatter_of_out_channel oc in
  Mustache.render_fmt fmt root_tmpl json ~partials:(fun name ->
      match name with
      | "inner_body" -> Some (Mustache.of_string markdown)
      | _ -> None);
  Out_channel.close oc

(* create output_dir, ignoring errors *)
let () =
  if config.dirs.create_output then
    try Core_unix.mkdir config.dirs.output with _ -> ()

(* link the static dir, printing errors but continuing *)
let () =
  if config.dirs.link_static then
    try
      Core_unix.symlink ~target:config.dirs.static
        ~link_name:(Filename.concat config.dirs.output "static")
    with e ->
      Printf.eprintf "Failed to link static, continuing: %s\n" (Exn.to_string e)

(* open input dir *)
let md_paths =
  input_filepaths
  @@
  try Core_unix.opendir config.dirs.input
  with e ->
    Format.eprintf "Failed to open input directory ((dirs ((input x)))): %s\n"
      (Exn.to_string e);
    exit 1

(* for every input and output path, compile and write *)
let _ =
  List.iter2 md_paths (input_to_output_filepaths md_paths)
    ~f:(fun in_fp out_fp ->
      let inner_html = compile_from_file in_fp in
      render_to_file out_fp inner_html meta_json;
      print_endline @@ "compiled " ^ in_fp ^ " to " ^ out_fp)
