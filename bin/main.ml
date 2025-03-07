open Core

module Config = struct
  type dirs = {
    input : string; [@default "src"]
    output : string; [@default "dist"]
    (* TODO: allow multiple templates *)
    templates : string; [@default "src/templates"]
    partials : string; [@default "src/partials"]
    static : string; [@default "src/static"]
    (* TODO: copy static? *)
    link_static : bool; [@default false]
    create_output : bool; [@default true]
  }
  [@@deriving of_sexp]

  (* TODO: allow this to be anything and automatically update json *)
  type meta = { name : string; description : string } [@@deriving of_sexp]
  type config = { dirs : dirs; meta : meta } [@@deriving of_sexp]
end
[@@deriving of_sexp]

let read_config filename =
  try Config.(config_of_sexp) @@ Sexp.load_sexp filename
  with e ->
    Format.eprintf "Failed to read/parse config from %s: %s\n" filename
      (Exn.to_string e);
    exit 1

let rec input_filepaths dirname dh =
  match Core_unix.readdir_opt dh with
  | Some fn -> (
      let acc = input_filepaths dirname dh in
      match Filename.split_extension fn with
      | _, Some ext when String.(ext = "md") ->
          Filename.concat dirname fn :: acc
      | _ -> acc)
  | None -> []

let to_output_filepaths dirname =
  List.map ~f:(fun fn ->
      Filename.concat dirname
        (Filename.basename @@ Filename.chop_extension fn ^ ".html"))

let compile_mustache_str str =
  try Mustache.of_string str
  with Mustache.Parse_error e ->
    print_endline str;
    Format.eprintf "%a@." Mustache.pp_template_parse_error e;
    exit 1

let compile_mustache_file fn =
  try compile_mustache_str @@ In_channel.read_all fn
  with e ->
    Format.eprintf "Failed to read %s: %s\n" fn (Exn.to_string e);
    exit 1

(* TODO: this will be super inefficient with lots of partials i think *)
let rec load_partials dirname dh =
  match Core_unix.readdir_opt dh with
  | Some fn -> (
      let acc = load_partials dirname dh in
      match Filename.split_extension fn with
      | name, Some ext when String.(ext = "mustache") ->
          Map.set acc ~key:name
            ~data:(compile_mustache_file @@ Filename.concat dirname fn)
      | _ -> acc)
  | None -> Map.empty (module String)

let compile_markdown_file filename =
  let ic = In_channel.create filename in
  let compiled = Omd.to_html (Omd.of_channel ic) in
  In_channel.close ic;
  compiled

let render_mustache_to_file filename inner_body context root_tmpl partials =
  let oc =
    try Out_channel.create filename
    with e ->
      Printf.eprintf
        "Failed to open file for writing: %s\n\
         You may want to set ((dirs ((create_output true)))) to create your \
         output directory.\n"
        (Exn.to_string e);
      exit 1
  in

  let fmt = Format.formatter_of_out_channel oc
  and inner_body = compile_mustache_str inner_body in

  let () =
    try
      Mustache.render_fmt fmt root_tmpl context ~partials:(fun name ->
          match name with
          | "inner_body" -> Some inner_body
          | _ -> Map.find partials name)
    with e ->
      Printf.eprintf "Failed to render to %s: %s\n" filename (Exn.to_string e);
      exit 1
  in
  Out_channel.close oc

let config =
  read_config @@ Option.value (Sys.getenv "STAML_CONFIG") ~default:"config.sexp"

let meta_context =
  `O
    [
      ("name", `String config.meta.name);
      ("description", `String config.meta.description);
    ]

(* create output dir, ignoring errors *)
let () =
  if config.dirs.create_output then
    try Core_unix.mkdir config.dirs.output with _ -> ()

(* link the static dir, ignoring errors *)
(* TODO: errors *)
let () =
  if config.dirs.link_static then
    try
      Core_unix.symlink
        ~target:(Filename.concat (Core_unix.getcwd ()) config.dirs.static)
        ~link_name:(Filename.concat config.dirs.output "static")
    with _ -> ()

(* open input dir *)
let md_paths =
  input_filepaths config.dirs.input
  @@
  try Core_unix.opendir config.dirs.input
  with e ->
    Format.eprintf
      "Failed to open input directory ((dirs ((input HERE)))): %s\n"
      (Exn.to_string e);
    exit 1

(* for every input and output path, compile md and render *)
let _ =
  let root_tmpl =
    compile_mustache_file
    @@ Filename.concat config.dirs.templates "root.mustache"
  in

  let partials =
    try
      let hdl = Core_unix.opendir config.dirs.partials in
      let p = load_partials config.dirs.partials hdl in
      Core_unix.closedir hdl;
      p
    with e ->
      Printf.eprintf
        "Failed to open partials directory: %s\nContinuing with no partials.\n"
        (Exn.to_string e);
      Map.empty (module String)
  in

  List.iter2 md_paths (to_output_filepaths config.dirs.output md_paths)
    ~f:(fun in_fp out_fp ->
      let inner_html = compile_markdown_file in_fp in
      render_mustache_to_file out_fp inner_html meta_context root_tmpl partials;
      print_endline @@ "Rendered: " ^ in_fp ^ " => " ^ out_fp)
