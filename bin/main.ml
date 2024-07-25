open Core

module Config = struct
  type dirs = {
    input : string; [@default "src"]
    output : string; [@default "dist"]
    templates : string; [@default "src/templates"]
    partials : string; [@default "src/partials"]
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

let read_config filename =
  try Config.(config_of_sexp) @@ Sexp.load_sexp filename
  with e ->
    Format.eprintf "Failed to read/parse config from %s: %s" filename
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

let compile_mustache_exn str =
  try Mustache.of_string str
  with Mustache.Parse_error e ->
    print_endline str;
    Format.eprintf "%a@." Mustache.pp_template_parse_error e;
    exit 1

let compile_mustache_file fn =
  try compile_mustache_exn @@ In_channel.read_all fn
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

let render_to_file filename inner_body context root_tmpl partials =
  let oc = Out_channel.create filename in
  let fmt = Format.formatter_of_out_channel oc in
  let inner_body = compile_mustache_exn inner_body in

  Mustache.render_fmt fmt root_tmpl context ~partials:(fun name ->
      match name with
      | "inner_body" -> Some inner_body
      | _ -> Map.find partials name);
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
    Format.eprintf "Failed to open input directory ((dirs ((input x)))): %s\n"
      (Exn.to_string e);
    exit 1

(* for every input and output path, compile and write *)
let _ =
  let root_tmpl =
    compile_mustache_file
    @@ Filename.concat config.dirs.templates "root.mustache"
  in
  let partials_hdl = Core_unix.opendir config.dirs.partials in
  let partials = load_partials config.dirs.partials partials_hdl in

  let _ =
    List.iter2 md_paths (to_output_filepaths config.dirs.output md_paths)
      ~f:(fun in_fp out_fp ->
        let inner_html = compile_markdown_file in_fp in
        render_to_file out_fp inner_html meta_context root_tmpl partials;
        print_endline @@ "compiled " ^ in_fp ^ " to " ^ out_fp)
  in
  Core_unix.closedir partials_hdl
