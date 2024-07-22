open Core

type dirs_config = {
  input : string; [@default "src"]
  output : string; [@default "dist"]
  mkdir_output : bool; [@default true]
}
[@@deriving of_sexp]

type config = { dirs : dirs_config } [@@deriving of_sexp]

let config_filepath =
  Option.value (Sys.getenv "STAML_CONFIG") ~default:"config.sexp"

let config =
  try config_of_sexp @@ Sexp.load_sexp config_filepath
  with e ->
    eprintf
      "Failed to read/parse config (defaults to config.sexp, set with \
       STAML_CONFIG env var): %s\n"
      (Exn.to_string e);
    exit 1

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

let compile_and_write ic oc = fprintf oc "%s" (Omd.to_html @@ Omd.of_channel ic)

(* create output_dir, ignoring errors *)
let () =
  if config.dirs.mkdir_output then
    try Core_unix.mkdir config.dirs.output with _ -> ()

(* open input dir *)
let md_paths =
  input_filepaths
  @@
  try Core_unix.opendir config.dirs.input
  with e ->
    eprintf "Failed to open input directory ((dirs ((input x)))): %s\n"
      (Exn.to_string e);
    exit 1

(* for every input and output path, compile and write *)
let _ =
  List.iter2 md_paths (input_to_output_filepaths md_paths)
    ~f:(fun in_fp out_fp ->
      let ic = In_channel.create in_fp in
      let oc = Out_channel.create out_fp in

      compile_and_write ic oc;
      print_endline @@ "compiled " ^ in_fp ^ " to " ^ out_fp;

      In_channel.close ic;
      Out_channel.close oc)
