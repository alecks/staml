(lang dune 3.16)

(name staml)

(generate_opam_files true)

(source
 (github alecks/staml))

(documentation https://github.com/alecks/staml)
(license MIT)

(authors "Alex P.")
(maintainers "Alex P.")

(package
 (name staml)
 (synopsis "fast static site generator")
 (description "fast OCaml static site generator using Omd for markdown compilation")
 (depends dune omd core core_unix ppx_sexp_conv mustache)
 (tags
  (website markdown)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
