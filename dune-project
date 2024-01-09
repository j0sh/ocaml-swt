(lang dune 3.6)

(name swt)

(generate_opam_files true)

(source
 (github j0sh/ocaml-swt))

(authors "Josh Allmann <joshua.allmann@gmail.com>")

(maintainers "Josh Allmann <joshua.allmann@gmail.com>")

(license LICENSE)

(documentation https://j0sh.github.io/swt)

(package
 (name swt)
 (synopsis "Simple Web Toolkit for OCaml")
 (description "Simple Web Toolkit for OCaml")
 (depends ocaml dune cohttp-lwt-unix cryptokit ppx_fields_conv)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
