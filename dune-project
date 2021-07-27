(lang dune 2.7)

(name fish)

(generate_opam_files true)

(license MIT)

(maintainers "idkjs")

(package
 (name fish)
 (synopsis "fish game")
 (description "Async app description")
 (depends
  ;; General system dependencies
  (dune
   (>= 2.2))
  (ocaml
   (>= 4.10.0))
  (core
   (>= 0.14.0))
  (ppx_compare
   (>= 0.14.0))
  (ppx_let
   (>= 0.14.0))
  (ppx_deriving
   (>= 5.2.1))
  (ppx_jane
   (>= 0.14.0))
  (graphics
   (>= 5.1.2))
  (extlib
   (>= 1.7.7-1))
  (yojson (>= 1.7.0))
  (async
   (>= 0.14.0))
  (reason
   (and
    (>= 3.6.0)
    (< 4.0.0)))
  (ounit2
   (>= 0.7.0))
  ;; Dev dependencies
  (ocaml-lsp-server :dev)))
