(lang dune 2.7)
; This file was generated by drom, using drom.toml

(cram enable)
(name core)
(allow_approximate_merlin)
(generate_opam_files false)
(version 0.1.0)
(formatting (enabled_for ocaml reason))
(using menhir 2.0)

(package
 (name core)
 (synopsis "The core project")
 (description "This is the description\nof the core OCaml project\n")
 (depends
   (ocaml (>= 4.07.0))
   (core_lib (= version))
   ppx_inline_test
   ppx_expect
   odoc
   ocamlformat
  )
 )

(package
 (name core_lib)
 (synopsis "The core project")
 (description "This is the description\nof the core OCaml project\n")
 (depends
   (ocaml (>= 4.07.0))
   ppx_inline_test
   ppx_expect
   odoc
   ocamlformat
  )
 )


