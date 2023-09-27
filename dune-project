(lang dune 3.7)

(name typeshell)

(generate_opam_files true)

(source
 (github NyuB/typeshell))

(authors "Brice Decaestecker")

(maintainers "Brice Decaestecker")

(license LICENSE)

(documentation https://github/NyuB/typeshell)

(using menhir 2.1)

(package
 (name typeshell)
 (synopsis "Checked dsl to bash transpiler")
 (description "Never write raw bash again")
 (depends 
  (ocaml (>= 5.0.0))
  dune
  graphics
  (alcotest :with-test)
  (ocamlformat :with-test)
  (ppx_expect :with-test))
 (tags
  (bash compiler)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
