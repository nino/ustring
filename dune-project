(lang dune 3.4)

(name ustring)

(generate_opam_files true)

(source
 (github nino/ustring))

(authors "Nino Annighoefer")

(maintainers "Nino Annighoefer")

(license MIT)

(documentation https://github.com/nino/ustring/blob/trunk/lib/ustring.mli)

(package
 (name ustring)
 (synopsis "OCaml unicode utilities")
 (description "An attempt to learn about Unicode by implementing some functions in OCaml.")
 (depends ocaml dune base ounit2 uuseg uuseg.string re stdio)
 (tags
  (topics unicode learning)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
