(lang dune 3.5)

(name binary_tree)

(generate_opam_files true)

(source
 (github TimeToStop/binary_tree))

(authors "Ratushnyak E. A.")

(maintainers "Ratushnyak E. A.")

(license LICENSE)

(documentation https://github.com/TimeToStop/binary_tree)

(package
 (name binary_tree)
 (synopsis "Lab2")
 (description "Binary tree in ocaml")
 (depends ocaml dune ounit2))