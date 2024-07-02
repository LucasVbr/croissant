(lang dune 3.4)

(name croissant)
(generate_opam_files true)
(source (github LucasVbr/croissant))
(authors "LucasVbr")
(maintainers "LucasVbr")
(license LICENSE)
;(documentation https://url/to/documentation)

(package
  (name croissant)
  (synopsis "A short synopsis")
  (description "A longer description")
  (depends ocaml dune alcotest menhir ocamlformat)
  (tags
    ("Custom programming language" "French")
  )
)
(using menhir 2.1)
