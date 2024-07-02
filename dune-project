(lang dune 3.4)

(name croissant)
(generate_opam_files true)
(source (github LucasVbr/croissant))
(authors "LucasVbr <contact@lucasvbr.dev>")
(maintainers "LucasVbr <contact@lucasvbr.dev>")
(license MIT)
;(documentation https://url/to/documentation)

(package
  (name croissant)
  (synopsis "A short synopsis")
  (description "A longer description")
  (depends ocaml dune menhir ocamlformat (alcotest :with-test))
  (tags
    ("Custom programming language" "French")
  )
)
(using menhir 2.1)
