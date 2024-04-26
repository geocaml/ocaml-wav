(lang dune 2.9)
(using mdx 0.1)
(name wav)
(generate_opam_files true)
(source
 (github geocaml/ocaml-wav))
(license MIT)
(authors "Patrick Ferris")

(maintainers "pf341@sirref.org")

(package
 (name wav)
 (synopsis "Pure OCaml WAV library")
 (description "An OCaml library for reading WAV files")
 (depends 
   cstruct 
   fmt 
   owl))