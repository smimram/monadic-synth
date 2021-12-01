(lang dune 2.8)
(version 0.1.0)
(name msynth)
(source (github smimram/ocaml-msynth))
(license LGPL-2.1)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")

(generate_opam_files true)
(package
  (name msynth)
  (synopsis "A monadic audio synthesizer")
  (depends
    (alsa (>= 0.3.0))
    lo
    graphics
  )
)