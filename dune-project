(lang dune 3.14)
(version 0.1.2)
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
    osc-unix
    graphics
    re
  )
)
