(lang dune 3.7)

(name OManifold)

(using directory-targets 0.1)

(using ctypes 0.1)

(cram disable)

(generate_opam_files true)

(source
 (github ocadml/omanifold))

(documentation https://ocadml.github.io/omanifold)

(license GPL-2.0-or-later)

(authors "Geoff deRosenroll<geoffderosenroll@gmail.com")

(maintainers "Geoff deRosenroll<geoffderosenroll@gmail.com")

(package
 (name OManifold)
 (synopsis "OCaml bindings to the Manifold solid modelling library")
 (description "OCaml bindings to the Manifold solid modelling library")
 (depends
  (ocaml
   (>= 4.14.0))
  ctypes
  ctypes-foreign
  integers
  conf-cmake
  (gg
   (>= 1.0.0))
  (OCADml
   (>= 0.6.0))
  conf-assimp
  conf-pkg-config))
