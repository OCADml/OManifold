(lang dune 3.3)

(name OManifold)

(using directory-targets 0.1)

(using ctypes 0.1)

(generate_opam_files true)

(source
 (github geoffder/omanifold))

(documentation https://geoffder.github.io/omanifold)

(license GPL-2.0-or-later)

(authors "Geoff deRosenroll<geoffderosenroll@gmail.com")

(maintainers "Geoff deRosenroll<geoffderosenroll@gmail.com")

(package
 (name OManifold)
 (synopsis "OCaml bindings to the Manifold solid modelling library.")
 (description "OCaml bindings to the Manifold solid modelling library.")
 (depends
  (ocaml
   (>= 4.14.0))
  ctypes
  ctypes-foreign
  (OCADml
   (>= 0.1.0))))
