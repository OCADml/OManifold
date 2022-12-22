# OManifold

OManifold provides OCaml bindings to the [Manifold](https://github.com/elalish/manifold)
solid modelling (C++) library, with integrations into the
[OCADml](https:/github.com/OCADml/OCADml) ecosystem. Via Manifold, this library
provides a means to verify, smooth, and perform boolean operations (with guaranteed manifold output) upon meshes generated in OCADml.

![Manifold menger mesh logo](docs/assets/manifold_menger_mesh.png)

## Notable differences from Manifold

- Angles are represented in radians (OCADml convention)
- OCaml runtime lock means that level set mesh generation from signed distance
  functions cannot take advantage of parallelism (sequential execution still provided)

## Usage

``` ocaml
open OCADml
open OManifold

let () =
  let s = Manifold.sphere 20.
  and cyl = Manifold.cylinder ~center:true ~height:40. 10. in
  Export.manifold "example.glb" (Manifold.sub s cyl)
```

## Documentation

Documentation for OManifold is available
[online](https://geoffder.github.io/OManifold/OManifold/index.html), covering the
[API](https://geoffder.github.io/OManifold/OManifold/index.html#api). Referring
to the [manual](https://ocadml.github.io/OCADml/OCADml/index.html) for [OCADml](https://github.com/OCADml/OCADml) is also likely to be helpful.
