module Conf = Configurator.V1
module Pkg = Conf.Pkg_config

let base_libs = [ "-lstdc++"; "-lassimp" ]

let query_tbb pkg =
  let pc = Pkg.query pkg ~package:"tbb" in
  pc, if Option.is_some pc then "TBB" else "NONE"

(* TODO: some of this could be further simplified now that the only external library
    for paralellism is TBB*)
  (* TODO: since I am using EXPORT, I need to also look for assimp and fail
    with a helpful error message if it is not found (manifold is no longer
    vendoring). TBB was never vendored, but the same should be done if the user
    wants TBB and they do not have it installed (otherwise the manifold build
    will fail) *)
let discover c =
  let pc =
    match Pkg.get c with
    | Some p ->
      let par, par_flag =
        match Option.map String.lowercase_ascii @@ Sys.getenv_opt "OMANIFOLD_PAR" with
        | Some "none" -> None, "NONE"
        | Some "tbb" -> query_tbb p
        | _ ->
          let pc = Pkg.query p ~package:"tbb" in
          if Option.is_some pc then pc, "TBB" else None, "NONE"
      in
      Out_channel.with_open_bin "par_flag" (fun oc -> Printf.fprintf oc "%s" par_flag);
      List.filter_map Fun.id [ par ]
      |> List.fold_left
           (fun (cs, ls) Pkg.{ cflags; libs } -> cflags :: cs, libs :: ls)
           ([], [ base_libs ])
      |> fun (cs, ls) -> Pkg.{ cflags = List.concat cs; libs = List.concat ls }
    | None -> Pkg.{ cflags = []; libs = base_libs }
  in
  Conf.Flags.write_sexp "c_flags.sexp" pc.cflags;
  Conf.Flags.write_sexp "c_library_flags.sexp" pc.libs

let () = Conf.main ~name:"manifold" discover
