module Conf = Configurator.V1
module Pkg = Conf.Pkg_config

let base_libs = [ "-lstdc++"; "-lassimp" ]

let discover c =
  let pc =
    match Pkg.get c with
    | Some p ->
      let par, par_flag =
        match Option.map String.lowercase_ascii @@ Sys.getenv_opt "OMANIFOLD_PAR" with
        | Some "OFF" -> None, "OFF"
        | _ ->
          let pc = Pkg.query p ~package:"tbb" in
          pc, if Option.is_some pc then "ON" else "OFF"
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
