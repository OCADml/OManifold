module Conf = Configurator.V1
module Pkg = Conf.Pkg_config

let base_libs = [ "-lstdc++"; "-lassimp" ]
let unix_usual_cuda = "/usr/local/cuda/lib64"
let maybe_arch_cuda = "/opt/cuda/lib64"

let default_cuda dir =
  Pkg.{ cflags = []; libs = [ Printf.sprintf "-L%s" dir; "-lcuda"; "-lcudart" ] }

let query_tbb pkg =
  let pc = Pkg.query pkg ~package:"tbb" in
  pc, if Option.is_some pc then "TBB" else "NONE"

let query_omp pkg =
  let pc = Pkg.query pkg ~package:"openmp" in
  pc, if Option.is_some pc then "OMP" else "NONE"

let discover c =
  let pc =
    match Pkg.get c with
    | Some p ->
      let cuda =
        match
          Option.map String.lowercase_ascii @@ Sys.getenv_opt "OMANIFOLD_USE_CUDA"
        with
        | Some ("on" | "true" | "yes") ->
          (* TODO: can probably remove these and trust pkg-config, it isn't
                  working due to messing with GPU packages recently *)
          if Sys.file_exists unix_usual_cuda && Sys.is_directory unix_usual_cuda
          then Some (default_cuda unix_usual_cuda)
          else if Sys.file_exists maybe_arch_cuda && Sys.is_directory maybe_arch_cuda
          then Some (default_cuda maybe_arch_cuda)
          else Pkg.query p ~package:"cuda cudart"
        | _ -> None
      and par, par_flag =
        match Option.map String.lowercase_ascii @@ Sys.getenv_opt "OMANIFOLD_PAR" with
        | Some "omp" -> query_omp p
        | Some "none" -> None, "NONE"
        | Some "tbb" -> query_tbb p
        | _ ->
          let pc = Pkg.query p ~package:"tbb" in
          if Option.is_some pc then pc, "TBB" else query_omp p
      in
      Out_channel.with_open_bin "use_cuda" (fun oc ->
        Printf.fprintf oc "%s" (if Option.is_some cuda then "ON" else "OFF") );
      Out_channel.with_open_bin "par_flag" (fun oc -> Printf.fprintf oc "%s" par_flag);
      List.filter_map Fun.id [ cuda; par ]
      |> List.fold_left
           (fun (cs, ls) Pkg.{ cflags; libs } -> cflags :: cs, libs :: ls)
           ([], [ base_libs ])
      |> fun (cs, ls) -> Pkg.{ cflags = List.concat cs; libs = List.concat ls }
    | None -> Pkg.{ cflags = []; libs = base_libs }
  in
  Conf.Flags.write_sexp "c_flags.sexp" pc.cflags;
  Conf.Flags.write_sexp "c_library_flags.sexp" pc.libs

let () = Conf.main ~name:"manifold" discover
