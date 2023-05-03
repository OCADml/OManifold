let unix_usual_cuda = "/usr/local/cuda/lib64"
let maybe_arch_cuda = "/opt/cuda/lib64"
let base_flags = [ "-lstdc++"; "-lassimp" ]
let cuda dir = [ Printf.sprintf "-L%s" dir; "-lcuda"; "-lcudart" ]

let par_flags =
  match Option.map String.lowercase_ascii @@ Sys.getenv_opt "OMANIFOLD_PAR" with
  | Some "omp" -> [ "-fopenmp" ]
  | Some "none" -> []
  | _ -> [ "-ltbb" ]

let use_cuda, cuda_flags =
  match Option.map String.lowercase_ascii @@ Sys.getenv_opt "OMANIFOLD_USE_CUDA" with
  | Some ("on" | "true" | "yes") ->
    if Sys.os_type = "Unix"
    then
      if Sys.file_exists unix_usual_cuda && Sys.is_directory unix_usual_cuda
      then true, cuda unix_usual_cuda
      else if Sys.file_exists maybe_arch_cuda && Sys.is_directory maybe_arch_cuda
      then true, cuda maybe_arch_cuda
      else false, []
    else (
      match Option.map String.lowercase_ascii @@ Sys.getenv_opt "OMANIFOLD_CUDA_PATH" with
      | Some path -> true, cuda path
      | None -> false, [] )
  | _ -> false, []

let () =
  Out_channel.with_open_bin "use_cuda" (fun oc ->
    Printf.fprintf oc "%s" (if use_cuda then "ON" else "OFF") )

let () =
  Out_channel.with_open_bin "c_library_flags" (fun oc ->
    List.iter (List.iter (Printf.fprintf oc "%s\n")) [ base_flags; par_flags; cuda_flags ] )
