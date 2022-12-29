let unix_usual_cuda = "/usr/local/cuda/lib64"
let maybe_arch_cuda = "/opt/cuda/lib64"
let base_flags = [ "-lstdc++"; "-lassimp"; "-fopenmp" ]
let cuda_flags dir = [ Printf.sprintf "-L%s" dir; "-lcuda"; "-lcudart" ]

let use_cuda, flags =
  let libs =
    if Sys.file_exists unix_usual_cuda && Sys.is_directory unix_usual_cuda
    then cuda_flags unix_usual_cuda
    else if Sys.file_exists maybe_arch_cuda && Sys.is_directory maybe_arch_cuda
    then cuda_flags maybe_arch_cuda
    else []
  in
  match libs with
  | [] -> false, base_flags
  | libs -> true, base_flags @ libs

let () =
  Out_channel.with_open_bin "use_cuda" (fun oc ->
      Printf.fprintf oc "%s" (if use_cuda then "ON" else "OFF") )

let () =
  Out_channel.with_open_bin "c_library_flags" (fun oc ->
      List.iter (Printf.fprintf oc "%s\n") flags )
