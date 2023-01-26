let unix_usual_cuda = "/usr/local/cuda/lib64"
let maybe_arch_cuda = "/opt/cuda/lib64"
let base_flags = [ "-lstdc++"; "-lassimp"; "-fopenmp" ]
let cuda_flags dir = [ Printf.sprintf "-L%s" dir; "-lcuda"; "-lcudart" ]

let use_cuda, flags =
  (* TODO:
     - is the runtime always at the usual unix location on macos?
     - windows support (seems like the directories are always versioned there,
       so it will need to be more sophisticated than the current naive and
       inflexible setup)
       https://docs.nvidia.com/cuda/cuda-installation-guide-microsoft-windows/#installing-cuda-development-tools
       For example: this guide says the default is:
       C:\Program Files\NVIDIA GPU Computing Toolkit\CUDA\v12.0 *)
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
