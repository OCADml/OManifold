let () =
  print_endline "#include <manifoldc.h>";
  Cstubs.write_c
    (* ~concurrency:Cstubs.unlocked *)
    (* ~concurrency:Cstubs.sequential *)
    Format.std_formatter
    ~prefix:Sys.argv.(1)
    (module Manifold_c_function_descriptions.Descriptions)
