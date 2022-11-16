let () =
  Cstubs.write_ml
    Format.std_formatter
    ~prefix:Sys.argv.(1)
    (module Manifold_c_function_descriptions.Descriptions);
