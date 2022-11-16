let () =
  print_endline "#include <manifoldc.h>";
  Cstubs_structs.write_c
    Format.std_formatter (module Manifold_c_type_descriptions.Descriptions)
