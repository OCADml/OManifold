let () =
  print_endline "#include \"structs.c\"";
  print_endline "#include <olm/crypto.h>";
  print_endline "#include <olm/error.h>";
  print_endline "#include <olm/inbound_group_session.h>";
  print_endline "#include <olm/megolm.h>";
  print_endline "#include <olm/olm.h>";
  print_endline "#include <olm/outbound_group_session.h>";
  print_endline "#include <olm/pk.h>";
  print_endline "#include <olm/sas.h>";
  Cstubs_structs.write_c
    Format.std_formatter (module Manifold_c_type_descriptions.Descriptions)
