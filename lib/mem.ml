let allocate_buf ?finalise n_bytes =
  Ctypes.(to_voidp @@ allocate_n ?finalise char ~count:n_bytes)

let finaliser t f buf = ignore @@ f Ctypes.(coerce (ptr char) (ptr t) buf)
