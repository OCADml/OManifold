open OCADml
open Ctypes
open C.Types

let voidp_coerce typ = Ctypes.coerce Ctypes_static.(ptr void) typ

let string_to_ptr ctyp s =
  Ctypes.(CArray.of_string s |> CArray.start |> coerce (ptr char) (ptr ctyp))

let size_of_int = Unsigned.Size_t.of_int
let size_to_int = Unsigned.Size_t.to_int
let vec2_to_v2 v = v2 (getf v Vec2.x) (getf v Vec2.y)
let vec3_to_v3 v = v3 (getf v Vec3.x) (getf v Vec3.y) (getf v Vec3.z)
let vec4_to_v4 v = v4 (getf v Vec4.x) (getf v Vec4.y) (getf v Vec4.z) (getf v Vec4.w)
let ivec3_to_tup v = getf v IVec3.x, getf v IVec3.y, getf v IVec3.z
let ivec3_to_list v = [ getf v IVec3.x; getf v IVec3.y; getf v IVec3.z ]
let ivec3_to_list_rev v = [ getf v IVec3.z; getf v IVec3.y; getf v IVec3.x ]

let vec2_of_v2 (v : v2) =
  let vec = make Vec2.t in
  setf vec Vec2.x (V2.x v);
  setf vec Vec2.y (V2.y v);
  vec

let vec3_of_v3 (v : v3) =
  let vec = make Vec3.t in
  setf vec Vec3.x (V3.x v);
  setf vec Vec3.y (V3.y v);
  setf vec Vec3.z (V3.z v);
  vec

let vec4_of_v4 (v : v4) =
  let vec = make Vec4.t in
  setf vec Vec4.x (Gg.V4.x v);
  setf vec Vec4.y (Gg.V4.y v);
  setf vec Vec4.z (Gg.V4.z v);
  setf vec Vec4.w (Gg.V4.w v);
  vec

let ivec3_of_tup (x, y, z) =
  let vec = make IVec3.t in
  setf vec IVec3.x x;
  setf vec IVec3.y y;
  setf vec IVec3.z z;
  vec
