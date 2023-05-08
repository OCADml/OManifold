#include <stdlib.h>

#define CAML_NAME_SPACE
#include "ctypes_cstubs_internals.h"
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/domain.h>
#include <caml/mlvalues.h>
#include <manifoldc.h>

/* CAMLexport value caml_cairo_create(value vsurf) */
/* { */
/*   CAMLparam1(vsurf); */
/*   CAMLlocal1(vcontext); */
/*   cairo_t *cr; */

/*   cr = cairo_create(SURFACE_VAL(vsurf)); */
/*   caml_check_status(cr); */
/*   /\* Cairo documentation says that [cairo_create] "references target, */
/*      so you can immediately call cairo_surface_destroy() on it if you */
/*      don't need to maintain a separate reference to it".  We leave */
/*      destroying the surface to the GC but that means there is no need */
/*      to increase the reference of [vsurf]. *\/ */
/*   CAIRO_ASSIGN(vcontext, cr); */
/*   CAMLreturn(vcontext); */
/* } */

// TODO:
// - find out whether it is possible to have the funptr given to manifoldc be
// one that actually gives the work to different ocaml domains so that some
// amount of parallel execution is still possible
// - the function sent has to be C, so that it can be called concurrently (can't
// coordinate handing off the calls to domains in ocaml code since then /that/
// ocaml code would be in violation of the runtime/domain lock.)
value test_level_set_seq(value x1361, value x1360, value x1359, value x1358,
                         value x1357) {
  void *x1362 = CTYPES_ADDR_OF_FATPTR(x1361);
  float (*x1363)(float, float, float) = CTYPES_ADDR_OF_FATPTR(x1360);
  struct ManifoldBox *x1364 = CTYPES_ADDR_OF_FATPTR(x1359);
  double x1365 = Double_val(x1358);
  double x1368 = Double_val(x1357);
  struct ManifoldMeshGL *x1371 =
      manifold_level_set_seq(x1362, x1363, x1364, (float)x1365, (float)x1368);
  return CTYPES_FROM_PTR(x1371);
}
