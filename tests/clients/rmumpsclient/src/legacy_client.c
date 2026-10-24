/* Client written the way packages built against rmumps <= 5.2.1-43 are
 * (e.g. Uno's dmumps_shim.c): legacy "dmumps_c" name, MUMPS 5.2.1 layout
 * (the headers they were compiled with; copies in abi1/). */

#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "abi1/dmumps_c.h"

SEXP legacy_solve(SEXP irn, SEXP jcn, SEXP a, SEXP rhs)
{
  typedef void (*dmumps_fn)(DMUMPS_STRUC_C *);
  static dmumps_fn fn = NULL;
  DMUMPS_STRUC_C id;
  int info1;
  SEXP x;

  if (fn == NULL)
    fn = (dmumps_fn) R_GetCCallable("rmumps", "dmumps_c");
  memset(&id, 0, sizeof id);
  id.job = -1;
  id.par = 1;
  id.sym = 0;
  id.comm_fortran = -987654;
  fn(&id);
  id.icntl[0] = -1;
  id.icntl[1] = -1;
  id.icntl[2] = -1;
  id.icntl[3] = 0;
  id.n = (MUMPS_INT) XLENGTH(rhs);
  id.nz = (MUMPS_INT) XLENGTH(a);
  id.nnz = (MUMPS_INT8) XLENGTH(a);
  id.irn = INTEGER(irn);
  id.jcn = INTEGER(jcn);
  id.a = REAL(a);
  x = PROTECT(Rf_duplicate(rhs));
  id.rhs = REAL(x);
  id.nrhs = 1;
  id.lrhs = id.n;
  id.job = 6;
  fn(&id);
  info1 = id.info[0];
  id.job = -2;
  fn(&id);
  if (info1 < 0)
    Rf_error("MUMPS INFO(1)=%d", info1);
  UNPROTECT(1);
  return x;
}
