/* Client of the safe C API in <rmumps/rmumps_api.h>. */

#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <rmumps/rmumps_api.h>

static rmumps_api api;

SEXP legacy_solve(SEXP irn, SEXP jcn, SEXP a, SEXP rhs);
SEXP l3_solve(SEXP irn, SEXP jcn, SEXP a, SEXP rhs);
SEXP abi1_safe_solve(SEXP irn, SEXP jcn, SEXP a, SEXP rhs);
SEXP abi1_size_mismatch(void);
SEXP abi1_not_initialized(void);
void abi1_load(void);

static void init_struct(DMUMPS_STRUC_C *id, int sym)
{
  memset(id, 0, sizeof *id);
  id->job = -1;
  id->par = 1;
  id->sym = sym;
  id->comm_fortran = -987654;
}

static void quiet(DMUMPS_STRUC_C *id)
{
  id->icntl[0] = -1;
  id->icntl[1] = -1;
  id->icntl[2] = -1;
  id->icntl[3] = 0;
}

/* Solves A x = rhs, A given as 1-based triplets. */
SEXP safe_solve(SEXP irn, SEXP jcn, SEXP a, SEXP rhs, SEXP sym)
{
  DMUMPS_STRUC_C id;
  char msg[256];
  int rc, info1, info2;
  SEXP x;

  init_struct(&id, Rf_asInteger(sym));
  rc = RMUMPS_DMUMPS_C(&api, &id, msg, sizeof msg);
  if (rc != RMUMPS_OK)
    Rf_error("%s", msg);
  quiet(&id);
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
  rc = RMUMPS_DMUMPS_C(&api, &id, msg, sizeof msg);
  info1 = id.info[0];
  info2 = id.info[1];
  id.job = -2;
  RMUMPS_DMUMPS_C(&api, &id, NULL, 0);
  if (rc != RMUMPS_OK)
    Rf_error("%s", msg);
  if (info1 < 0)
    Rf_error("MUMPS INFO(1)=%d INFO(2)=%d", info1, info2);
  UNPROTECT(1);
  return x;
}

/* Return code and message for a structure size that does not match. */
SEXP size_mismatch(void)
{
  DMUMPS_STRUC_C id;
  char msg[256];
  int rc;
  SEXP res;
  init_struct(&id, 0);
  rc = api.dmumps_c_safe(&id, sizeof id - 8, msg, sizeof msg);
  res = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res, 0, Rf_ScalarInteger(rc));
  SET_VECTOR_ELT(res, 1, Rf_mkString(msg));
  UNPROTECT(1);
  return res;
}

SEXP null_struct(void)
{
  char msg[256];
  return Rf_ScalarInteger(api.dmumps_c_safe(NULL, sizeof(DMUMPS_STRUC_C), msg,
                                            sizeof msg));
}

/* A client built for another layout: must get a clean R error. */
SEXP abi_mismatch(void)
{
  R_GetCCallable("rmumps", "rmumps_dmumps_c_safe_abi99");
  return R_NilValue;
}

SEXP meta(void)
{
  SEXP res = PROTECT(Rf_allocVector(VECSXP, 4));
  SET_VECTOR_ELT(res, 0, Rf_ScalarInteger(api.api_version()));
  SET_VECTOR_ELT(res, 1, Rf_mkString(api.mumps_version()));
  SET_VECTOR_ELT(res, 2, Rf_ScalarInteger(api.orderings_available()));
  SET_VECTOR_ELT(res, 3, Rf_ScalarInteger(RMUMPS_DMUMPS_ABI));
  UNPROTECT(1);
  return res;
}

static const R_CallMethodDef call_entries[] = {
  {"safe_solve", (DL_FUNC) &safe_solve, 5},
  {"size_mismatch", (DL_FUNC) &size_mismatch, 0},
  {"null_struct", (DL_FUNC) &null_struct, 0},
  {"abi_mismatch", (DL_FUNC) &abi_mismatch, 0},
  {"meta", (DL_FUNC) &meta, 0},
  {"legacy_solve", (DL_FUNC) &legacy_solve, 4},
  {"l3_solve", (DL_FUNC) &l3_solve, 4},
  {"abi1_safe_solve", (DL_FUNC) &abi1_safe_solve, 4},
  {"abi1_size_mismatch", (DL_FUNC) &abi1_size_mismatch, 0},
  {"abi1_not_initialized", (DL_FUNC) &abi1_not_initialized, 0},
  {NULL, NULL, 0}
};

void R_init_rmumpsclient(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, call_entries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  rmumps_api_load(&api);
  abi1_load();
}
