/* Copyright 2026 Balasubramanian Narasimhan. GPL (>= 2); see rmumps' inst/COPYRIGHTS. */
/* Client built against the rmumps 5.2.1-44 headers (DMUMPS_STRUC_C of
 * MUMPS 5.2.1, ABI 1; copies in abi1/), as Uno is when compiled in API mode
 * against rmumps 5.2.1-44. It must keep working with any later rmumps. */

#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "abi1/rmumps_api.h"

static rmumps_api api1;

void abi1_load(void)
{
  rmumps_api_load(&api1);
}

/* Solves A x = rhs (1-based triplets); returns list(x, info1, infog28,
 * deficiency, version). */
SEXP abi1_safe_solve(SEXP irn, SEXP jcn, SEXP a, SEXP rhs)
{
  DMUMPS_STRUC_C id;
  char msg[256];
  int rc, info1;
  SEXP x, res;

  memset(&id, 0, sizeof id);
  id.job = -1;
  id.par = 1;
  id.sym = 0;
  id.comm_fortran = -987654;
  rc = RMUMPS_DMUMPS_C(&api1, &id, msg, sizeof msg);
  if (rc != RMUMPS_OK)
    Rf_error("%s", msg);
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
  rc = RMUMPS_DMUMPS_C(&api1, &id, msg, sizeof msg);
  info1 = id.info[0];
  res = PROTECT(Rf_allocVector(VECSXP, 5));
  SET_VECTOR_ELT(res, 0, x);
  SET_VECTOR_ELT(res, 1, Rf_ScalarInteger(info1));
  SET_VECTOR_ELT(res, 2, Rf_ScalarInteger(id.infog[27]));
  SET_VECTOR_ELT(res, 3, Rf_ScalarInteger(id.deficiency));
  SET_VECTOR_ELT(res, 4, Rf_mkString(id.version_number));
  id.job = -2;
  RMUMPS_DMUMPS_C(&api1, &id, NULL, 0);
  if (rc != RMUMPS_OK)
    Rf_error("%s", msg);
  UNPROTECT(2);
  return res;
}

/* Size check of the ABI-1 entry point. */
SEXP abi1_size_mismatch(void)
{
  DMUMPS_STRUC_C id;
  char msg[256];
  memset(&id, 0, sizeof id);
  id.job = -1;
  return Rf_ScalarInteger(api1.dmumps_c_safe(&id, sizeof id + 8, msg, sizeof msg));
}

/* JOB other than -1 on a structure that was never initialized. */
SEXP abi1_not_initialized(void)
{
  DMUMPS_STRUC_C id;
  char msg[256];
  int rc;
  SEXP res;
  memset(&id, 0, sizeof id);
  id.job = 1;
  rc = api1.dmumps_c_safe(&id, sizeof id, msg, sizeof msg);
  res = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res, 0, Rf_ScalarInteger(rc));
  SET_VECTOR_ELT(res, 1, Rf_mkString(msg));
  UNPROTECT(1);
  return res;
}
