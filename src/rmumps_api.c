/*
 * Copyright 2026 Balasubramanian Narasimhan. GPL (>= 2); see inst/COPYRIGHTS.
 *
 * C entry points of rmumps registered for other packages (see
 * inst/include/rmumps/rmumps_api.h for the contract).
 *
 * The vendored code reports unrecoverable internal errors with rexit()
 * (Fortran) or rmumps_fatal() (C, src/shim), both of which longjmp like
 * Rf_error; rmumps_dmumps_c_safe() below catches them.
 */

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "../inst/include/rmumps/rmumps_api.h"
#include "rmumps_internal.h"
#include "shim/rmumps_shim.h"

/* Every call starts from the same state of the private random generator
 * used inside MUMPS, so results do not depend on earlier calls. */
void rmumps_dmumps_c_raw(DMUMPS_STRUC_C *id)
{
  rmumps_rng_reset();
  dmumps_c(id);
}

/* ---- safe call --------------------------------------------------------- */

typedef struct {
  DMUMPS_STRUC_C *id;
  char *msg;
  size_t msglen;
  int rc;
} safe_call;

static void set_msg(char *msg, size_t msglen, const char *text)
{
  if (msg != NULL && msglen > 0)
    snprintf(msg, msglen, "%s", text);
}

/* Test hook (src/rmumps_testhook.cpp): the next safe call raises an R
 * error inside the protected region, as rexit() would from MUMPS. */
static int fail_next = 0;
void rmumps_test_fail_next(void) { fail_next = 1; }

static SEXP safe_body(void *data)
{
  safe_call *s = (safe_call *) data;
  if (fail_next) {
    fail_next = 0;
    Rf_error("injected failure (test hook)");
  }
  rmumps_dmumps_c_raw(s->id);
  return R_NilValue;
}

static SEXP safe_handler(SEXP cond, void *data)
{
  safe_call *s = (safe_call *) data;
  if (Rf_inherits(cond, "interrupt")) {
    s->rc = RMUMPS_ERR_INTERRUPT;
    set_msg(s->msg, s->msglen, "rmumps: interrupted by the user");
    return R_NilValue;
  }
  s->rc = RMUMPS_ERR_R;
  /* A condition is a list whose first element is the message. */
  if (TYPEOF(cond) == VECSXP && XLENGTH(cond) > 0 &&
      TYPEOF(VECTOR_ELT(cond, 0)) == STRSXP &&
      XLENGTH(VECTOR_ELT(cond, 0)) > 0) {
    const char *m = CHAR(STRING_ELT(VECTOR_ELT(cond, 0), 0));
    if (s->msg != NULL && s->msglen > 0)
      snprintf(s->msg, s->msglen, "rmumps: %s", m);
  } else {
    set_msg(s->msg, s->msglen, "rmumps: error raised inside MUMPS");
  }
  return R_NilValue;
}

static void safe_toplevel(void *data)
{
  SEXP conds = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(conds, 0, Rf_mkChar("error"));
  SET_STRING_ELT(conds, 1, Rf_mkChar("interrupt"));
  R_tryCatch(safe_body, data, conds, safe_handler, data, NULL, NULL);
  UNPROTECT(1);
}

int rmumps_dmumps_c_safe(DMUMPS_STRUC_C *id, size_t sizeof_id, char *msg,
                         size_t msglen)
{
  safe_call s;
  set_msg(msg, msglen, "");
  if (id == NULL) {
    set_msg(msg, msglen, "rmumps: NULL DMUMPS_STRUC_C pointer");
    return RMUMPS_ERR_NULL;
  }
  if (sizeof_id != sizeof(DMUMPS_STRUC_C)) {
    if (msg != NULL && msglen > 0)
      snprintf(msg, msglen,
               "rmumps: DMUMPS_STRUC_C has %lu bytes in the caller but %lu "
               "in rmumps (MUMPS %s); rebuild the calling package against "
               "the installed rmumps",
               (unsigned long) sizeof_id,
               (unsigned long) sizeof(DMUMPS_STRUC_C), MUMPS_VERSION);
    return RMUMPS_ERR_SIZE;
  }
  if (id->job != -1 && id->info[0] == RMUMPS_INFO_POISONED) {
    if (id->job == -2)
      return RMUMPS_OK; /* do not free from an inconsistent state */
    set_msg(msg, msglen,
            "rmumps: this MUMPS instance failed earlier and cannot be used; "
            "create a new one");
    return RMUMPS_ERR_POISONED;
  }
  s.id = id;
  s.msg = msg;
  s.msglen = msglen;
  s.rc = RMUMPS_OK;
  /* R_ToplevelExec is the backstop for any jump R_tryCatch does not catch
   * (e.g. a restart invoked by a handler): nothing longjmps past us. */
  if (!R_ToplevelExec(safe_toplevel, &s) && s.rc == RMUMPS_OK) {
    s.rc = RMUMPS_ERR_R;
    set_msg(msg, msglen, "rmumps: MUMPS was interrupted by an R condition");
  }
  if (s.rc != RMUMPS_OK) {
    id->info[0] = RMUMPS_INFO_POISONED;
    id->info[1] = s.rc;
  }
  return s.rc;
}

/* ---- metadata ---------------------------------------------------------- */

static int rmumps_api_version(void) { return RMUMPS_API_VERSION; }

static const char *rmumps_mumps_version(void) { return MUMPS_VERSION; }

/* SCOTCH (ICNTL(7) = 3) is deliberately not reported: rmumps no longer
 * supports it. ICNTL(7) = 1 means a user-given ordering (perm_in). */
static int rmumps_orderings_available(void)
{
  return RMUMPS_ORDERING_BIT(0) | RMUMPS_ORDERING_BIT(1) |
    RMUMPS_ORDERING_BIT(2) | RMUMPS_ORDERING_BIT(4) |
    RMUMPS_ORDERING_BIT(5) | RMUMPS_ORDERING_BIT(6) |
    RMUMPS_ORDERING_BIT(7);
}

/* ---- registration ------------------------------------------------------ */

void rmumps_register_c_callables(void)
{
  R_RegisterCCallable("rmumps", RMUMPS_NAME_DMUMPS_C_SAFE,
                      (DL_FUNC) rmumps_dmumps_c_safe);
  R_RegisterCCallable("rmumps", RMUMPS_NAME_DMUMPS_C,
                      (DL_FUNC) rmumps_dmumps_c_raw);
  /* Older layouts, and the legacy name "dmumps_c" used by clients built
   * against rmumps <= 5.2.1-43 (MUMPS 5.2.1 layout). */
  rmumps_register_abi_adapters();
  R_RegisterCCallable("rmumps", "rmumps_api_version",
                      (DL_FUNC) rmumps_api_version);
  R_RegisterCCallable("rmumps", "rmumps_mumps_version",
                      (DL_FUNC) rmumps_mumps_version);
  R_RegisterCCallable("rmumps", "rmumps_orderings_available",
                      (DL_FUNC) rmumps_orderings_available);
}
