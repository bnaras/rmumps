/*
 * C entry points of rmumps registered for other packages (see
 * inst/include/rmumps/rmumps_api.h for the contract), and the error and
 * warning hooks called from the bundled MUMPS, PORD, METIS and SCOTCH code.
 */

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "../inst/include/rmumps/rmumps_api.h"
#include "rmumps_internal.h"

/* ---- error and warning hooks used by the bundled C libraries ----------- */

/* These are plain C on purpose: they are called from C code that is itself
 * called from Fortran, so throwing a C++ exception from here (as earlier
 * versions did through Rcpp::stop) is undefined behavior. Rf_error longjmps
 * like the rexit() used by the Fortran code; rmumps_dmumps_c_safe() below
 * catches both. */

void rcpp_error(const char *format, ...)
{
  char buf[1024];
  va_list ap;
  va_start(ap, format);
  vsnprintf(buf, sizeof buf, format, ap);
  va_end(ap);
  Rf_error("%s", buf);
}

void rcpp_warning(const char *format, ...)
{
  char buf[1024];
  va_list ap;
  va_start(ap, format);
  vsnprintf(buf, sizeof buf, format, ap);
  va_end(ap);
  Rf_warning("%s", buf);
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

static SEXP safe_body(void *data)
{
  safe_call *s = (safe_call *) data;
  dmumps_c(s->id);
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
  /* Legacy name used by clients built against rmumps <= 5.2.1-43. In
   * releases with a different DMUMPS_STRUC_C layout it becomes an adapter
   * from the 5.2.1 layout. */
  R_RegisterCCallable("rmumps", "dmumps_c", (DL_FUNC) dmumps_c);

  R_RegisterCCallable("rmumps", RMUMPS_NAME_DMUMPS_C_SAFE,
                      (DL_FUNC) rmumps_dmumps_c_safe);
  R_RegisterCCallable("rmumps", RMUMPS_NAME_DMUMPS_C, (DL_FUNC) dmumps_c);
  R_RegisterCCallable("rmumps", "rmumps_api_version",
                      (DL_FUNC) rmumps_api_version);
  R_RegisterCCallable("rmumps", "rmumps_mumps_version",
                      (DL_FUNC) rmumps_mumps_version);
  R_RegisterCCallable("rmumps", "rmumps_orderings_available",
                      (DL_FUNC) rmumps_orderings_available);
}
