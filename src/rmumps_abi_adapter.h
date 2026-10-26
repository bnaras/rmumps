/*
 * Copyright 2026 Balasubramanian Narasimhan. GPL (>= 2); see inst/COPYRIGHTS.
 *
 * Adapters for clients built against an older DMUMPS_STRUC_C layout.
 *
 * Included by the generated rmumps_abi_adapters.c, which defines, for each
 * old layout k, the struct rmumps_dmumps_struc_abi<k> and the functions
 * abi<k>_copy_in()/abi<k>_copy_out(), then instantiates
 * RMUMPS_ABI_ADAPTER(k, type).
 *
 * An old-layout struct is mirrored by a current-layout struct allocated at
 * JOB = -1 and freed at JOB = -2, found through its address. Every call
 * copies the old struct in, runs MUMPS on the mirror, and copies the result
 * back, so members set by MUMPS (INFO, pointers it allocates, ...) and by
 * the client round-trip unchanged. Members that exist only in the current
 * layout keep their values in the mirror between calls.
 *
 * Main thread only, like the rest of the API.
 */
#ifndef RMUMPS_ABI_ADAPTER_H
#define RMUMPS_ABI_ADAPTER_H

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "../inst/include/rmumps/rmumps_api.h"
#include "rmumps_internal.h"

#define RMUMPS_ABI_ASSERT(e) _Static_assert(e, #e)
/* The recorded offsets are those of LP64/LLP64 targets. */
#if defined(__LP64__) || defined(_LP64) || defined(_WIN64)
# define RMUMPS_ABI_CHECK_OFFSETS 1
#else
# define RMUMPS_ABI_CHECK_OFFSETS 0
#endif

typedef struct rmumps_abi_slot {
  const void *old;
  DMUMPS_STRUC_C *cur;
  struct rmumps_abi_slot *next;
} rmumps_abi_slot;

static rmumps_abi_slot *rmumps_abi_slots = NULL;

/* Mirror of `old`; a new zeroed one when `create` (JOB = -1). NULL if
 * there is none, or if memory is exhausted. */
static DMUMPS_STRUC_C *rmumps_abi_mirror(const void *old, int create)
{
  rmumps_abi_slot *s;
  for (s = rmumps_abi_slots; s != NULL; s = s->next)
    if (s->old == old) {
      if (create) memset(s->cur, 0, sizeof *s->cur);
      return s->cur;
    }
  if (!create) return NULL;
  s = (rmumps_abi_slot *) malloc(sizeof *s);
  if (s == NULL) return NULL;
  s->cur = (DMUMPS_STRUC_C *) calloc(1, sizeof *s->cur);
  if (s->cur == NULL) { free(s); return NULL; }
  s->old = old;
  s->next = rmumps_abi_slots;
  rmumps_abi_slots = s;
  return s->cur;
}

static void rmumps_abi_drop(const void *old)
{
  rmumps_abi_slot **p = &rmumps_abi_slots;
  while (*p != NULL) {
    if ((*p)->old == old) {
      rmumps_abi_slot *s = *p;
      *p = s->next;
      free(s->cur);
      free(s);
      return;
    }
    p = &(*p)->next;
  }
}

/* Copy a NUL-terminated string between char arrays of different lengths. */
static void copy_str(char *dst, size_t dstlen, const char *src, size_t srclen)
{
  size_t n = 0;
  while (n < srclen && n + 1 < dstlen && src[n] != '\0') n++;
  memcpy(dst, src, n);
  memset(dst + n, 0, dstlen - n);
}

static void rmumps_abi_msg(char *msg, size_t msglen, const char *text)
{
  if (msg != NULL && msglen > 0) {
    strncpy(msg, text, msglen - 1);
    msg[msglen - 1] = '\0';
  }
}

#define RMUMPS_ABI_NOT_INIT \
  "rmumps: this MUMPS instance was not initialized (JOB = -1) through " \
  "this interface"

/* Raw entry point (may longjmp, like the current-layout one) and safe entry
 * point (never longjmps) for layout k. */
#define RMUMPS_ABI_ADAPTER(k, T)                                             \
static void rmumps_dmumps_c_abi##k(T *o)                                     \
{                                                                            \
  DMUMPS_STRUC_C *c;                                                         \
  if (o == NULL) Rf_error("rmumps: NULL DMUMPS_STRUC_C pointer");            \
  c = rmumps_abi_mirror(o, o->job == -1);                                    \
  if (c == NULL) {                                                           \
    if (o->job == -2) return;                                                \
    Rf_error(o->job == -1 ? "rmumps: out of memory" : RMUMPS_ABI_NOT_INIT);  \
  }                                                                          \
  abi##k##_copy_in(c, o);                                                    \
  rmumps_dmumps_c_raw(c);                                                    \
  abi##k##_copy_out(o, c);                                                   \
  if (o->job == -2) rmumps_abi_drop(o);                                      \
}                                                                            \
                                                                             \
static int rmumps_dmumps_c_safe_abi##k(T *o, size_t sizeof_id, char *msg,    \
                                       size_t msglen)                        \
{                                                                            \
  DMUMPS_STRUC_C *c;                                                         \
  int rc, job;                                                               \
  rmumps_abi_msg(msg, msglen, "");                                           \
  if (o == NULL) {                                                           \
    rmumps_abi_msg(msg, msglen, "rmumps: NULL DMUMPS_STRUC_C pointer");      \
    return RMUMPS_ERR_NULL;                                                  \
  }                                                                          \
  if (sizeof_id != sizeof(T)) {                                              \
    rmumps_abi_msg(msg, msglen, "rmumps: DMUMPS_STRUC_C size mismatch; "     \
                   "rebuild the calling package against the installed "      \
                   "rmumps");                                                \
    return RMUMPS_ERR_SIZE;                                                  \
  }                                                                          \
  job = o->job;                                                              \
  c = rmumps_abi_mirror(o, job == -1);                                       \
  if (c == NULL) {                                                           \
    if (job == -2) return RMUMPS_OK;                                         \
    rmumps_abi_msg(msg, msglen, job == -1 ? "rmumps: out of memory"          \
                   : RMUMPS_ABI_NOT_INIT);                                   \
    return RMUMPS_ERR_NULL;                                                  \
  }                                                                          \
  abi##k##_copy_in(c, o);                                                    \
  rc = rmumps_dmumps_c_safe(c, sizeof *c, msg, msglen);                      \
  abi##k##_copy_out(o, c);                                                   \
  if (job == -2) rmumps_abi_drop(o);                                         \
  return rc;                                                                 \
}

#endif
