/*
 * Run-time support for rmumps_cshim.h and for the Fortran replacements
 * made by tools/vendor.R (transform T1).
 */
#include <limits.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <R_ext/Error.h>
#include <R_ext/RS.h>

#include "rmumps_shim.h"

/* GKlib's error-recovery state (GKlib/error.c). While a METIS API function
 * runs, gk_cur_jbufs >= 0 and gk_jbufs[gk_cur_jbufs] is the point it
 * returns METIS_ERROR from. SIGTERM is GKlib's SIGERR. */
extern __thread int gk_cur_jbufs;
extern __thread jmp_buf gk_jbufs[];

void rmumps_fatal(const char *what)
{
  if (gk_cur_jbufs >= 0)
    longjmp(gk_jbufs[gk_cur_jbufs], SIGTERM);
  Rf_error("rmumps: unrecoverable error in the MUMPS library (%s)", what);
}

/* Private generator: xorshift64*. The state is reset before every MUMPS
 * call (rmumps_rng_reset()), so results do not depend on call history and
 * the user's R RNG state is never used or changed. */
#define RMUMPS_RNG_SEED UINT64_C(0x9E3779B97F4A7C15)
static uint64_t rng_state = RMUMPS_RNG_SEED;

static uint64_t rng_next(void)
{
  uint64_t x = rng_state;
  x ^= x >> 12;
  x ^= x << 25;
  x ^= x >> 27;
  rng_state = x;
  return x * UINT64_C(2685821657736338717);
}

void rmumps_rng_reset(void)
{
  rng_state = RMUMPS_RNG_SEED;
}

int rmumps_rand(void)
{
  return (int) ((rng_next() >> 33) % ((uint64_t) RAND_MAX + 1u));
}

void rmumps_srand(unsigned int seed)
{
  rng_state = seed ? (uint64_t) seed * RMUMPS_RNG_SEED : RMUMPS_RNG_SEED;
  if (!rng_state) rng_state = RMUMPS_RNG_SEED;
}

/* Uniform on the open interval (0, 1): callers compute ceiling(r * n) and
 * must never get 0. */
double rmumps_unif(void)
{
  return ((double) (rng_next() >> 11) + 0.5) * (1.0 / 9007199254740992.0);
}

/* Replacement for CALL RANDOM_NUMBER(X), X a DOUBLE PRECISION scalar. */
void F77_NAME(rmumps_random_number)(double *x)
{
  *x = rmumps_unif();
}

int rmumps_vsprintf(char *buf, const char *fmt, va_list ap)
{
  return vsnprintf(buf, (size_t) INT_MAX, fmt, ap);
}

int rmumps_sprintf(char *buf, const char *fmt, ...)
{
  va_list ap;
  int n;
  va_start(ap, fmt);
  n = vsnprintf(buf, (size_t) INT_MAX, fmt, ap);
  va_end(ap);
  return n;
}
