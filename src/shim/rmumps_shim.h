/* Copyright 2026 Balasubramanian Narasimhan. GPL (>= 2); see inst/COPYRIGHTS. */
/* Declarations of the shim run-time used by rmumps' own code. */
#ifndef RMUMPS_SHIM_H
#define RMUMPS_SHIM_H

#include <stdarg.h>

#if defined(__GNUC__) || defined(__clang__)
# define RMUMPS_NORETURN __attribute__((noreturn))
#else
# define RMUMPS_NORETURN
#endif

#ifdef __cplusplus
extern "C" {
#endif
void rmumps_fatal(const char *what) RMUMPS_NORETURN;
void rmumps_rng_reset(void);
int rmumps_rand(void);
void rmumps_srand(unsigned int seed);
double rmumps_unif(void);
int rmumps_sprintf(char *buf, const char *fmt, ...);
int rmumps_vsprintf(char *buf, const char *fmt, va_list ap);
#ifdef __cplusplus
}
#endif

#endif
