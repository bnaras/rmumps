/*
 * Copyright 2026 Balasubramanian Narasimhan. GPL (>= 2); see inst/COPYRIGHTS.
 *
 * rmumps_cshim.h: compatibility shim for the vendored C sources (MUMPS,
 * PORD, METIS/GKlib).
 *
 * tools/vendor.R (transform T2) includes this header in every vendored .c
 * file, right after the file's last #include. It makes the vendored code
 * conform to "Writing R Extensions" §1.6.4 and §5.7:
 *
 *   - output to stdout/stderr (printf, fprintf, puts, ...) becomes a no-op;
 *     MUMPS reports errors through INFO/INFOG, which rmumps translates;
 *   - exit(), abort() and failed assertions call rmumps_fatal(), which
 *     raises an R error (or, inside a METIS API call, unwinds METIS so that
 *     it returns METIS_ERROR);
 *   - rand()/srand() use a private generator, so the user's RNG state is
 *     never touched;
 *   - sprintf()/vsprintf() go through vsnprintf().
 *
 * The declarations are guarded; the macro redefinitions are not, so that
 * including the header again re-applies them (e.g. after a later
 * <assert.h>, which redefines assert on every inclusion).
 */
#ifndef RMUMPS_CSHIM_H
#define RMUMPS_CSHIM_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#if defined(__GNUC__) || defined(__clang__)
# define RMUMPS_NORETURN __attribute__((noreturn))
#else
# define RMUMPS_NORETURN
#endif

#ifdef __cplusplus
extern "C" {
#endif
void rmumps_fatal(const char *what) RMUMPS_NORETURN;
int rmumps_rand(void);
void rmumps_srand(unsigned int seed);
int rmumps_sprintf(char *buf, const char *fmt, ...);
int rmumps_vsprintf(char *buf, const char *fmt, va_list ap);
#ifdef __cplusplus
}
#endif

#endif /* RMUMPS_CSHIM_H */

#undef printf
#undef vprintf
#undef fprintf
#undef vfprintf
#undef puts
#undef fputs
#undef putchar
#undef fputc
#undef putc
#undef perror
#undef fflush
#undef stdout
#undef stderr
#undef exit
#undef abort
#undef assert
#undef rand
#undef srand
#undef sprintf
#undef vsprintf

#define printf(...)         ((void) 0)
#define vprintf(fmt, ap)    ((void) 0)
#define fprintf(f, ...)     ((void) 0)
#define vfprintf(f, fmt, ap) ((void) 0)
#define puts(s)             ((void) 0)
#define fputs(s, f)         ((void) 0)
#define putchar(c)          ((void) 0)
#define fputc(c, f)         ((void) 0)
#define putc(c, f)          ((void) 0)
#define perror(s)           ((void) 0)
#define fflush(f)           ((void) 0)
#define stdout              ((FILE *) 0)
#define stderr              ((FILE *) 0)

#define exit(status)        rmumps_fatal("exit")
#define abort()             rmumps_fatal("abort")
#ifdef NDEBUG
# define assert(e)          ((void) 0)
#else
# define assert(e)          ((e) ? (void) 0 : rmumps_fatal("assertion " #e))
#endif

#define rand()              rmumps_rand()
#define srand(seed)         rmumps_srand(seed)
#define sprintf             rmumps_sprintf
#define vsprintf            rmumps_vsprintf
