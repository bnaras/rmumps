/*
 * Copyright 2026 Balasubramanian Narasimhan. GPL (>= 2); see inst/COPYRIGHTS.
 *
 * rmumps_api.h -- C interface to the MUMPS solver shipped in the rmumps
 * package, for use from other R packages.
 *
 * Usage (C or C++):
 *
 *   DESCRIPTION:  Imports: rmumps   LinkingTo: rmumps
 *   source:       #include <rmumps/rmumps_api.h>
 *
 *   static rmumps_api mumps;              // one per client package
 *
 *   void R_init_mypkg(DllInfo *dll) {     // or a .Call made from .onLoad
 *     ...
 *     rmumps_api_load(&mumps);            // R error if rmumps is too old
 *   }
 *
 *   DMUMPS_STRUC_C id;
 *   char msg[256];
 *   memset(&id, 0, sizeof id);
 *   id.job = -1; id.par = 1; id.sym = 0; id.comm_fortran = -987654;
 *   if (RMUMPS_DMUMPS_C(&mumps, &id, msg, sizeof msg) != RMUMPS_OK)
 *     ...                                 // msg says what happened
 *   if (id.info[0] < 0)
 *     ...                                 // a MUMPS error, see INFO(1:2)
 *
 * Contract:
 *
 * - Call MUMPS from R's main thread only, one call at a time, process-wide:
 *   the sequential MUMPS library keeps file-static state.
 * - Indices are 1-based. MUMPS_INT is 32-bit. Prefer 'nnz' (64-bit) to 'nz'.
 * - RMUMPS_DMUMPS_C (the "safe" entry point) never returns by longjmp: an R
 *   error or a user interrupt raised while MUMPS runs is caught and reported
 *   through the return value and 'msg'. This makes it callable from C++
 *   frames. The bundled MUMPS prints nothing; ICNTL(1:4) have no visible
 *   effect, and errors are reported only through INFO/INFOG.
 * - After such a failure the instance is "poisoned": INFO(1) is set to
 *   RMUMPS_INFO_POISONED (-99999) and INFO(2) to the return code. Further
 *   calls on it return RMUMPS_ERR_POISONED, and JOB = -2 returns without
 *   calling MUMPS (the instance's memory is leaked rather than freed from an
 *   inconsistent state). Start again with JOB = -1 on a zeroed structure.
 * - The raw entry point ('dmumps_c' member) is MUMPS' own dmumps_c: R errors
 *   raised inside it longjmp. Use it only from plain C frames that expect
 *   R semantics.
 * - Inertia and null pivots are available as usual through INFOG(12) and
 *   INFOG(28) in the structure.
 *
 * Versioning:
 *
 * Every entry point is registered under a name that carries the layout of
 * DMUMPS_STRUC_C it was built for (RMUMPS_DMUMPS_ABI). A client compiled
 * against headers of another layout cannot resolve it and gets R's clean
 * "function ... not provided by package 'rmumps'" error at load time instead
 * of memory corruption. The size of the client's structure is checked too.
 * When the layout changes, the entry points of the older layouts (and the
 * legacy name "dmumps_c" of rmumps <= 5.2.1-43) stay registered and reach
 * the current MUMPS through generated adapters, so existing client binaries
 * keep working; members that the old layout lacks keep MUMPS' defaults.
 *
 * Reserved for a possible later opaque-handle API (not implemented): names
 * starting with rmumps_handle_.
 */

#ifndef RMUMPS_API_H
#define RMUMPS_API_H

#include <stddef.h>
#include <R_ext/Rdynload.h>
#include "dmumps_c.h"
/* RMUMPS_DMUMPS_ABI: number of the DMUMPS_STRUC_C layout in dmumps_c.h
 * (1 = MUMPS 5.2.1, 2 = MUMPS 5.9.1; see the generated rmumps_abi.h). It
 * changes only when the layout changes, i.e. when existing binaries would
 * otherwise break; rmumps keeps serving the older layouts through
 * adapters. */
#include "rmumps_abi.h"

/* Version of this header's interface; grows when entry points are added. */
#define RMUMPS_API_VERSION 1

#define RMUMPS_STR_(x) #x
#define RMUMPS_STR(x) RMUMPS_STR_(x)

#define RMUMPS_NAME_DMUMPS_C_SAFE \
  "rmumps_dmumps_c_safe_abi" RMUMPS_STR(RMUMPS_DMUMPS_ABI)
#define RMUMPS_NAME_DMUMPS_C "rmumps_dmumps_c_abi" RMUMPS_STR(RMUMPS_DMUMPS_ABI)

/* Return codes of the safe entry point. */
#define RMUMPS_OK 0
#define RMUMPS_ERR_NULL 1        /* NULL structure pointer */
#define RMUMPS_ERR_SIZE 2        /* sizeof(DMUMPS_STRUC_C) differs */
#define RMUMPS_ERR_R 3           /* R error raised while MUMPS ran */
#define RMUMPS_ERR_INTERRUPT 4   /* user interrupt while MUMPS ran */
#define RMUMPS_ERR_POISONED 5    /* instance failed earlier, see above */

#define RMUMPS_INFO_POISONED (-99999)

/* Bits of rmumps_orderings_available(): bit k is set when ICNTL(7) = k is
 * supported. */
#define RMUMPS_ORDERING_BIT(k) (1 << (k))

typedef int (*rmumps_dmumps_c_safe_fn)(DMUMPS_STRUC_C *id, size_t sizeof_id,
                                       char *msg, size_t msglen);
typedef void (*rmumps_dmumps_c_fn)(DMUMPS_STRUC_C *id);

typedef struct rmumps_api {
  rmumps_dmumps_c_safe_fn dmumps_c_safe;
  rmumps_dmumps_c_fn dmumps_c;
  int (*api_version)(void);         /* RMUMPS_API_VERSION of the library */
  const char *(*mumps_version)(void);  /* e.g. "5.9.1" */
  int (*orderings_available)(void);    /* RMUMPS_ORDERING_BIT mask */
} rmumps_api;

/* Resolve all entry points. Raises an R error (longjmp) if rmumps does not
 * provide them, so call it where that is safe: R_init_<pkg> or a .Call
 * made from .onLoad, not from C++ code with live destructors. */
static inline void rmumps_api_load(rmumps_api *api)
{
  api->dmumps_c_safe = (rmumps_dmumps_c_safe_fn)
    R_GetCCallable("rmumps", RMUMPS_NAME_DMUMPS_C_SAFE);
  api->dmumps_c = (rmumps_dmumps_c_fn)
    R_GetCCallable("rmumps", RMUMPS_NAME_DMUMPS_C);
  api->api_version = (int (*)(void))
    R_GetCCallable("rmumps", "rmumps_api_version");
  api->mumps_version = (const char *(*)(void))
    R_GetCCallable("rmumps", "rmumps_mumps_version");
  api->orderings_available = (int (*)(void))
    R_GetCCallable("rmumps", "rmumps_orderings_available");
}

/* Safe call: returns RMUMPS_OK or one of the RMUMPS_ERR_* codes, with a
 * message in msg (which may be NULL when msglen is 0). */
#define RMUMPS_DMUMPS_C(api, id, msg, msglen) \
  ((api)->dmumps_c_safe((id), sizeof(DMUMPS_STRUC_C), (msg), (msglen)))

#endif /* RMUMPS_API_H */
