/* Declarations shared by the C and C++ sources of rmumps (not installed). */

#ifndef RMUMPS_INTERNAL_H
#define RMUMPS_INTERNAL_H

#include <stddef.h>
#include "../inst/include/rmumps/dmumps_c.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Calls dmumps_c(id), catching R errors and interrupts; see rmumps_api.h. */
int rmumps_dmumps_c_safe(DMUMPS_STRUC_C *id, size_t sizeof_id, char *msg,
                         size_t msglen);

/* Calls dmumps_c(id) directly (may longjmp). */
void rmumps_dmumps_c_raw(DMUMPS_STRUC_C *id);

/* Makes the next rmumps_dmumps_c_safe() call fail inside its protected
 * region (tests only). */
void rmumps_test_fail_next(void);

/* Registers the C entry points listed in rmumps_api.h. */
void rmumps_register_c_callables(void);

/* Registers the entry points for older DMUMPS_STRUC_C layouts, including
 * the legacy "dmumps_c" (generated: rmumps_abi_adapters.c). */
void rmumps_register_abi_adapters(void);

#ifdef __cplusplus
}
#endif

#endif /* RMUMPS_INTERNAL_H */
