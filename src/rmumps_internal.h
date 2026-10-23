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

/* Registers the C entry points listed in rmumps_api.h. */
void rmumps_register_c_callables(void);

#ifdef __cplusplus
}
#endif

#endif /* RMUMPS_INTERNAL_H */
