/*
 * Legacy location of the MUMPS C header, kept for clients built against
 * rmumps <= 5.2.1-43 that do '#include <dmumps_c.h>'.
 *
 * New code should include <rmumps/rmumps_api.h> (or <rmumps/dmumps_c.h>).
 * This forwarding header will be removed when rmumps moves to MUMPS 5.9.x,
 * whose DMUMPS_STRUC_C layout differs; a client still including it then fails
 * at compile time instead of silently using a mismatched structure.
 */
#include "rmumps/dmumps_c.h"
