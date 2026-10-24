/* rmumps: this file was modified by tools/vendor.R (see src/vendor/README.md) */
/*
 *
 *  This file is part of MUMPS 5.9.1, released
 *  on Mon Jul 20 09:00:43 UTC 2026
 *
 *
 *  Copyright 1991-2026 CERFACS, CNRS, ENS Lyon, INP Toulouse, Inria,
 *  Mumps Technologies, University of Bordeaux.
 *
 *  This version of MUMPS is provided to you free of charge. It is
 *  released under the CeCILL-C license 
 *  (see doc/CeCILL-C_V1-en.txt, doc/CeCILL-C_V1-fr.txt, and
 *  https://cecill.info/licences/Licence_CeCILL-C_V1-en.html)
 *
 */
#include <stdio.h>
#include "rmumps_cshim.h" /* rmumps: inserted by tools/vendor.R */
#include <stdlib.h>
#include "rmumps_cshim.h" /* rmumps: inserted by tools/vendor.R */
#include <string.h>
#include "rmumps_cshim.h" /* rmumps: inserted by tools/vendor.R */
#include "mumps_config_file_C.h"
#include "rmumps_cshim.h" /* rmumps: inserted by tools/vendor.R */
#include "mumps_common.h"
#include "rmumps_cshim.h" /* rmumps: inserted by tools/vendor.R */
/* Functions */
void MUMPS_CALL
MUMPS_GET_CONFIG_FILE_C(MUMPS_INT *len_config_file, char* config_file, mumps_ftnlen l1)
{
  char *tmp_config_file;
  /* rmumps: configuration files are not supported (the library must not
     read files that the R user did not name); behave as if
     MUMPS_CONFIG_FILE were unset. */
  tmp_config_file = "NAME_NOT_INITIALIZED";
  *len_config_file = strlen(tmp_config_file);
  config_file = strncpy(config_file, tmp_config_file, l1); 
}
void MUMPS_CALL
MUMPS_CONFIG_FILE_RETURN_C(void)
{
  /*
  This feature will be available in the future
  */
}
