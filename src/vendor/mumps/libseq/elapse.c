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
#if defined(_WIN32)
#include  "elapse.h"
#include "rmumps_cshim.h" /* rmumps: inserted by tools/vendor.R */
#include  <time.h>
#include "rmumps_cshim.h" /* rmumps: inserted by tools/vendor.R */
#include  <sys/timeb.h>
#include "rmumps_cshim.h" /* rmumps: inserted by tools/vendor.R */
void MUMPS_CALL mumps_elapse(double *val)
{
  time_t	ltime;
  struct    _timeb	tstruct;

  time (&ltime);
  _ftime(&tstruct);
  *val = (double) ltime + (double) tstruct.millitm*(0.001);
}

#else

#include "elapse.h"
#include "rmumps_cshim.h" /* rmumps: inserted by tools/vendor.R */
#include <sys/time.h>
#include "rmumps_cshim.h" /* rmumps: inserted by tools/vendor.R */
void mumps_elapse(double *val)
  {
    struct timeval time;
    gettimeofday(&time,(struct timezone *)0);
    *val=time.tv_sec+time.tv_usec*1.e-6;
  }
#endif
