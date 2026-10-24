/* rmumps: this file was modified by tools/vendor.R (see src/vendor/README.md) */
/*!
\file  error.c
\brief Various error-handling functions

This file contains functions dealing with error reporting and termination

\author George
\date 1/1/2007
\version\verbatim $Id: error.c 10711 2011-08-31 22:23:04Z karypis $ \endverbatim
*/


#define _GK_ERROR_C_  /* this is needed to properly declare the gk_jub* variables
                         as an extern function in GKlib.h */

#include <GKlib.h>
#include "rmumps_cshim.h" /* rmumps: inserted by tools/vendor.R */


/* These are the jmp_buf for the graceful exit in case of severe errors.
   Multiple buffers are defined to allow for recursive invokation. */
#define MAX_JBUFS 128
__thread int gk_cur_jbufs=-1;
__thread jmp_buf gk_jbufs[MAX_JBUFS];
__thread jmp_buf gk_jbuf;

/* rmumps: the holders of the old signal handlers are gone (no handlers are
   installed), and gk_errexit() always unwinds. */
static int gk_exit_on_error = 1;


/*************************************************************************/
/*! This function sets the gk_exit_on_error variable 
 */
/*************************************************************************/
void gk_set_exit_on_error(int value)
{
  gk_exit_on_error = value;
}



/*************************************************************************/
/*! This function prints an error message and exits  
 */
/*************************************************************************/
void errexit(char *f_str,...)
{
  /* rmumps: no output; unwind to the running METIS API function, which
     then returns METIS_ERROR (rmumps_fatal() does that when
     gk_cur_jbufs >= 0, and raises an R error otherwise). */
  (void) f_str;
  rmumps_fatal("METIS errexit");
}


/*************************************************************************/
/*! This function prints an error message and raises a signum signal
 */
/*************************************************************************/
void gk_errexit(int signum, char *f_str,...)
{
  /* rmumps: no output and no signal; longjmp straight to the point the
     running METIS API function returns METIS_ERROR* from. */
  (void) f_str;
  if (gk_cur_jbufs >= 0)
    longjmp(gk_jbufs[gk_cur_jbufs], signum);
  rmumps_fatal("METIS gk_errexit");
}


/***************************************************************************/
/*! This function sets a number of signal handlers and sets the return point 
    of a longjmp
*/
/***************************************************************************/
int gk_sigtrap() 
{
  if (gk_cur_jbufs+1 >= MAX_JBUFS)
    return 0;

  gk_cur_jbufs++;

  /* rmumps: no signal handlers (gk_errexit() longjmps directly) */

  return 1;
}
  

/***************************************************************************/
/*! This function sets the handlers for the signals to their default handlers
 */
/***************************************************************************/
int gk_siguntrap() 
{
  if (gk_cur_jbufs == -1)
    return 0;

  gk_cur_jbufs--;

  return 1;
}
  

/*************************************************************************/
/*! This function is the custome signal handler, which all it does is to
    perform a longjump to the most recent saved environment 
 */
/*************************************************************************/
void gk_sigthrow(int signum)
{
  longjmp(gk_jbufs[gk_cur_jbufs], signum);
}
  

/***************************************************************************
* This function sets a number of signal handlers and sets the return point 
* of a longjmp
****************************************************************************/
void gk_SetSignalHandlers()
{
  /* rmumps: not used by METIS; never install signal handlers */
}
  

/***************************************************************************
* This function sets the handlers for the signals to their default handlers
****************************************************************************/
void gk_UnsetSignalHandlers()
{
}
  

/*************************************************************************
* This function is the handler for SIGUSR1 that implements the cleaning up 
* process prior to a non-local exit.
**************************************************************************/
void gk_NonLocalExit_Handler(int signum)
{
  longjmp(gk_jbuf, signum);
}
  

/*************************************************************************/
/*! \brief Thread-safe implementation of strerror() */
/**************************************************************************/
char *gk_strerror(int errnum)
{
#if defined(WIN32) || defined(__MINGW32__)
  return strerror(errnum);
#else 
#ifndef SUNOS
  static __thread char buf[1024];

  strerror_r(errnum, buf, 1024);

  buf[1023] = '\0';
  return buf;
#else
  return strerror(errnum);
#endif
#endif
}



/*************************************************************************
* This function prints a backtrace of calling functions
**************************************************************************/
void PrintBackTrace()
{
#ifdef HAVE_EXECINFO_H
  void *array[10];
  int i, size;
  char **strings;

  size = backtrace(array, 10);
  strings = backtrace_symbols(array, size);
  
  printf("Obtained %d stack frames.\n", size);
  for (i=0; i<size; i++) {
    printf("%s\n", strings[i]);
  }
  free(strings);
#endif
}
