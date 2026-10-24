C     rmumps: this file was modified by tools/vendor.R (see src/vendor/README.md)
C
C  This file is part of MUMPS 5.9.1, released
C  on Mon Jul 20 09:00:43 UTC 2026
C
C
C  Copyright 1991-2026 CERFACS, CNRS, ENS Lyon, INP Toulouse, Inria,
C  Mumps Technologies, University of Bordeaux.
C
C  This version of MUMPS is provided to you free of charge. It is
C  released under the CeCILL-C license 
C  (see doc/CeCILL-C_V1-en.txt, doc/CeCILL-C_V1-fr.txt, and
C  https://cecill.info/licences/Licence_CeCILL-C_V1-en.html)
C
      SUBROUTINE MUMPS_PRINT_IF_DEFINED(MPG)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: MPG
      IF (MPG.LE.0) RETURN
C~    write(MPG,*) "================================================="
#if defined(MPI_TO_K_OMP)
#if defined(MTKO_VERBOSE)
C~    write(MPG, *) 
C~   & "MUMPS compiled with option -DMTKO_VERBOSE"
#endif
#endif
#if defined(AFFINITY_VERBOSE)
C~    write(MPG, *) "MUMPS compiled with option -DAFFINITY_VERBOSE"
#endif
#if defined(AVOID_MPI_IN_PLACE)
C~    WRITE(MPG,*)
C~   &  " MUMPS compiled with option -DAVOID_MPI_IN_PLACE"
#endif
#if defined(NO_64BIT_ADDRESSES_IN_C)
C~    WRITE(MPG,*)
C~   &  " MUMPS compiled with option -DNO_64BIT_ADDRESSES_IN_C"
#endif
#if defined(LARGEMATRICES)
C~    write(MPG,*) "MUMPS compiled with option -DLARGEMATRICES"
#endif
#if defined(GEMMT_AVAILABLE)
C~    write(MPG, *) "MUMPS compiled with option -DGEMMT_AVAILABLE"
#endif
#if defined(DETERMINISTIC_PARALLEL_GRAPH)
C~    write(MPG,*) "MUMPS compiled with option"
C~   &     ," -DDETERMINISTIC_PARALLEL_GRAPH"
#endif
#if defined(metis)
C~    write(MPG,*) "MUMPS compiled with option -Dmetis"
#endif
#if defined(metis4)
C~    write(MPG,*) "MUMPS compiled with option -Dmetis4"
#endif
#if defined(MPI_TO_K_OMP)
C~    write(MPG,*) "MUMPS version includes compiled code ",
C~   &             "for MPI_TO_K_OMP"
#  if defined(MTKO_NO_MASK_FUSION)
C~    write(MPG, *) "MUMPS compiled with option -DMTKO_NO_MASK_FUSION"
#  endif
#  if defined(USE_LIBHWLOC)
C~    write(MPG, *) "MUMPS compiled with option -DUSE_LIBHWLOC"
#  endif
#  if defined(USE_SCHEDAFFINITY)
C~    write(MPG, *) "MUMPS compiled with option -DUSE_SCHEDAFFINITY"
#  endif
#  if defined(MTKO_ADVANCED)
C~    write(MPG, *) "MUMPS compiled with option -DMTKO_ADVANCED"
#    if !defined(USE_LIBHWLOC)&&!defined(USE_SCHEDAFFINITY)
C~    write(MPG, *) "MUMPS should not be compiled with -DMTKO_ADVANCED",
C~   &      " without either -DUSE_LIBHWLOC or -DUSE_SCHEDAFFINITY"
#    endif
#  endif
#  if defined(MTKO_ADVANCED_VERBOSE)
#  if ! defined(MTKO_ADVANCED)
C~    write(MPG, *) "MUMPS should not be compiled with ",
C~   &              "-DMTKO_ADVANCED_VERBOSE without -DMTKO_ADVANCED"
      CALL MUMPS_ABORT()
#  else
C~    write(MPG, *) "MUMPS compiled with option -DMTKO_ADVANCED_VERBOSE"
#  endif
#  endif
#endif
#if defined(MUMPS_ALLOC_FROM_C)
C~    WRITE(MPG,*) "MUMPS compiled with option -DMUMPS_ALLOC_FROM_C"
#endif
#if defined(MUMPS_NOF2003)
C~    write(MPG,*) "MUMPS compiled with option -DMUMPS_NOF2003"
#endif
#if defined(MUMPS_SCOTCHIMPORTOMPTHREADS)
C~    write(MPG,*)
C~   &"MUMPS compiled with option -DMUMPS_SCOTCHIMPORTTHREADS"
#endif
#if defined(MUMPS_WINLARGEFILES)
C~    write(MPG,*) "MUMPS compiled with option -DMUMPS_WINLARGEFILES"
#endif
#if defined(MUMPS_USE_BLAS2)
C~    write(MPG,*) "MUMPS compiled with option -DMUMPS_USE_BLAS2"
#endif
#if defined(NOSCALAPACK)
C~    write(MPG,*) "MUMPS compiled with option -DNOSCALAPACK"
#endif
#if defined(OLD_OOC_NOPANEL)
C~    write(MPG,*) "MUMPS compiled with option -DOLD_OOC_NOPANEL"
#endif
#if defined(parmetis)
C~    write(MPG,*) "MUMPS compiled with option -Dparmetis"
#endif
#if defined(parmetis3)
C~    write(MPG,*) "MUMPS compiled with option -Dparmetis3"
#endif
#if defined(pord)
C~    write(MPG,*) "MUMPS compiled with option -Dpord"
#endif
#if defined(PRINT_BACKTRACE_ON_ABORT)
C~    write(MPG, *)
C~   &  "MUMPS compiled with option -DPRINT_BACKTRACE_ON_ABORT"
#endif
#if defined(ptscotch)
C~    write(MPG,*) "MUMPS compiled with option -Dptscotch"
#endif
#if defined(scotch)
C~    write(MPG,*) "MUMPS compiled with option -Dscotch"
#endif
#if defined(NOAGG1)
C~    write(MPG,*) "MUMPS compiled with option -DNOAGG1"
#endif
#if defined(NOAGG2)
C~    write(MPG,*) "MUMPS compiled with option -DNOAGG2"
#endif
#if defined(NOAGG3)
C~    write(MPG,*) "MUMPS compiled with option -DNOAGG3"
#endif
#if defined(NOAGG4)
C~    write(MPG,*) "MUMPS compiled with option -DNOAGG4"
#endif
#if defined(NOAGG5)
C~    write(MPG,*) "MUMPS compiled with option -DNOAGG5"
#endif
#if defined(NOAMALGTOFATHER)
C~    write(MPG,*) "MUMPS compiled with -DNOAMALGTOFATHER"
#endif
#if defined(NO_FDM_DESCBAND)
C~    write(MPG,*) "MUMPS compiled with -DNO_FDM_DESCBAND"
#endif
#if defined(NO_FDM_MAPROW)
C~    write(MPG,*) "MUMPS compiled with -DNO_FDM_MAPROW"
#endif
#if defined(NO_SAVE_RESTORE)
C~    write(MPG,*) "MUMPS compiled with -DNO_SAVE_RESTORE"
#endif
#if defined(NO_SPLIT_IN_BLRGROUPING)
C~    write(MPG,*) "MUMPS compiled with -DNO_SPLIT_IN_BLRGROUPING"
#endif
#if defined(NODYNAMICCB)
C~    write(MPG,*) "MUMPS compiled with option -DNODYNAMICCB"
#endif
#if defined(USE_GPU)
C~    write(MPG, *) "MUMPS compiled with option -DUSE_GPU"
#  if defined(USE_XKBLAS)
C~    write(MPG, *) "MUMPS compiled with option -DUSE_XKBLAS"
#  endif
#  if defined(USE_XKBLAS_COPYSCALE)
C~    write(MPG, *) "MUMPS compiled with option -DUSE_XKBLAS_COPYSCALE"
#  endif
#  if defined(GEMMT_AVAILABLE_FOR_GPU)
C~    write(MPG, *) "MUMPS compiled with option ",
C~   &              "-DGEMMT_AVAILABLE_FOR_GPU"
#  endif
#endif
#if defined(USE_OLD_SCALING)
C~    write(MPG,*) "MUMPS compiled with option -DUSE_OLD_SCALING"
#endif
#if defined(VHOFFLOAD)
C~    write(MPG,*) "MUMPS compiled with -DVHOFFLOAD"
#endif
#if defined(WORKAROUNDINTELILP64MPI2INTEGER)
C~    write(MPG,*) "MUMPS compiled with option"
C~   &     ," -DWORKAROUNDINTELILP64MPI2INTEGER"
#endif
#if defined(WORKAROUNDILP64MPICUSTOMREDUCE)
C~    write(MPG,*) "MUMPS compiled with option"
C~   &     ," -DWORKAROUNDILP64MPICUSTOMREDUCE"
#endif
#if defined(WORKAROUNDINTELILP64OPENMPLIMITATION)
C~    write(MPG,*) "MUMPS compiled with option"
C~   &     ," -DWORKAROUNDINTELILP64OPENMPLIMITATION"
#endif
#if defined(ZERO_TRIANGLE)
C~    write(MPG,*) "MUMPS compiled with option -DZERO_TRIANGLE"
#endif
#if defined(SINGLEINDOUBLE)
C~    write(MPG,*) "MUMPS compiled with option -DSINGLEINDOUBLE"
#endif
#if defined(BLR_NOOPENMP)
C~    write(MPG,*) "MUMPS compiled with option -DBLR_NOOPENMP"
#endif
C~    write(MPG,*) 
C~   &   "This MUMPS version includes code for mixed precision BLR"
#if ! defined(NOMIXHPC)
C~   &   ," including memory accessors during solve"
#endif
#if defined(NOMIXHPC)
C~    write(MPG,*) "MUMPS compiled with NOMIXHPC"
#endif
#if defined(STAT_ES_SOLVE)
C~    write(MPG,*) "MUMPS compiled with option -DSTAT_ES_SOLVE"
#endif
#if defined(USE_AVX512_VBMI)
C~    write(MPG,*) "MUMPS compiled with option -DUSE_AVX512_VBMI"
#endif
#if defined(nrm2_returns_double)
C~    write(MPG,*) "MUMPS compiled with option -Dnrm2_returns_double"
#endif
C~    write(MPG,*) "================================================="
      RETURN
      END SUBROUTINE MUMPS_PRINT_IF_DEFINED
