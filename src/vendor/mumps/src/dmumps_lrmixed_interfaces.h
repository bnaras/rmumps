C     rmumps: this file was modified by tools/vendor.R (see src/vendor/README.md)
C
C     rmumps: explicit interfaces of the mixed-precision dispatch
C     routines of dlr_core.F, for callers in other files. Their matrix
C     arguments hold data of any precision and are only passed on to
C     the conversion and BLAS kernels, so they are exempt from type,
C     kind and rank checking. The definitions carry the !GCC$ and !DEC$
C     attributes only: flang accepts IGNORE_TKR in interfaces and module
C     procedures, not in the definition of an external procedure.
C
      INTERFACE
      SUBROUTINE DMUMPS_LRMIXED_CAST(A, B, nrows, ncols, ldA, ldB,
     &                               precA, precB)
!GCC$ ATTRIBUTES NO_ARG_CHECK :: A, B
!DEC$ ATTRIBUTES NO_ARG_CHECK :: A, B
!DIR$ IGNORE_TKR (TKRC) A, B
      INTEGER :: A(*), B(*)
      INTEGER :: nrows, ncols, ldA, ldB, precA, precB
      END SUBROUTINE DMUMPS_LRMIXED_CAST
      SUBROUTINE DMUMPS_LRMIXED_PLUS(X, Y, nrows, ncols, ldX, ldY,
     &                               precX, precY)
!GCC$ ATTRIBUTES NO_ARG_CHECK :: X, Y
!DEC$ ATTRIBUTES NO_ARG_CHECK :: X, Y
!DIR$ IGNORE_TKR (TKRC) X, Y
      INTEGER :: X(*), Y(*)
      INTEGER :: nrows, ncols, ldX, ldY, precX, precY
      END SUBROUTINE DMUMPS_LRMIXED_PLUS
      SUBROUTINE DMUMPS_LRMIXED_GEMM(TRANSA, TRANSB, M, N, K, ALPHA,
     &                   A, LDA, B, LDB, BETA, C, LDC, prec)
!GCC$ ATTRIBUTES NO_ARG_CHECK :: A, B, C
!DEC$ ATTRIBUTES NO_ARG_CHECK :: A, B, C
!DIR$ IGNORE_TKR (TKRC) A, B, C
      INTEGER :: A(*), B(*), C(*)
      INTEGER :: M, N, K, LDA, LDB, LDC, prec
      DOUBLE PRECISION :: ALPHA, BETA
      CHARACTER(len=1) :: TRANSA, TRANSB
      END SUBROUTINE DMUMPS_LRMIXED_GEMM
      END INTERFACE
