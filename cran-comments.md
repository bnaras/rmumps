## Submission

rmumps 5.9.1-1 updates the bundled MUMPS from 5.2.1 to 5.9.1.

- The bundled MUMPS, PORD and METIS 5.1.0 sources are generated from the
  pristine upstream tarballs by a reproducible pipeline kept in the source
  repository (`tools/`, excluded from the tarball): pinned checksums,
  numbered patches with a stated reason each, and transforms that remove
  all console output, process exits, file access and use of the compiler or
  C library random generators from the compiled code.
- The compiled code calls none of the entry points listed in "Writing R
  Extensions" §1.6.4 (checked with `tools:::check_compiled_code` and a
  stricter symbol lint covering gfortran and LLVM flang runtime names).
- The Fortran sources no longer need `-fallow-argument-mismatch`: the
  argument mismatches of MUMPS were fixed by interfaces and patches, and the
  package compiles without significant warnings under `-Wall -pedantic`
  with gfortran 16, gcc 16 and clang 21. They also compile with LLVM
  flang 21.
- SCOTCH is no longer bundled.
- Copyright holders of all bundled code are listed in `Authors@R` and
  `inst/COPYRIGHTS`; the CeCILL-C (MUMPS) and Apache-2.0 (METIS) license
  texts are included in `src/vendor`.

## Test environments

- macOS 26.6 (arm64), R 4.6.1, Apple clang 21, GNU Fortran 16.2:
  `R CMD check --as-cran` with `_R_CHECK_COMPILATION_FLAGS_=true`; also
  builds with `-Wall -pedantic` (clang and gcc 16 as C compiler), a
  `-fcheck=pointer,array-temps` Fortran build, and the C/C++ code under
  AddressSanitizer and UndefinedBehaviorSanitizer.
- Debian testing (arm64, in a container), R 4.6.1:
  - gcc/gfortran 16.2 with the CRAN Debian flags
    (`-g -O2 -Wall -pedantic -mtune=native`): `R CMD check --as-cran`
    clean apart from a WARNING about the `-Wno-*` C++ flags that were set
    in the local `~/.R/Makevars` to mimic the CRAN configuration;
  - clang 21 and LLVM flang 21 (`flang-21`, `libflang_rt.runtime`):
    `R CMD check --as-cran` Status OK;
  - gcc ASan/UBSan, Fortran included (package built with
    `-fsanitize=address,undefined`, R preloading libasan): no reports in
    the tests and examples;
  - valgrind 3.27 memcheck on the tests and examples: no errors, no
    leaks;
  - an LTO build emulating the CRAN LTO check (`-flto` for C, C++ and
    Fortran, `gcc-ar`): no `-Wlto-type-mismatch` warning, tests pass
    against that build.
- To be run before upload (not yet done): win-builder (R-devel, R-release)
  and mac-builder.

## R CMD check results

0 errors | 0 warnings | 1 note (local, macOS)

- "installed package size" may be reported on some platforms: the library
  bundles MUMPS, PORD and METIS.

## Reverse dependencies

- Uno, r2sundials, multbxxc (Imports/LinkingTo): their tests pass both when
  built from source against this version and, as binaries built against
  rmumps 5.2.1-43, when run with this version. Binaries built against older
  versions keep working because the C entry points of the older
  `DMUMPS_STRUC_C` layout (including the legacy name "dmumps_c") are served
  through generated adapters.
- Uno 2.7.4-1 built from source against this version stops at its
  configure step, because the top-level copies of the MUMPS headers were
  removed (their layout changed). This submission is meant to follow an
  Uno release that uses the new versioned C interface
  `<rmumps/rmumps_api.h>` (TO BE CONFIRMED before upload).
- conos and sccore (Suggests): their only use of rmumps,
  `Rmumps$new(laplacian, copy = FALSE)$solve(rhs)`, gives the same results
  as before and as `Matrix::solve()`.
