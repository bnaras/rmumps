# Maintainer tools

Not part of the package tarball (`^tools$` is in `.Rbuildignore`). Run every
command from the package root.

| File | Purpose |
|------|---------|
| `vendor.lock` | Pinned upstream tarballs: version, URL, mirror, SHA-256 |
| `manifest/<component>.dcf` | File selection, preprocessor defines, T1 allowlists |
| `patches/<component>/NNNN-*.patch` | Changes to the **pristine** sources, applied in order |
| `transforms/` | T1 (Fortran I/O), T2 (C shim), T3 (object list, headers, ABI), with `tests/run.R` |
| `abi/` | `DMUMPS_STRUC_C` layout records, ABI registry, adapter rules |
| `logs/` | What T1 did, per routine (reviewed in diffs) |
| `lint-symbols.R` | Forbidden-symbol lint of a build |
| `golden/make-golden.R` | Reference results for `tests/testthat/test_golden.R` |

## What `generate` produces

From the pinned tarballs only (nothing in `upstream/` is used):

- `src/vendor/mumps`, `src/vendor/metis`: the selected, patched and
  transformed sources (committed; never edit them by hand);
- `src/Makevars.vendor`: object lists, explicit `.F` rules, Fortran module
  dependencies;
- `inst/include/rmumps/{dmumps_c.h,mumps_c_types.h,mumps_compat.h,mumps_int_def.h,rmumps_abi.h}`
  (`rmumps_api.h` there is hand-written);
- `src/rmumps_abi_adapters.c` and `tools/abi/*`: see "Layout changes".

`verify` regenerates into a temporary directory and fails on any
difference, so a hand edit in `src/vendor` cannot go unnoticed.

## Importing a new MUMPS release

Cadence: every x.y.0 release; x.y.z only for fixes that matter to a
sequential double precision build. Expect 1–3 days per minor release.

1. Edit `tools/vendor.lock` (version, URL, SHA-256; cross-check the hash with
   conda-forge's `mumps` recipe), then `Rscript tools/vendor.R fetch`.
2. `Rscript tools/vendor.R generate`. It stops on:
   - a patch that no longer applies: refit it against the new pristine file
     (`git apply --3way` in a scratch repository holding the old pristine
     tree as the base helps), keep its header up to date, or drop it if its
     "Obsolete when" test is met;
   - a T1 failure (an I/O statement it does not understand, a new
     RANDOM_NUMBER site, file I/O outside an IO-disabled routine, a new
     output unit): fix the manifest or add a patch. Never weaken T1 to make
     a failure go away.
   - a changed object list in upstream's `src/Makefile` is picked up
     automatically; check `src/Makevars.vendor` in the diff.
3. Review `git diff --stat` and `tools/logs/mumps-t1.log`.
4. Build and lint:
   `R CMD INSTALL --preclean --no-clean-on-error -l <lib> .` then
   `Rscript tools/lint-symbols.R <lib>/rmumps` (must report nothing).
5. Tests: `Rscript tools/transforms/tests/run.R`, the package tests,
   `Rscript tests/clients/run-client-tests.R <lib>`, and the reverse
   dependencies (Uno, r2sundials, multbxxc, conos, sccore), both from source
   and as binaries built against the previous rmumps.
6. Review the ICNTL defaults set in `src/rmumps_module.cpp` against the new
   user guide, update `NEWS`, and run `R CMD check --as-cran`.

## Layout changes (ABI)

`generate` measures `DMUMPS_STRUC_C` with a small C program. If the layout
differs from the newest entry of `tools/abi/registry.dcf`, it records the new
layout as the next ABI number, writes `RMUMPS_DMUMPS_ABI` into
`inst/include/rmumps/rmumps_abi.h`, and generates adapters from every older
layout into `src/rmumps_abi_adapters.c`, so that binaries built against older
headers keep working. A member that exists only in an old layout needs a rule
in `tools/abi/rules.dcf` (generation stops until it has one); a member whose
type changed needs a code change in `tools/transforms/abi.R`. Never edit or
remove registry entries. Mention the new ABI in `NEWS`: clients built from
source against the new headers bind to the new names automatically.

## Hand edits under deadline

If a fix must go into `src/vendor` directly (e.g. a CRAN deadline), edit the
file, then record it as a patch against pristine with

    Rscript tools/vendor.R capture <component> <short-name>

and fill in the patch header (Why / Upstream / Obsolete when). `capture`
refuses edits that overlap lines changed by a transform; write those patches
by hand. Finish with `generate` and `verify`.

## Patch headers

Every patch starts with:

    Why: <reason>
    Upstream: <not sent | sent YYYY-MM-DD | merged in x.y.z>
    Obsolete when: <test that tells when the patch can be dropped>

## "CRAN emailed me"

- *Compiled code calls exit/printf/stderr/...*: run `lint-symbols.R` on a
  build with the flavor's compiler; the per-object listing points at the
  source. Add a T2 shim macro or a patch, regenerate, re-lint.
- *Compiler warnings*: reproduce with that flavor's flags (CRAN's Debian
  flags are `-g -O2 -Wall -pedantic -mtune=native`); fixes are patches.
- *A new upstream release is needed*: follow "Importing" above.
