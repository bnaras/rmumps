#!/usr/bin/env Rscript
# Copyright 2026 Balasubramanian Narasimhan. GPL (>= 2); see inst/COPYRIGHTS.
#
# Forbidden-symbol lint for the compiled package.
#
# Usage (from the package root, after a build that left the objects in src/):
#   Rscript tools/lint-symbols.R [installed-package-dir]
#
# 1. Runs R's own check (tools:::check_compiled_code) on the installed
#    package, if its directory is given.
# 2. Lists, per object file under src/, every undefined symbol that would
#    write to stdout/stderr, terminate the process, do Fortran I/O, use a
#    compiler-runtime RNG, or belong to another MUMPS arithmetic. The list
#    covers gfortran and LLVM flang runtime names, some of which R's own
#    table does not have.
# Exits with status 1 if anything is found.

invisible(Sys.setlocale("LC_ALL", "C"))

FORBIDDEN <- c(
  # C library
  "printf", "vprintf", "puts", "putchar", "perror", "fprintf", "vfprintf",
  "fputs", "fputc", "putc", "sprintf", "vsprintf",
  "exit", "_exit", "_Exit", "abort", "rand", "srand", "random", "srandom",
  "drand48", "srand48", "system",
  "stdout", "stderr", "__stdoutp", "__stderrp", "__iob_func", "__acrt_iob_func",
  "__assert", "__assert_fail", "__assert_rtn", "_assert", "__assert2",
  # gfortran runtime
  "_gfortran_st_write", "_gfortran_st_write_done", "_gfortran_st_read",
  "_gfortran_st_read_done", "_gfortran_st_open", "_gfortran_st_close",
  "_gfortran_st_inquire", "_gfortran_st_flush", "_gfortran_st_rewind",
  "_gfortran_st_backspace", "_gfortran_st_endfile",
  "_gfortran_stop_string", "_gfortran_stop_numeric", "_gfortran_stop_numeric_f08",
  "_gfortran_error_stop_string", "_gfortran_error_stop_numeric",
  "_gfortran_random_r4", "_gfortran_random_r8", "_gfortran_arandom_r4",
  "_gfortran_arandom_r8", "_gfortran_random_seed_i4", "_gfortran_random_seed_i8",
  "_gfortran_exit_i4", "_gfortran_exit_i8", "_gfortran_abort",
  "_gfortran_flush_i4", "_gfortran_flush_i8", "_gfortran_system")
FORBIDDEN_RE <- c(
  "^_FortranAio",          # flang I/O runtime (Begin*, Output*, Input*, Inquire*, ...)
  "^_FortranA(Exit|StopStatement|StopStatementText|FailImageStatement|Abort|PauseStatement)",
  "^_FortranARandom",
  "^_gfortran_transfer_",  # data transfer of a Fortran I/O statement
  "^[scz]mumps_", "^[SCZ]MUMPS_", "^__[scz]mumps_")

strip_us <- function(s) {
  # macOS prefixes C symbols with "_"
  if (Sys.info()[["sysname"]] == "Darwin") sub("^_", "", s) else s
}

undefined <- function(obj) {
  out <- suppressWarnings(system2("nm", c("-u", shQuote(obj)), stdout = TRUE, stderr = FALSE))
  out <- trimws(sub("^ *U +", "", out))
  strip_us(out[nzchar(out)])
}

args <- commandArgs(trailingOnly = TRUE)
bad <- FALSE

if (length(args)) {
  cat("R's check_compiled_code on", args[1L], ":\n")
  r <- tools:::check_compiled_code(args[1L])
  if (length(r)) { print(r); bad <- TRUE } else cat("  OK\n")
}

objs <- sort(list.files("src", pattern = "\\.o$", recursive = TRUE, full.names = TRUE),
             method = "radix")
if (!length(objs)) stop("no object files under src/; build first (e.g. R CMD INSTALL --preclean --no-clean-on-error)")
hits <- list()
for (o in objs) {
  u <- undefined(o)
  h <- u[u %in% FORBIDDEN | Reduce(`|`, lapply(FORBIDDEN_RE, grepl, x = u), FALSE)]
  if (length(h)) hits[[o]] <- sort(unique(h), method = "radix")
}
cat(sprintf("%d object files scanned\n", length(objs)))
if (length(hits)) {
  bad <- TRUE
  for (o in names(hits)) cat(sprintf("%-50s %s\n", o, paste(hits[[o]], collapse = " ")))
  tab <- sort(table(unlist(hits)), decreasing = TRUE)
  cat("\nby symbol:\n"); print(tab)
} else cat("no forbidden symbols\n")
if (bad) quit(status = 1L)
