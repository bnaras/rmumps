#!/usr/bin/env Rscript
# Propose T4 entries (tools/conformance/mumps-sections.tsv) from a gfortran
# or LLVM flang build log: one row per call that passes an element of a
# pointer or assumed-shape array to an array dummy.
#
# Usage (from the package root, after a build of the generated sources with
# the temporary -fallow-argument-mismatch, so that every site is reported;
# flang stops at the first file with errors unless make runs with -k):
#   Rscript tools/conformance-harvest.R <install.log> > new-entries.tsv
# Review the rows (the rewrite must be valid for each), merge them into the
# table, regenerate and rebuild.

invisible(Sys.setlocale("LC_ALL", "C"))
read_lines <- function(path) readLines(path, warn = FALSE, encoding = "bytes")
source(file.path("tools", "transforms", "fixedform.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("usage: conformance-harvest.R <install.log>")
log <- read_lines(args[1L])
# gfortran: location line, then "Error: Element of assumed-shape or pointer
# array ..." a few lines below
loc_re <- "^(vendor/[^ :]+\\.[Ff]):([0-9]+):([0-9]+):$"
locs <- list()
for (h in grep("^(Warning|Error): Element of assumed-shape or pointer array", log)) {
  j <- h - 1L
  while (j > 0L && !grepl(loc_re, log[j])) j <- j - 1L
  if (j > 0L) locs[[length(locs) + 1L]] <- regmatches(log[j], regexec(loc_re, log[j]))[[1]]
}
# LLVM flang: one line, "./vendor/f.F:L:C: error: Element of assumed-shape
# array may not be associated with a dummy argument 'x=' array"
flang_re <- paste0("^(?:\\./)?(vendor/[^ :]+\\.[Ff]):([0-9]+):([0-9]+): error: ",
                   "Element of (assumed-shape|pointer) array may not be associated")
for (l in grep(flang_re, log, value = TRUE, perl = TRUE))
  locs[[length(locs) + 1L]] <- regmatches(l, regexec(flang_re, l, perl = TRUE))[[1]]
files <- list()
rows <- character()
for (m in locs) {
  f <- m[2L]; ln <- as.integer(m[3L]); col <- as.integer(m[4L])
  if (is.null(files[[f]])) {
    L <- read_lines(file.path("src", f))
    st <- ff_statements(L)
    st$unit <- ff_units(st$squeezed)
    files[[f]] <- list(L = L, st = st)
  }
  L <- files[[f]]$L; st <- files[[f]]$st
  k <- which(st$first <= ln & st$last >= ln)
  if (length(k) != 1L) next
  act <- ff_strip_if(st$squeezed[k])[["action"]]
  if (!startsWith(act, "CALL")) next
  callee <- sub("^CALL([A-Z0-9_]+).*$", "\\1", act)
  # the argument starting at the caret (which may sit on the delimiter)
  s <- substr(L[ln], col, nchar(L[ln]))
  s <- sub("^[(,&]", "", sub("^ +", "", s))
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  d <- 0L; out <- character()
  for (c in ch) {
    if (c == "(") d <- d + 1L
    if (c == ")") { if (d == 0L) break; d <- d - 1L }
    if (c == "," && d == 0L) break
    out <- c(out, c)
  }
  arg <- toupper(gsub("[[:space:]]", "", paste(out, collapse = "")))
  if (!nzchar(arg) || !endsWith(arg, ")")) {
    message(sprintf("skipped (review by hand): %s:%d %s %s", f, ln, callee, arg))
    next
  }
  rows <- c(rows, paste(basename(f), st$unit[k], callee, arg, sep = "\t"))
}
writeLines(sort(unique(rows), method = "radix"))
