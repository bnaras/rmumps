#!/usr/bin/env Rscript
# Copyright 2026 Balasubramanian Narasimhan. GPL (>= 2); see inst/COPYRIGHTS.
#
# Vendoring pipeline for the MUMPS and METIS sources shipped in src/vendor.
#
# Usage (from the package root):
#   Rscript tools/vendor.R fetch            download and checksum the pinned tarballs
#   Rscript tools/vendor.R generate         regenerate src/vendor, src/Makevars.vendor
#                                           and inst/include/rmumps from pristine
#   Rscript tools/vendor.R verify           regenerate into a temporary directory and
#                                           diff against the working tree
#   Rscript tools/vendor.R capture <comp> <name>
#                                           record hand edits of src/vendor/<comp> as a
#                                           new patch tools/patches/<comp>/NNNN-<name>.patch
#
# Pristine tarballs are pinned in tools/vendor.lock and cached in
# $RMUMPS_VENDOR_CACHE (default tools/.cache). See tools/README.md.

invisible(Sys.setlocale("LC_ALL", "C"))
options(warn = 1, stringsAsFactors = FALSE)

if (!file.exists("DESCRIPTION") ||
    read.dcf("DESCRIPTION", "Package")[1, 1] != "rmumps")
  stop("run tools/vendor.R from the rmumps package root", call. = FALSE)

for (f in c("fixedform.R", "t1_fortran.R", "t2_c.R", "t3_meta.R", "t4_sections.R",
            "t5_pedantic.R", "abi.R"))
  source(file.path("tools", "transforms", f))

CACHE <- Sys.getenv("RMUMPS_VENDOR_CACHE", file.path("tools", ".cache"))

# ---------------------------------------------------------------- helpers

say <- function(...) message(sprintf(...))

read_lock <- function(path = file.path("tools", "vendor.lock")) {
  m <- read.dcf(path)
  rows <- lapply(seq_len(nrow(m)), function(i) as.list(m[i, ]))
  names(rows) <- m[, "Component"]
  rows
}

dcf_list <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(character())
  v <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  v[nzchar(v)]
}

read_manifest <- function(comp) {
  m <- read.dcf(file.path("tools", "manifest", paste0(comp, ".dcf")),
                keep.white = character())[1, ]
  g <- function(f) if (f %in% names(m)) m[[f]] else NA
  list(component = comp,
       arith = g("Arith"),
       makefile = g("Makefile"),
       makefile_vars = dcf_list(g("Makefile.Vars")),
       files = dcf_list(g("Files")),
       exclude = dcf_list(g("Exclude")),
       defines_c = dcf_list(g("Defines.C")),
       defines = dcf_list(g("Defines.Fortran")),
       unknown_defines = dcf_list(g("Defines.Unknown")),
       fortran_headers = dcf_list(g("Fortran.Headers")),
       units = gsub("[[:space:]]", "", toupper(dcf_list(g("Units")))),
       io_disabled = gsub("[[:space:]]+", " ", dcf_list(g("IO-Disabled"))),
       rng = gsub("[[:space:]]+", " ", dcf_list(g("RNG"))))
}

# Write lines with LF endings on every platform.
write_lf <- function(lines, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, "wb")
  on.exit(close(con))
  writeLines(lines, con, sep = "\n", useBytes = TRUE)
}

read_lines <- function(path) readLines(path, warn = FALSE, encoding = "bytes")

sha256 <- function(path) unname(tools::sha256sum(path))

tarball_path <- function(ent) file.path(CACHE, basename(ent$URL))

# ---------------------------------------------------------------- fetch

cmd_fetch <- function() {
  dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
  for (ent in read_lock()) {
    dest <- tarball_path(ent)
    if (!file.exists(dest) || sha256(dest) != ent$SHA256) {
      urls <- c(ent$URL, if (!is.null(ent$Mirror)) ent$Mirror)
      ok <- FALSE
      for (u in urls) {
        say("fetching %s", u)
        r <- try(utils::download.file(u, dest, mode = "wb", quiet = TRUE), silent = TRUE)
        if (!inherits(r, "try-error") && file.exists(dest) && sha256(dest) == ent$SHA256) {
          ok <- TRUE; break
        }
        say("  failed or checksum mismatch")
      }
      if (!ok) stop("could not fetch ", ent$Component, call. = FALSE)
    }
    say("%-6s %-8s %s  OK", ent$Component, ent$Version, ent$SHA256)
  }
}

# Extract the pinned tarball of `comp` into `dir`, after checking its hash.
extract <- function(ent, dir) {
  tb <- tarball_path(ent)
  if (!file.exists(tb)) stop("missing ", tb, "; run 'Rscript tools/vendor.R fetch'", call. = FALSE)
  if (sha256(tb) != ent$SHA256) stop("checksum mismatch for ", tb, call. = FALSE)
  utils::untar(tb, exdir = dir, tar = "internal")
  root <- file.path(dir, ent$Root)
  if (!dir.exists(root)) stop("tarball has no ", ent$Root, call. = FALSE)
  root
}

# ---------------------------------------------------------------- select

expand_globs <- function(root, pats) {
  out <- character()
  for (p in pats) {
    if (grepl("[*?]", p)) {
      d <- dirname(p)
      re <- utils::glob2rx(basename(p))
      hit <- list.files(file.path(root, d), pattern = re)
      hit <- hit[!dir.exists(file.path(root, d, hit))]
      out <- c(out, if (d == ".") hit else file.path(d, hit))
    } else {
      if (!file.exists(file.path(root, p))) stop("listed file not found: ", p, call. = FALSE)
      out <- c(out, p)
    }
  }
  out
}

# Object list of the upstream MUMPS Makefile with ARITH substituted.
mumps_objects <- function(root, man) {
  txt <- paste(read_lines(file.path(root, man$makefile)), collapse = "\n")
  txt <- gsub("\\\\[ \t]*\n", " ", txt)
  objs <- character()
  for (v in man$makefile_vars) {
    m <- regmatches(txt, regexec(paste0("\n", v, "[ \t]*=[ \t]*([^\n]*)"), txt))[[1]]
    if (length(m) < 2L) stop("Makefile variable not found: ", v, call. = FALSE)
    objs <- c(objs, strsplit(trimws(m[2L]), "[[:space:]]+")[[1]])
  }
  objs <- gsub("$(ARITH)", man$arith, objs, fixed = TRUE)
  objs <- objs[nzchar(objs)]
  if (any(!grepl("^[A-Za-z0-9_]+\\.o$", objs)))
    stop("unexpected object names: ", paste(objs[!grepl("^[A-Za-z0-9_]+\\.o$", objs)], collapse = " "), call. = FALSE)
  objs
}

select_files <- function(root, man) {
  files <- expand_globs(root, man$files)
  if (man$component == "mumps") {
    objs <- mumps_objects(root, man)
    src <- character()
    for (o in objs) {
      b <- sub("\\.o$", "", o)
      if (b == paste0(man$arith, "mumps_c")) { src <- c(src, "src/mumps_c.c"); next }
      cand <- file.path("src", paste0(b, c(".F", ".c")))
      cand <- cand[file.exists(file.path(root, cand))]
      if (length(cand) != 1L) stop("no unique source for ", o, call. = FALSE)
      src <- c(src, cand)
    }
    files <- c(files, src)
  }
  excl <- expand_globs(root, man$exclude)
  files <- setdiff(unique(files), excl)
  sort(files, method = "radix")
}

# ---------------------------------------------------------------- patches

patch_files <- function(comp) {
  d <- file.path("tools", "patches", comp)
  sort(list.files(d, pattern = "^[0-9]{4}-.*\\.patch$", full.names = TRUE), method = "radix")
}

apply_patches <- function(stage, comp) {
  for (p in patch_files(comp)) {
    say("  patch %s", basename(p))
    out <- suppressWarnings(system2("git", c("-C", shQuote(stage), "apply", "--whitespace=nowarn",
                                             "-p1", shQuote(normalizePath(p))),
                                    stdout = TRUE, stderr = TRUE))
    st <- attr(out, "status")
    if (!is.null(st) && st != 0)
      stop("patch failed: ", basename(p), "\n", paste(out, collapse = "\n"), call. = FALSE)
  }
}

# ---------------------------------------------------------------- generate

MOD_NOTE_F <- "C     rmumps: this file was modified by tools/vendor.R (see src/vendor/README.md)"
MOD_NOTE_C <- "/* rmumps: this file was modified by tools/vendor.R (see src/vendor/README.md) */"

# Stage, patch and transform one component. Returns the transformed file
# set as a named list of character vectors (names are paths relative to the
# component root) plus the transform log.
build_component <- function(ent, tmp) {
  comp <- ent$Component
  man <- read_manifest(comp)
  say("%s %s", comp, ent$Version)
  root <- extract(ent, file.path(tmp, "pristine", comp))
  files <- select_files(root, man)
  stage <- file.path(tmp, "stage", comp)
  for (f in files) {
    dir.create(dirname(file.path(stage, f)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(root, f), file.path(stage, f))
  }
  apply_patches(stage, comp)
  # files created by patches belong to the output too
  staged <- list.files(stage, recursive = TRUE, all.files = TRUE)
  files <- sort(union(intersect(files, staged), staged), method = "radix")
  out <- list()
  log <- character()
  errors <- character()
  sections <- t4_read_table(file.path("tools", "conformance", paste0(comp, "-sections.tsv")))
  for (f in files) {
    lines <- read_lines(file.path(stage, f))
    pristine <- if (file.exists(file.path(root, f))) read_lines(file.path(root, f)) else character()
    fortran <- grepl("\\.[Ff]$", f) || f %in% man$fortran_headers
    if (fortran) {
      r <- tryCatch(t1_transform(lines, basename(f), man),
                    error = function(e) conditionMessage(e))
      if (is.character(r)) { errors <- c(errors, r); next }
      lines <- r$lines
      log <- c(log, r$log)
      r <- tryCatch(t4_transform(lines, basename(f), sections),
                    error = function(e) conditionMessage(e))
      if (is.character(r)) { errors <- c(errors, r); next }
      lines <- r$lines
      log <- c(log, r$log)
      r <- tryCatch(t5_transform(lines, basename(f)),
                    error = function(e) conditionMessage(e))
      if (is.character(r)) { errors <- c(errors, r); next }
      lines <- r$lines
      log <- c(log, r$log)
    } else if (grepl("\\.(c|h)$", f)) {
      lines <- t2_transform(lines, f, comp)
    }
    if (!identical(lines, pristine)) {
      note <- if (fortran) MOD_NOTE_F else if (grepl("\\.(c|h)$", f)) MOD_NOTE_C else NULL
      lines <- c(note, lines)
    }
    out[[f]] <- lines
  }
  if (length(errors)) stop(paste(errors, collapse = "\n"), call. = FALSE)
  list(files = out, log = log, man = man, ent = ent)
}

generate_into <- function(target) {
  lock <- read_lock()
  tmp <- tempfile("vendor")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  comps <- lapply(lock, build_component, tmp = tmp)
  vend <- file.path(target, "src", "vendor")
  unlink(vend, recursive = TRUE)
  for (cp in comps) {
    for (f in names(cp$files))
      write_lf(cp$files[[f]], file.path(vend, cp$ent$Component, f))
    write_lf(sort(cp$log, method = "radix"),
             file.path(target, "tools", "logs", paste0(cp$ent$Component, "-t1.log")))
  }
  t3_generate(comps, target)
  invisible(comps)
}

cmd_generate <- function() {
  generate_into(".")
  say("done; run 'Rscript tools/lint-symbols.R' after building")
}

GENERATED <- c("src/vendor", "src/Makevars.vendor", "src/rmumps_abi_adapters.c",
               "inst/include/rmumps", "tools/logs", "tools/abi")

# Record the DMUMPS_STRUC_C layout of the headers in `incdir` as a new ABI
# (bootstrap or recovery; generate does this itself when the layout changes).
cmd_abi_record <- function(incdir, version) {
  if (missing(incdir) || missing(version) || is.na(version))
    stop("usage: abi-record <include-dir> <mumps-version>", call. = FALSE)
  d <- file.path("tools", "abi")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  reg <- abi_read_registry(d)
  lay <- abi_dump(incdir)
  f <- sprintf("dmumps_struc_%s.txt", version)
  write_lf(abi_format(lay, version), file.path(d, f))
  reg <- rbind(reg, data.frame(ABI = max(c(0L, reg$ABI)) + 1L, MUMPS = version, Layout = f))
  abi_write_registry(reg, file.path(d, "registry.dcf"))
  say("recorded ABI %d: MUMPS %s (sizeof %d)", max(reg$ABI), version, lay$sizeof)
}

cmd_verify <- function() {
  tmp <- tempfile("verify")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  # carry over the hand-written parts of the generated directories
  dir.create(file.path(tmp, "inst", "include", "rmumps"), recursive = TRUE)
  for (h in t3_handwritten_headers())
    file.copy(file.path("inst", "include", "rmumps", h), file.path(tmp, "inst", "include", "rmumps", h))
  generate_into(tmp)
  bad <- FALSE
  for (g in GENERATED) {
    out <- suppressWarnings(system2("diff", c("-r", "-q", "-x", "'*.o'", "-x", "'*.mod'",
                                              "-x", "'*.smod'", shQuote(g), shQuote(file.path(tmp, g))),
                                    stdout = TRUE, stderr = TRUE))
    if (length(out)) { bad <- TRUE; writeLines(out) }
  }
  if (bad) stop("generated files differ from a fresh regeneration", call. = FALSE)
  say("verify: generated files are up to date")
}

cmd_capture <- function(comp, name) {
  if (missing(comp) || missing(name)) stop("usage: capture <component> <name>", call. = FALSE)
  if (!grepl("^[a-z0-9-]+$", name)) stop("patch name must match [a-z0-9-]+", call. = FALSE)
  tmp <- tempfile("capture")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ent <- read_lock()[[comp]]
  cp <- build_component(ent, tmp)
  # The working tree holds transformed files; the hand edit is the
  # difference between them and a fresh regeneration. Patches apply to
  # pristine, so the difference is mapped back onto the patched pristine
  # file only when the transform left the edited lines untouched; otherwise
  # the patch must be written by hand.
  stage <- file.path(tmp, "stage", comp)
  a <- file.path(tmp, "a"); b <- file.path(tmp, "b")
  changed <- character()
  for (f in names(cp$files)) {
    work <- file.path("src", "vendor", comp, f)
    if (!file.exists(work)) next
    w <- read_lines(work)
    if (identical(w, cp$files[[f]])) next
    st <- read_lines(file.path(stage, f))
    # undo what can be undone exactly: the modification note and the T2
    # shim includes
    strip <- function(x) {
      if (length(x) && x[1L] %in% c(MOD_NOTE_F, MOD_NOTE_C)) x <- x[-1L]
      x[x != T2_INCLUDE]
    }
    gf <- file.path(tmp, "gen"); wf <- file.path(tmp, "work"); df <- file.path(tmp, "edit.diff")
    write_lf(strip(cp$files[[f]]), gf)
    write_lf(strip(w), wf)
    d <- suppressWarnings(system2("diff", c("-u", shQuote(gf), shQuote(wf)), stdout = TRUE))
    write_lf(d, df)
    # apply the edit to the patched pristine file; its context must match,
    # i.e. the edit must not touch lines changed by a transform
    write_lf(st, file.path(a, f))
    write_lf(st, file.path(b, f))
    r <- suppressWarnings(system2("patch", c("-s", "-F", "0", "--no-backup-if-mismatch",
                                             shQuote(file.path(b, f)), shQuote(df)),
                                  stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(r, "status")))
      stop(f, ": the edit overlaps lines changed by a transform; write this patch by hand",
           call. = FALSE)
    if (identical(read_lines(file.path(b, f)), st)) {
      unlink(c(file.path(a, f), file.path(b, f)))
      next
    }
    changed <- c(changed, f)
  }
  if (!length(changed)) { say("no hand edits found"); return(invisible()) }
  d <- file.path("tools", "patches", comp)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  n <- length(patch_files(comp)) + 1L
  out <- file.path(d, sprintf("%04d-%s.patch", n, name))
  diff <- suppressWarnings(system2("git", c("diff", "--no-index", "--no-prefix", "--",
                                            shQuote(a), shQuote(b)), stdout = TRUE))
  # git >= 2.5x prints absolute paths without their leading slash
  for (p in c(a, sub("^/", "", a)))
    diff <- gsub(paste0(p, "/"), "a/", diff, fixed = TRUE)
  for (p in c(b, sub("^/", "", b)))
    diff <- gsub(paste0(p, "/"), "b/", diff, fixed = TRUE)
  hdr <- c("Why: <one paragraph>",
           "Upstream: <not sent | sent YYYY-MM-DD | merged in x.y.z>",
           "Obsolete when: <test that tells when this patch can be dropped>",
           "")
  write_lf(c(hdr, diff), out)
  say("wrote %s (%d file(s)); fill in its header", out, length(changed))
}

# ---------------------------------------------------------------- main

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args)) args <- "help"
  switch(args[1L],
         fetch = cmd_fetch(),
         generate = cmd_generate(),
         verify = cmd_verify(),
         capture = cmd_capture(args[2L], args[3L]),
         "abi-record" = cmd_abi_record(args[2L], args[3L]),
         {
           cat("usage: Rscript tools/vendor.R fetch|generate|verify|capture <component> <name>\n")
           quit(status = if (args[1L] == "help") 0L else 1L)
         })
}
