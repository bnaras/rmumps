# Unit tests of the vendoring transforms. Run from the package root:
#   Rscript tools/transforms/tests/run.R

invisible(Sys.setlocale("LC_ALL", "C"))
for (f in c("fixedform.R", "t1_fortran.R", "t2_c.R", "abi.R"))
  source(file.path("tools", "transforms", f))
write_lf <- function(lines, path) writeLines(lines, path)
read_lines <- function(path) readLines(path, warn = FALSE)

n_ok <- 0L
failures <- character()
expect <- function(ok, what) {
  if (isTRUE(ok)) n_ok <<- n_ok + 1L else failures <<- c(failures, what)
}
expect_error <- function(expr, pattern, what) {
  msg <- tryCatch({ force(expr); "" }, error = function(e) conditionMessage(e))
  expect(nzchar(msg) && grepl(pattern, msg), paste(what, "->", msg))
}

man <- list(defines = c("metis", "pord", "NO_SAVE_RESTORE"),
            unknown_defines = "_WIN32",
            units = c("*", "6", "MP", "LP"),
            io_disabled = c("x.F CFG", "y.F *"),
            rng = "x.F RNG")

# Lines are given with "|" standing for column 1, so the fixed-form columns
# are explicit.
src <- function(...) sub("^\\|", "", c(...))
t1 <- function(lines, file = "x.F") t1_transform(lines, file, man)$lines

# ---- fixedform -------------------------------------------------------------

st <- ff_statements(src(
  "|      X = 'it''s ! not a comment'   ! trailing",
  "|      CALL F(A,",
  "|C comment inside a continuation",
  "|     &       B)"))
expect(nrow(st) == 2L && st$last[2] == 4L, "continuation across a comment line")
expect(st$squeezed[1] == "X='_____________________'", "literal masked, comment dropped")

expect(ff_eval_cond("defined(metis) && !defined(_OPENMP)", "metis") == "on", "cond on")
expect(ff_eval_cond("defined(scotch) || defined(ptscotch)", "metis") == "off", "cond off")
expect(ff_eval_cond("defined(_WIN32)", "metis", "_WIN32") == "unknown", "cond unknown")
expect(ff_eval_cond("MUMPS_ARITH == 2", "metis") == "unknown", "unparsed cond")
act <- ff_active(c("#if defined(scotch)", "a", "#else", "b", "#endif", "c"), "metis")
expect(identical(act[c(2, 4, 6)], c("off", "on", "on")), "ff_active else branch")

expect(identical(ff_strip_if("IF(A.GT.(B))WRITE(6,*)X"),
                 c(cond = "A.GT.(B)", action = "WRITE(6,*)X")), "logical IF split")
expect(ff_strip_if("IF(A)THEN")[["cond"]] == "", "block IF is not a logical IF")

# ---- T1 --------------------------------------------------------------------

out <- t1(src(
  "|      SUBROUTINE S(MP)",
  "|      IF (MP .GT. 0) WRITE(MP,*) 'value',",
  "|     &   ' x'",
  "|      WRITE(6,'(A)', IOSTAT=IERR) 'msg'",
  "|      X = 'WRITE(6,*) is text'",
  "|      STOPPED = .TRUE.",
  "|      END SUBROUTINE S"))
expect(startsWith(out[2], "C~") && startsWith(out[3], "C~"), "logical IF WRITE commented")
expect(startsWith(out[4], "C~") && trimws(out[5]) == "IERR = 0", "IOSTAT of a WRITE set to 0")
expect(out[6] == "      X = 'WRITE(6,*) is text'", "WRITE inside a literal untouched")
expect(out[7] == "      STOPPED = .TRUE.", "STOPPED is not STOP")

out <- t1(src(
  "|      SUBROUTINE S2",
  "|      DO 10 I = 1, N",
  "| 10   IF (LP.GE.0) WRITE(LP,*) I",
  "|      IF (BAD) STOP 'internal'",
  "|      END"))
expect(any(grepl("^10    CONTINUE$", out)), "labeled WRITE becomes labeled CONTINUE")
expect(any(grepl("IF \\(BAD\\) CALL REXIT\\('MUMPS internal error in '//'S2'\\)", out)),
       "STOP becomes CALL REXIT with the routine name")

out <- t1(src(
  "|      SUBROUTINE S3",
  "|      WRITE(MP,*) A,",
  "|#if defined(metis)",
  "|     &  B",
  "|#endif",
  "|     &  C",
  "|!$    WRITE(MP,*) 'omp'",
  "|      END"))
expect(out[3] == "#if defined(metis)" && out[5] == "#endif" && startsWith(out[4], "C~") &&
         startsWith(out[6], "C~"), "directive lines inside a WRITE are kept")
expect(startsWith(out[7], "C~"), "OpenMP sentinel WRITE commented")

out <- t1(src(
  "|      SUBROUTINE S4",
  "|#if defined(scotch)",
  "|      OPEN(10, FILE='f')",
  "|#endif",
  "|      INQUIRE(UNIT=I, OPENED=BUSY)",
  "|      END"))
expect(out[3] == "      OPEN(10, FILE='f')", "OPEN in a removed region untouched")
expect(startsWith(out[5], "C~") && trimws(out[6]) == "BUSY = .FALSE.", "INQUIRE OPENED")

out <- t1(src(
  "|      SUBROUTINE CFG",
  "|      OPEN(UNIT=U, FILE=F, STATUS='OLD', IOSTAT=IERR)",
  "|      WRITE(STR, '(I5)') N",
  "|      CLOSE(U)",
  "|      END SUBROUTINE CFG",
  "|      SUBROUTINE RNG",
  "|      CALL RANDOM_NUMBER(Y)",
  "|      END SUBROUTINE RNG"))
expect(startsWith(out[2], "C~") && trimws(out[3]) == "IERR = 1", "disabled OPEN, IOSTAT = 1")
expect(startsWith(out[4], "C~") && startsWith(out[5], "C~"), "disabled internal WRITE and CLOSE")
expect(trimws(out[9]) == "CALL RMUMPS_RANDOM_NUMBER(Y)", "allowlisted RANDOM_NUMBER")
out <- t1(src("|      SUBROUTINE ANY", "|      READ(5,*) X", "|      END"), file = "y.F")
expect(startsWith(out[2], "C~"), "IO-Disabled 'file *' covers every routine")

expect_error(t1(src("|      SUBROUTINE T", "|      WRITE(STR,*) N", "|      END")),
             "unknown unit", "internal WRITE outside a disabled routine fails")
expect_error(t1(src("|      SUBROUTINE T", "|      OPEN(10, FILE='f')", "|      END")),
             "OPEN outside", "OPEN outside a disabled routine fails")
expect_error(t1(src("|      SUBROUTINE T", "|      CALL RANDOM_NUMBER(Y)", "|      END")),
             "allowlist", "RANDOM_NUMBER outside the allowlist fails")
expect_error(t1(src("|      SUBROUTINE T", "|      WRITE(6,*) X; Y = 1", "|      END")),
             "';'", "two statements on one line fail")

em <- t1_emit(paste0("IF (", strrep("A.AND.", 12), "B) CALL REXIT('a long literal, not to split')"))
expect(all(nchar(em) <= 72L) && length(em) > 1L && all(substr(em[-1], 6, 6) == "&"),
       "t1_emit continues long statements within 72 columns")
expect(sum(grepl("'a long literal, not to split'", em, fixed = TRUE)) == 1L,
       "t1_emit never splits a character literal")

# ---- T2 --------------------------------------------------------------------

out <- t2_transform(c("/* header", "#include <old.h>", "*/", "#include <stdio.h>",
                      "#if defined(X)", "#include \"a.h\"", "#endif", "int f(void);"),
                    "f.c", "mumps")
expect(sum(out == T2_INCLUDE) == 2L && out[5] == T2_INCLUDE && out[8] == T2_INCLUDE,
       "shim after each include, none inside a block comment")
expect(identical(t2_transform("int x;", "f.h", "mumps"), "int x;"), "headers untouched")

# ---- ABI -------------------------------------------------------------------

lay <- function(names, decls, offs) list(
  members = data.frame(name = names, decl = decls, offset = offs, size = 4L),
  sizeof = 16L, ptrsize = 8L)
old <- lay(c("a", "gone", "s"), c("MUMPS_INT a", "MUMPS_INT gone", "char s[4]"), c(0L, 4L, 8L))
cur <- lay(c("a", "s"), c("MUMPS_INT a", "char s[8]"), c(0L, 4L))
expect_error(abi_adapter_c(old, cur, 1L, list()), "no adapter rule", "missing adapter rule fails")
code <- abi_adapter_c(old, cur, 1L, list(gone = list(out = "c->a")))
expect(any(grepl("o->gone = c->a;", code, fixed = TRUE)) &&
         any(grepl("copy_str(c->s", code, fixed = TRUE)), "adapter rules and resized strings")
expect(abi_same(old, old) && !abi_same(old, cur), "layout comparison")

# ----------------------------------------------------------------------------

cat(sprintf("%d checks passed\n", n_ok))
if (length(failures)) {
  cat("FAILED:\n", paste0("  ", failures, "\n"), sep = "")
  quit(status = 1L)
}
