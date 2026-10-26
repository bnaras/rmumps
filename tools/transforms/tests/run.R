# Copyright 2026 Balasubramanian Narasimhan. GPL (>= 2); see inst/COPYRIGHTS.
#
# Unit tests of the vendoring transforms. Run from the package root:
#   Rscript tools/transforms/tests/run.R

invisible(Sys.setlocale("LC_ALL", "C"))
for (f in c("fixedform.R", "t1_fortran.R", "t2_c.R", "t4_sections.R", "t5_pedantic.R", "abi.R"))
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

out <- t2_void_params(c("int f();", "static void g() {", "  h();", "  x = k();", "}",
                        "#define FOO() bar()", "#define M(nu) void nu (); \\",
                        "  void q(); \\", "  y = r();",
                        "extern \"C\" {", "void s();", "#ifdef __cplusplus", "}",
                        "/* int c(); */ int d();"))
expect(out[1] == "int f(void);" && out[2] == "static void g(void) {", "declarations get (void)")
expect(out[3] == "  h();" && out[4] == "  x = k();", "calls in function bodies untouched")
expect(out[6] == "#define FOO() bar()", "zero-parameter macros and expression macros untouched")
expect(out[7] == "#define M(nu) void nu (); \\" && out[8] == "  void q(void); \\" &&
         out[9] == "  y = r();", "macro parameters and macro calls untouched")
expect(out[11] == "void s(void);", "extern \"C\" braces do not count as a scope")
expect(out[14] == "/* int c(); */ int d(void);", "comments skipped")

# ---- T4 ----------------------------------------------------------------------

tab <- data.frame(file = "x.F", routine = c("S", "S", "S"), callee = c("F", "F", "G"),
                  arg = c("P(1)", "ID%Q(1,1)", "A(I+1)"))
out <- t4_transform(src(
  "|      SUBROUTINE S",
  "|      CALL F(N, P(1), ID%Q(1,1))",
  "|      CALL F(N, P(1),",
  "|     &       ID%Q(1, 1))",
  "|      CALL G(A(I+1))",
  "|      CALL H(P(1))",
  "|      END"), "x.F", tab)$lines
expect(out[2] == "      CALL F(N, P(1:), ID%Q(1:,1:))", "T4 rewrites listed elements")
expect(out[3] == "      CALL F(N, P(1:)," && out[4] == "     &       ID%Q(1:, 1:))", "T4 across continuation lines")
expect(out[5] == "      CALL G(A(I+1:))" && out[6] == "      CALL H(P(1))", "T4 only touches listed calls")
expect_error(t4_transform(src("|      SUBROUTINE S", "|      CALL F(X(1))", "|      END"), "x.F", tab),
             "match nothing", "T4 stale entries fail")
long <- t4_transform(src(
  "|      SUBROUTINE S",
  "|      CALL G(AAAAAAAAAAAA, BBBBBBBBBBBB, CCCCCCCCCCCC, DDDDDDDD, A(I+1))",
  "|      END"), "x.F", tab[3, ])$lines
expect(all(nchar(long) <= 72L) && length(long) == 4L && substr(long[3], 6, 6) == "&",
       "T4 re-wraps lines past column 72")
cmt <- t4_transform(src(
  "|      SUBROUTINE S",
  "|      CALL F(B,",
  "|     &       R_ptr(1,1), !LRB%R_CALC(PCALC,PCALC)%VAL(1+OFFSET_R_CALC), ",
  "|     &       N)",
  "|      END"), "x.F", data.frame(file = "x.F", routine = "S", callee = "F",
                                  arg = "R_PTR(1,1)"))$lines
expect(length(cmt) == 5L && startsWith(cmt[3], "     &       R_ptr(1:,1:), !LRB%R_CALC(PCALC,PCALC)"),
       "T4 does not re-wrap a line whose trailing comment passes column 72")
cols <- t4_transform(src(
  "|      SUBROUTINE S",
  "|      CALL F(LRB%Q(1, K+1), B)",
  "|      END"), "x.F", data.frame(file = "x.F", routine = "S", callee = "F",
                                  arg = "LRB%Q(1,K+1)"))$lines
expect(cols[2] == "      CALL F(LRB%Q(1:, K+1:), B)", "T4 trailing columns P(1,J) -> P(1:,J:)")
expect_error(t4_transform(src("|      SUBROUTINE S", "|      CALL F(R(K,1))", "|      END"), "x.F",
                          data.frame(file = "x.F", routine = "S", callee = "F", arg = "R(K,1)")),
             "cannot make a section", "T4 refuses a row block P(J,1) (not contiguous)")

# ---- T5 ----------------------------------------------------------------------

out <- t5_transform(src(
  "|      INTEGER*8 N8, M8",
  "|      integer*1 :: B(10)",
  "|      complex*8 Z",
  "|      CHARACTER*48 MSG",
  "|      REAL*8 FUNCTION F(X)",
  "|      X = INTEGER*2",
  "|C     INTEGER*4 IN A COMMENT"), "x.F")$lines
expect(out[1] == "      INTEGER(8) N8, M8" && out[2] == "      integer(1) :: B(10)",
       "T5 INTEGER*n, case kept")
expect(out[3] == "      complex(4) Z" && out[4] == "      CHARACTER(LEN=48) MSG",
       "T5 COMPLEX*n (kind n/2) and CHARACTER*n")
expect(out[5] == "      REAL(8) FUNCTION F(X)", "T5 typed FUNCTION")
expect(out[6] == "      X = INTEGER*2" && out[7] == "C     INTEGER*4 IN A COMMENT",
       "T5 leaves non-declarations and comments alone")

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
