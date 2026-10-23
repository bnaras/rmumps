# Capture golden reference results from an installed rmumps build.
#
# Usage (from the package root):
#   Rscript tools/golden/make-golden.R <lib.loc> [output.rds]
#
# <lib.loc> is a library containing the rmumps version to capture (e.g. a
# scratch library holding 5.2.1-43 built from the tagged commit). The output
# defaults to tests/testthat/fixtures/golden-<version>.rds.
#
# Matrices are stored as plain 0-based triplets (i, j, x, n) so that future
# tests do not depend on Matrix class coercions. For sym = 1 or 2, only the
# upper triangle is stored, which is what MUMPS expects. An independent dense
# reference solution (base::solve) is stored next to the rmumps results.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L)
  stop("usage: Rscript tools/golden/make-golden.R <lib.loc> [output.rds]")
lib <- args[[1L]]

suppressPackageStartupMessages({
  library(Matrix)
  library(rmumps, lib.loc = lib)
})
ver <- packageDescription("rmumps", lib.loc = lib, fields = "Version")
out <- if (length(args) >= 2L) args[[2L]] else
  file.path("tests", "testthat", "fixtures", sprintf("golden-%s.rds", ver))

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(20261023)

## ---- matrix zoo ------------------------------------------------------------

as_triplet <- function(a, sym) {
  a <- as(as(as(a, "dMatrix"), "generalMatrix"), "TsparseMatrix")
  keep <- if (sym == 0L) rep(TRUE, length(a@x)) else a@i <= a@j
  list(i = a@i[keep], j = a@j[keep], x = a@x[keep], n = nrow(a))
}

rand_unsym <- function(n, per_row) {
  a <- matrix(0, n, n)
  a[sample.int(n * n, per_row * n)] <- runif(per_row * n, -1, 1)
  diag(a) <- rowSums(abs(a)) + 1
  a
}

rand_spd <- function(n, per_row) {
  b <- rand_unsym(n, per_row)
  crossprod(b) + diag(n)
}

rand_sym_indef <- function(n, per_row) {
  b <- rand_unsym(n, per_row)
  s <- b + t(b)
  diag(s) <- diag(s) * rep(c(1, -1), length.out = n)
  s
}

laplace1d <- function(n) {
  a <- diag(2, n)
  a[cbind(1:(n - 1), 2:n)] <- -1
  a[cbind(2:n, 1:(n - 1))] <- -1
  a
}

convdiff2d <- function(m, beta = 0.3) {
  n <- m * m
  a <- matrix(0, n, n)
  id <- function(r, c) (c - 1L) * m + r
  for (c in 1:m) for (r in 1:m) {
    k <- id(r, c)
    a[k, k] <- 4
    if (r > 1) a[k, id(r - 1, c)] <- -1 - beta
    if (r < m) a[k, id(r + 1, c)] <- -1 + beta
    if (c > 1) a[k, id(r, c - 1)] <- -1
    if (c < m) a[k, id(r, c + 1)] <- -1
  }
  a
}

cases <- list(
  unsym_random   = list(a = rand_unsym(100, 5),     sym = 0L),
  spd_random     = list(a = rand_spd(80, 3),        sym = 1L),
  sym_indef      = list(a = rand_sym_indef(60, 3),  sym = 2L),
  laplace1d_spd  = list(a = laplace1d(200),         sym = 1L),
  laplace1d_gen  = list(a = laplace1d(200),         sym = 2L),
  convdiff2d     = list(a = convdiff2d(12),         sym = 0L),
  one_by_one     = list(a = matrix(3.5, 1, 1),      sym = 0L)
)

orderings <- c(AMD = 0L, AMF = 2L, PORD = 4L, METIS = 5L, QAMD = 6L, AUTO = 7L)

## ---- helpers ---------------------------------------------------------------

try_val <- function(expr) tryCatch(expr, error = function(e)
  structure(conditionMessage(e), class = "golden_error"))

new_solver <- function(tr, sym) Rmumps$new(tr$i, tr$j, tr$x, tr$n, sym)

run_case <- function(cs) {
  a <- cs$a
  n <- nrow(a)
  tr <- as_triplet(a, cs$sym)
  x_true <- seq_len(n) / n
  b <- drop(a %*% x_true)
  bt <- drop(crossprod(a, x_true))
  B <- cbind(b, 2 * b, a %*% rep(1, n))
  Bs <- as(as(Matrix(diag(n)[, seq_len(min(4L, n)), drop = FALSE],
                     sparse = TRUE), "generalMatrix"), "CsparseMatrix")
  ref <- list(x = solve(a, b), xt = solve(t(a), bt), X = solve(a, B),
              det = det(a), log2absdet = determinant(a)$modulus[[1]] / log(2))

  per_ord <- lapply(orderings, function(perm) {
    am <- new_solver(tr, cs$sym)
    am$set_permutation(perm)
    res <- list(
      x    = try_val(am$solve(b)),
      xt   = try_val(am$solvet(bt)),
      X    = try_val(am$solve(B)),
      Xs   = try_val(solve(am, Bs)),
      det  = try_val(am$det()),
      log2det = try_val(am$log2det()),
      inv  = if (n <= 60L) try_val(am$inv()) else NULL,
      sym_perm = try_val(am$get_sym_perm()),
      uns_perm = try_val(am$get_uns_perm()),
      infos    = try_val(am$get_infos()),
      icntl    = try_val(am$get_icntl()),
      cntl     = try_val(am$get_cntl())
    )
    # Matrix value update with the same pattern: scale by 2, solve again.
    res$x_after_update <- try_val({
      am2 <- new_solver(tr, cs$sym)
      am2$set_permutation(perm)
      invisible(am2$solve(b))
      am2$set_mat_data(2 * tr$x)
      am2$solve(b)
    })
    rm(am)
    res
  })
  list(triplet = tr, sym = cs$sym, x_true = x_true, b = b, bt = bt, B = B,
       Bs = list(i = Bs@i, p = Bs@p, x = Bs@x, dim = Bs@Dim),
       reference = ref, results = per_ord)
}

## ---- run -------------------------------------------------------------------

golden <- lapply(cases, run_case)

# Error path: a structurally singular matrix (empty column).
sing <- Matrix::sparseMatrix(i = 1:4, j = c(1, 2, 2, 4), x = c(1, 2, 3, 4),
                             dims = c(4, 4))
sing_tr <- as_triplet(sing, 0L)
sing_am <- new_solver(sing_tr, 0L)
golden_singular <- list(
  triplet = sing_tr,
  error = try_val(sing_am$solve(rep(1, 4))),
  infos = try_val(sing_am$get_infos())
)
rm(sing_am)

## ---- sanity: the golden values must themselves be right ---------------------

# Errors are recorded as behavior of this version (e.g. a PORD limitation);
# wrong numbers are not acceptable in a golden file and abort the run.
bad <- character()
errs <- character()
for (nm in names(golden)) {
  g <- golden[[nm]]
  for (o in names(g$results)) {
    r <- g$results[[o]]
    for (item in names(r)) if (inherits(r[[item]], "golden_error"))
      errs <- c(errs, sprintf("%s/%s/%s: %s", nm, o, item,
                              trimws(gsub("\\s+", " ", r[[item]]))))
    ref <- g$reference
    checks <- list(
      x = list(r$x, ref$x),
      xt = list(r$xt, ref$xt),
      X = list(r$X, ref$X),
      x_after_update = list(r$x_after_update, ref$x / 2),
      det = list(r$det, ref$det)
    )
    for (ck in names(checks)) {
      got <- checks[[ck]][[1L]]
      want <- checks[[ck]][[2L]]
      if (is.null(got) || inherits(got, "golden_error")) next
      scale <- max(1, max(abs(want)))
      if (max(abs(unclass(got) - unclass(want))) > 1e-8 * scale)
        bad <- c(bad, sprintf("%s/%s/%s: differs from base R reference", nm, o, ck))
    }
  }
}
if (!inherits(golden_singular$error, "golden_error"))
  bad <- c(bad, "singular matrix did not raise an error")

meta <- list(
  rmumps_version = ver,
  mumps_version = new_solver(as_triplet(diag(1), 0L), 0L)$mumps_version(),
  r_version = R.version.string,
  platform = R.version$platform,
  matrix_version = as.character(packageVersion("Matrix")),
  blas = tryCatch(extSoftVersion()[["BLAS"]], error = function(e) NA_character_),
  la_library = tryCatch(La_library(), error = function(e) NA_character_),
  created = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  orderings = orderings,
  recorded_errors = errs,
  sanity_problems = bad
)

dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(meta = meta, cases = golden, singular = golden_singular), out,
        version = 3, compress = "xz")

cat(sprintf("wrote %s (%s bytes)\n", out, format(file.size(out), big.mark = ",")))
cat(sprintf("rmumps %s, MUMPS %s, %d cases x %d orderings\n", ver,
            meta$mumps_version, length(golden), length(orderings)))
if (length(errs))
  cat("recorded errors (behavior of this version):\n",
      paste0("  ", errs, "\n"), sep = "")
if (length(bad)) {
  cat("SANITY PROBLEMS:\n", paste0("  ", bad, "\n"), sep = "")
  quit(status = 1)
}
cat("sanity: all rmumps solutions match base::solve\n")
