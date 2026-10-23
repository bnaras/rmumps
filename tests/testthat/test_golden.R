context("golden regression (results of rmumps 5.2.1-43)")

# The fixture is written by tools/golden/make-golden.R. Results are compared
# through solutions, determinants and residuals, never through permutations
# or info vectors, which legitimately change between MUMPS versions.

golden <- readRDS(test_path("fixtures", "golden-5.2.1-43.rds"))

triplet_dense <- function(tr, sym) {
  a <- matrix(0, tr$n, tr$n)
  a[cbind(tr$i + 1L, tr$j + 1L)] <- tr$x
  if (sym != 0L) {
    low <- lower.tri(a)
    a[low] <- t(a)[low]
  }
  a
}

close_to <- function(got, want, tol = 1e-8) {
  scale <- max(1, max(abs(want)))
  max(abs(unclass(got) - unclass(want))) <= tol * scale
}

for (cname in names(golden$cases)) {
  g <- golden$cases[[cname]]
  tr <- g$triplet
  a <- triplet_dense(tr, g$sym)
  Bs <- Matrix::sparseMatrix(i = g$Bs$i, p = g$Bs$p, x = g$Bs$x,
                             dims = g$Bs$dim, index1 = FALSE)
  for (oname in names(g$results)) {
    r <- g$results[[oname]]
    perm <- golden$meta$orderings[[oname]]
    test_that(sprintf("%s with %s ordering matches 5.2.1-43", cname, oname), {
      am <- Rmumps$new(tr$i, tr$j, tr$x, tr$n, g$sym)
      am$set_permutation(perm)
      x <- am$solve(g$b)
      # 5.2.1-43 failed on PORD for a 1 x 1 matrix; now it must work.
      want_x <- if (inherits(r$x, "golden_error")) g$reference$x else r$x
      expect_true(close_to(x, want_x))
      expect_true(close_to(drop(a %*% x), g$b))
      if (!inherits(r$xt, "golden_error"))
        expect_true(close_to(am$solvet(g$bt), r$xt))
      if (!inherits(r$X, "golden_error"))
        expect_true(close_to(am$solve(g$B), r$X))
      if (!inherits(r$Xs, "golden_error"))
        expect_true(close_to(solve(am, Bs), r$Xs))
      if (!inherits(r$det, "golden_error"))
        expect_true(close_to(am$det(), r$det))
      if (!inherits(r$log2det, "golden_error"))
        expect_equal(am$log2det(), r$log2det, tolerance = 1e-10)
      if (!is.null(r$inv) && !inherits(r$inv, "golden_error"))
        expect_true(close_to(am$inv(), r$inv))
      am$set_mat_data(2 * tr$x)
      expect_true(close_to(am$solve(g$b), g$reference$x / 2))
    })
  }
}

test_that("singular matrix error is unchanged", {
  s <- golden$singular$triplet
  am <- Rmumps$new(s$i, s$j, s$x, s$n)
  expect_error(am$solve(rep(1, s$n)), "rmumps: job=6, info\\[1\\]=-10, info\\[2\\]=1",
               fixed = FALSE)
})
