context("interface and error handling")

diag_solver <- function(n = 5) Rmumps$new(0:(n - 1), 0:(n - 1), rep(2, n), n)

test_that("full-length control and information vectors", {
  am <- diag_solver()
  expect_length(am$get_icntl(), 60L)
  expect_length(am$get_cntl(), 15L)
  am$solve(rep(1, 5))
  infos <- am$get_infos()
  expect_length(infos$info, 80L)
  expect_length(infos$infog, 80L)
  expect_length(infos$rinfo, 40L)
  expect_length(infos$rinfog, 40L)
  am$set_icntl(5L, 58L)
  expect_equal(am$get_icntl()[58], 5L)
  am$set_cntl(0.5, 7L)
  expect_equal(am$get_cntl()[7], 0.5)
})

test_that("out-of-range control indices are errors", {
  am <- diag_solver()
  expect_error(am$set_icntl(1L, 61L), "out of range")
  expect_error(am$set_icntl(1L, 0L), "out of range")
  expect_error(am$set_cntl(1, 16L), "out of range")
  expect_error(am$set_keep(1L, 501L), "out of range")
  expect_error(am$set_icntl(1:2, 1L), "must be the same")
})

test_that("SCOTCH falls back to AUTO with a warning", {
  am <- diag_solver()
  expect_warning(am$set_permutation(RMUMPS_PERM_SCOTCH), "SCOTCH")
  expect_equal(am$get_permutation(), RMUMPS_PERM_AUTO)
  expect_equal(am$solve(rep(2, 5)), rep(1, 5))
})

test_that("PORD works on a 1 x 1 matrix", {
  am <- Rmumps$new(0L, 0L, 4, 1L)
  am$set_permutation(RMUMPS_PERM_PORD)
  expect_equal(am$solve(2), 0.5)
  expect_equal(am$get_permutation(), RMUMPS_PERM_PORD)
})

test_that("determinant() method", {
  am <- Rmumps$new(0:2, 0:2, c(2, -3, 4), 3L)
  d <- determinant(am)
  expect_s3_class(d, "det")
  expect_equal(d$sign, -1)
  expect_equal(as.numeric(d$modulus), log(24))
  expect_equal(as.numeric(determinant(am, logarithm = FALSE)$modulus), 24)
})

test_that("solvet() is exported and solves the transposed system", {
  am <- Rmumps$new(c(0L, 1L, 0L), c(0L, 1L, 1L), c(2, 4, 1), 2L)
  a <- matrix(c(2, 0, 1, 4), 2)
  expect_true("solvet" %in% getNamespaceExports("rmumps"))
  expect_equal(solvet(am, c(2, 9)), solve(t(a), c(2, 9)))
})

test_that("user permutation is kept alive", {
  n <- 6
  am <- diag_solver(n)
  am$set_perm_in(rev(seq_len(n)))
  invisible(gc())
  expect_equal(am$solve(rep(2, n)), rep(1, n))
  expect_error(am$set_perm_in(1:3), "length")
})

test_that("pointer interface", {
  a <- matrix(c(4, 1, 0, 1, 3, 1, 0, 1, 2), 3)
  am <- Rmumps$new(c(0L, 1L, 0L, 1L, 2L, 1L, 2L), c(0L, 0L, 1L, 1L, 1L, 2L, 2L),
                   c(4, 1, 1, 3, 1, 1, 2), 3L)
  pm <- Rmumps__xptr(am)
  expect_true(inherits(pm, "externalptr"))
  Rmumps__symbolic(pm)
  Rmumps__numeric(pm)
  expect_equal(Rmumps__det(pm), det(a))
  expect_equal(Rmumps__log2det(pm), log2(det(a)))
  expect_equal(Rmumps__mumps_version(pm), am$mumps_version())
  Rmumps__set_icntl(pm, 2L, 58L)
  expect_equal(Rmumps__get_icntl(pm)[58], 2L)
  Rmumps__set_cntl(pm, 0.25, 7L)
  expect_equal(Rmumps__get_cntl(pm)[7], 0.25)
  expect_length(Rmumps__get_infos(pm)$infog, 80L)
  expect_length(Rmumps__get_sym_perm(pm), 3L)
  expect_error(Rmumps__xptr(list()), "Rmumps\\$new")
})

test_that("an R error inside MUMPS poisons the instance cleanly", {
  # The bundled MUMPS 5.2.1 reports its diagnostics as R warnings; with
  # warn = 2 they become R errors raised inside Fortran.
  am <- diag_solver()
  am$set_icntl(c(6L, 6L, 6L, 4L), 1:4)
  op <- options(warn = 2)
  on.exit(options(op))
  err <- tryCatch(am$solve(rep(1, 5)), error = function(e) conditionMessage(e))
  options(op)
  skip_if(is.numeric(err), "this MUMPS build raises no diagnostics")
  expect_match(err, "^rmumps: ")
  expect_error(am$solve(rep(1, 5)), "failed earlier")
  rm(am)
  expect_silent(invisible(gc()))
})

test_that("C entry points are registered", {
  dll <- getLoadedDLLs()[["rmumps"]]
  expect_false(is.null(dll))
  # The registered .Call routines include the Rcpp module and exports.
  routines <- getDLLRegisteredRoutines(dll)$.Call
  expect_true("_rcpp_module_boot_mod_Rmumps" %in% names(routines))
  expect_true("_rmumps_Rmumps__xptr" %in% names(routines))
})
