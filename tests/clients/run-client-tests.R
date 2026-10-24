# Builds the test client package against an installed rmumps and checks every
# way a package can link to it (maintainer gate, not run by R CMD check).
#
# Usage (from the package root):
#   Rscript tests/clients/run-client-tests.R <lib.loc of rmumps>

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L)
  stop("usage: Rscript tests/clients/run-client-tests.R <lib.loc>")
rmumps_lib <- normalizePath(args[[1L]])
client_lib <- tempfile("clientlib")
dir.create(client_lib)
.libPaths(c(client_lib, rmumps_lib, .libPaths()))

src <- file.path("tests", "clients", "rmumpsclient")
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
status <- system2(file.path(R.home("bin"), "R"),
                  c("CMD", "INSTALL", "--no-test-load", "-l", shQuote(client_lib),
                    shQuote(src)), stdout = TRUE, stderr = TRUE)
if (!is.null(attr(status, "status"))) {
  writeLines(status)
  stop("building the client package failed")
}

suppressPackageStartupMessages(library(rmumpsclient, lib.loc = client_lib))
ns <- asNamespace("rmumpsclient")
call <- function(name, ...) .Call(get(paste0("C_", name), envir = ns), ...)

# 3 x 3 unsymmetric system, 1-based triplets
irn <- c(1L, 2L, 1L, 2L, 3L, 3L)
jcn <- c(1L, 1L, 2L, 2L, 2L, 3L)
a <- c(4, 1, 2, 3, 1, 5)
A <- matrix(0, 3, 3)
A[cbind(irn, jcn)] <- a
b <- c(1, 2, 3)
x_ref <- solve(A, b)

check <- function(ok, what) {
  cat(if (isTRUE(ok)) "ok   " else "FAIL ", what, "\n", sep = "")
  if (!isTRUE(ok)) failed <<- c(failed, what)
}
failed <- character()

x <- call("safe_solve", irn, jcn, a, b, 0L)
check(all.equal(x, x_ref) == TRUE, "safe C API solves")

m <- call("meta")
abi <- as.integer(sub(".*RMUMPS_DMUMPS_ABI ([0-9]+).*", "\\1",
  grep("define RMUMPS_DMUMPS_ABI", readLines(system.file(
    "include", "rmumps", "rmumps_abi.h", package = "rmumps", lib.loc = rmumps_lib)),
    value = TRUE)))
check(m[[1]] == 1L && m[[4]] == abi, "API version 1, DMUMPS ABI of the installed headers")
inc <- system.file("include", "rmumps", package = "rmumps", lib.loc = rmumps_lib)
hdr <- unlist(lapply(list.files(inc, full.names = TRUE), readLines))
hdr_ver <- sub('.*"([^"]+)".*', "\\1",
               grep("^#define MUMPS_VERSION \"", hdr, value = TRUE))[1L]
check(identical(m[[2]], hdr_ver), "MUMPS version reported matches the headers")
check(bitwAnd(m[[3]], 2^3) == 0 && bitwAnd(m[[3]], 2^5) != 0,
      "orderings: METIS available, SCOTCH not")

sm <- call("size_mismatch")
check(sm[[1]] == 2L && grepl("bytes", sm[[2]]), "size mismatch is reported")
check(call("null_struct") == 1L, "NULL structure is reported")

e <- tryCatch(call("abi_mismatch"), error = function(e) conditionMessage(e))
check(is.character(e) && grepl("not provided", e),
      "other ABI gets a clean 'not provided' error")

x <- call("legacy_solve", irn, jcn, a, b)
check(all.equal(x, x_ref) == TRUE, "legacy \"dmumps_c\" callable solves (5.2.1 layout)")

r1 <- call("abi1_safe_solve", irn, jcn, a, b)
check(all.equal(r1[[1]], x_ref) == TRUE && r1[[2]] == 0L,
      "ABI-1 safe API (rmumps 5.2.1-44 headers) solves")
check(r1[[3]] == r1[[4]], "ABI-1 'deficiency' mirrors INFOG(28)")
check(nzchar(r1[[5]]) && startsWith(r1[[5]], m[[2]]), "ABI-1 version_number is copied back")
check(call("abi1_size_mismatch") == 2L, "ABI-1 size mismatch is reported")
ni <- call("abi1_not_initialized")
check(ni[[1]] != 0L && grepl("not initialized", ni[[2]]),
      "ABI-1 call on an uninitialized structure is refused")
for (i in 1:200) call("abi1_safe_solve", irn, jcn, a, b)
check(TRUE, "ABI-1 repeated init/solve/free cycles")

r <- call("l3_solve", irn, jcn, a, b)
check(all.equal(r$x, x_ref) == TRUE, "Rcpp interface solves")
check(all.equal(r$xt, solve(t(A), b)) == TRUE, "Rcpp interface solves transposed")
check(all.equal(r$det, det(A)) == TRUE, "Rcpp interface determinant")
check(isTRUE(r$irn_ok), "Rmumps::irn member is where clients expect it")

if (length(failed)) {
  cat(length(failed), "client check(s) failed\n")
  quit(status = 1)
}
cat("all client checks passed\n")
