\name{rmumps-package}
\alias{rmumps-package}
\alias{rmumps}
\docType{package}
\title{
  Rcpp port of MUMPS library for LU or LDL^t factorization of sparse matrices
}
\description{
  Creates a MUMPS compatible object storing a sparse matrix.
  Gives a possibility to do separately symbolic analysis,
  factorization and system solving.
}
\details{
  Create a new Rmumps object with \code{A <- Rmumps$new(asparse)} then solve a
  linear system with one or many right hand sides \code{x <- solve(A, b)}.
  Cf. \link{Rmumps}  
}
\author{
  Serguei Sokol, INRA

  Maintainer: Serguei Sokol (sokol at insa-toulouse.fr)
}
\references{
  MUMPS official site \url{https://mumps-solver.org}

  Sokol S. \emph{rmumps: Wrapper for 'MUMPS' Library}.
  \url{https://CRAN.R-project.org/package=rmumps}
}
\examples{
  # 3 x 3 system given as triplets (0-based indices): A = diag(4, 5, 6)
  A <- Rmumps$new(0:2, 0:2, c(4, 5, 6), 3L)
  x <- solve(A, c(4, 10, 18))
  x  # 1 2 3
}
