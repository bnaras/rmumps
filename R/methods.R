solve.Rcpp_Rmumps <- function (a, b, ...) if(missing(b)) a$inv() else a$solve(b)
determinant.Rcpp_Rmumps <- function (x, logarithm=TRUE, ...) {
  d=x$det()
  li=x$get_infos()
  m=li$rinfog[12]
  z=list()
  z$sign=sign(m)
  z$modulus=if (logarithm) li$infog[34]*log(2.)+log(abs(m)) else abs(d)
  attr(z$modulus, "logarithm")=logarithm
  class(z)="det"
  z
}
dim.Rcpp_Rmumps <- function (x) x$dim()
nrow.Rcpp_Rmumps <- function (x) x$nrow()
ncol.Rcpp_Rmumps <- function (x) x$ncol()
print.Rcpp_Rmumps <- function (x, ...) x$print()
show.Rcpp_Rmumps <- function (object) object$show()

#' Solve a Transposed System
#'
#' Solves \eqn{A^T x = b} for an \code{Rmumps} object holding \eqn{A}.
#'
#' @param a an object created with \code{Rmumps$new()}.
#' @param b right-hand side(s), as accepted by the \code{solve()} method of
#'   \code{Rmumps}. If missing, the transpose of the inverse is returned.
#' @param ... not used.
#' @return The solution: a vector or a matrix, like \code{solve()}.
#' @examples
#' a <- Rmumps$new(c(0L, 1L, 0L), c(0L, 1L, 1L), c(2, 4, 1), 2L)
#' solvet(a, c(2, 9))
#' @export
solvet <- function(a, b, ...) UseMethod("solvet")
solvet.Rcpp_Rmumps <- function (a, b, ...) if(missing(b)) base::t(a$inv()) else a$solvet(b)

# Coercions used from C++ (Rmumps constructors and sparse rhs). They avoid
# the deprecated direct coercions to "dgTMatrix"/"dgCMatrix" of Matrix >= 1.5.
.as_dgT <- function(x) as(as(as(x, "dMatrix"), "generalMatrix"), "TsparseMatrix")
.as_dgC <- function(x) as(as(as(x, "dMatrix"), "generalMatrix"), "CsparseMatrix")
