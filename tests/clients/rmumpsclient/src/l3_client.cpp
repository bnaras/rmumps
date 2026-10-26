// Copyright 2026 Balasubramanian Narasimhan. GPL (>= 2); see rmumps' inst/COPYRIGHTS.
// Client of the Rcpp interface (L3), written like r2sundials and multbxxc.

#include <rmumps.h>
using namespace Rcpp;

extern "C" SEXP l3_solve(SEXP irn_, SEXP jcn_, SEXP a_, SEXP rhs_) {
BEGIN_RCPP
  IntegerVector irn(irn_), jcn(jcn_);
  NumericVector a(a_), rhs(rhs_);
  int n = rhs.size();
  XPtr<Rmumps> pm = rmumps::Rmumps__ptr_ijv(XPtr<int>(irn.begin(), false),
                                            XPtr<int>(jcn.begin(), false),
                                            XPtr<double>(a.begin(), false),
                                            n, a.size(), 0);
  rmumps::Rmumps__set_permutation(pm, RMUMPS_PERM_AMD);
  // r2sundials reads the 'irn' member directly: its offset is frozen.
  bool irn_ok = pm->irn.size() == (size_t) a.size() && pm->irn[0] == irn[0];
  NumericVector x = clone(rhs);
  rmumps::Rmumps__solveptr(pm, XPtr<double>(x.begin(), false), n, 1);
  NumericVector xt = clone(rhs);
  rmumps::Rmumps__solvetptr(pm, XPtr<double>(xt.begin(), false), n, 1);
  double det = rmumps::Rmumps__det(pm);
  rmumps::Rmumps__del_ptr(pm);
  return List::create(_["x"] = x, _["xt"] = xt, _["det"] = det,
                      _["irn_ok"] = irn_ok);
END_RCPP
}
