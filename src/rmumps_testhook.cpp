// Test hook, kept out of the C++ interface of rmumps (this file has no
// Rcpp::interfaces attribute) and out of the exported R functions.

#include <Rcpp.h>
#include "rmumps_internal.h"

//' Make the next MUMPS call fail inside the error boundary (tests only)
//' @noRd
// [[Rcpp::export(name = ".rmumps_test_fail_next", invisible = true)]]
void rmumps_test_fail_next_r() {
  rmumps_test_fail_next();
}
