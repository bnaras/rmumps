#ifndef __rmumps_h__
#define __rmumps_h__

#define RMUMPS_PERM_AMD 0
#define RMUMPS_PERM_AMF 2
#define RMUMPS_PERM_SCOTCH 3 /* no longer supported: falls back to AUTO */
#define RMUMPS_PERM_PORD 4
#define RMUMPS_PERM_METIS 5
#define RMUMPS_PERM_QAMD 6
#define RMUMPS_PERM_AUTO 7

#include <set>
#include <string>
#include <vector>
#include "rmumps/dmumps_c.h"
#include <Rcpp.h>

// C++ interface to the Rmumps class (Rcpp clients: LinkingTo: rmumps, Rcpp).
//
// Binary compatibility: client packages compiled against this header access
// the data members below directly. Their order and types up to and including
// 'buf' are frozen: do not reorder, insert or remove them, and do not add
// virtual functions. New members go at the end of the private section. Only
// the documented members are part of the interface; 'param' changes type with
// the MUMPS version.
class Rmumps {
public:
  std::vector<MUMPS_INT> irn;
  std::vector<MUMPS_INT> jcn;
  std::vector<MUMPS_INT> irhs_ptr;
  std::vector<MUMPS_INT> irhs_sparse;
  Rcpp::NumericVector rhs;
  Rcpp::NumericMatrix mrhs; // many rhs
  Rcpp::NumericVector rhs_sparse;
  Rcpp::NumericVector anz;
  bool copy;
  int ncore;
  std::set<int> jobs;
  MUMPS_INT sym;
#define NBUF 512
  char buf[NBUF];

  Rmumps(Rcpp::RObject mat);
  Rmumps(Rcpp::RObject mat, int sym);
  Rmumps(Rcpp::RObject mat, int sym, bool copy_);
  Rmumps(Rcpp::IntegerVector i, Rcpp::IntegerVector j, Rcpp::NumericVector x, int n);
  Rmumps(Rcpp::IntegerVector i, Rcpp::IntegerVector j, Rcpp::NumericVector x, int n, int sym);
  Rmumps(Rcpp::IntegerVector i, Rcpp::IntegerVector j, Rcpp::NumericVector x, int n, int sym, bool copy_);
  Rmumps(MUMPS_INT *i, MUMPS_INT *j, double *a, MUMPS_INT n, MUMPS_INT nz, MUMPS_INT sym);
  ~Rmumps();
  void clean();
  //Rmumps shallow_copy(Rmumps a);
  bool get_copy();
  void set_copy(bool copy_);
  int get_permutation();
  void set_permutation(int perm);
  int get_ncore();
  void set_ncore(int ncore_);
  int get_sym();
  void set_sym(int sym);
  void do_job(int job);
  void symbolic();
  void numeric();
  SEXP solve(Rcpp::RObject b);
  SEXP solvet(Rcpp::RObject b);
  Rcpp::NumericVector solvev(Rcpp::NumericVector b);
  Rcpp::NumericMatrix solvem(Rcpp::NumericMatrix b);
  void solveptr(double* b, int nrow, int nrhs);
  Rcpp::NumericMatrix inv();
  Rcpp::NumericMatrix solves(Rcpp::S4 mat);
  Rcpp::NumericMatrix solvestm(Rcpp::List mat);
  void set_rhs_ptr(double *b);
  void set_rhs(Rcpp::NumericVector b);
  void set_mrhs(Rcpp::NumericMatrix b);
  void set_mat_ptr(double *x);
  void set_mat_data(Rcpp::NumericVector x);
  void set_icntl(Rcpp::IntegerVector iv, Rcpp::IntegerVector ii);
  void set_cntl(Rcpp::NumericVector v, Rcpp::IntegerVector iv);
  Rcpp::IntegerVector get_icntl();
  Rcpp::NumericVector get_cntl();
  void set_keep(Rcpp::IntegerVector iv, Rcpp::IntegerVector ii);
  Rcpp::IntegerVector get_keep();
  Rcpp::List get_infos();
  Rcpp::NumericVector get_rhs();
  Rcpp::NumericMatrix get_mrhs();
  Rcpp::IntegerVector dim();
  int nrow();
  int ncol();
  void print();
  Rcpp::List triplet();
  std::string mumps_version();
  double det();
  double log2det();
  Rcpp::IntegerVector get_sym_perm();
  Rcpp::IntegerVector get_uns_perm();
  void set_perm_in(Rcpp::IntegerVector p_in);
  void solvetptr(double* b, int nrow, int nrhs);
  bool is_poisoned();
private:
  int ref; // counts shallow copies of this object
  DMUMPS_STRUC_C param;
  void new_mat(Rcpp::RObject mat, int sym, bool copy_);
  void new_ijv(Rcpp::IntegerVector i0, Rcpp::IntegerVector j0, Rcpp::NumericVector x, int n, int sym, bool copy_);
  void tri_init(MUMPS_INT *irn, MUMPS_INT *jcn, double *a, MUMPS_INT sym);
  void call_mumps(int job);
  Rcpp::IntegerVector perm_in;
};
#include "rmumps_RcppExports.h"
#endif // __rmumps_h__
