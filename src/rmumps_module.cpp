// Port of sequential MUMPS to R.
// Only a subset of MUMPS is exposed to R.
// Author of port: Serguei Sokol, INRA, Toulouse, FRANCE
// Copyright 2015, INRA
// v 0.1 2015-09-02

#include <Rcpp.h>
using namespace Rcpp;

//#include <libseq/mpi.h> // useless in sequential mode
#define JOB_INIT -1
#define JOB_END -2
#define USE_COMM_WORLD -987654
#define ICNTL(I) icntl[(I)-1] /* macro s.t. indices match documentation */

#ifdef _OPENMP
#include <omp.h>
#endif

#include "../inst/include/rmumps.h"
#include "../inst/include/rmumps/rmumps_api.h"
#include "rmumps_internal.h"

// lengths of the MUMPS control and information arrays, taken from the
// structure so that they follow the vendored MUMPS version
#define RMUMPS_NELEM(m) ((int) (sizeof(((DMUMPS_STRUC_C *) 0)->m) / sizeof(((DMUMPS_STRUC_C *) 0)->m[0])))
#define RMUMPS_NICNTL RMUMPS_NELEM(icntl)
#define RMUMPS_NCNTL RMUMPS_NELEM(cntl)
#define RMUMPS_NINFO RMUMPS_NELEM(info)
#define RMUMPS_NRINFO RMUMPS_NELEM(rinfo)
#define RMUMPS_NKEEP RMUMPS_NELEM(keep)

Rmumps::~Rmumps() { if (ref == 0) clean(); }
void Rmumps::clean() {
  // Never throws: called from the destructor. A poisoned instance is skipped
  // by rmumps_dmumps_c_safe(), and any failure here is ignored.
  param.job=JOB_END;
  rmumps_dmumps_c_safe(&param, sizeof param, NULL, 0);
}
bool Rmumps::is_poisoned() {
  return param.info[0] == RMUMPS_INFO_POISONED;
}

// Short descriptions of MUMPS errors (INFO(1) < 0), from the user guide.
static const char *info_message(int info1) {
  switch (info1) {
  case -1: return "an error occurred on another processor";
  case -2: return "NNZ (or NZ) is out of range";
  case -3: return "MUMPS was called with an invalid value of JOB";
  case -4: return "error in the user-provided permutation (PERM_IN)";
  case -5: return "not enough memory for real workspace during analysis";
  case -6: return "the matrix is singular in structure";
  case -7: return "not enough memory for integer workspace during analysis";
  case -8: return "the main internal integer workarray is too small; increase ICNTL(14)";
  case -9: return "the main internal real workarray is too small; increase ICNTL(14)";
  case -10: return "the matrix is numerically singular";
  case -11: return "the internal real workarray is too small for the solution phase";
  case -12: return "the internal real workarray is too small for iterative refinement";
  case -13: return "a memory allocation failed";
  case -14: return "the internal integer workarray is too small for the solution phase";
  case -15: return "the integer workarray is too small for iterative refinement or error analysis";
  case -16: return "N is out of range";
  case -17: return "the internal send buffer is too small";
  case -19: return "the maximum allowed memory size (ICNTL(23)) is too small";
  case -20: return "the internal reception buffer is too small";
  case -21: return "the value of PAR is not valid for a single process";
  case -22: return "an input array is not allocated or has a wrong size";
  case -23: return "MPI was not initialized";
  case -24: return "NELT is out of range";
  case -25: return "a problem occurred in BLACS initialization";
  case -26: return "LRHS is out of range";
  case -27: return "NZ_RHS and IRHS_PTR(NRHS+1) do not match";
  case -28: return "IRHS_PTR(1) is not equal to 1";
  case -29: return "LSOL_LOC is smaller than INFO(23)";
  case -30: return "SCHUR_LLD is out of range";
  case -32: return "incompatible values of NRHS and ICNTL(25)";
  case -34: return "LREDRHS is out of range";
  case -38: return "parallel analysis was requested but not available";
  case -40: return "the matrix was declared positive definite (SYM = 1) but a negative or null pivot was found";
  case -44: return "the solve phase cannot be done because the factors were discarded (ICNTL(31))";
  case -45: return "NRHS is not positive";
  case -46: return "NZ_RHS is not positive";
  case -47: return "entries of A^-1 were requested (ICNTL(30)) but NRHS is not equal to N";
  case -48: return "entries of A^-1 were requested (ICNTL(30)) with an incompatible ICNTL value (index in INFO(2))";
  case -49: return "SIZE_SCHUR has an incorrect value";
  case -50: return "an error occurred while computing the fill-reducing ordering";
  case -51: return "the graph is too large for an ordering library built with 32-bit integers";
  case -52: return "the ordering libraries must use the same integer size as MUMPS";
  case -53: return "internal error: inconsistent input data between two consecutive calls";
  default:
    if (info1 <= -90 && info1 >= -99) return "an out-of-core (disk I/O) error occurred";
    return "see the MUMPS user guide for this INFO(1) value";
  }
}

// One MUMPS call through the error boundary: R errors and interrupts raised
// inside MUMPS become a C++ exception here, never a longjmp through Fortran.
void Rmumps::call_mumps(int job) {
  char msg[NBUF];
  param.job=job;
  int rc=rmumps_dmumps_c_safe(&param, sizeof param, msg, sizeof msg);
  if (rc != RMUMPS_OK)
    stop("%s (job=%d)", msg, job);
}
/* unfortunately, there is an unwanted extra destructor call
Rmumps Rmumps::shallow_copy(Rmumps a) {
  Rmumps x=a;
  x.ref++;
  return x;
}
* */
bool Rmumps::get_copy() {
  return copy;
}
void Rmumps::set_copy(bool copy_) {
  copy=copy_;
}
void Rmumps::set_permutation(int perm) {
  if (perm < RMUMPS_PERM_AMD || perm > RMUMPS_PERM_AUTO || perm == 1)
    stop("Rmumps::set_permutation: invalid perm value %d", perm);
  if (perm == RMUMPS_PERM_SCOTCH) {
    warning("Rmumps::set_permutation: SCOTCH ordering is no longer supported; using RMUMPS_PERM_AUTO instead");
    perm=RMUMPS_PERM_AUTO;
  }
  if (perm != param.ICNTL(7))
    jobs.erase(jobs.begin(), jobs.end()); // invalidate previous factorizations both symbolic and numeric
  param.ICNTL(7)=perm;
}
int Rmumps::get_permutation() {
  return param.ICNTL(7);
}
int Rmumps::get_ncore() {
  return ncore;
}
void Rmumps::set_ncore(int ncore_) {
#ifdef _OPENMP
  ncore=ncore_;
#else
  ncore=1;
#endif
}
int Rmumps::get_sym() {
  return param.sym;
}
void Rmumps::set_sym(int sym) {
  stop("Parameter 'sym' cannot be changed after matrix initialization");
}
void Rmumps::do_job(int job) {
  // downgrade job request if preliminaries are already done 
  switch (job) {
  case 4:
    if (jobs.count(1) == 1) {
       job=2;
    }
    break;
  case 5:
  case 6:
    if (jobs.count(2) == 1) {
       job=3;
    }
    break;
  }
  // do preliminary work if need
  switch (job) {
  case 2:
    if (jobs.count(1) != 1) {
       do_job(1);
    }
    break;
  case 3:
    if (jobs.count(2) != 1) {
       do_job(2);
    }
    break;
  }
  if (job != JOB_INIT && is_poisoned())
    stop("rmumps: this Rmumps object failed earlier and cannot be used; create a new one");
  // PORD cannot order a 1 x 1 matrix (it fails with "#stages = 2"); any
  // ordering gives the same result there, so use AMD for the analysis.
  bool pord_guard=(job == 1 || job == 4 || job == 6) && param.n <= 1 &&
    param.ICNTL(7) == RMUMPS_PERM_PORD;
  if (pord_guard)
    param.ICNTL(7)=RMUMPS_PERM_AMD;
  call_mumps(job);
  if (pord_guard)
    param.ICNTL(7)=RMUMPS_PERM_PORD;
  if (param.info[0] < 0) {
    stop("rmumps: job=%d, info[1]=%d, info[2]=%d: %s", job, param.info[0],
         param.info[1], info_message(param.info[0]));
  }
  if (param.info[0] > 0 && job != JOB_INIT) {
    warning("rmumps: job=%d, info[1]=%d, info[2]=%d (MUMPS warning, see the MUMPS user guide)",
            job, param.info[0], param.info[1]);
  }
  /* combined jobs are split for the record */
  switch (job) {
  case 1:
  case 2:
  case 3:
    jobs.insert({job});
    break;
  case 4:
    jobs.insert({1, 2});
    break;
  case 5:
    jobs.insert({2, 3});
    break;
  case 6:
    jobs.insert({1, 2, 3});
    break;
  }
}
void Rmumps::symbolic() {
  do_job(1);
}
void Rmumps::numeric() {
  do_job(4);
}
SEXP Rmumps::solvet(RObject b) {
  // solve A^t x = b
  int icntl9=param.ICNTL(9);
  param.ICNTL(9) = 2;
  SEXP res;
  try {
    res=solve(b);
  } catch (...) {
    param.ICNTL(9) = icntl9;
    throw;
  }
  param.ICNTL(9) = icntl9;
  return res;
}
SEXP Rmumps::solve(RObject b) {
  /* wrapper for multiple type rhs */
  switch(b.sexp_type()) {
  case INTSXP:
  case REALSXP:
    if (b.hasAttribute("dim")) {
      // many dense rhs
      return solvem(as<NumericMatrix>(b));
    } else {
      // one dense rhs
      return solvev(as<NumericVector>(b));
    }
    break;
  case S4SXP:
    // sparse or dense many rhs
    if (b.inherits("dgeMatrix")) {
      // many dense rhs
      NumericVector bc(b.attr("x"));
      bc.attr("dim")=b.attr("Dim");
      return solvem(as<NumericMatrix>(bc));
    } else {
      // many sparse rhs
      return solves(as<S4>(b));
    }
    break;
  case VECSXP:
    // sparse or dense many rhs
    if (b.inherits("simple_triplet_matrix")) {
      // many sparse rhs
      return solvestm(as<List>(b));
    } else {
      snprintf(buf, NBUF-1, "expected simple_triplet_matrix but got something else");
      stop(buf);
    }
    break;
  default:
    snprintf(buf, NBUF-1, "unauthorized SEXP type of rhs (%d)", b.sexp_type());
    stop(buf);
    return R_NilValue;
  }
  /*return List::create(Named("is_null") = wrap(b.isNULL()),
                      Named("type") = wrap(b.sexp_type()),
                      Named("has_class") = wrap(b.isObject()),
                      Named("is_S4") = wrap(b.isS4()),
                      Named("attr_names") = b.attributeNames());
  */
  return R_NilValue;
}
NumericVector Rmumps::solvev(NumericVector b) {
  if (copy) {
    rhs=clone(b); // will hold the solution without altering b
  } else {
    rhs=b;
  }
  param.rhs=&*rhs.begin();
  param.nrhs=1;
  param.lrhs=0;
  param.ICNTL(20)=0; // rhs is dense
  do_job(6);
  return(rhs);
}
NumericMatrix Rmumps::solvem(NumericMatrix b) {
  if (copy) {
    mrhs=clone(b);
  } else {
    mrhs=b;
  }
  param.lrhs=b.nrow();
  param.ICNTL(20)=0; // rhs is dense
  param.rhs=&*mrhs.begin();
  param.nrhs=b.ncol();
#if 0 // #ifdef _OPENMP
  // unfortunately, MUMPS is not thread safe, there is a writing in working arrays during solving phase
  int ncol=b.ncol();
  int nthr=std::max(1, std::min(ncol , ncore)); // thread number to launch
  int ncol_th=b.ncol()/nthr; // col number per thread
  if (nthr == 1) {
    param.nrhs=ncol;
    do_job(6);
  } else {
    #pragma omp parallel for num_threads(nthr)
    for (int ith=0; ith < nthr; ith++) {
      Rmumps pth=*this;
      pth.ref++;
      /* Rcout << "jobs=" << std::endl;
      std::copy(jobs.begin(), jobs.end(), std::ostream_iterator<int>(Rcout, ", "));
      Rcout << std::endl;
      Rcout << "pth.jobs=" << std::endl;
      std::copy(pth.jobs.begin(), pth.jobs.end(), std::ostream_iterator<int>(Rcout, ", "));
      Rcout << std::endl;
      */
      pth.param.rhs=&*mrhs.begin()+ith*ncol_th*pth.param.lrhs;
      pth.param.nrhs=(ith == nthr-1 ? ncol-ith*ncol_th : ncol_th);
      /*
      Rcout << "param.rhs=" << param.rhs << ", nrhs=" << pth.param.nrhs << std::endl;
      Rcout << "pth.param.rhs=" << pth.param.rhs << std::endl;
      */
      pth.do_job(6);
    }
  }
#else
  do_job(6);
#endif
  return(mrhs);
}
void Rmumps::solveptr(double* b, int lrhs, int nrhs) {
  // in place solve
  param.rhs=b;
  param.nrhs=nrhs;
  param.lrhs=lrhs;
  param.ICNTL(20)=0; // rhs is dense
  do_job(3);
}
void Rmumps::solvetptr(double* b, int lrhs, int nrhs) {
  // in place solve of the transposed system A^t x = b
  int icntl9=param.ICNTL(9);
  param.ICNTL(9)=2;
  try {
    solveptr(b, lrhs, nrhs);
  } catch (...) {
    param.ICNTL(9)=icntl9;
    throw;
  }
  param.ICNTL(9)=icntl9;
}
NumericMatrix Rmumps::inv() {
  MUMPS_INT n=param.n;
  MUMPS_INT nrhs=param.n;
  MUMPS_INT nz_rhs=param.n;
  rhs_sparse=NumericVector(n, 1.);
  irhs_ptr.resize(nrhs+1);
  irhs_sparse.resize(nz_rhs);
  for (int i=0; i < nz_rhs; i++) { // move to 1-based indices
    irhs_sparse[i]=i+1;
  }
  for (int i=0; i <= nrhs; i++) { // move to 1-based indices
    irhs_ptr[i]=i+1;
  }
  mrhs=NumericMatrix(n, nrhs);
  param.nz_rhs=nz_rhs;
  param.nrhs=nrhs;
  param.lrhs=n;
  param.irhs_ptr=&*irhs_ptr.begin();
  param.irhs_sparse=&*irhs_sparse.begin();
  param.rhs_sparse=&*rhs_sparse.begin();
  param.rhs=&*mrhs.begin(); // will hold the solution
  param.ICNTL(20)=1; // decide automaticaly about this sparsity exploit
  do_job(6);
  return(mrhs);
}
NumericMatrix Rmumps::solves(S4 mat) {
  // solve sparse rhs (may be multiple)
  // if dim(mat) == (0,0) then an inverse matrix is requested
  IntegerVector di(mat.slot("Dim"));
  if (!mat.inherits("dgCMatrix")) {
    Function as_=Environment::namespace_env("rmumps")[".as_dgC"];
    mat=as_(mat);
    //stop("matrix must be of dgTMatrix class (cf. pkg Matrix)");
  }
  //if (!mat.inherits("dgCMatrix")) {
  //  CharacterVector cstr(mat.slot("class"));
  //  snprintf(buf, NBUF-1, "sparse rhs matrix must be of dgCMatrix class. Instead '%s' class is received (cf. pkg Matrix)", as<std::string>(cstr[0]).c_str());
  //  stop(buf);
  //}
  if (di[0] == 0 && di[1] == 0) {
     return(inv());
  }
  if (di[0] != param.n) stop("sparse rhs matrix must have the same number of rows than system matrix A");
  /* prepare a triplicate: sparse, pointer rhs_sparse */
  rhs_sparse=mat.slot("x");
  MUMPS_INT n=di[0];;
  MUMPS_INT nrhs=di[1];
  MUMPS_INT nz_rhs=rhs_sparse.size();
  IntegerVector mi(mat.slot("i"));
  IntegerVector mp(mat.slot("p"));
  irhs_ptr.resize(nrhs+1);
  irhs_sparse.resize(nz_rhs);
  for (int i=0; i < nz_rhs; i++) { // move to 1-based indices
    irhs_sparse[i]=mi[i]+1;
  }
  for (int i=0; i <= nrhs; i++) { // move to 1-based indices
    irhs_ptr[i]=mp[i]+1;
  }

  mrhs=NumericMatrix(n, nrhs);
  param.nz_rhs=nz_rhs;
  param.nrhs=nrhs;
  param.lrhs=n;
  param.irhs_ptr=&*irhs_ptr.begin();
  param.irhs_sparse=&*irhs_sparse.begin();
  param.rhs_sparse=&*rhs_sparse.begin();
  param.rhs=&*mrhs.begin(); // will hold the solution
  param.ICNTL(20)=1; // decide automaticaly about this sparsity exploit
  do_job(6);
  return(mrhs);
}
NumericMatrix Rmumps::solvestm(List mat) {
  // solve sparse rhs (may be multiple)
  // mat is expected to be of type slam::simple_triplet_matrix
  // if dim(mat) == (0,0) then an inverse matrix is requested
  int nrow=mat["nrow"];
  int ncol=mat["ncol"];
  if (!mat.inherits("simple_triplet_matrix")) {
    snprintf(buf, NBUF-1, "solvestm() expects an rhs matrix of simple_triplet_matrix class");
    stop(buf);
  }
  if (nrow == 0 && ncol == 0) {
     return(inv());
  }
  if (nrow != param.n) stop("sparse rhs matrix must have the same number of rows than system matrix A");
  /* prepare a triplicate: sparse, pointer rhs_sparse */
  IntegerVector mi(as<IntegerVector>(mat["i"]));
  IntegerVector mj(as<IntegerVector>(mat["j"]));
  NumericVector mv(as<NumericVector>(mat["v"]));
//print(mi);
//print(mj);
//print(mv);

  MUMPS_INT n=nrow;
  MUMPS_INT nrhs=ncol;
  MUMPS_INT nz_rhs=mi.size();
  irhs_ptr.resize(nrhs+1);
  irhs_sparse.resize(nz_rhs);

  // bring to ccs format
  // first sort, as stm does not impose this
  IntegerVector iv1=mi+(mj-1)*nrow;
  IntegerVector iv1s=clone(iv1).sort();
  IntegerVector o=match(iv1s, iv1);
  mi=mi[o-1];
  mj=mj[o-1];
  rhs_sparse=mv[o-1];
//print(mi);
//print(mj);
//print(mv);
  for (int i=0; i < nz_rhs; i++) {
    irhs_sparse[i]=mi[i];
  }
  irhs_ptr[0]=1;
  int ip=0;
  for (int i=1; i <= nrhs; i++) {
    // count entries in this column
    int count=0;
    for (; ip < nz_rhs && mj[ip] == i; ip++, count++) {
//Rcout << "ip=" << ip << "; c=" << count << std::endl;
    }
//Rcout << "after ip=" << ip << "; c=" << count << std::endl;
    irhs_ptr[i]=irhs_ptr[i-1]+count;
  }
//Rcout << "nz_rhs=" << nz_rhs << std::endl;
//print(IntegerVector(irhs_ptr.begin(), irhs_ptr.end()));
  mrhs=NumericMatrix(n, nrhs);
  param.nz_rhs=nz_rhs;
  param.nrhs=nrhs;
  param.lrhs=n;
  param.irhs_ptr=&*irhs_ptr.begin();
  param.irhs_sparse=&*irhs_sparse.begin();
  param.rhs_sparse=&*rhs_sparse.begin();
  param.rhs=&*mrhs.begin(); // will hold the solution
  param.ICNTL(20)=1; // decide automaticaly about this sparsity exploit
  do_job(6);
  return(mrhs);
}
void Rmumps::set_rhs_ptr(double *b) {
  // one dense rhs
  param.ICNTL(20)=0; // rhs is dense;
  param.rhs=b;
  param.nrhs=1;
  param.lrhs=0;
}

void Rmumps::set_rhs(NumericVector b) {
  if (copy) {
    rhs=clone(b);
  } else {
    rhs=b;
  }
  set_rhs_ptr(&*rhs.begin());
}
void Rmumps::set_mrhs(NumericMatrix b) {
  // many dense rhs
  param.ICNTL(20)=0; // rhs is dense;
  if (copy) {
    mrhs=clone(b);
  } else {
    mrhs=b;
  }
  param.rhs=&*mrhs.begin();
  param.nrhs=mrhs.ncol();
  param.lrhs=mrhs.nrow();
}
void Rmumps::set_mat_ptr(double *x) {
  // for the same matrix pattern, set new entry values
  param.a=x;
  // tell that previous numeric factorization is no more valid
  jobs.erase(2);
}

void Rmumps::set_mat_data(NumericVector x) {
  // for the same matrix pattern, set new entry values
  if (copy) {
    anz=clone(x);
  } else {
    anz=x;
  }
  set_mat_ptr(&*anz.begin());
}
NumericVector Rmumps::get_rhs() {
  return rhs;
}
NumericMatrix Rmumps::get_mrhs() {
  return mrhs;
}
void Rmumps::set_icntl(IntegerVector iv, IntegerVector ii) {
  // set control vector ICNTL at positions in ii (1-based)) to the values in iv
  if (iv.size() != ii.size()) {
    snprintf(buf, NBUF-1, "set_icntl: length(iv) and length(ii) must be the same (got %d and %d respectively)", (int) iv.size(), (int) ii.size());
    stop(buf);
  }
  for (auto i=0; i < ii.size(); i++) {
    if (ii[i] == NA_INTEGER || ii[i] > RMUMPS_NICNTL || ii[i] < 1)
      stop("set_icntl: index %d is out of range 1..%d", ii[i], RMUMPS_NICNTL);
    if (iv[i] == NA_INTEGER)
      stop("set_icntl: NA value for ICNTL(%d)", ii[i]);
  }
  for (auto i=0; i < ii.size(); i++)
    param.ICNTL(ii[i])=iv[i];
}
void Rmumps::set_cntl(NumericVector v, IntegerVector iv) {
  // set control vector CNTL at positions in iv (1-based)) to the values in v
  if (v.size() != iv.size()) {
    snprintf(buf, NBUF-1, "set_cntl: length(v) and length(iv) must be the same (got %d and %d respectively)", (int) v.size(), (int) iv.size());
    stop(buf);
  }
  for (auto i=0; i < iv.size(); i++) {
    if (iv[i] == NA_INTEGER || iv[i] > RMUMPS_NCNTL || iv[i] < 1)
      stop("set_cntl: index %d is out of range 1..%d", iv[i], RMUMPS_NCNTL);
  }
  for (auto i=0; i < iv.size(); i++)
    param.cntl[iv[i]-1]=v[i];
}
IntegerVector Rmumps::get_icntl() {
  // return control vector ICNTL
  return IntegerVector(param.icntl, param.icntl+RMUMPS_NICNTL);
}
NumericVector Rmumps::get_cntl() {
  // return control vector CNTL
  return NumericVector(param.cntl, param.cntl+RMUMPS_NCNTL);
}
List Rmumps::get_infos() {
  // return a named list with vectors info, rinfo, infog and rinfog
  NumericVector rinfo(RMUMPS_NRINFO), rinfog(RMUMPS_NRINFO);
  IntegerVector info(RMUMPS_NINFO), infog(RMUMPS_NINFO);
  for (auto i=0; i < rinfo.size(); i++)
    rinfo[i]=param.rinfo[i];
  for (auto i=0; i < info.size(); i++)
    info[i]=param.info[i];
  for (auto i=0; i < rinfog.size(); i++)
    rinfog[i]=param.rinfog[i];
  for (auto i=0; i < infog.size(); i++)
    infog[i]=param.infog[i];
  return List::create(
    _("info")=info,
    _("rinfo")=rinfo,
    _("infog")=infog,
    _("rinfog")=rinfog
  );
}
void Rmumps::set_keep(IntegerVector iv, IntegerVector ii) {
  // set control vector KEEP at positions in ii (1-based)) to the values in iv
  // only 1 <= ii <= 500 are effectively used
  if (iv.size() != ii.size()) {
    snprintf(buf, NBUF-1, "set_keep: length(iv) and length(ii) must be the same (got %d and %d respectively)", (int) iv.size(), (int) ii.size());
    stop(buf);
  }
  for (auto i=0; i < ii.size(); i++) {
    if (ii[i] == NA_INTEGER || ii[i] > RMUMPS_NKEEP || ii[i] < 1)
      stop("set_keep: index %d is out of range 1..%d", ii[i], RMUMPS_NKEEP);
  }
  for (auto i=0; i < ii.size(); i++)
    param.keep[ii[i]-1]=iv[i];
}
IntegerVector Rmumps::get_keep() {
  // get a copy of control vector KEEP of length 500
  IntegerVector res(RMUMPS_NKEEP);
  for (auto i=0; i < res.size(); i++)
    res[i]=param.keep[i];
  return res;
}

IntegerVector Rmumps::dim() {
  return IntegerVector::create(param.n, param.n);
}
int Rmumps::nrow() {
  return param.n;
}
int Rmumps::ncol() {
  return param.n;
}
void Rmumps::print() {
  Rcout << "A " << param.n << "x" << param.n << " Rmumps matrix" << std::endl;
  Rcout << "Decomposition(s) done: ";
  if (jobs.count(1) == 1) {
    Rcout << "symbolic";
  } else {
    Rcout << "none";
  }
  if (jobs.count(2) == 1) {
    Rcout << ", numeric";
  }
  Rcout << std::endl;
}
List Rmumps::triplet() {
  // returns a list with fields i, j, v of class slam::simple_triplet_matrix
  // representing the stored matrix. Objects i, j, v are cloned so the
  // returned object can be manipulated without risk for the original matrix
  List tr=List::create(
    _["i"]=IntegerVector(param.irn, param.irn+param.nz),
    _["j"]=IntegerVector(param.jcn, param.jcn+param.nz),
    _["v"]=NumericVector(param.a, param.a+param.nz),
    _["nrow"]=IntegerVector(1, param.n),
    _["ncol"]=IntegerVector(1, param.n),
    _["dimnames"]=R_NilValue
  );
  tr.attr("class")="simple_triplet_matrix";
  return tr;
}
void Rmumps::set_perm_in(IntegerVector p_in) {
  // set a vector of symmetric permutation provided by user
  if (p_in.size() != param.n)
    stop("set_perm_in: the permutation must have length %d (got %d)", (int) param.n, (int) p_in.size());
  param.ICNTL(7)=1;
  param.ICNTL(28)=1;
  perm_in=clone(p_in); // kept alive by this object while MUMPS uses it
  param.perm_in=(MUMPS_INT*) &*perm_in.begin();
  jobs.erase(jobs.begin(), jobs.end()); // invalidate previous factorizations both symbolic and numeric
}
IntegerVector Rmumps::get_sym_perm() {
  // returns a vector of symmetric permutation used by mumps
  if (param.sym_perm)
    return IntegerVector(param.sym_perm, param.sym_perm+param.n);
  else
    return IntegerVector(0);
}
IntegerVector Rmumps::get_uns_perm() {
  // returns a vector of unsymmetric permutation used by mumps
  // Rcout << "param.uns_perm=" << param.uns_perm << std::endl;
  if (param.uns_perm)
    return IntegerVector(param.uns_perm, param.uns_perm+param.n);
  else
    return IntegerVector(0);
}
std::string Rmumps::mumps_version() { return MUMPS_VERSION; }
double Rmumps::det() {
  if (jobs.count(2) != 1 || param.ICNTL(33) != 1) {
    param.ICNTL(33)=1;
    do_job(4);
  }
  return param.rinfog[12-1]*exp2(param.infog[34-1]);
}
double Rmumps::log2det() {
  if (jobs.count(2) != 1 || param.ICNTL(33) != 1) {
    param.ICNTL(33)=1;
    do_job(4);
  }
  return log2(param.rinfog[12-1]) + param.infog[34-1];
}

/* constructors */
Rmumps::Rmumps(RObject mat, int sym, bool copy_) {
  new_mat(mat, sym, copy_);
}
Rmumps::Rmumps(RObject mat, int sym) {
  new_mat(mat, sym, true);
}
Rmumps::Rmumps(RObject mat) {
  new_mat(mat, 0, true); // default for sym=0 (unsymetric), copy_true
}
Rmumps::Rmumps(IntegerVector i0, IntegerVector j0, NumericVector x, int n) {
  new_ijv(i0, j0, x, n, 0, true);
}
Rmumps::Rmumps(IntegerVector i0, IntegerVector j0, NumericVector x, int n, int sym) {
  new_ijv(i0, j0, x, n, sym, true);
}
Rmumps::Rmumps(IntegerVector i0, IntegerVector j0, NumericVector x, int n, int sym, bool copy_) {
  new_ijv(i0, j0, x, n, sym, copy_);
}
Rmumps::Rmumps(MUMPS_INT *i, MUMPS_INT *j, double *a, MUMPS_INT n, MUMPS_INT nz, MUMPS_INT sym) {
  irn.resize(nz);
  jcn.resize(nz);
  copy=false;
  std::copy(i, i+nz, irn.begin());
  std::copy(j, j+nz, jcn.begin());
  tri_init(i, j, a, sym);
  param.n=n;
  param.nz=nz;
}
/* end of constructors */

/* helpers */
void Rmumps::new_mat(RObject mat_, int sym, bool copy_) {
  MUMPS_INT n=-1;
  MUMPS_INT nz=-1;
  switch(mat_.sexp_type()) {
  case S4SXP: {
    S4 mat=as<S4>(mat_);
    if (!mat.inherits("dgTMatrix")) {
       Function as_=Environment::namespace_env("rmumps")[".as_dgT"];
       mat=as_(mat);
       //stop("matrix must be of dgTMatrix class (cf. pkg Matrix)");
    }
    IntegerVector di(mat.slot("Dim"));
    if (di[0] != di[1]) stop("matrix must be square");
    /* prepare a triplicate: irn, jcn, a */
    NumericVector x(mat.slot("x"));
    IntegerVector mi(mat.slot("i"));
    IntegerVector mj(mat.slot("j"));
    n=di[0];
    nz=x.size();
    irn.resize(nz);
    jcn.resize(nz);
    copy=copy_;
    if (copy) {
      anz=clone(x);
    } else {
      anz=x;
    }
    for (int i=0; i < nz; i++) { // move to 1-based indices
      irn[i]=mi[i]+1;
      jcn[i]=mj[i]+1;
    }
    break;
  }
  case VECSXP: {
    List mat=as<List>(mat_);
    if (!mat_.inherits("simple_triplet_matrix")) {
       stop("constructor input of type VECSXP must be of simple_triplet_matrix class (cf. pkg slam)");
    }
    n=mat["nrow"];
    if (n != as<int>(mat["ncol"])) stop("matrix must be square");
    /* prepare a triplicate: irn, jcn, a */
    NumericVector x(as<NumericVector>(mat["v"]));
    IntegerVector mi(as<IntegerVector>(mat["i"]));
    IntegerVector mj(as<IntegerVector>(mat["j"]));
    nz=x.size();
    irn.resize(nz);
    jcn.resize(nz);
    copy=copy_;
    if (copy) {
      anz=clone(x);
    } else {
      anz=x;
    }
    for (int i=0; i < nz; i++) { // already in 1-based indices
      irn[i]=mi[i];
      jcn[i]=mj[i];
    }
    break;
  }
  default:
    snprintf(buf, NBUF-1, "constructor from a single object is expecting Matrix::dgTMatrix (i.e. S4SXP) or slam::simple_triplet_matrix (i.e. VECSXP) class as input. Got '%d' SEXP instead", mat_.sexp_type());
    stop(buf);
  }
  //Rf_PrintValue(wrap(irn));
  //Rf_PrintValue(wrap(jcn));
  //Rf_PrintValue(wrap(anz));
  tri_init(&*irn.begin(), &*jcn.begin(), &*anz.begin(), sym);
  param.n=n;
  param.nz=nz;
}
void Rmumps::new_ijv(IntegerVector i0, IntegerVector j0, NumericVector x, int n_, int sym, bool copy_) {
  MUMPS_INT nz=x.size();
  MUMPS_INT n=n_;
  irn.resize(nz);
  jcn.resize(nz);
  for (int i=0; i < nz; i++) { // move to 1-based indices
    irn[i]=i0[i]+1;
    jcn[i]=j0[i]+1;
  }
  copy=copy_;
  if (copy) {
    anz=clone(x);
  } else {
    anz=x;
  }
  //Rf_PrintValue(wrap(irn));
  //Rf_PrintValue(wrap(jcn));
  //Rf_PrintValue(wrap(anz));
  tri_init(&*irn.begin(), &*jcn.begin(), &*anz.begin(), sym);
  param.n=n;
  param.nz=nz;
}

// real initializer
void Rmumps::tri_init(MUMPS_INT *irn, MUMPS_INT *jcn, double *a, MUMPS_INT sym) {
  this->sym=sym;
  this->ncore=1;
  this->ref=0;
  /* Initialize a MUMPS instance. Use MPI_COMM_WORLD */
  memset(&param, 0, sizeof param); // MUMPS reads some fields before setting them
  param.job=JOB_INIT;
  param.par=1;
  param.sym=sym;
  param.comm_fortran=USE_COMM_WORLD;
  do_job(JOB_INIT);
  param.irn=irn;
  param.jcn=jcn;
  param.a=a;
  /* No outputs */
  param.ICNTL(1)=-1; param.ICNTL(2)=-1; param.ICNTL(3)=-1; param.ICNTL(4)=0;
  param.ICNTL(5)=0; param.ICNTL(18)=0; // matrix is centralized and assembled
  param.ICNTL(6)=0; // no column permutation
  param.ICNTL(7)=RMUMPS_PERM_AUTO; // automatic choice for symmetric permutation
  param.ICNTL(8)=77; // automatic choice for scaling
  param.ICNTL(9)=1; // solve a x = b (not a^t x = b)
  param.ICNTL(10)=0; // no iterative refinement
  param.ICNTL(11)=0; // no error analysis
  param.ICNTL(12)=0; // not used (as sym != 2)
  param.ICNTL(13)=0; // parallel factorization of the root node
  param.ICNTL(14)=50; // 50% working space increase during numeric phase
  // 15-17 are not used
  param.ICNTL(19)=0; // compleate factorization (no schur complement)
  param.ICNTL(20)=0; // dense rhs
  param.ICNTL(21)=0; // the solution is centralized (i.e. not distributed)
  param.ICNTL(22)=0; // in-core factorization
  param.ICNTL(23)=0; // decide him self for working space
  param.ICNTL(24)=0; // null pivot row will give an error
  param.ICNTL(25)=0; // normal solution step 
  param.ICNTL(26)=0; // not used as (19) == 0
  param.ICNTL(27)=-8; // automatic bloc size for mrhs
  param.ICNTL(28)=0; // automatic choice seq or parallel ordering
  param.ICNTL(29)=0; // automatic choice for tool of parallel ordering
  param.ICNTL(30)=0; // no selected entries of a^-1 are calculated
  param.ICNTL(31)=0; // matrix factors are kept (not discarded)
  param.ICNTL(32)=0; // standart factorization (without forward elim of rhs)
  param.ICNTL(33)=1; // compute the detreminant
  // out-of-core files (only if the user sets ICNTL(22)=1) go to the session
  // temporary directory, never to the current one
  std::string tmpdir=as<std::string>(Function("tempdir")());
  if (tmpdir.size() < sizeof param.ooc_tmpdir)
    snprintf(param.ooc_tmpdir, sizeof param.ooc_tmpdir, "%s", tmpdir.c_str());
}
//RCPP_EXPOSED_CLASS(Rmumps)
RCPP_MODULE(mod_Rmumps){
  using namespace Rcpp ;
  class_<Rmumps>("Rmumps")
  // expose the default constructor
  .constructor<SEXP>()
  .constructor<SEXP, int>()
  .constructor<SEXP, int, bool>()
  .constructor<IntegerVector, IntegerVector, NumericVector, int>()
  .constructor<IntegerVector, IntegerVector, NumericVector, int, int>()
  .constructor<IntegerVector, IntegerVector, NumericVector, int, int, bool>()
  //.finalizer(&cleanme) // crashes by double freeing of the same pointer
  
  .property("rhs", &Rmumps::get_rhs, &Rmumps::set_rhs)
  .property("mrhs", &Rmumps::get_mrhs, &Rmumps::set_mrhs)
  .field("copy", &Rmumps::copy, "copy or not input parameters")
  .field_readonly("sym", &Rmumps::sym)
  .field("ncore", &Rmumps::ncore, "how many cores to use within OpenMP regions")
  
  .method("symbolic", &Rmumps::symbolic , "Analyze sparsity pattern")
  .method("numeric", &Rmumps::numeric, "Factorize sparse matrix")
  .method("solve", &Rmumps::solve, "Solve sparse system with one or many, sparse or dense rhs")
  .method("solvet", &Rmumps::solvet, "Solve transpose of sparse system with one or many, sparse or dense rhs")
  .method("inv", &Rmumps::inv, "Calculate the inverse of a sparse matrix")
  .method("set_mat_data", &Rmumps::set_mat_data, "Update matrix entries keeping the non zero pattern untouched")
  .method("set_permutation", &Rmumps::set_permutation, "Set permutation method for matrix permutation")
  .method("get_permutation", &Rmumps::get_permutation, "Get permutation method for matrix permutation")
  .method("set_icntl", &Rmumps::set_icntl, "Set ICNTL parameter vector")
  .method("get_icntl", &Rmumps::get_icntl, "Get ICNTL parameter vector")
  .method("set_cntl", &Rmumps::set_cntl, "Set CNTL parameter vector")
  .method("get_cntl", &Rmumps::get_cntl, "Get CNTL parameter vector")
  .method("get_infos", &Rmumps::get_infos, "Get a named list of information vectors")
  .method("set_keep", &Rmumps::set_keep, "Set KEEP parameter vector")
  .method("get_keep", &Rmumps::get_keep, "Get a copy of KEEP parameter vector (length=500)")
  .method("dim", &Rmumps::dim, "Return a vector with matrix dimensions")
  .method("nrow", &Rmumps::nrow, "Return an integer with matrix row number")
  .method("ncol", &Rmumps::ncol, "Return an integer with matrix column number")
  .method("print", &Rmumps::print, "Print the size of matrix and decompositions done")
  .method("show", &Rmumps::print, "Print the size of matrix and decompositions done")
  .method("triplet", &Rmumps::triplet, "Return an object of simple_triplet_matrix class with i, j, v fields representing the matrix")
  .method("det", &Rmumps::det, "Return determinant of the matrix")
  .method("log2det", &Rmumps::log2det, "Return log2(determinant) of the matrix (i.e. log base 2)")
  .method("mumps_version", &Rmumps::mumps_version, "Return used mumps version")
  .method("set_perm_in", &Rmumps::set_perm_in, "Set permutation vector defined by user")
  .method("get_sym_perm", &Rmumps::get_sym_perm, "Get symmetric permutation used by MUMPS")
  .method("get_uns_perm", &Rmumps::get_uns_perm, "Get unsymmetrical permutation used by MUMPS")
  ;
}

// wrappers to be exposed via rmumps namespace (ccallable behind the stage)
// [[Rcpp::interfaces(r, cpp)]]
//' Solve via Pointer
//'
//' This is a C wrapper to \code{Rmumps::solveptr()} method. Available in R too.
//' In C++ code can be used as \code{rmumps::Rmumps__solveptr(pobj, pb, lrhs, nrhs)}
//'
//' @param pobj pointer of type XPtr<Rmumps>, object having sparse matrix
//' @param pb pointer of type XPtr<double>, vector or dense matrix of rhs
//' @param lrhs integer, leading dimension in pb
//' @param nrhs integer, number of rhs to solve.
//' @export
// [[Rcpp::export]]
void Rmumps__solveptr(XPtr<Rmumps> pobj, XPtr<double> pb, int lrhs, int nrhs) {
  XPtr<Rmumps> p(pobj);
  XPtr<double> b(pb);
  p->solveptr(b, lrhs, nrhs);
}
//' Construct via Triplet Pointers
//'
//' This is a C wrapper to \code{Rmumps::Rmumps(i, j, v, n, nz, sym)} constructor. Available in R too.
//' In C++ code can be used as \code{rmumps::Rmumps__ptr_ijv(pi, pj, pa, n, nz, sym)}
//'
//' @param pi pointer of type XPtr<int>, vector of i-indeces for sparse triplet
//' @param pj pointer of type XPtr<int>, vector of j-indeces for sparse triplet
//' @param pa pointer of type XPtr<double>, vector or values for sparse triplet
//' @param n integer, size of the matrix (n x n)
//' @param nz integer, number of non zeros in the matrix
//' @param sym integer, 0 means general (non symmetric) matrix, 1 - symmetric with pivotes on the main diagonal, 2 - general symmetric (pivotes may be anywhere)
//' @return pointer of type XPtr<Rmumps> pointing to newly created object. To avoid memory leakage, it is user's responsibility to call \code{Rmumps__del_ptr(pm)} in a due moment (where \code{pm} is the returned pointer).
//' @export
// [[Rcpp::export]]
XPtr<Rmumps> Rmumps__ptr_ijv(XPtr<int> pi, XPtr<int> pj, XPtr<double> pa, int n, int nz, int sym) {
  XPtr<int> i(pi);
  XPtr<int> j(pj);
  XPtr<double> a(pa);
  XPtr<Rmumps> p(new Rmumps((MUMPS_INT *)i, (MUMPS_INT *)j, (double *)a, (MUMPS_INT) n, (MUMPS_INT) nz, (MUMPS_INT) sym), false);
  return(p);
}
//' Delete via Pointer
//'
//' This is a C wrapper to \code{Rmumps::~Rmumps()} destructor. Available in R too.
//' In C++ code can be used as \code{rmumps::Rmumps__del_ptr(pm)}
//'
//' @param pm pointer of type XPtr<Rmumps>, object to be deleted
//' @export
// [[Rcpp::export]]
void Rmumps__del_ptr(XPtr<Rmumps> pm) {
  Rmumps *p=XPtr<Rmumps>(pm);
  delete p;
}
//' Explore via Triplet
//'
//' This is a C wrapper to \code{Rmumps::triplet()} method. Available in R too.
//' In C++ code can be used as \code{rmumps::Rmumps__triplet(pm)}
//'
//' @param pm pointer of type XPtr<Rmumps>, object having sparse matrix to be explored
//' @return a list with sparse triplet described with fields i, j, v
//' @export
// [[Rcpp::export]]
List Rmumps__triplet(XPtr<Rmumps> pm) {
  XPtr<Rmumps> p(pm);
  return(p->triplet());
}
//' Set Matrix via Pointer
//'
//' This is a C wrapper to \code{Rmumps::set_mat_ptr(a)} method. Available in R too.
//' In C++ code can be used as \code{rmumps::Rmumps__set_mat_ptr(pm)}. Using this method invalidates previous numeric decomposition (but not symbolic one).
//'
//' @param pm pointer of type XPtr<Rmumps>, object having sparse matrix to be replaced with second parameter
//' @param pa pointer of type XPtr<double>, value vector from sparse triplet providing a new matrix. Structure of the new matrix must be identical to the old one. That's why there is no need to provide i and j for the new triplet.
//' @export
// [[Rcpp::export]]
void Rmumps__set_mat_ptr(XPtr<Rmumps> pm, XPtr<double> pa) {
  XPtr<Rmumps> p(pm);
  XPtr<double> a(pa);
  p->set_mat_ptr(a);
}
//' Set Permutation Parameter
//'
//' This is a C wrapper to \code{Rmumps::set_permutation(permutation)} method. Available in R too.
//' In C++ code can be used as \code{rmumps::Rmumps__set_permutation(pm, permutation)}
//'
//' @param pm pointer of type XPtr<Rmumps>, object having sparse matrix permuted according to a chosen method.
//' @param permutation integer one of predefined constants (cf. \code{\link{RMUMPS_PERM}}). Setting a new permutation invalidates current symbolic and numeric matrix decompositions.
//' @export
// [[Rcpp::export]]
void Rmumps__set_permutation(XPtr<Rmumps> pm, int permutation) {
  XPtr<Rmumps> p(pm);
  p->set_permutation(permutation);
}
//' Get Permutation Parameter
//'
//' This is a C wrapper to \code{Rmumps::get_permutation()} method. Available in R too.
//' In C++ code can be used as \code{rmumps::Rmumps__get_permutation(pm)}
//'
//' @param pm pointer of type XPtr<Rmumps>, object having sparse matrix permuted according to some method.
//' @return integer defining permutation method used before matrix decomposition.
//' @export
// [[Rcpp::export]]
int Rmumps__get_permutation(XPtr<Rmumps> pm) {
  XPtr<Rmumps> p(pm);
  return(p->get_permutation());
}
//' Pointer Interface to Rmumps Methods
//'
//' C wrappers to methods of the \code{Rmumps} class, for objects held as
//' pointers (e.g. created with \code{\link{Rmumps__ptr_ijv}} or obtained with
//' \code{Rmumps__xptr}). Available in R too. In C++ code they can be used as
//' \code{rmumps::Rmumps__symbolic(pm)} etc. (\code{LinkingTo: rmumps, Rcpp}).
//'
//' @param pm pointer of type XPtr<Rmumps>.
//' @param obj an object created with \code{Rmumps$new()}.
//' @param pb pointer of type XPtr<double>, vector or dense matrix of rhs,
//'   overwritten with the solution.
//' @param lrhs integer, leading dimension in pb.
//' @param nrhs integer, number of rhs to solve.
//' @param iv,ii integer vectors of values and 1-based positions.
//' @param v numeric vector of values.
//' @param p_in integer vector, a permutation of \code{1:n}.
//' @return \code{Rmumps__xptr}: a pointer of type XPtr<Rmumps> to the object
//'   inside \code{obj}, without a finalizer (\code{obj} keeps owning it and
//'   must stay alive). \code{Rmumps__get_icntl}, \code{Rmumps__get_cntl},
//'   \code{Rmumps__get_infos}, \code{Rmumps__det}, \code{Rmumps__log2det},
//'   \code{Rmumps__get_sym_perm}, \code{Rmumps__get_uns_perm},
//'   \code{Rmumps__mumps_version}: the same values as the corresponding
//'   methods of \code{Rmumps}. The other functions are called for their side
//'   effects and return \code{NULL} invisibly.
//' @name Rmumps__pointer_api
//' @examples
//' a <- Rmumps$new(c(0L, 1L), c(0L, 1L), c(2, 4), 2L)
//' pm <- Rmumps__xptr(a)
//' Rmumps__numeric(pm)
//' Rmumps__det(pm)
// [[Rcpp::export]]
XPtr<Rmumps> Rmumps__xptr(RObject obj) {
  if (!obj.isS4() || !obj.inherits("Rcpp_Rmumps"))
    stop("Rmumps__xptr: 'obj' must be an object created with Rmumps$new()");
  Environment env(as<S4>(obj).slot(".xData"));
  SEXP p=env.get(".pointer");
  if (TYPEOF(p) != EXTPTRSXP || R_ExternalPtrAddr(p) == NULL)
    stop("Rmumps__xptr: 'obj' holds no valid pointer (was it saved and reloaded?)");
  return XPtr<Rmumps>((Rmumps *) R_ExternalPtrAddr(p), false, R_NilValue, obj);
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
void Rmumps__symbolic(XPtr<Rmumps> pm) {
  pm->symbolic();
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
void Rmumps__numeric(XPtr<Rmumps> pm) {
  pm->numeric();
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
void Rmumps__solvetptr(XPtr<Rmumps> pm, XPtr<double> pb, int lrhs, int nrhs) {
  pm->solvetptr(pb, lrhs, nrhs);
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
void Rmumps__set_icntl(XPtr<Rmumps> pm, IntegerVector iv, IntegerVector ii) {
  pm->set_icntl(iv, ii);
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
IntegerVector Rmumps__get_icntl(XPtr<Rmumps> pm) {
  return pm->get_icntl();
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
void Rmumps__set_cntl(XPtr<Rmumps> pm, NumericVector v, IntegerVector iv) {
  pm->set_cntl(v, iv);
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
NumericVector Rmumps__get_cntl(XPtr<Rmumps> pm) {
  return pm->get_cntl();
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
List Rmumps__get_infos(XPtr<Rmumps> pm) {
  return pm->get_infos();
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
double Rmumps__det(XPtr<Rmumps> pm) {
  return pm->det();
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
double Rmumps__log2det(XPtr<Rmumps> pm) {
  return pm->log2det();
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
IntegerVector Rmumps__get_sym_perm(XPtr<Rmumps> pm) {
  return pm->get_sym_perm();
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
IntegerVector Rmumps__get_uns_perm(XPtr<Rmumps> pm) {
  return pm->get_uns_perm();
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
void Rmumps__set_perm_in(XPtr<Rmumps> pm, IntegerVector p_in) {
  pm->set_perm_in(p_in);
}
//' @rdname Rmumps__pointer_api
// [[Rcpp::export]]
std::string Rmumps__mumps_version(XPtr<Rmumps> pm) {
  return pm->mumps_version();
}

// Called from the R_init_rmumps() generated by Rcpp::compileAttributes(),
// after the .Call entry points are registered.
// [[Rcpp::init]]
void rmumps_init(DllInfo *dll) {
  rmumps_register_c_callables();
}
