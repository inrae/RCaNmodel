#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Cholesky>
#include <Eigen/Sparse>
#include <limits>
#include <dqrng.h>
#include <dqrng_RcppExports.h>
#include <Eigen/Eigenvalues>
#include <Eigen/LU>
#include <testthat.h>
#include "sampleCaNCPP.h"


// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(dqrng)]]
using namespace Rcpp;
using namespace RcppEigen;
using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::VectorXd;

//' @export
//' @useDynLib RCaNmodel





void updateS(Eigen::MatrixXd &S, Eigen::MatrixXd &S2, Eigen::MatrixXd &S0, Eigen::VectorXd &M, Eigen::VectorXd &delta0, const Eigen::VectorXd &x, int iter){
  delta0 = x - M; // delta new point wrt old mean
  M+= delta0/(double)iter;     // sample mean
  if (iter>1){
    if (iter == 2) {
      S2 += x*x.transpose();
    } else {
      Eigen::VectorXd xstd=x/sqrt(iter-2.);
      S2 += xstd*xstd.transpose();
      S2 *= (iter-2.)/(iter - 1.);
      S=S2- iter*(M*M.transpose())/(iter-1.);
    }
    //S.diagonal() = S.diagonal().cwiseMax(0.0001 * M.cwiseProduct(M)); //this ensures a minimum CV of 1%
  } else {
    S2=x*x.transpose();
  }
  
}


IntegerVector order_(const NumericVector & x) {
  NumericVector sorted = clone(x).sort();
  return match(sorted, x);
}





// [[Rcpp::interfaces(r,cpp)]]

//' Complex Polytope Gibbs Sampling
//' This function draw uniform samples in a convex polytope with inequality constraints
//'
//' @param N the number of samples to generate
//' @param A a matrix
//' @param b a vector of length equals to nrow(A)
//' @param x0 a vector of length equals to nrcol(A) that should be in the polytope, for example returned by \code{\link{chebyCentre}}
//' @param thin thinning interval
//' @param method (1 gibbs, 2 hit-and-run, 3 chrr) 
//' @param seed seed of the dqrng generator
//' @param stream stream of the dqrng generator
//' @param covMat prespecified covmatrix (avoid initialisation and discard) if
//' prespecified (default null)
//'
//' @section Details:
//' This function is based on an initial matlab code developped called CPRND
//' (https://ch.mathworks.com/matlabcentral/fileexchange/34208-uniform-distribution-over-a-convex-polytope)
//' It generates samples within the complex polytope defined by \eqn{A \cdot x \leqslant   b}
//' The CHRR algorithm is a C++ translation of cobratoolbox code written
//' by Yin Zhang licensed under GNU GPL-3 https://github.com/opencobra/cobratoolbox/ 
//' and of matlab code written by Ben Cousins 
//' (https://github.com/Bounciness/Volume-and-Sampling) is a C++ translation of
//' matlab functions provided in the opencobra toolbox and 
//' in this github repository
//'
//' @return a matrix with one row per sample and one column per parameter
//' @examples
//' n <- 20
//' A1 <- -diag(n)
//' b1 <- as.matrix(rep(0,n))
//' A2 <- diag(n)
//' b2 <- as.matrix(rep(1,n))
//' A <- rbind(A1,A2)
//' b <- rbind(b1,b2)
//' X0 <- chebyCentre(A,b)
//' x <- cpgs(1000,A,b,X0)
//' @export
// [[Rcpp::export]]


List cpgs(const int N, const Eigen::MatrixXd &A ,
          const Eigen::VectorXd &b,
          const Eigen::VectorXd &x0,
          const int thin=1,
          const int method=1,
          const int seed=1,
          const int stream=1,
          Rcpp::Nullable<Eigen::MatrixXd> covMat = R_NilValue,
          Rcpp::Nullable<Eigen::MatrixXd> savedN_total = R_NilValue,
          Rcpp::Nullable<Eigen::VectorXd> savedp_shift = R_NilValue) {
  dqrng::dqRNGkind("Xoroshiro128+");
  dqrng::dqset_seed(IntegerVector::create(seed),
                    IntegerVector::create(stream));
  int p=A.cols();
  int m=A.rows();
  double inf = std::numeric_limits<double>::max();
  
  // Check input arguments
  if (m < (p+1) || b.size()!=m || x0.size()!=p){
    throw std::range_error("dimensions mismatch");
  }
  Eigen::VectorXd b2(b.size());
  b2=b;
  // Initialisation
  Eigen::MatrixXd X(N,p);
  Eigen::VectorXd x(p);
  Eigen::VectorXd y(p);
  x=x0;
  
  // Initialize variables for keeping track of sample mean, covariance
  // and isotropic transform.
  Eigen::VectorXd M(p);
  M.setZero();
  Eigen::MatrixXd S2(p,p);
  S2.setZero();
  
  // outer products.
  Eigen::MatrixXd S(p,p);
  Eigen::MatrixXd S0(p,p);
  S.setIdentity();
  
  IntegerVector index=Rcpp::seq(0,p-1);
  NumericVector u(p);
  
  Eigen::MatrixXd T1(p,p);
  Eigen::MatrixXd T2(p,p);
  T1.setIdentity();
  Eigen::MatrixXd W(m,p);
  Eigen::VectorXd delta(m); //vector used to store distance to bounds
  
  W = A;
  bool adapt=true;
  bool updatingS=true;
  Eigen::VectorXd d(m);
  Eigen::VectorXd d2(m);
  Eigen::VectorXd delta0(p);
  Eigen::VectorXd delta1(p);
  Eigen::VectorXd z(m);
  Eigen::MatrixXd L(p,p);
  Eigen::MatrixXd D(p,p);
  Eigen::VectorXd Dtmp(p);
  Eigen::VectorXd Dzero=VectorXd::Constant(p,1.0e-16);
  Eigen::LDLT<MatrixXd> ldltOfS(S.cols());
  int runup=0; //number of adapt
  int discard=0; //number of discard
  int isample=0; //number of sample
  int n=0; //total number of iterations
  int stage=0; //0 adapting phase, 1 discarding phase, 2 sampling
  if (covMat.isNotNull()) {
    stage = 2;
    T1 = Rcpp::as< Map<MatrixXd> >(covMat);
    T2=T1.inverse();
    updatingS = false;
    Rcout<<"using provided S"<<std::endl;
  }
  if (method == 3) {
    stage = 2;
    updatingS=false;
  }
  int runupmax= p*log2(p); //https://doi.org/10.1021/acs.jproteome.5b01029
  int sampleit=0; //total number of iteration during sampling, useful for thin
  int discardmax=runupmax;
  MatrixXd T = MatrixXd::Identity(m, m);
  MatrixXd N_total = MatrixXd::Identity(m, m);
  MatrixXd N_total_inv(m, m);
  VectorXd p_shift=VectorXd::Zero(m);
  if (savedp_shift.isNotNull() && savedN_total.isNotNull()){
    Rcout<<"## using saved rounded polytope"<<std::endl;
    p_shift = Rcpp::as< Map<VectorXd> >(savedp_shift);
    N_total = Rcpp::as< Map<MatrixXd> >(savedN_total);
    W = A * N;
    b2 = b - A * p_shift;
  } else if (method == 3 ) {//chrr
    Rcout<<"## rounding polytope"<<std::endl;
    round(W, b2, x, N_total, p_shift, T, 80);
    Rcout<<"## check "<<(W*x-b2).maxCoeff()<<std::endl;
    Rcout<<"## check "<<W.maxCoeff()<<" "<<W.minCoeff()<<std::endl;
    Rcout<<"## check "<<b2.maxCoeff()<<" "<<b2.minCoeff()<<std::endl;
    Rcout<<"## check "<<N_total.maxCoeff()<<" "<<N_total.minCoeff()<<std::endl;
    Rcout<<"## check "<<p_shift.maxCoeff()<<" "<<p_shift.minCoeff()<<std::endl;
    
  }

  while (isample<N){               //sampling loop
    //std::random_shuffle(index.begin(), index.end()); //we change the order to
    //limit the influence of initial ordering
    
    if (method <= 2){
      y=x;
    }
    // compute approximate stochastic transformation
    if (((stage==1 && discard==0) || (stage>0 && updatingS==true)) && method <3){ //first true
      //sample, we now make the isotropic transformation
      ldltOfS.compute(S.transpose());
      D=ldltOfS.vectorD().cwiseMax(Dzero).asDiagonal();
      L=ldltOfS.matrixL();
      T1=ldltOfS.transpositionsP().transpose()*L*D.sqrt();
      T2=T1.inverse();
      W = A*T1;
    }
    if (stage>0 && method <=2) y=T2*y; //otherwise y=I^-1 * y=y
    if (method < 2 || stage == 0){
      index=dqrng::dqsample_int(p,p,false);
      NumericVector alea2=dqrng::dqrunif(p);
      // choose p new components
      for (int ip=0;ip<p;++ip){
        int i=index[p-ip-1];
        //Find points where the line with the (p-1) components x_i
        //fixed intersects the bounding polytope.
        z = W.col(i); //prevent any divisions by 0
        if (ip==0)
          d2=(b2 - W*y);
        d=d2.cwiseQuotient(z);
        double tmin=-inf;
        double tmax=inf;
        for (int j=0;j<m;++j){
          if (z(j)<0 && tmin<d(j)) tmin=d(j);
          if (z(j)>0 && tmax>d(j)) tmax=d(j);
        }
        tmin=std::min(0.0, tmin);
        tmax=std::max(0.0, tmax);
        
        
        double delta = -y(i);
        y(i) += (tmin+(tmax-tmin)*alea2(i));
        y(i)=std::min(std::max(y(i),-inf),inf);
        //Rcout<<tmin<<" "<<tmax<<" "<<y(i)<<std::endl;
        delta += y(i);
        d2 =d2- W.col(i)*delta; //we do this to avoid making a matrix
        //multiplication for each parameter (we just update the value of the
        //constraint with the delta of parameter)
      }
    } //1st stage or hit and run or gibbs
    else if (method >= 2) { //hit and run
      Eigen::Map<Eigen::VectorXd> u(Rcpp::as<Eigen::Map<Eigen::VectorXd> >(dqrng::dqrnorm(p)));
      u /= u.norm();
      z = W * u;
      d2 = (b2 - W*y);
      d = d2.cwiseQuotient(z);
      double tmin=-inf;
      double tmax=inf;
      for (int j=0;j<m;++j){
        if (z(j)<0 && tmin<d(j)) tmin=d(j);
        if (z(j)>0 && tmax>d(j)) tmax=d(j);
      }
      tmin=std::min(0.0, tmin);
      tmax=std::max(0.0, tmax);
      
      
      y += (tmin+(tmax-tmin)*dqrng::dqrunif(1)[0])*u;
      
      //y =std::min(std::max(y(i),-inf),inf);
    } //hit and run or chrr
    if (method <=2){
      x=T1*y;
    } else{
      x = N_total * y + p_shift;
    }
    
    if (stage==0){//still in adapting phase
      ++runup;
      if (updatingS) updateS(S, S2, S0, M, delta0, x, runup);
      if (runup==runupmax){
        updatingS=false;
        stage=1;
        Rcout<<"########adapation ended after "<<runup<<" iterations"<<std::endl;
      }
    } else if (stage==1){ //we are in discarding phase
      ++discard;
      if (discard==discardmax){
        stage=2;
        Rcout<<"#######end of discarding phase"<<std::endl;
      }
    } else{ //we are in sampling phase
      if ((sampleit % thin) == 0){
        X.row(isample)=x.col(0);
        ++isample;
      }
      ++sampleit;
    }
    if (n % 100 == 0) Rcout<<"##iteration "<<n<<" stage "<<stage<<std::endl;
    ++n;
  }
  if (method <= 2){
    return (List::create(Named("X") = X, 
                         Named("covMat") = S,
                         Named("p_shift") =  savedp_shift,
                         Named("N_total") = savedN_total));
  } else {
    return (List::create(Named("X") = X, 
                         Named("covMat") = covMat,
                         Named("p_shift") = p_shift,
                         Named("N_total") = N_total,
                         Named("W") = W,
                         Named("b2") = b2));
  }
}


using Eigen::FullPivLU;




//' Complex Polytope Gibbs Sampling
//' This function draw uniform samples in a convex polytope with both equality and inequality constraints
//'
//' @param N the number of samples to generate
//' @param A a matrix of coefficients of inequality constants A.x<=b
//' @param b a vector of length equals to nrow(A)
//' @param C a matrix of coefficients of inequality constants C.x=v
//' @param v a vector of length equals to nrow(C)
//' @param x0 a vector of length equals to ncol(A) that should be in the polytope, for example returned by \code{\link{chebyCentre}}
//' @param thin the thinning interval
//' @param method (1 gibbs sampling, 2 hitandrun, 3 chrr)
//' @param seed seed of the dqrng generator
//' @param stream stream of the dqrng generator
//' @param covMat prespecified covmatrix (avoid initialisation and discard) if
//' prespecified (default null)
//'
//' @section Details:
//' This function is based on an initial matlab code developped called CPRND
//' (https://ch.mathworks.com/matlabcentral/fileexchange/34208-uniform-distribution-over-a-convex-polytope)
//' It generates samples within the complex polytope defined by \eqn{A \cdot x \leqslant   b}
//'
//' @return a list with two elements: a matrix with one row per sample and one
//'  column per parameter and a list with the covariance matrix used in the
//'  algorithm that can be used to resample the model
//' @examples
//' n <- 20
//' A1 <- -diag(n)
//' b1 <- as.matrix(rep(0,n))
//' A2 <- diag(n)
//' b2 <- as.matrix(rep(1,n))
//' A <- rbind(A1,A2)
//' b <- rbind(b1,b2)
//' C <- rbind(c(1,1,rep(0,n-2)),c(0,0,1,1,rep(0,n-4)))
//' v <- matrix(rep(0.2,2),2)
//' X0 <- rep(0.1,n)
//' x <- cpgsEquality(1000,A,b,C,v,X0)
//' @export
// [[Rcpp::export]]


List cpgsEquality(const int N, const Eigen::MatrixXd &A,
                  const Eigen::VectorXd &b, const Eigen::MatrixXd &C,
                  const Eigen::VectorXd &v, const Eigen::VectorXd &x0,
                  const int thin=1, const int method=1,
                  const int seed=1, const int stream=1,
                  Rcpp::Nullable<Eigen::MatrixXd> covMat = R_NilValue){
  int p=A.cols();
  int m=A.rows();
  int p2=C.cols();
  int m2=C.rows();
  
  MatrixXd X(N,p);
  
  // Check input arguments
  if (m < (p+1) || b.size()!=m || x0.size()!=p){
    throw std::range_error("dimensions mismatch");
  }
  if (v.size()!=m2 || x0.size()!=p2){
    throw std::range_error("dimensions mismatch");
  }
  // Initialisation
  FullPivLU<MatrixXd> lu(C);
  MatrixXd Nt = lu.kernel();
  MatrixXd Abis=A*Nt;
  VectorXd bbis=b-A*x0;
  
  VectorXd x0bis=VectorXd::Zero(Nt.cols());
  List x=cpgs(N, Abis, bbis, x0bis, thin, method, seed, stream, covMat);
  Eigen::MatrixXd xtmp(Rcpp::as<Eigen::MatrixXd>(x["X"]));
  for(int i=0;i<N;++i) {
    X.row(i)=Nt*(xtmp.row(i).transpose())+x0;
  }
  return (List::create(Named("X") = X, Named("covMat") = x["covMat"]));
}







//' degenerateSubSpace
//' This function reduces the space defined by inequality and unequality 
//' constraints
//'
//' @param A a matrix of coefficients of inequality constants A.x<=b
//' @param b a vector of length equals to nrow(A)
//' @param C a matrix of coefficients of inequality constants C.x=v
//' @param v a vector of length equals to nrow(C)
//' @param x0 a vector of length equals to ncol(A) that should be in the polytope, for example returned by \code{\link{chebyCentre}}
//'
//' @return a list with elements A2 and b2 defining the subscape A2 x <= b2 and
//' Nt that can be used to convert results in appropriate format
//' 
//' @examples
//' n <- 5
//' A1 <- -diag(n)
//' b1 <- as.matrix(rep(0,n))
//' A2 <- diag(n)
//' b2 <- as.matrix(rep(1,n))
//' A <- rbind(A1,A2)
//' b <- rbind(b1,b2)
//' C <- rbind(c(1,1,rep(0,n-2)),c(0,0,1,1,rep(0,n-4)))
//' v <- matrix(rep(0.2,2),2)
//' X0 <- rep(0.1,n)
//' x <- degenerateSubSpace(A,b,C,v,X0)
//' @export
// [[Rcpp::export]]


List degenerateSubSpace(const Eigen::MatrixXd &A,
                  const Eigen::VectorXd &b, const Eigen::MatrixXd &C,
                  const Eigen::VectorXd &v, const Eigen::VectorXd &z){
  int p=A.cols();
  int m=A.rows();
  int p2=C.cols();
  int m2=C.rows();
  
  // Check input arguments
  if (m < (p+1) || b.size()!=m || z.size()!=p){
    throw std::range_error("dimensions mismatch");
  }
  if (v.size()!=m2 || z.size()!=p2){
    throw std::range_error("dimensions mismatch");
  }
  // Initialisation
  FullPivLU<MatrixXd> lu(C);
  MatrixXd Nt = lu.kernel();
  MatrixXd Abis=A*Nt;
  VectorXd bbis=b-A*z;

  return (List::create(Named("A2") = Abis, 
                       Named("b2") = bbis,
                       Named("Nt") = Nt));
}





// [[Rcpp::export]]
List sampleCaNCPP(const int N, 
                  const Eigen::MatrixXd &A ,
                  const Eigen::VectorXd &b,
                  const Eigen::MatrixXd &C ,const Eigen::VectorXd &v,
                  const Eigen::MatrixXd &L,
                  const Eigen::VectorXd &x0, 
                  const int thin, 
                  const int method=1,
                  const int seed=1,
                  const int stream=1,
                  Rcpp::Nullable<Eigen::MatrixXd> covMat = R_NilValue,
                  Rcpp::Nullable<Eigen::MatrixXd> savedN_total = R_NilValue,
                  Rcpp::Nullable<Eigen::VectorXd> savedp_shift = R_NilValue) {
  int p=A.cols();
  int m2=C.rows();
  
  List x;
  MatrixXd B(N, L.rows());
  if(m2>0){ //there are equality constraints
    x=cpgsEquality(N, 
                   A, 
                   b,
                   C,
                   v, 
                   x0, 
                   thin,
                   method, 
                   seed,
                   stream, 
                   covMat);
  } else{
    x=cpgs(N,
           A,
           b,
           x0,
           thin,
           method,
           seed,
           stream,
           covMat, 
           savedN_total,
           savedp_shift);
  }
  Eigen::MatrixXd F(Rcpp::as<Eigen::MatrixXd>(x["X"]));
  for (int i=0;i<N;++i){
    B.row(i)=L*F.row(i).transpose();
  }
  return(List::create(F,B, x["covMat"], x["p_shift"], x["N_total"]));
}




//' gmscale
//' scaling of matrix with a geometric mean procedure
//'MatrixXd A, VectorXd &cscale, VectorXd &rscale, double scltol
//' @param A a matrix of coefficients of inequality constants A.x<=b
//' @param b a vector of length equals to nrow(A)
//' @param cscale the column scaling vector
//' @param rscale the row scaling vector
//' @param scltol tolerance
//'
//' @section Details:
//' The CHRR algorithm is a C++ translation of cobratoolbox code written
//' by Yin Zhang licensed under GNU GPL-3 https://github.com/opencobra/cobratoolbox/ 
//' and of matlab code written by Ben Cousins 
//' (https://github.com/Bounciness/Volume-and-Sampling) is a C++ translation of
//' matlab functions provided in the opencobra toolbox and 
//' in this github repository % `m x n` sparse matrix `A`.
//' 
//' An iterative procedure based on geometric means is used,
//' following a routine written by Robert Fourer, 1979.
//' Several passes are made through the columns and rows of `A`.
//' The main steps are:
//'
//'   1. Compute :math:`aratio = max_j (max_i Aij / min_i Aij)`.
//'   2. Divide each row `i` by :math:`sqrt( max_j Aij * min_j Aij)`.
//'   3. Divide each column `j` by :math:`sqrt( max_i Aij * min_i Aij)`.
//'   4. Compute `sratio` as in Step 1.
//'   5. If :math:`sratio < aratio * scltol`,
//'      set :math:`aratio = sratio` and repeat from Step 2.
//'
//' To dampen the effect of very small elements, on each pass,
//' a new row or column scale will not be smaller than sqrt(damp)
//' times the largest (scaled) element in that row or column.
//'
//' Use of the scales:
//' To apply the scales to a linear program,
//' :math:`min c^T x` st :math:`A x = b`, :math:`l \leq x \leq u`,
//' we need to define "barred" quantities by the following relations:
//' `A = R Abar C`, `b = R bbar`, `C cbar = c`,
//' `C l = lbar`, `C u = ubar`, `C x = xbar`.
//'
//' This gives the scaled problem
//' :math:`min\ cbar^T xbar` st :math:`Abar\ xbar = bbar`, :math:`lbar \leq xbar \leq ubar`.
//'
//' .. Author: - Michael Saunders, Systems Optimization Laboratory, Stanford University.
//' ..
//'    07 Jun 1996: First f77 version, based on MINOS 5.5 routine m2scal.
//'    24 Apr 1998: Added final pass to make column norms = 1.
//'    18 Nov 1999: Fixed up documentation.
//'    26 Mar 2006: (Leo Tenenblat) First Matlab version based on Fortran version.
//'    21 Mar 2008: (MAS) Inner loops j = 1:n optimized.
//'    09 Apr 2008: (MAS) All loops replaced by sparse-matrix operations.
//'                 We can't find the biggest and smallest Aij
//'                 on each scaling pass, so no longer print them.
//'    24 Apr 2008: (MAS, Kaustuv) Allow for empty rows and columns.
//'    13 Nov 2009: gmscal.m renamed gmscale.m.
//' @export
// [[Rcpp::export]]



void gmscale(Eigen::MatrixXd A, 
             Eigen::VectorXd &cscale, 
             Eigen::VectorXd &rscale, 
             double scltol){


  int m = A.rows();
  int n = A.cols();
  
  A = A.array().abs().matrix();
  int maxpass=10;
  double aratio=1e50;
  double damp=1e-4;
  double small = 1e-8;
  double eps=2.2204e-16;
  Eigen::MatrixXd SA;
  rscale = Eigen::VectorXd::Constant(m, 1);
  cscale = Eigen::VectorXd::Constant(n, 1);
  
  /*--------------------------------------------------------------
   Main loop.
   --------------------------------------------------------------*/
    Eigen::MatrixXd invSA= Eigen::MatrixXd::Zero(m,n);
  for (int npass = 0; npass <= maxpass; ++ npass){
    // Find the largest column ratio.
    // Also set new column scales (except on pass 0).
    
    rscale = (rscale.array() ==0).select(1, rscale);
    Eigen::MatrixXd Rinv    = rscale.cwiseInverse().asDiagonal();
    SA      = Rinv*A;
    invSA= (SA.array()==0).select(1e9,SA.cwiseInverse());
    Eigen::VectorXd cmax    = SA.colwise().maxCoeff();   // column vector
    Eigen::VectorXd cmin    = invSA.colwise().maxCoeff();   // column vector
    cmin    = (cmin.array() + eps).matrix().cwiseInverse();
    double sratio  = cmax.cwiseProduct(cmin.cwiseInverse()).maxCoeff();   // Max col ratio
    if (npass > 0){
      cscale = cmax.cwiseProduct(cmin.cwiseMax(damp*cmax)).array().sqrt().matrix();
    }
    if (npass >= 2 && sratio >= aratio*scltol){
      break;
    }
    if (npass == maxpass) {
      break;
    }
    aratio  = sratio;
    // Set new row scales for the next pass.
    
    cscale = (cscale.array()==0).select(1,cscale);
    Eigen::DiagonalMatrix<double, Eigen::Dynamic> Cinv = cscale.cwiseInverse().asDiagonal();
    SA      = A*Cinv;                  // Scaled A
    invSA= (SA.array()==0).select(1e9,SA.cwiseInverse());
    Eigen::VectorXd rmax    = SA.rowwise().maxCoeff();   // column vector
    Eigen::VectorXd rmin    = invSA.rowwise().maxCoeff();   // column vector
    rmin    = (rmin.array() + eps).matrix().cwiseInverse();
    rscale  = rmax.cwiseProduct(rmin.cwiseMax(damp*rmax)).array().sqrt().matrix();
  }
  /*---------------------------------------------------------------
   End of main loop.
   ---------------------------------------------------------------*/
  
  // Reset column scales so the biggest element
  // in each scaled column will be 1.
  // Again, allow for empty rows and columns.
  
  rscale = (rscale.array() ==0).select(1, rscale);
  Eigen::MatrixXd Rinv    = rscale.cwiseInverse().asDiagonal();
  SA      = Rinv*A;
  
  cscale  = SA.colwise().maxCoeff();   // column vector
  cscale = (cscale.array()==0).select(1,cscale);
  
  // end of gmscale
}




//' mve_solver
//' Find the maximum volume ellipsoid
//' @param A a matrix of coefficients of inequality constants A.x<=b
//' @param b a vector of length equals to nrow(A)
//' @param x0 a solution of the polytope the column scaling vector
//' @param reg a tuning parameter
//' @param x vector to store the center of the ellipse
//' @param E2 matrix to store the ellipse E'E
//' @param maxiter a tuning parameter
//' @param tol a tuning parameter
//'
//' @section Details:
//' The CHRR algorithm is a C++ translation of cobratoolbox code written
//' by Yin Zhang licensed under GNU GPL-3 https://github.com/opencobra/cobratoolbox/ 
//' and of matlab code written by Ben Cousins 
//' (https://github.com/Bounciness/Volume-and-Sampling) is a C++ translation of
//' matlab functions provided in the opencobra toolbox and 
//' in this github repository % `m x n` sparse matrix `A`.
//' 
//'Find the maximum volume ellipsoid
//'    {v:  v = x + Es, ||s|| <= 1}
//'  inscribing a full-dimensional polytope
//'          {v:  Av <= b}
//'  Input:  A, b --- defining the polytope
//'          x0 --- interior point (Ax0 < b)
//'  Output: x --- center of the ellipsoid
//'          E2 --- E'*E
//'--------------------------------------
//' Yin Zhang, Rice University, 07/29/02
//'--------------------------------------
//'lines modified by me (Ben Cousins) have a %Ben after them
//' @export
// [[Rcpp::export]]


bool mve_solver(Eigen::MatrixXd A,
                Eigen::VectorXd b,
                const Eigen::VectorXd &x0,
                double reg,
                Eigen::VectorXd & x,
                Eigen::MatrixXd & E2,
                const int maxiter = 50,
                const double tol  = 1.e-6){
  bool converged = false;
  int m = A.rows();
  int n = A.cols();
  double minmu = 1.e-8;
  double tau0 = .75;
  double last_r1=-999999999999;
  double last_r2=-999999999999;
  double bnrm = A.norm();
  Rcout<<"##starts mve_solver"<<std::endl;
  
  Eigen::VectorXd bmAx0 = b - A*x0;
  Eigen::MatrixXd tmp=Eigen::MatrixXd::Zero(m,m);
  
  tmp.diagonal()=bmAx0.cwiseInverse();
  A=tmp * A;
  
  b=Eigen::VectorXd::Constant(m, 1);
  x =  Eigen::VectorXd::Zero(n);
  Eigen::VectorXd y = Eigen::VectorXd::Constant(m, 1);
  Eigen::DiagonalMatrix<double, Eigen::Dynamic> Y = Eigen::VectorXd::Zero(m).asDiagonal();
  Eigen::VectorXd bmAx = b;
  double res = 1;
  int msg = 1;
  
  double astep;
  double prev_obj = -99999999999;
  Eigen::VectorXd Adx(m);
  
  Eigen::VectorXd z(m);
  
  for (int iter =0; iter < maxiter; ++iter){
    Rcout<<"##iter "<<iter<<std::endl;
    
    if (iter > 0) {
      bmAx -= astep*Adx;
    }
    
    Y.diagonal()=y;
    E2 = (A.transpose() * Y* A).inverse();
    Eigen::MatrixXd Q = A * E2 * A.transpose();
    Eigen::VectorXd h=Q.diagonal().array().sqrt().matrix();
    if (iter == 0){
      double t = bmAx.cwiseProduct(h.cwiseInverse()).minCoeff();
      y /= t*t;
      h *= t;
      z = (bmAx-h).cwiseMax(1.e-1);
      Q *= t*t;
      Y.diagonal() = (Y.diagonal().array()/(t*t)).matrix();
    }
    
    Eigen::VectorXd yz = y.cwiseProduct(z);
    Eigen::VectorXd yh = y.cwiseProduct(h);
    double gap = yz.sum()/((double)m);
    double rmu = std::min(0.5, gap) * gap;
    rmu = std::max(rmu, minmu);
    Eigen::VectorXd R1 = - A.transpose()*yh;
    Eigen::VectorXd R2 = bmAx - h -z;
    Eigen::VectorXd R3 = - yz + Eigen::VectorXd::Constant(m,rmu);
    double r1 = R1.array().abs().sum();
    double r2 = R2.array().abs().sum();
    double r3 = R3.array().abs().sum();


    res = std::max(r1,std::max(r2, r3));
    double objval = log(E2.determinant())*.5;
    
    if (iter%10==0){
      Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es(E2);
      Eigen::VectorXd eigen=es.eigenvalues().real();
      if (abs((last_r1-r1)/std::min(abs(last_r1),abs(r1)))<1e-2 && abs((last_r2 - r2)/std::min(abs(last_r2),abs(r2)))<1e-2 && eigen.maxCoeff()/eigen.minCoeff() >100 && reg>1e-10){
        break;
      }

      last_r2 = r2;
      last_r1 = r1;
    }
    
    if ((res < tol * (1+bnrm) && rmu <= minmu) || (iter > 99 & prev_obj != -99999999999 && (prev_obj >= (1-tol) * objval || prev_obj <=(1-tol) * objval))) {
      x = x + x0;
      msg = 1;
      converged=true;
      break;
    }
    prev_obj = objval;
    Eigen::MatrixXd YQ=Y*Q;
    Eigen::MatrixXd YQQY = YQ.cwiseProduct(YQ.transpose());
    Eigen::VectorXd y2h= 2*yh;
    Eigen::MatrixXd YA=Y*A;
    
    
    tmp=Eigen::MatrixXd::Zero(m,m);
    tmp.diagonal()=y2h.cwiseProduct(z).cwiseMax(reg);
    
    Eigen::MatrixXd G = YQQY+ tmp ;
    
    Eigen::VectorXd rsG = Eigen::VectorXd::Constant(G.rows(), 1);
    Eigen::VectorXd csG = Eigen::VectorXd::Constant(G.cols(), 1);
    gmscale(G,csG, rsG, .99); //Ben
    Eigen::MatrixXd T = (rsG.cwiseInverse().asDiagonal()*G*csG.cwiseInverse().asDiagonal()).inverse() * (rsG.cwiseInverse().asDiagonal()*((h+z).asDiagonal()*YA)); //Ben
    T = csG.cwiseInverse().asDiagonal()*T; //Ben
    
    
    
    Eigen::MatrixXd ATP=(y2h.asDiagonal()*T-YA).transpose();
    Eigen::VectorXd R3Dy=R3.cwiseProduct(y.cwiseInverse());
    
    Eigen::VectorXd R23 = R2-R3Dy;
    Eigen::MatrixXd ATP_A = ATP*A;
    
    ATP_A+=Eigen::VectorXd::Constant(n,reg).asDiagonal();
    
    
    Eigen::VectorXd dx=ATP_A.inverse() * (R1+ATP*R23); //Ben
    
    //dx = csA.cwiseInverse().asDiagonal() * dx; //Ben
    Adx = A*dx;
    Eigen::VectorXd dyDy=(rsG.cwiseInverse().asDiagonal()*G*csG.cwiseInverse().asDiagonal()).inverse()*(rsG.cwiseInverse().asDiagonal()*y2h.cwiseProduct(Adx-R23));
    dyDy=csG.cwiseInverse().asDiagonal()*dyDy;
    Eigen::VectorXd dy = y.cwiseProduct(dyDy);
    Eigen::VectorXd dz=R3Dy - z.cwiseProduct(dyDy);
    double ax=-1/std::min(-(Adx.cwiseProduct(bmAx.cwiseInverse()).eval().maxCoeff()), -.5);
    double ay=-1/std::min(dyDy.minCoeff(), -.5);
    double az = -1/std::min(dz.cwiseProduct(z.cwiseInverse()).eval().minCoeff(), -.5);
    double tau = std::max(tau0, 1 -res);
    astep = tau*std::min(1.,std::min(ax,std::min(ay,az)));
    x += astep*dx;
    y += astep*dy;
    z += astep*dz;
    
    if (reg>1e-6 && iter > 10){
      break;
      
    }
  }
  Rcout<<"end mve solver converged "<<converged<<std::endl;
  return converged;
}










//' mve_run_cobra
//' Find the maximum volume ellipsoid
//' @param A a matrix of coefficients of inequality constants A.x<=b
//' @param b a vector of length equals to nrow(A)
//' @param x0 a solution of the polytope the column scaling vector
//' @param reg a tuning parameter
//' @param x vector to store the center of the ellipse
//' @param E matrix to store the ellipse
//' @param maxiter a tuning parameter
//'
//' @section Details:
//' The CHRR algorithm is a C++ translation of cobratoolbox code written
//' by Yin Zhang licensed under GNU GPL-3 https://github.com/opencobra/cobratoolbox/ 
//' and of matlab code written by Ben Cousins 
//' (https://github.com/Bounciness/Volume-and-Sampling) is a C++ translation of
//' matlab functions provided in the opencobra toolbox and 
//' in this github repository % `m x n` sparse matrix `A`.
//' 
//'Find the maximum volume ellipsoid
//'    {v:  v = x + Es, ||s|| <= 1}
//'  inscribing a full-dimensional polytope
//'          {v:  Av <= b}
//'  Input:  A, b --- defining the polytope
//'          x0 --- interior point (Ax0 < b)
//'  Output: x --- center of the ellipsoid
//'          E2 --- E'*E
//'--------------------------------------
//' Yin Zhang, Rice University, 07/29/02
//'--------------------------------------
//'lines modified by me (Ben Cousins) have a %Ben after them
//' @export
// [[Rcpp::export]]


bool mve_run_cobra(const Eigen::MatrixXd &A,
                   const Eigen::VectorXd &b,
                   const Eigen::MatrixXd &x0,
                   double reg, Eigen::VectorXd & x,
                   Eigen::MatrixXd & E,
                   int maxiter){
  double tol2 = 1.e-6;
  int m=A.rows();
  int n=A.cols();
  Eigen::MatrixXd E2(n,n);
  
  bool converged=mve_solver(A,b,x0,reg, x, E2, maxiter,tol2);
  
  E=E2.llt().matrixU().transpose();
  return converged;
}



//' shiftPolytope
//' shift a polytope and saves into to undo the transformation
//' @param A a matrix of coefficients of inequality constants A.x<=b
//' @param b a vector of length equals to nrow(A)
//' @param N the matrix storing the total transformation 
//' (including current and previous transformations)
//' @param p the vector storing the total shift
//' (including current and previous transformations)
//' @param T a matrix
//' @param trans matrix of transformation to be applied
//' @param shift vector of shift to be applied
//'
//' @section Details:
//' The CHRR algorithm is a C++ translation of cobratoolbox code written
//' by Yin Zhang licensed under GNU GPL-3 https://github.com/opencobra/cobratoolbox/ 
//' and of matlab code written by Ben Cousins 
//' (https://github.com/Bounciness/Volume-and-Sampling) is a C++ translation of
//' matlab functions provided in the opencobra toolbox and 
//' in this github repository % `m x n` sparse matrix `A`.
//' 
//' shift the polytope by a point and apply a transformation, while retaining
//'the information to undo the transformation later (to recover the samples)
//'let x denote the original space, y the current space, and z the new space
//'we have
//'
//'   P.A y <= P.b   and x = N*y+p
//'
//'  applying the transformation
//'
//'   trans * z + shift = y
//'
//' yields the system
//'
//'  x = (N * trans) * z + N*shift + p
//'  (P.A * trans) * z <= P.b - P.A * shift
//' @export
// [[Rcpp::export]]
void shiftPolytope(Eigen::MatrixXd & A, Eigen::VectorXd & b,
                   Eigen::MatrixXd & N,
                   Eigen::VectorXd &p, Eigen::MatrixXd &T,
                   const Eigen::MatrixXd & trans, 
                   const Eigen::VectorXd & shift){
  p = p + N*shift;
  N = N * trans;
  T = T * trans;
  b = b - A*shift;
  A = A*trans;
}


//' round
//' this function rounds the polytope and is adapted from the preprocess matlab
//' function
//' @param A a matrix of coefficients of inequality constants A.x<=b
//' @param b a vector of length equals to nrow(A)
//' @param N_total the to store the transformation
//' @param p_shift the vector storing the total shift
//' @param T a matrix
//' @param maxiter a tuning parameter
//'
//' @section Details:
//' The CHRR algorithm is a C++ translation of cobratoolbox code written
//' by Yin Zhang licensed under GNU GPL-3 https://github.com/opencobra/cobratoolbox/ 
//' and of matlab code written by Ben Cousins 
//' (https://github.com/Bounciness/Volume-and-Sampling) is a C++ translation of
//' matlab functions provided in the opencobra toolbox and 
//' in this github repository % `m x n` sparse matrix `A`.
//' 
//' The algorithms rounds the polytope for
//' the volume/sampling algorithms to be accurate and efficient.
//' @export
// [[Rcpp::export]]


  
void round(Eigen::MatrixXd &A,
           Eigen::VectorXd &b,
           Eigen::VectorXd &x0,
           Eigen::MatrixXd & N_total,
           Eigen::VectorXd & p_shift,
           Eigen::MatrixXd & T,
           int maxiter = 80){
  
  
  int m=A.cols();
  int n=A.rows();
  p_shift=Eigen::VectorXd::Zero(m);
  
  //check that x0 is a good answer, otherwise, finds another one
/*  while (((A*x0-b).array()>=0).any()){
    Eigen::VectorXd xnew(m);
    newSolution(A,b,x0,xnew);
    x0=xnew;
}*/

  //scale the matrix to help with numerical precision
  Eigen::VectorXd cs(m);
  Eigen::VectorXd rs(n);
  gmscale(A,cs, rs, 0.99);

  A=rs.cwiseInverse().asDiagonal()*A*cs.cwiseInverse().asDiagonal();
  b=rs.cwiseInverse().asDiagonal()*b;
  N_total=Eigen::MatrixXd::Identity(m, m) * cs.cwiseInverse().asDiagonal();
  
  x0=N_total.inverse()*(x0-p_shift);
  
  int max_its=20;
  int its=0;
  double reg=1e-3;
  Eigen::MatrixXd Tmve = Eigen::MatrixXd::Identity(m, m);
  T = Eigen::MatrixXd::Identity(m, m);
  Eigen::EigenSolver<Eigen::MatrixXd> es;
  Eigen::VectorXd eigen=es.compute(Tmve, false).eigenvalues().real();
  bool converged=false;
  Rcout<<"##starting loop "<<std::endl;
  
  while (((eigen.maxCoeff() > 6 * eigen.minCoeff()) && !converged) || reg >1e-6){
    ++its;
    Rcout<<"##rounding iteration "<<its<<std::endl;
    
  /*  while (((A*x0-b).array()>=0).any()){
      Eigen::VectorXd xnew(m);
      newSolution(A,b,x0,xnew);
      x0=xnew;
  }*/

    reg=std::max(reg/10., 1e-10);
    Eigen::VectorXd T_shift(m);
    converged=mve_run_cobra(A, b, x0, reg, T_shift, Tmve, maxiter);
    shiftPolytope(A, b, N_total, p_shift, T, Tmve, T_shift);

    
    //x0=Tmve.inverse()*(x0-T_shift); // we shift x0 to be a solution of the shifted polytope
    x0=N_total.inverse()*(x0-p_shift);

    Eigen::VectorXd row_norms=A.rowwise().norm();
    A=row_norms.cwiseInverse().asDiagonal()*A;
    b=row_norms.cwiseInverse().asDiagonal()*b;

    if (its == max_its){
      break;
    }
    eigen=es.compute(Tmve, false).eigenvalues().real();

  }
  /*if (b.minCoeff()<=0){
    shiftPolytope(A, b, N_total, p_shift, T, MatrixXd::Identity(m,m), x0);
  }*/
}


//important here the function should return A and b that are rounded Polytope, plus at least N_total and p_shift that allows converting x a solution of
// the rounded Polytope to y a solution of the initial polytope as
// y = N_total * x + p_shift
//Note that Ainital * N = Arounded
//and brounded=binitial-Ainitial*p_shift



/* *******************************************
 * Unit testing
 ***************************************** */

bool similar(const Eigen::MatrixXd &A, const Eigen::MatrixXd&B){
  Eigen::MatrixXd diff=(A-B).array().abs().matrix();
  return(diff.maxCoeff()<=1e-3);
}

context("test C++ functions") {
  
  //unit testing of shiftPolytope
  
  test_that("shiftPolytope is ok") {
    Eigen::MatrixXd matA(4,2);
    matA << 1, 0, 0, 4, -1, 0, 0, -3;
    Eigen::VectorXd b(4);
    b<< 1, 3, -2, -6;
    Eigen::MatrixXd N(2,2);
    N = Eigen::MatrixXd::Identity(2,2);
    Eigen::MatrixXd trans(2,2);
    trans << 1,0,1,1;
    Eigen::MatrixXd T (2,2);
    Eigen::VectorXd Pshift(2);
    Pshift =Eigen::VectorXd::Zero(2);
    Eigen::VectorXd shift(2);
    T=trans;
    shift=Eigen::VectorXd::Constant(2,1);
    shiftPolytope(matA,b, N, Pshift,T, trans,shift);
    
    Eigen::MatrixXd Afinal(4,2);
    Afinal<<1,0,4,4,-1,0,-3,-3;
    Eigen::VectorXd bfinal(4);
    bfinal<<0, -1,-1,-3;
    Eigen::MatrixXd Nfinal(2,2);
    Nfinal<<1,0,1,1;
    Eigen::VectorXd shiftfinal(2);
    shiftfinal<<1,1;
    
    expect_true(similar(matA, Afinal));
    expect_true(similar(b, bfinal));
    expect_true(similar(N, Nfinal));
    expect_true(similar(Pshift, shiftfinal));
  }
  

  
  //unit testing of gmscale
  test_that("gmscale is ok") {
    Eigen::MatrixXd matA(4,2);
    matA << 1.0000,0.0,0,4,-3,0,0,-100;
    Eigen::VectorXd cscale = Eigen::VectorXd(matA.cols());
    Eigen::VectorXd rscale = Eigen::VectorXd(matA.rows());
    double scltol=1e-6;
    Eigen::VectorXd cscalefinal = Eigen::VectorXd(matA.cols());
    cscalefinal <<1.0000000000000002,1.0000000000000002;
    Eigen::VectorXd rscalefinal = Eigen::VectorXd(matA.rows());
    rscalefinal<<1,4,3,100;
    gmscale(matA,cscale,rscale,scltol);
    expect_true(similar(cscale, cscalefinal));
    expect_true(similar(rscale, rscalefinal));
    
  }
  
  
  
  //unit testing of mve_solver
  test_that("mve_solver is ok") {
    Eigen::MatrixXd A(4,2);
    A <<  1.0000,0.0,0,4,-3,0,0,-100;
    Eigen::VectorXd b(4);
    b<<100,5,-3,-50;
    Eigen::VectorXd x0(2);
    x0 <<75,.75;
    Eigen::VectorXd x(2);
    Eigen::MatrixXd E2(2,2);
    Eigen::VectorXd xfinal(2);
    Eigen::MatrixXd E2final(2,2);
    E2final<< 632.88,0,0,.191956;
    xfinal << -0.193302,0.0845026;
    mve_solver(A,
               b,
               x0,
               1,
               x,
               E2,
               50,1e-6);
    expect_true(similar(E2, E2final));
    expect_true(similar(x, xfinal));
    
  }
  
  //unit testing of round
  test_that("round is ok") {
    Eigen::MatrixXd matA(4,2);
    matA << 1, 0, 0,1,-1,0,0,-1;
    
    Eigen::MatrixXd matAfinal(4,2);
    matAfinal << 1, 0, 0,1,-1,0,0,-1;
    
    Eigen::VectorXd b(4);
    b<<100,1,100,1;
    
    Eigen::VectorXd bfinal(4);
    bfinal<<1,1,1,1;
    
    
    //This is a long polytope
    Eigen::VectorXd x0(2); //initial solution
    x0=Eigen::VectorXd::Constant(2,0);
    
    Eigen::VectorXd x0final(2); //initial solution
    x0final=Eigen::VectorXd::Zero(2);
    
    Eigen::MatrixXd T(2,2);
    Eigen::MatrixXd Tfinal(2,2);
    Tfinal<<100,0,0,1;
    
    Eigen::MatrixXd N(2,2);
    Eigen::MatrixXd Nfinal(2,2);
    Nfinal<<100,0,0,1;
    Eigen::VectorXd p_shift(2);
    Eigen::VectorXd p_shiftfinal(2);
    p_shiftfinal<< 0,0;
    
    
    round(matA,b,x0,N,p_shift,T);
    
    expect_true(similar(matA, matAfinal));
    expect_true(similar(b, bfinal));
    expect_true(similar(N, Nfinal));
    expect_true(similar(T, Tfinal));
    expect_true(similar(p_shift, p_shiftfinal));
    
  }
  
  
  
}


