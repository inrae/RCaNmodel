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


// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(dqrng)]]
using namespace Rcpp;
using namespace RcppEigen;
using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::VectorXd;

//' @export
//' @useDynLib RCaNmodel





void updateS(Eigen::MatrixXd &S, Eigen::MatrixXd &S2, Eigen::MatrixXd &S0, Eigen::MatrixXd &M, Eigen::MatrixXd &delta0, const Eigen::VectorXd &x, int iter){
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


double computeCrit(Eigen::MatrixXd &S, Eigen::MatrixXd &M, int iter){
  //we know that the true variance is in the 95%interval
  //k * s²/(qchish(0.975,k)) to k * s²/(qchish(0.975,k))
  //therefore, we can look at the size of the range compared
  //to squared sample mean to check whether we have achieved
  //sufficient precision
  int k = iter-1;
  double chi025 = R::qchisq(0.025, k, true, false);
  double chi975 = R::qchisq(0.975, k, true, false);
  VectorXd Range=k*S.diagonal()*(1/chi025-1/chi975);
  return Range.cwiseQuotient(M.cwiseProduct(M)).maxCoeff();
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
//' @param gibbs if true, gibbs sampling, else hitandrun
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
                     const bool gibbs=true,
                     const int seed=1,
                     const int stream=1,
                     Rcpp::Nullable<Eigen::MatrixXd> covMat = R_NilValue) {
  dqrng::dqRNGkind("Xoroshiro128+");
  dqrng::dqset_seed(IntegerVector::create(seed),
                    IntegerVector::create(stream));
  int p=A.cols();
  Eigen::ArrayXd critHist=Eigen::ArrayXd::Constant(p,999999); // keep tracks of the last criterion values
  int m=A.rows();
  double inf = std::numeric_limits<double>::max();

  // Check input arguments
  if (m < (p+1) || b.size()!=m || x0.size()!=p){
    throw std::range_error("dimensions mismatch");
  }
  // Initialisation
  Eigen::MatrixXd X(N,p);
  Eigen::MatrixXd x(p,1);
  Eigen::MatrixXd y(p,1);
  x=x0;

  // Initialize variables for keeping track of sample mean, covariance
  // and isotropic transform.
  Eigen::MatrixXd M(p,1);
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
  Eigen::MatrixXd d(m,1);
  Eigen::MatrixXd d2(m,1);
  Eigen::MatrixXd delta0(p,1);
  Eigen::MatrixXd delta1(p,1);
  Eigen::MatrixXd z(m,1);
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
  int runupmax= p*log2(p); //https://doi.org/10.1021/acs.jproteome.5b01029
  int sampleit=0; //total number of iteration during sampling, useful for thin
  int discardmax=runupmax;
  double crit=0;
  while (isample<N){               //sampling loop
    //std::random_shuffle(index.begin(), index.end()); //we change the order to
    //limit the influence of initial ordering

      index=dqrng::dqsample_int(p,p,false);
    y=x;
    // compute approximate stochastic transformation
    if ((stage==1 && discard==0) || (stage>0 && updatingS==true)){ //first true
      //sample, we now make the isotropic transformation
      ldltOfS.compute(S.transpose());
      D=ldltOfS.vectorD().cwiseMax(Dzero).asDiagonal();
      L=ldltOfS.matrixL();
      T1=ldltOfS.transpositionsP().transpose()*L*D.sqrt();
      T2=T1.inverse();
      W = A*T1;
    }
    if (stage>0) y=T2*y; //otherwise y=I^-1 * y=y

    if (gibbs == true || stage == 0){
      NumericVector alea2=dqrng::dqrunif(p);
      // choose p new components
      for (int ip=0;ip<p;++ip){
        int i=index[p-ip-1];
        //Find points where the line with the (p-1) components x_i
        //fixed intersects the bounding polytope.
        z = W.col(i); //prevent any divisions by 0
        if (ip==0)
          d2=(b - W*y);
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
    }
    else {
        Eigen::Map<Eigen::VectorXd> u(Rcpp::as<Eigen::Map<Eigen::VectorXd> >(dqrng::dqrnorm(p)));
        u /= u.norm();
        z = W * u;
        d2=(b - W*y);
        d=d2.cwiseQuotient(z);
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
      }
    x=T1*y;

    if (stage==0){//still in adapting phase
      ++runup;
      if (updatingS) updateS(S, S2, S0, M, delta0, x, runup);
      crit=computeCrit(S,M,runup);
      critHist(runup % p)=crit;
      if (runup>p & critHist.minCoeff()==crit & std::sqrt((critHist - critHist.mean()).square().sum()/(p-1))/critHist.mean()<0.01){
        stage=1;
        updatingS=false;
        Rcout<<"########adapation successful after "<<runup<<" iterations"<<std::endl;
        discardmax=runup;
      } else if (runup==runupmax){
        updatingS=false;
        critHist.setConstant(99999999);
        stage=1;
        M.setZero();
        S2.setZero();
        S.setIdentity();
        Rcout<<"########adapation unsuccessful after "<<runup<<" iterations"<<std::endl;
      }
    } else if (stage==1){ //we are in adapting phase
      ++discard;
      if (updatingS) {
        updateS(S, S2, S0, M, delta0, x, discard);
        crit=computeCrit(S,M,discard);
        critHist(discard % p)=crit;
      }
      if (discard>p & critHist.minCoeff()==crit & std::sqrt((critHist - critHist.mean()).square().sum()/(p-1))/critHist.mean()<0.01) {
        if (updatingS) Rcout<<"##stop updating S during discarding phase"<<std::endl;
        updatingS=false;
      }
      if (discard==discardmax){
        critHist.setConstant(99999999);
        stage=2;
        M.setZero();
        S.setIdentity();
        S2.setZero();
        Rcout<<"#######end of discarding phase"<<std::endl;
        if (updatingS) Rcout<<"S still updated"<<std::endl;
      }
    } else{ //we are in sampling phase
      if ((sampleit % thin) == 0){
        X.row(isample)=x.col(0);
        ++isample;
      }
      if (updatingS) {
        updateS(S, S2, S0, M, delta0, x, isample);
        crit=computeCrit(S,M,isample);
        critHist(isample % p)=crit;
      }
      if (isample>p & critHist.minCoeff()==crit & std::sqrt((critHist - critHist.mean()).square().sum()/(p-1))/critHist.mean()<0.0^1) {
        if (updatingS) Rcout<<"##stop updating S during sampling phase"<<std::endl;
        updatingS=false;
      }
      ++sampleit;
    }
    if (n % 100 == 0) Rcout<<"##iteration "<<n<<" stage "<<stage<<" crit "<<crit<<" cvHist "<<std::sqrt((critHist - critHist.mean()).square().sum()/(p-1))/critHist.mean()<<std::endl;
    ++n;
  }
  return (List::create(Named("X") = X, Named("covMat") = S));
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
//' @param gibbs if true, gibbs sampling, else hitandrun
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
                             const int thin=1, const bool gibbs=true,
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
  List x=cpgs(N, Abis, bbis, x0bis, thin, gibbs, seed, stream, covMat);
  Eigen::MatrixXd xtmp(Rcpp::as<Eigen::MatrixXd>(x["X"]));
  for(int i=0;i<N;++i) {
    X.row(i)=Nt*(xtmp.row(i).transpose())+x0;
  }
  return (List::create(Named("X") = X, Named("covMat") = x["covMat"]));
}





// [[Rcpp::export]]
List sampleCaNCPP(const int N, 
                  const Eigen::MatrixXd &A ,
                  const Eigen::VectorXd &b,
               const Eigen::MatrixXd &C ,const Eigen::VectorXd &v,
               const Eigen::MatrixXd &L,
               const Eigen::VectorXd &x0, 
               const int thin, 
               const bool gibbs=true,
               const int seed=1,
               const int stream=1,
               Rcpp::Nullable<Eigen::MatrixXd> covMat = R_NilValue) {
  int p=A.cols();
  int m2=C.rows();

    List x;
  MatrixXd B(N, L.rows());
  if(m2>0){ //there are equality constraints
    x=cpgsEquality(N, A, b, C, v, x0, thin, gibbs, seed, stream, covMat);
  } else{
    x=cpgs(N, A, b, x0, thin,gibbs, seed, stream, covMat);
  }
  Eigen::MatrixXd F(Rcpp::as<Eigen::MatrixXd>(x["X"]));
  for (int i=0;i<N;++i){
    B.row(i)=L*F.row(i).transpose();
  }
  return(List::create(F,B, x["covMat"]));
}






void gmscale(MatrixXd A, VectorXd &cscale, VectorXd &rscale, double scltol){
  /*% Geometric-Mean Scaling finds the scale values for the
   % `m x n` sparse matrix `A`.
   %
   % USAGE:
   %
   %    [cscale, rscale] = gmscale(A, iprint, scltol)
   %
   % INPUTS:
   %    A(i, j):           contains entries of `A`.
   %    iprint:            > 0 requests messages to the screen (0 means no output).
   %    scltol:            should be in the range (0.0, 1.0).
   %                       Typically `scltol` = 0.9.  A bigger value like 0.99 asks
   %                       `gmscale` to work a little harder (more passes).
   %
   % OUTPUTS:
   %    cscale, rscale:    column vectors of column and row scales such that
   %                       `R` (inverse) `A` `C` (inverse) should have entries near 1.0,
   %                       where `R= diag(rscale)`, `C = diag(cscale)`.
   %
   % An iterative procedure based on geometric means is used,
   % following a routine written by Robert Fourer, 1979.
   % Several passes are made through the columns and rows of `A`.
   % The main steps are:
   %
   %   1. Compute :math:`aratio = max_j (max_i Aij / min_i Aij)`.
   %   2. Divide each row `i` by :math:`sqrt( max_j Aij * min_j Aij)`.
   %   3. Divide each column `j` by :math:`sqrt( max_i Aij * min_i Aij)`.
   %   4. Compute `sratio` as in Step 1.
   %   5. If :math:`sratio < aratio * scltol`,
   %      set :math:`aratio = sratio` and repeat from Step 2.
   %
   % To dampen the effect of very small elements, on each pass,
   % a new row or column scale will not be smaller than sqrt(damp)
   % times the largest (scaled) element in that row or column.
   %
   % Use of the scales:
   % To apply the scales to a linear program,
   % :math:`min c^T x` st :math:`A x = b`, :math:`l \leq x \leq u`,
   % we need to define "barred" quantities by the following relations:
   % `A = R Abar C`, `b = R bbar`, `C cbar = c`,
   % `C l = lbar`, `C u = ubar`, `C x = xbar`.
   %
   % This gives the scaled problem
   % :math:`min\ cbar^T xbar` st :math:`Abar\ xbar = bbar`, :math:`lbar \leq xbar \leq ubar`.
   %
   % .. Author: - Michael Saunders, Systems Optimization Laboratory, Stanford University.
   % ..
   %    07 Jun 1996: First f77 version, based on MINOS 5.5 routine m2scal.
   %    24 Apr 1998: Added final pass to make column norms = 1.
   %    18 Nov 1999: Fixed up documentation.
   %    26 Mar 2006: (Leo Tenenblat) First Matlab version based on Fortran version.
   %    21 Mar 2008: (MAS) Inner loops j = 1:n optimized.
   %    09 Apr 2008: (MAS) All loops replaced by sparse-matrix operations.
   %                 We can't find the biggest and smallest Aij
   %                 on each scaling pass, so no longer print them.
   %    24 Apr 2008: (MAS, Kaustuv) Allow for empty rows and columns.
   %    13 Nov 2009: gmscal.m renamed gmscale.m.
   */
  
  int m = A.rows();
  int n = A.cols();
  
  A = A.array().abs().matrix();
  int maxpass=10;
  double aratio=1e50;
  double damp=1e-4;
  double small = 1e-8;
  double eps=2.2204e-16;
  MatrixXd SA;
  rscale = VectorXd::Constant(m, 1);
  cscale = VectorXd::Constant(n, 1);
  
  /*--------------------------------------------------------------
   Main loop.
   --------------------------------------------------------------*/
  for (int npass = 0; npass <= maxpass; npass ++){
    
    // Find the largest column ratio.
    // Also set new column scales (except on pass 0).
    
    rscale = (rscale.array() ==0).select(1, rscale);
    MatrixXd Rinv    = rscale.cwiseInverse().asDiagonal();
    SA      = Rinv*A;
    MatrixXd invSA= MatrixXd::Zero(m,n);
    invSA= (SA.array()==0).select(0,SA.cwiseInverse());
    VectorXd cmax    = SA.colwise().maxCoeff();   // column vector
    VectorXd cmin    = invSA.colwise().maxCoeff();   // column vector
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
    invSA= (SA.array()==0).select(0,SA.cwiseInverse());
    VectorXd rmax    = SA.rowwise().maxCoeff();   // column vector
    VectorXd rmin    = invSA.rowwise().maxCoeff();   // column vector
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
  MatrixXd Rinv    = rscale.cwiseInverse().asDiagonal();
  SA      = Rinv*A;
  
  cscale  = SA.colwise().maxCoeff();   // column vector
  cscale = (cscale.array()==0).select(1,cscale);
  
  // end of gmscale
}





void mve_solver(MatrixXd A, 
                VectorXd b, 
                const VectorXd &x0,
                double reg, 
                VectorXd & x,  
                MatrixXd & E2,
                const int maxiter = 50, 
                const double tol  = 1.e-4){
  int m = A.rows();
  int n = A.cols();
  double minmu = 1.e-8;
  double tau0 = .75;
  double last_r1=-999999999999;
  double last_r2=-999999999999;
  double bnrm = A.norm();
  VectorXd bmAx0 = b - A*x0;
  A=bmAx0.cwiseInverse().asDiagonal() * A;
  b=VectorXd::Constant(m, 1);
  x =  VectorXd::Zero(n);
  VectorXd y = VectorXd::Constant(m, 1);
  Eigen::DiagonalMatrix<double, Eigen::Dynamic> Y = VectorXd::Zero(m).asDiagonal();
  VectorXd bmAx = b;
  VectorXd resid1 = VectorXd::Zero(maxiter);
  VectorXd resid2 = VectorXd::Zero(maxiter);
  VectorXd resid3 = VectorXd::Zero(maxiter);
  VectorXd cond1 = VectorXd::Zero(maxiter);
  VectorXd cond2 = VectorXd::Zero(maxiter);
  VectorXd condE = VectorXd::Zero(maxiter);
  
  double res = 1;
  int msg = 1;
  
  double astep;
  double prev_obj = -99999999999;
  VectorXd Adx(m);
  for (int iter =0; iter < maxiter; ++iter){
    if (iter > 0) {
      bmAx -= astep*Adx;
    }
    Y.diagonal()=y;
    E2 = (A.transpose() * y.asDiagonal() * A).inverse();
    condE(iter) = E2.lu().rcond();
    MatrixXd Q = A * E2 * A.transpose();
    VectorXd h=Q.diagonal().array().sqrt().matrix();
    VectorXd z(m);
    if (iter == 0){
      double t = bmAx.cwiseProduct(h.cwiseInverse()).minCoeff();
      y /= t*t;
      h *= t;
      z = (bmAx-h).cwiseMax(1.e-1);
      Q *= t*t;
      Y.diagonal() = (Y.diagonal().array()/(t*t)).matrix();
    }
    VectorXd yz = y.cwiseProduct(z);
    VectorXd yh = y.cwiseProduct(h);
    double gap = yz.sum()/((double)m);
    double rmu = std::min(0.5, gap) * gap;
    rmu = std::max(rmu, minmu);
    
    VectorXd R1 = - A.transpose()*yh;
    VectorXd R2 = bmAx - h -z;
    VectorXd R3 = - yz + VectorXd::Constant(rmu, m);
    double r1 = R1.array().abs().sum();
    double r2 = R2.array().abs().sum();
    double r3 = R3.array().abs().sum();
    res = std::max(r1,std::max(r2, r3));
    double objval = log(E2.determinant())*.5;
    
    if (iter%10==0){
      Eigen::EigenSolver<Eigen::MatrixXd> es;
      VectorXd eigen=es.compute(E2, false).eigenvalues().real();
        if (abs((last_r1-r1)/std::min(abs(last_r1),abs(r1)))<1e-2 && abs((last_r2 - r2)/std::min(abs(last_r2),abs(r2)))<1e-2 && E2.maxCoeff()/E2.minCoeff() >100 && reg>1e-10){
          break;
        }
        last_r2 = r2;
        last_r1 = r1;
    }
    
    
    
    if ((res < tol * (1+bnrm) && rmu <= minmu) || (iter > 99 & prev_obj != -99999999999 && (prev_obj >= (1-tol) * objval || prev_obj <=(1-tol) * objval))) {
      x = x + x0;
      msg = 1;
      break;
    }
    prev_obj = objval;
    
    MatrixXd YQ=Y*Q;
    MatrixXd YQQY = YQ.cwiseProduct(YQ.transpose());
    VectorXd y2h= 2*yh;
    MatrixXd YA=Y*A;
    MatrixXd G = YQQY+ y2h.cwiseProduct(z).cwiseMax(1.e-8);
    VectorXd rsG = VectorXd::Constant(G.rows(), 1);
    VectorXd csG = VectorXd::Constant(G.cols(), 1);
    gmscale(G,csG, rsG, .99); //Ben
      
    MatrixXd T = (rsG.cwiseInverse().asDiagonal()*G*csG.cwiseInverse().asDiagonal()).inverse() * (rsG.cwiseInverse().asDiagonal()*((h+z).asDiagonal()*YA)); //Ben
    T = csG.cwiseInverse().asDiagonal()*T; //Ben
    
    cond1(iter) = (rsG.cwiseInverse().asDiagonal() * G * csG.cwiseInverse().asDiagonal()).lu().rcond();
    resid1(iter) = (G*T - (h+z).asDiagonal() * YA).norm();
    
    MatrixXd ATP=(y2h.asDiagonal()*T-YA).transpose();
    VectorXd R3Dy=R3.cwiseProduct(y.cwiseInverse());
    VectorXd R23 = R2-R3Dy;
    MatrixXd ATP_A = ATP*A;
    
    VectorXd csA=VectorXd::Constant(ATP_A.cols(),1);
    VectorXd rsA=VectorXd::Constant(ATP_A.rows(),1);
    gmscale(ATP_A,csA, rsA, 0.99);
    
    VectorXd dx=(rsA.cwiseInverse().asDiagonal()*ATP_A*csA.cwiseInverse().asDiagonal()).inverse()*(rsA.cwiseInverse()*(R1+ATP*R23)); //Ben
      dx = csA.cwiseInverse().asDiagonal() * dx; //Ben
    cond2(iter)=(rsA.cwiseInverse().asDiagonal()*ATP_A*csA.cwiseInverse().asDiagonal()).lu().rcond();
    resid2(iter) = ((ATP_A)*dx-(R1+ATP*R23)).norm();
    Adx = A*dx;
    
    
    VectorXd dyDy=(rsG.cwiseInverse().asDiagonal()*G*csG.cwiseInverse().asDiagonal()).inverse()*(rsG.cwiseInverse().asDiagonal()*y2h.cwiseProduct(Adx-R23));
      dyDy=csG.cwiseInverse().asDiagonal()*dyDy;
    resid3(iter)=(G*dyDy - y2h.cwiseProduct(Adx-R23)).norm();
    VectorXd dy = y.cwiseProduct(dyDy);
    VectorXd dz=R3Dy - z.cwiseProduct(dyDy);
    double ax=-1/std::min(-(Adx.cwiseProduct(bmAx.cwiseInverse()).eval().maxCoeff()), -.5);
    double ay=-1/std::min((dyDy.cwiseProduct(bmAx.cwiseInverse()).eval().minCoeff()), -.5);
    double az = -1/std::min(dz.cwiseProduct(z.cwiseInverse()).eval().minCoeff(), -.5); 
    double tau = std::max(tau0, 1 -res);
    astep = tau*std::min(1.,std::min(ax,std::min(ay,az)));
    x += astep*dx;
    y += astep*dy;
    z += astep*dz;
    if (cond2(iter)<1e-5 && iter >= 10){
      break; 
    }
  }
}
  
  
  
  
  
  
  
  
  
  
  


void mve_run_cobra(const MatrixXd &A, const VectorXd &b, const MatrixXd &x0, double reg, VectorXd & x, MatrixXd & E){
  /*%  Find the maximum volume ellipsoid
   %     Ell = {v:  v = x + Es, ||s|| <= 1}
   %  or Ell = {v:  ||E^{-1}(v-x)|| <= 1}
   %  inscribing a full-dimensional polytope
   %          {v:  Av <= b}
   %  Input:  A, b --- defining the polytope
   %   (Optinal x0 --- interior point, A*x0 < b)
   %  Output:  x --- center of the ellipsoid
   %           E --- matrix defining ellipsoid
   %--------------------------------------
   % Yin Zhang, Rice University, 07/29/02
   % Last modified: 09/29/16
   %--------------------------------------
   %lines modified by me (Ben Cousins) have a %Ben after them */
  
  int maxiter = 80; 
  double tol1 = 1.e-8;
  double tol2 = 1.E-6;
  int m=A.rows();
  int n=A.cols();
  MatrixXd E2(n,n);

  mve_solver(A,b,x0,reg, x, E2, maxiter,tol2);
  
  E=A.llt().matrixU().transpose();
}




void shiftPolytope(MatrixXd & A, VectorXd & b,
                   MatrixXd & N,
                   VectorXd &p, MatrixXd &T,
                   const MatrixXd & trans, 
                   const VectorXd & shift){
  p = p + N*shift;
  N = N * trans;
  T = T * trans;
  b = b - A*shift;
  A = A*trans;
}



void newSolution(const Eigen::MatrixXd &A ,
                 const Eigen::VectorXd &b,
                 const Eigen::VectorXd &x0,
                 VectorXd &x) {
  dqrng::dqRNGkind("Xoroshiro128+");
  int p=A.cols();
  int m=A.rows();
  
  double inf = std::numeric_limits<double>::max();
  
  // Check input arguments
  if (m < (p+1) || b.size()!=m || x0.size()!=p){
    throw std::range_error("dimensions mismatch");
  }
  // Initialisation
  Eigen::MatrixXd y(p,1);
  x=x0;
  
  
  NumericVector u(p);
  
  Eigen::MatrixXd T1(p,p);
  Eigen::MatrixXd T2(p,p);
  T1.setIdentity();
  Eigen::VectorXd delta(m); //vector used to store distance to bounds
  Eigen::MatrixXd d(m,1);
  Eigen::MatrixXd d2(m,1);
  Eigen::MatrixXd z(m,1);
  
  //std::random_shuffle(index.begin(), index.end()); //we change the order to
  //limit the influence of initial ordering
  
  IntegerVector index=dqrng::dqsample_int(p,p,false);
  y=x;
  // compute approximate stochastic transformation
  NumericVector alea2=dqrng::dqrunif(p);
  // choose p new components
  for (int ip=0;ip<p;++ip){
    int i=index[p-ip-1];
    //Find points where the line with the (p-1) components x_i
    //fixed intersects the bounding polytope.
    z = A.col(i); //prevent any divisions by 0
    if (ip==0)
      d2=(b - A*y);
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
    d2 -= A.col(i)*delta; //we do this to avoid making a matrix
    //multiplication for each parameter (we just update the value of the
    //constraint with the delta of parameter)
  }

  x=y;
  
}




void round(MatrixXd &A,
           VectorXd &b, 
           VectorXd &x0,
           MatrixXd & N_total,
           VectorXd & p_shift,
           MatrixXd & T){

  
  int m=A.cols();
  int n=A.rows();
  p_shift=VectorXd::Zero(m);
  
  //check that x0 is a good answer, otherwise, finds another one
  while (((A*x0-b).array()>0).any()){
    VectorXd xnew(m);
    newSolution(A,b,x0,xnew);
    x0=xnew;
  }
  
  //scale the matrix to help with numerical precision
  VectorXd cs(m);
  VectorXd rs(n);
  gmscale(A,cs, rs, 0.99);
  A=rs.cwiseInverse().asDiagonal()*A*cs.cwiseInverse().asDiagonal();
  b=rs.cwiseInverse().asDiagonal()*b;
  N_total=MatrixXd::Identity(m, m) * cs.cwiseInverse().asDiagonal();
  
  int max_its=20;
  int its=0;
  double reg=1e-3;
  MatrixXd Tmve = MatrixXd::Identity(m, m);
  T = MatrixXd::Identity(m, m);
  int converged = 0;
  Eigen::EigenSolver<Eigen::MatrixXd> es;
  VectorXd eigen=es.compute(Tmve, false).eigenvalues().real();
  while (((eigen.maxCoeff()>6*eigen.minCoeff()) && !converged) || reg >1e-6 || converged ==2){
      ++its;
    VectorXd xnew;
      newSolution(A, b, x0, xnew);
      x0=xnew;
      reg=std::max(reg/10., 1e-10);
      VectorXd T_shift(m);
      mve_run_cobra(A, b, x0, reg, T_shift, Tmve);
      
      shiftPolytope(A, b, N_total, p_shift, T, Tmve, T_shift);
      x0=Tmve.inverse()*(x0-T_shift); // we shift x0 to be a solution of the shifted polytope
      VectorXd row_norms=A.rowwise().norm();
      A=row_norms.cwiseInverse().asDiagonal()*A;
      b=row_norms.cwiseInverse().asDiagonal()*b;
      if (its == max_its){
        break;
      }
      }
      if (b.minCoeff()<=0){
        VectorXd xnew;
        newSolution(A, b, x0, xnew);
        x0=xnew;
        shiftPolytope(A, b, N_total, p_shift, T, MatrixXd::Identity(m,m), x0);
      }
}

//important here the function should return A and b that are rounded Polytope, plus at least N_total and p_shift that allows converting x a solution of
// the rounded Polytope to y a solution of the initial polytope as
// y = N_total * x + p_shift
//Note that Ainital * N = Arounded
//and brounded=binitial-Ainitial*p_shift



/* *******************************************
 * Unit testing
 ***************************************** */

bool similar(const MatrixXd &A, const MatrixXd&B){
  MatrixXd diff=(A-B).array().abs().matrix();
  return(diff.maxCoeff()<=1e-3);
}

context("test C++ functions") {
  
  //unit testing of shiftPolytope
  
  test_that("shiftPolytope is ok") {
    MatrixXd matA(4,2);
    matA << 1, 0, 0, 4, -1, 0, 0, -3;
    VectorXd b(4);
    b<< 1, 3, -2, -6;
    MatrixXd N(2,2);
    N = MatrixXd::Identity(2,2);
    MatrixXd trans(2,2);
    trans << 1,0,1,1;
    MatrixXd T (2,2);
    VectorXd Pshift(2);
    Pshift =VectorXd::Zero(2);
    VectorXd shift(2);
    T=trans;
    shift=VectorXd::Constant(2,1);
    shiftPolytope(matA,b, N, Pshift,T, trans,shift);
    
    MatrixXd Afinal(4,2);
    Afinal<<1,0,4,4,-1,0,-3,-3;
    VectorXd bfinal(4);
    bfinal<<0, -1,-1,-3;
    VectorXd Nfinal(2,2);
    Nfinal<<1,0,1,1;
    VectorXd shiftfinal(2);
    shiftfinal<<1,1;
    
    expect_true(similar(matA, Afinal));
    expect_true(similar(b, bfinal));
    expect_true(similar(N, Nfinal));
    expect_true(similar(Pshift, shiftfinal));
  }
  
  
  //unit testing of newSolution
  
    test_that("newSolution is in the polytope") {
      MatrixXd matA(4,2);
      matA << 1, 0, 0, 1, -1, 0, 0, -1;
      VectorXd b(4);
      b<< 1, 1, -1, -1;
      MatrixXd x0=VectorXd::Zero(2);
      VectorXd xnew(2);
      xnew=VectorXd::Zero(2);
      
      for (int i=0;i<10;++i){
        newSolution(matA,b,x0,xnew);
        expect_true(xnew.minCoeff()>=-1);
        expect_true(xnew.maxCoeff()<=1);
        
      }
      
    }
  
  
  //unit testing of gmscale
  test_that("gmscale is ok") {
    MatrixXd matA(3,3);
    matA << 1, 0, 3, 0;
    VectorXd cscale = VectorXd(matA.cols());
    VectorXd rscale = VectorXd(matA.rows());
    double scltol=1e-8;
    VectorXd cscalefinal = VectorXd(matA.cols());
    cscalefinal <<1.0000000000000002,1.0000000000000002,1.7320508075688776;
    VectorXd rscalefinal = VectorXd(matA.rows());
    rscalefinal<<0.99999999999999978,1.9999999999999996,1.732050807568877;
    gmscale(matA,cscale,rscale,scltol);
    expect_true(similar(cscale, cscalefinal));
    expect_true(similar(rscale, rscalefinal));
    
  }
  
  //unit testing of round
  test_that("round is ok") {
    MatrixXd matA(4,2);
    matA << 1, 0, 0,1,-1,0,0,-1;
    
    MatrixXd matAfinal(4,2);
    matAfinal << 100, 0, 0,1,-100,0,0,-1;
    
    VectorXd b(4);
    b<<100,1,100,1;
    
    VectorXd bfinal(4);
    bfinal<<100,1,100,1;
    
    
    //This is a long polytope
    VectorXd x0(2); //initial solution
    x0=VectorXd::Zero(2);
    
    VectorXd x0final(2); //initial solution
    x0final=VectorXd::Zero(2);
    
    MatrixXd T(2,2);
    MatrixXd Tfinal(2,2);
    Tfinal<<100,0,0,1;
    
    MatrixXd N(2,2);
    MatrixXd Nfinal(2,2);
    Nfinal<<100,0,0,1;
    VectorXd p_shift(2);
    VectorXd p_shiftfinal(2);
    p_shiftfinal<< 0,0;
    
    
    round(matA,b,x0,N,p_shift,T);
    
    expect_true(similar(matA, matAfinal));
    expect_true(similar(b, bfinal));
    expect_true(similar(N, Nfinal));
    expect_true(similar(T, Tfinal));
    expect_true(similar(p_shift, p_shiftfinal));
    
  }
  
  
  
}


