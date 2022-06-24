#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Cholesky>
#include <Eigen/Sparse>
#include <limits>
#include <dqrng.h>
#include <dqrng_RcppExports.h>
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(dqrng)]]
using namespace Rcpp;
using namespace RcppEigen;
using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::VectorXd;

//' @export
//' @useDynLib RCaN





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
    S = Rcpp::as< Map<MatrixXd> >(covMat);
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


        y += (tmin+(tmax-tmin)*dqrng::runif())*u;

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
List sampleCaNCPP(const int N, const Eigen::MatrixXd &A ,const Eigen::VectorXd &b,
               const Eigen::MatrixXd &C ,const Eigen::VectorXd &v,
               const Eigen::MatrixXd &L,
               const Eigen::VectorXd &x0, const int thin, const bool gibbs=true,
               const int seed=1, const int stream=1,
               Rcpp::Nullable<Eigen::MatrixXd> covMat = R_NilValue) {
  int p=A.cols();
  int m2=C.rows();
  MatrixXd F(N, p);
  List x;
  MatrixXd B(N, L.rows());
  if(m2>0){ //there are equality constraints
    x=cpgsEquality(N, A, b, C, v, x0, thin, gibbs, seed, stream, covMat);
  } else{
    x=cpgs(N, A, b, x0, thin,gibbs, seed, stream, covMat);
  }
  Eigen::MatrixXd xtmp(Rcpp::as<Eigen::MatrixXd>(x["X"]));
  for (int i=0;i<N;++i){
    B.row(i)=L*xtmp.row(i).transpose();
  }
  return(List::create(F,B, x["covMat"]));
}




