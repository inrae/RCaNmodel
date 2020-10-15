#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Cholesky>
#include <Eigen/Sparse>
#include <limits>
#include <dqrng.h>
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
    //S.diagonal() = S.diagonal().cwiseMax(0.0001 * M.cwiseProduct(M)); //this                                                                //ensures                                                          //a minimum CV of 1%
  } else {
    S2=x*x.transpose();
  }

}



IntegerVector order_(const NumericVector & x) {
  NumericVector sorted = clone(x).sort();
  return match(sorted, x);
}


void findOrder(IntegerVector & index, const Eigen::MatrixXd &W, const Eigen::VectorXd &y, const Eigen::VectorXd & b, Eigen::VectorXd &delta){
  /*  int nbpar = W.cols();
   int nbconstr = b.size();
   delta = b - W*y;

   Eigen::VectorXd deltastd(nbpar);
   Eigen::VectorXd coeff(nbconstr);
   NumericVector range(nbpar);

   for (int param = 0; param<nbpar; ++param){
   coeff=W.col(param);
   deltastd=delta.cwiseQuotient(coeff);
   range[param]=(coeff.array()>0).select(deltastd,9e9).minCoeff()-(coeff.array()<0).select(deltastd,-9e9).maxCoeff();
   }
   index=index[order_(range)];*/
}




// [[Rcpp::interfaces(r,cpp)]]

//' Complex Polytope Gibbs Sampling
//' This function draw uniform samples in a convex polytope with inequality constraints
//'
//' @param N the number of samples to generate
//' @param A a matrix
//' @param b a vector of length equals to nrow(A)
//' @param x0 a vector of length equals to nrcol(A) that should be in the polytope, for example returned by \code{\link{chebycenter}}
//' @param thin thinning interval
//' @param test if true, tryes a method to decrease autocorrelation
//' @param seed seed of the dqrng generator
//' @param stream stream of the dqrng generator
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
//' X0 <- chebycenter(A,b)
//' x <- cpgs(1000,A,b,X0)
//' @export
// [[Rcpp::export]]


Eigen::MatrixXd cpgs(const int N, const Eigen::MatrixXd &A ,const Eigen::VectorXd &b,const Eigen::VectorXd &x0, const int thin=1, const bool test=false, const int seed=1, const int stream=1) {
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
  int runupmax= 10*p*(p+1);
  int sampleit=0; //total number of iteration during sampling, useful for thin
  int discardmax=runupmax;
  double crit=0;
  while (isample<N){               //sampling loop
    //std::random_shuffle(index.begin(), index.end()); //we change the order to
    //limit the influence of initial ordering
    if (test & !updatingS){
      findOrder(index,W,y,b,delta);
    } else{
      index=dqrng::dqsample_int(p,p,false);
    }
    y=x;
    NumericVector alea2=dqrng::dqrunif(p);
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
    x=T1*y;

    if (stage==0){//still in adapting phase
      ++runup;
      if (updatingS) updateS(S, S2, S0, M, delta0, delta1, x, runup);
      crit=(S0.diagonal()-S.diagonal()).array().abs().cwiseQuotient(S0.diagonal().array().abs()).matrix().maxCoeff();
      if(runup>p & crit<0.001){
        stage=1;
        updatingS=false;
        Rcout<<"########adapation successful after "<<runup<<" iterations"<<std::endl;
        discardmax=runup;
      } else if (runup==runupmax){
        stage=1;
        M.setZero();
        S2.setZero();
        S.setIdentity();
        Rcout<<"########adapation unsuccessful after "<<runup<<" iterations"<<std::endl;
      }
    } else if (stage==1){ //we are in adapting phase
      ++discard;
      if (updatingS) {
        updateS(S, S2, S0, M, delta0, delta1, x, discard);
        crit=(S0.diagonal()-S.diagonal()).array().abs().cwiseQuotient(S0.diagonal().array().abs()).matrix().maxCoeff();
      }
      if (crit < 0.001) {
        if (updatingS) Rcout<<"##stop updating S during discarding phase"<<std::endl;
        updatingS=false;
      }
      if (discard==discardmax){
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
        updateS(S, S2, S0, M, delta0, delta1, x, isample);
        crit=(S0.diagonal()-S.diagonal()).array().abs().cwiseQuotient(S0.diagonal().array().abs()).matrix().maxCoeff();
      }
      if (crit < 0.001) {
        if (updatingS) Rcout<<"##stop updating S during sampling phase"<<std::endl;
        updatingS=false;
      }
      ++sampleit;
    }
    if (n % 100 == 0) Rcout<<"##iteration "<<n<<" stage "<<stage<<" crit "<<crit<<" S0.norm "<<S0.norm()<<" S-S0 "<<(S-S0).norm()<<std::endl;
    ++n;
  }
  return X;
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
//' @param x0 a vector of length equals to ncol(A) that should be in the polytope, for example returned by \code{\link{chebycenter}}
//' @param thin the thinning interval
//' @param test if true, tryes a method to decrease autocorrelation
//' @param seed seed of the dqrng generator
//' @param stream stream of the dqrng generator
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
//' C <- rbind(c(1,1,rep(0,n-2)),c(0,0,1,1,rep(0,n-4)))
//' v <- matrix(rep(0.2,2),2)
//' X0 <- rep(0.1,n)
//' x <- cpgsEquality(1000,A,b,C,v,X0)
//' @export
// [[Rcpp::export]]


Eigen::MatrixXd cpgsEquality(const int N, const Eigen::MatrixXd &A,
                             const Eigen::VectorXd &b, const Eigen::MatrixXd &C,
                             const Eigen::VectorXd &v, const Eigen::VectorXd &x0,
                             const int thin=1, const bool test=false,
                             const int seed=1, const int stream=1){
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
  MatrixXd x=cpgs(N, Abis, bbis, x0bis, thin,test,seed,stream);
  for(int i=0;i<N;++i) {
    X.row(i)=Nt*x.row(i).transpose()+x0;
  }
  return X;
}





// [[Rcpp::export]]
List fitCaN(const int N, const Eigen::MatrixXd &A ,const Eigen::VectorXd &b,
            const Eigen::MatrixXd &C ,const Eigen::VectorXd &v,
            const Eigen::MatrixXd &L,
            const Eigen::VectorXd &x0, const int thin, const bool test=false,
            const int seed=1, const int stream=1) {
  int p=A.cols();
  int m2=C.rows();
  MatrixXd F(N, p);
  MatrixXd B(N, L.rows());
  if(m2>0){ //there are equality constraints
    F=cpgsEquality(N, A, b, C, v, x0, thin, test, seed, stream);
  } else{
    F=cpgs(N, A, b, x0, thin,test, seed, stream);
  }
  for (int i=0;i<N;++i){
    B.row(i)=L*F.row(i).transpose();
  }
  return(List::create(F,B));
}




