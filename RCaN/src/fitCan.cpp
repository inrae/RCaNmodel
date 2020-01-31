#include <Rcpp.h>
#include <RcppEigen.h>
#include <cpgsR.h>

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(cpgsR)]]
using namespace Rcpp;
using namespace RcppEigen;

using Eigen::MatrixXd;
using Eigen::VectorXd;

//' @export
//' @useDynLib RCaN


// [[Rcpp::export]]
List fitCaN(const int N, const Eigen::MatrixXd &A ,const Eigen::VectorXd &b,
            const Eigen::MatrixXd &C ,const Eigen::VectorXd &v,
            const Eigen::MatrixXd &L,
            const Eigen::VectorXd &x0, const int thin,
            const bool test) {
  int p=A.cols();
  int m2=C.rows();
  MatrixXd F(N, p);
  MatrixXd B(N, L.rows());
  if(m2>0){ //there are equality constraints
    F=cpgsR::cpgsEquality(N, A, b, C, v, x0, thin, test);
  } else{
    F=cpgsR::cpgs(N, A, b, x0, thin, test);
  }
  for (int i=0;i<N;++i){
    B.row(i)=L*F.row(i).transpose();
  }
  return(List::create(F,B));
}
