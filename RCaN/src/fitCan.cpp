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
List fitCaN(const int N,const Eigen::MatrixXd &A ,const Eigen::VectorXd &b,const Eigen::MatrixXd &C ,const Eigen::VectorXd &v,const Eigen::MatrixXd &L, const Eigen::VectorXd M,const Eigen::VectorXd &x0) {
  int p=A.cols();
  int m=A.rows();
  int p2=C.cols();
  int m2=C.rows();
  MatrixXd F(N,p);
  MatrixXd B(N,M.size());
  if(m2>0){ //there are equality constraints
    F=cpgsR::cpgsEquality(N,A,b,C,v,x0);
  } else{
    F=cpgsR::cpgs(N,A,b,x0);
  }
  for (int i=0;i<N;++i){
    B.row(i)=L*F.row(i).transpose()+M;
  }
  return(List::create(F,B));
}
