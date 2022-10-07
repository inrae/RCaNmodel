#ifndef SAMPLECANCPP_H
#define SAMPLECANCPP_H

#include <Rcpp.h>
#include <RcppEigen.h>



using namespace Rcpp;
using namespace RcppEigen;
using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::VectorXd;






void updateS(Eigen::MatrixXd &S, Eigen::MatrixXd &S2, Eigen::MatrixXd &S0, Eigen::VectorXd &M, Eigen::VectorXd &delta0, const Eigen::VectorXd &x, int iter);

IntegerVector order_(const NumericVector & x) ;


List cpgs(const int N, const Eigen::MatrixXd &A ,
          const Eigen::VectorXd &b,
          const Eigen::VectorXd &x0,
          const int thin,
          const int method,
          const int seed,
          const int stream,
          Rcpp::Nullable<Eigen::MatrixXd> covMat,
          Rcpp::Nullable<Eigen::MatrixXd> savedN_total,
          Rcpp::Nullable<Eigen::VectorXd> savedp_shift) ;


List cpgsEquality(const int N, const Eigen::MatrixXd &A,
                  const Eigen::VectorXd &b, const Eigen::MatrixXd &C,
                  const Eigen::VectorXd &v, const Eigen::VectorXd &x0,
                  const int thin, const int method,
                  const int seed, const int stream,
                  Rcpp::Nullable<Eigen::MatrixXd> covMat);


List sampleCaNCPP(const int N, 
                  const Eigen::MatrixXd &A ,
                  const Eigen::VectorXd &b,
                  const Eigen::MatrixXd &C ,const Eigen::VectorXd &v,
                  const Eigen::MatrixXd &L,
                  const Eigen::VectorXd &x0, 
                  const int thin, 
                  const int method,
                  const int seed,
                  const int stream,
                  Rcpp::Nullable<Eigen::MatrixXd> covMat,
                  Rcpp::Nullable<Eigen::MatrixXd> N_total,
                  Rcpp::Nullable<Eigen::VectorXd> p_shift);






void gmscale(MatrixXd A, VectorXd &cscale, VectorXd &rscale, double scltol);




bool mve_solver(MatrixXd A,
                VectorXd b,
                const VectorXd &x0,
                double reg,
                VectorXd & x,
                MatrixXd & E2,
                const int maxiter,
                const double tol);











bool mve_run_cobra(const MatrixXd &A, 
                   const VectorXd &b, 
                   const MatrixXd &x0, 
                   double reg, VectorXd & x,
                   MatrixXd & E,
                   int maxiter=80);




void shiftPolytope(MatrixXd & A, VectorXd & b,
                   MatrixXd & N,
                   VectorXd &p, MatrixXd &T,
                   const MatrixXd & trans, 
                   const VectorXd & shift);


//This function is called preprocess in matlab

void round(MatrixXd &A,
           VectorXd &b, 
           VectorXd &x0,
           MatrixXd & N_total,
           VectorXd & p_shift,
           MatrixXd & T,
           int maxiter);

List degenerateSubSpace(const Eigen::MatrixXd &A,
                        const Eigen::VectorXd &b, const Eigen::MatrixXd &C,
                        const Eigen::VectorXd &v, const Eigen::VectorXd &z);
#endif
