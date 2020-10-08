// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_RCaN_RCPPEXPORTS_H_GEN_
#define RCPP_RCaN_RCPPEXPORTS_H_GEN_

#include <RcppEigen.h>
#include <Rcpp.h>

namespace RCaN {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("RCaN", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("RCaN", "_RCaN_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in RCaN");
            }
        }
    }

    inline Eigen::MatrixXd cpgs(const int N, const Eigen::MatrixXd& A, const Eigen::VectorXd& b, const Eigen::VectorXd& x0, const int thin = 1, const bool test = false, const int seed = 1, const int stream = 1) {
        typedef SEXP(*Ptr_cpgs)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_cpgs p_cpgs = NULL;
        if (p_cpgs == NULL) {
            validateSignature("Eigen::MatrixXd(*cpgs)(const int,const Eigen::MatrixXd&,const Eigen::VectorXd&,const Eigen::VectorXd&,const int,const bool,const int,const int)");
            p_cpgs = (Ptr_cpgs)R_GetCCallable("RCaN", "_RCaN_cpgs");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_cpgs(Shield<SEXP>(Rcpp::wrap(N)), Shield<SEXP>(Rcpp::wrap(A)), Shield<SEXP>(Rcpp::wrap(b)), Shield<SEXP>(Rcpp::wrap(x0)), Shield<SEXP>(Rcpp::wrap(thin)), Shield<SEXP>(Rcpp::wrap(test)), Shield<SEXP>(Rcpp::wrap(seed)), Shield<SEXP>(Rcpp::wrap(stream)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Eigen::MatrixXd >(rcpp_result_gen);
    }

    inline Eigen::MatrixXd cpgsEquality(const int N, const Eigen::MatrixXd& A, const Eigen::VectorXd& b, const Eigen::MatrixXd& C, const Eigen::VectorXd& v, const Eigen::VectorXd& x0, const int thin = 1, const bool test = false, const int seed = 1, const int stream = 1) {
        typedef SEXP(*Ptr_cpgsEquality)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_cpgsEquality p_cpgsEquality = NULL;
        if (p_cpgsEquality == NULL) {
            validateSignature("Eigen::MatrixXd(*cpgsEquality)(const int,const Eigen::MatrixXd&,const Eigen::VectorXd&,const Eigen::MatrixXd&,const Eigen::VectorXd&,const Eigen::VectorXd&,const int,const bool,const int,const int)");
            p_cpgsEquality = (Ptr_cpgsEquality)R_GetCCallable("RCaN", "_RCaN_cpgsEquality");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_cpgsEquality(Shield<SEXP>(Rcpp::wrap(N)), Shield<SEXP>(Rcpp::wrap(A)), Shield<SEXP>(Rcpp::wrap(b)), Shield<SEXP>(Rcpp::wrap(C)), Shield<SEXP>(Rcpp::wrap(v)), Shield<SEXP>(Rcpp::wrap(x0)), Shield<SEXP>(Rcpp::wrap(thin)), Shield<SEXP>(Rcpp::wrap(test)), Shield<SEXP>(Rcpp::wrap(seed)), Shield<SEXP>(Rcpp::wrap(stream)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<Eigen::MatrixXd >(rcpp_result_gen);
    }

    inline List fitCaN(const int N, const Eigen::MatrixXd& A, const Eigen::VectorXd& b, const Eigen::MatrixXd& C, const Eigen::VectorXd& v, const Eigen::MatrixXd& L, const Eigen::VectorXd& x0, const int thin, const int seed = 1, const int stream = 1) {
        typedef SEXP(*Ptr_fitCaN)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_fitCaN p_fitCaN = NULL;
        if (p_fitCaN == NULL) {
            validateSignature("List(*fitCaN)(const int,const Eigen::MatrixXd&,const Eigen::VectorXd&,const Eigen::MatrixXd&,const Eigen::VectorXd&,const Eigen::MatrixXd&,const Eigen::VectorXd&,const int,const int,const int)");
            p_fitCaN = (Ptr_fitCaN)R_GetCCallable("RCaN", "_RCaN_fitCaN");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_fitCaN(Shield<SEXP>(Rcpp::wrap(N)), Shield<SEXP>(Rcpp::wrap(A)), Shield<SEXP>(Rcpp::wrap(b)), Shield<SEXP>(Rcpp::wrap(C)), Shield<SEXP>(Rcpp::wrap(v)), Shield<SEXP>(Rcpp::wrap(L)), Shield<SEXP>(Rcpp::wrap(x0)), Shield<SEXP>(Rcpp::wrap(thin)), Shield<SEXP>(Rcpp::wrap(seed)), Shield<SEXP>(Rcpp::wrap(stream)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<List >(rcpp_result_gen);
    }

}

#endif // RCPP_RCaN_RCPPEXPORTS_H_GEN_
