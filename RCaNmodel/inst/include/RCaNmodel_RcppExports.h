// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_RCaNmodel_RCPPEXPORTS_H_GEN_
#define RCPP_RCaNmodel_RCPPEXPORTS_H_GEN_

#include <RcppEigen.h>
#include <Rcpp.h>

namespace RCaNmodel {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("RCaNmodel", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("RCaNmodel", "_RCaNmodel_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in RCaNmodel");
            }
        }
    }

    inline List cpgs(const int N, const Eigen::MatrixXd& A, const Eigen::VectorXd& b, const Eigen::VectorXd& x0, const int thin = 1, const bool gibbs = true, const int seed = 1, const int stream = 1, Rcpp::Nullable<Eigen::MatrixXd> covMat = R_NilValue) {
        typedef SEXP(*Ptr_cpgs)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_cpgs p_cpgs = NULL;
        if (p_cpgs == NULL) {
            validateSignature("List(*cpgs)(const int,const Eigen::MatrixXd&,const Eigen::VectorXd&,const Eigen::VectorXd&,const int,const bool,const int,const int,Rcpp::Nullable<Eigen::MatrixXd>)");
            p_cpgs = (Ptr_cpgs)R_GetCCallable("RCaNmodel", "_RCaNmodel_cpgs");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_cpgs(Shield<SEXP>(Rcpp::wrap(N)), Shield<SEXP>(Rcpp::wrap(A)), Shield<SEXP>(Rcpp::wrap(b)), Shield<SEXP>(Rcpp::wrap(x0)), Shield<SEXP>(Rcpp::wrap(thin)), Shield<SEXP>(Rcpp::wrap(gibbs)), Shield<SEXP>(Rcpp::wrap(seed)), Shield<SEXP>(Rcpp::wrap(stream)), Shield<SEXP>(Rcpp::wrap(covMat)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<List >(rcpp_result_gen);
    }

    inline List cpgsEquality(const int N, const Eigen::MatrixXd& A, const Eigen::VectorXd& b, const Eigen::MatrixXd& C, const Eigen::VectorXd& v, const Eigen::VectorXd& x0, const int thin = 1, const bool gibbs = true, const int seed = 1, const int stream = 1, Rcpp::Nullable<Eigen::MatrixXd> covMat = R_NilValue) {
        typedef SEXP(*Ptr_cpgsEquality)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_cpgsEquality p_cpgsEquality = NULL;
        if (p_cpgsEquality == NULL) {
            validateSignature("List(*cpgsEquality)(const int,const Eigen::MatrixXd&,const Eigen::VectorXd&,const Eigen::MatrixXd&,const Eigen::VectorXd&,const Eigen::VectorXd&,const int,const bool,const int,const int,Rcpp::Nullable<Eigen::MatrixXd>)");
            p_cpgsEquality = (Ptr_cpgsEquality)R_GetCCallable("RCaNmodel", "_RCaNmodel_cpgsEquality");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_cpgsEquality(Shield<SEXP>(Rcpp::wrap(N)), Shield<SEXP>(Rcpp::wrap(A)), Shield<SEXP>(Rcpp::wrap(b)), Shield<SEXP>(Rcpp::wrap(C)), Shield<SEXP>(Rcpp::wrap(v)), Shield<SEXP>(Rcpp::wrap(x0)), Shield<SEXP>(Rcpp::wrap(thin)), Shield<SEXP>(Rcpp::wrap(gibbs)), Shield<SEXP>(Rcpp::wrap(seed)), Shield<SEXP>(Rcpp::wrap(stream)), Shield<SEXP>(Rcpp::wrap(covMat)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<List >(rcpp_result_gen);
    }

    inline List sampleCaNCPP(const int N, const Eigen::MatrixXd& A, const Eigen::VectorXd& b, const Eigen::MatrixXd& C, const Eigen::VectorXd& v, const Eigen::MatrixXd& L, const Eigen::VectorXd& x0, const int thin, const bool gibbs = true, const int seed = 1, const int stream = 1, Rcpp::Nullable<Eigen::MatrixXd> covMat = R_NilValue) {
        typedef SEXP(*Ptr_sampleCaNCPP)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_sampleCaNCPP p_sampleCaNCPP = NULL;
        if (p_sampleCaNCPP == NULL) {
            validateSignature("List(*sampleCaNCPP)(const int,const Eigen::MatrixXd&,const Eigen::VectorXd&,const Eigen::MatrixXd&,const Eigen::VectorXd&,const Eigen::MatrixXd&,const Eigen::VectorXd&,const int,const bool,const int,const int,Rcpp::Nullable<Eigen::MatrixXd>)");
            p_sampleCaNCPP = (Ptr_sampleCaNCPP)R_GetCCallable("RCaNmodel", "_RCaNmodel_sampleCaNCPP");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_sampleCaNCPP(Shield<SEXP>(Rcpp::wrap(N)), Shield<SEXP>(Rcpp::wrap(A)), Shield<SEXP>(Rcpp::wrap(b)), Shield<SEXP>(Rcpp::wrap(C)), Shield<SEXP>(Rcpp::wrap(v)), Shield<SEXP>(Rcpp::wrap(L)), Shield<SEXP>(Rcpp::wrap(x0)), Shield<SEXP>(Rcpp::wrap(thin)), Shield<SEXP>(Rcpp::wrap(gibbs)), Shield<SEXP>(Rcpp::wrap(seed)), Shield<SEXP>(Rcpp::wrap(stream)), Shield<SEXP>(Rcpp::wrap(covMat)));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (Rcpp::internal::isLongjumpSentinel(rcpp_result_gen))
            throw Rcpp::LongjumpException(rcpp_result_gen);
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(Rcpp::as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<List >(rcpp_result_gen);
    }

    inline List degenerateSubSpace(const Eigen::MatrixXd& A, const Eigen::VectorXd& b, const Eigen::MatrixXd& C, const Eigen::VectorXd& v, const Eigen::VectorXd& z) {
        typedef SEXP(*Ptr_degenerateSubSpace)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_degenerateSubSpace p_degenerateSubSpace = NULL;
        if (p_degenerateSubSpace == NULL) {
            validateSignature("List(*degenerateSubSpace)(const Eigen::MatrixXd&,const Eigen::VectorXd&,const Eigen::MatrixXd&,const Eigen::VectorXd&,const Eigen::VectorXd&)");
            p_degenerateSubSpace = (Ptr_degenerateSubSpace)R_GetCCallable("RCaNmodel", "_RCaNmodel_degenerateSubSpace");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_degenerateSubSpace(Shield<SEXP>(Rcpp::wrap(A)), Shield<SEXP>(Rcpp::wrap(b)), Shield<SEXP>(Rcpp::wrap(C)), Shield<SEXP>(Rcpp::wrap(v)), Shield<SEXP>(Rcpp::wrap(z)));
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

#endif // RCPP_RCaNmodel_RCPPEXPORTS_H_GEN_
