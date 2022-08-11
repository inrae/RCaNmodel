#include "vaidya_walker.h"

#include <cmath>
#include <algorithm>
#include <dqrng.h>
#include <dqrng_RcppExports.h>
#include <Rcpp.h>
#include <RcppEigen.h>

using namespace Rcpp;
using Eigen::Map;

namespace pwalk {

template <typename Dtype>
bool VaidyaWalker<Dtype>::doSample(Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& new_sample, const Dtype lazy){
  proposal(new_sample);
  this->nb_curr_samples_ += 1;
  // for lazy markov chain
  Dtype random_num = (Dtype) dqrng::dqrunif(1)[1];
  // check balance and check in polytope
  if (random_num < lazy && this->checkInPolytope(new_sample) && acceptRejectReverse(new_sample)){
    this->curr_sample_ = new_sample;
    return true;
  } else {
    new_sample = this->curr_sample_;
    return false;
  }
}

template <typename Dtype>
void VaidyaWalker<Dtype>::proposal(Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& new_sample){
  Eigen::Map<Eigen::VectorXd> gaussian_step (
      Rcpp::as<Eigen::Map<Eigen::VectorXd > >(dqrng::dqrnorm(this->nb_dim_, 0, 1)));

  // get hessian
  Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic> new_sqrt_inv_hess = Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic>::Zero(this->nb_dim_, this->nb_dim_);
  sqrtInvHessBarrier(this->curr_sample_, new_sqrt_inv_hess);

  new_sample = this->curr_sample_ + r_ / std::sqrt(std::sqrt(Dtype(this->nb_dim_)*Dtype(this->nb_cons_)))  * (new_sqrt_inv_hess * gaussian_step);
}

template <typename Dtype>
bool VaidyaWalker<Dtype>::acceptRejectReverse(const Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& new_sample){
  // get hessian on x
  Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic> new_sqrt_inv_hess_x = Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic>::Zero(this->nb_dim_, this->nb_dim_);
  sqrtInvHessBarrier(this->curr_sample_, new_sqrt_inv_hess_x);
  // get hessian on y
  Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic> new_sqrt_inv_hess_y = Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic>::Zero(this->nb_dim_, this->nb_dim_);
  sqrtInvHessBarrier(new_sample, new_sqrt_inv_hess_y);

  Dtype scale = r_ / std::sqrt(std::sqrt(Dtype(this->nb_dim_)*Dtype(this->nb_cons_)));
  Dtype p_y_to_x = gaussian_density<Dtype>(this->curr_sample_, new_sample, new_sqrt_inv_hess_y.inverse()/scale);
  Dtype p_x_to_y = gaussian_density<Dtype>(new_sample, this->curr_sample_, new_sqrt_inv_hess_x.inverse()/scale);

  Dtype ar_ratio = std::min<Dtype>(1., p_y_to_x/p_x_to_y);

  Dtype random_num = (Dtype) dqrng::dqrunif(1)[1];
  // lazy version of the walk
  if (random_num > ar_ratio) {
    return false;
  }

  return true;
}

// This is sqrt of inverse of the hybrid hessian n/m H(x) + Q(x)
template <typename Dtype>
void VaidyaWalker<Dtype>::sqrtInvHessBarrier(const Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& new_sample, Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic>& new_sqrt_inv_hess){
  Eigen::Matrix<Dtype, Eigen::Dynamic, 1> inv_slack = (this->cons_b_ - this->cons_A_ * new_sample).cwiseInverse();

  Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic> half_hess = inv_slack.asDiagonal()* this->cons_A_;
  Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic> new_hess = half_hess.transpose() * half_hess;

  Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic> new_hess_inv = new_hess.inverse();

  // compute leverage scores
  // Eigen::Matrix<Dtype, Eigen::Dynamic, 1> score  = ((this->cons_A_ * new_hess_inv).cwiseProduct(this->cons_A_)).rowwise().sum().cwiseProduct(inv_slack).cwiseProduct(inv_slack);
  Eigen::Matrix<Dtype, Eigen::Dynamic, 1> score  = ((half_hess * new_hess_inv).cwiseProduct(half_hess)).rowwise().sum();

  // compute vaidya hessian
  Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic> vaidya_new_hess = half_hess.transpose() * score.asDiagonal() * half_hess + Dtype(this->nb_dim_)/Dtype(this->nb_cons_) * new_hess;

  // compute eigenvectors and eigenvalues
  Eigen::SelfAdjointEigenSolver<Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic> > es(vaidya_new_hess);

  Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic> V = es.eigenvectors();
  Eigen::Matrix<Dtype, Eigen::Dynamic, 1> Dv = es.eigenvalues();
  new_sqrt_inv_hess = V * Dv.cwiseInverse().cwiseSqrt().asDiagonal() * V.transpose();
}

INSTANTIATE_CLASS(VaidyaWalker);

} // namespace pwalk
