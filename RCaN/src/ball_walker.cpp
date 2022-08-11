#include "ball_walker.h"

#include <cmath>
#include <Rcpp.h>
#include <dqrng.h>
#include <dqrng_RcppExports.h>
#include <RcppEigen.h>

using namespace Rcpp;
using Eigen::Map;

namespace pwalk {

template <typename Dtype>
bool BallWalker<Dtype>::doSample(Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& new_sample, const Dtype lazy){
  proposal(new_sample);
  this->nb_curr_samples_ += 1;
  // for lazy markov chain
  Dtype random_num = (Dtype) dqrng::dqrunif(1)[1];
  // check balance and check in polytope
  if (random_num < lazy &&  this->checkInPolytope(new_sample) && acceptRejectReverse(new_sample)){
    this->curr_sample_ = new_sample;
    return true;
  } else {
    new_sample = this->curr_sample_;
    return false;
  }
}

template <typename Dtype>
void BallWalker<Dtype>::proposal(Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& new_sample){
  Eigen::Map<Eigen::VectorXd> gaussian_step (
      Rcpp::as<Eigen::Map<Eigen::VectorXd > >(dqrng::dqrnorm(this->nb_dim_, 0, 1)));

  new_sample = this->curr_sample_ + r_/std::sqrt(Dtype(this->nb_dim_)) * gaussian_step;
}

template <typename Dtype>
bool BallWalker<Dtype>::acceptRejectReverse(const Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& new_sample){
  Dtype random_num = (Dtype) dqrng::dqrunif(1)[1];
  // lazy version of the walk
  if (random_num > 0.99) {
    return false;
  }

  return true;
}


INSTANTIATE_CLASS(BallWalker);

} // namespace pwalk
