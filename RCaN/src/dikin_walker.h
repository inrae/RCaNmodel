#ifndef PWALK_DIKIN_WALKER_HPP_
#define PWALK_DIKIN_WALKER_HPP_

#include <Eigen/Dense>
#include "math_functions.h"
#include "walker.h"

namespace pwalk {

template <typename Dtype>
class DikinWalker: public Walker<Dtype> {
public:
  DikinWalker(const Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& initialization, const Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic>& cons_A, const Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& cons_b, const Dtype r) : Walker<Dtype>(initialization, cons_A, cons_b), r_(r){}

  // getter for radius
  Dtype getRadius() {
    return r_;
  }

  void proposal(Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& new_sample);

  bool acceptRejectReverse(const Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& new_sample);

  bool doSample(Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& new_sample, const Dtype lazy = Dtype(0.5));

  void sqrtInvHessBarrier(const Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& new_sample, Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic>& new_sqrt_inv_hess);

private:
  const Dtype r_;
};

} // namespace pwalk

#endif // PWALK_DIKIN_WALKER_HPP_
