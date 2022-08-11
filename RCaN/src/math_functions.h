#ifndef PWALK_UTIL_MATH_FUNCTIONS_HPP_
#define PWALK_UTIL_MATH_FUNCTIONS_HPP_

#include <Eigen/Dense>

namespace pwalk {

// unnormalized gaussian density
template <typename Dtype>
Dtype gaussian_density(const Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& x, const Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& mu, const Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic>& sqrt_inv_cov);

} // namespace pwalk



#endif // PWALK_UTIL_MATH_FUNTIONS_HPP_
