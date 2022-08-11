#include "math_functions.h"
#include <cmath>

namespace pwalk {

template <typename Dtype>
Dtype gaussian_density(const Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& x, const Eigen::Matrix<Dtype, Eigen::Dynamic, 1>& mu, const Eigen::Matrix<Dtype, Eigen::Dynamic, Eigen::Dynamic>& sqrt_cov) {
  Eigen::Matrix<Dtype, Eigen::Dynamic, 1> c = sqrt_cov * (x - mu);
  return std::exp(-0.5*c.dot(c)) * sqrt_cov.determinant();
}


template
float gaussian_density(const Eigen::Matrix<float, Eigen::Dynamic, 1>& x, const Eigen::Matrix<float, Eigen::Dynamic, 1>& mu, const Eigen::Matrix<float, Eigen::Dynamic, Eigen::Dynamic>& sqrt_inv_cov);


template
double gaussian_density(const Eigen::Matrix<double, Eigen::Dynamic, 1>& x, const Eigen::Matrix<double, Eigen::Dynamic, 1>& mu, const Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic>& sqrt_inv_cov);

} // namespace pwalk
