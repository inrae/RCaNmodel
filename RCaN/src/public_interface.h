#ifndef PWALK_PUBLIC_INTERFACE_HPP_
#define PWALK_PUBLIC_INTERFACE_HPP_

#include <Eigen/Dense>

Eigen::MatrixXd generateDikinWalkSamples(const Eigen::VectorXd& initialization, const Eigen::MatrixXd& cons_A, const Eigen::VectorXd& cons_b, const double r, const int nb_samples, const int thin);

Eigen::MatrixXd generateVaidyaWalkSamples(const Eigen::VectorXd& initialization, const Eigen::MatrixXd& cons_A, const Eigen::VectorXd& cons_b, const double r, const int nb_samples, const int thin);

Eigen::MatrixXd generateJohnWalkSamples(const Eigen::VectorXd& initialization, const Eigen::MatrixXd& cons_A, const Eigen::VectorXd& cons_b, const double r, const int nb_samples, const int thin);


#endif // PWALK_PUBLIC_INTERFACE_HPP_
