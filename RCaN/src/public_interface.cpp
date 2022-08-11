#include "dikin_walker.h"
#include "vaidya_walker.h"
#include "john_walker.h"


Eigen::MatrixXd generateDikinWalkSamples(const Eigen::VectorXd& initialization, const Eigen::MatrixXd& cons_A, const Eigen::VectorXd& cons_b, const double r, const int nb_samples, const int thin){

  Eigen::MatrixXd samples = Eigen::MatrixXd::Zero(nb_samples, cons_A.cols());

  pwalk::DikinWalker<double> dikinw = pwalk::DikinWalker<double>(initialization, cons_A, cons_b, r);

  Eigen::VectorXd new_sample = Eigen::VectorXd::Zero(cons_A.cols());

  int i = 0;
  int iter = 0;

  while (i < nb_samples){
    dikinw.doSample(new_sample);
    if ((iter % thin) == 0){
      samples.row(i) = new_sample;
      ++i;
      }
      ++iter;
  }

  return samples;
}

Eigen::MatrixXd generateVaidyaWalkSamples(const Eigen::VectorXd& initialization, const Eigen::MatrixXd& cons_A, const Eigen::VectorXd& cons_b, const double r, const int nb_samples, const int thin){

  Eigen::MatrixXd samples = Eigen::MatrixXd::Zero(nb_samples, cons_A.cols());

  pwalk::VaidyaWalker<double> vaidyaw = pwalk::VaidyaWalker<double>(initialization, cons_A, cons_b, r);

  Eigen::VectorXd new_sample = Eigen::VectorXd::Zero(cons_A.cols());
  int i = 0;
  int iter = 0;

  while (i < nb_samples){
    vaidyaw.doSample(new_sample);
    if ((iter % thin) == 0){
      samples.row(i) = new_sample;
      ++i;
      }
      ++iter;
  }


  return samples;
}

Eigen::MatrixXd generateJohnWalkSamples(const Eigen::VectorXd& initialization, const Eigen::MatrixXd& cons_A, const Eigen::VectorXd& cons_b, const double r, const int nb_samples, const int thin){

  Eigen::MatrixXd samples = Eigen::MatrixXd::Zero(nb_samples, cons_A.cols());

  pwalk::JohnWalker<double> johnw = pwalk::JohnWalker<double>(initialization, cons_A, cons_b, r);

  Eigen::VectorXd new_sample = Eigen::VectorXd::Zero(cons_A.cols());
  int i = 0;
  int iter = 0;

  while (i < nb_samples){
    johnw.doSample(new_sample);
    if ((iter % thin) == 0){
      samples.row(i) = new_sample;
      ++i;
      }
      ++iter;
  }

  return samples;
}

