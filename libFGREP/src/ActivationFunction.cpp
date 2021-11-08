#include "ActivationFunction.hpp"

using namespace ANN;


double Sigmoid::value(double v) {
  if (m_scale == 1.0)
    return 1.0 / (1.0 + exp(-v));
  else
    return 1.0 / (1.0 + exp(-m_scale * v));
}


double Sigmoid::derivative(double v) {
  double tmp_d;
  
  if (m_scale == 1.0) {
    tmp_d = 1 + exp(-v);
    return exp(-v) / (tmp_d * tmp_d);
  } else {
    tmp_d = 1 + exp(-m_scale * v);
    return m_scale * exp(-m_scale * v) / (tmp_d * tmp_d);
  }
}

