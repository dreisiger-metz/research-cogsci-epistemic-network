#include "Statistics.hpp"

using namespace Utilities::Statistics;

const double Normal::TwoPi = 6.283185307179586;




double Normal::normrnd() {
  if ((m_count++ % 2) == 0) {
    // If [m_count] is even, generate two unifmrs, create two normals, and
    // return the first normal
    m_unifrnd1 = m_uniform->unifrnd(m_seed2);
    m_seed1 = m_seed2;
    m_seed2 = m_seed2;
    m_unifrnd2 = m_uniform->unifrnd(m_seed1);

    m_normrnd1 = sqrt(-2.0 * log(m_unifrnd1)) * cos(TwoPi * m_unifrnd2);
    m_normrnd2 = sqrt(-2.0 * log(m_unifrnd1)) * sin(TwoPi * m_unifrnd2);

    return m_normrnd1;

  } else {
    //  If [m_count] is odd, return the second normal
    return m_normrnd2;
  }
}


double Normal::normrnd(int &seed) {
  if ((m_count++ % 2) == 0) {
    // If [m_count] is even, generate two unifmrs, create two normals, and
    // return the first normal
    m_unifrnd1 = m_uniform->unifrnd(seed);
    m_seed1 = seed;
    m_seed2 = seed;
    m_unifrnd2 = m_uniform->unifrnd(m_seed1);

    m_normrnd1 = sqrt(-2.0 * log(m_unifrnd1)) * cos(TwoPi * m_unifrnd2);
    m_normrnd2 = sqrt(-2.0 * log(m_unifrnd1)) * sin(TwoPi * m_unifrnd2);

    seed = m_seed1;
    return m_normrnd1;

  } else {
    //  If [m_count] is odd, return the second normal
    seed = m_seed2;
    return m_normrnd2;
  }
}




double Uniform::unifrnd(int &seed) {
  m_tmp = seed / 127773;
  seed = 16807 * (seed - m_tmp * 127773) - m_tmp * 2836;

  if (seed < 0)
    seed = seed + 2147483647;

  return (double) (seed) * 4.656612875E-10;
}
