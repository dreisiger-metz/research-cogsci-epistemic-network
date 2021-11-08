// ============================================================================
// Filename          : $RCSfile: Statistics.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 15-Jan-2009
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 06:06:34 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines the interfaces of the stats-
//                     related utility classes.
//
// Linker options    : 
//
// Revision history  : 
// ----------------------------------------------------------------------------
// Notes             : Based upon code by John Burkardt from
//                     http://people.scs.fsu.edu/~burkardt/cpp_src/normal/
// ============================================================================
#ifndef libFGREP__Statistics_hpp
#define libFGREP__Statistics_hpp

#include <math.h>


namespace Utilities {

  namespace Statistics {


    //  =========================================================================
    /// @class    Uniform
    /// @brief    The class that contains methods related to the uniform
    ///           probability distribution
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class Uniform {
    public:
      Uniform() { m_seed = 1; }
      Uniform(int seed) { m_seed = seed; }
      virtual ~Uniform() { };

      int seed() { return m_seed; }
      void seed(int seed) { m_seed = seed; }

      double unifrnd() { return unifrnd(m_seed); }
      double unifrnd(int &seed);
      double unifrnd(double a, double b) { return unifrnd(a, b, m_seed); }
      double unifrnd(double a, double b, int &seed) { return a + (b - a) * unifrnd(seed); }

    protected:
      int m_seed;
      int m_tmp;
    };




    //  =========================================================================
    /// @class    Normal
    /// @brief    The class that contains methods related to the normal
    ///           probability distribution
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class Normal {
    public:
      Normal() { m_uniform = new Uniform(); this->seed(1); }
      Normal(int seed) { m_uniform = new Uniform(); this->seed(seed); }
      virtual ~Normal() { delete m_uniform; }

      int seed() { if ((m_count % 2) == 0) return m_seed1; else return m_seed2; }
      void seed(int seed) { m_seed1 = seed; m_seed2 = seed; m_count = 0; }

      double normrnd();
      double normrnd(int &seed);
      double normrnd(double mu, double sigma) { return mu + sigma * normrnd(); }
      double normrnd(double mu, double sigma, int &seed) { return mu + sigma * normrnd(seed); }

    protected:
      int m_seed1;
      int m_seed2;
      int m_count;

      double m_unifrnd1;
      double m_unifrnd2;
      double m_normrnd1;
      double m_normrnd2;

      Uniform *m_uniform;

      static const double TwoPi;
    };
    
  }

}

#endif
