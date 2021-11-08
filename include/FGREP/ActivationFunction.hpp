// ============================================================================
// Filename          : $RCSfile: ActivationFunction.hpp,v $
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
// Purpose           : This header file defines the Activation functions and
//                     their base class
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libFGREP__ActivationFunction_hpp
#define libFGREP__ActivationFunction_hpp

#include <math.h>


namespace ANN {

  
  //  =========================================================================
  /// @class    ActivationFunction
  /// @brief    The base class for all activation functions
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  =========================================================================
  class ActivationFunction {
  public:
    virtual ~ActivationFunction() { };

    virtual double value(double v) = 0;
    virtual double derivative(double v) = 0;
  };
  
  

  
  //  =========================================================================
  /// @class    Sigmoid
  /// @brief    A class that implements the sigmoid functions
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  =========================================================================
  class Sigmoid : public ActivationFunction {
  public:
    Sigmoid() { m_scale = 1.0; }
    Sigmoid(double scale) { m_scale = scale; }
    
    double value(double v);
    double derivative(double v);
    
  protected:
    double m_scale;
  };

}
  
#endif
