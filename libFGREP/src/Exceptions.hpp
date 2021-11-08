// ============================================================================
// Filename          : $RCSfile: Exceptions.hpp,v $
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
// Purpose           : This header file defines the exceptions used by libFGREP
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libFGREP__Exceptions_hpp
#define libFGREP__Exceptions_hpp

#include <string>


namespace ANN {
  
  namespace Exception {
    
    //  =========================================================================
    /// @class    BaseException
    /// @brief    The base class for all of AMBR's exceptions.
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class BaseException {
    public:
      BaseException(std::string comment) { this->comment = comment; }
      
      std::string comment;
    };
    
    
    //  =========================================================================
    /// @class    IncompatibleVectorSize
    /// @brief    This class of exception is thrown when the size of an input
    ///           vector or target pattern does not match its corresponding layer
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class IncompatibleVectorSize : public BaseException {
    public:
      IncompatibleVectorSize(std::string comment) : BaseException(comment) { };
    };
    
    
    //  =========================================================================
    /// @class    DuplicateTerm
    /// @brief    This class of exception is thrown when something tries to add
    ///           an pre-existing FGREP term to the lexicon
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class DuplicateTerm : public BaseException {
    public:
      DuplicateTerm(std::string comment) : BaseException(comment) { };
    };
    
    
    //  =========================================================================
    /// @class    UnknownTerm
    /// @brief    This class of exception is thrown when a sentence refers to an
    ///           undeclared term
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class UnknownTerm : public BaseException {
    public:
      UnknownTerm(std::string comment) : BaseException(comment) { };
    };
    
    
    //  =========================================================================
    /// @class    UnknownSentence
    /// @brief    This class of exception is thrown when an invalid sentence ID 
    ///           is given
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class UnknownSentence : public BaseException {
    public:
      UnknownSentence(std::string comment) : BaseException(comment) { };
    };
    
    
    //  =========================================================================
    /// @class    IncompatibleClassType
    /// @brief    This class of exception is thrown when a Neuron::load(...)
    ///           method attempts to restore a state stream that was generated
    ///           by a different neural class
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class IncompatibleClassType : public BaseException {
    public:
      IncompatibleClassType(std::string comment) : BaseException(comment) { };
    };


    //  =========================================================================
    /// @class    IncompatibleNetworkGeometry
    /// @brief    This class of exception is thrown when a network class' load
    ///           method attempts to restore a state stream that was generated
    ///           by a neural network with a different geometry
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class IncompatibleNetworkGeometry : public BaseException {
    public:
      IncompatibleNetworkGeometry(std::string comment) : BaseException(comment) { };
    };


  }
  
}
  
#endif
