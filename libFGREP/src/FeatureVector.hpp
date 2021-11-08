// ============================================================================
// Filename          : $RCSfile: FeatureVector.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 19-Jan-2009
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 06:06:34 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines the interface of the feature
//                     vector class
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libFGREP__FeatureVector_hpp
#define libFGREP__FeatureVector_hpp

#include <string>
#include <vector>

#include "Endian.hpp"
#include "Exceptions.hpp"
#include "Perceptron.hpp"


namespace ANN {


  //  ========================================================================80
  /// @class    FeatureVector
  /// @brief    A class that can represent a network's feature vectors
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class FeatureVector {
  public:
    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates an instance of the distributed representation
    /// base.
    ///
    /// @param label  the term's identifying label
    /// @param size   the dimensionality of the distributed representation
    //  ======================================================================80
    FeatureVector(std::string label, unsigned size);


    //  ======================================================================80
    /// The destructor
    //  ======================================================================80
    virtual ~FeatureVector() { };


    //  ======================================================================80
    /// The index operator
    ///
    /// This convenience method overloads the [] operator and returns a refer-
    /// ence to the specified element of FeatureVector::representation.
    ///
    /// @param index   the element of representation to retrieve
    ///
    /// @returns a reference to the specified element
    //  ======================================================================80
    inline double &operator[](const unsigned int index) { return representation[index]; }


    //  ======================================================================80
    /// Returns the vector's size
    ///
    /// This convenience method simply calls the size() method of FeatureVector::
    /// representation.
    ///
    /// @returns representation.size()
    //  ======================================================================80
    inline unsigned size() { return representation.size(); }


    //  ======================================================================80
    /// Loads the feature vector into the input layer
    ///
    /// This method loads the feature vector into the input layer.
    ///
    /// @param inputLayer   the network's input layer
    ///
    /// @returns the length of the representation
    //  ======================================================================80
    virtual unsigned loadRepresentation(std::vector<Perceptron*> &inputLayer) throw (Exception::IncompatibleVectorSize);


    //  ======================================================================80
    /// Saves the input layer's pattern of activation into feature vector
    ///
    /// This method updates the feature vector based upon the input layer's
    ///  pattern of activation.
    ///
    /// @param inputLayer   the network's input layer
    ///
    /// @returns the length of the representation
    //  ======================================================================80
    virtual unsigned saveRepresentation(std::vector<Perceptron*> &inputLayer) throw (Exception::IncompatibleVectorSize);


    //  ======================================================================80
    /// Sets the output layer's error signal
    ///
    /// This method sets the output layer's error signal based upon the feature
    /// vector.
    ///
    /// @param outputLayer   the network's output layer
    ///
    /// @returns the length of the representation
    //  ======================================================================80
    virtual unsigned setErrorSignals(std::vector<Perceptron*> &outputLayer) throw (Exception::IncompatibleVectorSize);


    //  ======================================================================80
    /// Saves the feature vector's state to an output file stream
    ///
    /// This method saves the state of the feature vector to the specified 
    /// output file stream
    ///
    /// @param ofp   the output file stream
    ///
    /// @returns the number of bytes written to the output stream
    //  ======================================================================80
    virtual unsigned save(FILE *ofp);


    //  ======================================================================80
    /// Loads the feature vector's state from an input file stream
    ///
    /// This method loads the state of the feature vector from the specified 
    /// input file stream
    ///
    /// @param ifp   the input file stream
    ///
    /// @returns the number of bytes read from the input stream
    //  ======================================================================80
    virtual unsigned load(FILE *ifp);


    //  ======================================================================80
    /// Prints the feature vector's state
    ///
    /// This method prints the state of the feature vector to the specified 
    /// output file stream
    ///
    /// @param ofp     the input file stream
    /// @param prefix  the constant C-style string that will be prefixed
    ///                to each line of output; this allows for arbitrary 
    ///                levels of indenting
    ///
    /// @returns the number of bytes read from the input stream
    //  ======================================================================80
    virtual unsigned print(FILE *ofp, const char *prefix);


    std::string          label;            /// the vector's identifying label
    std::vector<double>  representation;   /// the vector proper

  protected:
    unsigned m_i;
  };
  
 }
  
#endif
