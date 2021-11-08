// ============================================================================
// Filename          : $RCSfile: MultilayerPerceptronNetwork.hpp,v $
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
// Purpose           : This header file defines the interface of the Multi-
//                     layer Perceptron network
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libFGREP__MultilayerPerceptronNetwork_hpp
#define libFGREP__MultilayerPerceptronNetwork_hpp

#include <stdarg.h>
#include <string.h>

#include "Exceptions.hpp"
#include "Perceptron.hpp"
#include "FeatureVector.hpp"


namespace ANN {

  // Forward declarations
  namespace Test {
    class MultilayerPerceptronNetworkTest;
  }
  

  //  ========================================================================80
  /// @class    MultilayerPerceptronNetwork
  /// @brief    A class that implements a multi-layer perceptron network
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class MultilayerPerceptronNetwork {
  public:
    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates a multi-layer network of perceptrons.
    ///
    /// @param label          the network's identifying label
    /// @param learningRate   the perceptrons' initial learning rate
    /// @param momentum       the perceptrons' initial momentum
    /// @param layers         the number of layers in the network
    /// @param ...            the number of neurons in each layer of the network
    //  ======================================================================80
    MultilayerPerceptronNetwork(std::string label, double learningRate, double momentum, unsigned layers, ...);


    //  ======================================================================80
    /// The destructor
    //  ======================================================================80
    virtual ~MultilayerPerceptronNetwork();


    //  ======================================================================80
    /// Adds a training datum
    ///
    /// This method adds another input--target pattern to the network's training
    /// set
    ///
    /// @param inputPattern    the input pattern
    /// @param targetPattern   and its corresponding target pattern
    ///
    /// @returns the (new) total number of input--target pairs
    //  ======================================================================80
    unsigned addTrainingDatum(FeatureVector inputPattern, FeatureVector targetPattern) throw (Exception::IncompatibleVectorSize);


    //  ======================================================================80
    /// Processes an input pattern
    ///
    /// This method loads the specified pattern into the input layer, and propa-
    /// gates the activation forward through to the output layer.
    ///
    /// @param input   the input pattern
    ///
    /// @returns the pattern of activation at the results at the output layer
    //  ======================================================================80
    FeatureVector processInput(FeatureVector &input) throw (Exception::IncompatibleVectorSize);


    //  ======================================================================80
    /// Trains the network
    ///
    /// This method iterates over all training samples and implements the feed-
    /// forward, error backpropagation learning algorithm.
    ///
    /// @param errorThreshold   the minimum average squared error level at 
    ///                         which the training stops
    ///
    /// @returns the average error squared of the output for this epoch
    //  ======================================================================80
    double trainNetwork(double errorThreshold);


    //  ======================================================================80
    /// Trains the network
    ///
    /// This method iterates over all training samples and implements the feed-
    /// forward, error backpropagation learning algorithm.
    ///
    /// @param iterations   the desired number of training epochs
    ///
    /// @returns the average error squared of the output for this epoch
    //  ======================================================================80
    double trainNetwork(unsigned iterations);


    //  ======================================================================80
    /// Saves the network's state to an output file stream
    ///
    /// This method saves the state of the network to the specified output file
    /// stream.
    ///
    /// @param ofp   the output file stream
    ///
    /// @returns the number of bytes written to the output stream
    //  ======================================================================80
    //unsigned save(FILE *ofp);


    //  ======================================================================80
    /// Loads the network's state from an input file stream
    ///
    /// This method loads the state of the network from the specified input file
    /// stream.
    ///
    /// @param ifp   the input file stream
    ///
    /// @returns the number of bytes read from the input stream
    //  ======================================================================80
    //unsigned load(FILE *ifp);
    

    //  ======================================================================80
    /// Prints the network's information
    ///
    /// This method prints the details of the multilayer perceptron network to
    /// the specified output stream.
    ///
    /// @param ostream the output stream to which the information will be
    ///                printed
    /// @param prefix  the constant C-style string that will be prefixed
    ///                to each line of output; this allows for arbitrary 
    ///                levels of indenting
    ///
    /// @retval the number of characters printed
    //  ======================================================================80
    unsigned print(FILE *ofp, const char *prefix);


    inline unsigned epochs() { return m_epochs; }


    std::string  label;
    double       learningRate;
    double       momentum;
    
    std::vector<std::vector<Perceptron*> >  perceptron;
    std::vector<FeatureVector>              inputPattern;
    std::vector<FeatureVector>              targetPattern;
    

  protected:
    unsigned m_numberOfLayers;
    unsigned m_sizeOfInputLayer;
    unsigned m_sizeOfOutputLayer;

    unsigned m_epochs;
    
    Sigmoid *m_sigmoid;
    
    
    friend class Test::MultilayerPerceptronNetworkTest;
  };
  
}
  
#endif
