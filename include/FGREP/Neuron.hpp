// ============================================================================
// Filename          : $RCSfile: Neuron.hpp,v $
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
// Purpose           : This header file defines the interfaces of the Neuron
//                     base class
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libFGREP__Neuron_hpp
#define libFGREP__Neuron_hpp

#include <math.h>
#include <stdio.h>
#include <string>
#include <vector>

#include "ActivationFunction.hpp"
#include "Statistics.hpp"


namespace ANN {

  
  // Static helper objects
  //static Utilities::Statistics::Uniform Uniform;
  //static Utilities::Statistics::Normal  Normal;
  
  
  // Forward declarations
  class Link;
  class Neuron;

  namespace Test {
    class NeuronTest;
    class PerceptronTest;
  }
  


  
  //  =========================================================================
  /// @class    Neuron
  /// @brief    The base class of all other neurons
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  =========================================================================
  class Neuron {
  public:
    /// An enumeration of the different types of neurons
    enum NeuronType {
      BaseNeuronType,    /// a basic neuron
      FixedSourceType,   /// a fixed-source neuron
      PerceptronType     /// a perceptron
    };
    

    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates an instance of a generic neuron.
    ///
    /// @param label   the neuron's identifying label
    //  ======================================================================80
    Neuron(std::string label);


    //  ======================================================================80
    /// The destructor
    //  ======================================================================80
    virtual ~Neuron();


    //  ======================================================================80
    /// Makes another neuron an input
    ///
    /// This method adds the specified neuron as an input.
    ///
    /// @param inputNeuron    a pointer to the neuron being added
    /// @param weight         the initial weight of the link
    ///
    /// @returns the (new) total number of inputs
    //  ======================================================================80
    virtual unsigned addInput(Neuron *input, double weight);


    //  ======================================================================80
    /// Saves the neuron's state to an output file stream
    ///
    /// This method saves the state of the neuron to the specified output file
    /// stream.
    ///
    /// @param ofp   the output file stream
    ///
    /// @returns the number of bytes written to the output stream
    //  ======================================================================80
    //virtual unsigned save(FILE *ofp);


    //  ======================================================================80
    /// Loads the neuron's state from an input file stream
    ///
    /// This method loads the state of the neuron from the specified input file
    /// stream.
    ///
    /// @param ifp   the input file stream
    ///
    /// @returns the number of bytes read from the input stream
    //  ======================================================================80
    //virtual unsigned load(FILE *ifp);

    
    //  ======================================================================80
    /// Prints the neuron's information
    ///
    /// This method prints the details of the neuron to the specified output
    /// stream.
    ///
    /// @param ostream the output stream to which the information will be
    ///                printed
    /// @param prefix  the constant C-style string that will be prefixed
    ///                to each line of output; this allows for arbitrary 
    ///                levels of indenting
    ///
    /// @retval the number of characters printed
    //  ======================================================================80
    virtual unsigned print(FILE *ofp, const char *prefix);
    
    
    std::string label;

    std::vector<Link*> input;
    std::vector<Link*> output;
    
    double outputValue;
    double localField;
    double localGradient;
    
    NeuronType type;
    
    friend class Perceptron;
    friend class Test::NeuronTest;
    friend class Test::PerceptronTest;
  };
  
  
  


  
  //  =========================================================================
  /// @class    FixedSource
  /// @brief    A class of neuron that can be used to bias other neurons
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  =========================================================================
  class FixedSource : public Neuron {
  public:
    FixedSource(std::string label, double activation)  : Neuron(label) { outputValue = activation; type = Neuron::FixedSourceType; }
    FixedSource(std::string label, double &activation) : Neuron(label) { outputValue = activation; type = Neuron::FixedSourceType; }
  };






  //  =========================================================================
  /// @class    Link
  /// @brief    The class used to represent weighted links between neurons
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  =========================================================================
  class Link {
  public:
    Link(Neuron *left, Neuron *right, double weight);
    
    Neuron *left;    /// the neuron closest to the input layer
    Neuron *right;   /// the neuron closest to the output layer
    double  weight;  /// the weight of the link
  };

}

#endif
