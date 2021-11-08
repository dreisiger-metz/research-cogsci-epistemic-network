// ============================================================================
// Filename          : $RCSfile: Perceptron.hpp,v $
// Version           : $Revision: 1.2 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 15-Jan-2009
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 11:22:59 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines the interface of the Percep-
//                     ron neuron
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libFGREP__Perceptron_hpp
#define libFGREP__Perceptron_hpp

#include "Neuron.hpp"


namespace ANN {


  //  ========================================================================80
  /// @class    Perceptron
  /// @brief    A class that implements the basic, non-linear perceptron
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class Perceptron : public Neuron {
  public:
    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates an instance of a perceptron neuron.
    ///
    /// @param label           the perceptron's identifying label
    /// @param learningRate    the perceptron's initial learning rate
    /// @param momentum        the perceptron's initial momentum
    /// @param bias            the perceptron's bias
    /// @param actFn           a pointer to an object that implements the per-
    ///                        ceptron's activation function and its derivative
    //  ======================================================================80
    Perceptron(std::string label, double &learningRate, double &momentum, double bias, ActivationFunction *actFn);


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
    unsigned addInput(Neuron *input, double weight);


    //  ======================================================================80
    /// Makes another neuron an input
    ///
    /// This method adds the specified neuron as an input with a random inter-
    /// neuron link weight.
    ///
    /// @param inputNeuron    a pointer to the neuron being added
    ///
    /// @returns the (new) total number of inputs
    //  ======================================================================80
    unsigned addInput(Neuron *input);

    
    //  ======================================================================80
    /// Recalculates the neuron's output values
    ///
    /// This method recalculates the neuron's local field and output values.
    ///
    /// @returns the new output value
    //  ======================================================================80
    double updateActivation();


    //  ======================================================================80
    /// Updates the neuron's input weights
    ///
    /// This method updates the neuron's input weights according to the standard
    /// backpropagation learning rule.
    ///
    /// @returns the square of the neuron's error signal
    //  ======================================================================80
    double updateWeights();


    //  ======================================================================80
    /// Accumulate the change in weight change due to the current training datum
    ///
    /// This method updates the neuron's delta-weight, according to the batch
    /// form of the backpropagation learning rule, for the current input--output
    /// training datum.
    ///
    /// @note After updateDeltaWeight() has been called for each training datum,
    ///     updateWeightBatch must be called to update the weights proper.
    //  ======================================================================80
    void updateDeltaWeights();


    //  ======================================================================80
    /// Updates the neuron's input weights based upon the accumulated delta-weights
    ///
    /// This method updates the neuron's input weights according to the batch 
    /// form of backpropagation learning rule, and the delta-weights that have
    /// been accumulated by the updateDeltaWeights() method.
    ///
    /// @returns the neuron's average error squared for the batch
    //  ======================================================================80
    double updateWeightsBatch();


    //  ======================================================================80
    /// Prints the perceptron's information
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
    

    std::vector<double> previousInputWeight;
    std::vector<double> deltaInputWeight;

    double &learningRate;
    double &momentum;
    double errorSignal;

    static Utilities::Statistics::Uniform Uniform;
    static Utilities::Statistics::Normal  Normal;


  protected:
    unsigned m_i, m_numberOfTrainingPoints;
    double   m_d;

    ActivationFunction *m_actFn;

    static FixedSource BiasSource;
    
    
    friend class Test::PerceptronTest;
  };
  
}

#endif
