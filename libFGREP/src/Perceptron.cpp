//  ==========================================================================80
// Filename          : $RCSfile: Perceptron.cpp,v $
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
// Purpose           : This file implements the Perceptron neuron
//
// Linker options    : 
//
// Revision history  : 
//  ==========================================================================80
#include "Perceptron.hpp"

using namespace std;
using namespace ANN;


/// This fixed-activation 'neuron' is used to as the bias source
FixedSource Perceptron::BiasSource("bias-source", 1.0);

Utilities::Statistics::Uniform Perceptron::Uniform;
Utilities::Statistics::Normal  Perceptron::Normal;




//  ==========================================================================80
/// The constructor
///
/// This constructor creates an instance of a perceptron neuron.
///
/// @param label           the perceptron's identifying label
/// @param learningRate    the perceptron's initial learning rate
/// @param momentum        the perceptron's initial momentum
/// @param bias            the perceptron's bias
/// @param actFn           a pointer to an object that implements the percep-
///                        tron's activation function and its derivative
//  ==========================================================================80
Perceptron::Perceptron(string label, double &learningRate, double &momentum, double bias, ActivationFunction *actFn) : Neuron(label), learningRate(learningRate), momentum(momentum) {
  errorSignal = 0.0;
  m_actFn = actFn;
  m_numberOfTrainingPoints = 0;
  
  input.push_back(new Link(&BiasSource, this, bias));
  previousInputWeight.push_back(0.0);
  deltaInputWeight.push_back(0.0);

  type = Neuron::PerceptronType;
}




//  ==========================================================================80
/// Makes another neuron an input
///
/// This method adds the specified neuron as an input.
///
/// @param inputNeuron    a pointer to the neuron being added
/// @param weight         the initial weight of the link
///
/// @returns the (new) total number of inputs
//  ==========================================================================80
unsigned Perceptron::addInput(Neuron *inputNeuron, double weight) {
  // add a zero entry to our previous input and delta weight vector
  previousInputWeight.push_back(0.0);
  deltaInputWeight.push_back(0.0);
  
  // and call our superclass' method
  return Neuron::addInput(inputNeuron, weight);
}




//  ==========================================================================80
/// Makes another neuron an input
///
/// This method adds the specified neuron as an input with a random inter-
/// neuron link weight.
///
/// @param inputNeuron    a pointer to the neuron being added
///
/// @returns the (new) total number of inputs
//  ==========================================================================80
unsigned Perceptron::addInput(Neuron *inputNeuron) {
  // need to confirm that these values of [a] and [b] will give the right 
  // standard deviation (was (-5.0, 5.0), (-1.0, 1.0) from Miikkulainen, p54)
  return addInput(inputNeuron, Perceptron::Uniform.unifrnd(-1.0, 1.0));
}




//  ==========================================================================80
/// Recalculates the neuron's output values
///
/// This method recalculates the neuron's local field and output values.
///
/// @returns the new output value
//  ==========================================================================80
double Perceptron::updateActivation() {
  for (localField = 0.0, m_i = 0; m_i < input.size(); m_i++)
    localField += input[m_i]->weight * input[m_i]->left->outputValue;
  outputValue = m_actFn->value(localField);
  
  return outputValue;
}




//  ==========================================================================80
/// Updates the neuron's input weights
///
/// This method updates the neuron's input weights according to the standard
/// backpropagation learning rule.
///
/// @returns the square of the neuron's error signal
//  ==========================================================================80
double Perceptron::updateWeights() {
  // Work out our error signal (if we're not an output neuron) and local gradient,
  if (output.size() > 0)
    for (errorSignal = 0.0, m_i = 0; m_i < output.size(); m_i++)
      errorSignal += output[m_i]->weight * output[m_i]->right->localGradient;
  localGradient = m_actFn->derivative(localField) * errorSignal;
  
  // and use the gradient to work out our new weights.  Note that we need to use
  // of [m_d] to temporarily store the deltaWeights due to our dependence on the
  // previous /and/ current input weights.
  for (m_i = 0; m_i < input.size(); m_i++) {
    // Q: Is this correct, or should it be \rho w(n-1) + (1 - \rho) (\nu \delta y^{l-1})?
    m_d = momentum * previousInputWeight[m_i] + learningRate * localGradient * input[m_i]->left->outputValue;
    previousInputWeight[m_i] = input[m_i]->weight;
    input[m_i]->weight += m_d;
  }

  // Note that this doesn't need to be halved as we're not /averaging/ the 
  // error --- we're a single perceptron after all...
  return errorSignal * errorSignal;
}




//  ==========================================================================80
/// Accumulate the change in weight change due to the current training datum
///
/// This method updates the neuron's delta-weight, according to the batch form
/// of the backpropagation learning rule, for the current input--output training
/// datum.
///
/// @note After updateDeltaWeight() has been called for each training datum,
///     updateWeight
//  ==========================================================================80
void Perceptron::updateDeltaWeights() {
  // Work out our error signal (if we're not an output neuron) and local 
  // gradient (here m_d is our temporary error signal),
  if (output.size() > 0)
    for (errorSignal = 0.0, m_i = 0; m_i < output.size(); m_i++)
      errorSignal += output[m_i]->weight * output[m_i]->right->localGradient;
  localGradient = m_actFn->derivative(localField) * errorSignal;
  
  // and use the gradient to work out our new delta-weight.  Q: Is this correct,
  // or should it be \rho w(n-1) + (1 - \rho) (\nu \delta y^{l-1})?
  for (m_i = 0; m_i < input.size(); m_i++)
    deltaInputWeight[m_i] += momentum * previousInputWeight[m_i] + learningRate * localGradient * input[m_i]->left->outputValue;

  // and update our accumulated error-squared and training point counters
  //errorSignal += m_d * m_d;
  m_numberOfTrainingPoints++;
}




//  ==========================================================================80
/// Updates the neuron's input weights based upon the accumulated delta-weights
///
/// This method updates the neuron's input weights according to the batch
/// form of backpropagation learning rule, and the delta-weights that have 
/// been accumulated by the updateDeltaWeights() method.
///
/// @returns the neuron's average error squared for the batch
//  ==========================================================================80
double Perceptron::updateWeightsBatch() {
  // Save our current input weights, calculate the new ones based upon our
  // accumulation of the delta-weights, and reset the latter vector.
  for (m_i = 0; m_i < input.size(); m_i++) {
    previousInputWeight[m_i] = input[m_i]->weight;
    input[m_i]->weight += deltaInputWeight[m_i] / m_numberOfTrainingPoints;
    deltaInputWeight[m_i] = 0.0;
  }

  // Note that this doesn't need to be halved as we're not /averaging/ the
  // error --- we're a single perceptron after all...
  errorSignal /= m_numberOfTrainingPoints;
  m_numberOfTrainingPoints = 0;

  return errorSignal;
}




//  ==========================================================================80
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
//  ==========================================================================80
unsigned Perceptron::print(FILE *ofp, const char *prefix) {
  unsigned i, count;
  
  count = fprintf(ofp, "%s%s:\n", prefix, label.c_str());
  count += fprintf(ofp, "%s  output = %1.12g\n", prefix, outputValue);
  count += fprintf(ofp, "%s  localField = %1.12g\n", prefix, localField);
  count += fprintf(ofp, "%s  localGradient = %1.12g\n", prefix, localGradient);
  count += fprintf(ofp, "%s  errorSignal = %1.12g\n", prefix, errorSignal);
  count += fprintf(ofp, "%s  learningRate = %1.12g\n", prefix, learningRate);
  count += fprintf(ofp, "%s  momentum = %1.12g\n", prefix, momentum);
  if (input.size() > 0) {
    count += fprintf(ofp, "%s  inputLabels = { ", prefix);
    for (i = 0; i < input.size() - 1; i++)
      count += fprintf(ofp, "%s, ", input[i]->left->label.c_str());
    count += fprintf(ofp, "%s }\n", input[i]->left->label.c_str());
    
    count += fprintf(ofp, "%s  inputWeights = { ", prefix);
    for (i = 0; i < input.size() - 1; i++)
      count += fprintf(ofp, "%1.12g, ", input[i]->weight);
    count += fprintf(ofp, "%1.12g }\n", input[i]->weight);
    
    count += fprintf(ofp, "%s  previousInputWeights = { ", prefix);
    for (i = 0; i < input.size() - 1; i++)
      count += fprintf(ofp, "%1.12g, ", previousInputWeight[i]);
    count += fprintf(ofp, "%1.12g }\n", previousInputWeight[i]);
  }
  return count;
}
