#include "Neuron.hpp"

using namespace std;
using namespace ANN;


//Utilities::Statistics::Uniform ANN::Uniform;
//Utilities::Statistics::Normal  ANN::Normal;


//  ==========================================================================80
/// The constructor
///
/// This constructor creates an instance of a generic neuron.
///
/// @param label   the neuron's identifying label
//  ==========================================================================80
Neuron::Neuron(string label) {
  this->label = label;
  
  outputValue = 0.0;
  localField = 0.0;
  localGradient = 0.0;

  type = Neuron::BaseNeuronType;
}




//  ==========================================================================80
/// The destructor
//  ==========================================================================80
Neuron::~Neuron() {
  // Only delete all of our /incoming/ links (the outgoing ones are owned by
  // the neurons in the next layer)
  for (unsigned i = 0; i < input.size(); i++)
    delete input[i];
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
unsigned Neuron::addInput(Neuron *inputNeuron, double weight) {
  Link *newLink = new Link(inputNeuron, this, weight);
  
  // Add the new link to [input] and the input neuron's [output] vector
  input.push_back(newLink);
  inputNeuron->output.push_back(newLink);
  
  return input.size();
}




//  ==========================================================================80
/// Saves the neuron's state to an output file stream
///
/// This method saves the state of the neuron to the specified output file
/// stream.
///
/// @param ofp   the output file stream
///
/// @returns the number of bytes written to the output stream
//  ==========================================================================80
/*
unsigned Neuron::save(FILE *ofp) {
  unsigned count = 0, size;

  // Save our label
  size = label.length() + 1;
  count += fwrite((void *) &size, sizeof(size), 1, ofp);
  count += fwrite((void *) label.c_str(), size, 1, ofp);
  
  // and our scalar parameters.
  count += fwrite((void *) &outputValue, sizeof(outputValue), 1, ofp);
  count += fwrite((void *) &localField, sizeof(localField), 1, ofp);
  count += fwrite((void *) &localGradient, sizeof(localGradient), 1, ofp);
  
  // We can't really save any of the information about our links as (a) we can't
  // assume that they'll exist when we're re-loaded, and (b) we don't know
  // where (in memory) they will be (reconstructing the network will have to
  // be the responsibility of a network-level class).

  return count;
}
*/




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
/*
unsigned Neuron::load(FILE *ifp) {
  char *temp_s;
  unsigned count = 0, size;

  // Load our label
  count += fread((void *) &size, sizeof(size), 1, ifp);
  temp_s = new char[size];
  count += fread((void *) temp_s, size, 1, ifp);
  label = temp_s;
  delete[] temp_s;

  // and our scalar parameters.
  count += fread((void *) &outputValue, sizeof(outputValue), 1, ifp);
  count += fread((void *) &localField, sizeof(localField), 1, ifp);
  count += fread((void *) &localGradient, sizeof(localGradient), 1, ifp);

  // As above, we can't re-create our links because we don't know if they
  // exist yet, and even if they do, we don't know where in memory they live;
  // given that we might be loading state over an existing neuron, what we
  // need to do is clear the input and output vectors...
  input.clear();
  output.clear();

  return count;
}
*/




//  ==========================================================================80
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
//  ==========================================================================80
unsigned Neuron::print(FILE *ofp, const char *prefix) {
  unsigned i, count;
  
  count = fprintf(ofp, "%s%s:\n", prefix, label.c_str());
  count += fprintf(ofp, "%s  output = %1.12g\n", prefix, outputValue);
  count += fprintf(ofp, "%s  localField = %1.12g\n", prefix, localField);
  count += fprintf(ofp, "%s  localGradient = %1.12g\n", prefix, localGradient);
  if (input.size() > 0) {
    count += fprintf(ofp, "%s  inputLabels = { ", prefix);
    for (i = 0; i < input.size() - 1; i++)
      count += fprintf(ofp, "%s, ", input[i]->left->label.c_str());
    count += fprintf(ofp, "%s }\n", input[i]->left->label.c_str());
    
    count += fprintf(ofp, "%s  inputWeights = { ", prefix);
    for (i = 0; i < input.size() - 1; i++)
      count += fprintf(ofp, "%1.12g, ", input[i]->weight);
    count += fprintf(ofp, "%1.12g }\n", input[i]->weight);
    
  }
  return count;
}






//  ======================================================================80
/// The constructor
///
/// This constructor creates a bi-directional link between two neurons
///
/// @param left     the neuron closest to the input layer
/// @param right    the neuron closest to the output layer
/// @param weight   the weight of the link
//  ======================================================================80
Link::Link(Neuron *left, Neuron *right, double weight) {
  this->left = left;
  this->right = right;
  this->weight = weight;
}
