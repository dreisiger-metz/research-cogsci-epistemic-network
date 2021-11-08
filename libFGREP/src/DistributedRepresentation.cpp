//  ==========================================================================80
// Filename          : $RCSfile: DistributedRepresentation.cpp,v $
// Version           : $Revision: 1.3 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 15-Jan-2009
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/13 13:01:35 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This file implements the distributed representations
//                     used by FGREP
//
// Linker options    : 
//
// Revision history  : 
//  ==========================================================================80
#include "DistributedRepresentation.hpp"

using namespace std;
using namespace ANN;
using namespace Utilities;




//  ==========================================================================80
/// The constructor
///
/// This constructor creates an instance of the distributed representation
/// base.
///
/// @param label  the term's identifying label
/// @param size   the dimensionality of the distributed representation
//  ==========================================================================80
DistributedRepresentationBase::DistributedRepresentationBase(std::string label, unsigned size) : FeatureVector(label, size) { 
  for (m_i = 0; m_i < representation.size(); m_i++)
    deltaRepresentation.push_back(0.0);

  fixed = false;
  height = 0;
  
  m_usageCounter = 0;
  m_averageReconstructionErrorSquared = 0.0;
  m_cumulativeAverageReconstructionErrorSquared = 0.0;
}




//  ==========================================================================80
/// Loads the distributed representation into the input layer
///
/// This method loads the distributed representation into the input layer.
///
/// @param inputLayer   the network's input layer
/// @param offset       the term's offset from the first neuron in the
///                     input layer
///
/// @returns the length of the representation
//  ==========================================================================80
unsigned DistributedRepresentationBase::loadRepresentation(vector<Perceptron*> &inputLayer, unsigned offset) throw (Exception::IncompatibleVectorSize) {
  if (representation.size() + offset <= inputLayer.size())
    for (m_i = 0; m_i < representation.size(); m_i++)
      inputLayer[offset + m_i]->outputValue = representation[m_i];
  else {
    Exception::IncompatibleVectorSize e(label);
    throw e;
  }

  return m_i;
}




//  ==========================================================================80
/// Saves the distributed representation from the input layer
///
/// This method updates the term's distributed representation based upon
/// the input layer's pattern of activation.
///
/// @param inputLayer   the network's input layer
/// @param offset       the term's offset from the first neuron in the
///                     input layer
///
/// @returns the length of the representation
//  ==========================================================================80
unsigned DistributedRepresentationBase::saveRepresentation(std::vector<Perceptron*> &inputLayer, unsigned offset) throw (Exception::IncompatibleVectorSize) {
  if (!fixed) {
    if (representation.size() + offset <= inputLayer.size())
      for (m_i = 0; m_i < representation.size(); m_i++)
        representation[m_i] = inputLayer[offset + m_i]->outputValue;
    else {
      Exception::IncompatibleVectorSize e(label);
      throw e;
    }
  }
  
  return representation.size();
}




//  ==========================================================================80
/// Prints the distributed representation's state
///
/// This method prints the state of the distributed representation to the
/// specified output file stream
///
/// @param ofp      the input file stream
/// @param prefix   the constant C-style string that will be prefixed
///                 to each line of output; this allows for arbitrary 
///                 levels of indenting
/// @param verbose  a flag which, if true, also prints out the term's average
///                 error squared
///
/// @returns the number of bytes written to the output stream
//  ==========================================================================80
unsigned DistributedRepresentationBase::print(FILE *ofp, const char *prefix, bool verbose) {
  unsigned i, count = 0;

  count += fprintf(ofp, "%s%20s    ", prefix, label.c_str());
  for (i = 0; i < representation.size() - 1; i++)
    count += fprintf(ofp, "%1.12lg  ", representation[i]);
  if (verbose)
    count += fprintf(ofp, "%1.12lg  %1.12lg\n", representation[i], m_cumulativeAverageReconstructionErrorSquared / m_usageCounter);
  else
    count += fprintf(ofp, "%1.12lg\n", representation[i]);

  return count;
}




//  ==========================================================================80
/// Sets the output layer's error signal
///
/// This method sets the output layer's error signal based upon our 
/// distributed representation.
///
/// @param outputLayer    the network's output layer
/// @param offset         the term's offset from the first neuron in the
///                       input layer
///
/// @returns the length of the sentence's representation
//  ==========================================================================80
unsigned DistributedRepresentationBase::setErrorSignals(vector<Perceptron*> &outputLayer, unsigned offset) throw (Exception::IncompatibleVectorSize) {
  if (representation.size() + offset <= outputLayer.size())
    for (m_i = 0; m_i < representation.size(); m_i++)
      outputLayer[offset + m_i]->errorSignal = representation[m_i] - outputLayer[offset + m_i]->outputValue;
  else {
    Exception::IncompatibleVectorSize e(label);
    throw e;
  }

  return m_i;
}




//  ==========================================================================80
/// Saves the distributed representation's state to an output file stream
///
/// This method saves the state of the distributed representation to the
/// specified output file stream
///
/// @param ofp   the output file stream
///
/// @returns the number of bytes written to the output stream
//  ==========================================================================80
unsigned DistributedRepresentationBase::save(FILE *ofp) {
  unsigned count = FeatureVector::save(ofp), temp_u;

  temp_u = fixed;
  Endian::swap_4((void *) &temp_u);
  count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);              // fixed
  temp_u = height;
  Endian::swap_4((void *) &temp_u);
  count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);              // height
  
  return (count);
}




//  ==========================================================================80
/// Loads the distributed representation's state from an input file stream
///
/// This method loads the state of the distributed representation from the
/// specified input file stream
///
/// @param ifp   the input file stream
///
/// @returns the number of bytes read from the input stream
//  ==========================================================================80
unsigned DistributedRepresentationBase::load(FILE *ifp) {
  unsigned count = FeatureVector::load(ifp);
  
  count += fread((void *) &fixed, sizeof(fixed), 1, ifp);               // fixed
  Endian::swap_4((void *) &fixed);
  count += fread((void *) &height, sizeof(height), 1, ifp);             // height
  Endian::swap_4((void *) &height);
  
  return count;
}




//  ==========================================================================80
/// Updates the representation's average error
///
/// This method calculates the representation's reconstruction error based
/// upon the output layer's pattern of activation, and updates the average
/// error accumulators.  It should be called once for every active sentence in
/// which the term appears
///
/// @param outputLayer   the network's output layer
/// @param offset        the term's offset from the first neuron in the
///                      input layer
///
/// @returns the length of the sentence's representation
//  ==========================================================================80
unsigned DistributedRepresentationBase::updateAverageReconstructionErrorSquared(vector<Perceptron*> &outputLayer, unsigned offset) throw (Exception::IncompatibleVectorSize) {
  if (representation.size() + offset <= outputLayer.size())
    for (m_averageReconstructionErrorSquared = 0.0, m_i = 0; m_i < representation.size(); m_i++) {
      m_d = representation[m_i] - outputLayer[offset + m_i]->outputValue;
      m_averageReconstructionErrorSquared += m_d * m_d;
    }
  else {
    Exception::IncompatibleVectorSize e(label);
    throw e;
  }

  m_cumulativeAverageReconstructionErrorSquared += m_averageReconstructionErrorSquared / (2 * m_i);
  m_usageCounter++;

  return m_i;
}




//  ==========================================================================80
/// Update deltaRepresentation using the input layer's pattern of activation
///
/// This method updates the distributed representation's [deltaRepresentation]
/// based upon the input layer's pattern of activation.
///
/// @param inputLayer   the network's input layer
///
/// @returns the length of the representation
//  ==========================================================================80
unsigned DistributedRepresentationBase::updateDeltaRepresentation(vector<Perceptron*> &inputLayer, unsigned offset) throw (Exception::IncompatibleVectorSize) {
  if (representation.size() + offset <= inputLayer.size())
    for (m_i = 0; m_i < representation.size(); m_i++)
      deltaRepresentation[m_i] += (inputLayer[offset + m_i]->outputValue - representation[m_i]);

  else {
    Exception::IncompatibleVectorSize e(label);
    throw e;
  }

  return m_i;
}




//  ==========================================================================80
/// Update the representation based upon [deltaRepresentation]
///
/// This method updates the distributed representation's [deltaRepresentation]
/// based upon the input layer's pattern of activation.
///
/// @param inputLayer   the network's input layer
///
/// @returns the length of the representation
//  ==========================================================================80
unsigned DistributedRepresentationBase::updateRepresentationBatch() {
  if ((m_usageCounter > 0) && !fixed)
    for (m_i = 0; m_i < representation.size(); m_i++) {
      representation[m_i] += (deltaRepresentation[m_i] / m_usageCounter);
      deltaRepresentation[m_i] = 0.0;
    }

  m_usageCounter = 0;

  return representation.size();
}






//  ==========================================================================80
/// The constructor
///
/// This constructor creates a distributed representation and places it,
/// randomly, in [0.0, 1.0]^size space.
///
/// @param label  the term's identifying label
/// @param size   the dimensionality of the distributed representation
//  ==========================================================================80
DistributedTermRepresentation::DistributedTermRepresentation(string label, unsigned size) : DistributedRepresentationBase(label, size) {
  for (m_i = 0; m_i < size; m_i++)
    representation[m_i] = Perceptron::Uniform.unifrnd();
  
  fixed = false;
  height = 0;
}






//  ==========================================================================80
/// The constructor
///
/// This constructor creates a distributed representation and places it at the
/// point specified by { value, value, ..., value }.
///
/// @param label  the term's identifying label
/// @param size   the dimensionality of the distributed representation
/// @param value  the offset, from the origin, for each dimension
//  ==========================================================================80
DistributedTermRepresentation::DistributedTermRepresentation(string label, unsigned size, double value) : DistributedRepresentationBase(label, size) {
  for (m_i = 0; m_i < size; m_i++)
    representation[m_i] = value;

  fixed = true;
}


//  ==========================================================================80
/// The constructor
///
/// This constructor creates a distributed representation and places it at
/// the point specified by { value, value, ..., value }.
///
/// @param label  the term's identifying label
/// @param size   the dimensionality of the distributed representation
/// @param value  the recurring subset of the representation's coordinates
//  ==========================================================================80
DistributedTermRepresentation::DistributedTermRepresentation(string label, unsigned size, vector<double> value) : DistributedRepresentationBase(label, size) {
  for (m_i = 0; m_i < size; m_i++)
    representation[m_i] = value[(m_i < value.size())?m_i:value.size()-1];

  fixed = true;
}






//  ==========================================================================80
/// The constructor
///
/// This constructor creates a distributed representation and places it at
/// the point specified by { value, value, ..., value }.
///
/// @param label     the sentence's identifying label
/// @param size      the dimensionality of the distributed representation
/// @param term      pointers (of type DistributedRepresentationBase) to
///                      each of the sentence's terms
/// @param weight    the weight, or strength, of the sentence
//  ==========================================================================80
DistributedSentenceRepresentation::DistributedSentenceRepresentation(string label, unsigned size, vector<DistributedRepresentationBase*> term, double weight) : DistributedRepresentationBase(label, size) {
  unsigned i;
  
  this->term = term;
  this->weight = weight;

  iterations = 0;
  numberOfTerms = term.size();
  m_combinedAverageReconstructionErrorsSquared = 0.0;

  // and work out this sentence's height
  for (i = 0; i < term.size(); i++)
    this->height = (term[i]->height > height)?term[i]->height:height;
  this->height++;
}




//  ==========================================================================80
/// Loads the distributed representation into the input layer
///
/// This method loads the term's distributed representation into the input
/// layer.
///
/// @param inputLayer   the network's input layer
///
/// @returns the length of the sentence's representation
//  ==========================================================================80
unsigned DistributedSentenceRepresentation::loadTermRepresentations(vector<Perceptron*> &inputLayer) {
  for (m_offset = 0, m_i = 0; m_i < numberOfTerms; m_i++)
    m_offset += term[m_i]->loadRepresentation(inputLayer, m_offset);

  return m_offset;
}


//  ==========================================================================80
/// Saves the distributed representation from the input layer
///
/// This method updates the term's distributed representation based upon
/// the input layer's pattern of activation.
///
/// @param inputLayer   the network's input layer
///
/// @returns the length of the sentence's representation
//  ==========================================================================80
unsigned DistributedSentenceRepresentation::saveTermRepresentations(vector<Perceptron*> &inputLayer) {
  for (m_offset = 0, m_i = 0; m_i < numberOfTerms; m_i++)
    m_offset += term[m_i]->saveRepresentation(inputLayer, m_offset);

  return m_offset;
}




//  ==========================================================================80
/// Saves the distributed representation's state to an output file stream
///
/// This method saves the state of the distributed representation to the
/// specified output file stream
///
/// @param ofp   the output file stream
///
/// @returns the number of bytes written to the output stream
//  ==========================================================================80
unsigned DistributedSentenceRepresentation::save(FILE *ofp) {
  unsigned count, i, j, temp_u;
  double temp_d;

  count = DistributedRepresentationBase::save(ofp);
  
  for (i = 0; i < numberOfTerms; i++) {
    j = term[i]->label.length() + 1;
    temp_u = j;
    Endian::swap_4((void *) &temp_u);
    count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);            // length of the term's label
    count += fwrite((void *) term[i]->label.c_str(), j, 1, ofp);          // the term's label
  }
  temp_d = weight;
  Endian::swap_8((void *) &temp_d);
  count += fwrite((void *) &temp_d, sizeof(temp_d), 1, ofp);              // the sentence's weight
  
  return count;
}




//  ==========================================================================80
/// Loads the distributed representation's state from an input file stream
///
/// This method loads the state of the distributed representation from the
/// specified input file stream
///
/// @param ifp   the input file stream
///
/// @returns the number of bytes read from the input stream
//  ==========================================================================80
unsigned DistributedSentenceRepresentation::load(FILE *ifp) {
  char *temp_s;
  unsigned count, i, size;
  
  count = DistributedRepresentationBase::load(ifp);
  
  for (i = 0; i < numberOfTerms; i++) {
    count += fread((void *) &size, sizeof(size), 1, ifp);                   // length of the term's label
    Endian::swap_4((void *) &size);
    temp_s = new char[size];
    count += fread((void *) temp_s, size, 1, ifp);                          // the term's label
    term[i]->label = temp_s;
    delete[] temp_s;
  }
  
  count += fread((void *) &weight, sizeof(weight), 1, ifp);                 // the sentence's weight
  Endian::swap_8((void *) &weight);
  
  return count;
}




//  ==========================================================================80
/// Sets the output layer's error signal
///
/// This method sets the output layer's error signal based upon our 
/// distributed representation.
///
/// @param outputLayer   the network's output layer
///
/// @returns the length of the sentence's representation
//  ==========================================================================80
unsigned DistributedSentenceRepresentation::setTermErrorSignals(vector<Perceptron*> &outputLayer) {
  for (m_offset = 0, m_i = 0; m_i < numberOfTerms; m_i++)
    m_offset += term[m_i]->setErrorSignals(outputLayer, m_offset);
  
  return m_offset;
}


//  ==========================================================================80
/// Updates the sentence and representation's average error
///
/// This method calculates the sentence and representation's reconstruction
/// error based upon the output layer's pattern of activation, and updates
/// the average error accumulators.
///
/// @param outputLayer   the network's output layer
///
/// @returns the length of the sentence's representation
//  ==========================================================================80
unsigned DistributedSentenceRepresentation::updateAverageErrorsSquared(vector<Perceptron*> &outputLayer) {
  for (m_combinedAverageReconstructionErrorsSquared = 0.0, m_offset = 0, m_i = 0; m_i < numberOfTerms; m_i++) {
    m_offset += term[m_i]->updateAverageReconstructionErrorSquared(outputLayer, m_offset);
    m_combinedAverageReconstructionErrorsSquared += term[m_i]->averageReconstructionErrorSquared();
  }

  return m_offset;
}


//  ==========================================================================80
/// Reset the sentence and representation's average errors
//  ==========================================================================80
void DistributedSentenceRepresentation::resetAverageErrorsSquared() {
  for (m_i = 0; m_i < numberOfTerms; m_i++)
    term[m_i]->resetAverageReconstructionErrorSquared();

  m_usageCounter = 0;
  m_averageReconstructionErrorSquared = 0.0;
  m_cumulativeAverageReconstructionErrorSquared = 0.0;
  m_combinedAverageReconstructionErrorsSquared = 0.0;
}


//  ==========================================================================80
/// Update deltaRepresentation using the input layer's pattern of activation
///
/// This method updates the distributed representations' [deltaRepresentation]
/// based upon the input layer's pattern of activation.
///
/// @param inputLayer   the network's input layer
///
/// @returns the length of the sentence's representation
//  ==========================================================================80
unsigned DistributedSentenceRepresentation::updateTermDeltaRepresentations(vector<Perceptron*> &inputLayer) {
  for (m_offset = 0, m_i = 0; m_i < numberOfTerms; m_i++)
    m_offset += term[m_i]->updateDeltaRepresentation(inputLayer, m_offset);

  return m_offset;
}


//  ==========================================================================80
/// Batch-update the distributed representation
///
/// This method updates the distributed representation based upon the delta-
/// representations that have been accumulated by the updateDeltaRepresen-
/// tation method;  it also clears the accumulators and the usage counter.
///
/// @returns the length of the representation
//  ==========================================================================80
unsigned DistributedSentenceRepresentation::updateTermRepresentationsBatch() {
  for (m_offset = 0, m_i = 0; m_i < numberOfTerms; m_i++)
    m_offset += term[m_i]->updateRepresentationBatch();

  return m_offset;
}


//  ======================================================================80
/// Prints the distributed representation's state
///
/// This method prints the state of the distributed representation to the
/// specified output file stream
///
/// @param ofp      the input file stream
/// @param prefix   the constant C-style string that will be prefixed
///                 to each line of output; this allows for arbitrary 
///                 levels of indenting
/// @param verbose  a flag which, if true, causes additional information
///                 (e.g. errors and weights) to be printed out
///
/// @returns the number of bytes written to the output stream
//  ======================================================================80
unsigned DistributedSentenceRepresentation::print(FILE *ofp, const char *prefix, bool verbose) {
  m_offset = fprintf(ofp, "%s%s  ", prefix, label.c_str());
  for (m_i = 0; m_i < numberOfTerms - 1; m_i++)
    m_offset += fprintf(ofp, "%s ", term[m_i]->label.c_str());
  m_offset += fprintf(ofp, "%s  (weight=%lf, iterations=%d, avg-error-sq=%1.12lg)\n", term[m_i]->label.c_str(), weight, iterations, combinedAverageReconstructionErrorsSquared());

  return m_offset;
}


