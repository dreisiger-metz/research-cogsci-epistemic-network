//  ==========================================================================80
// Filename          : $RCSfile: FeatureVector.cpp,v $
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
// Purpose           : This file implements the feature vector class
//
// Linker options    : 
//
// Revision history  : 
//  ==========================================================================80
#include "FeatureVector.hpp"

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
FeatureVector::FeatureVector(string label, unsigned size) {
  this->label = label;

  for (m_i = 0; m_i < size; m_i++)
    this->representation.push_back(0.0);
}




//  ==========================================================================80
/// Loads the feature vector into the input layer
///
/// This method loads the feature vector into the input layer.
///
/// @param inputLayer   the network's input layer
///
/// @returns the length of the representation
//  ==========================================================================80
unsigned FeatureVector::loadRepresentation(vector<Perceptron*> &inputLayer) throw (Exception::IncompatibleVectorSize) {
  if (inputLayer.size() == representation.size())
    for (m_i = 0; m_i < representation.size(); m_i++)
      inputLayer[m_i]->outputValue = representation[m_i];
  else {
    Exception::IncompatibleVectorSize e(label);
    throw e;
  }

  return m_i;
}




//  ==========================================================================80
/// Saves the input layer's pattern of activation into feature vector
///
/// This method updates the feature vector based upon the input layer's
///  pattern of activation.
///
/// @param inputLayer   the network's input layer
///
/// @returns the length of the representation
//  ==========================================================================80
unsigned FeatureVector::saveRepresentation(std::vector<Perceptron*> &inputLayer) throw (Exception::IncompatibleVectorSize) {
  if (inputLayer.size() == representation.size())
    for (m_i = 0; m_i < representation.size(); m_i++)
      this->representation[m_i] = inputLayer[m_i]->outputValue;
  else {
    Exception::IncompatibleVectorSize e(label);
    throw e;
  }

  return m_i;
}




//  ==========================================================================80
/// Sets the output layer's error signal
///
/// This method sets the output layer's error signal based upon the feature
/// vector.
///
/// @param outputLayer   the network's output layer
///
/// @returns the length of the representation
//  ==========================================================================80
unsigned FeatureVector::setErrorSignals(vector<Perceptron*> &outputLayer) throw (Exception::IncompatibleVectorSize) {
  if (outputLayer.size() == representation.size())
    for (m_i = 0; m_i < representation.size(); m_i++)
      outputLayer[m_i]->errorSignal = representation[m_i] - outputLayer[m_i]->outputValue;
  else {
    Exception::IncompatibleVectorSize e(label);
    throw e;
  }

  return m_i;
}




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
unsigned FeatureVector::save(FILE *ofp) {
  unsigned count = 0, size, temp_u;
  double temp_d;

  size = label.size() + 1;
  temp_u = size;
  Endian::swap_4((void *) &temp_u);
  count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);            // length of the label
  count += fwrite((void *) label.c_str(), size, 1, ofp);                // the label itself

  size = representation.size();
  temp_u = size;
  Endian::swap_4((void *) &temp_u);
  count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);            // the size of the representation
  for (m_i = 0; m_i < size; m_i++) {
    temp_d = representation[m_i];
    Endian::swap_8((void *) &temp_d);
    count += fwrite((void *) &temp_d, sizeof(temp_d), 1, ofp);          // representation[m_i]
  }
  
  return count;
}




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
unsigned FeatureVector::load(FILE *ifp) {
  char *temp_s;
  unsigned count = 0, size;
  double temp_d;

  count += fread((void *) &size, sizeof(size), 1, ifp);                 // length of the label
  Endian::swap_4((void *) &size);
  temp_s = new char[size];
  count += fread((void *) temp_s, size, 1, ifp);                        // the label itself
  label = temp_s;
  delete[] temp_s;

  count += fread((void *) &size, sizeof(size), 1, ifp);                 // the size of the representation
  Endian::swap_4((void *) &size);
  representation.clear();
  for (m_i = 0; m_i < size; m_i++) {
    count += fread((void *) &temp_d, sizeof(double), 1, ifp);           // representation[m_i]
    Endian::swap_8((void *) &temp_d);
    representation.push_back(temp_d);
  }

  return count;
}




//  ==========================================================================80
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
//  ==========================================================================80
unsigned FeatureVector::print(FILE *ofp, const char *prefix) {
  unsigned i, count = 0;

  count += fprintf(ofp, "%s%20s    ", prefix, label.c_str());
  for (i = 0; i < representation.size() - 1; i++)
    count += fprintf(ofp, "%1.12g  ", representation[i]);
  count += fprintf(ofp, "%1.12g\n", representation[i]);

  return count;
}

