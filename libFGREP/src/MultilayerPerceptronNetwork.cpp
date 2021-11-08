#include "MultilayerPerceptronNetwork.hpp"

using namespace std;
using namespace ANN;




//  ==========================================================================80
/// The constructor
///
/// This constructor creates a multi-layer network of perceptrons.
///
/// @param label          the network's identifying label
/// @param learningRate   the perceptrons' initial learning rate
/// @param momentum       the perceptrons' initial momentum
/// @param layers         the number of layers in the network
/// @param ...            the number of neurons in each layer of the network
//  ==========================================================================80
MultilayerPerceptronNetwork::MultilayerPerceptronNetwork(string label, double learningRate, double momentum, unsigned layers, ...) {
  unsigned i, j, k, layerSize;
  char perceptronLabel[16];
  va_list ap;
  vector<Perceptron*> layer;
  
  // Initialise the network's parameters,
  this->label = label;
  this->learningRate = learningRate;
  this->momentum = momentum;
  
  m_epochs = 0;
  m_sigmoid = new Sigmoid();
  
  // create the neurons and the layers,
  va_start(ap, layers);
  for (i = 0; i < layers; i++) {
    layerSize = va_arg(ap, unsigned);
    for (j = 0, layer.clear(); j < layerSize; j++) {
      sprintf(perceptronLabel, "p-%d-%d", i, j);
      layer.push_back(new Perceptron(perceptronLabel, this->learningRate, this->momentum, 0.0, m_sigmoid));
    }
    
    perceptron.push_back(layer);
  }
  va_end(ap);
  
  // initialise the other network-related variables,
  m_numberOfLayers = layers;
  m_sizeOfInputLayer = perceptron[0].size();
  m_sizeOfOutputLayer = perceptron[layers - 1].size();

  // and connect the layers together
  for (i = 1; i < layers; i++)
    for (j = 0; j < perceptron[i].size(); j++)
      for (k = 0; k < perceptron[i - 1].size(); k++)
	perceptron[i][j]->addInput(perceptron[i - 1][k]);
}




//  ==========================================================================80
/// The destructor
//  ==========================================================================80
MultilayerPerceptronNetwork::~MultilayerPerceptronNetwork() {
  unsigned i, j;
  
  for (i = 0; i < perceptron.size(); i++)
    for (j = 0; j < perceptron[i].size(); j++)
      delete perceptron[i][j];
  
  delete m_sigmoid;
}




//  ==========================================================================80
/// Adds a training datum
///
/// This method adds another input--target pattern to the network's training set
///
/// @param inputPattern    the input pattern
/// @param targetPattern   and its corresponding target pattern
///
/// @returns the (new) total number of input--target pairs
//  ==========================================================================80
unsigned MultilayerPerceptronNetwork::addTrainingDatum(FeatureVector inputPattern, FeatureVector targetPattern) throw (Exception::IncompatibleVectorSize) {
  // Make sure that the size of the input and target patterns match the number
  // of elements in the input and output layers
  if (inputPattern.size() != m_sizeOfInputLayer) {
    Exception::IncompatibleVectorSize e("inputPattern's size != size of the input layer");
    throw e;
  }
  if (targetPattern.size() != m_sizeOfOutputLayer) {
    Exception::IncompatibleVectorSize e("targetPattern's size != size of the output layer");
    throw e;
  }
  
  // They are, so add the vectors
  this->inputPattern.push_back(inputPattern);
  this->targetPattern.push_back(targetPattern);
  
  return this->inputPattern.size();
}




//  ==========================================================================80
/// Processes an input pattern
///
/// This method loads the specified pattern into the input layer, and propagates
/// the activation forward through to the output layer.
///
/// @param input   the input pattern
///
/// @returns the pattern of activation at the results at the output layer
//  ==========================================================================80
FeatureVector MultilayerPerceptronNetwork::processInput(FeatureVector &input) throw (Exception::IncompatibleVectorSize) {
  unsigned i, j;
  FeatureVector output("output for " + input.label, m_sizeOfOutputLayer);

  if (input.size() == m_sizeOfInputLayer) {
    // Load the input pattern into the input layer,
    input.loadRepresentation(perceptron[0]);
      
    // propagate the activation forward,
    for (i = 0; i < m_numberOfLayers; i++)
      for (j = 0; j < perceptron[i].size(); j++)
	perceptron[i][j]->updateActivation();

    // and copy the value of the output layer into [output]
    output.saveRepresentation(perceptron[m_numberOfLayers - 1]);

    return output;

  } else {
    Exception::IncompatibleVectorSize e("input size does not match the size of the input layer");
    throw e;
  }
}




//  ==========================================================================80
/// Trains the network
///
/// This method iterates over all training samples and implements the feed-
/// forward, error backpropagation learning algorithm.
///
/// @param errorThreshold   the minimum average squared error level at which 
///                         the training stops
///
/// @returns the average error squared of the output for this epoch
//  ==========================================================================80
double MultilayerPerceptronNetwork::trainNetwork(double errorThreshold) {
  unsigned i, j, k, size = inputPattern.size(), outputLayer = m_numberOfLayers - 1;
  double averageErrorSquared = errorThreshold + 1.0;
  
  while (averageErrorSquared > errorThreshold) {
    averageErrorSquared = 0.0;
    
    // For each input--target training sample:
    for (i = 0; i < size; i++) {
      // load the input pattern into the input layer,
      inputPattern[i].loadRepresentation(perceptron[0]);
      
      // propagate the activation forward (notice how we skip the input layer),
      for (j = 1; j < m_numberOfLayers; j++)
        for (k = 0; k < perceptron[j].size(); k++)
          perceptron[j][k]->updateActivation();
      
      // work out the error signal at the output layer,
      targetPattern[i].setErrorSignals(perceptron[outputLayer]);
      
      // propagate the errors back through the network
      averageErrorSquared = 0.0;
      for (j = outputLayer; j != (unsigned) -1; j--)
        for (k = 0; k < perceptron[j].size(); k++)
          perceptron[j][k]->updateWeights();
      
      // And work out the total squared error of the output neurons for this
      // input--target sample
      for (j = 0; j < m_sizeOfOutputLayer; j++)
        averageErrorSquared += (perceptron[outputLayer][j]->errorSignal * perceptron[outputLayer][j]->errorSignal);
    }
    
    averageErrorSquared = averageErrorSquared / (2 * size);
    m_epochs++;
  }
  
  // And return the final average squared error
  return averageErrorSquared;
}




//  ==========================================================================80
/// Trains the network
///
/// This method iterates over all training samples and implements the feed-
/// forward, error backpropagation learning algorithm.
///
/// @param iterations   the desired number of training epochs
///
/// @returns the average error squared of the output for this epoch
//  ==========================================================================80
double MultilayerPerceptronNetwork::trainNetwork(unsigned iterations) {
  unsigned i, j, k, l, size = inputPattern.size(), outputLayer = m_numberOfLayers - 1, finalIteration = iterations - 1;
  double averageErrorSquared = 0.0;
  
  for (i = 0; i < iterations; i++) {
    // For each input--target training sample:
    for (j = 0; j < size; j++) {
      // load the input pattern into the input layer,
      inputPattern[j].loadRepresentation(perceptron[0]);
      
      // propagate the activation forward (notice how we skip the input layer),
      for (k = 1; k < m_numberOfLayers; k++)
        for (l = 0; l < perceptron[k].size(); l++)
          perceptron[k][l]->updateActivation();
      
      // work out the error signal at the output layer,
      targetPattern[j].setErrorSignals(perceptron[outputLayer]);
      
      // and propagate the errors back through the network
      for (k = outputLayer; k != (unsigned) -1; k--)
        for (l = 0; l < perceptron[k].size(); l++)
          perceptron[k][l]->updateWeights();
      
      // And, if this our last iteration, work out the total squared error of
      // the output neurons for our set of input--target samples
      if (i == finalIteration)
        for (k = 0; k < m_sizeOfOutputLayer; k++)
          averageErrorSquared += (perceptron[outputLayer][k]->errorSignal * perceptron[outputLayer][k]->errorSignal);
    }
  }
  m_epochs += i;
  
  // And return the squared error, averaged over all training data, as of
  // the final epoch
  return averageErrorSquared / (2 * size);
}




//  ==========================================================================80
/// Saves the network's state to an output file stream
///
/// This method saves the state of the network to the specified output file
/// stream.
///
/// @param ofp   the output file stream
///
/// @returns the number of bytes written to the output stream
//  ==========================================================================80
/*
unsigned MultilayerPerceptronNetwork::save(FILE *ofp) {
  unsigned i, j, k,count = 0, size;

  // Save our label,
  size = label.length() + 1;
  count += fwrite((void *) &size, sizeof(size), 1, ofp);
  count += fwrite((void *) label.c_str(), size, 1, ofp);

  // our scalar parameters,
  count += fwrite((void *) &learningRate, sizeof(learningRate), 1, ofp);
  count += fwrite((void *) &momentum, sizeof(momentum), 1, ofp);
  count += fwrite((void *) &m_numberOfLayers, sizeof(m_numberOfLayers), 1, ofp);
  count += fwrite((void *) &m_epochs, sizeof(m_epochs), 1, ofp);

  // our network of perceptrons,
  for (i = 0; i < m_numberOfLayers; i++) {
    size = perceptron[i].size();   // size of this layer
    count += fwrite((void *) &size, sizeof(size), 1, ofp);
    for (j = 0; j < size; j++)
      count += perceptron[i][j]->save(ofp);
  }

  // their weights,
  for (i = 0; i < m_numberOfLayers; i++)
    for (j = 0; j < perceptron[i].size(); j++) {
      size = perceptron[i][j]->input.size();
      count += fwrite((void *) &size, sizeof(size), 1, ofp);  // number of input weights for this neuron
      for (k = 0; k < perceptron[i][j]->input.size(); k++)
	count += fwrite((void *) &perceptron[i][j]->input[k]->weight, sizeof(double), 1, ofp);
    }

  // and our input--target patterns.
  size = inputPattern.size();
  count += fwrite((void *) &size, sizeof(size), 1, ofp);
  for (i = 0; i < size; i++) {
    count += inputPattern[i].save(ofp);
    count += targetPattern[i].save(ofp);
  }

  return count;
}
*/




//  ==========================================================================80
/// Loads the network's state from an input file stream
///
/// This method loads the state of the network from the specified input file
/// stream.
///
/// @param ifp   the input file stream
///
/// @returns the number of bytes read from the input stream
//  ==========================================================================80
/*
unsigned MultilayerPerceptronNetwork::load(FILE *ifp) {
  char *temp_s;
  unsigned i, j, k, count = 0, size;
  Perceptron *temp_p;
  FeatureVector temp_f("", m_sizeOfInputLayer);
  vector<Perceptron*> layer;

  // Load our label,
  count += fread((void *) &size, sizeof(size), 1, ifp);
  temp_s = new char[size];
  count += fread((void *) temp_s, size, 1, ifp);
  label = temp_s;
  delete[] temp_s;

  // our scalar parameters
  count += fread((void *) &learningRate, sizeof(learningRate), 1, ifp);
  count += fread((void *) &momentum, sizeof(momentum), 1, ifp);
  count += fread((void *) &m_numberOfLayers, sizeof(m_numberOfLayers), 1, ifp);
  count += fread((void *) &m_epochs, sizeof(m_epochs), 1, ifp);

  // our network of perceptrons
  for (i = 0; i < m_numberOfLayers; i++)   // delete our existing perceptrons
    for (j = 0; j < perceptron[i].size(); j++)
      delete perceptron[i][j];
  perceptron.clear();

  for (i = 0; i < m_numberOfLayers; i++) {
    layer.clear();
    count += fread((void *) &size, sizeof(size), 1, ifp);
    for (j = 0; j < size; j++) {
      temp_p = new Perceptron("", learningRate, momentum, 0.0, m_sigmoid);
      count += temp_p->load(ifp);
      layer.push_back(temp_p);
    }
    perceptron.push_back(layer);
  }

  // their weights,
  for (i = 0; i < m_numberOfLayers; i++)
    for (j = 0; j < perceptron[i].size(); j++) {
      count += fread((void *) &size, sizeof(size), 1, ifp);  // number of input weights for this neuron
      for (k = 0; k < perceptron[i][j]->input.size(); k++)
	count += fwrite((void *) &perceptron[i][j]->input[k]->weight, sizeof(double), 1, ifp);
    }

  // and our input--target pairs
  count += fread((void *) &size, sizeof(size), 1, ifp);
  inputPattern.clear();
  targetPattern.clear();
  for (i = 0; i < size; i++) {
    count += temp_f.load(ifp);
    inputPattern.push_back(temp_f);
    count += temp_f.load(ifp);
    targetPattern.push_back(temp_f);
  }
  
  return count;
}
*/




//  ==========================================================================80
/// Prints the network's information
///
/// This method prints the details of the multilayer perceptron network to the
/// specified output stream.
///
/// @param ostream the output stream to which the information will be
///                printed
/// @param prefix  the constant C-style string that will be prefixed
///                to each line of output; this allows for arbitrary 
///                levels of indenting
///
/// @retval the number of characters printed
//  ==========================================================================80
unsigned MultilayerPerceptronNetwork::print(FILE *ofp, const char *prefix) {
  char *prefix2 = new char[strlen(prefix) + 3];
  unsigned i, j, count;
  
  sprintf(prefix2, "%s  ", prefix);
  
  count = fprintf(ofp, "%s%s (a %d-layer perceptron network with ", prefix, label.c_str(), m_numberOfLayers);
  for (i = 0; i < m_numberOfLayers - 1; i++)
    count += fprintf(ofp, "%d : ", (int) perceptron[i].size());
  count += fprintf(ofp, "%d neurons)\n", (int) perceptron[i].size());
  
  for (i = 0; i < m_numberOfLayers; i++)
    for (j = 0; j < perceptron[i].size(); j++)
      perceptron[i][j]->print(ofp, prefix2);
  
  delete[] prefix2;
  
  return count;
}
