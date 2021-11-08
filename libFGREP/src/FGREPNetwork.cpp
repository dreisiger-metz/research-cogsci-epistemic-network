// ============================================================================
// Filename          : $RCSfile: FGREPNetwork.cpp,v $
// Version           : $Revision: 1.3 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 15-Jan-2009
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/13 12:58:26 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This file implements the FGREP network
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#include "FGREPNetwork.hpp"

using namespace std;
using namespace ANN;
using namespace Utilities;


//  ==========================================================================80
/// The constructor
///
/// This constructor creates a instance of an FGREP network of perceptrons.
///
/// @param label          the network's identifying label
/// @param learningRate   the perceptrons' initial learning rate
/// @param momentum       the perceptrons' initial momentum
/// @param representationSize   the size of the distributed representations
/// @param layers         the number of layers in the network
/// @param ...                  the number of neurons in each layer of the 
///                             network (the input and output layer sizes 
///                             must be a multiple of [representationSize])
///
/// @note The constructor was essentially lifted from MultilayerPerceptron
//  ==========================================================================80
FGREPNetwork::FGREPNetwork(string label, double learningRate, double momentum, unsigned representationSize, unsigned layers, ...) {
  unsigned i, j, k, layerSize;
  char perceptronLabel[24];
  va_list ap;
  vector<Perceptron*> layer;
  
  
  // Initialise the network's parameters,
  this->label = label;
  this->learningRate = learningRate;
  this->momentum = momentum;
  
  m_running = false;
  m_maximumHeight = 0;
  m_epochs = 0;
  m_sigmoid = new Sigmoid();
  
  
  // create the neurons and the layers,
  va_start(ap, layers);
  for (i = 0; i < layers; i++) {
    layerSize = va_arg(ap, unsigned);
    for (j = 0, layer.clear(); j < layerSize; j++) {
      sprintf(perceptronLabel, "perceptron-%d-%d", i, j);
      layer.push_back(new Perceptron(perceptronLabel, this->learningRate, this->momentum, 0.0, m_sigmoid));
    }
    
    perceptron.push_back(layer);
  }
  va_end(ap);
  
  // initialise the other network-related variables,
  m_sizeOfRepresentations = representationSize;
  m_numberOfLayers = layers;
  m_sizeOfInputLayer = perceptron[0].size();
  m_sizeOfOutputLayer = perceptron[layers - 1].size();
  
  // and connect the layers together
  for (i = 1; i < layers; i++)
    for (j = 0; j < perceptron[i].size(); j++)
      for (k = 0; k < perceptron[i - 1].size(); k++)
        perceptron[i][j]->addInput(perceptron[i - 1][k]);
  
  
  // Finally, add the NULL and DontCare terms to termRepresentationMap
  termRepresentationMap["NULL"] = new DistributedTermRepresentation("NULL", representationSize, 0.0);
  termRepresentationMap["DontCare"] = new DistributedTermRepresentation("DontCare", representationSize, 0.5);
}




//  ==========================================================================80
/// The destructor
//  ==========================================================================80
FGREPNetwork::~FGREPNetwork() {
  unsigned i, j;
  multimap<unsigned, DistributedSentenceRepresentation*>::iterator k;
  map<string, DistributedRepresentationBase*>::iterator l;
  
  // Nothing special here --- just delete the perceptrons,
  for (i = 0; i < perceptron.size(); i++)
    for (j = 0; j < perceptron[i].size(); j++)
      delete perceptron[i][j];
  
  // the sentences,
  for (k = heightSentenceMap.begin(); k != heightSentenceMap.end(); k++)
    delete k->second;
  
  // the terms from the term--representation map
  for (l = termRepresentationMap.begin(); l != termRepresentationMap.end(); l++)
    delete l->second;
  
  // and, finally, our activation function.
  delete m_sigmoid;
}




//  ==========================================================================80
/// Declares a new term
///
/// This method adds the named term to the term--distributed representation
/// map and associates it with a random representation
///
/// @param term    the term being declared
///
/// @returns the (new) total number of terms
//  ==========================================================================80
unsigned FGREPNetwork::declareTerm(string term) throw (Exception::DuplicateTerm) {
  // If this is a new term, create a new DistributedTermRepresentation and add it
  // to the term--representation map;  otherwise, throw an exception.
  if (termRepresentationMap.find(term) == termRepresentationMap.end())
    termRepresentationMap[term] = new DistributedTermRepresentation(term, m_sizeOfRepresentations);
  else {
    Exception::DuplicateTerm e(term);
    throw e;
  }
  
  return termRepresentationMap.size();
}




//  ==========================================================================80
/// Adds a pre-constructed, but distinct, term or sentence
///
/// This method adds the named term or sentence to the term--distributed 
/// representation map
///
/// @param representation   the distributed representation to be added
///
/// @returns the (new) total number of terms
//  ==========================================================================80
unsigned FGREPNetwork::addTerm(DistributedRepresentationBase *representation) throw (Exception::DuplicateTerm, Exception::IncompatibleVectorSize) {
  // If this is a new term (and it contains the right number of elements),
  // add it to the term--representation map; otherwise, throw an exception.
  if (termRepresentationMap.find(representation->label) == termRepresentationMap.end()) {
    if (representation->size() == m_sizeOfRepresentations)
      termRepresentationMap[representation->label] = representation;
    else {
      Exception::IncompatibleVectorSize e(representation->label);
      throw e;
    } 
  } else {
    Exception::DuplicateTerm e(representation->label);
    throw e;
  }
  
  return termRepresentationMap.size();
}




//  ==========================================================================80
/// Adds a sentence to the training set
///
/// This method adds the specified sequence of terms to the training set.
///
/// @param episode    the 'episode' to which this sentence belongs
/// @param relation   the relation being expressed
/// @param arg1       the relation's first argument
/// @param arg2       the relation's first argument
/// @param arg3       the relation's first argument
///
/// @returns the (new) total number of input--target pairs
//  ==========================================================================80
unsigned FGREPNetwork::addSentence(string label, string sentence, double weight) throw (Exception::UnknownTerm) {
  unsigned oldPos = 0, newPos = sentence.find_first_of(" ", oldPos);
  string term;
  vector<DistributedRepresentationBase*> representation;
  map<string, DistributedRepresentationBase*>::iterator i;
  DistributedSentenceRepresentation *dsr;
  
  // Go through [sentence], and for each term in it, try to retrieve its cor-
  // responding DistributedRepresentationBase* from the term--representation
  // map;  assuming we could do that for each term in the sentence, create
  // a new DistributedSentenceRepresentation, and add it to [this->sentence].
  do {
    term = sentence.substr(oldPos, newPos - oldPos);
    if ((i = termRepresentationMap.find(term)) != termRepresentationMap.end())
      representation.push_back(i->second);
    else {
      Exception::UnknownTerm e(term);
      throw e;
    }
    
    oldPos = newPos + 1;
    newPos = sentence.find_first_of(" ", oldPos);
  } while (oldPos != 0);
  
  
  // If (label == ""), construct one from its constituent terms' labels (and
  // yes, I'm reusing [oldPos] as a generic counter variable --- my bad)
  if (label == "") {
    label = "sentence--";
    for (oldPos = 0; oldPos < representation.size() - 1; oldPos++)
      label += representation[oldPos]->label + "-";
    label += representation[oldPos]->label;
  }
  
  
  // Create the new sentence representation and add it to this->sentence and 
  // this->sentenceHeightSentenceMap; if the size of the sentence's representation
  // matches the size of the terms', then add it as a term too.
  dsr = new DistributedSentenceRepresentation(label, perceptron[(m_numberOfLayers + 1)/2].size(), representation, weight);
  sentenceRepresentationMap[label] = dsr;
  heightSentenceMap.insert(pair<unsigned, DistributedSentenceRepresentation*>(dsr->height, dsr));
  if (dsr->height > m_maximumHeight)
    m_maximumHeight = dsr->height;
  
  if (dsr->representation.size() == m_sizeOfRepresentations)
    this->addTerm((DistributedRepresentationBase *) dsr);
  
  return heightSentenceMap.size();
}




//  ==========================================================================80
/// Trains the network
///
/// This method iterates over all training samples and implements the 
/// incremental feed-forward, error backpropagation learning algorithm.
///
/// @param iterations   the desired number of training epochs
///
/// @returns the average error squared of the output for all training sentences
//  ==========================================================================80
double FGREPNetwork::trainNetworkByNumberOfIterationsIncremental(unsigned iterations) {
  /*
   unsigned i, j, k, l, size = sentence.size(), outputLayer = m_numberOfLayers - 1, finalIteration = iterations - 1;
   double averageErrorSquared = 0.0;
   vector<unsigned> previousIterations;
   vector<DistributedSentenceRepresentation*> preRandomisedSentence, randomisedSentence;
   
   // Initialise our randomising vectors and save the sentences' current
   // iteration counters
   for (j = 0; j < size; j++) {
   randomisedSentence.push_back(NULL);
   preRandomisedSentence.push_back(NULL);
   previousIterations.push_back(sentence[j]->iterations);
   sentence[j]->iterations = 0;
   }
   
   
   for (m_running = true, i = 0; m_running && (i < iterations); i++) {
   for (j = 0; preRandomisedSentence[j] = sentence[j], j < size; j++);
   for (j = 0; j < size; ) {
   k = rand() % size;
   if (preRandomisedSentence[k] != NULL) {
   randomisedSentence[j++] = preRandomisedSentence[k];
   preRandomisedSentence[k] = NULL;
   }
   }
   
   // For each sentence:
   for (j = 0; j < size; j++) {
   // (a) load the input pattern into the input layer,
   randomisedSentence[j]->loadTermRepresentations(perceptron[0]);
   
   // (b) propagate the activation forward, skipping the input layer,
   for (k = 1; k < m_numberOfLayers; k++)
   for (l = 0; l < perceptron[k].size(); l++)
   perceptron[k][l]->updateActivation();
   
   // (c) work out the error signal at the output layer,
   randomisedSentence[j]->setTermErrorSignals(perceptron[outputLayer]);
   
   // (d) propagate the errors back through the network, (but only if this
   //     sentence is due for another iteration)
   if (randomisedSentence[j]->iterations <= randomisedSentence[j]->weight * i) {
   for (k = outputLayer; k != (unsigned) -1; k--)
   for (l = 0; l < perceptron[k].size(); l++)
   perceptron[k][l]->updateWeights();
   
   // (e) update the activation (and thus the distributed representation)
   //     at the input layer as per Miikkulainen (1993, p49), and
   for (k = 0; k < m_sizeOfInputLayer; k++) {
   perceptron[0][k]->outputValue = perceptron[0][k]->outputValue + learningRate * perceptron[0][k]->localGradient;
   if (perceptron[0][k]->outputValue < 0.0)
   perceptron[0][k]->outputValue = 0.0;
   else if (perceptron[0][k]->outputValue > 1.0)
   perceptron[0][k]->outputValue = 1.0;
   }
   
   // (f) use this new pattern to update the distributed representations.
   randomisedSentence[j]->saveTermRepresentations(perceptron[0]);
   
   randomisedSentence[j]->iterations++;
   } // if (randomisedSentence[j]->iterations <= randomisedSentence[j]->weight * i)
   
   // Also, if this our last iteration, work out the total squared error of
   // the output neurons for our set of input--target samples
   if (i == finalIteration)
   for (k = 0; k < m_sizeOfOutputLayer; k++)
   averageErrorSquared += (perceptron[outputLayer][k]->errorSignal * perceptron[outputLayer][k]->errorSignal);
   } // for (j = 0; j < size; j++)
   
   } // for (i = 0; m_running && (i < iterations); i++)
   
   
   // Finish up
   m_epochs += i;
   for (j = 0; j < size; j++)
   sentence[j]->iterations += previousIterations[j];
   
   // And return the final average squared error for all of the training data.
   return averageErrorSquared / (2 * m_sizeOfOutputLayer * size);
   */
  return 0;
}




//  ==========================================================================80
/// Trains the network
///
/// This method iterates over all training samples and implements the 
/// incremental feed-forward, error backpropagation learning algorithm.
///
/// @param errorThreshold   the minimum average squared error level at 
///                         which the training stops
/// @param maximumEpochs    the maximum number of epochs after which the 
///                         training stops;  if zero, training continues
///                         until the error threshold is reached
///
/// @returns the average error squared of the output for all training sentences
//  ==========================================================================80
double FGREPNetwork::trainNetworkByAverageErrorSquaredIncremental(double errorThreshold, unsigned maximumEpochs) {
  /*
   unsigned j, k, l, size = sentence.size(), outputLayer = m_numberOfLayers - 1, epochs = 0;
   double averageErrorSquared = errorThreshold + 1.0;
   vector<unsigned> previousIterations;
   vector<DistributedSentenceRepresentation*> preRandomisedSentence, randomisedSentence;
   
   // Initialise our randomising vectors and save the sentences' current
   // iteration counters
   for (j = 0; j < size; j++) {
   randomisedSentence.push_back(NULL);
   preRandomisedSentence.push_back(NULL);
   previousIterations.push_back(sentence[j]->iterations);
   sentence[j]->iterations = 0;
   }
   
   
   m_running = true;
   while (m_running && ((maximumEpochs == 0) || (epochs < maximumEpochs)) && (averageErrorSquared > errorThreshold)) {
   averageErrorSquared = 0.0;
   for (j = 0; preRandomisedSentence[j] = sentence[j], j < size; j++);
   for (j = 0; j < size; ) {
   k = rand() % size;
   if (preRandomisedSentence[k] != NULL) {
   randomisedSentence[j++] = preRandomisedSentence[k];
   preRandomisedSentence[k] = NULL;
   }
   }
   
   // For each sentence:
   for (j = 0; j < size; j++) {
   // (a) load the input pattern into the input layer
   randomisedSentence[j]->loadTermRepresentations(perceptron[0]);
   
   // (b) propagate the activation forward, skipping the input layer,
   for (k = 1; k < m_numberOfLayers; k++)
   for (l = 0; l < perceptron[k].size(); l++)
   perceptron[k][l]->updateActivation();
   
   // (c) work out the error signal at the output layer,
   randomisedSentence[j]->setTermErrorSignals(perceptron[outputLayer]);
   
   // (d) propagate the errors back through the network (but only if this
   //     sentence is due for another iteration)
   if (randomisedSentence[j]->iterations <= randomisedSentence[j]->weight * epochs) {
   for (k = outputLayer; k != (unsigned) -1; k--)
   for (l = 0; l < perceptron[k].size(); l++)
   perceptron[k][l]->updateWeights();
   
   // (e) update the activation (and thus the distributed representation)
   //     at the input layer as per Miikkulainen (1993, p49), and
   for (k = 0; k < m_sizeOfInputLayer; k++) {
   perceptron[0][k]->outputValue = perceptron[0][k]->outputValue + learningRate * perceptron[0][k]->localGradient;
   if (perceptron[0][k]->outputValue < 0.0)
   perceptron[0][k]->outputValue = 0.0;
   else if (perceptron[0][k]->outputValue > 1.0)
   perceptron[0][k]->outputValue = 1.0;
   }
   
   // (f) use this new pattern to update the distributed representations,
   //     and increment its iteration counter.
   randomisedSentence[j]->saveTermRepresentations(perceptron[0]);
   randomisedSentence[j]->iterations++;
   }
   
   // Finally, work out the total squared error of the output neurons for
   // our set of input--target samples,
   for (k = 0; k < m_sizeOfOutputLayer; k++)
   averageErrorSquared += (perceptron[outputLayer][k]->errorSignal * perceptron[outputLayer][k]->errorSignal);
   } // for (j = 0; j < size; j++)
   
   // and then work out the average
   averageErrorSquared /= (2 * m_sizeOfOutputLayer * size);
   epochs++;
   } // while (m_running && ((maximumEpochs == 0) || (epochs < maximumEpochs)) && ...
   
   
   // Finish up
   m_epochs += epochs;
   for (j = 0; j < size; j++)
   sentence[j]->iterations += previousIterations[j];
   
   // And return the final average squared error for all of the training data.
   return averageErrorSquared;
   */
  return 0;
}




//  ==========================================================================80
/// Trains the network
///
/// This method iterates over all training samples and implements the 
/// batch feed-forward, error backpropagation learning algorithm.
///
/// @param iterations   the desired number of training epochs
///
/// @returns the average error squared of the output for this epoch
//  ==========================================================================80
double FGREPNetwork::trainNetworkByNumberOfIterationsBatch(unsigned iterations) {
  unsigned i, j, k, l, outputLayer = m_numberOfLayers - 1, finalIteration = iterations - 1;
  double averageErrorSquared = 0.0;
  vector<unsigned> previousIterations;
  multimap<unsigned, DistributedSentenceRepresentation*>::iterator sentence;
  
  // Save the sentences' current iteration counters.
  for (sentence = heightSentenceMap.begin(); sentence != heightSentenceMap.end(); sentence++) {
    previousIterations.push_back(sentence->second->iterations);
    sentence->second->iterations = 0;
  }
  
  
  for (m_running = true, i = 0; m_running && (i < iterations); i++) {
    // For each sentence, in order of increasing height,
    for (j = 1; j <= m_maximumHeight; j++)
      for (sentence = heightSentenceMap.find(j); sentence != heightSentenceMap.upper_bound(j); sentence++) {
        // (a) load the input pattern into the input layer,
        sentence->second->loadTermRepresentations(perceptron[0]);
        
        // (b) propagate the activation forward, skipping the input layer,
        for (k = 1; k < m_numberOfLayers; k++)
          for (l = 0; l < perceptron[k].size(); l++)
            perceptron[k][l]->updateActivation();
        
        // (c) work out the error signal at the output layer,
        sentence->second->setTermErrorSignals(perceptron[outputLayer]);
        
        // (d) propagate the errors back through the network and accumulate the delta-
        //     weights (but only if this sentence is due for another iteration)
        if (sentence->second->iterations <= sentence->second->weight * i) {
          for (k = outputLayer; k != (unsigned) -1; k--)
            for (l = 0; l < perceptron[k].size(); l++)
              perceptron[k][l]->updateDeltaWeights();
          
          // (e) update the activation (and thus the distributed representation)
          //     at the input layer as per Miikkulainen (1993, p49), and
          for (k = 0; k < m_sizeOfInputLayer; k++) {
            perceptron[0][k]->outputValue = perceptron[0][k]->outputValue + learningRate * perceptron[0][k]->localGradient;
            if (perceptron[0][k]->outputValue < 0.0)
              perceptron[0][k]->outputValue = 0.0;
            else if (perceptron[0][k]->outputValue > 1.0)
              perceptron[0][k]->outputValue = 1.0;
          }
          
          // (f) and accumulate the changes to update the distributed representations.
          sentence->second->updateTermDeltaRepresentations(perceptron[0]);
          
          sentence->second->iterations++;
        } // if (sentence->second->iterations <= sentence->second->weight * i)
        
        // Also, if this our last iteration, work out the total squared error of
        // the output neurons for our set of input--target samples
        if (i == finalIteration)
          for (k = 0; k < m_sizeOfOutputLayer; k++)
            averageErrorSquared += (perceptron[outputLayer][k]->errorSignal * perceptron[outputLayer][k]->errorSignal);
        
      } // for (sentence = heightSentenceMap.find(j); sentence != heightSentenceMap.upper_bound(j); sentence++)
    
    
    // Having gone through all of the sentences and accumulated the delta
    // weights and delta representations, we now need to update them.
    for (k = 0; k < m_numberOfLayers; k++)
      for (l = 0; l < perceptron[k].size(); l++)
        perceptron[k][l]->updateWeightsBatch();
    
    for (sentence = heightSentenceMap.find(j); sentence != heightSentenceMap.upper_bound(j); sentence++)
      sentence->second->updateTermRepresentationsBatch();
  } // for (m_running = true, i = 0; m_running && (i < iterations); i++)
  
  
  // Finish up
  m_epochs += i;
  for (j = 0, sentence = heightSentenceMap.begin(); sentence != heightSentenceMap.end(); sentence++)
    sentence->second->iterations += previousIterations[j++];
  
  // And return the final average squared error for all of the training data.
  return averageErrorSquared / (2 * m_sizeOfOutputLayer * heightSentenceMap.size());
}




//  ==========================================================================80
/// Trains the network
///
/// This method iterates over all training samples and implements the 
/// batch feed-forward, error backpropagation learning algorithm.
///
/// @param errorThreshold   the minimum average squared error level at 
///                         which the training stops
/// @param maximumEpochs    the maximum number of epochs after which the 
///                         training stops;  if zero, training continues
///                         until the error threshold is reached
///
/// @returns the average error squared of the output for all training sentences
//  ==========================================================================80
double FGREPNetwork::trainNetworkByAverageErrorSquaredBatch(double errorThreshold, unsigned maximumEpochs) {
  unsigned j, k, l, outputLayer = m_numberOfLayers - 1, epochs = 0;
  double averageErrorSquared = errorThreshold + 1.0;
  vector<unsigned> previousIterations;
  multimap<unsigned, DistributedSentenceRepresentation*>::iterator sentence;
  
  // Save the sentences' current iteration counters
  for (sentence = heightSentenceMap.begin(); sentence != heightSentenceMap.end(); sentence++) {
    previousIterations.push_back(sentence->second->iterations);
    sentence->second->iterations = 0;
  }
  
  
  m_running = true;
  while (m_running && ((maximumEpochs == 0) || (epochs < maximumEpochs)) && (averageErrorSquared > errorThreshold)) {
    averageErrorSquared = 0.0;
    
    // For each sentence, in order of increasing height,
    for (j = 1; j <= m_maximumHeight; j++)
      for (sentence = heightSentenceMap.find(j); sentence != heightSentenceMap.upper_bound(j); sentence++) {
        
        // (a) load the input pattern into the input layer
        sentence->second->loadTermRepresentations(perceptron[0]);

        // (b) propagate the activation forward, skipping the input layer,
        for (k = 1; k < m_numberOfLayers; k++)
          for (l = 0; l < perceptron[k].size(); l++)
            perceptron[k][l]->updateActivation();
        
        // (c) work out the error signal at the output layer,
        sentence->second->setTermErrorSignals(perceptron[outputLayer]);
        
        // (d) propagate the errors back through the network (but only if this
        //     sentence is due for another iteration)
        if (sentence->second->iterations <= sentence->second->weight * epochs) {
          for (k = outputLayer; k != (unsigned) -1; k--)
            for (l = 0; l < perceptron[k].size(); l++)
              perceptron[k][l]->updateDeltaWeights();
          
          // (e) update the activation (and thus the distributed representation)
          //     at the input layer as per Miikkulainen (1993, p49), and
          for (k = 0; k < m_sizeOfInputLayer; k++) {
            perceptron[0][k]->outputValue = perceptron[0][k]->outputValue + learningRate * perceptron[0][k]->localGradient;
            if (perceptron[0][k]->outputValue < 0.0)
              perceptron[0][k]->outputValue = 0.0;
            else if (perceptron[0][k]->outputValue > 1.0)
              perceptron[0][k]->outputValue = 1.0;
          }
          
          // (f) use this new pattern to update the distributed representations,
          //     and increment its iteration counter.
          sentence->second->updateTermDeltaRepresentations(perceptron[0]);
          
          sentence->second->iterations++;
        } // if (sentence->second->iterations <= sentence->second->weight * epochs)
        
        // Finally, work out the total squared error of the output neurons for
        // our set of input--target samples,
        for (k = 0; k < m_sizeOfOutputLayer; k++)
          averageErrorSquared += (perceptron[outputLayer][k]->errorSignal * perceptron[outputLayer][k]->errorSignal);
        
      } // for (sentence = heightSentenceMap.find(j); sentence != heightSentenceMap.upper_bound(j); sentence++)
    
    // Having gone through all of the sentences and accumulated the delta
    // weights and delta representations, we now need to update them,
    for (k = 0; k < m_numberOfLayers; k++)
      for (l = 0; l < perceptron[k].size(); l++)
        perceptron[k][l]->updateWeightsBatch();
    
    for (sentence = heightSentenceMap.find(j); sentence != heightSentenceMap.upper_bound(j); sentence++)
      sentence->second->updateTermRepresentationsBatch();
    
    // and then work out the average error squared
    averageErrorSquared /= (2 * m_sizeOfOutputLayer * heightSentenceMap.size());
    epochs++;
    
  } // while (m_running && ((maximumEpochs == 0) || (epochs < maximumEpochs)) && ...
  
  
  // Finish up
  m_epochs += epochs;
  for (j = 0, sentence = heightSentenceMap.begin(); sentence != heightSentenceMap.end(); sentence++)
    sentence->second->iterations += previousIterations[j++];
  
  // And return the final average squared error for all of the training data.
  return averageErrorSquared;
}




//  ==========================================================================80
/// Processes the sentences and distributed representations
///
/// This method iterates over all training samples, propagating the activa-
/// tion through the network, and calculates all of the average per-term
/// and per-sentence errors squared.
///
/// @returns the average error squared of the output for all sentences
//  ==========================================================================80
double FGREPNetwork::processRepresentations() {
  unsigned k, l, outputLayer = m_numberOfLayers - 1;
  double averageErrorSquared = 0.0;
  multimap<unsigned, DistributedSentenceRepresentation*>::iterator sentence;
  
  // For each sentence:
  for (sentence = heightSentenceMap.begin(); sentence != heightSentenceMap.end(); sentence++)
    sentence->second->resetAverageErrorsSquared();
  
  for (sentence = heightSentenceMap.begin(); sentence != heightSentenceMap.end(); sentence++) {
    // (a) load the input pattern into the input layer,
    sentence->second->loadTermRepresentations(perceptron[0]);
    
    // (b) propagate the activation forward, skipping the input layer,
    for (k = 1; k < m_numberOfLayers; k++)
      for (l = 0; l < perceptron[k].size(); l++)
        perceptron[k][l]->updateActivation();
    
    // (c) and update the sentence and representations' average errors
    sentence->second->updateAverageErrorsSquared(perceptron[outputLayer]);
    // PRD: this might need changing after terms and sentences are merged!
    averageErrorSquared += sentence->second->combinedAverageReconstructionErrorsSquared();  // already scaled by 2 * m_sizeOfOutputLayer
  }
  
  // And return the final average squared error for all of the training data.
  if (heightSentenceMap.size() > 0)
    return averageErrorSquared / heightSentenceMap.size();
  else
    return 0.0;
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
unsigned FGREPNetwork::save(FILE *ofp) {
  unsigned i, j, k, count = 0, size, termsPerSentence = m_sizeOfInputLayer / m_sizeOfRepresentations, temp_u;
  double temp_d;
  map<string, DistributedRepresentationBase*>::iterator term;
  
  // ---------------------------------------------------------------------
  // First, we save the network's label, its parameters, and its geometry;
  size = label.length() + 1;
  temp_u = size;
  Endian::swap_4((void *) &temp_u);
  count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);      // label.length()
  count += fwrite((void *) label.c_str(), size, 1, ofp);          // label
  temp_d = learningRate;
  Endian::swap_8((void *) &temp_d);
  count += fwrite((void *) &temp_d, sizeof(temp_d), 1, ofp);      // learningRate
  temp_d = momentum;
  Endian::swap_8((void *) &temp_d);
  count += fwrite((void *) &temp_d, sizeof(temp_d), 1, ofp);      // momentum
  temp_u = m_sizeOfRepresentations;
  Endian::swap_4((void *) &temp_u);
  count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);      // m_sizeOfRepresentations
  temp_u = m_numberOfLayers;
  Endian::swap_4((void *) &temp_u);
  count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);      // m_numberOfLayers
  temp_u = m_epochs;
  Endian::swap_4((void *) &temp_u);
  count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);      // m_epochs
  
  for (i = 0; i < m_numberOfLayers; i++) {
    temp_u = perceptron[i].size();
    Endian::swap_4((void *) &temp_u);
    count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);    // perceptron[i].size()
    for (j = 0; j < perceptron[i].size(); j++) {
      temp_u = perceptron[i][j]->input.size();
      Endian::swap_4((void *) &temp_u);
      count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);  // perceptron[i][j]->input.size()
    }
  }
  
  // -------------------------------------------------------------------------
  // then we save the network's weights (current and previous), iterating over
  // each layer, neuron and input
  for (i = 0; i < m_numberOfLayers; i++)
    for (j = 0; j < perceptron[i].size(); j++)
      for (k = 0; k < perceptron[i][j]->input.size(); k++) {
        temp_d = perceptron[i][j]->input[k]->weight;
        Endian::swap_8((void *) &temp_d);
        count += fwrite((void *) &temp_d, sizeof(temp_d), 1, ofp);    // perceptron[i][j]->input[k]->weight
        temp_d = perceptron[i][j]->previousInputWeight[k];
        Endian::swap_8((void *) &temp_d);
        count += fwrite((void *) &temp_d, sizeof(temp_d), 1, ofp);    // perceptron[i][j]->previousInputWeight[k]
      }
  
  
  // ----------------------------------------------------
  // next, we save our distributed /term/ representations
  for (temp_u = 0, term = termRepresentationMap.begin(); term != termRepresentationMap.end(); term++)
    temp_u += (dynamic_cast<DistributedTermRepresentation*>(term->second) != 0);
  Endian::swap_4((void *) &temp_u);
  count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);           // the number of /terms/ in the map
  for (term = termRepresentationMap.begin(); term != termRepresentationMap.end(); term++) {
    if (dynamic_cast<DistributedTermRepresentation*>(term->second) != 0)
      count += term->second->save(ofp);                                // and the terms themselves
  }
  
  
  // ---------------------------------------------------------
  // and finally the sentences (in increasing order of height)
  temp_u = heightSentenceMap.size();
  Endian::swap_4((void *) &temp_u);
  count += fwrite((void *) &temp_u, sizeof(temp_u), 1, ofp);                  // the number of sentences
  for (i = 1; i <= m_maximumHeight; i++)
    for (sentence = heightSentenceMap.find(i); sentence != heightSentenceMap.upper_bound(i); sentence++)
      count += sentence->second->save(ofp);                                   // the sentence's term-like properties
  
  return count;
}




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
unsigned FGREPNetwork::load(FILE *ifp) throw (Exception::IncompatibleNetworkGeometry) {
  char *temp_s;
  unsigned i, j, k, count = 0, size, termsPerSentence = m_sizeOfInputLayer / m_sizeOfRepresentations, type;
  unsigned sizeOfRepresentations, numberOfLayers;
  double temp_d;
  
  // ----------------------------------------------------------
  // Load the network's label, its parameters and its geometry;
  count += fread((void *) &size, sizeof(size), 1, ifp);           // label.length()
  Endian::swap_4((void *) &size);
  temp_s = new char[size];
  count += fread((void *) temp_s, size, 1, ifp);                  // label
  label = temp_s;
  delete[] temp_s;
  
  count += fread((void *) &learningRate, sizeof(learningRate), 1, ifp);
  Endian::swap_8((void *) &learningRate);                         // learningRate
  count += fread((void *) &momentum, sizeof(momentum), 1, ifp);
  Endian::swap_8((void *) &momentum);                             // momentum
  count += fread((void *) &sizeOfRepresentations, sizeof(sizeOfRepresentations), 1, ifp);
  Endian::swap_4((void *) &sizeOfRepresentations);                // sizeOfReresentations
  count += fread((void *) &numberOfLayers, sizeof(numberOfLayers), 1, ifp);
  Endian::swap_4((void *) &numberOfLayers);                       // numberOfLayers
  count += fread((void *) &m_epochs, sizeof(m_epochs), 1, ifp);
  Endian::swap_4((void *) &m_epochs);                             // m_epochs
  
  if ((sizeOfRepresentations != m_sizeOfRepresentations) || (numberOfLayers != m_numberOfLayers)) {
    Exception::IncompatibleNetworkGeometry e("incompatible representation size or number of layers");
    throw e;
  }
  
  for (i = 0; i < m_numberOfLayers; i++) {
    count += fread((void *) &size, sizeof(size), 1, ifp);
    Endian::swap_4((void *) &size);                               // perceptron[i].size()
    if (size != perceptron[i].size()) {
      temp_s = new char[80];
      sprintf(temp_s, "layer %d contains %d neurons; expected %d", i + 1, size, (unsigned) perceptron[i].size());
      Exception::IncompatibleNetworkGeometry e(temp_s);
      delete[] temp_s;
      throw e;
    }
    for (j = 0; j < perceptron[i].size(); j++) {
      count += fread((void *) &size, sizeof(size), 1, ifp);
      Endian::swap_4((void *) &size);                             // perceptron[i][j]->input.size()
      if (size != perceptron[i][j]->input.size()) {
        temp_s = new char[80];
        sprintf(temp_s, "neuron(%d, %d) has %d inputs; expected %d", i, j, size, (unsigned) perceptron[i][j]->input.size());
        Exception::IncompatibleNetworkGeometry e(temp_s);
        delete[] temp_s;
        throw e;
      }
    }
  }
  
  
  // ----------------------------------------------------------
  // then we load the network's weights (current and previous);
  for (i = 0; i < m_numberOfLayers; i++)
    for (j = 0; j < perceptron[i].size(); j++)
      for (k = 0; k < perceptron[i][j]->input.size(); k++) {
        count += fread((void *) &perceptron[i][j]->input[k]->weight, sizeof(double), 1, ifp);
        Endian::swap_8((void *) &perceptron[i][j]->input[k]->weight);          // perceptron[i][j]->input[k]->weight
        count += fread((void *) &perceptron[i][j]->previousInputWeight[k], sizeof(double), 1, ifp);
        Endian::swap_8((void *) &perceptron[i][j]->previousInputWeight[k]);    // perceptron[i][j]->previousInputWeight[k]
      }
  
  
  // --------------------------------------------
  // followed by the distributed representations,
  DistributedRepresentationBase *temp_dr;
  
  for (map<string, DistributedRepresentationBase*>::iterator term = termRepresentationMap.begin(); term != termRepresentationMap.end(); term++)
    delete term->second;
  termRepresentationMap.clear();
  
  count += fread((void *) &size, sizeof(size), 1, ifp);     // the number of terms
  Endian::swap_4((void *) &size);
  for (i = 0; i < size; i++) {                              // and the terms themselves
    temp_dr = new DistributedTermRepresentation("", m_sizeOfRepresentations);
    temp_dr->load(ifp);
    termRepresentationMap[temp_dr->label] = temp_dr;
  }

  
  // -----------------
  // and the sentences
  vector<DistributedRepresentationBase*> terms;
  
  for (i = 0; i < sentence.size(); i++)
    delete sentence[i];
  sentence.clear();
  
  count += fread((void *) &size, sizeof(size), 1, ifp);     // the number of sentences
  Endian::swap_4((void *) &size);
  for (i = 0; i < size; i++) {
    terms.clear();
    for (j = 0; j < termsPerSentence; j++) {
      count += fread((void *) &k, sizeof(k), 1, ifp);       // k == length of the name
      Endian::swap_4((void *) &k);
      temp_s = new char[k];
      count += fread((void *) temp_s, k, 1, ifp);
      terms.push_back(termRepresentationMap[temp_s]);
      delete[] temp_s;
    }
    count += fread((void *) &temp_d, sizeof(double), 1, ifp);
    Endian::swap_8((void *) &temp_d);
    sentence.push_back(new DistributedSentenceRepresentation(temp_d, terms));
  }
  
  return count;
}




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
unsigned FGREPNetwork::print(FILE *ofp, const char *prefix) {
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
