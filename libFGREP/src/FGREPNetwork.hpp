// ============================================================================
// Filename          : $RCSfile: FGREPNetwork.hpp,v $
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
// Purpose           : This header file defines the interface of the FGREP
//                     network
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libFGREP__FGREPNetwork_hpp
#define libFGREP__FGREPNetwork_hpp

#include <stdarg.h>
#include <string.h>
#include <map>

#include "Endian.hpp"
#include "Exceptions.hpp"
#include "Perceptron.hpp"
#include "DistributedRepresentation.hpp"


namespace ANN {

  // Forward declarations
  namespace Test {
    class FGREPNetworkTest;
  }
  

  //  ========================================================================80
  /// @class    FGREPNetwork
  /// @brief    A class that implements an FGREP network
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class FGREPNetwork {
  public:
    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates a instance of an FGREP network of perceptrons.
    ///
    /// @param label                the network's identifying label
    /// @param learningRate         the perceptrons' initial learning rate
    /// @param momentum             the perceptrons' initial momentum
    /// @param representationSize   the size of the distributed representations
    /// @param layers               the number of layers in the network
    /// @param ...                  the number of neurons in each layer of the 
    ///                             network (the input and output layer sizes 
    ///                             must be a multiple of [representationSize])
    //  ======================================================================80
    FGREPNetwork(std::string label, double learningRate, double momentum, unsigned representationSize, unsigned layers, ...);


    //  ======================================================================80
    /// The destructor
    //  ======================================================================80
    virtual ~FGREPNetwork();


    //  ======================================================================80
    /// Declares a new term
    ///
    /// This method adds the named term to the term--distributed representation
    /// map and associates it with a random representation
    ///
    /// @param term    the term being declared
    ///
    /// @returns the (new) total number of terms
    //  ======================================================================80
    unsigned declareTerm(std::string term) throw (Exception::DuplicateTerm);


    //  ======================================================================80
    /// Adds a new term
    ///
    /// This method adds the named term to the term--distributed representation
    /// map
    ///
    /// @param representation   its distributed representation
    ///
    /// @returns the (new) total number of terms
    //  ======================================================================80
    unsigned addTerm(DistributedRepresentationBase *representation) throw (Exception::DuplicateTerm, Exception::IncompatibleVectorSize);


    //  ======================================================================80
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
    //  ======================================================================80
    unsigned addSentence(std::string label, std::string sentence, double weight = 1.0) throw (Exception::UnknownTerm);


    //  ======================================================================80
    /// Trains the network
    ///
    /// This method iterates over all training samples and implements the 
    /// incremental feed-forward, error backpropagation learning algorithm.
    ///
    /// @param iterations   the desired number of training epochs
    ///
    /// @returns the average error squared of the output for this epoch
    //  ======================================================================80
    double trainNetworkByNumberOfIterationsIncremental(unsigned iterations);


    //  ======================================================================80
    /// Trains the network
    ///
    /// This method iterates over all training samples and implements the 
    /// incremental feed-forward, error backpropagation learning algorithm.
    ///
    /// @param errorThreshold   the minimum average squared error level at 
    ///                         which the training stops
    /// @param maximumEpochs    the maximum number of epochs after which the 
    ///                         training stops
    ///
    /// @returns the average error squared of the output for this epoch
    //  ======================================================================80
    double trainNetworkByAverageErrorSquaredIncremental(double errorThreshold, unsigned maximumEpochs = 0);


    //  ======================================================================80
    /// Trains the network
    ///
    /// This method iterates over all training samples and implements the 
    /// batch feed-forward, error backpropagation learning algorithm.
    ///
    /// @param iterations   the desired number of training epochs
    ///
    /// @returns the average error squared of the output for this epoch
    //  ======================================================================80
    double trainNetworkByNumberOfIterationsBatch(unsigned iterations);


    //  ======================================================================80
    /// Trains the network
    ///
    /// This method iterates over all training samples and implements the 
    /// batch feed-forward, error backpropagation learning algorithm.
    ///
    /// @param errorThreshold   the minimum average squared error level at 
    ///                         which the training stops
    /// @param maximumEpochs    the maximum number of epochs after which the 
    ///                         training stops
    ///
    /// @returns the average error squared of the output for this epoch
    //  ======================================================================80
    double trainNetworkByAverageErrorSquaredBatch(double errorThreshold, unsigned maximumEpochs = 0);


    //  ======================================================================80
    /// Processes the sentences and distributed representations
    ///
    /// This method iterates over all training samples, propagating the activa-
    /// tion through the network, and calculates all of the average per-term
    /// and per-sentence errors squared.
    ///
    /// @returns the average error squared of the output for all sentences
    //  ======================================================================80
    double processRepresentations();


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
    unsigned save(FILE *ofp);


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
    unsigned load(FILE *ifp) throw (Exception::IncompatibleNetworkGeometry);


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

    
    inline void abortTraining() { m_running = false; }


    inline unsigned epochs() { return m_epochs; }


    std::string label;
    double learningRate;
    double momentum;

    
    /// the network used to develop the distributed representations
    std::vector<std::vector<Perceptron*> > perceptron;

    /// a map between the string term names and their corresponding distributed
    /// representation structures
    std::map<std::string, DistributedRepresentationBase*> termRepresentationMap;
    
    /// a map between the string sentence labels and their corresponding 
    /// distributed representation structures
    std::map<std::string, DistributedSentenceRepresentation*> sentenceRepresentationMap;
    
    /// a multi-map between tree height and the sentences that occupy those levels;
    /// this is used to ensure that the sentences' representations are developed in
    /// the correct order, and during the save--load cycles to resolve dependencies
    std::multimap<unsigned, DistributedSentenceRepresentation*> heightSentenceMap;

    
  protected:
    bool     m_running;

    unsigned m_sizeOfRepresentations;
    unsigned m_numberOfLayers;
    unsigned m_sizeOfInputLayer;
    unsigned m_sizeOfOutputLayer;

    unsigned m_maximumHeight;
    
    unsigned m_epochs;
    
    Sigmoid *m_sigmoid;
    
    
    friend class Test::FGREPNetworkTest;
  };
  
}
  
#endif
