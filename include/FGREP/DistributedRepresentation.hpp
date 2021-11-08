// ============================================================================
// Filename          : $RCSfile: DistributedRepresentation.hpp,v $
// Version           : $Revision: 1.2 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 19-Jan-2009
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 11:22:59 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines the interface of the distrib-
//                     uted representations used by FGREP
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libFGREP__DistributedRepresentation_hpp
#define libFGREP__DistributedRepresentation_hpp

#include <stdarg.h>

#include "FeatureVector.hpp"


namespace ANN {


  //  ========================================================================80
  /// @class    DistributedRepresentationBase
  /// @brief    The base class for the distributed representations used by FGREP
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class DistributedRepresentationBase : public FeatureVector {
  public:
    enum RepresentationType {
      DistributedRepresentationBaseType,   /// the base class
      DistributedRepresentationType,       /// the normal class, with initially random components
      FixedRepresentationType              /// the class whose components are fixed
    };


    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates an instance of the distributed representation
    /// base.
    ///
    /// @param label  the term's identifying label
    /// @param size   the dimensionality of the distributed representation
    //  ======================================================================80
    DistributedRepresentationBase(std::string label, unsigned size);


    //  ======================================================================80
    /// Loads the distributed representation into the input layer
    ///
    /// This method loads the distributed representation into the input layer.
    ///
    /// @param inputLayer   the network's input layer
    /// @param offset       the term's offset from the first neuron in the
    ///                     input layer
    ///
    /// @returns the length of the representation
    //  ======================================================================80
    virtual unsigned loadRepresentation(std::vector<Perceptron*> &inputLayer, unsigned offset) throw (Exception::IncompatibleVectorSize);


    //  ======================================================================80
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
    //  ======================================================================80
    virtual unsigned saveRepresentation(std::vector<Perceptron*> &inputLayer, unsigned offset) throw (Exception::IncompatibleVectorSize);


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
    /// @param verbose  a flag which, if true, also prints out the term's average
    ///                 error squared
    ///
    /// @returns the number of bytes written to the output stream
    //  ======================================================================80
    virtual unsigned print(FILE *ofp, const char *prefix, bool verbose = false);


    //  ======================================================================80
    /// Sets the output layer's error signal
    ///
    /// This method sets the output layer's error signal based upon our 
    /// distributed representation.
    ///
    /// @param outputLayer   the network's output layer
    /// @param offset        the term's offset from the first neuron in the
    ///                      input layer
    ///
    /// @returns the length of the sentence's representation
    //  ======================================================================80
    unsigned setErrorSignals(std::vector<Perceptron*> &outputLayer, unsigned offset) throw (Exception::IncompatibleVectorSize);


    //  ======================================================================80
    /// Updates the representation's average error
    ///
    /// This method calculates the representation's reconstruction error based
    /// upon the output layer's pattern of activation, and updates the average
    /// error accumulators.
    ///
    /// @param outputLayer   the network's output layer
    /// @param offset        the term's offset from the first neuron in the
    ///                      input layer
    ///
    /// @returns the length of the sentence's representation
    //  ======================================================================80
    unsigned updateAverageErrors(std::vector<Perceptron*> &outputLayer, unsigned offset) throw (Exception::IncompatibleVectorSize);


    //  ======================================================================80
    /// Reset the representation's average errors and sentence counter
    //  ======================================================================80
    void resetAverageErrors() { averageErrorSquared = 0.0; cumulativeAverageErrorSquared = 0.0; usageCounter = 0; }


    //  ======================================================================80
    /// Update deltaRepresentation using the input layer's pattern of activation
    ///
    /// This method updates the distributed representation's [deltaRepresentation]
    /// based upon the input layer's pattern of activation.
    ///
    /// @param inputLayer   the network's input layer
    /// @param offset       the term's offset from the first neuron in the
    ///                     input layer
    ///
    /// @returns the length of the representation
    //  ======================================================================80
    virtual unsigned updateDeltaRepresentation(std::vector<Perceptron*> &inputLayer, unsigned offset) throw (Exception::IncompatibleVectorSize);


    //  ======================================================================80
    /// Batch-update the distributed representation
    ///
    /// This method updates the distributed representation based upon the delta-
    /// representations that have been accumulated by the updateDeltaRepresen-
    /// tation method;  it also clears the accumulators and the usage counter.
    ///
    /// @returns the length of the representation
    //  ======================================================================80
    virtual unsigned updateRepresentationBatch();


    std::vector<double> deltaRepresentation;

    RepresentationType  type;
    unsigned            usageCounter;
    double              averageErrorSquared;
    double              cumulativeAverageErrorSquared;

  protected:
    double              m_d;
  };






  //  ========================================================================80
  /// @class    DistributedRepresentation
  /// @brief    A class that initialises a random distributed representation
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class DistributedRepresentation : public DistributedRepresentationBase {
  public:
    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates a distributed representation and places it,
    /// randomly, in [0.0, 1.0]^size space.
    ///
    /// @param label  the term's identifying label
    /// @param size   the dimensionality of the distributed representation
    //  ======================================================================80
    DistributedRepresentation(std::string label, unsigned size);
  };
  
  
  


  
  //  ========================================================================80
  /// @class    FixedRepresentation
  /// @brief    A class that generates a specific and fixed distributed 
  ///           representation
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class FixedRepresentation : public DistributedRepresentationBase {
  public:
    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates a distributed representation and places it at
    /// the point specified by { value, value, ..., value }.
    ///
    /// @param label  the term's identifying label
    /// @param size   the dimensionality of the distributed representation
    /// @param value  the offset, from the origin, for each dimension
    //  ======================================================================80
    FixedRepresentation(std::string label, unsigned size, double value);


    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates a distributed representation and places it at
    /// the point specified by { value, value, ..., value }.
    ///
    /// @param label  the term's identifying label
    /// @param size   the dimensionality of the distributed representation
    /// @param value  the recurring subset of the representation's coordinates
    //  ======================================================================80
    FixedRepresentation(std::string label, unsigned size, std::vector<double> value);


    //  ======================================================================80
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
    //  ======================================================================80
    unsigned saveRepresentation(std::vector<Perceptron*> &inputLayer, unsigned offset) throw (Exception::IncompatibleVectorSize);
  };






  //  ========================================================================80
  /// @class    DistributedSentenceRepresentation
  /// @brief    A class that represents sentences using their distributed
  ///           representations.
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class DistributedSentenceRepresentation {
  public:
    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates a distributed representation and places it at
    /// the point specified by { value, value, ..., value }.
    ///
    /// @param weight          the sentence's weight
    /// @param size            the dimensionality of the sentence's distributed
    ///                        representation (alternatively, the size of the 
    ///                        network's hidden layer)
    /// @param numberOfTerms   the number of terms in each sentence
    /// @param ...             pointers (of type DistributedRepresentationBase)
    ///                        to each of the sentence's terms
    //  ======================================================================80
    DistributedSentenceRepresentation(double weight, unsigned size, unsigned numberOfTerms, ...);


    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates a distributed representation and places it at
    /// the point specified by { value, value, ..., value }.
    ///
    /// @param weight       the sentence's weight
    /// @param size         the dimensionality of the sentence's distributed
    ///                     representation (alternatively, the size of the 
    ///                     network's hidden layer)
    /// @param term         pointers (of type DistributedRepresentationBase) to
    ///                     each of the sentence's terms
    //  ======================================================================80
    DistributedSentenceRepresentation(double weight, unsigned size, std::vector<DistributedRepresentationBase*> term);


    //  ======================================================================80
    /// Loads the distributed representation into the input layer
    ///
    /// This method loads the term's distributed representation into the input
    /// layer.
    ///
    /// @param inputLayer   the network's input layer
    ///
    /// @returns the length of the sentence's representation
    //  ======================================================================80
    unsigned loadRepresentations(std::vector<Perceptron*> &inputLayer);


    //  ======================================================================80
    /// Saves the distributed representation from the input layer
    ///
    /// This method updates the term's distributed representation based upon
    /// the input layer's pattern of activation.
    ///
    /// @param inputLayer   the network's input layer
    ///
    /// @returns the length of the sentence's representation
    //  ======================================================================80
    unsigned saveRepresentations(std::vector<Perceptron*> &inputLayer);


    //  ======================================================================80
    /// Saves the pattern of activation across the hidden layer
    ///
    /// This method updates the sentences's pattern of activation based upon
    /// the state of the hidden layer.
    ///
    /// @param hiddenLayer   the network's hiddenlayer
    ///
    /// @returns the length of the sentence's pattern of activation
    //  ======================================================================80
    unsigned savePatternOfActivation(std::vector<Perceptron*> &hiddenLayer);


    //  ======================================================================80
    /// Sets the output layer's error signal
    ///
    /// This method sets the output layer's error signal based upon our 
    /// distributed representation.
    ///
    /// @param outputLayer   the network's output layer
    ///
    /// @returns the length of the sentence's representation
    //  ======================================================================80
    unsigned setErrorSignals(std::vector<Perceptron*> &outputLayer);


    //  ======================================================================80
    /// Updates the sentence and representation's average error
    ///
    /// This method calculates the sentence and representation's reconstruction
    /// error based upon the output layer's pattern of activation, and updates
    /// the average error accumulators.
    ///
    /// @param outputLayer   the network's output layer
    ///
    /// @returns the length of the sentence's representation
    //  ======================================================================80
    unsigned updateAverageErrors(std::vector<Perceptron*> &outputLayer);


    //  ======================================================================80
    /// Reset the sentence and representation's average errors
    //  ======================================================================80
    void resetAverageErrors();


    //  ======================================================================80
    /// Update deltaRepresentation using the input layer's pattern of activation
    ///
    /// This method updates the distributed representations' [deltaRepresentation]
    /// based upon the input layer's pattern of activation.
    ///
    /// @param inputLayer   the network's input layer
    ///
    /// @returns the length of the sentence's representation
    //  ======================================================================80
    unsigned updateDeltaRepresentations(std::vector<Perceptron*> &inputLayer);


    //  ======================================================================80
    /// Batch-update the distributed representation
    ///
    /// This method updates the distributed representation based upon the delta-
    /// representations that have been accumulated by the updateDeltaRepresen-
    /// tation method;  it also clears the accumulators and the usage counter.
    ///
    /// @returns the length of the representation
    //  ======================================================================80
    unsigned updateRepresentationsBatch();


    unsigned print(FILE *ofp, const char *prefix, bool verbose);


    /// the number of terms in the sentence
    unsigned numberOfTerms;
    /// the number of times that the sentence has been used to train the network
    unsigned iterations;
    /// the sentence's weight --- used to determine how often it's used during training
    double weight;
    /// the sentence's average reconstruction error squared
    double averageErrorSquared;
    /// the size of the sentence's pattern of activation --- i.e. its distributed representation
    unsigned size;
    /// the sentence's pattern of activation
    std::vector<double> patternOfActivation;

    // the terms themselves
    std::vector<DistributedRepresentationBase*> term;


  protected:
    unsigned m_i, m_offset;
    double   m_d;
  };

  
}
  
#endif
