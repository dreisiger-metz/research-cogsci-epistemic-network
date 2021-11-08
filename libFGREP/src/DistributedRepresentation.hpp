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
    /// @param verbose  a flag which, if true, causes additional information
    ///                 (e.g. errors and weights) to be printed out
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
    /// Saves the distributed representation's state to an output file stream
    ///
    /// This method saves the state of the distributed representation to the
    /// specified output file stream
    ///
    /// @param ofp   the output file stream
    ///
    /// @returns the number of bytes written to the output stream
    //  ======================================================================80
    virtual unsigned save(FILE *ofp);
    
    
    //  ======================================================================80
    /// Loads the distributed representation's state from an input file stream
    ///
    /// This method loads the state of the distributed representation from the
    /// specified input file stream
    ///
    /// @param ifp   the input file stream
    ///
    /// @returns the number of bytes read from the input stream
    //  ======================================================================80
    virtual unsigned load(FILE *ifp);

    
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
    unsigned updateAverageReconstructionErrorSquared(std::vector<Perceptron*> &outputLayer, unsigned offset) throw (Exception::IncompatibleVectorSize);


    //  ======================================================================80
    /// Returns the representation's average error squared, averaged over all
    /// of the sentences in which it appeared this epoch
    //  ======================================================================80
    virtual double averageReconstructionErrorSquared() { return m_cumulativeAverageReconstructionErrorSquared / m_usageCounter; }
    
    
    //  ======================================================================80
    /// Reset the representation's average errors and sentence counter
    //  ======================================================================80
    void resetAverageReconstructionErrorSquared() { m_averageReconstructionErrorSquared = 0.0; m_cumulativeAverageReconstructionErrorSquared = 0.0; m_usageCounter = 0; }


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
    /// If [fixed] is not true, this method updates the distributed represen-
    /// tation based upon the deltaRepresentations that have been accumulated
    /// by the updateDeltaRepresentation method;  it also clears the accumula-
    /// tors and the usage counter.
    ///
    /// @returns the length of the representation
    //  ======================================================================80
    virtual unsigned updateRepresentationBatch();

    
    /// the vector that accumulates per-epoch changes to the representation
    std::vector<double> deltaRepresentation;
    /// determines whether or not the representation should be updated during training
    bool                fixed;
    /// the height of this representation, measured from the bottom of the representational tree
    unsigned height;

    
  protected:
    /// general-purpose member variable to reduce our memory allocation needs
    double              m_d;
    /// the number of times that this representation has been used this epoch
    unsigned            m_usageCounter;
    /// a persistent temporary variable used by ::updateAverageErrorSquared
    double              m_averageReconstructionErrorSquared;
    /// a persistent temporary variable used by the ::*AverageErrorSquared methods
    double              m_cumulativeAverageReconstructionErrorSquared;
  };






  //  ========================================================================80
  /// @class    DistributedTermRepresentation
  /// @brief    A class that represents terms
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class DistributedTermRepresentation : public DistributedRepresentationBase {
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
    DistributedTermRepresentation(std::string label, unsigned size);

    
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
    DistributedTermRepresentation(std::string label, unsigned size, double value);


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
    DistributedTermRepresentation(std::string label, unsigned size, std::vector<double> value);
  };






  //  ========================================================================80
  /// @class    DistributedSentenceRepresentation
  /// @brief    A class that represents sentences using their distributed
  ///           representations.
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class DistributedSentenceRepresentation : public DistributedRepresentationBase {
  public:
    //  ======================================================================80
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
    //  ======================================================================80
    DistributedSentenceRepresentation(std::string label, unsigned size, std::vector<DistributedRepresentationBase*> term, double weight = 1.0);

    
    //  ======================================================================80
    /// Loads the constituents' distributed representation into the input layer
    ///
    /// This method loads the term's distributed representation into the input
    /// layer.
    ///
    /// @param inputLayer   the network's input layer
    ///
    /// @returns the length of the sentence's representation
    //  ======================================================================80
    unsigned loadTermRepresentations(std::vector<Perceptron*> &inputLayer);


    //  ======================================================================80
    /// Saves the constituents' distributed representation from the input layer
    ///
    /// This method updates the term's distributed representation based upon
    /// the input layer's pattern of activation.
    ///
    /// @param inputLayer   the network's input layer
    ///
    /// @returns the length of the sentence's representation
    //  ======================================================================80
    unsigned saveTermRepresentations(std::vector<Perceptron*> &inputLayer);


    //  ======================================================================80
    /// Saves the distributed representation's state to an output file stream
    ///
    /// This method saves the state of the distributed representation to the
    /// specified output file stream
    ///
    /// @param ofp   the output file stream
    ///
    /// @returns the number of bytes written to the output stream
    //  ======================================================================80
    virtual unsigned save(FILE *ofp);
    
    
    //  ======================================================================80
    /// Loads the distributed representation's state from an input file stream
    ///
    /// This method loads the state of the distributed representation from the
    /// specified input file stream
    ///
    /// @param ifp   the input file stream
    ///
    /// @returns the number of bytes read from the input stream
    //  ======================================================================80
    virtual unsigned load(FILE *ifp);
    
    
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
    unsigned setTermErrorSignals(std::vector<Perceptron*> &outputLayer);


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
    unsigned updateAverageErrorsSquared(std::vector<Perceptron*> &outputLayer);


    //  ======================================================================80
    /// Returns the sum of the individual terms' reconstruction errors squared,
    /// as calculated for this particular sentence
    //  ======================================================================80
    double combinedAverageReconstructionErrorsSquared() { return m_combinedAverageReconstructionErrorsSquared; }
    
    
    //  ======================================================================80
    /// Reset the sentence and representation's average errors
    //  ======================================================================80
    void resetAverageErrorsSquared();


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
    unsigned updateTermDeltaRepresentations(std::vector<Perceptron*> &inputLayer);


    //  ======================================================================80
    /// Batch-update the distributed representation
    ///
    /// This method updates the distributed representation based upon the delta-
    /// representations that have been accumulated by the updateDeltaRepresen-
    /// tation method;  it also clears the accumulators and the usage counter.
    ///
    /// @returns the length of the representation
    //  ======================================================================80
    unsigned updateTermRepresentationsBatch();


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
    unsigned print(FILE *ofp, const char *prefix, bool verbose = true);


    /// the number of terms in the sentence
    unsigned numberOfTerms;
    /// the number of times that the sentence has been used to train the network
    unsigned iterations;
    /// the sentence's weight --- used to determine how often it's used during training
    double weight;
    // the terms themselves
    std::vector<DistributedRepresentationBase*> term;

    
  protected:
    unsigned  m_i, m_offset;
    double    m_d;
    double    m_combinedAverageReconstructionErrorsSquared;
    
  };

  
}
  
#endif
