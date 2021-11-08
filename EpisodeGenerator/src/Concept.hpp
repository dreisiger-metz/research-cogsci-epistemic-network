// ============================================================================
// Filename          : $RCSfile: Concept.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 10-Feb-2009
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 06:03:32 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines the interface of the Concept
//                     base class and the derived atomic and episode classes.
//
// Revision history  : 
// ============================================================================
#ifndef EpisodeGenerator__Concept_hpp
#define EpisodeGenerator__Concept_hpp

#include <stdarg.h>
#include <string.h>
#include <string>
#include <vector>
#include <map>

#include "FGREP/Statistics.hpp"


namespace EpisodeGenerator {


  // Forward declaration
  class Concept;


  //  ========================================================================80
  /// @class    Link
  /// @brief    This class maintains weighted links between concepts
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class Link {
  public:
    Link(std::string name, Concept *concept, double weight);
    Link(std::string name, Concept *concept);

    std::string name;
    Concept    *concept;
    double      weight;
  };




  //  ========================================================================80
  /// @class    Concept
  /// @brief    The base class for all other concepts --- basic or abstract
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class Concept {
  public:
    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates an instance of a concept.
    ///
    /// @param className   the name of the concept that this class represents
    //  ======================================================================80
    Concept(std::string className);


    //  ======================================================================80
    /// The destructor
    //  ======================================================================80
    virtual ~Concept();


    //  ======================================================================80
    /// Adds a child concept to our concept vector
    ///
    /// This method accepts a label, a weight and a subconcept and adds them to
    /// our concept vector;  if the subconcept is NULL, then nothing is added.
    ///
    /// @param subconceptName   the choice's mnemonic 
    /// @param weight           the weight of the chosen sentence
    /// @param number           the number of concept -- probability pairs
    //  ======================================================================80
    Concept *add(std::string subconceptName, double weight, Concept *subconcept);


    //  ======================================================================80
    /// Randomly select a child concept and add it to our concept vector
    ///
    /// This variable-argument method accepts an arbitrarily long list of
    /// concept -- probability pairs and select one of them.  If the chosen
    /// 'attribute' is non-NULL, it is added to our concept vector;  if it is
    /// NULL, then nothing is added.
    ///
    /// @param subconceptName   the choice's mnemonic 
    /// @param weight           the weight of the chosen sentence
    /// @param number           the number of concept -- probability pairs
    ///
    /// @note this method deletes any non-NULL concept that was not chosen
    //  ======================================================================80
    Concept *addOneOf(std::string subconceptName, double weight, unsigned number, ...);


    //  ======================================================================80
    /// Removes a child concept from our concept vector
    ///
    /// This method removes the named subconcept from our concept vector;  if the
    /// subconcept does not exists, then it does nothing
    ///
    /// @param subconceptName   the concept's mnemonic 
    //  ======================================================================80
    void remove(std::string subconceptName);


    //  ======================================================================80
    /// Prints this episode's fragment
    ///
    /// This method recursively generates and prints the episode fragment corres-
    /// ponding to this concept instance.
    ///
    /// @param ofp      the output stream to which the information will be
    ///                 printed
    /// @param prefix   a string that will preface every line of printed output
    /// @param weight   the factor by which to scale the concept's own weights
    ///
    /// @return the number of characters sent to [ofp]
    //  ======================================================================80
    virtual unsigned print(FILE *ofp, const char *prefix, double weight = 1.0);
    
      
    std::string conceptName;    /// the name of the concept this class represents
    std::string instanceName;   /// the name of this instance of the concept
    double weight;              /// the weight of this concept instance

    std::vector<Link*> subconcept;


    /// a static flag that defines the sentence-weight threshold --- if it is
    /// zero, then all weights passed to the add(...) method are preserved; if
    /// it is non zero, then all weights below it are rounded down to zero (and
    /// the corresponding sentences are effectively ignored) and all weights
    /// above it are rounded up to one
    static double SentenceWeightThreshold;


    /// a static helper class that generates the random, uniformly-distributed
    /// numbers that are used to make our choices
    static Utilities::Statistics::Uniform Uniform;


  protected:
    unsigned instanceNumber;

    /// a static 'counter' map between the concept names and their numbers
    /// of instances
    static std::map<std::string, unsigned> ConceptCounterMap;
  };


  

  //  ========================================================================80
  /// @class    AtomicConcept
  /// @brief    This class represents concepts that consist of a single term
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class AtomicConcept : public Concept {
  public:
    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor initialises the atomic concept and sets [value]
    /// accordingly
    ///
    /// @param value   the string that will be printed by AtomicConcept::print()
    //  ======================================================================80
    AtomicConcept(std::string value);


    //  ======================================================================80
    /// Prints this atomic concept's value
    ///
    /// @param ofp      the output stream to which the information will be
    ///                 printed
    /// @param prefix   a string that will preface every line of printed output
    /// @param weight   the factor by which to scale the concept's own weights
    ///
    /// @return the number of characters sent to [ofp]
    //  ======================================================================80
    unsigned print(FILE *ofp, const char *prefix, double weight = 1.0);

    
    std::string value;
  };




  //  ========================================================================80
  /// @class    Episode
  /// @brief    This is the base class of all episodic concepts
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class Episode : public Concept {
  public:
    //  ======================================================================80
    /// The constructor
    //  ======================================================================80
    Episode(std::string conceptName = "episode") : Concept(conceptName) { };

    //  ======================================================================80
    /// Prints this episode
    ///
    /// This method recursively prints the episode fragments contained in its
    /// subconcept vector.
    ///
    /// @param ofp      the output stream to which the information will be
    ///                 printed
    /// @param prefix   a string that will preface every line of printed output
    /// @param weight   the factor by which to scale the concept's own weights
    ///
    /// @return the number of characters sent to [ofp]
    //  ======================================================================80
    unsigned print(FILE *ofp, const char *prefix, double weight = 1.0);
  };


}
  
#endif
