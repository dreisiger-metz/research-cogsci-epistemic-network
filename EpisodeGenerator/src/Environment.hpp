// ============================================================================
// Filename          : $RCSfile: Environment.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 11-Feb-2009
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 06:03:32 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines a bunch of environments in
//                     which an episode may be set.
//
// Revision history  : 
// ============================================================================
#ifndef EpisodeGenerator__Environment_hpp
#define EpisodeGenerator__Environment_hpp
#include "Concept.hpp"
#include "Living.hpp"


namespace EpisodeGenerator {


  //  ========================================================================80
  /// @class    BasicRoom
  /// @brief    Describes a basic room
  ///
  /// This class defines an environment that contains the following:
  ///   is room ((size--large, 0.2), (size--huge, 0.5), (size--enormous, 0.3))
  ///   is room atmospheric--calm
  ///   is room atmospheric--dry
  /// 
  /// (As with most of these environmental concepts, it should probably be
  /// weighted down so as not to overwhelm the elements in the foreground.)
  //  ========================================================================80
  class BasicRoom : public Concept {
  public:
    BasicRoom(std::string conceptName = "#basic-room") : Concept(conceptName) {
      addOneOf("is-size", 1.0, 3,
	       new AtomicConcept("is  " + instanceName + " size--large"), 0.2,
	       new AtomicConcept("is  " + instanceName + " size--huge"), 0.5,
	       new AtomicConcept("is  " + instanceName + " size--enormous"), 0.3);
      add("is-calm", 0.5, new AtomicConcept("is  " + instanceName + " atmospheric--calm"));
      add("is-dry", 0.5, new AtomicConcept("is  " + instanceName + " atmospheric--dry"));
      addOneOf("temperature", 0.5, 2,
	       new AtomicConcept("is  " + instanceName + " temperature--cool"), 0.5,
	       new AtomicConcept("is  " + instanceName + " temperature--warm"), 0.5);
    }
  };


  class Room : public BasicRoom {
  public:
    Room(std::string conceptName = "#room") : BasicRoom(conceptName) {
      addOneOf("is-brightness", 1.0, 3,
	       new AtomicConcept("is  " + instanceName + " brightness--dark"), 0.2,
	       new AtomicConcept("is  " + instanceName + " brightness--dim"), 0.3,
	       new AtomicConcept("is  " + instanceName + " brightness--light"), 0.5);
      addOneOf("is-colour", 1.0, 4,
	       new AtomicConcept("is  " + instanceName + " colour--light-grey"), 0.3,
	       new AtomicConcept("is  " + instanceName + " colour--light-yellow"), 0.3,
	       new AtomicConcept("is  " + instanceName + " colour--light-blue"), 0.2,
	       new AtomicConcept("is  " + instanceName + " colour--light-green"), 0.2);

      // and maybe there's even a flower/pot-plant
      addOneOf("has-flower", 0.5, 2, new Flower(), 0.2, NULL, 0.8);
    }
  };


  class Park : public Concept {
  public:
    Park(std::string conceptName = "#park") : Concept(conceptName) {
      add("is-size", 1.0, new AtomicConcept("is  " + instanceName + " size--infinite"));
      addOneOf("wind-speed", 0.5, 2,
	       new AtomicConcept("is  " + instanceName + " atmospheric--calm"), 0.6,
	       new AtomicConcept("is  " + instanceName + " atmospheric--windy"), 0.4);
      addOneOf("dryness", 0.5, 2,
	       new AtomicConcept("is  " + instanceName + " atmospheric--dry"), 0.6,
	       new AtomicConcept("is  " + instanceName + " atmospheric--humid"), 0.4);
      addOneOf("temperature", 0.5, 4,
	       new AtomicConcept("is  " + instanceName + " temperature--cold"), 0.3,
	       new AtomicConcept("is  " + instanceName + " temperature--cool"), 0.2,
	       new AtomicConcept("is  " + instanceName + " temperature--warm"), 0.2,
	       new AtomicConcept("is  " + instanceName + " temperature--hot"), 0.3);

      // and it probably contains trees, grass and maybe the odd bird
      addOneOf("trees", 0.75, 2, new Tree(), 0.8, NULL, 0.2);
      addOneOf("grass", 0.5, 2, new Grass(), 0.9, NULL, 0.1);
      addOneOf("birds", 0.25, 3, new Magpie(), 0.3, new Robin(), 0.3, NULL, 0.4);
    }
  };


}

#endif
