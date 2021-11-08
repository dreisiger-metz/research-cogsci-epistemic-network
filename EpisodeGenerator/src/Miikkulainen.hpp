// ============================================================================
// Filename          : $RCSfile: Miikkulainen.hpp,v $
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
// Purpose           : This header file defines a bunch of plant and animal-
//                     related conceptual classes.
//
// Revision history  : 
// ============================================================================
#ifndef EpisodeGenerator__Miikkulainen_hpp
#define EpisodeGenerator__Miikkulainen_hpp
#include "Concept.hpp"


namespace EpisodeGenerator {

  // 1. ate <human>
  // 2. ate <human> <food>
  // 3. ate <human> <food-1> 
  //    ate <human> <food-2>
  // or ate <human> <food-1> <food-2>
  // 4. ate <human> <food>
  //    used <human> <utensil>

  //  ========================================================================80
  /// @class    LivingThing
  /// @brief    The base of all living organisms
  //  ========================================================================80
  class Template_1 : public Concept {
  public:
    Template_1(std::string conceptName = "template-1") : Concept(conceptName) {
      addOneOf("",  1.0, 2, 
	       new AtomicConcept("is  " + instanceName + " size--2.0m"), 0.4,
	       new AtomicConcept("is  " + instanceName + " size--3.0m"), 0.4,
	       new AtomicConcept("is  " + instanceName + " size--5.0m"), 0.2);
      
      //add("can-grow", 0.2, new AtomicConcept("can " + instanceName + " grow"));
    }
  };




  //  ========================================================================80
  /// @class    Plant
  /// @brief    The base of all forms of plant life
  //  ========================================================================80
  class Plant : public LivingThing {
  public:
    Plant(std::string conceptName = "#plant") : LivingThing(conceptName) {
      add("has-roots",  0.5, new AtomicConcept("has " + instanceName + " part--roots"));
    }
  };


  class Tree : public Plant {
  public:
    Tree(std::string conceptName = "#tree") : Plant(conceptName) {
      addOneOf("is-big",  1.0, 2, 
	       new AtomicConcept("is  " + instanceName + " size--2.0m"), 0.4,
	       new AtomicConcept("is  " + instanceName + " size--3.0m"), 0.4,
	       new AtomicConcept("is  " + instanceName + " size--5.0m"), 0.2);

      add("is-green",  1.0,  new AtomicConcept("is  " + instanceName + " colour--green"));
      add("is-brown",  0.75, new AtomicConcept("is  " + instanceName + " colour--brown"));

      add("has-bark",     0.8, new AtomicConcept("has " + instanceName + " part--bark"));
      add("has-branches", 0.8, new AtomicConcept("has " + instanceName + " part--branches"));
      add("has-leaves",   1.0, new AtomicConcept("has " + instanceName + " part--leaves"));
    }
  };


  class Shrub : public Plant {
  public:
    Shrub(std::string conceptName = "#shrub") : Plant(conceptName) {
      Concept *temp_c;

      addOneOf("is-medium",  1.0, 2, 
	       new AtomicConcept("is  " + instanceName + " size--1.0m"), 0.3,
	       new AtomicConcept("is  " + instanceName + " size--1.5m"), 0.5,
	       new AtomicConcept("is  " + instanceName + " size--2.0m"), 0.2);

      add("is-green", 1.0,  new AtomicConcept("is  " + instanceName + " colour--green"));
      add("is-brown", 0.25, new AtomicConcept("is  " + instanceName + " colour--brown"));

      add("has-bark",     0.2, new AtomicConcept("has " + instanceName + " part--bark"));
      add("has-branches", 0.4, new AtomicConcept("has " + instanceName + " part--branches"));
      add("has-leaves",   1.0, new AtomicConcept("has " + instanceName + " part--leaves"));
      temp_c = addOneOf("has-petals", 0.8, 2, 
			new AtomicConcept("has " + instanceName + " part--petals"), 0.4,
			NULL, 0.6);
      if (temp_c)
	addOneOf("petals-colour", 0.2, 2, 
		 new AtomicConcept("is  " + instanceName + " colour--red"), 0.5,
		 new AtomicConcept("is  " + instanceName + " colour--yellow"), 0.5);
    }
  };


  class Flower : public Plant {
  public:
    Flower(std::string conceptName = "#flower") : Plant(conceptName) {
      addOneOf("is-tiny",  1.0, 2, 
	       new AtomicConcept("is  " + instanceName + " size--5.0cm"), 0.75,
	       new AtomicConcept("is  " + instanceName + " size--10cm"), 0.25);
      add("is-green",  1.0, new AtomicConcept("is  " + instanceName + " colour--green"));
      addOneOf("petals-colour", 0.2, 2, 
	       new AtomicConcept("is  " + instanceName + " colour--red"), 0.5,
	       new AtomicConcept("is  " + instanceName + " colour--yellow"), 0.5);

      add("has-leaves",   0.5, new AtomicConcept("has " + instanceName + " part--leaves"));
      add("has-petals",   1.0, new AtomicConcept("has " + instanceName + " part--petals"));
    }
  };


  class Grass : public Plant {
  public:
    Grass(std::string conceptName = "#grass") : Plant(conceptName) {
      add("is-green",   1.0, new AtomicConcept("is  " + instanceName + " colour--green"));
      add("has-leaves", 0.5, new AtomicConcept("has " + instanceName + " part--leaves"));
    }
  };




  //  ========================================================================80
  /// @class    Animal
  /// @brief    The base of all animals
  //  ========================================================================80
  class Animal : public LivingThing {
  public:
    Animal(std::string conceptName = "#animal") : LivingThing(conceptName) {
      //add("can-move",  0.9, new AtomicConcept("can " + instanceName + " motion--move"));

      add("has-eyes",  0.9, new AtomicConcept("has " + instanceName + " part--eyes"));
      add("has-mouth", 0.5, new AtomicConcept("has " + instanceName + " part--mouth"));
    }
  };




  class Mammal : public Animal {
  public:
    Mammal(std::string conceptName = "#mammal") : Animal(conceptName) {
      add("can-walk", 0.9, new AtomicConcept("can " + instanceName + " motion--walk"));
      add("can-run",  0.9, new AtomicConcept("can " + instanceName + " motion--run"));
      
      add("has-skin", 0.2, new AtomicConcept("has " + instanceName + " part--skin"));
      add("has-legs", 1.0, new AtomicConcept("has " + instanceName + " part--legs"));
      add("has-ears", 0.5, new AtomicConcept("has " + instanceName + " part--ears"));
      add("has-nose", 0.2, new AtomicConcept("has " + instanceName + " part--nose"));
    }
  };


  class Dog : public Mammal {
  public:
    Dog(std::string conceptName = "#dog") : Mammal(conceptName) {
      add("can-bark",  0.9, new AtomicConcept("can " + instanceName + " sound--bark"));
      add("can-pant",  0.5, new AtomicConcept("can " + instanceName + " action--pant"));
      
      add("has-tail", 0.75, new AtomicConcept("has " + instanceName + " part--tail"));
      
      addOneOf("is-size", 1.0, 2,
	       new AtomicConcept("is  " + instanceName + " size--30cm"), 0.2,
	       new AtomicConcept("is  " + instanceName + " size--50cm"), 0.6,
	       new AtomicConcept("is  " + instanceName + " size--1.0m"), 0.2);
      addOneOf("is-colour", 1.0, 4,
	       new AtomicConcept("is  " + instanceName + " colour--black"), 0.2,
	       new AtomicConcept("is  " + instanceName + " colour--brown"), 0.3,
	       new AtomicConcept("is  " + instanceName + " colour--grey"), 0.4,
	       new AtomicConcept("is  " + instanceName + " colour--white"), 0.1);
    }
  };


  class Cat : public Mammal {
  public:
    Cat(std::string conceptName = "#cat") : Mammal(conceptName) {
      add("can-bounce", 0.2, new AtomicConcept("can " + instanceName + " motion--bounce"));
      add("can-meow",   0.7, new AtomicConcept("can " + instanceName + " sound--meow"));
      add("can-lick",   0.2, new AtomicConcept("can " + instanceName + " action--lick"));

      add("has-tail",  0.75, new AtomicConcept("has " + instanceName + " part--tail"));

      addOneOf("is-size", 1.0, 2, 
	       new AtomicConcept("is  " + instanceName + " size--30cm"), 0.8,
	       new AtomicConcept("is  " + instanceName + " size--50cm"), 0.2);
      addOneOf("is-colour", 1.0, 5,
	       new AtomicConcept("is  " + instanceName + " colour--black"), 0.2,
	       new AtomicConcept("is  " + instanceName + " colour--grey"), 0.3,
	       new AtomicConcept("is  " + instanceName + " colour--white"), 0.1,
	       new AtomicConcept("is  " + instanceName + " colour--brown"), 0.2,
	       new AtomicConcept("is  " + instanceName + " colour--orange"), 0.2);
    }
  };




  class Bird : public Animal {
  public:
    Bird(std::string conceptName = "#bird") : Animal(conceptName) {
      add("can-fly",  0.9, new AtomicConcept("can " + instanceName + " motion--fly"));
      add("can-walk", 0.5, new AtomicConcept("can " + instanceName + " motion--walk"));
      add("can-run",  0.2, new AtomicConcept("can " + instanceName + " motion--run"));

      add("has-feathers", 1.0, new AtomicConcept("has " + instanceName + " part--feathers"));
      add("has-wings",    0.8, new AtomicConcept("has " + instanceName + " part--wings"));
      add("has-legs",     0.8, new AtomicConcept("has " + instanceName + " part--legs"));
      add("has-beak",     0.8, new AtomicConcept("has " + instanceName + " part--beak"));
    }
  };


  class Robin : public Bird {
  public:
    Robin(std::string conceptName = "#robin") : Bird(conceptName) {
      add("can-chirp", 0.5, new AtomicConcept("can " + instanceName + " sound--chirp"));

      add("is-size",   1.0, new AtomicConcept("is  " + instanceName + " size--10cm"));
      add("is-colour", 0.6, new AtomicConcept("is  " + instanceName + " colour--black"));
      add("is-colour", 0.4, new AtomicConcept("is  " + instanceName + " colour--red"));
    }
  };


  class Magpie : public Bird {
  public:
    Magpie(std::string conceptName = "#magpie") : Bird(conceptName) {
      add("is-size",   1.0, new AtomicConcept("is  " + instanceName + " size--30cm"));
      add("is-colour", 0.7, new AtomicConcept("is  " + instanceName + " colour--black"));
      add("is-colour", 0.3, new AtomicConcept("is  " + instanceName + " colour--white"));
    }
  };




  class Fish : public Animal {
  public:
    Fish(std::string conceptName = "#fish") : Animal(conceptName) {
      add("can-swim",   0.9, new AtomicConcept("can " + instanceName + " motion--swim"));

      add("has-gills",  1.0, new AtomicConcept("has " + instanceName + " part--gills"));
      add("has-scales", 1.0, new AtomicConcept("has " + instanceName + " part--scales"));
    }
  };


  class Sunfish : public Fish {
  public:
    Sunfish(std::string conceptName = "#sunfish") : Fish(conceptName) {
      add("is-size",   1.0, new AtomicConcept("is  " + instanceName + " size--50cm"));
      add("is-colour", 1.0, new AtomicConcept("is  " + instanceName + " colour--yellow"));
    }
  };


  class Salmon : public Fish {
  public:
    Salmon(std::string conceptName = "#salmon") : Fish(conceptName) {
      add("is-size",   1.0, new AtomicConcept("is  " + instanceName + " size--30cm"));
      add("is-colour", 1.0, new AtomicConcept("is  " + instanceName + " colour--red"));
    }
  };


}

#endif
