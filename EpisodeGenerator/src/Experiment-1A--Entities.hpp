// ============================================================================
// Filename          : $RCSfile: Experiment-1A--Entities.hpp,v $
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
#ifndef EpisodeGenerator__Experiment_1A__Entities_hpp
#define EpisodeGenerator__Experiment_1A__Entities_hpp
#include "Concept.hpp"


// Yes, this is, generally speaking, a bad thing to do, but given the stand-
// alone nature of the Episode Generator, and the fact that it makes all of
// the following class definitions easier, I can live with it...
using namespace std;


namespace EpisodeGenerator {


  //  ========================================================================80
  /// @class    Thing
  /// @brief    The base of all things
  //  ========================================================================80
  class Thing : public Concept {
  public:
    Thing(string conceptName = "#thing") : Concept(conceptName) {
      // Nothing to see here
    }
  };




  //  ========================================================================80
  /// @class    LivingThing
  /// @brief    The base of all living organisms
  //  ========================================================================80
  class LivingThing : public Thing {
  public:
    LivingThing(string conceptName = "#living-thing") : Thing(conceptName) {
      // Nothing to see here
    }
  };




  //  ========================================================================80
  /// @class    Plant
  /// @brief    The base of all forms of plant life
  //  ========================================================================80
  class Plant : public LivingThing {
  public:
    Plant(string conceptName = "#plant") : LivingThing(conceptName) {
      add("sway",  1.0, new AtomicConcept("can " + instanceName + " motion--sway"));

      add("has-leaves", 1.0, new AtomicConcept("has " + instanceName + " part--leaves"));
      add("is-green",   1.0, new AtomicConcept("is  " + instanceName + " colour--green"));
    }
  };


  class Tree : public Plant {
  public:
    Tree(string conceptName = "#tree") : Plant(conceptName) {
      add("has-bark",     1.0, new AtomicConcept("has " + instanceName + " part--bark"));
      add("has-trunk",    1.0, new AtomicConcept("has " + instanceName + " part--trunk"));
      add("has-branches", 1.0, new AtomicConcept("has " + instanceName + " part--branches"));
      add("has-roots",    0.5, new AtomicConcept("has " + instanceName + " part--roots"));
    }
  };


  class Jarrah : public Tree {
  public:
    Jarrah(string conceptName = "#jarrah") : Tree(conceptName) {
      addOneOf("height", 1.0, 3,
	       new AtomicConcept("is  " + instanceName + " height--5.0m"), 0.4,
	       new AtomicConcept("is  " + instanceName + " height--10m"), 0.4,
	       new AtomicConcept("is  " + instanceName + " height--20m"), 0.2);
      add("width", 1.0, new AtomicConcept("is  " + instanceName + " width--3.0m"));

      add ("is-brown", 1.0, new AtomicConcept("is  " + instanceName + " colour--brown"));
      add ("is-rough", 0.8, new AtomicConcept("is  " + instanceName + " texture--rough"));
    }
  };


  class GhostGum : public Tree {
  public:
    GhostGum(string conceptName = "#ghost-gum") : Tree(conceptName) {
      addOneOf("height", 1.0, 3,
	       new AtomicConcept("is  " + instanceName + " height--5.0m"), 0.4,
	       new AtomicConcept("is  " + instanceName + " height--10m"), 0.5,
	       new AtomicConcept("is  " + instanceName + " height--20m"), 0.1);
      add("width",      1.0, new AtomicConcept("is  " + instanceName + " width--5.0m"));
      add ("is-grey",   1.0, new AtomicConcept("is  " + instanceName + " colour--grey"));
      add ("is-smooth", 0.8, new AtomicConcept("is  " + instanceName + " texture--smooth"));
    }
  };


  class Wattle : public Tree {
  public:
    Wattle(string conceptName = "#wattle") : Tree(conceptName) {
      Concept *temp_c;

      addOneOf("height", 1.0, 2,
	       new AtomicConcept("is  " + instanceName + " height--3.0m"), 0.5,
	       new AtomicConcept("is  " + instanceName + " height--5.0m"), 0.5);
      add("width",    1.0, new AtomicConcept("is  " + instanceName + " width--3.0m"));
      add("is-brown", 1.0, new AtomicConcept("is  " + instanceName + " colour--brown"));
      add("is-rough", 0.8, new AtomicConcept("is  " + instanceName + " texture--rough"));
      temp_c = addOneOf("has-petals", 0.8, 2, NULL, 0.5,
			new AtomicConcept("has " + instanceName + " part--petals"), 0.5);
      if (temp_c)
	add("petal-colour", 0.8, new AtomicConcept("is  " + instanceName + " colour--yellow"));
    }
  };




  class Shrub : public Plant {
  public:
    Shrub(string conceptName = "#shrub") : Plant(conceptName) {
      add("has-bark",     0.8, new AtomicConcept("has " + instanceName + " part--bark"));
      add("has-branches", 1.0, new AtomicConcept("has " + instanceName + " part--branches"));
    }
  };


  class GrassTree : public Shrub {
  public:
    GrassTree(string conceptName = "#grass-tree") : Shrub(conceptName) {
      addOneOf("height",  1.0, 3, 
	       new AtomicConcept("is  " + instanceName + " height--1.5m"), 0.33,
	       new AtomicConcept("is  " + instanceName + " height--2.0m"), 0.34,
	       new AtomicConcept("is  " + instanceName + " height--3.0m"), 0.33);
      add("width",    1.0, new AtomicConcept("is  " + instanceName + " width--1.0m"));
      add("is-black", 1.0, new AtomicConcept("is  " + instanceName + " colour--black"));
      add("is-rough", 0.8, new AtomicConcept("is  " + instanceName + " texture--rough"));
    }
  };


  class GeraldtonWax : public Shrub {
  public:
    GeraldtonWax(string conceptName = "#geraldton-wax") : Shrub(conceptName) {
      addOneOf("height",  1.0, 2, 
	       new AtomicConcept("is  " + instanceName + " height--1.5m"), 0.5,
	       new AtomicConcept("is  " + instanceName + " height--2.0m"), 0.5);
      add("width",        1.0, new AtomicConcept("is  " + instanceName + " width--1.5m"));
      add("is-brown",     0.5, new AtomicConcept("is  " + instanceName + " colour--brown"));
      add("is-rough",     0.5, new AtomicConcept("is  " + instanceName + " texture--rough"));

      add("has-petals",   0.8, new AtomicConcept("has " + instanceName + " part--petals"));
      add("petal-colour", 0.8, new AtomicConcept("is  " + instanceName + " colour--magenta"));
    }
  };


  class BottleBrush : public Shrub {
  public:
    BottleBrush(string conceptName = "#bottle-brush") : Shrub(conceptName) {
      Concept *temp_c;

      addOneOf("height",  1.0, 2, 
	       new AtomicConcept("is  " + instanceName + " height--1.0m"), 0.5,
	       new AtomicConcept("is  " + instanceName + " height--1.5m"), 0.5);
      addOneOf("width",  1.0, 2, 
	       new AtomicConcept("is  " + instanceName + " width--1.0m"), 0.5,
	       new AtomicConcept("is  " + instanceName + " width--1.5m"), 0.5);
      add("is-brown", 0.8, new AtomicConcept("is  " + instanceName + " colour--brown"));
      add("is-rough", 0.5, new AtomicConcept("is  " + instanceName + " texture--rough"));

      temp_c = addOneOf("has-petals", 0.6, 2, NULL, 0.4,
			new AtomicConcept("has " + instanceName + " part--petals"), 0.6);
      if (temp_c)
	addOneOf("petal-colour", 0.6, 2, 
		 new AtomicConcept("is  " + instanceName + " colour--red"), 0.5,
		 new AtomicConcept("is  " + instanceName + " colour--yellow"), 0.5);
    }
  };




  class Flower : public Plant {
  public:
    Flower(string conceptName = "#flower") : Plant(conceptName) {
      add("has-petals", 1.0, new AtomicConcept("has " + instanceName + " part--petals"));
    }
  };


  class KangarooPaw : public Flower {
  public:
    KangarooPaw(string conceptName = "#kangaroo-paw") : Flower(conceptName) {
      add("height", 1.0, new AtomicConcept("is  " + instanceName + " height--1.0m"));
      add("width",  1.0, new AtomicConcept("is  " + instanceName + " width--30cm"));
      addOneOf("petal-colour",  1.0, 3, 
	       new AtomicConcept("is  " + instanceName + " colour--red"), 0.4,
	       new AtomicConcept("is  " + instanceName + " colour--orange"), 0.3,
	       new AtomicConcept("is  " + instanceName + " colour--yellow"), 0.3);
      add("is-fuzzy", 0.5, new AtomicConcept("is  " + instanceName + " texture--fuzzy"));
    }
  };


  class Everlasting : public Flower {
  public:
    Everlasting(string conceptName = "#everlasting") : Flower(conceptName) {
      add("height", 1.0, new AtomicConcept("is  " + instanceName + " height--30cm"));
      add("width",  1.0, new AtomicConcept("is  " + instanceName + " width--10cm"));
      addOneOf("petal-colour",  1.0, 2, 
	       new AtomicConcept("is  " + instanceName + " colour--red"), 0.5,
	       new AtomicConcept("is  " + instanceName + " colour--yellow"), 0.5);
    }
  };




  class Grass : public Plant {
  public:
    Grass(string conceptName = "#grass") : Plant(conceptName) {
      add("height", 1.0, new AtomicConcept("is  " + instanceName + " height--1.0cm"));
      add("width",  1.0, new AtomicConcept("is  " + instanceName + " width--20m"));
    }
  };




  //  ========================================================================80
  /// @class    Animal
  /// @brief    The base of all animals
  //  ========================================================================80
  class Animal : public LivingThing {
  public:
    Animal(string conceptName = "#animal") : LivingThing(conceptName) {
      add("has-eyes",  0.8, new AtomicConcept("has " + instanceName + " part--eyes"));
      add("has-mouth", 0.8, new AtomicConcept("has " + instanceName + " part--mouth"));
    }
  };




  class Mammal : public Animal {
  public:
    Mammal(string conceptName = "#mammal") : Animal(conceptName) {
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
    Dog(string conceptName = "#dog") : Mammal(conceptName) {
      add("has-tail",  0.8, new AtomicConcept("has " + instanceName + " part--tail"));
      
      add("can-bark",  0.8, new AtomicConcept("can " + instanceName + " sound--bark"));
      add("can-pant",  0.5, new AtomicConcept("can " + instanceName + " action--pant"));
      
      addOneOf("height", 1.0, 3,
	       new AtomicConcept("is  " + instanceName + " height--30cm"), 0.2,
	       new AtomicConcept("is  " + instanceName + " height--50cm"), 0.6,
	       new AtomicConcept("is  " + instanceName + " height--1.0m"), 0.2);
      add("width",     1.0, new AtomicConcept("is  " + instanceName + " width--30cm"));
      addOneOf("is-colour", 1.0, 4,
	       new AtomicConcept("is  " + instanceName + " colour--black"), 0.2,
	       new AtomicConcept("is  " + instanceName + " colour--brown"), 0.3,
	       new AtomicConcept("is  " + instanceName + " colour--grey"), 0.4,
	       new AtomicConcept("is  " + instanceName + " colour--white"), 0.1);
    }
  };


  class Cat : public Mammal {
  public:
    Cat(string conceptName = "#cat") : Mammal(conceptName) {
      add("has-tail",   1.0, new AtomicConcept("has " + instanceName + " part--tail"));

      add("can-bounce", 0.8, new AtomicConcept("can " + instanceName + " motion--bounce"));
      add("can-meow",   0.8, new AtomicConcept("can " + instanceName + " sound--meow"));
      add("can-lick",   0.5, new AtomicConcept("can " + instanceName + " action--lick"));

      addOneOf("height", 1.0, 2, 
	       new AtomicConcept("is  " + instanceName + " height--30cm"), 0.8,
	       new AtomicConcept("is  " + instanceName + " height--50cm"), 0.2);
      add("width",     1.0, new AtomicConcept("is  " + instanceName + " width--10cm"));
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
    Bird(string conceptName = "#bird") : Animal(conceptName) {
      add("can-fly",    1.0, new AtomicConcept("can " + instanceName + " motion--fly"));
      add("can-walk",   0.9, new AtomicConcept("can " + instanceName + " motion--walk"));
      add("can-run",    0.9, new AtomicConcept("can " + instanceName + " motion--run"));
      add("can-bounce", 0.8, new AtomicConcept("can " + instanceName + " motion--bounce"));

      add("has-feathers", 0.8, new AtomicConcept("has " + instanceName + " part--feathers"));
      add("has-wings",    1.0, new AtomicConcept("has " + instanceName + " part--wings"));
      add("has-legs",     1.0, new AtomicConcept("has " + instanceName + " part--legs"));
      add("has-beak",     1.0, new AtomicConcept("has " + instanceName + " part--beak"));
    }
  };


  class Emu : public Bird {
  public:
    Emu(string conceptName = "#emu") : Bird(conceptName) {
      remove("can-fly");     // naughty emu!
      remove("can-bounce");

      add("height",   1.0, new AtomicConcept("is  " + instanceName + " height--2.0m"));
      add("width",    1.0, new AtomicConcept("is  " + instanceName + " width--50cm"));
      add("is-brown", 0.8, new AtomicConcept("is  " + instanceName + " colour--brown"));
    }
  };





  class Duck : public Bird {
  public:
    Duck(string conceptName = "#duck") : Bird(conceptName) {
      add("can-swim", 1.0, new AtomicConcept("can " + instanceName + " motion--swim"));

      add("height",   1.0, new AtomicConcept("is  " + instanceName + " height--30cm"));
      add("width",    1.0, new AtomicConcept("is  " + instanceName + " width--10cm"));
      add("is-brown", 0.8, new AtomicConcept("is  " + instanceName + " colour--brown"));
      add("is-black", 0.5, new AtomicConcept("is  " + instanceName + " colour--black"));
    }
  };


  class Magpie : public Bird {
  public:
    Magpie(string conceptName = "#magpie") : Bird(conceptName) {
      add("can-sing", 0.5, new AtomicConcept("can " + instanceName + " sound--sing"));

      add("height",   1.0, new AtomicConcept("is  " + instanceName + " height--30cm"));
      add("width",    1.0, new AtomicConcept("is  " + instanceName + " width--10cm"));
      add("is-black", 0.8, new AtomicConcept("is  " + instanceName + " colour--black"));
      add("is-white", 0.8, new AtomicConcept("is  " + instanceName + " colour--white"));
    }
  };


  class TwentyEight : public Bird {
  public:
    TwentyEight(string conceptName = "#twenty-eight") : Bird(conceptName) {
      add("can-sing", 0.5, new AtomicConcept("can " + instanceName + " sound--sing"));

      add("height",    1.0, new AtomicConcept("is  " + instanceName + " height--30cm"));
      add("width",     1.0, new AtomicConcept("is  " + instanceName + " width--5.0cm"));
      add("is-green",  1.0, new AtomicConcept("is  " + instanceName + " colour--green"));
      add("is-yellow", 0.8, new AtomicConcept("is  " + instanceName + " colour--yellow"));
      add("is-blue",   0.5, new AtomicConcept("is  " + instanceName + " colour--blue"));
    }
  };




  class Fish : public Animal {
  public:
    Fish(string conceptName = "#fish") : Animal(conceptName) {
      add("can-swim",   1.0, new AtomicConcept("can " + instanceName + " motion--swim"));

      add("has-gills",  1.0, new AtomicConcept("has " + instanceName + " part--gills"));
      add("has-scales", 1.0, new AtomicConcept("has " + instanceName + " part--scales"));
      //add("has-tail", 1.0, new AtomicConcept("has " + instanceName + " part--tail"));
    }
  };


  class Shark : public Fish {
  public:
    Shark(string conceptName = "#shark") : Fish(conceptName) {
      add("height",   1.0, new AtomicConcept("is  " + instanceName + " height--2.0m"));
      add("width",    1.0, new AtomicConcept("is  " + instanceName + " width--50cm"));
      add("is-grey",  1.0, new AtomicConcept("is  " + instanceName + " colour--grey"));
     }
  };


  class Salmon : public Fish {
  public:
    Salmon(string conceptName = "#salmon") : Fish(conceptName) {
      add("height", 1.0, new AtomicConcept("is  " + instanceName + " height--50cm"));
      add("width",  1.0, new AtomicConcept("is  " + instanceName + " width--10cm"));
      add("is-red", 1.0, new AtomicConcept("is  " + instanceName + " colour--red"));
    }
  };


  class ClownFish : public Fish {
  public:
    ClownFish(string conceptName = "#clown-fish") : Fish(conceptName) {
      add("height",    1.0, new AtomicConcept("is  " + instanceName + " height--10cm"));
      add("width",     1.0, new AtomicConcept("is  " + instanceName + " width--1.0cm"));
      add("is-orange", 1.0, new AtomicConcept("is  " + instanceName + " colour--orange"));
      add("is-white",  1.0, new AtomicConcept("is  " + instanceName + " colour--white"));
    }
  };


}

#endif
