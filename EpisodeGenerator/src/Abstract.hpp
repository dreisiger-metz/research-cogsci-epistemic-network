// ============================================================================
// Filename          : $RCSfile: Living.hpp,v $
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
#ifndef EpisodeGenerator__Living_hpp
#define EpisodeGenerator__Living_hpp
#include "Concept.hpp"


namespace EpisodeGenerator {


  //  ========================================================================80
  /// @class    Class_0
  /// @brief    The base of class-0* classes
  //  ========================================================================80
  class Class_0 : public Concept {
  public:
    Class_0(std::string conceptName = "#class-0") : Concept(conceptName) {
      add("has-feature--0",  1.0,  new AtomicConcept("has " + instanceName + " feature--0"));
      add("has-ability--0",  1.0,  new AtomicConcept("can " + instanceName + " ability--0"));

      addOneOf("random-feature-0",  1.0, 2, 
	       new AtomicConcept("has " + instanceName + " feature--14"), 0.5,
	       new AtomicConcept("has " + instanceName + " feature--15"), 0.5);

      addOneOf("random-feature-2",  1.0, 4, 
	       new AtomicConcept("has " + instanceName + " feature--18"), 0.25,
	       new AtomicConcept("has " + instanceName + " feature--19"), 0.25,
	       new AtomicConcept("has " + instanceName + " feature--20"), 0.25,
	       new AtomicConcept("has " + instanceName + " feature--21"), 0.25);
      addOneOf("random-feature-3",  1.0, 4, 
	       new AtomicConcept("has " + instanceName + " feature--18"), 0.25,
	       new AtomicConcept("has " + instanceName + " feature--19"), 0.25,
	       new AtomicConcept("has " + instanceName + " feature--20"), 0.25,
	       new AtomicConcept("has " + instanceName + " feature--21"), 0.25);
    }
  };


  //  ========================================================================80
  /// @class    Class_1
  /// @brief    The base of class-1* classes
  //  ========================================================================80
  class Class_1 : public Concept {
  public:
    Class_1(std::string conceptName = "#class-1") : Concept(conceptName) {
      add("has-feature--1",  1.0,  new AtomicConcept("has " + instanceName + " feature--1"));
      add("has-ability--1",  1.0,  new AtomicConcept("can " + instanceName + " ability--1"));

      addOneOf("random-feature-1",  1.0, 2, 
	       new AtomicConcept("has " + instanceName + " feature--16"), 0.5,
	       new AtomicConcept("has " + instanceName + " feature--17"), 0.5);

      addOneOf("random-feature-4",  1.0, 4, 
	       new AtomicConcept("has " + instanceName + " feature--18"), 0.25,
	       new AtomicConcept("has " + instanceName + " feature--19"), 0.25,
	       new AtomicConcept("has " + instanceName + " feature--20"), 0.25,
	       new AtomicConcept("has " + instanceName + " feature--21"), 0.25);
      addOneOf("random-feature-5",  1.0, 4, 
	       new AtomicConcept("has " + instanceName + " feature--18"), 0.25,
	       new AtomicConcept("has " + instanceName + " feature--19"), 0.25,
	       new AtomicConcept("has " + instanceName + " feature--20"), 0.25,
	       new AtomicConcept("has " + instanceName + " feature--21"), 0.25);
    }
  };




  //  ========================================================================80
  /// @class    Class_00
  /// @brief    The base of class-00* classes
  //  ========================================================================80
  class Class_00 : public Class_0 {
  public:
    Class_00(std::string conceptName = "#class-00") : Class_0(conceptName) {
      add("has-feature--2",  1.0,  new AtomicConcept("has " + instanceName + " feature--2"));
      add("has-ability--2",  1.0,  new AtomicConcept("can " + instanceName + " ability--2"));
    }
  };


  //  ========================================================================80
  /// @class    Class_01
  /// @brief    The base of class-01* classes
  //  ========================================================================80
  class Class_01 : public Class_0 {
  public:
    Class_01(std::string conceptName = "#class-01") : Class_0(conceptName) {
      add("has-feature--3",  1.0,  new AtomicConcept("has " + instanceName + " feature--3"));
      add("has-ability--3",  1.0,  new AtomicConcept("can " + instanceName + " ability--3"));
    }
  };


  //  ========================================================================80
  /// @class    Class_10
  /// @brief    The base of class-10* classes
  //  ========================================================================80
  class Class_10 : public Class_1 {
  public:
    Class_10(std::string conceptName = "#class-10") : Class_1(conceptName) {
      add("has-feature--4",  1.0,  new AtomicConcept("has " + instanceName + " feature--3"));  // noise
      add("has-ability--4",  1.0,  new AtomicConcept("can " + instanceName + " ability--4"));
    }
  };


  //  ========================================================================80
  /// @class    Class_11
  /// @brief    The base of class-11* classes
  //  ========================================================================80
  class Class_11 : public Class_1 {
  public:
    Class_11(std::string conceptName = "#class-11") : Class_1(conceptName) {
      add("has-feature--5",  1.0,  new AtomicConcept("has " + instanceName + " feature--5"));
      add("has-ability--5",  1.0,  new AtomicConcept("can " + instanceName + " ability--5"));
    }
  };




  //  ========================================================================80
  /// @class    Class_000
  //  ========================================================================80
  class Class_000 : public Class_00 {
  public:
    Class_000(std::string conceptName = "#class-000") : Class_00(conceptName) {
      add("has-feature--6",  1.0,  new AtomicConcept("has " + instanceName + " feature--6"));
      add("has-ability--6",  1.0,  new AtomicConcept("can " + instanceName + " ability--6"));
    }
  };


  //  ========================================================================80
  /// @class    Class_001
  //  ========================================================================80
  class Class_001 : public Class_00 {
  public:
    Class_001(std::string conceptName = "#class-001") : Class_00(conceptName) {
      add("has-feature--7",  1.0,  new AtomicConcept("has " + instanceName + " feature--7"));
      add("has-ability--7",  1.0,  new AtomicConcept("can " + instanceName + " ability--7"));
    }
  };


  //  ========================================================================80
  /// @class    Class_010
  //  ========================================================================80
  class Class_010 : public Class_01 {
  public:
    Class_010(std::string conceptName = "#class-010") : Class_01(conceptName) {
      add("has-feature--8",  1.0,  new AtomicConcept("has " + instanceName + " feature--8"));
      add("has-ability--8",  1.0,  new AtomicConcept("can " + instanceName + " ability--8"));
    }
  };


  //  ========================================================================80
  /// @class    Class_011
  //  ========================================================================80
  class Class_011 : public Class_01 {
  public:
    Class_011(std::string conceptName = "#class-011") : Class_01(conceptName) {
      add("has-feature--9",  1.0,  new AtomicConcept("has " + instanceName + " feature--9"));
      add("has-ability--9",  1.0,  new AtomicConcept("can " + instanceName + " ability--9"));
    }
  };


  //  ========================================================================80
  /// @class    Class_100
  //  ========================================================================80
  class Class_100 : public Class_10 {
  public:
    Class_100(std::string conceptName = "#class-100") : Class_10(conceptName) {
      add("has-feature--10",  1.0,  new AtomicConcept("has " + instanceName + " feature--10"));
      add("has-ability--10",  1.0,  new AtomicConcept("can " + instanceName + " ability--10"));
    }
  };


  //  ========================================================================80
  /// @class    Class_101
  //  ========================================================================80
  class Class_101 : public Class_10 {
  public:
    Class_101(std::string conceptName = "#class-101") : Class_10(conceptName) {
      add("has-feature--11",  1.0,  new AtomicConcept("has " + instanceName + " feature--11"));
      add("has-ability--11",  1.0,  new AtomicConcept("can " + instanceName + " ability--11"));
    }
  };


  //  ========================================================================80
  /// @class    Class_110
  //  ========================================================================80
  class Class_110 : public Class_11 {
  public:
    Class_110(std::string conceptName = "#class-110") : Class_11(conceptName) {
      add("has-feature--12",  1.0,  new AtomicConcept("has " + instanceName + " feature--12"));
      add("has-ability--12",  1.0,  new AtomicConcept("can " + instanceName + " ability--12"));
    }
  };


  //  ========================================================================80
  /// @class    Class_111
  //  ========================================================================80
  class Class_111 : public Class_11 {
  public:
    Class_111(std::string conceptName = "#class-111") : Class_11(conceptName) {
      add("has-feature--13",  1.0,  new AtomicConcept("has " + instanceName + " feature--13"));
      add("has-ability--13",  1.0,  new AtomicConcept("can " + instanceName + " ability--13"));
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
