#include <sys/time.h>

#include "Concept.hpp"
#include "Living.hpp"
#include "Environment.hpp"


using namespace EpisodeGenerator;



class TermTestingEpisode : public Concept {
public:
  TermTestingEpisode(std::string conceptName = "#full-term-testing-episode") : Concept(conceptName) {
    add("tree", 1.0, new Tree());
    add("shrub", 1.0, new Shrub());
    add("flower", 1.0, new Flower());
    add("grass", 1.0, new Grass());
    add("dog", 1.0, new Dog());
    add("cat", 1.0, new Cat());
    add("robin", 1.0, new Robin());
    add("magpie", 1.0, new Magpie());
    add("sunfish", 1.0, new Sunfish());
    add("salmon", 1.0, new Salmon());
  }
};


class FullTermTestingEpisode : public Concept {
public:
  FullTermTestingEpisode(std::string conceptName = "#full-term-testing-episode") : Concept(conceptName) {
    add("living-thing", 1.0, new LivingThing());
    add("plant", 1.0, new Plant());
    add("tree", 1.0, new Tree());
    add("shrub", 1.0, new Shrub());
    add("flower", 1.0, new Flower());
    add("grass", 1.0, new Grass());
    add("animal", 1.0, new Animal());
    add("mammal", 1.0, new Mammal());
    add("dog", 1.0, new Dog());
    add("cat", 1.0, new Cat());
    add("bird", 1.0, new Bird());
    add("robin", 1.0, new Robin());
    add("magpie", 1.0, new Magpie());
    add("fish", 1.0, new Fish());
    add("sunfish", 1.0, new Sunfish());
    add("salmon", 1.0, new Salmon());
  }
};


class TestEpisode : public Episode {
public:
  TestEpisode(std::string conceptName = "test-episode") : Episode(conceptName) {
    Concept *predator, *prey;

    add("environment", 0.25, new Park());
    predator = addOneOf("predator", 1.0, 2, new Dog(), 0.4, new Cat(), 0.6);
    prey = addOneOf("prey", 1.0, 3, new Cat(), 0.4, new Robin(), 0.4, new Magpie(), 0.2);

    add("predator-chasing", 1.0, new AtomicConcept("is  " + predator->instanceName + " action-chasing"));
    add("prey-fleeing", 1.0, new AtomicConcept("is  " + prey->instanceName + " action-fleeing"));
  }
};




int main(int argc, char **argv) {
  struct timeval now;

  gettimeofday(&now, NULL);
  Concept::Uniform.seed(now.tv_sec);

 
  unsigned i, number = 10;
  TermTestingEpisode e[number];

  for (i = 0; i < number; i++) {
    e[i].print(stdout, "");
    printf("\n");
  }

  return 0;
}

