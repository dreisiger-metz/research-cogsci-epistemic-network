#include <sys/time.h>

#include "Experiment-1A--Entities.hpp"


using namespace std;
using namespace EpisodeGenerator;


class Experiment_1A1_Generator : public Concept {
public:
  Experiment_1A1_Generator(string conceptName = "#experiment-1a1-episode") : Concept(conceptName) {
    add("jarrah", 1.0, new Jarrah());
    add("ghost-gum", 1.0, new GhostGum());
    add("wattle", 1.0, new Wattle());
    add("grass-tree", 1.0, new GrassTree());
    add("geraldton-wax", 1.0, new GeraldtonWax());
    add("bottle-brush", 1.0, new BottleBrush());
    add("kangaroo-paw", 1.0, new KangarooPaw());
    add("everlasting", 1.0, new Everlasting());
    add("grass", 1.0, new Grass());

    add("dog", 1.0, new Dog());
    add("cat", 1.0, new Cat());
    add("emu", 1.0, new Emu());
    add("duck", 1.0, new Duck());
    add("magpie", 1.0, new Magpie());
    add("twenty-eight", 1.0, new TwentyEight());
    add("shark", 1.0, new Shark());
    add("salmon", 1.0, new Salmon());
    add("clown-fish", 1.0, new ClownFish());
  }
};




int main(int argc, char **argv) {
  struct timeval now;

  gettimeofday(&now, NULL);
  Concept::Uniform.seed(now.tv_sec);

 
  unsigned i, number = 1;
  Experiment_1A1_Generator e[number];

  for (i = 0; i < number; i++) {
    e[i].print(stdout, "");
    printf("\n");
  }

  return 0;
}

