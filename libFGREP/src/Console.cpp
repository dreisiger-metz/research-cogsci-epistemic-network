#include <stdio.h>
#include <stdlib.h>

#include <string>
#include <readline/readline.h>
#include <readline/history.h>

#include "FGREPConsole.hpp"

using namespace std;
using namespace ANN;


int main(int argc, char **argv) {
  unsigned i = 1, representationSize = 12, termsPerSentence = 3, hiddenLayerSize = 0, seed = 0;
  FILE *ifp = stdin;
  FGREPConsole *console;

  while (i < (unsigned) argc) {
    if (!strcmp("--representation-size", argv[i]))
      sscanf(argv[++i], "%d", &representationSize);
    else if (!strcmp("--terms-per-sentence", argv[i]))
      sscanf(argv[++i], "%d", &termsPerSentence);
    else if (!strcmp("--hidden-layer-size", argv[i]))
      sscanf(argv[++i], "%d", &hiddenLayerSize);
    else if (!strcmp("--seed", argv[i]))
      sscanf(argv[++i], "%d", &seed);
    else if (!strcmp("--load-commands", argv[i])) {
      ifp = fopen(argv[++i], "r");
      if (ifp == NULL) {
	printf("error opening file '%s'\n", argv[1]);
	return 1;
      }
    } else {
      printf("unknown argument -- usage is:\n    %s [--representation-size s] [--terms-per-sentence n] [--hidden-layer-size h] [--seed v] [--load-commands <command-filename>]\n", argv[0]);
      return 2;
    }

    i++;
  }
  

  console = new FGREPConsole(representationSize, termsPerSentence, hiddenLayerSize, seed, ifp, stdout);
  console->runLoop();

  delete console;

  return 0;
}
