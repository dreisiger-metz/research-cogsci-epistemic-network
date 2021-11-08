#include <stdio.h>
#include "Statistics.hpp"



int main(int argc, char **argv) {
  unsigned temp_u;
  Utilities::Statistics::Uniform Uniform;
  Utilities::Statistics::Normal Normal;

  if (argc == 2) {
    sscanf(argv[1], "%d", &temp_u);
    Uniform.seed(temp_u);
  }
  printf("seed is %d\n", Uniform.seed());
  
  for (int i = 0; i < 10; i++)
    printf("%1.12lf (seed = %d)\n", Uniform.unifrnd(), Uniform.seed());
    //printf("%1.12lf\n", n.normrnd());

  return 0;
}
