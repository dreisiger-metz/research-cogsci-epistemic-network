#include "cppunit/ui/text/TestRunner.h"
#include "NeuronTest.hpp"
#include "PerceptronTest.hpp"
#include "FeatureVectorTest.hpp"
#include "DistributedRepresentationTest.hpp"
#include "MultilayerPerceptronNetworkTest.hpp"
#include "FGREPNetworkTest.hpp"


int main(int argc, char **argv) {
  CppUnit::TextUi::TestRunner runner;

  runner.addTest(ANN::Test::NeuronTest::suite());
  runner.addTest(ANN::Test::PerceptronTest::suite());
  runner.addTest(ANN::Test::FeatureVectorTest::suite());
  runner.addTest(ANN::Test::DistributedRepresentationTest::suite());
  //runner.addTest(ANN::Test::MultilayerPerceptronNetworkTest::suite());
  runner.addTest(ANN::Test::FGREPNetworkTest::suite());

  runner.run();

  return 0;
}
