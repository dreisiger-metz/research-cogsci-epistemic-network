#include "MultilayerPerceptronNetworkTest.hpp"

using namespace std;
using namespace ANN;
using namespace ANN::Test;




void MultilayerPerceptronNetworkTest::setUp() {
  mlp = new MultilayerPerceptronNetwork("test-mlp", 0.05, 0.0, 3, 4, 4, 4);
}




void MultilayerPerceptronNetworkTest::tearDown() {
  delete mlp;
}




void MultilayerPerceptronNetworkTest::testConstructorDestructor() {
  MultilayerPerceptronNetwork *local;

  local = new MultilayerPerceptronNetwork("local-mlp", 0.05, 0.0, 3, 4, 3, 2);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::label was incorrectly set", local->label == "local-mlp");
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::learningRate was incorrectly set", local->learningRate == 0.05);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::momentum was incorrectly set", local->momentum == 0.0);

  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::perceptron.size() incorrect", local->perceptron.size() == 3);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::m_numberOfLayers incorrect", local->perceptron.size() == 3);
  CPPUNIT_ASSERT_MESSAGE("layer-1.size() incorrect", local->perceptron[0].size() == 4);
  CPPUNIT_ASSERT_MESSAGE("m_sizeOfInputLayer incorrect", local->m_sizeOfInputLayer == 4);
  CPPUNIT_ASSERT_MESSAGE("layer-2.size() incorrect", local->perceptron[1].size() == 3);
  CPPUNIT_ASSERT_MESSAGE("layer-3.size() incorrect", local->perceptron[2].size() == 2);
  CPPUNIT_ASSERT_MESSAGE("m_sizeOfOutputLayer incorrect", local->m_sizeOfOutputLayer == 2);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::inputPattern.size() incorrect", local->inputPattern.size() == 0);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::targetPattern.size() incorrect", local->targetPattern.size() == 0);
  
  delete local;


  local = new MultilayerPerceptronNetwork("local-mlp", 0.05, 0.0, 4, 4, 3, 2, 1);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::perceptron.size() incorrect", local->perceptron.size() == 4);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::m_numberOfLayers incorrect", local->perceptron.size() == 4);
  CPPUNIT_ASSERT_MESSAGE("layer-1.size() incorrect", local->perceptron[0].size() == 4);
  CPPUNIT_ASSERT_MESSAGE("m_sizeOfInputLayer incorrect", local->m_sizeOfInputLayer == 4);
  CPPUNIT_ASSERT_MESSAGE("layer-2.size() incorrect", local->perceptron[1].size() == 3);
  CPPUNIT_ASSERT_MESSAGE("layer-3.size() incorrect", local->perceptron[2].size() == 2);
  CPPUNIT_ASSERT_MESSAGE("layer-4.size() incorrect", local->perceptron[3].size() == 1);
  CPPUNIT_ASSERT_MESSAGE("m_sizeOfOutputLayer incorrect", local->m_sizeOfOutputLayer == 1);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::inputPattern.size() incorrect", local->inputPattern.size() == 0);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::targetPattern.size() incorrect", local->targetPattern.size() == 0);

  delete local;
}




void MultilayerPerceptronNetworkTest::testAddTrainingData() {
  unsigned flag;
  FeatureVector input("input", 3), target("target", 4);

  input[0] = 0.0;    input[1] = 0.25;  input[2] = 0.5;
  target[0] = 0.25;  target[1] = 0.5;  target[2] = 0.75;  target[3] = 1.0;

  flag = 0;
  try {
    mlp->addTrainingDatum(input, target);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::addTrainingDatum failed to throw an exception for a invalid input vector", flag == 1);

  input.representation.push_back(0.75);
  flag = 0;
  try {
    mlp->addTrainingDatum(input, target);
  } catch (Exception::BaseException &e) {
    printf("Exception caught - %s\n", e.comment.c_str());
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::addTrainingDatum threw an exception for a valid input--target pair", flag == 0);

  target.representation.push_back(1.25);
  flag = 0;
  try {
    mlp->addTrainingDatum(input, target);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::addTrainingDatum failed to throw an exception for a invalid target vector", flag == 1);
}




void MultilayerPerceptronNetworkTest::testTrainNetworkIterative() {
  double oldError, newError;
  FeatureVector in("in", 4), out("out", 4);
  vector<FeatureVector> inputs;

  // create our input vectors
  in[0] = 0.0;  in[1] = 0.0;  in[2] = 0.0;  in[3] = 0.0;  inputs.push_back(in);
  in[0] = 0.0;  in[1] = 1.0;  in[2] = 0.0;  in[3] = 0.0;  inputs.push_back(in);
  in[0] = 0.0;  in[1] = 0.0;  in[2] = 1.0;  in[3] = 0.2;  inputs.push_back(in);
  in[0] = 0.0;  in[1] = 0.0;  in[2] = 1.0;  in[3] = 0.5;  inputs.push_back(in);

  // add them to the network (assuming auto-association)
  mlp->addTrainingDatum(inputs[0], inputs[0]);
  mlp->addTrainingDatum(inputs[1], inputs[1]);
  mlp->addTrainingDatum(inputs[2], inputs[2]);
  mlp->addTrainingDatum(inputs[3], inputs[3]);

  // call ::trainNetwork once and note the error it returns
  oldError = mlp->trainNetwork((unsigned) 1);

  newError = mlp->trainNetwork((unsigned) 999);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::trainNetwork() has increased its error", newError < oldError);
  oldError = newError;

  newError = mlp->trainNetwork((unsigned) 9000);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::trainNetwork() has increased its error", newError < oldError);
  oldError = newError;
}




void MultilayerPerceptronNetworkTest::testTrainNetworkErrorThreshold() {
  double oldError, newError;
  FeatureVector in("in", 4), out("out", 4);
  vector<FeatureVector> inputs;

  // create our input vectors
  in[0] = 0.0;  in[1] = 0.0;  in[2] = 0.0;  in[3] = 0.0;  inputs.push_back(in);
  in[0] = 0.0;  in[1] = 1.0;  in[2] = 0.0;  in[3] = 0.0;  inputs.push_back(in);
  in[0] = 0.0;  in[1] = 0.0;  in[2] = 1.0;  in[3] = 0.2;  inputs.push_back(in);
  in[0] = 0.0;  in[1] = 0.0;  in[2] = 1.0;  in[3] = 0.5;  inputs.push_back(in);

  // add them to the network (assuming auto-association)
  mlp->addTrainingDatum(inputs[0], inputs[0]);
  mlp->addTrainingDatum(inputs[1], inputs[1]);
  mlp->addTrainingDatum(inputs[2], inputs[2]);
  mlp->addTrainingDatum(inputs[3], inputs[3]);

  // call ::trainNetwork with a large error threshold and note the actual value it returns
  oldError = mlp->trainNetwork(5.0);
  newError = mlp->trainNetwork(oldError / 10.0);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::trainNetwork() failed to reach lower error threshold", newError < oldError);
  oldError = newError;

  //newError = mlp->trainNetwork(oldError / 10.0);
  //CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::trainNetwork() failed to reach lower error threshold", newError < oldError);
}




void MultilayerPerceptronNetworkTest::testPrintNetwork() {
  #ifdef __DEBUG__INTERACTIVE_TESTS
  char answer[16];

  answer[0] = 'y';
  printf("\nAbout to print [(MultilayerPerceptronNetwork) mlp] with no indent:\n");
  mlp->print(stdout, "");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::print(...) failed to print correctly", answer[0] == 'y');

  answer[0] = 'y';
  printf("\nAbout to print [(MultilayerPerceptronNetwork) mlp] with a two-space indent:\n");
  mlp->print(stdout, "  ");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("MultilayerPerceptronNetwork::print(...) failed to print correctly", answer[0] == 'y');

  #endif
}




void MultilayerPerceptronNetworkTest::testSaveLoadNetwork() {

}
