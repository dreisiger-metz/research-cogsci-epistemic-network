#include "PerceptronTest.hpp"


using namespace ANN;
using namespace ANN::Test;


void PerceptronTest::setUp() {
  learningRate = 0.01;
  momentum = 0.0;
  
  sigmoid = new Sigmoid();
  
  p_11 = new Perceptron("p_11", learningRate, momentum, 0.0, (ActivationFunction *) sigmoid);
  p_12 = new Perceptron("p_12", learningRate, momentum, 0.0, (ActivationFunction *) sigmoid);
  p_21 = new Perceptron("p_21", learningRate, momentum, 0.0, (ActivationFunction *) sigmoid);
  p_31 = new Perceptron("p_31", learningRate, momentum, 0.0, (ActivationFunction *) sigmoid);
  p_32 = new Perceptron("p_32", learningRate, momentum, 0.0, (ActivationFunction *) sigmoid);
}




void PerceptronTest::tearDown() {
  delete p_11;
  delete p_12;
  delete p_21;
  delete p_31;
  delete p_32;

  delete sigmoid;
}




void PerceptronTest::testConstructorDestructor() {
  double learningRate = 0.01, momentum = 0.0;
  
  Perceptron *percept = new Perceptron("local-perceptron", learningRate, momentum, 0.0, (ActivationFunction *) sigmoid);

  CPPUNIT_ASSERT_MESSAGE("Perceptron::Perceptron() failed to initialise its learning rate", percept->learningRate == learningRate);
  learningRate = 0.05;
  CPPUNIT_ASSERT_MESSAGE("Perceptron::learningRate is not a reference variable", percept->learningRate == learningRate);

  CPPUNIT_ASSERT_MESSAGE("Perceptron::Perceptron() failed to initialise its momentum", percept->momentum == momentum);
  momentum = 0.05;
  CPPUNIT_ASSERT_MESSAGE("Perceptron::momentum is not a reference variable", percept->momentum == momentum);

  CPPUNIT_ASSERT_MESSAGE("Perceptron::input.size() != 1", percept->input.size() == 1);
  CPPUNIT_ASSERT_MESSAGE("Perceptron::input[0] is not pointing to a unit bias", percept->input[0]->left->outputValue == 1.0);
  CPPUNIT_ASSERT_MESSAGE("Perceptron::Perceptron() failed to initialise the bias link", percept->input[0]->weight == 0.0);

  CPPUNIT_ASSERT_MESSAGE("Perceptron::previousInputWeight.size() != 1", percept->previousInputWeight.size() == 1);
  CPPUNIT_ASSERT_MESSAGE("Perceptron::previousInputWeight[0] != 0.0", percept->previousInputWeight[0] == 0.0);
  
  delete percept;
}




void PerceptronTest::testAddInput() {
  CPPUNIT_ASSERT_MESSAGE("Perceptron::input.size() != 1", p_21->input.size() == 1);
  CPPUNIT_ASSERT_MESSAGE("Perceptron::output.size() != 0", p_21->output.size() == 0);

  p_21->addInput(p_11, 0.2);
  p_21->addInput(p_12);

  CPPUNIT_ASSERT_MESSAGE("Perceptron::input.size() != 3", p_21->input.size() == 3);
  CPPUNIT_ASSERT_MESSAGE("*(Perceptron::input[1]) is incorrect", (p_21->input[1]->left == p_11) && (p_21->input[1]->right == p_21) && (p_21->input[1]->weight == 0.2));
  CPPUNIT_ASSERT_MESSAGE("*(Perceptron::input[2]) is incorrect", (p_21->input[2]->left == p_12) && (p_21->input[2]->right == p_21));
}




void PerceptronTest::testUpdateActivation() {
  // set up a simple 2:1:2 network
  p_21->addInput(p_11);
  p_21->addInput(p_12);

  p_31->addInput(p_21);
  p_32->addInput(p_21);
  
  // initialise the 'input' layer and p_32's bias
  p_11->outputValue = 0.25;
  p_12->outputValue = 0.75;
  p_32->input[0]->weight = 0.5;
  
  // and propagate these activations forward through the network
  p_21->updateActivation();
  p_31->updateActivation();
  p_32->updateActivation();
  
  CPPUNIT_ASSERT_MESSAGE("p_21->outputValue incorrect", p_21->outputValue == sigmoid->value((p_21->input[1]->weight * 0.25) + (p_21->input[2]->weight * 0.75)));
  CPPUNIT_ASSERT_MESSAGE("p_31->outputValue incorrect", p_31->outputValue == sigmoid->value(p_31->input[1]->weight * p_21->outputValue));
  CPPUNIT_ASSERT_MESSAGE("p_32->outputValue incorrect", p_32->outputValue == sigmoid->value(p_32->input[0]->weight + p_32->input[1]->weight * p_21->outputValue));
}




void PerceptronTest::testUpdateWeights() {
  // Set up a simple 2:1:2 network with specific link weights,
  p_21->addInput(p_11, 0.1);
  p_21->addInput(p_12, 0.2);
  
  p_31->addInput(p_21, 0.3);
  p_32->addInput(p_21, 0.4);
  
  // initialise the 'input' layer and propagate the activation,
  p_11->outputValue = 0.25;
  p_12->outputValue = 0.75;

  p_21->updateActivation();
  p_31->updateActivation();
  p_32->updateActivation();
  
  // zero the output neurons' error signal and call ::updateWeights...
  p_31->errorSignal = 0.0;
  p_32->errorSignal = 0.0;

  p_31->updateWeights();
  p_32->updateWeights();
  p_21->updateWeights();

  // ... nothing should happen
  CPPUNIT_ASSERT_MESSAGE("p_31's link weights changed", ((p_31->input[0]->weight == 0.0) && (p_31->input[1]->weight == 0.3)));
  CPPUNIT_ASSERT_MESSAGE("p_32's link weights changed", ((p_32->input[0]->weight == 0.0) && (p_32->input[1]->weight == 0.4)));
  CPPUNIT_ASSERT_MESSAGE("p_21's link weights changed", ((p_21->input[0]->weight == 0.0) && (p_21->input[1]->weight == 0.1) && (p_21->input[2]->weight == 0.2)));

  // Now, set the error signals to something non-zero
  p_31->errorSignal = 0.3;
  p_32->errorSignal = 0.5;

  p_31->updateWeights();
  p_32->updateWeights();
  p_21->updateWeights();
  
  // ... the link weights should change
  CPPUNIT_ASSERT_MESSAGE("p_31's link weights have not changed", ((p_31->input[0]->weight != 0.0) && (p_31->input[1]->weight != 0.3)));
  CPPUNIT_ASSERT_MESSAGE("p_32's link weights have not changed", ((p_32->input[0]->weight != 0.0) && (p_32->input[1]->weight != 0.4)));
  CPPUNIT_ASSERT_MESSAGE("p_21's link weights have not changed", ((p_21->input[0]->weight != 0.0) && (p_21->input[1]->weight != 0.1) && (p_21->input[2]->weight != 0.2)));
}




void PerceptronTest::testPrint() {
  #ifdef __DEBUG__INTERACTIVE_TESTS
  char answer[16];

  // Set up a simple 2:1:2 network with specific link weights,
  p_21->addInput(p_11, 0.1);
  p_21->addInput(p_12, 0.2);

  p_31->addInput(p_21, 0.3);
  p_32->addInput(p_21, 0.4);

  // initialise the 'input' layer and propagate the activation,
  p_11->outputValue = 0.25;
  p_12->outputValue = 0.75;

  p_21->updateActivation();
  p_31->updateActivation();
  p_32->updateActivation();

  answer[0] = 'y';
  printf("\nAbout to print [(Perceptron) p_21] with no indent:\n");
  p_21->print(stdout, "");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("Perceptron::print(...) failed to print correctly", answer[0] == 'y');

  answer[0] = 'y';
  printf("\nAbout to print [(Perceptron) p_21] with a two-space indent:\n");
  p_21->print(stdout, "  ");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("Perceptron::print(...) failed to print correctly", answer[0] == 'y');

  #endif
}




void PerceptronTest::testSaveLoad() {
  /*
  FILE *tmp = tmpfile();   // create a tmpfile

  // Initialise two perceptrons
  p_11->errorSignal = 0.1;
  p_11->previousInputWeight[0] = 0.2;

  p_12->errorSignal = 0.3;
  p_12->previousInputWeight[0] = 0.4;

  // and save them;
  CPPUNIT_ASSERT_MESSAGE("Perceptron::save failed to write any bytes to the output stream", p_11->save(tmp) > 0);
  CPPUNIT_ASSERT_MESSAGE("Perceptron::save failed to write any bytes to the output stream", p_12->save(tmp) > 0);
  fflush(tmp);

  // rewind tmp and load them back into the perceptrons (the other way around)
  fseek(tmp, 0, SEEK_SET);
  CPPUNIT_ASSERT_MESSAGE("Perceptron::load failed to read any bytes from the input stream", p_12->load(tmp) > 0);
  CPPUNIT_ASSERT_MESSAGE("Perceptron::load failed to read any bytes from the input stream", p_11->load(tmp) > 0);

  // (Note that the other variables are tested by the NeuronTest class...)
  CPPUNIT_ASSERT_MESSAGE("p_11->errorSignal incorrect after load", p_11->errorSignal == 0.3);
  CPPUNIT_ASSERT_MESSAGE("p_11->PreviousInputWeight incorrect after load", p_11->previousInputWeight[0] == 0.4);

  CPPUNIT_ASSERT_MESSAGE("p_12->errorSignal incorrect after load", p_12->errorSignal == 0.1);
  CPPUNIT_ASSERT_MESSAGE("p_12->PreviousInputWeight incorrect after load", p_12->previousInputWeight[0] == 0.2);
  
  fclose(tmp);
  */
}
