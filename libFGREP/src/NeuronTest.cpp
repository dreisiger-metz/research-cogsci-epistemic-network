#include "NeuronTest.hpp"

using namespace ANN;
using namespace ANN::Test;


void NeuronTest::testConstructorDestructor() {
  Neuron *localNeuron = new Neuron("local-neuron");

  CPPUNIT_ASSERT_MESSAGE("Neuron::Neuron() failed to initialise its output value", localNeuron->outputValue == 0.0);
  CPPUNIT_ASSERT_MESSAGE("Neuron::Neuron() failed to initialise its local field", localNeuron->localField == 0.0);
  CPPUNIT_ASSERT_MESSAGE("Neuron::Neuron() failed to initialise its local gradient", localNeuron->localGradient == 0.0);

  CPPUNIT_ASSERT_MESSAGE("Neuron::Neuron::input's size is non-zero", localNeuron->input.size() == 0);
  CPPUNIT_ASSERT_MESSAGE("Neuron::Neuron::output's size is non-zero", localNeuron->output.size() == 0);
  
  delete localNeuron;
}




void NeuronTest::testSaveLoad() {
  /*
  FILE *tmp = tmpfile();   // create a tmpfile
  Neuron *neuron2 = new Neuron("neuron-2");

  // Initialise the second neuron
  neuron2->outputValue = 0.5;
  neuron2->localField = 0.6;
  neuron2->localGradient = 0.7;
  
  // and save them;
  CPPUNIT_ASSERT_MESSAGE("Neuron::save failed to write any bytes to the output stream", neuron->save(tmp) > 0);
  CPPUNIT_ASSERT_MESSAGE("Neuron::save failed to write any bytes to the output stream", neuron2->save(tmp) > 0);
  fflush(tmp);

  // rewind tmp and load them back into the neuron (the other way around)
  fseek(tmp, 0, SEEK_SET);
  CPPUNIT_ASSERT_MESSAGE("Neuron::load failed to read any bytes from the input stream", neuron2->load(tmp) > 0);
  CPPUNIT_ASSERT_MESSAGE("Neuron::load failed to read any bytes from the input stream", neuron->load(tmp) > 0);

  CPPUNIT_ASSERT_MESSAGE("neuron->label incorrect after load", neuron->label == "neuron-2");
  CPPUNIT_ASSERT_MESSAGE("neuron->outputValue incorrect after load", neuron->outputValue == 0.5);
  CPPUNIT_ASSERT_MESSAGE("neuron->localField incorrect after load", neuron->localField == 0.6);
  CPPUNIT_ASSERT_MESSAGE("neuron->localGradient incorrect after load", neuron->localGradient == 0.7);
  CPPUNIT_ASSERT_MESSAGE("neuron2->label incorrect after load", neuron2->label == "neuron");
  CPPUNIT_ASSERT_MESSAGE("neuron2->outputValue incorrect after load", neuron2->outputValue == 0.0);
  CPPUNIT_ASSERT_MESSAGE("neuron2->localField incorrect after load", neuron2->localField == 0.0);
  CPPUNIT_ASSERT_MESSAGE("neuron2->localGradient incorrect after load", neuron2->localGradient == 0.0);

  fclose(tmp);
  */
}
