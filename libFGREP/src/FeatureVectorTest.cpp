#include "FeatureVectorTest.hpp"

using namespace std;
using namespace ANN;
using namespace ANN::Test;


const unsigned FeatureVectorTest::LayerSize = 10;

void FeatureVectorTest::setUp() {
  char label[16];

  for (unsigned i = 0; i < LayerSize; i++) {
    sprintf(label, "p-%d", i);
    m_layer.push_back(new Perceptron(label, m_learningRate, m_momentum, 0.0, &m_sigmoid));
  }
}




void FeatureVectorTest::tearDown() {
  for (unsigned i = 0; i < LayerSize; i++)
    delete m_layer[i];
}




void FeatureVectorTest::testConstructorDestructor() {
  FeatureVector *f;

  f = new FeatureVector("local-test", 6);
  CPPUNIT_ASSERT_MESSAGE("FeatureVector::FeatureVector(...) failed to set the ::label correctly", f->label == "local-test");
  CPPUNIT_ASSERT_MESSAGE("FeatureVector::FeatureVector(...) failed to initialise ::representation correctly", f->representation.size() == 6);
  delete f;

  f = new FeatureVector("local-test", 10);
  CPPUNIT_ASSERT_MESSAGE("FeatureVector::FeatureVector(...) failed to initialise ::representation correctly", f->representation.size() == 10);
  delete f;
}




void FeatureVectorTest::testLoadSaveRepresentation() {
  unsigned i, flag;
  vector<Perceptron*> wrongLayer;
  FeatureVector input("local-input", LayerSize);


  // Initialise input and output
  for (i = 0; i < LayerSize; i++) {
    m_layer[i]->outputValue = 0.0;
    input[i] = 0.1 + 0.05 * i;
  }

  // and wrongLayer with != LayerSize perceptrons
  wrongLayer.push_back(new Perceptron("w-1", m_learningRate, m_momentum, 0.0, &m_sigmoid));
  wrongLayer.push_back(new Perceptron("w-2", m_learningRate, m_momentum, 0.0, &m_sigmoid));


  // Test ::loadRepresentation
  flag = 0;
  try {
    input.loadRepresentation(wrongLayer);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("FeatureVector::loadRepresentation() failed to throw an exception for an incorrectly sized vector", flag == 1);

  flag = 0;
  try {
    input.loadRepresentation(m_layer);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("FeatureVector::loadRepresentation() threw an exception for a correctly sized vector", flag == 0);

  for (flag = 0, i = 0; i < LayerSize; i++)
    flag += (m_layer[i]->outputValue != input[i]);
  CPPUNIT_ASSERT_MESSAGE("FeatureVector::loadRepresentation() failed to set the layer's outputs correctly", flag == 0);


  // Test ::saveRepresentation
  for (i = 0; i < LayerSize; i++)
    m_layer[i]->outputValue = 0.0;
  
  flag = 0;
  try {
    input.saveRepresentation(wrongLayer);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("FeatureVector::saveRepresentation() failed to throw an exception for an incorrectly sized vector", flag == 1);
  flag = 0;
  try {
    input.saveRepresentation(m_layer);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("FeatureVector::saveRepresentation() threw an exception for a correctly sized vector", flag == 0);

  for (flag = 0, i = 0; i < LayerSize; i++)
    flag += (m_layer[i]->outputValue != input[i]);
  CPPUNIT_ASSERT_MESSAGE("FeatureVector::saveRepresentation() failed to set the feature vector correctly", flag == 0);
}




void FeatureVectorTest::testSetErrorSignals() {
  unsigned i, flag;
  vector<Perceptron*> wrongLayer;
  FeatureVector output("local-output", LayerSize);


  // Initialise input and output
  for (i = 0; i < LayerSize; i++) {
    m_layer[i]->outputValue = 0.1;
    m_layer[i]->errorSignal = 0.0;
    output[i] = 0.1 + 0.025 * i;
  }

  // and wrongLayer with != LayerSize perceptrons
  wrongLayer.push_back(new Perceptron("w-1", m_learningRate, m_momentum, 0.0, &m_sigmoid));
  wrongLayer.push_back(new Perceptron("w-2", m_learningRate, m_momentum, 0.0, &m_sigmoid));

  flag = 0;
  try {
    output.setErrorSignals(wrongLayer);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("FeatureVector::setErrorSignals() failed to throw an exception for an incorrectly sized vector", flag == 1);

  flag = 0;
  try {
    output.setErrorSignals(m_layer);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("FeatureVector::setErrorSignals() threw an exception for a correctly sized vector", flag == 0);

  for (flag = 0, i = 0; i < LayerSize; i++)
    flag += (m_layer[i]->errorSignal != (output[i] - 0.1));
  CPPUNIT_ASSERT_MESSAGE("FeatureVector::setErrorSignals() failed to set the layer's outputs correctly", flag == 0);
}
