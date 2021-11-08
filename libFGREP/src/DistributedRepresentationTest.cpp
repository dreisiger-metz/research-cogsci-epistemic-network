#include "DistributedRepresentationTest.hpp"

using namespace std;
using namespace ANN;
using namespace ANN::Test;


const unsigned DistributedRepresentationTest::LayerSize = 36;




void DistributedRepresentationTest::setUp() {
  char label[16];

  for (unsigned i = 0; i < LayerSize; i++) {
    sprintf(label, "p-%d", i);
    m_layer.push_back(new Perceptron(label, m_learningRate, m_momentum, 0.0, &m_sigmoid));
  }
}




void DistributedRepresentationTest::tearDown() {
  for (unsigned i = 0; i < LayerSize; i++)
    delete m_layer[i];
}




void DistributedRepresentationTest::testConstructorDestructor() {
  // We don't need to test DistributedRepresentationBase's constructor
  // or destructor as they're trivial --- instead we're going to test
  // DistributedRepresentation and FixedSources'
  unsigned i, flag;
  double mean, variance;
  DistributedRepresentation d("local-distributed", LayerSize);
  FixedRepresentation       f("local-fixed", LayerSize, 0.5);

  // Check out [d],
  for (mean = 0.0, i = 0; i < d.size(); i++)
    mean += d[i];
  mean /= d.size();

  for (variance = 0.0, flag = 0, i = 0; i < d.size(); i++) {
    variance += (d[i] - mean) * (d[i] - mean);
    flag += (d[i] == 0.0);
  }
  CPPUNIT_ASSERT_MESSAGE("The variance of DistributedRepresentation::representation is zero", variance > 0.0);
  CPPUNIT_ASSERT_MESSAGE("All of DistributedRepresentation::representation's elements are zero", flag < d.size());

  
  // and check out [f]
  for (flag = 0, i = 0; i < f.size(); i++)
    flag += (f[i] != 0.5);
  CPPUNIT_ASSERT_MESSAGE("FixedRepresentation::FixedRepresentation failed to initialise ::representation correctly", flag == 0);

  vector<double> vals;
  vals.push_back(0.0);
  vals.push_back(0.1);
  vals.push_back(0.2);
  vals.push_back(0.3);
  FixedRepresentation fv("local-fixed2", LayerSize, vals);
  for (flag = 0, i = 0; i < fv.size(); i++)
    flag += (fv[i] != vals[(i < vals.size())?i:vals.size() - 1]);
  CPPUNIT_ASSERT_MESSAGE("FixedRepresentation::FixedRepresentation failed to initialise ::representation correctly", flag == 0);
}




void DistributedRepresentationTest::testLoadSaveRepresentation() {
  unsigned i, flag;
  DistributedRepresentation input("local-input", LayerSize);
  FixedRepresentation       fixed("fixed-input", LayerSize, 0.8);

  // Initialise input and output
  for (i = 0; i < LayerSize; i++)
    input[i] = 0.1 + 0.05 * i;

  // Test ::loadRepresentation
  flag = 0;
  try {
    // try loading past the end of [m_layer]
    input.loadRepresentation(m_layer, 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("DistributedRepresentation::loadRepresentation() failed to throw an exception for an invalid offset", flag == 1);

  flag = 0;
  try {
    input.loadRepresentation(m_layer, 0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("DistributedRepresentation::loadRepresentation() threw an exception for a valid offset", flag == 0);

  for (flag = 0, i = 0; i < LayerSize; i++)
    flag += (m_layer[i]->outputValue != input[i]);
  CPPUNIT_ASSERT_MESSAGE("DistributedRepresentation::loadRepresentation() failed to set the layer's outputs correctly", flag == 0);


  // Test ::saveRepresentation
  for (i = 0; i < LayerSize; i++)
    m_layer[i]->outputValue = 0.0;
  
  flag = 0;
  try {
    input.saveRepresentation(m_layer, 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("DistributedRepresentation::saveRepresentation() failed to throw an exception for an invalid offset", flag == 1);
  flag = 0;
  try {
    input.saveRepresentation(m_layer, 0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("DistributedRepresentation::saveRepresentation() threw an exception for a valid offset", flag == 0);

  for (flag = 0, i = 0; i < LayerSize; i++)
    flag += (m_layer[i]->outputValue != input[i]);
  CPPUNIT_ASSERT_MESSAGE("DistributedRepresentation::saveRepresentation() failed to set the distributed representation", flag == 0);

  // and the same for FixedRepresentation::saveRepresentation
  flag = 0;
  try {
    fixed.saveRepresentation(m_layer, 0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("FixedRepresentation::saveRepresentation() threw an exception for a valid offset", flag == 0);
  for (flag = 0, i = 0; i < LayerSize; i++)
    flag += (fixed[i] != 0.8);
  CPPUNIT_ASSERT_MESSAGE("FixedRepresentation::saveRepresentation() overwrote the contents of ::representation", flag == 0);
}




void DistributedRepresentationTest::testSetErrorSignals() {
  unsigned i, flag;
  DistributedRepresentation output("local-output", LayerSize);


  // Initialise input and output
  for (i = 0; i < LayerSize; i++) {
    m_layer[i]->outputValue = 0.1;
    m_layer[i]->errorSignal = 0.0;
    output[i] = 0.1 + 0.025 * i;
  }


  flag = 0;
  try {
    output.setErrorSignals(m_layer, 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("DistributedRepresentation::setErrorSignals() failed to throw an exception for an invalid offset", flag == 1);

  flag = 0;
  try {
    output.setErrorSignals(m_layer, 0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("DistributedRepresentation::setErrorSignals() threw an exception for a correctly sized vector", flag == 0);
  
  //  for (flag = 0, i = 0; i < LayerSize; i++)
  //    printf("m_layer[i]->outputValue == %lf, m_layer[i]->errorSignal == %lf, output[i] == %lf\n", m_layer[i]->outputValue, m_layer[i]->errorSignal, output[i]);
  
  for (flag = 0, i = 0; i < LayerSize; i++)
    flag += (m_layer[i]->errorSignal - (output[i] - m_layer[i]->outputValue) > 10e-15);   // don't know why but I'm getting rounding errors of ~10E-17
  CPPUNIT_ASSERT_MESSAGE("DistributedRepresentation::setErrorSignals() failed to set the layer's outputs correctly", flag == 0);
}




void DistributedRepresentationTest::testDistributedSentenceRepresentation() {
  unsigned i, j, flag;
  vector<DistributedRepresentationBase*> dr;
  DistributedSentenceRepresentation *dsr;

  dr.push_back(new DistributedRepresentation("relation", LayerSize / 3));
  dr.push_back(new DistributedRepresentation("argument-1", LayerSize / 3));
  dr.push_back(new DistributedRepresentation("argument-2", LayerSize / 3));

  dsr = new DistributedSentenceRepresentation(1.0, 3, dr[0], dr[1], dr[2]); //, dr[3], dr[4]);
  CPPUNIT_ASSERT_MESSAGE("DistributedSentenceRepresentation::DistributedSentenceRepresentation(unsigned, ...) failed", ((dsr->weight == 1.0) && (dsr->numberOfTerms == 3) && (dsr->term[0] == dr[0]) && (dsr->term[1] == dr[1]) && (dsr->term[2] == dr[2])));
  delete dsr;

  dsr = new DistributedSentenceRepresentation(0.8, dr);
  CPPUNIT_ASSERT_MESSAGE("DistributedSentenceRepresentation::DistributedSentenceRepresentation(vector<...>) failed", ((dsr->weight == 0.8) && (dsr->numberOfTerms == 3) && (dsr->term[0] == dr[0]) && (dsr->term[1] == dr[1]) && (dsr->term[2] == dr[2])));


  // clear m_layer's outputs
  for (i = 0; i < LayerSize; i++)
    m_layer[i]->outputValue = 0.0;

  // load the random representations into m_layer
  dsr->loadRepresentations(m_layer);
  for (flag = 0, i = 0; i < dsr->term.size(); i++)
    for (j = 0; j < LayerSize / dsr->term.size(); j++)
      flag += (m_layer[(LayerSize / dsr->term.size()) * i + j]->outputValue != (*(dsr->term[i]))[j]);

  CPPUNIT_ASSERT_MESSAGE("DistributedSentenceRepresentation::loadRepresentations() failed to set the layer's outputs correctly", flag == 0);


  // reinitialise m_layer's outputs
  for (i = 0; i < LayerSize; i++)
    m_layer[i]->outputValue = ((double) i ) / LayerSize;

  // and save the neurons' outputs back into the distributed representations
  dsr->saveRepresentations(m_layer);
  for (flag = 0, i = 0; i < dsr->term.size(); i++)
    for (j = 0; j < LayerSize / dsr->term.size(); j++)
      flag += (m_layer[(LayerSize / dsr->term.size()) * i + j]->outputValue != (*(dsr->term[i]))[j]);

  CPPUNIT_ASSERT_MESSAGE("DistributedSentenceRepresentation::loadRepresentations() failed to save the layer's outputs correctly", flag == 0);


  // reinitialise m_layer's outputs
  for (i = 0; i < LayerSize; i++)
    m_layer[i]->outputValue = 0.1;

  // and save the neurons' outputs back into the distributed representations
  dsr->setErrorSignals(m_layer);
  for (flag = 0, i = 0; i < dsr->term.size(); i++)
    for (j = 0; j < LayerSize / dsr->term.size(); j++)
      flag += (m_layer[(LayerSize / dsr->term.size()) * i + j]->errorSignal - ((*(dsr->term[i]))[j] - m_layer[(LayerSize / dsr->term.size()) * i + j]->outputValue) > 10e-15);

  CPPUNIT_ASSERT_MESSAGE("DistributedSentenceRepresentation::setErrorSignals() failed to set the layer's error signals correctly", flag == 0);

  delete dsr;
}
