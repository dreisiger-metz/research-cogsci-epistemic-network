#include "FGREPNetworkTest.hpp"

using namespace std;
using namespace ANN;
using namespace ANN::Test;




void FGREPNetworkTest::setUp() {
  //          FGREPNetwork(<name>,      <lr> <mo> <dr><l><l1><l2><l3>
  fgrep = new FGREPNetwork("test-fgrep", 0.1, 0.0, 12, 3, 36, 18, 36);

  // From Rogers & McClelland (2004, p395)
  fgrep->addTerm(new FixedRepresentation("is", 12, 0.2));
  fgrep->addTerm(new FixedRepresentation("can", 12, 0.8));

  fgrep->declareTerm("#pine");
  fgrep->declareTerm("#oak");
  fgrep->declareTerm("#rose");
  fgrep->declareTerm("#daisy");
  fgrep->declareTerm("#robin");
  fgrep->declareTerm("#canary");
  fgrep->declareTerm("#sunfish");
  fgrep->declareTerm("#salmon");

  fgrep->declareTerm("pretty");
  fgrep->declareTerm("big");
  fgrep->declareTerm("living");
  fgrep->declareTerm("green");
  fgrep->declareTerm("red");
  fgrep->declareTerm("yellow");

  fgrep->declareTerm("grow");
  fgrep->declareTerm("move");
  fgrep->declareTerm("swim");
  fgrep->declareTerm("fly");
  fgrep->declareTerm("sing");

  fgrep->declareTerm("skin");
  fgrep->declareTerm("roots");
  fgrep->declareTerm("leaves");
  fgrep->declareTerm("bark");
  fgrep->declareTerm("branch");
  fgrep->declareTerm("petals");
  fgrep->declareTerm("wings");
  fgrep->declareTerm("feathers");
  fgrep->declareTerm("gills");
  fgrep->declareTerm("scales");

  try {
    fgrep->addSentence("is #pine big");
    fgrep->addSentence("is #pine living");
    fgrep->addSentence("is #pine green");
    fgrep->addSentence("is #oak big");
    fgrep->addSentence("is #oak living");
    fgrep->addSentence("is #rose pretty");
    fgrep->addSentence("is #rose living");
    fgrep->addSentence("is #rose red");
    fgrep->addSentence("is #daisy pretty");
    fgrep->addSentence("is #daisy living");
    fgrep->addSentence("is #daisy yellow");
    fgrep->addSentence("is #robin living");
    fgrep->addSentence("is #robin red");
    fgrep->addSentence("is #canary living");
    fgrep->addSentence("is #canary yellow");
    fgrep->addSentence("is #sunfish living");
    fgrep->addSentence("is #sunfish yellow");
    fgrep->addSentence("is #salmon living");
    fgrep->addSentence("is #salmon red");
    fgrep->addSentence("can #pine grow");
    fgrep->addSentence("can #oak grow");
    fgrep->addSentence("can #rose grow");
    fgrep->addSentence("can #daisy grow");
    fgrep->addSentence("can #robin grow");
    fgrep->addSentence("can #robin move");
    fgrep->addSentence("can #robin fly");
    fgrep->addSentence("can #canary grow");
    fgrep->addSentence("can #canary move");
    fgrep->addSentence("can #canary fly");
    fgrep->addSentence("can #canary sing");
    fgrep->addSentence("can #sunfish grow");
    fgrep->addSentence("can #sunfish move");
    fgrep->addSentence("can #sunfish swim");
    fgrep->addSentence("can #salmon grow", 1.0);
    fgrep->addSentence("can #salmon move", 0.9);
    fgrep->addSentence("can #salmon swim", 0.8);
  } catch (Exception::BaseException &e) {
    printf("Exception while adding sentence - '%s'\n", e.comment.c_str());
  }
}




void FGREPNetworkTest::tearDown() {
  delete fgrep;
}




void FGREPNetworkTest::testConstructorDestructor() {
  FGREPNetwork *local;

  local = new FGREPNetwork("local-fgrep", 0.1, 0.0, 8, 3, 8, 4, 8);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::label was incorrectly set", local->label == "local-fgrep");
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::learningRate was incorrectly set", local->learningRate == 0.1);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::momentum was incorrectly set", local->momentum == 0.0);

  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::perceptron.size() incorrect", local->perceptron.size() == 3);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::m_numberOfLayers incorrect", local->perceptron.size() == 3);
  CPPUNIT_ASSERT_MESSAGE("layer-1.size() incorrect", local->perceptron[0].size() == 8);
  CPPUNIT_ASSERT_MESSAGE("m_sizeOfInputLayer incorrect", local->m_sizeOfInputLayer == 8);
  CPPUNIT_ASSERT_MESSAGE("layer-2.size() incorrect", local->perceptron[1].size() == 4);
  CPPUNIT_ASSERT_MESSAGE("layer-3.size() incorrect", local->perceptron[2].size() == 8);
  CPPUNIT_ASSERT_MESSAGE("m_sizeOfOutputLayer incorrect", local->m_sizeOfOutputLayer == 8);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::termRepresentationMap.size() incorrect", local->termRepresentationMap.size() == 2);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::sentence.size() incorrect", local->sentence.size() == 0);
  
  delete local;


  local = new FGREPNetwork("local-fgrep", 0.1, 0.0, 8, 4, 8, 4, 4, 8);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::perceptron.size() incorrect", local->perceptron.size() == 4);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::m_numberOfLayers incorrect", local->perceptron.size() == 4);
  CPPUNIT_ASSERT_MESSAGE("layer-1.size() incorrect", local->perceptron[0].size() == 8);
  CPPUNIT_ASSERT_MESSAGE("m_sizeOfInputLayer incorrect", local->m_sizeOfInputLayer == 8);
  CPPUNIT_ASSERT_MESSAGE("layer-2.size() incorrect", local->perceptron[1].size() == 4);
  CPPUNIT_ASSERT_MESSAGE("layer-3.size() incorrect", local->perceptron[2].size() == 4);
  CPPUNIT_ASSERT_MESSAGE("layer-4.size() incorrect", local->perceptron[3].size() == 8);
  CPPUNIT_ASSERT_MESSAGE("m_sizeOfOutputLayer incorrect", local->m_sizeOfOutputLayer == 8);

  delete local;
}




void FGREPNetworkTest::testDeclareTerm() {
  unsigned flag, oldSize = fgrep->termRepresentationMap.size();

  flag = 0;
  try {
    fgrep->declareTerm("term-1");
    fgrep->declareTerm("term-2");
    fgrep->declareTerm("term-3");
    fgrep->declareTerm("term-4");
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::declareTerm threw an exception for a new term", flag == 0);

  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::declareTerm failed to add the correct number of terms", fgrep->termRepresentationMap.size() - oldSize == 4);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::declareTerm failed to add 'term-1' to the termRepresenationMap", 
			 fgrep->termRepresentationMap.find("term-1") != fgrep->termRepresentationMap.end());
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::declareTerm failed to add 'term-1' to the termRepresenationMap", 
			 fgrep->termRepresentationMap.find("term-1") != fgrep->termRepresentationMap.end());
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::declareTerm failed to add 'term-1' to the termRepresenationMap", 
			 fgrep->termRepresentationMap.find("term-1") != fgrep->termRepresentationMap.end());
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::declareTerm failed to add 'term-1' to the termRepresenationMap", 
			 fgrep->termRepresentationMap.find("term-1") != fgrep->termRepresentationMap.end());

  flag = 0;
  try {
    fgrep->declareTerm("term-1");
  } catch (Exception::BaseException &e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::declareTerm failed to throw an exception for an existing term", flag == 1);
}




void FGREPNetworkTest::testTrainNetworkByNumberOfIterationsIncremental() {
  double oldError, newError;

  // call ::trainNetwork once and note the error it returns
  oldError = fgrep->trainNetworkByNumberOfIterationsIncremental((unsigned) 1);
  newError = fgrep->trainNetworkByNumberOfIterationsIncremental((unsigned) 199);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::trainNetworkByNumberOfIterationsIncremental() has increased its error", newError < oldError);

  fgrep->learningRate = 0.05;
  oldError = newError;
  newError = fgrep->trainNetworkByNumberOfIterationsIncremental((unsigned) 300);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::trainNetworkByNumberOfIterationsIncremental() has increased its error", newError < oldError);

  fgrep->learningRate = 0.025;
  oldError = newError;
  newError = fgrep->trainNetworkByNumberOfIterationsIncremental((unsigned) 500);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::trainNetworkByNumberOfIterationsIncremental() has increased its error", newError < oldError);
}




void FGREPNetworkTest::testTrainNetworkByAverageErrorSquaredIncremental() {
  double oldError, newError;
  map<string, DistributedRepresentationBase*>::iterator j;

  // call ::trainNetwork with a large error threshold and note the actual value it returns
  oldError = fgrep->trainNetworkByAverageErrorSquaredIncremental(5.0);
  newError = fgrep->trainNetworkByAverageErrorSquaredIncremental(oldError / 10.0);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::trainNetworkByAverageErrorSquaredIncremental() failed to reach lower error threshold", newError < oldError);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::trainNetworkByAverageErrorSquaredIncremental() returned with too high an error", newError < oldError / 10.0);

  oldError = newError;
  newError = fgrep->trainNetworkByAverageErrorSquaredIncremental(oldError / 10.0);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::trainNetworkByAverageErrorSquaredIncremental() failed to reach lower error threshold", newError < oldError);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::trainNetwork(ByAverageErrorSquared) returned with too high an error", newError < oldError / 10.0);

  //oldError = newError;
  //newError = fgrep->trainNetworkByAverageErrorSquaredIncremental(oldError / 10.0);
  //CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::trainNetworkByAverageErrorSquaredIncremental() failed to reach lower error threshold", newError < oldError);
  //CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::trainNetworkByAverageErrorSquaredIncremental() returned with too high an error", newError < oldError / 10.0);
}







/*
void FGREPNetworkTest::testTrainNetworkByAverageAbsoluteDeltaR() {
  unsigned j, k;
  double averageAbsoluteDeltaR;
  vector<double> representation;
  vector<vector<double> > oldTermRepresentations;
  map<string, DistributedRepresentationBase*>::iterator i;

  // call ::trainNetwork with an average delta-r of 0.1%
  fgrep->trainNetworkByAverageAbsoluteDeltaR(0.001);

  // save the soon-to-be-old representations
  for (i = fgrep->termRepresentationMap.begin(); i != fgrep->termRepresentationMap.end(); i++) {
    representation.clear();
    for (j = 0; j < fgrep->m_sizeOfRepresentations; j++)
      representation.push_back(i->second->representation[j]);
    oldTermRepresentations.push_back(representation);
  }

  // train for one more epoch
  fgrep->trainNetworkByNumberOfIterationsIncremental(1);

  // and compare the two sets of representations
  for (j = 0, i = fgrep->termRepresentationMap.begin(); i != fgrep->termRepresentationMap.end(); j++, i++)
    for (k = 0; k < fgrep->m_sizeOfRepresentations; k++)
      averageAbsoluteDeltaR += fabs(i->second->representation[k] - oldTermRepresentations[j][k]);
  averageAbsoluteDeltaR /= fgrep->termRepresentationMap.size() * fgrep->m_sizeOfRepresentations;

  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::trainNetworkByAverageAbsoluteDeltaR() failed to reach the correct average absolute delta-r", averageAbsoluteDeltaR < 0.001);
}
*/




void FGREPNetworkTest::testPrintNetwork() {
  #ifdef __DEBUG__INTERACTIVE_TESTS
  char answer[16];

  answer[0] = 'y';
  printf("\nAbout to print [(FGREPNetwork) fgrep] with no indent:\n");
  fgrep->print(stdout, "");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::print(...) failed to print correctly", answer[0] == 'y');

  answer[0] = 'y';
  printf("\nAbout to print [(FGREPNetwork) fgrep] with a two-space indent:\n");
  fgrep->print(stdout, "  ");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::print(...) failed to print correctly", answer[0] == 'y');

  #endif
}




void FGREPNetworkTest::testSaveLoadNetwork() {
  unsigned i, j, k, flag;
  FILE *tmp = tmpfile();   // create a tmpfile
  FGREPNetwork *fgrep_2 = new FGREPNetwork("local-test-fgrep", 0.1, 0.0, 12, 3, 36, 18, 36);
  map<string, DistributedRepresentationBase*>::iterator t1, t2;

  // Train the network a few times,
  fgrep->trainNetworkByNumberOfIterationsIncremental((unsigned) 100);

  // and save it.
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::save() failed to write any bytes to the output stream", fgrep->save(tmp) > 0);
  fflush(tmp);

  // Rewind tmp and load it into a new FGREP network
  fseek(tmp, 0, SEEK_SET);
  flag = 0;
  try {
    CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::load() failed to read any bytes to the output stream", fgrep_2->load(tmp) > 0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::load() threw an exception while reading a valid save file", flag == 0);

  // Finally, compare the two sets of network weights, representations and training sets
  flag = 0;
  for (i = 0; i < fgrep->perceptron.size(); i++)
    for (j = 0; j < fgrep->perceptron[i].size(); j++)
      for (k = 0; k < fgrep->perceptron[i][j]->input.size(); k++) {
	flag += (fgrep->perceptron[i][j]->input[k]->weight != fgrep_2->perceptron[i][j]->input[k]->weight);
	flag += (fgrep->perceptron[i][j]->previousInputWeight[k] != fgrep_2->perceptron[i][j]->previousInputWeight[k]);
      }
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::load() failed to restore the current and previous weights correctly", flag == 0);

  flag = 0;
  for (t1 = fgrep->termRepresentationMap.begin(), t2 = fgrep_2->termRepresentationMap.begin();
       t1 != fgrep->termRepresentationMap.end(); t1++, t2++) {
    flag += (t1->second->label != t2->second->label);
    for (i = 0; i < t1->second->representation.size(); i++)
      flag += (t1->second->representation[i] != t2->second->representation[i]);
  }
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::load() failed to restore the distributed representations correctly", flag == 0);

  flag = 0;
  for (i = 0; i < fgrep->sentence.size(); i++)
    for (j = 0; j < fgrep->sentence[i]->numberOfTerms; j++)
      flag += (fgrep->sentence[i]->term[j]->label != fgrep_2->sentence[i]->term[j]->label);
  CPPUNIT_ASSERT_MESSAGE("FGREPNetwork::load() failed to restore the training sentences correctly", flag == 0);

  delete fgrep_2;
}

