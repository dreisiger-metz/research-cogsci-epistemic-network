// ============================================================================
// Filename          : $RCSfile: Concept.cpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 11-Feb-2009
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 06:03:32 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This file implements the Concept and AtomicConcept
//                     classes
//
// Revision history  : 
// ============================================================================
#include "Concept.hpp"


using namespace std;
using namespace EpisodeGenerator;


Utilities::Statistics::Uniform  Concept::Uniform;
double                          Concept::SentenceWeightThreshold = 0.001;

map<string, unsigned>           Concept::ConceptCounterMap;
  

//  ==========================================================================80
/// The constructor
///
/// This constructor creates and initialises an inter-concept link.
///
/// @param name      the name of the link
/// @param concept   the concept being linked to
/// @param weight    the link's weight
//  ==========================================================================80
Link::Link(std::string name, Concept *concept, double weight) {
  this->name = name;
  this->concept = concept;
  this->weight = weight;
}


//  ==========================================================================80
/// The constructor
///
/// This constructor creates and initialises an inter-concept link with a 
/// default weight of 1.0.
///
/// @param name      the name of the link
/// @param concept   the concept being linked to
//  ==========================================================================80
Link::Link(std::string name, Concept *concept) {
  this->name = name;
  this->concept = concept;
  this->weight = 1.0;
}






//  ==========================================================================80
/// The constructor
///
/// This constructor creates an instance of a concept.
///
/// @param conceptName   the name of the concept that this class represents
//  ==========================================================================80
Concept::Concept(string conceptName) {
  char instanceName[conceptName.length() + 5];

  this->conceptName = conceptName;
  instanceNumber = Concept::ConceptCounterMap[conceptName]++;
  sprintf(instanceName, "%s-%d", conceptName.c_str(), instanceNumber);
  this->instanceName = instanceName;
}




//  ==========================================================================80
/// The destructor
//  ==========================================================================80
Concept::~Concept() {
  unsigned i;

  // If we were the last instance of our conceptual class to be created,
  // decrement the counter --- this is, obviously, not re-enterant, but
  // this won't be a problem given the way in which we'll be using the
  // episode generator.
  if (Concept::ConceptCounterMap[conceptName] == instanceNumber - 1)
    Concept::ConceptCounterMap[conceptName]--;

  for (i = 0; i < subconcept.size(); i++) {
    delete subconcept[i]->concept;
    delete subconcept[i];
  }
}




//  ==========================================================================80
/// Adds a child concept to our concept vector
///
/// This method accepts a label, a weight and a subconcept and adds them to
/// our concept vector;  if the subconcept is NULL, then nothing is added.
///
/// @param subconceptName   the choice's mnemonic 
/// @param weight           the weight of the chosen sentence
/// @param number           the number of concept -- probability pairs
//  ==========================================================================80
Concept *Concept::add(string subconceptName, double weight, Concept *subconcept) {
  if (subconcept != NULL) {
    if (SentenceWeightThreshold == 0.0)
      this->subconcept.push_back(new Link(subconceptName, subconcept, weight));
    else if (weight >= SentenceWeightThreshold)
      this->subconcept.push_back(new Link(subconceptName, subconcept, 1.0));
  }

  return subconcept;
}




//  ==========================================================================80
/// Randomly select a child concept and add it to our concept vector
///
/// This variable-argument method accepts an arbitrarily long list of
/// concept -- probability pairs and select one of them.  If the chosen
/// 'attribute' is non-NULL, it is added to our concept vector;  if it is
/// NULL, then nothing is added.
///
/// @param subconceptName   the choice's mnemonic 
/// @param weight           the weight of the chosen sentence
/// @param number           the number of concept -- probability pairs
///
/// @return the chosen concept
///
/// @note this method deletes any non-NULL concept that was not chosen
//  ==========================================================================80
Concept *Concept::addOneOf(string subconceptName, double weight, unsigned number, ...) {
  unsigned i;
  double temp_d;
  vector<Concept*> concepts;
  vector<double>   cumulativeProbabilities;
  va_list ap;
  Concept *choice = NULL;

  // Retrieve the concept -- probability pairs, and
  va_start(ap, number);
  for (i = 0; i < number; i++) {
    concepts.push_back(va_arg(ap, Concept *));
    cumulativeProbabilities.push_back(va_arg(ap, double));
  }
  va_end(ap);

  if (number == 1) {
    choice = concepts[0];
    concepts[0] = NULL;

  } else {
    // normalise the probabilities and make them cumulative.
    for (temp_d = 0.0, i = 0; i < number; i++)
      temp_d += cumulativeProbabilities[i];
    for (i = 0; i < number; i++)
      cumulativeProbabilities[i] /= temp_d;

    for (i = 1; i < number; i++)
      cumulativeProbabilities[i] += cumulativeProbabilities[i - 1];

    // Roll the dice and work out which Concept* to pick
    temp_d = Concept::Uniform.unifrnd(0.0, 1.0);
    for (i = 0; i < number; i++)
      if (temp_d <= cumulativeProbabilities[i]) {
	choice = concepts[i];
	concepts[i] = NULL;
	break;
      }
    
  }

  // If the chosen concept is non-NULL, push it and the sub-concept's
  // name onto their respective vectors
  if (choice != NULL) {
    if (SentenceWeightThreshold == 0.0)
      this->subconcept.push_back(new Link(subconceptName, choice, weight));
    else if (weight >= SentenceWeightThreshold)
      this->subconcept.push_back(new Link(subconceptName, choice, 1.0));
  }

  // And finally, delete everything else
  for (i = 0; i < number; i++)
    if (concepts[i] != NULL)
      delete concepts[i];

  return choice;
}




//  ==========================================================================80
/// Removes a child concept from our concept vector
///
/// This method removes the named subconcept from our concept vector;  if the
/// subconcept does not exists, then it does nothing
///
/// @param subconceptName   the choice's mnemonic 
//  ==========================================================================80
void Concept::remove(string subconceptName) {
  for (unsigned i = 0; i < subconcept.size(); i++)
    if (subconcept[i]->name == subconceptName) {
      delete subconcept[i]->concept;
      delete subconcept[i];
      subconcept.erase(subconcept.begin() + i);

      break;
    }
}




//  ==========================================================================80
/// Prints this episode's fragment
///
/// This method recursively generates and prints the episode fragment corres-
/// ponding to this concept instance.
///
/// @param ofp      the output stream to which the information will be
///                 printed
/// @param prefix   a string that will preface every line of printed output
/// @param weight   the factor by which to scale the concept's own weights
///
/// @return the number of characters sent to [ofp]
//  ==========================================================================80
unsigned Concept::print(FILE *ofp, const char *prefix, double weight) {
  unsigned i, count = 0;

  for (i = 0; i < subconcept.size(); i++)
    count += subconcept[i]->concept->print(ofp, prefix, subconcept[i]->weight * weight);

  return count;
}





//  ======================================================================80
/// The constructor
///
/// This constructor initialises the atomic concept and sets [value]
/// accordingly
///
/// @param value   the string that will be printed by AtomicConcept::print()
//  ======================================================================80
AtomicConcept::AtomicConcept(string value) : Concept("atomic-concept") {
  this->value = value;
}




//  ======================================================================80
/// Prints this atomic concept's value
///
/// @param ofp      the output stream to which the information will be
///                 printed
/// @param prefix   a string that will preface every line of printed output
///
/// @return the number of characters sent to [ofp]
//  ======================================================================80
unsigned AtomicConcept::print(FILE *ofp, const char *prefix, double weight) {
  if (weight == 1.0)
    return fprintf(ofp, "%s%s\n", prefix, value.c_str());
  else
    return fprintf(ofp, "%s%s %lf\n", prefix, value.c_str(), weight);
}






//  ==========================================================================80
/// Prints this episode
///
/// This method recursively prints the episode fragments contained in its
/// subconcept vector.
///
/// @param ofp      the output stream to which the information will be
///                 printed
/// @param prefix   a string that will preface every line of printed output
/// @param weight   the factor by which to scale the concept's own weights
///
/// @return the number of characters sent to [ofp]
//  ==========================================================================80
unsigned Episode::print(FILE *ofp, const char *prefix, double weight) {
  char fullPrefix[strlen(prefix) + instanceName.length() + 2];
  unsigned i, count = 0;
  
  sprintf(fullPrefix, "%s%s ", prefix, instanceName.c_str());
  for (i = 0; i < subconcept.size(); i++)
    count += subconcept[i]->concept->print(ofp, fullPrefix, subconcept[i]->weight * weight);
  
  return count;
}
