#include "AMBRTest.hpp"

using namespace std;
using namespace AMBR::Test;

#define NUMBER_CONCEPT_AGENTS 10
#define NUMBER_INSTANCE_AGENTS 10

void AMBRTest::setUp() {
  unsigned i;
  Slot *slot;
  ConceptAgent *conceptAgent;
  InstanceAgent *instanceAgent;
  
  try {
    // Based upon Rogers and McClelland (2004), page 395.
    conceptAgent = new ConceptAgent("is", Agent::RelationAgent, "the instantiation relationship");
    conceptAgent->addSlot(new Slot("base", Slot::RelationSlot, "the base concept"));
    conceptAgent->addSlot(new Slot("derived", Slot::RelationSlot, "the derived concept"));
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("can", Agent::RelationAgent, "a relationship of ability");
    conceptAgent->addSlot(new Slot("object", Slot::RelationSlot, "the object"));
    conceptAgent->addSlot(new Slot("ability", Slot::RelationSlot, "the thing that the object can do"));
    conceptAgents.push_back(conceptAgent);
    
    
    conceptAgent = new ConceptAgent("living-thing", Agent::ObjectAgent, "represents all living things");
    conceptAgent->addSubClass("plant", 0.8);
    conceptAgent->addSubClass("animal", 0.9);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("plant", Agent::ObjectAgent, "represents all plants");
    conceptAgent->addSuperClass("living-thing", 1.0);
    conceptAgent->addSubClass("tree", 0.9);
    conceptAgent->addSubClass("flower", 0.8);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("animal", Agent::ObjectAgent, "represents all animals");
    conceptAgent->addSuperClass("living-thing", 1.0);
    conceptAgent->addSubClass("bird", 0.8);
    conceptAgent->addSubClass("fish", 0.7);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("tree", Agent::ObjectAgent, "represents all trees");
    conceptAgent->addSuperClass("plant", 1.0);
    conceptAgent->addSubClass("pine", 0.8);
    conceptAgent->addSubClass("oak", 0.9);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("flower", Agent::ObjectAgent, "represents all flowers");
    conceptAgent->addSuperClass("plant", 1.0);
    conceptAgent->addSubClass("rose", 0.9);
    conceptAgent->addSubClass("daisy", 0.8);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("bird", Agent::ObjectAgent, "represents all birds");
    conceptAgent->addSuperClass("animal", 1.0);
    conceptAgent->addSubClass("robin", 0.95);
    conceptAgent->addSubClass("canary", 0.8);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("fish", Agent::ObjectAgent, "represents all fish");
    conceptAgent->addSuperClass("animal", 1.0);
    conceptAgent->addSubClass("sunfish", 0.7);
    conceptAgent->addSubClass("salmon", 0.9);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("pine", Agent::ObjectAgent, "represents all pine trees");
    conceptAgent->addSuperClass("tree", 1.0);
    conceptAgent->addAssociation("oak", 0.5);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("oak", Agent::ObjectAgent, "represents all oaks");
    conceptAgent->addSuperClass("tree", 1.0);
    conceptAgent->addAssociation("pine", 0.2);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("rose", Agent::ObjectAgent, "represents all roses");
    conceptAgent->addSuperClass("flower", 1.0);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("daisy", Agent::ObjectAgent, "represents all daisies");
    conceptAgent->addSuperClass("flower", 1.0);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("robin", Agent::ObjectAgent, "represents all robins");
    conceptAgent->addSuperClass("bird", 1.0);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("canary", Agent::ObjectAgent, "represents all canaries");
    conceptAgent->addSuperClass("bird", 1.0);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("sunfish", Agent::ObjectAgent, "represents all sunfish");
    conceptAgent->addSuperClass("fish", 1.0);
    conceptAgents.push_back(conceptAgent);
    
    conceptAgent = new ConceptAgent("salmon", Agent::ObjectAgent, "represents all salmon");
    conceptAgent->addSuperClass("fish", 1.0);
    conceptAgents.push_back(conceptAgent);
    
     conceptAgents.push_back(new ConceptAgent("big", Agent::ObjectAgent, ""));
     conceptAgents.push_back(new ConceptAgent("green", Agent::ObjectAgent, ""));
     conceptAgents.push_back(new ConceptAgent("red", Agent::ObjectAgent, ""));
     
     conceptAgents.push_back(new ConceptAgent("grow", Agent::ObjectAgent, ""));
     conceptAgents.push_back(new ConceptAgent("move", Agent::ObjectAgent, ""));
     conceptAgents.push_back(new ConceptAgent("swim", Agent::ObjectAgent, ""));
     conceptAgents.push_back(new ConceptAgent("fly", Agent::ObjectAgent, ""));
     conceptAgents.push_back(new ConceptAgent("sing", Agent::ObjectAgent, ""));
     
    
    instanceAgent = new InstanceAgent("robin-1", Agent::ObjectAgent, "a robin in an oak tree");
    instanceAgent->addInstanceOf("robin", 1.0);
    instanceAgent->addAssociation("oak-1", 1.0);
    instanceAgents.push_back(instanceAgent);
    
    instanceAgent = new InstanceAgent("canary-1", Agent::ObjectAgent, "a canary in an oak tree");
    instanceAgent->addInstanceOf("canary", 1.0);
    instanceAgent->addAssociation("oak-1", 1.0);
    instanceAgents.push_back(instanceAgent);
    
    instanceAgent = new InstanceAgent("red-1", Agent::ObjectAgent, "the colour red");
    instanceAgent->addInstanceOf("red", 1.0);
    instanceAgents.push_back(instanceAgent);
    
    instanceAgent = new InstanceAgent("is-1", Agent::ObjectAgent, "");
    slot = new Slot("base", Slot::RelationSlot, "the base concept");
    slot->addInstanceOf("is.base", 1.0);
    slot->addCoreference("robin-1", 1.0);
    instanceAgent->addSlot(slot);
    slot = new Slot("derived", Slot::RelationSlot, "the derived concept");
    slot->addInstanceOf("is.derived", 1.0);
    slot->addCoreference("red-1", 1.0);
    instanceAgent->addSlot(slot);
    instanceAgents.push_back(instanceAgent);
    
    
    instanceAgent = new InstanceAgent("oak-1", Agent::ObjectAgent, "an oak tree");
    instanceAgent->addInstanceOf("oak", 1.0);
    instanceAgent->addAssociation("robin-1", 0.1);
    instanceAgents.push_back(instanceAgent);
    
    instanceAgent = new InstanceAgent("big-1", Agent::ObjectAgent, "BIIIG");
    instanceAgent->addInstanceOf("big", 1.0);
    instanceAgents.push_back(instanceAgent);
    
    instanceAgent = new InstanceAgent("is-1", Agent::ObjectAgent, "");
    slot = new Slot("base", Slot::RelationSlot, "the base concept");
    slot->addInstanceOf("is.base", 1.0);
    slot->addCoreference("robin-1", 1.0);
    instanceAgent->addSlot(slot);
    slot = new Slot("derived", Slot::RelationSlot, "the derived concept");
    slot->addInstanceOf("is.derived", 1.0);
    slot->addCoreference("big-1", 1.0);
    instanceAgent->addSlot(slot);
    instanceAgents.push_back(instanceAgent);
    
    
    AMBR::Manager->printAgentSummaries();
    
    for (i = 0; i < conceptAgents.size(); i++)
      conceptAgents[i]->initialise();
    for (i = 0; i < instanceAgents.size(); i++)
      instanceAgents[i]->initialise();
  } catch (Exception::BaseException e) {
    printf("Caught exception %s!\n", e.comment.c_str());
  }
}




void AMBRTest::tearDown() {
}




void AMBRTest::testSystem() {
  try {
    AMBR::Manager->start();
    instanceAgents[4]->lockActivation(1.0);
    usleep(10000);
    instanceAgents[0]->lockActivation(1.0);
    instanceAgents[1]->lockActivation(1.0);
    usleep(990000);
    AMBR::Manager->printAgentSummaries();
    usleep(1000000);
    AMBR::Manager->stop();
  } catch (Exception::BaseException e) {
    printf("Caught exception %s!\n", e.comment.c_str());
  }
}
