#include "AgentTest.hpp"


void AgentTest::setUp() {
  Agent::AgentNameToPointerMap.clear();

  agent_1 = new Agent("test-agent-1", Agent::ObjectAgent, "A sample agent");
  agent_2 = new Agent("test-agent-2", Agent::ObjectAgent, "Another sample agent");

  slot_1 = new Slot("test-slot-1", Slot::AspectSlot, "A sample slot");
  slot_2 = new Slot("test-slot-2", Slot::AspectSlot, "Another sample slot");
  slot_3 = new Slot("test-slot-3", Slot::AspectSlot, "A third sample slot");

  agent_1->addSlot(slot_1);
  agent_1->addSlot(slot_2);
  agent_2->addSlot(slot_3);
}




void AgentTest::tearDown() {
  delete agent_1;
  delete agent_2;

  // the slots are deleted by the agent that owns them
}




void AgentTest::testConstructorDestructor() {
  map<string, Agent*>::iterator i;
  map<Agent*, unsigned>::iterator j;

  Agent *agent = new Agent("local-test-agent", Agent::ObjectAgent, "A local agent");

  // Test that it sets the agent's identity,
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent(...) failed to set name", agent->name == "local-test-agent");
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent(...) failed to set type", agent->type == Agent::ObjectAgent);
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent(...) failed to set comment", agent->comment == "A local agent");

  // its activation variables and reference vectors,
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent(...) failed to initialise its activation", agent->m_activation == 0);
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent(...) failed to initialise m_tick__activationMessage", agent->m_tick__activationMessage != NULL);
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent(...) failed to add associations to m_agentReferenceVectors", 
			 (agent->m_agentReferenceVectors.size() == 1) && (agent->m_agentReferenceVectors[0] == &(agent->associations)));
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent(...) failed to add coreferences to m_slotReferenceVectors", 
			 (agent->m_slotReferenceVectors.size() == 1) && (agent->m_slotReferenceVectors[0] == &(agent->coreferences)));

  // and updates the name-to-pointer static map; it should /not/ update AgentsWithUnresolvedReferences
  // until there is an actual unresolved reference.
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent(...) failed to add 'local-test-agent' to Agent::AgentNameToPointerMap",
			 Agent::AgentNameToPointerMap.find("local-test-agent") != Agent::AgentNameToPointerMap.end());
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent(...) incorrectly added 'local-test-agent' to Agent::AgentsWithUnresolvedReferences", 
			 Agent::AgentsWithUnresolvedReferences.find(agent) == Agent::AgentsWithUnresolvedReferences.end());
  

  // Upon deletion, it should remove itself from the static maps
  delete agent;
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent(...) failed to remove 'local-test-agent' from Agent::AgentNameToPointerMap", 
			 Agent::AgentNameToPointerMap.find("local-test-agent") == Agent::AgentNameToPointerMap.end());
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent(...) failed to remove 'local-test-agent' from Agent::AgentsWithUnresolvedReferences", 
			 Agent::AgentsWithUnresolvedReferences.find(agent) == Agent::AgentsWithUnresolvedReferences.end());
}




void AgentTest::testAddCoreference() {
  unsigned flag;

  // Try adding coreferences by name
  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("Agent::addCoreference(...) failed to resolve existing coreference",
			   agent_1->addCoreference("test-agent-2.test-slot-3", 1.0) == 0);
    CPPUNIT_ASSERT_MESSAGE("Agent::addCoreference(...) failed to resolve existing coreference",
			   agent_2->addCoreference("test-agent-1.test-slot-1", 1.0) == 0);
    CPPUNIT_ASSERT_MESSAGE("Agent::addCoreference(...) failed to resolve existing coreference",
			   agent_2->addCoreference("test-agent-1.test-slot-2", 0.5) == 0);
    CPPUNIT_ASSERT_MESSAGE("Agent::addCoreference(...) incorrectly resolved a non-existent coreference",
                           agent_2->addCoreference("test-agent-1.test-slot-0", 1.0) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::addCoreference(...) threw an exception for a valid slot name", flag == 0);

  
  // Make sure they were added to Agent::coreferences; note that all but the
  // last of their pointers should have been resolved automatically
  CPPUNIT_ASSERT_MESSAGE("agent_1->coreference has an incorrect number of entries", 
			 agent_1->coreferences.size() == 1);
  CPPUNIT_ASSERT_MESSAGE("agent_2->coreference has an incorrect number of entries", 
			 agent_2->coreferences.size() == 3);

  CPPUNIT_ASSERT_MESSAGE("agent_1->coreference[0] incorrect", 
			 (agent_1->coreferences[0]->name == "test-agent-2.test-slot-3") && 
			 (agent_1->coreferences[0]->weight == 1.0) &&
			 (agent_1->coreferences[0]->slot == slot_3));
  CPPUNIT_ASSERT_MESSAGE("agent_2->coreference[0] incorrect", 
			 (agent_2->coreferences[0]->name == "test-agent-1.test-slot-1") && 
			 (agent_2->coreferences[0]->weight == 1.0) &&
			 (agent_2->coreferences[0]->slot == slot_1));
  CPPUNIT_ASSERT_MESSAGE("agent_2->coreference[1] incorrect", 
			 (agent_2->coreferences[1]->name == "test-agent-1.test-slot-2") && 
			 (agent_2->coreferences[1]->weight == 0.5) &&
			 (agent_2->coreferences[1]->slot == slot_2));
  CPPUNIT_ASSERT_MESSAGE("agent_2->coreference[2] incorrect", 
                         (agent_2->coreferences[2]->slot == NULL) &&
                         (agent_2->coreferences[2]->name == "test-agent-1.test-slot-0") && 
                         (agent_2->coreferences[2]->weight == 1.0));
  
  // Verify that Agent::initialise() throws exceptions for non-existent references
  // and correctly resolves valid, but late-coming references
  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::initialise() failed to throw an exception for a non-existent slot", flag == 1);

  Slot *slot_0 = new Slot("test-slot-0", Slot::AspectSlot, "A local test slot");
  agent_1->addSlot(slot_0);
  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::initialise() failed to resolve slot reference after it was added", flag == 0);
  CPPUNIT_ASSERT_MESSAGE("agent_2->coreference[2]->slot is incorrect", 
                         agent_2->coreferences[2]->slot == slot_0);


  
  // Make sure that Agent::addCoreference(...) does throw an exception when it
  // is given an agent's name
  try {
    flag = 0;
    agent_1->addCoreference("test-agent-2", 1.0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::addCoreference(...) failed to throw an exception for an invalid slot name", flag == 1);

  // or a slot that has already been added
  try {
    flag = 0;
    agent_1->addCoreference("test-agent-2.test-slot-3", 1.0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::addCoreference(...) failed to throw an exception for a duplicate slot", flag == 1);
}




void AgentTest::testAddAssociation() {
  unsigned flag;

  // Try adding associations by name
  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("Agent::addAssociation(...) returned an incorrect index",
			   agent_1->addAssociation("test-agent-2", 1.0) == 0);
    CPPUNIT_ASSERT_MESSAGE("Agent::addAssociation(...) returned an incorrect index",
			   agent_2->addAssociation("test-agent-1", 0.5) == 0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::addAssociation(...) threw an exception for a valid agent name", flag == 0);

  // Make sure they were added to Agent::associations
  CPPUNIT_ASSERT_MESSAGE("agent_1->association has an incorrect number of entries", 
			 agent_1->associations.size() == 1);
  CPPUNIT_ASSERT_MESSAGE("agent_2->association has an incorrect number of entries", 
			 agent_2->associations.size() == 1);

  CPPUNIT_ASSERT_MESSAGE("agent_1->association[0] incorrect", 
			 (agent_1->associations[0]->name == "test-agent-2") && 
			 (agent_1->associations[0]->weight == 1.0));
  CPPUNIT_ASSERT_MESSAGE("agent_2->association[0] incorrect", 
			 (agent_2->associations[0]->name == "test-agent-1") && 
			 (agent_2->associations[0]->weight == 0.5));

  // Unlike the slot tests, /these/ associations should resolve upon addition
  // as the agents' addresses are placed into Agent::AgentNameToPointerMap by
  // their constructors.
  CPPUNIT_ASSERT_MESSAGE("agent_1->association[0]->agent did not resolve", 
			 agent_1->associations[0]->agent == agent_2);
  CPPUNIT_ASSERT_MESSAGE("agent_2->association[0]->agent did not resolve", 
			 agent_2->associations[0]->agent == agent_1);

  // Make sure that Agent::addAssociation(...) does throw an exception when it
  // is given a slot's name
  try {
    flag = 0;
    agent_1->addAssociation("test-agent-2.test-slot-3", 1.0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::addAssociation(...) failed to throw an exception for an invalid agent name", flag == 1);

  // or an agent that has already been added
  try {
    flag = 0;
    agent_1->addAssociation("test-agent-2", 1.0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::addAssociation(...) failed to throw an exception for a duplicate agent", flag == 1);
}




void AgentTest::testAddGetSlot() {
  // We'll start by using the slots that were added in AgentTest::setUp()
  
  // Check to make sure that the agents point to the right slots
  CPPUNIT_ASSERT_MESSAGE("agent_1->slots has an incorrect number of entries", agent_1->slots.size() == 2);
  CPPUNIT_ASSERT_MESSAGE("agent_2->slots has an incorrect number of entries", agent_2->slots.size() == 1);
  CPPUNIT_ASSERT_MESSAGE("agent_1->slots[0] points to the wrong slot", agent_1->slots[0] == slot_1);
  CPPUNIT_ASSERT_MESSAGE("agent_1->slots[1] points to the wrong slot", agent_1->slots[1] == slot_2);
  CPPUNIT_ASSERT_MESSAGE("agent_2->slots[0] points to the wrong slot", agent_2->slots[0] == slot_3);

  // Test Agent::getSlot(...)
  CPPUNIT_ASSERT_MESSAGE("agent_1->getSlot(...) returned the wrong slot", agent_1->getSlot("test-slot-1") == slot_1);
  CPPUNIT_ASSERT_MESSAGE("agent_1->getSlot(...) returned the wrong slot", agent_1->getSlot("test-slot-2") == slot_2);
  CPPUNIT_ASSERT_MESSAGE("agent_2->getSlot(...) returned the wrong slot", agent_2->getSlot("test-slot-3") == slot_3);

  CPPUNIT_ASSERT_MESSAGE("agent_1->getSlot(...) incorrectly returned NULL", agent_1->getSlot("test-slot-1") != NULL);
  CPPUNIT_ASSERT_MESSAGE("agent_1->getSlot(...) incorrectly returned NULL", agent_1->getSlot("test-slot-2") != NULL);
  CPPUNIT_ASSERT_MESSAGE("agent_2->getSlot(...) incorrectly returned NULL", agent_2->getSlot("test-slot-3") != NULL);

  CPPUNIT_ASSERT_MESSAGE("agent_1->getSlot(...) failed to return NULL", agent_1->getSlot("non-existent-test-slot") == NULL);
  CPPUNIT_ASSERT_MESSAGE("agent_2->getSlot(...) failed to return NULL", agent_2->getSlot("non-existent-test-slot") == NULL);
  CPPUNIT_ASSERT_MESSAGE("Agent::getSlot(...) added a non-existent slot to m_slotNameToPointerMap upon a call to Agent::getSlot(...)",
			 agent_1->getSlot("non-existent-test-slot") == NULL);

  // And check the exception throwing
  unsigned flag, idx, temp;
  Slot *slot = new Slot("local-test-slot", Slot::AspectSlot, "A local test slot");
  try {
    flag = 0;
    temp = agent_1->slots.size();
    idx = agent_1->addSlot(slot);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::addSlot(...) failed to add [slot] to agent_1->slots", idx == temp);
  CPPUNIT_ASSERT_MESSAGE("Agent::addSlot(...) agent_1->slots[idx] != [slot]", agent_1->slots[idx] == slot);
  CPPUNIT_ASSERT_MESSAGE("Agent::addSlot(...) threw an exception for a valid slot", flag == 0);

  try {
    flag = 0;
    agent_1->addSlot(NULL);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::addSlot(...) failed to throw an exception for a NULL slot", flag == 1);

  try {
    flag = 0;
    agent_1->addSlot(slot);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::addSlot(...) failed to throw an exception for a duplicate slot", flag == 1);
  
  // and we don't need to delete [slot] as it is now owned by [agent_1]
}




void AgentTest::testInitialise() {
  unsigned flag;
  Agent *agent_3;
  Slot *slot_4, *slot_5;

  agent_3 = new Agent("test-agent-3", Agent::ObjectAgent, "A local sample agent");
  
  slot_4 = new Slot("test-slot-4", Slot::AspectSlot, "A local sample slot");
  slot_5 = new Slot("test-slot-5", Slot::AspectSlot, "Another local sample slot");

  agent_3->addSlot(slot_4);
  agent_3->addSlot(slot_5);
  
  // Declare the references,
  try {
    flag = 0;
    agent_1->addCoreference("test-agent-2.test-slot-3", 0.1);
    agent_1->addAssociation("test-agent-2", 0.2);
    agent_1->slots[0]->addSuperClass("test-agent-2.test-slot-3", 0.3);
    agent_1->slots[1]->addSuperClass("test-agent-3.test-slot-4", 0.25);
    
    agent_2->addCoreference("test-agent-1.test-slot-1", 1.0);
    agent_2->addCoreference("test-agent-1.test-slot-2", 0.9);
    agent_2->addCoreference("test-agent-3.test-slot-5", 0.85);
    agent_2->addAssociation("test-agent-1", 0.8);
    agent_2->slots[0]->addSuperClass("test-agent-1.test-slot-1", 0.7);
    agent_2->slots[0]->addInstanceOf("test-agent-1.test-slot-2", 0.6);
    agent_2->slots[0]->addCoreference("test-agent-1", 0.5);
    agent_2->slots[0]->addAssociation("test-agent-1", 0.4);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("An exception was thrown while adding a reference to agent_1 or agent_2", flag == 0);
 
  // initialise the agents,
  try {
    flag = 0;
    agent_1->initialise();
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::initialise() threw and exception", flag == 0);

  // and check the maximal link weights
  CPPUNIT_ASSERT_MESSAGE("agent_1->initialise() failed to find the maximal inter-agent link weight",
                         ((double) agent_1->m_agentPointerToLinkWeightMap[agent_2] == (double) 0.3));
  CPPUNIT_ASSERT_MESSAGE("agent_1->initialise() failed to find the maximal inter-agent link weight",
                         agent_1->m_agentPointerToLinkWeightMap[agent_3] == (double) 0.25);
  CPPUNIT_ASSERT_MESSAGE("agent_2->initialise() failed to find the maximal inter-agent link weight",
                         agent_2->m_agentPointerToLinkWeightMap[agent_1] == (double) 1.0);
  CPPUNIT_ASSERT_MESSAGE("agent_2->initialise() failed to find the maximal inter-agent link weight",
                         agent_2->m_agentPointerToLinkWeightMap[agent_3] == (double) 0.85);

  // and the normalised, maximal link weights --- and yes, all of these floats are kinda necessary
  // to ensure that the normalisation takes place at a single-precision level of accuracy
  CPPUNIT_ASSERT_MESSAGE("agent_1->initialise() failed to correctly normalise its maximal inter-agent link weights",
                         agent_1->m_agentPointerToNormalisedLinkWeightMap[agent_2] ==  0.3 / 0.55);
  CPPUNIT_ASSERT_MESSAGE("agent_1->initialise() failed to correctly normalise its maximal inter-agent link weights",
                         agent_1->m_agentPointerToNormalisedLinkWeightMap[agent_3] == 0.25 / 0.55);
  CPPUNIT_ASSERT_MESSAGE("agent_2->initialise() failed to correctly normalise its maximal inter-agent link weights",
                         agent_2->m_agentPointerToNormalisedLinkWeightMap[agent_1] == 1.0 / 1.85);
  CPPUNIT_ASSERT_MESSAGE("agent_2->initialise() failed to correctly normalise its maximal inter-agent link weights",
                         (agent_2->m_agentPointerToNormalisedLinkWeightMap[agent_3] - (0.85 / 1.85)) < 0.000001);
  
  delete agent_3;
}




void AgentTest::testPrintParse() {
  #ifdef __DEBUG__INTERACTIVE_TESTS
  char answer[4096];
  FILE *tmp = tmpfile();
  Agent *agent_3;

  agent_1->addCoreference("test-agent-2.test-slot-3", 1.0);
  agent_2->addCoreference("test-agent-1.test-slot-1", 1.0);
  agent_2->addCoreference("test-agent-1.test-slot-2", 0.5);

  agent_1->addAssociation("test-agent-2", 1.0);
  agent_2->addAssociation("test-agent-1", 0.5);


  answer[0] = 'y';
  printf("\nAbout to print [(Agent) agent_1] with no indent:\n");
  agent_1->print(stdout, "");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("Agent::print(...) failed to print correctly", answer[0] == 'y');

  answer[0] = 'y';
  printf("\nAbout to print [(Agent) agent_2] with no indent:\n");
  agent_2->print(stdout, "");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("Agent::print(...) failed to print correctly", answer[0] == 'y');

  answer[0] = 'y';
  printf("\nAbout to print [(Agent) agent_2] with a two-space indent:\n");
  agent_2->print(stdout, "  ");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("Agent::print(...) failed to print correctly", answer[0] == 'y');


  // Now we are going to 'print' an agent to our temporary file
  // are parse it back in --- the resulting data should, of
  // course, be the same
  agent_1->print(tmp, "");
  agent_1->addAssociation("this-should-only-appear-in-the-second-parsing", 0.0);
  agent_1->print(tmp, "  ");
  fprintf(tmp, "(defagent  decoy-agent  \"A decoy agent representing the start of the next agent\"\n");
  fflush(tmp);
  fseek(tmp, 0, SEEK_SET);   // rewind

  printf("\nAbout to print two copies of the parsed test-agent-1 with a two-space indent\n--- with the exception of the agent names and comments, the first should\nmatch previous printout, while the second should have an additional association:\n\n");
  agent_3 = new Agent("ignore-this-mismatch", Agent::SituationAgent, "The agent's name and comment field don't need to match");
  fgets(answer, 4096, tmp);  // skip over the first (agent-defining) line
  agent_3->parse(tmp);
  agent_3->print(stdout, "  ");
  delete agent_3;
  agent_3 = new Agent("ignore-this-mismatch-too", Agent::SituationAgent, "See above");
  fgets(answer, 4096, tmp);  // skip over the next agent-defining line
  agent_3->parse(tmp);
  agent_3->print(stdout, "  ");
  printf("\nDoes this look right?  (y/n) ");
  answer[0] = 'y';
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("Agent::parse(...) failed to correctly parse the output of Agent::print", answer[0] == 'y');

  delete agent_3;
  fclose(tmp);
  #endif
}




void AgentTest::testUpdateActivation() {
  unsigned flag;
  Agent *agent_3 = new Agent("test-agent-3", Agent::ObjectAgent, "A local sample agent");
  Slot *slot_4 = new Slot("test-slot-4", Slot::AspectSlot, "A local sample slot");
  agent_3->addSlot(slot_4);


  // Set up links between all agents,
  try {
    flag = 0;
    agent_1->addCoreference("test-agent-2.test-slot-3", 1.0);
    agent_1->slots[1]->addAssociation("test-agent-3", 0.25);

    agent_2->addAssociation("test-agent-1", 0.5);
    agent_2->slots[0]->addInstanceOf("test-agent-1.test-slot-1", 1.0);
    agent_2->addCoreference("test-agent-3.test-slot-4", 1.0);
    agent_2->slots[0]->addAssociation("test-agent-3", 0.5);

    agent_3->slots[0]->addSuperClass("test-agent-1.test-slot-2", 0.5);
    agent_3->addAssociation("test-agent-2", 0.5);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("An exception was thrown while setting up the inter-agent links", flag == 0);

  // and initialise the agents.
  try {
    flag = 0;
    agent_1->initialise();
    agent_2->initialise();
    agent_3->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("An exception was thrown while initialising the agents", flag == 0);


  // Next we check the inter-agent link weights.
  CPPUNIT_ASSERT_MESSAGE("Raw link weight from test-agent-1 to test-agent-2 incorrect", agent_1->m_agentPointerToLinkWeightMap[agent_2] == 1.0);
  CPPUNIT_ASSERT_MESSAGE("Raw link weight from test-agent-1 to test-agent-3 incorrect", agent_1->m_agentPointerToLinkWeightMap[agent_3] == 0.25);
  CPPUNIT_ASSERT_MESSAGE("Raw link weight from test-agent-2 to test-agent-1 incorrect", agent_2->m_agentPointerToLinkWeightMap[agent_1] == 1.0);
  CPPUNIT_ASSERT_MESSAGE("Raw link weight from test-agent-2 to test-agent-3 incorrect", agent_2->m_agentPointerToLinkWeightMap[agent_3] == 1.0);
  CPPUNIT_ASSERT_MESSAGE("Raw link weight from test-agent-3 to test-agent-1 incorrect", agent_3->m_agentPointerToLinkWeightMap[agent_1] == 0.5);
  CPPUNIT_ASSERT_MESSAGE("Raw link weight from test-agent-3 to test-agent-2 incorrect", agent_3->m_agentPointerToLinkWeightMap[agent_2] == 0.5);

  CPPUNIT_ASSERT_MESSAGE("Normalised link weight from test-agent-1 to test-agent-2 incorrect", agent_1->m_agentPointerToNormalisedLinkWeightMap[agent_2] == 0.8);
  CPPUNIT_ASSERT_MESSAGE("Normalised link weight from test-agent-1 to test-agent-3 incorrect", agent_1->m_agentPointerToNormalisedLinkWeightMap[agent_3] == 0.2);
  CPPUNIT_ASSERT_MESSAGE("Normalised link weight from test-agent-2 to test-agent-1 incorrect", agent_2->m_agentPointerToNormalisedLinkWeightMap[agent_1] == 0.5);
  CPPUNIT_ASSERT_MESSAGE("Normalised link weight from test-agent-2 to test-agent-3 incorrect", agent_2->m_agentPointerToNormalisedLinkWeightMap[agent_3] == 0.5);
  CPPUNIT_ASSERT_MESSAGE("Normalised link weight from test-agent-3 to test-agent-1 incorrect", agent_3->m_agentPointerToNormalisedLinkWeightMap[agent_1] == 0.5);
  CPPUNIT_ASSERT_MESSAGE("Normalised link weight from test-agent-3 to test-agent-2 incorrect", agent_3->m_agentPointerToNormalisedLinkWeightMap[agent_2] == 0.5);


  // And now on to the activation-related tests...
  // =============================================
  CPPUNIT_ASSERT_MESSAGE("test-agent-1's activation is non-zero", agent_1->m_activation == 0.0);
  CPPUNIT_ASSERT_MESSAGE("test-agent-2's activation is non-zero", agent_2->m_activation == 0.0);
  CPPUNIT_ASSERT_MESSAGE("test-agent-3's activation is non-zero", agent_3->m_activation == 0.0);

  // Send test-agent-1 some activation messages
  Message::Activation activation(0.5);
  agent_1->receive(&activation);
  CPPUNIT_ASSERT_MESSAGE("test-agent-1's incoming activation is incorrect", agent_1->m_incomingActivation == 0.5);
  CPPUNIT_ASSERT_MESSAGE("test-agent-1's activation is non-zero", agent_1->m_activation == 0.0);
  agent_1->receive(&activation);
  CPPUNIT_ASSERT_MESSAGE("test-agent-1's incoming activation is incorrect", agent_1->m_incomingActivation == 1.0);

  // Make sure ::updateActivation() uses this incoming activation 
  try {
    flag = 0;
    agent_1->updateActivation();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("test-agent-1 threw an exception while updating its activation", flag == 0);

  CPPUNIT_ASSERT_MESSAGE("test-agent-1's incoming activation is non-zero", agent_1->m_incomingActivation == 0.0);
  CPPUNIT_ASSERT_MESSAGE("test-agent-1's activation is zero", agent_1->m_activation > 0.0);
  CPPUNIT_ASSERT_MESSAGE("test-agent-1's activation is (incorrectly) below Agents::ActivationThreshold",
                         agent_1->m_activation >= Agent::ActivationThreshold);

  // Set test-agent-1's activation to 1.0.  For an Agent::ActivationMinimumIncoming
  // of 0.444444 (and given the normalised linke weights above), this ensure that
  // test-agent-2 will exceed both thresholds
  agent_1->m_activation = 1.0;
  agent_2->m_activation = 0.0;
  agent_2->m_incomingActivation = 0.0;
  agent_3->m_activation = 0.0;
  agent_3->m_incomingActivation = 0.0;
  agent_1->tick(); agent_2->tick(); agent_3->tick();
  agent_1->updateActivation();  agent_2->updateActivation();  agent_3->updateActivation();

  CPPUNIT_ASSERT_MESSAGE("test-agent-1's activation is incorrect", agent_1->m_activation == 0.98);
  CPPUNIT_ASSERT_MESSAGE("test-agent-3's activation is incorrect", agent_3->m_activation == 0.0);
  CPPUNIT_ASSERT_MESSAGE("test-agent-2's activation is incorrect", 
                         fabs(agent_2->m_activation - (Agent::ActivationThreshold + ((Agent::ActivationExcitationRate * 0.8 * 1.0) * Agent::ActivationMaximum) * Agent::TimeQuantum)) < 0.000001);
  CPPUNIT_ASSERT_MESSAGE("test-agent-2's activation is (incorrectly) below Agents::ActivationThreshold",
                         agent_2->m_activation >= Agent::ActivationThreshold);

  // And make sure than when test-agent-2 becomes inactive, it removes itself
  // from Agent::AgentsInWorkingMemory
  while (agent_2->m_activation > 0.0) {
    agent_1->updateActivation();  agent_2->updateActivation();  agent_3->updateActivation();
  }
  
  // Test the lock and unlockActivation methods
  agent_1->lockActivation(5.0);
  agent_2->lockActivation(0.0);
  agent_3->m_activation = 0.0;
  agent_3->m_incomingActivation = 0.0;
  // Now, when we tick the agents, agent-1's activation should remain at 5, agent-2's at 0
  // and agent-3 should become active
  agent_1->updateActivation();  agent_2->updateActivation();  agent_3->updateActivation();
  agent_1->tick(); agent_2->tick(); agent_3->tick();
  agent_1->updateActivation();  agent_2->updateActivation();  agent_3->updateActivation();
  agent_1->tick(); agent_2->tick(); agent_3->tick();
  CPPUNIT_ASSERT_MESSAGE("test-agent-1's activation is incorrect", agent_1->m_activation == 5.0);
  CPPUNIT_ASSERT_MESSAGE("test-agent-2's activation is incorrect", agent_2->m_activation == 0.0);
  CPPUNIT_ASSERT_MESSAGE("test-agent-3's activation is incorrect", agent_3->m_activation >  0.0);

  agent_1->unlockActivation();
  agent_2->unlockActivation();
  agent_1->updateActivation();  agent_2->updateActivation();  agent_3->updateActivation();
  CPPUNIT_ASSERT_MESSAGE("test-agent-1's activation is incorrect", agent_1->m_activation <= 1.0);
  CPPUNIT_ASSERT_MESSAGE("test-agent-2's activation is incorrect", agent_2->m_activation > 0.0);

  // Test the p_recentActivity arrays
  /*
  agent_1->lockActivation(0.0);
  agent_2->lockActivation(0.0);
  agent_3->lockActivation(0.0);
  agent_1->tick();  agent_2->tick();  agent_3->tick();
  for (i = 0, flag = 0; i < Agent::RecentActivitySize; i++)
    flag += agent_1->p_recentActivity[i];
  CPPUNIT_ASSERT_MESSAGE("agent_1->p_recentActivity was not reset", flag == 0);
  for (i = 0, flag = 0; i < Agent::RecentActivitySize; i++)
    flag += agent_2->p_recentActivity[i];
  CPPUNIT_ASSERT_MESSAGE("agent_2->p_recentActivity was not reset", flag == 0);
  for (i = 0, flag = 0; i < Agent::RecentActivitySize; i++)
    flag += agent_3->p_recentActivity[i];
  CPPUNIT_ASSERT_MESSAGE("agent_3->p_recentActivity was not reset", flag == 0);

  agent_1->lockActivation(1.0);
  agent_2->lockActivation(0.5);
  agent_3->lockActivation(0.2);
  for (i = 0; i < Agent::RecentActivitySize + 5; i++) {
    agent_1->tick();
    agent_2->tick();
    agent_3->tick();
  }
  for (i = 0, flag = 0; i < Agent::RecentActivitySize; i++)
    flag += agent_1->p_recentActivity[i];
  CPPUNIT_ASSERT_MESSAGE("agent_1's p_recentActivity array reflects an incorrect postTick ratio", flag == 10);
  for (i = 0, flag = 0; i < Agent::RecentActivitySize; i++)
    flag += agent_2->p_recentActivity[i];
  CPPUNIT_ASSERT_MESSAGE("agent_2's p_recentActivity array reflects an incorrect postTick ratio", flag == 5);
  for (i = 0, flag = 0; i < Agent::RecentActivitySize; i++)
    flag += agent_3->p_recentActivity[i];
  CPPUNIT_ASSERT_MESSAGE("agent_3's p_recentActivity array reflects an incorrect postTick ratio", flag == 2);
  
  // and lastly, make sure it handles changes in activation levels on the fly
  agent_1->lockActivation(0.6);
  for (i = 0; i < Agent::RecentActivitySize / 2; i++) {
    agent_1->tick();
    agent_2->tick();
    agent_3->tick();
  }
  for (i = 0, flag = 0; i < Agent::RecentActivitySize; i++)
    flag += agent_1->p_recentActivity[i];
  // (Yeah, it's not very nice, but, especially if Agent::RecentActivitySize is small-ish, or changes, the
  // exact running average is a bit crude --- asymptotically, it will be within 10% though.)
  CPPUNIT_ASSERT_MESSAGE("agent_1's p_recentActivity array reflects an incorrect postTick ratio", (flag > 5) && ( flag < 10));
  for (i = 0; i < Agent::RecentActivitySize; i++) {
    agent_1->tick();
    agent_2->tick();
    agent_3->tick();
  }
  for (i = 0, flag = 0; i < Agent::RecentActivitySize; i++)
    flag += agent_1->p_recentActivity[i];
  CPPUNIT_ASSERT_MESSAGE("agent_1's p_recentActivity array reflects an incorrect postTick ratio", flag == 6);
  
  // Penultimately, we need to test the delivery of outgoing messages
  agent_1->lockActivation(0.0);
  agent_2->lockActivation(0.0);
  agent_3->lockActivation(0.0);
  agent_1->lockActivation(0.9);
  agent_2->lockActivation(0.9);   // message should go out rightaway
  agent_3->lockActivation(0.2);   // message should be delayed by one tick
  agent_1->tick();  agent_2->tick();  agent_3->tick();
  CPPUNIT_ASSERT_MESSAGE("test-agent-2 has a message in its m_incomingMarkers queue", agent_2->m_incomingMarkers.size() == 0);
  CPPUNIT_ASSERT_MESSAGE("test-agent-3 has a message in its m_incomingMarkers queue", agent_3->m_incomingMarkers.size() == 0);
  agent_1->send(agent_2, new Message::Marker(agent_1, agent_1->m_localtime));
  agent_1->send(agent_3, new Message::Marker(agent_1, agent_1->m_localtime));
  agent_1->tick();  agent_2->tick();  agent_3->tick();
  CPPUNIT_ASSERT_MESSAGE("test-agent-2 has no message in its m_incomingMarkers queue", agent_2->m_incomingMarkers.size() == 1);
  CPPUNIT_ASSERT_MESSAGE("test-agent-3 has a message in its m_incomingMarkers queue", agent_3->m_incomingMarkers.size() == 0);
  agent_1->tick();  agent_2->tick();  agent_3->tick();
  CPPUNIT_ASSERT_MESSAGE("test-agent-2 has no message in its m_incomingMarkers queue", agent_2->m_incomingMarkers.size() == 1);
  CPPUNIT_ASSERT_MESSAGE("test-agent-3 has no message in its m_incomingMarkers queue", agent_3->m_incomingMarkers.size() == 1);
  
  // And ideally, we /should/ make sure that Agent::tick() sorts its reference and
  // slot vectors when it /doesn't/ have enough activation to do everything that
  // its derived classes need to in any given tick.  Unfortunately, Agent::nopsThis-
  // Tick() always returns a zero, so this sorting will never occur;  as such, this
  // particular test will have to be defered to Concept- or InstanceAgentTest.
   */
  
  delete agent_3;
}












void ConceptAgentTest::setUp() {
  Agent::AgentNameToPointerMap.clear();

  agent_1 = new ConceptAgent("test-agent-1", Agent::ObjectAgent, "A sample agent");
  agent_2 = new ConceptAgent("test-agent-2", Agent::ObjectAgent, "Another sample agent");
  agent_3 = new ConceptAgent("test-agent-3", Agent::ObjectAgent, "A third sample agent");

  slot_1 = new Slot("test-slot-1", Slot::AspectSlot, "A sample slot");
  slot_2 = new Slot("test-slot-2", Slot::AspectSlot, "Another sample slot");
  slot_3 = new Slot("test-slot-3", Slot::AspectSlot, "A third sample slot");

  agent_1->addSlot(slot_1);
  agent_2->addSlot(slot_2);
  agent_3->addSlot(slot_3);
}




void ConceptAgentTest::tearDown() {
  delete agent_1;
  delete agent_2;
  delete agent_3;

  // the slots are deleted by the agents that own them
}




void ConceptAgentTest::testAddSuperSubClasses() {
  unsigned flag;

  // Try adding super- and sub-classes by name
  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSuperClass(...) failed to resolve existing ConceptAgent",
			   agent_2->addSuperClass("test-agent-1", 1.0) == 0);
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSuperClass(...) incorrectly resolved a non-existent ConceptAgent",
                           agent_2->addSuperClass("test-agent-0", 1.0) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSuperClass(...) threw an exception for a valid agent name", flag == 0);

  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSubClass(...) failed to resolve existing ConceptAgent",
			   agent_2->addSubClass("test-agent-3", 0.5) == 0);
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSubClass(...) incorrectly resolved a non-existent ConceptAgent",
                           agent_2->addSubClass("test-agent-4", 1.0) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSubClass(...) threw an exception for a valid agent name", flag == 0);


  // Make sure they were added to the ConceptAgent::*Classes; note the first pointers
  // should have been resolved automatically, the second should be NULL
  CPPUNIT_ASSERT_MESSAGE("agent_2->superClasses has an incorrect number of entries", 
			 agent_2->superClasses.size() == 2);
  CPPUNIT_ASSERT_MESSAGE("agent_2->subClasses has an incorrect number of entries", 
			 agent_2->subClasses.size() == 2);

  CPPUNIT_ASSERT_MESSAGE("agent_2->superClasses[0] incorrect", 
			 (agent_2->superClasses[0]->name == "test-agent-1") && 
			 (agent_2->superClasses[0]->weight == 1.0));
  CPPUNIT_ASSERT_MESSAGE("agent_2->superClasses[1] incorrect", 
			 (agent_2->superClasses[1]->name == "test-agent-0") && 
			 (agent_2->superClasses[1]->weight == 1.0) );
  CPPUNIT_ASSERT_MESSAGE("agent_2->subClasses[0] incorrect", 
			 (agent_2->subClasses[0]->name == "test-agent-3") && 
			 (agent_2->subClasses[0]->weight == 0.5));
  CPPUNIT_ASSERT_MESSAGE("agent_2->subClasses[1] incorrect", 
                         (agent_2->subClasses[1]->name == "test-agent-4") && 
                         (agent_2->subClasses[1]->weight == 1.0));
  
  CPPUNIT_ASSERT_MESSAGE("agent_2->superClasses[0]->agent is incorrect", 
			 agent_2->superClasses[0]->agent == agent_1);
  CPPUNIT_ASSERT_MESSAGE("agent_2->superClasses[1]->agent is not NULL", 
			 agent_2->superClasses[1]->agent == NULL);
  CPPUNIT_ASSERT_MESSAGE("agent_2->subClasses[0]->agent is incorrect", 
			 agent_2->subClasses[0]->agent == agent_3);
  CPPUNIT_ASSERT_MESSAGE("agent_2->subClasses[1]->agent is not NULL", 
                         agent_2->subClasses[1]->agent == NULL);


  // And make sure that the add*Classes methods throw an exception when we try
  // to add an InstanceAgent
  InstanceAgent *agent_5 = new InstanceAgent("test-agent-5", Agent::ObjectAgent, "An instance agent");
  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSuperClass(...) failed to resolve an existing InstanceAgent",
                           agent_2->addSuperClass("test-agent-5", 1.0) == 0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSuperClass(...) failed to throw an exception for an InstanceAgent", flag == 1);

  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSubClass(...) failed to resolve an existing InstanceAgent",
                           agent_2->addSubClass("test-agent-5", 1.0) == 0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSubClass(...) failed to throw an exception for an InstanceAgent", flag == 1);

  delete agent_5;


  // Verify that Agent::initialise() throws exceptions for non-existent references
  // and correctly resolves valid, but late-coming references
  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to throw an exception for the non-existent agents", flag == 1);

  ConceptAgent *agent_0 = new ConceptAgent("test-agent-0", Agent::ObjectAgent, "A local test agent");
  ConceptAgent *agent_4 = new ConceptAgent("test-agent-4", Agent::ObjectAgent, "Another local test agent");
  try {
    flag = 0;
    agent_2->initialise();
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to resolve the agent reference after they were added", flag == 0);
  CPPUNIT_ASSERT_MESSAGE("agent_2->superClasses[1]->agent is incorrect", 
                         agent_2->superClasses[1]->agent == agent_0);
  CPPUNIT_ASSERT_MESSAGE("agent_2->subClasses[1]->agent is incorrect", 
                         agent_2->subClasses[1]->agent == agent_4);

  
  // Make sure that add*Class(...) methods throw an exception when they are
  // given a slot's name
  try {
    flag = 0;
    agent_2->addSuperClass("test-agent-1.test-slot-1", 1.0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSuperClass(...) failed to throw an exception for an invalid agent name", flag == 1);

  try {
    flag = 0;
    agent_2->addSubClass("test-agent-1.test-slot-1", 1.0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSubClass(...) failed to throw an exception for an invalid agent name", flag == 1);

  // or an agent that has already been added
  try {
    flag = 0;
    agent_2->addSuperClass("test-agent-1", 1.0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSuperClass(...) failed to throw an exception for a duplicate agent", flag == 1);

  try {
    flag = 0;
    agent_2->addSubClass("test-agent-3", 1.0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSubClass(...) failed to throw an exception for a duplicate agent", flag == 1);
  
  delete agent_0;
  delete agent_4;
}




void ConceptAgentTest::testAddInstance() {
  unsigned flag;
  
  // Try adding InstanceAgents by name
  InstanceAgent *agent_4 = new InstanceAgent("test-agent-4", Agent::ObjectAgent, "A local test InstanceAgent");
  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addInstance(...) failed to resolve existing InstanceAgent",
			   agent_2->addInstance("test-agent-4", 1.0) == 0);
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addInstance(...) incorrectly resolved a non-existent InstanceAgent",
                           agent_2->addInstance("test-agent-5", 0.5) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addInstance(...) threw an exception for a valid agent name", flag == 0);


  // Make sure they were added to the ConceptAgent::instances; note the first pointer
  // should have been resolved automatically, the second should be NULL
  CPPUNIT_ASSERT_MESSAGE("agent_2->instances has an incorrect number of entries", 
			 agent_2->instances.size() == 2);

  CPPUNIT_ASSERT_MESSAGE("agent_2->instances[0] incorrect", 
			 (agent_2->instances[0]->name == "test-agent-4") && 
			 (agent_2->instances[0]->weight == 1.0));
  CPPUNIT_ASSERT_MESSAGE("agent_2->instances[1] incorrect", 
			 (agent_2->instances[1]->name == "test-agent-5") && 
			 (agent_2->instances[1]->weight == 0.5) );

  CPPUNIT_ASSERT_MESSAGE("agent_2->instances[0]->agent is incorrect", 
			 agent_2->instances[0]->agent == agent_4);
  CPPUNIT_ASSERT_MESSAGE("agent_2->instances[1]->agent is not NULL", 
			 agent_2->instances[1]->agent == NULL);


  // And make sure that the ConceptAgent::addInstance throws an exception when we try
  // to add a ConceptAgent
  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addInstance(...) failed to resolve an existing ConceptAgent",
                           agent_2->addInstance("test-agent-1", 1.0) == 0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addInstance(...) failed to throw an exception for a ConceptAgent", flag == 1);


  // Verify that ConceptAgent::initialise() throws exceptions for non-existent references
  // and correctly resolves valid, but late-coming references
  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to throw an exception for the non-existent agent", flag == 1);

  InstanceAgent *agent_5 = new InstanceAgent("test-agent-5", Agent::ObjectAgent, "A local test InstanceAgent");
  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to resolve the agent reference after it was added", flag == 0);
  CPPUNIT_ASSERT_MESSAGE("agent_2->instances[1]->agent is incorrect", 
                         agent_2->instances[1]->agent == agent_5);



  // Make sure that ConceptAgent::addInstance(...) throws an exception it is
  // given a slot's name
  try {
    flag = 0;
    agent_2->addInstance("test-agent-1.test-slot-1", 1.0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addInstance(...) failed to throw an exception for an invalid agent name", flag == 1);

  // or an agent that has already been added
  try {
    flag = 0;
    agent_2->addInstance("test-agent-1", 1.0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addInstance(...) failed to throw an exception for a duplicate agent", flag == 1);
  
  delete agent_4;
  delete agent_5;
}




void ConceptAgentTest::testInitialise() {
  unsigned flag;
  Agent *agent_4, *agent_5, *agent_6;

  // We don't need to verify that ConceptAgent::initialise() correctly
  // resolves superclass, subclass and instance references for existing
  // agents --- this was already done above


  // Verify that ConceptAgent::initialise() throws exceptions when it tries
  // to add incorrectly-typed, late-comers to the superclass, subclass and
  // instance vectors
  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSuperClass(...) incorrectly resolved a non-existent agent",
                           agent_2->addSuperClass("test-agent-4", 0.7) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSuperClass(...) threw an exception for a valid agent name", flag == 0);
  
  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to throw an exception for the non-existent agents", flag == 1);
  
  agent_4 = new InstanceAgent("test-agent-4", Agent::ObjectAgent, "A local test InstanceAgent");
  try {
    flag = 0;
    agent_2->initialise();
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to throw an exception upon finding an InstanceAgent in ConceptAgent::superClasses", flag == 1);
  delete agent_4;

  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSuperClass(...) incorrectly resolved a non-existent agent",
                           agent_2->addSuperClass("test-agent-4", 0.7) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSuperClass(...) threw an exception for a valid agent name", flag == 0);

  agent_4 = new ConceptAgent("test-agent-4", Agent::ObjectAgent, "A local test ConceptAgent");

  try {
    flag = 0;
    agent_2->initialise();
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to resolve the ConceptAgent pointed to by superClass[0]", flag == 0);
  
  
  // Repeat for a subclass
  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSubclass(...) incorrectly resolved a non-existent agent",
                           agent_2->addSubClass("test-agent-5", 0.8) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSubclass(...) threw an exception for a valid agent name", flag == 0);

  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to throw an exception for the non-existent agents", flag == 1);

  agent_5 = new InstanceAgent("test-agent-5", Agent::ObjectAgent, "A local test InstanceAgent");
  try {
    flag = 0;
    agent_2->initialise();
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to throw an exception upon finding an InstanceAgent in ConceptAgent::subclasses", flag == 1);
  delete agent_5;

  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSubclass(...) incorrectly resolved a non-existent agent",
                           agent_2->addSubClass("test-agent-5", 0.8) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addSubclass(...) threw an exception for a valid agent name", flag == 0);
  
  agent_5 = new ConceptAgent("test-agent-5", Agent::ObjectAgent, "A local test ConceptAgent");

  try {
    flag = 0;
    agent_2->initialise();
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to resolve the ConceptAgent pointed to by subclass[0]", flag == 0);

  // Repeat for an instance
  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addInstance(...) incorrectly resolved a non-existent agent",
                           agent_2->addInstance("test-agent-6", 0.9) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addInstance(...) threw an exception for a valid agent name", flag == 0);

  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to throw an exception for the non-existent agents", flag == 1);

  agent_6 = new ConceptAgent("test-agent-6", Agent::ObjectAgent, "A local test ConceptAgent");
  try {
    flag = 0;
    agent_2->initialise();
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to throw an exception upon finding an InstanceAgent in ConceptAgent::instancees", flag == 1);
  delete agent_6;

  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addInstance(...) incorrectly resolved a non-existent agent",
                           agent_2->addInstance("test-agent-6", 0.9) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::addInstance(...) threw an exception for a valid agent name", flag == 0);
  
  agent_6 = new InstanceAgent("test-agent-6", Agent::ObjectAgent, "A local test InstanceAgent");

  try {
    flag = 0;
    agent_2->initialise();
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::initialise() failed to resolve the ConceptAgent pointed to by instance[0]", flag == 0);

  
  // Now, we need to exercise the maximal link weight aspect of ::initialise().  At this point,
  // we have agent_2->superClass == agent_4 (0.7), agent_2->subClass == agent_5 (0.8),
  // agent_2->instance == agent_6 (0.9)
  agent_4->addSlot(new Slot("test-slot-4", Slot::AspectSlot, "A local sample slot"));
  agent_5->addSlot(new Slot("test-slot-5", Slot::AspectSlot, "Another local sample slot"));
  agent_6->addSlot(new Slot("test-slot-6", Slot::AspectSlot, "Another local sample slot"));
  
  try {
    flag = 0;
    // Add some coreferences (they, associations and slots have already been 
    // tested in AgentTest::testInitialise()
    agent_2->addCoreference("test-agent-1.test-slot-1", 0.05);
    agent_2->addCoreference("test-agent-3.test-slot-3", 0.1);
    agent_2->addCoreference("test-agent-4.test-slot-4", 0.65);
    agent_2->addCoreference("test-agent-5.test-slot-5", 0.8);
    agent_2->addCoreference("test-agent-6.test-slot-6", 0.95);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("An exception was thrown while adding references to agent_2", flag == 0);

  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::initialise() threw and exception", flag == 0);
  
  CPPUNIT_ASSERT_MESSAGE("agent_2->initialise() failed to find the maximal inter-agent link weight",
                         agent_2->m_agentPointerToLinkWeightMap[agent_1] == (double) 0.05);
  CPPUNIT_ASSERT_MESSAGE("agent_2->initialise() failed to find the maximal inter-agent link weight",
                         agent_2->m_agentPointerToLinkWeightMap[agent_3] == (double) 0.1);
  CPPUNIT_ASSERT_MESSAGE("agent_2->initialise() failed to find the maximal inter-agent link weight",
                         agent_2->m_agentPointerToLinkWeightMap[agent_4] == (double) 0.7);
  CPPUNIT_ASSERT_MESSAGE("agent_2->initialise() failed to find the maximal inter-agent link weight",
                         agent_2->m_agentPointerToLinkWeightMap[agent_5] == (double) 0.8);
  CPPUNIT_ASSERT_MESSAGE("agent_2->initialise() failed to find the maximal inter-agent link weight",
                         agent_2->m_agentPointerToLinkWeightMap[agent_6] == (double) 0.95);
  
  // Finally, we should check agent_2->m_agentPointerToNormalisedLinkWeightMap to verify that
  // its sum is (pretty close to) 1.0


  // Next, we need to make sure that the slots and the agent/slot reference
  // vectors are only sorted when needed.  To do this, we'll fiddle around with
  // the agents' innards.  (Note that we couldn't really do this in AgentTest::
  // testInitialise as nopsThisTick() always returns zero.)
  
  delete agent_4;
  delete agent_5;
  delete agent_6;
}




void ConceptAgentTest::testPrintParse() {
  #ifdef __DEBUG__INTERACTIVE_TESTS
  char answer[4096];
  FILE *tmp = tmpfile();
  ConceptAgent  *temp_c;
  InstanceAgent *temp_i = new InstanceAgent("test-agent-4", Agent::ObjectAgent, "A local test InstanceAgent");


  agent_1->addCoreference("test-agent-2.test-slot-2", 1.0);
  agent_2->addCoreference("test-agent-3.test-slot-3", 1.0);
  agent_3->addCoreference("test-agent-1.test-slot-1", 0.5);

  agent_1->addAssociation("test-agent-2", 1.0);
  agent_2->addAssociation("test-agent-3", 0.5);
  agent_3->addAssociation("test-agent-1", 0.25);

  agent_1->addSubClass("test-agent-2", 1.0);
  agent_2->addSuperClass("test-agent-1", 1.0);
  agent_2->addSubClass("test-agent-3", 1.0);
  agent_3->addSubClass("test-agent-2", 1.0);
  agent_2->addInstance("test-agent-4", 1.0);

  answer[0] = 'y';
  printf("\nAbout to print [(ConceptAgent) agent_1] with no indent:\n");
  agent_1->print(stdout, "");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::print(...) failed to print correctly", answer[0] == 'y');

  answer[0] = 'y';
  printf("\nAbout to print [(ConceptAgent) agent_2] with no indent:\n");
  agent_2->print(stdout, "");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::print(...) failed to print correctly", answer[0] == 'y');

  answer[0] = 'y';
  printf("\nAbout to print [(ConceptAgent) agent_3] with a two-space indent:\n");
  agent_3->print(stdout, "  ");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::print(...) failed to print correctly", answer[0] == 'y');


  // Now we are going to 'print' an agent to our temporary file
  // are parse it back in --- the resulting data should, of
  // course, be the same
  agent_2->print(tmp, "");
  agent_2->addAssociation("this-should-only-appear-in-the-second-parsing", 0.0);
  agent_2->print(tmp, "  ");
  fprintf(tmp, "(defagent  decoy-agent  \"A decoy agent representing the start of the next agent\"\n");
  fflush(tmp);
  fseek(tmp, 0, SEEK_SET);   // rewind

  printf("\nAbout to print two copies of the parsed test-agent-2 with a two-space indent\n--- with the exception of the agent names and comments, the first should\nmatch previous printout, while the second should have an additional association:\n\n");
  temp_c = new ConceptAgent("ignore-this-mismatch", Agent::SituationAgent, "The agent's name and comment field don't need to match");
  fgets(answer, 4096, tmp);  // skip over the first (agent-defining) line
  try {
    temp_c->parse(tmp);
  } catch (Exception::BaseException e) {
    printf("Exception caught!  %s\n", e.comment.c_str());
  }
  temp_c->print(stdout, "  ");
  delete temp_c;
  temp_c = new ConceptAgent("ignore-this-mismatch-too", Agent::SituationAgent, "See above");
  fgets(answer, 4096, tmp);  // skip over the next agent-defining line
  try {
    temp_c->parse(tmp);
  } catch (Exception::BaseException e) {
    printf("Exception caught!  %s\n", e.comment.c_str());
  }
  temp_c->print(stdout, "  ");
  printf("\nDoes this look right?  (y/n) ");
  answer[0] = 'y';
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("ConceptAgent::parse(...) failed to correctly parse the output of ConceptAgent::print", answer[0] == 'y');

  delete temp_i;
  delete temp_c;
  fclose(tmp);
  #endif
}




void ConceptAgentTest::testTick() {
  // Create, add and initialise all of the agents that we need

  // Check to see if Agent::tick() 
}












void InstanceAgentTest::setUp() {
  Agent::AgentNameToPointerMap.clear();

  agent_1 = new InstanceAgent("test-agent-1", Agent::ObjectAgent, "A sample agent");
  agent_2 = new InstanceAgent("test-agent-2", Agent::ObjectAgent, "Another sample agent");
  agent_3 = new InstanceAgent("test-agent-3", Agent::ObjectAgent, "A third sample agent");

  slot_1 = new Slot("test-slot-1", Slot::AspectSlot, "A sample slot");
  slot_2 = new Slot("test-slot-2", Slot::AspectSlot, "Another sample slot");
  slot_3 = new Slot("test-slot-3", Slot::AspectSlot, "A third sample slot");

  agent_1->addSlot(slot_1);
  agent_2->addSlot(slot_2);
  agent_3->addSlot(slot_3);
}




void InstanceAgentTest::tearDown() {
  delete agent_1;
  delete agent_2;
  delete agent_3;

  // the slots are deleted by the agents that own them
}




void InstanceAgentTest::testAddInstanceOf() {
  unsigned flag;
  
  // Try adding ConceptAgents by name
  ConceptAgent *agent_4 = new ConceptAgent("test-agent-4", Agent::ObjectAgent, "A local test ConceptAgent");
  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("InstanceAgent::addInstanceOf(...) failed to resolve existing ConceptAgent",
			   agent_2->addInstanceOf("test-agent-4", 1.0) == 0);
    CPPUNIT_ASSERT_MESSAGE("InstanceAgent::addInstanceOf(...) incorrectly resolved a non-existent ConceptAgent",
                           agent_2->addInstanceOf("test-agent-5", 0.5) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::addInstanceOf(...) threw an exception for a valid agent name", flag == 0);


  // Make sure they were added to the InstanceAgent::instances; note the first pointer
  // should have been resolved automatically, the second should be NULL
  CPPUNIT_ASSERT_MESSAGE("agent_2->instanceOf has an incorrect number of entries", 
			 agent_2->instanceOf.size() == 2);

  CPPUNIT_ASSERT_MESSAGE("agent_2->instanceOf[0] incorrect", 
			 (agent_2->instanceOf[0]->name == "test-agent-4") && 
			 (agent_2->instanceOf[0]->weight == 1.0));
  CPPUNIT_ASSERT_MESSAGE("agent_2->instanceOf[1] incorrect", 
			 (agent_2->instanceOf[1]->name == "test-agent-5") && 
			 (agent_2->instanceOf[1]->weight == 0.5) );

  CPPUNIT_ASSERT_MESSAGE("agent_2->instanceOf[0]->agent is incorrect", 
			 agent_2->instanceOf[0]->agent == agent_4);
  CPPUNIT_ASSERT_MESSAGE("agent_2->instanceOf[1]->agent is not NULL", 
			 agent_2->instanceOf[1]->agent == NULL);


  // And make sure that the InstanceAgent::addInstanceOf throws an exception when we try
  // to add an InstanceAgent
  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("InstanceAgent::addInstanceOf(...) failed to resolve an existing ConceptAgent",
                           agent_2->addInstanceOf("test-agent-1", 1.0) == 0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::addInstanceOf(...) failed to throw an exception for an InstanceAgent", flag == 1);


  // Verify that InstanceAgent::initialise() throws exceptions for non-existent references
  // and correctly resolves valid, but late-coming references
  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::initialise() failed to throw an exception for the non-existent agent", flag == 1);

  ConceptAgent *agent_5 = new ConceptAgent("test-agent-5", Agent::ObjectAgent, "A local test ConceptAgent");
  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::initialise() failed to resolve the agent reference after it was added", flag == 0);
  CPPUNIT_ASSERT_MESSAGE("agent_2->instanceOf[1]->agent is incorrect", 
                         agent_2->instanceOf[1]->agent == agent_5);



  // Make sure that InstanceAgent::addInstanceOf(...) throws an exception it is
  // given a slot's name
  try {
    flag = 0;
    agent_2->addInstanceOf("test-agent-1.test-slot-1", 1.0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::addInstanceOf(...) failed to throw an exception for an invalid agent name", flag == 1);

  // or an agent that has already been added
  try {
    flag = 0;
    agent_2->addInstanceOf("test-agent-1", 1.0);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::addInstanceOf(...) failed to throw an exception for a duplicate agent", flag == 1);
  
  delete agent_4;
  delete agent_5;
}




void InstanceAgentTest::testInitialise() {
  unsigned flag;
  Agent *temp_a;

  // We don't need to verify that InstanceAgent::initialise() correctly
  // resolves instanceOf references for existing agents --- this was
  // already done above


  // Verify that InstanceAgent::initialise() throws exceptions when it tries
  // to add incorrectly-typed, late-comers to the superclass, subclass and
  // instance vectors
  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("InstanceAgent::addInstanceOf(...) incorrectly resolved a non-existent agent",
                           agent_2->addInstanceOf("test-agent-4", 0.5) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::addInstanceOf(...) threw an exception for a valid agent name", flag == 0);

  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::initialise() failed to throw an exception for the non-existent agents", flag == 1);

  temp_a = new InstanceAgent("test-agent-4", Agent::ObjectAgent, "A local test InstanceAgent");
  try {
    flag = 0;
    agent_2->initialise();
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::initialise() failed to throw an exception upon finding an InstanceAgent in InstanceAgent::instanceOf", flag == 1);
  delete temp_a;

  try {
    flag = 0;
    CPPUNIT_ASSERT_MESSAGE("InstanceAgent::addInstanceOf(...) incorrectly resolved a non-existent agent",
                           agent_2->addInstanceOf("test-agent-4", 0.5) == 1);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::addInstanceOf(...) threw an exception for a valid agent name", flag == 0);
  
  temp_a = new ConceptAgent("test-agent-4", Agent::ObjectAgent, "A local test ConceptAgent");

  try {
    flag = 0;
    agent_2->initialise();
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::initialise() failed to resolve the ConceptAgent pointed to by instanceOf[0]", flag == 0);
  
  
  
  
  // Now, we need to exercise the maximal link weight aspect of ::initialise().  At this point,
  // we have agent_2->instanceOf == agent_4 (0.8)
  temp_a->addSlot(new Slot("test-slot-4", Slot::AspectSlot, "A local sample slot"));
  
  try {
    flag = 0;
    // Add some coreferences (they, associations and slots have already been 
    // tested in AgentTest::testInitialise()
    agent_2->addCoreference("test-agent-1.test-slot-1", 0.1);
    agent_2->addCoreference("test-agent-3.test-slot-3", 0.2);
    agent_2->addCoreference("test-agent-4.test-slot-4", 0.3);
    agent_2->addAssociation("test-agent-4", 0.4);
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("An exception was thrown while adding references to agent_2", flag == 0);
  
  try {
    flag = 0;
    agent_2->initialise();
  } catch (...) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Agent::initialise() threw and exception", flag == 0);
  
  CPPUNIT_ASSERT_MESSAGE("agent_2->initialise() failed to find the maximal inter-agent link weight",
                         agent_2->m_agentPointerToLinkWeightMap[agent_1] == (double) 0.1);
  CPPUNIT_ASSERT_MESSAGE("agent_2->initialise() failed to find the maximal inter-agent link weight",
                         agent_2->m_agentPointerToLinkWeightMap[agent_3] == (double) 0.2);
  CPPUNIT_ASSERT_MESSAGE("agent_2->initialise() failed to find the maximal inter-agent link weight",
                         agent_2->m_agentPointerToLinkWeightMap[temp_a] == (double) 0.5);
  
  delete temp_a;
}





void InstanceAgentTest::testPrintParse() {
  #ifdef __DEBUG__INTERACTIVE_TESTS
  char answer[4096];
  FILE *tmp = tmpfile();
  ConceptAgent *temp_c = new ConceptAgent("test-agent-4", Agent::ObjectAgent, "A local test ConceptAgent");
  InstanceAgent *temp_i;


  agent_1->addCoreference("test-agent-2.test-slot-2", 1.0);
  agent_2->addCoreference("test-agent-3.test-slot-3", 1.0);
  agent_3->addCoreference("test-agent-1.test-slot-1", 0.5);

  agent_1->addAssociation("test-agent-2", 1.0);
  agent_2->addAssociation("test-agent-3", 0.5);
  agent_3->addAssociation("test-agent-1", 0.25);

  agent_2->addInstanceOf("test-agent-4", 1.0);

  answer[0] = 'y';
  printf("\nAbout to print [(InstanceAgent) agent_1] with no indent:\n");
  agent_1->print(stdout, "");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::print(...) failed to print correctly", answer[0] == 'y');

  answer[0] = 'y';
  printf("\nAbout to print [(InstanceAgent) agent_2] with no indent:\n");
  agent_2->print(stdout, "");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::print(...) failed to print correctly", answer[0] == 'y');

  answer[0] = 'y';
  printf("\nAbout to print [(InstanceAgent) agent_3] with a two-space indent:\n");
  agent_3->print(stdout, "  ");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::print(...) failed to print correctly", answer[0] == 'y');



  // Now we are going to 'print' an agent to our temporary file
  // are parse it back in --- the resulting data should, of
  // course, be the same
  agent_2->print(tmp, "");
  agent_2->addAssociation("this-should-only-appear-in-the-second-parsing", 0.0);
  agent_2->print(tmp, "  ");
  fprintf(tmp, "(defagent  decoy-agent  \"A decoy agent representing the start of the next agent\"\n");
  fflush(tmp);
  fseek(tmp, 0, SEEK_SET);   // rewind

  printf("\nAbout to print two copies of the parsed test-agent-2 with a two-space indent\n--- with the exception of the agent names and comments, the first should\nmatch previous printout, while the second should have an additional association:\n\n");
  temp_i = new InstanceAgent("ignore-this-mismatch", Agent::SituationAgent, "The agent's name and comment field don't need to match");
  fgets(answer, 4096, tmp);  // skip over the first (agent-defining) line
  try {
    temp_i->parse(tmp);
  } catch (Exception::BaseException e) {
    printf("Exception caught!  %s\n", e.comment.c_str());
  }
  temp_i->print(stdout, "  ");
  delete temp_i;
  temp_i = new InstanceAgent("ignore-this-mismatch-too", Agent::SituationAgent, "See above");
  fgets(answer, 4096, tmp);  // skip over the next agent-defining line
  try {
    temp_i->parse(tmp);
  } catch (Exception::BaseException e) {
    printf("Exception caught!  %s\n", e.comment.c_str());
  }
  temp_i->print(stdout, "  ");
  printf("\nDoes this look right?  (y/n) ");
  answer[0] = 'y';
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("InstanceAgent::parse(...) failed to correctly parse the output of InstanceAgent::print", answer[0] == 'y');

  delete temp_i;
  delete temp_c;
  fclose(tmp);
  #endif
}
