#include "SlotTest.hpp"


void SlotTest::setUp() {
  slot = new Slot("test-slot", Slot::AspectSlot, "A sample slot");
  slot_1 = new Slot("slot-1", Slot::AspectSlot, "agent-1's sample slot");
  slot_2 = new Slot("slot-1", Slot::AspectSlot, "agent-2's sample slot");

  agent_1 = new Agent("agent-1", Agent::ObjectAgent, "A sample agent");
  agent_2 = new Agent("agent-2", Agent::ObjectAgent, "Another sample agent");
  agent_1->addSlot(slot_1);
  agent_2->addSlot(slot_2);
}




void SlotTest::tearDown() {
  delete slot;

  delete agent_1;
  delete agent_2;
}




void SlotTest::testConstructor() {
  Slot slot1("local-slot-1", Slot::AspectSlot, "A local test slot");
  Slot slot2("local-slot-2", Slot::RelationSlot, "Another local test slot");

  CPPUNIT_ASSERT_MESSAGE("Slot::Slot(...) failed to set name", slot1.name == "local-slot-1");
  CPPUNIT_ASSERT_MESSAGE("Slot::Slot(...) failed to set name", slot2.name == "local-slot-2");

  CPPUNIT_ASSERT_MESSAGE("Slot::Slot(...) failed to set type", slot1.type == Slot::AspectSlot);
  CPPUNIT_ASSERT_MESSAGE("Slot::Slot(...) failed to set type", slot2.type == Slot::RelationSlot);

  CPPUNIT_ASSERT_MESSAGE("Slot::Slot(...) failed to set comment", slot1.comment == "A local test slot");
  CPPUNIT_ASSERT_MESSAGE("Slot::Slot(...) failed to set comment", slot2.comment == "Another local test slot");
}




void SlotTest::testAddSuperClass() {
  unsigned flag;
  
  try {
    flag = 0;
    slot->addSuperClass("agent-1.slot-1", 0.5);
    slot->addSuperClass("agent-2.slot-1", 1.0);
  } catch (Exception::InvalidSlot e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Slot::addSuperClass(...) threw an exception for a valid slot name", !flag);

  CPPUNIT_ASSERT_MESSAGE("Slot::addSuperClass(...) failed to add all of the slots", slot->superClasses.size() == 2);
  CPPUNIT_ASSERT_MESSAGE("Slot::addSuperClass(...) failed to set slot name", slot->superClasses[0]->name == "agent-1.slot-1");
  CPPUNIT_ASSERT_MESSAGE("Slot::addSuperClass(...) failed to set slot weight", slot->superClasses[0]->weight == 0.5);

  try {
    flag = 0;
    slot->addSuperClass("agent-2", 1.0);
  } catch (Exception::InvalidSlot e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Slot::addSuperClass(...) failed to throw an exception for an invalid slot name", flag);

  try {
    flag = 0;
    slot->addSuperClass("agent-1.slot-1", 0.5);
  } catch (Exception::InvalidSlot e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Slot::addSuperClass(...) failed to throw an exception for a duplicate slot name", flag);
}




void SlotTest::testAddInstanceOf() {
  unsigned flag;
  
  try {
    flag = 0;
    slot->addInstanceOf("agent-1.slot-1", 0.5);
    slot->addInstanceOf("agent-2.slot-1", 1.0);
  } catch (Exception::InvalidSlot e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Slot::addInstanceOf(...) threw an exception for a valid slot name", !flag);

  CPPUNIT_ASSERT_MESSAGE("Slot::addInstanceOf(...) failed to add all of the slots", slot->instanceOf.size() == 2);
  CPPUNIT_ASSERT_MESSAGE("Slot::addInstanceOf(...) failed to set slot name", slot->instanceOf[0]->name == "agent-1.slot-1");
  CPPUNIT_ASSERT_MESSAGE("Slot::addInstanceOf(...) failed to set slot weight", slot->instanceOf[0]->weight == 0.5);

  try {
    flag = 0;
    slot->addInstanceOf("agent-2", 1.0);
  } catch (Exception::InvalidSlot e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Slot::addInstanceOf(...) failed to throw an exception for an invalid slot name", flag);

  try {
    flag = 0;
    slot->addInstanceOf("agent-1.slot-1", 0.5);
  } catch (Exception::InvalidSlot e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Slot::addInstanceOf(...) failed to throw an exception for a duplicate slot name", flag);
}




void SlotTest::testAddCoreference() {
  unsigned flag;
  
  try {
  flag = 0;
    slot->addCoreference("agent-1", 0.5);
    slot->addCoreference("agent-2", 1.0);
  } catch (Exception::InvalidSlot e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Slot::addCoreference(...) threw an exception for a valid agent name", !flag);

  CPPUNIT_ASSERT_MESSAGE("Slot::addCoreference(...) failed to add all of the slots", slot->coreferences.size() == 2);
  CPPUNIT_ASSERT_MESSAGE("Slot::addCoreference(...) failed to set slot name", slot->coreferences[0]->name == "agent-1");
  CPPUNIT_ASSERT_MESSAGE("Slot::addCoreference(...) failed to set slot weight", slot->coreferences[0]->weight == 0.5);

  try {
    flag = 0;
    slot->addCoreference("agent-2.slot-1", 1.0);
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Slot::addCoreference(...) failed to throw an exception for an invalid agent name", flag);

  try {
    flag = 0;
    slot->addCoreference("agent-1", 0.5);
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Slot::addCoreference(...) failed to throw an exception for a duplicate agent name", flag);
}




void SlotTest::testAddAssociation() {
  unsigned flag;
  
  try {
    flag = 0;
    slot->addAssociation("agent-1", 0.5);
    slot->addAssociation("agent-2", 1.0);
  } catch (Exception::InvalidSlot e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Slot::addAssociation(...) threw an exception for a valid agent name", !flag);

  CPPUNIT_ASSERT_MESSAGE("Slot::addAssociation(...) failed to add all of the slots", slot->associations.size() == 2);
  CPPUNIT_ASSERT_MESSAGE("Slot::addAssociation(...) failed to set slot name", slot->associations[0]->name == "agent-1");
  CPPUNIT_ASSERT_MESSAGE("Slot::addAssociation(...) failed to set slot weight", slot->associations[0]->weight == 0.5);

  try {
    flag = 0;
    slot->addAssociation("agent-2.slot-1", 1.0);
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Slot::addAssociation(...) failed to throw an exception for an invalid agent name", flag);

  try {
    flag = 0;
    slot->addAssociation("agent-1", 0.5);
  } catch (Exception::InvalidAgent e) {
    flag = 1;
  }
  CPPUNIT_ASSERT_MESSAGE("Slot::addAssociation(...) failed to throw an exception for a duplicate agent name", flag);
}




void SlotTest::testResolveReferences() {
}




void SlotTest::testPrintParse() {
  #ifdef __DEBUG__INTERACTIVE_TESTS
  char answer[4096];
  FILE *tmp = tmpfile();
  
  slot->addSuperClass("agent-1.slot-1", 0.1);
  slot->addSuperClass("agent-2.slot-1", 0.2);

  slot->addInstanceOf("agent-1.slot-1", 0.3);
  slot->addInstanceOf("agent-2.slot-1", 0.4);

  slot->addCoreference("agent-2", 0.5);
  slot->addAssociation("agent-2", 0.6);


  answer[0] = 'y';
  printf("\nAbout to print [slot] with no indent:\n");
  slot->print(stdout, "");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("Slot::print(...) failed to print correctly", answer[0] == 'y');

  answer[0] = 'y';
  printf("\nAbout to print [slot] with a two-space indent:\n");
  slot->print(stdout, "  ");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("Slot::print(...) failed to print correctly with a two-space indent", answer[0] == 'y');


  // Now we are going to 'print' slot to our temporary file
  // are parse it back in --- the resulting data should, of
  // course, be the same
  slot->print(tmp, "  ");
  slot->addAssociation("this-should-only-appear-in-the-second-parsing", 0.0);
  slot->print(tmp, "  ");
  fprintf(tmp, "  :decoy-slot  \"A decoy slot representing the start of the next slot\"");
  fflush(tmp);
  fseek(tmp, 0, SEEK_SET);   // rewind
  fgets(answer, 4096, tmp);  // skip over the first (slot-defining) line

  slot_1 = new Slot("ignore-this-mismatch", Slot::RelationSlot, "The slot's name and comment field don't need to match");
  slot_1->parse(tmp);
  fgets(answer, 4096, tmp);  // skip over the next slot-defining line
  slot_2 = new Slot("ignore-this-mismatch-too", Slot::RelationSlot, "See above");
  slot_2->parse(tmp);
  answer[0] = 'y';
  printf("\nAbout to print two copies of the parsed slot with a two-space indent --- with\nthe exception of the slot names and comments, the first should match the \nprevious printout, while the second should have an additional association:\n\n");
  slot_1->print(stdout, "  ");
  slot_2->print(stdout, "  ");
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("Slot::parse(...) failed to correctly parse the output of Slot::print", answer[0] == 'y');

  delete slot_1;
  delete slot_2;
  fclose(tmp);
  #endif
}
