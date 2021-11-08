#include "ManagerTest.hpp"

using namespace VariableRateExecution;
using namespace VariableRateExecution::Test;


void ManagerTest::setUp() {
  char name[32];
  unsigned i, numberOfAgents = 20;

  manager = new Manager(10, 0.05, 0.0, 1.0, 0.2);
  for (i = 0; i < numberOfAgents; i++) {
    sprintf(name, "test-agent-%d", i);
    agents.push_back(new TestAgent(string(name), i / ((double) (numberOfAgents - 1)), manager));
  }
}




void ManagerTest::tearDown() {
  // Note that we don't need to delete the agents as that's handled by the 
  // manager's destructor
  delete manager;
}




void ManagerTest::testConstructorDestructor() {
  unsigned i;

  CPPUNIT_ASSERT_MESSAGE("m_running == true", manager->m_running == false);

  for (i = 0; i < manager->m_priorityLevels; i++)
    CPPUNIT_ASSERT_MESSAGE("m_agentPriorityQueue[i].size() != m_agentPriorityQueueSize[i]",
			   manager->m_agentPriorityQueue[i].size() == manager->m_agentPriorityQueueSize[i]);

  CPPUNIT_ASSERT_MESSAGE("m_agentPointerMap.size() != agents.size()", 
			 manager->m_agentPointerMap.size() == agents.size());
}

   

   
void ManagerTest::testAddRemoveAgent() {
  unsigned i, j, flag, queue;
  TestAgent *agent;

  // Make sure that the elements of [agents] are in m_agentPointerMap and the
  // appropriate priority queues
  for (i = 0; i < agents.size(); i++) {
    CPPUNIT_ASSERT_MESSAGE("agent not in manager->m_agentPointerMap", 
			   (manager->m_agentPointerMap.find(agents[i]) != manager->m_agentPointerMap.end()));
    queue = agents[i]->priorityLevel();
    for (flag = 0, j = 0; j < manager->m_agentPriorityQueueSize[queue]; j++)
      if (manager->m_agentPriorityQueue[queue][j] == agents[i]) {
	flag = 1;
	break;
      }

    CPPUNIT_ASSERT_MESSAGE("agent[i] was not found in the queue returned by agent[i]->priorityLevel()",
			   flag == 1);
  }

  // Create a new agent
  agent = new TestAgent("local-test-agent", 1.0, manager);
  queue = agent->priorityLevel();
  CPPUNIT_ASSERT_MESSAGE("m_agentPointerMap.size() != agents.size() + 1", 
  		 manager->m_agentPointerMap.size() == agents.size() + 1);
  for (flag = 0, i = 0; i < manager->m_agentPriorityQueueSize[queue]; i++)
    if (manager->m_agentPriorityQueue[queue][i] == agent) {
      flag = 1;
      break;
    }
  CPPUNIT_ASSERT_MESSAGE("agent was not found in the queue returned by agent->priorityLevel()",
			 flag == 1);

  // remove and delete it
  //manager->removeAgent(agent);
  delete agent;

  CPPUNIT_ASSERT_MESSAGE("m_agentPointerMap.size() != agents.size()", 
		 manager->m_agentPointerMap.size() == agents.size());
  for (flag = 0, i = 0; i < manager->m_agentPriorityQueueSize[queue]; i++)
    if (manager->m_agentPriorityQueue[queue][i] == agent) {
      flag = 1;
      break;
    }
  CPPUNIT_ASSERT_MESSAGE("agent was found in the queue returned by agent->priorityLevel() before it was deleted",
			 flag == 0);
}




void ManagerTest::testStartStop() {
  unsigned i;

  CPPUNIT_ASSERT_MESSAGE("m_running == true", manager->m_running == false);
  CPPUNIT_ASSERT_MESSAGE("m_time != 0", manager->m_time == 0);
  manager->tick();
  manager->printAgentSummaries();
  CPPUNIT_ASSERT_MESSAGE("m_running == true", manager->m_running == false);
  CPPUNIT_ASSERT_MESSAGE("m_time == 0", manager->m_time == 1);
  manager->start();
  usleep(1000);
  CPPUNIT_ASSERT_MESSAGE("m_running == false", manager->m_running == true);
  CPPUNIT_ASSERT_MESSAGE("m_time == 0", manager->m_time > 1);
  manager->printAgentSummaries();
  
  for (i = 0; i < agents.size(); i++)
    manager->sendMessage(agents[i], agents[(i + 1) % agents.size()], new Message::BaseMessage(0));

  usleep(4999000);
  manager->stop();
  manager->printAgentSummaries();


  i = manager->m_time;
  CPPUNIT_ASSERT_MESSAGE("m_running == true", manager->m_running == false);
  usleep(10000);
  CPPUNIT_ASSERT_MESSAGE("Manager::stop() failed to stop the run-loop", manager->m_time - i < 2);

  /*
  for (i = 0; i < agents.size(); i++) {
    printf("agent %s ticked %d times\n", agents[i]->name.c_str(), agents[i]->totalNumberOfTicks());
    CPPUNIT_ASSERT_MESSAGE("agents[i] ticked less often than agents[i-1]",
			   (agents[i]->totalNumberOfTicks() >= previousNumberOfTicks));
    CPPUNIT_ASSERT_MESSAGE("agents[i]->p_totalNOPSThisTick is less than agents[i-1]'s",
			   (agents[i]->p_totalNOPSThisTick >= previousNumberOfNOPS));
    previousNumberOfTicks = agents[i]->totalNumberOfTicks();
    previousNumberOfNOPS = agents[i]->p_totalNOPSThisTick;
  }
   */
}

