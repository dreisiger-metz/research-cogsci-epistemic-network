#include "AgentTest.hpp"


using namespace std;
using namespace VariableRateExecution;
using namespace VariableRateExecution::Test;


void AgentTest::setUp() {
  manager = new Manager(10, 0.01, 0.0, 1.0, 0.1);
  agent_1 = new TestAgent("test-agent-1", 1.0, manager);
  agent_2 = new TestAgent("test-agent-2", 0.5, manager);
}

  
void AgentTest::tearDown() {
  delete manager;
}
      
void AgentTest::testConstructorDestructor() {
  Agent *agent = new TestAgent("local-test-agent", 0.9, manager);

  CPPUNIT_ASSERT_MESSAGE("Agent::Agent() failed to correctly set name", 
		  agent->name == "local-test-agent");
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent() failed to correctly set m_activation", 
			 fabs(agent->m_activation - 0.9) < 0.000001);
  CPPUNIT_ASSERT_MESSAGE("Agent::Agent() failed to correctly set m_manager", 
		  agent->m_manager == manager);
}


void AgentTest::testStillActive() {
  double act_1 = agent_1->m_activation;
  double act_2 = agent_2->m_activation;

  agent_1->updateActivation();
  agent_2->updateActivation();

  CPPUNIT_ASSERT_MESSAGE("TestAgent::updateActivation() failed to change agent_1->m_activation",
			 agent_1->m_activation != act_1);
  CPPUNIT_ASSERT_MESSAGE("TestAgent::updateActivation() failed to change agent_2->m_activation",
			 agent_2->m_activation != act_2);
}
