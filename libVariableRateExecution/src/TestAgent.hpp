// ============================================================================
// Filename          : $RCSfile: TestAgent.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 30-Oct-2008
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 05:05:54 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines the derived Agent class that
//                     is used by the Agent and Manager test suites
//
// Revision history  : 
// ============================================================================
#ifndef libVariableRateExecution__TestAgent_hpp
#define libVariableRateExecution__TestAgent_hpp

#include "Agent.hpp"


namespace VariableRateExecution {

  namespace Test {
    class AgentTest;
  }


  class TestAgent : public Agent {
  public:
    TestAgent(std::string name, double activation, Manager *manager) : Agent(name, activation, manager) {
      printf("TestAgent::TestAgent(...) called for %s at time %d\n", name.c_str(), m_manager->time());
    }
    ~TestAgent() {
      printf("TestAgent::~TestAgent() called for %s at time %d\n", name.c_str(), m_manager->time());
    }

  protected:
    bool tick() { return true; }
    void updateActivation() { m_activation *= 0.99999; }

    bool activate() {
      printf("  TestAgent::activate() called on %s at time %d\n", name.c_str(), m_manager->time());
      return true;
    }
    bool deactivate() {
      printf("  TestAgent::deactivate() called on %s at time %d\n", name.c_str(), m_manager->time());
      m_manager->registerCallback(this, 50, callback);
      return true;
    }
    bool receive(Message::BaseMessage *message) {
      printf("  TestAgent::receive(...) received a message at time %d\n", time());
      return true;
    }

    static void callback(Agent *that) { ((TestAgent *) that)->callbackHook(); }
    void callbackHook() {
      printf("   %s final activation was %g after %d ticks\n", name.c_str(), m_activation, m_manager->time());
      delete this;
    }

    friend class VariableRateExecution::Test::AgentTest;
  };
}

#endif

