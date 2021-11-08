// ============================================================================
// Filename          : $RCSfile: AgentTest.hpp,v $
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
// Purpose           : This is the header of the Agent test suite
//
// Revision history  : 
// ============================================================================
#ifndef libVariableRateExecution__AgentTest_hpp
#define libVariableRateExecution__AgentTest_hpp

#include "math.h"
#include "cppunit/extensions/HelperMacros.h"

#include "Agent.hpp"
#include "TestAgent.hpp"
#include "Manager.hpp"


using namespace std;


namespace VariableRateExecution {

  namespace Test {
    
    class AgentTest : public CPPUNIT_NS::TestFixture {
      CPPUNIT_TEST_SUITE(AgentTest);
      CPPUNIT_TEST(testConstructorDestructor);
      CPPUNIT_TEST(testStillActive);
      CPPUNIT_TEST_SUITE_END();
      
    public:
      void setUp();
      void tearDown();
      
      void testConstructorDestructor();
      void testStillActive();

      Manager *manager;      
      TestAgent *agent_1, *agent_2;
    };
  }
}

#endif

