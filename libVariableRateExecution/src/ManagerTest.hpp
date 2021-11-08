// ============================================================================
// Filename          : $RCSfile: ManagerTest.hpp,v $
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
// Purpose           : This file defines the Manager class' test suite
//
// Revision history  : 
// ============================================================================
#ifndef libVariableRateExecution__ManagerTest_hpp
#define libVariableRateExecution__ManagerTest_hpp

#include <math.h>
#include <unistd.h>

#include "cppunit/extensions/HelperMacros.h"

#include "Agent.hpp"
#include "Manager.hpp"
#include "TestAgent.hpp"


using namespace std;


namespace VariableRateExecution {

  namespace Test {
    
    class ManagerTest : public CPPUNIT_NS::TestFixture {
      CPPUNIT_TEST_SUITE(ManagerTest);
      CPPUNIT_TEST(testConstructorDestructor);
      CPPUNIT_TEST(testAddRemoveAgent);
      CPPUNIT_TEST(testStartStop);
      CPPUNIT_TEST_SUITE_END();
      
    public:
      void setUp();
      void tearDown();
      
      void testConstructorDestructor();
      void testAddRemoveAgent();
      void testStartStop();

      Manager            *manager;
      vector<TestAgent*>  agents;
    };
  }
}

#endif

