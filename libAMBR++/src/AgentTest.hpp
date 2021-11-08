// ============================================================================
// Filename          : $RCSfile: AgentTest.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 21-Jul-2008
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 05:54:51 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines the interface of the AMBR::
//                     DUAL micro-agent.
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libAMBR__AgentTest_hpp
#define libAMBR__AgentTest_hpp

#include <math.h>
#include <string>
#include "Agent.hpp"
#include "ConceptAgent.hpp"
#include "InstanceAgent.hpp"
#include "cppunit/extensions/HelperMacros.h"


using namespace std;
using namespace AMBR::Test;


namespace AMBR {
  namespace Test {
    
    class AgentTest : public CPPUNIT_NS::TestFixture {
      CPPUNIT_TEST_SUITE(AgentTest);
      CPPUNIT_TEST(testConstructorDestructor);
      CPPUNIT_TEST(testAddCoreference);
      CPPUNIT_TEST(testAddAssociation);
      CPPUNIT_TEST(testAddGetSlot);
      CPPUNIT_TEST(testInitialise);
      CPPUNIT_TEST(testPrintParse);
      CPPUNIT_TEST(testUpdateActivation);
      CPPUNIT_TEST_SUITE_END();
      
    public:
      void setUp();
      void tearDown();
      
      void testConstructorDestructor();
      void testAddCoreference();
      void testAddAssociation();
      void testAddGetSlot();
      void testInitialise();
      void testPrintParse();
      void testUpdateActivation();
      
      
    protected:
      Agent *agent_1, *agent_2;
      Slot *slot_1, *slot_2, *slot_3;
    };
    
    
    

    class ConceptAgentTest : public CPPUNIT_NS::TestFixture {
      CPPUNIT_TEST_SUITE(ConceptAgentTest);
      CPPUNIT_TEST(testAddSuperSubClasses);
      CPPUNIT_TEST(testAddInstance);
      CPPUNIT_TEST(testInitialise);
      CPPUNIT_TEST(testPrintParse);
      CPPUNIT_TEST(testTick);
      CPPUNIT_TEST_SUITE_END();
      
    public:
      void setUp();
      void tearDown();
      
      void testAddSuperSubClasses();
      void testAddInstance();
      void testInitialise();
      void testPrintParse();
      void testTick();
      
      
    protected:
      ConceptAgent *agent_1, *agent_2, *agent_3;
      Slot *slot_1, *slot_2, *slot_3;
    };
    
    
    
    
    class InstanceAgentTest : public CPPUNIT_NS::TestFixture {
      CPPUNIT_TEST_SUITE(InstanceAgentTest);
      CPPUNIT_TEST(testAddInstanceOf);
      CPPUNIT_TEST(testInitialise);
      CPPUNIT_TEST(testPrintParse);
      CPPUNIT_TEST(testTick);
      CPPUNIT_TEST_SUITE_END();
      
    public:
      void setUp();
      void tearDown();
      
      void testAddSuperSubClasses();
      void testAddInstanceOf();
      void testInitialise();
      void testPrintParse();
      void testTick() { };
      
      
    protected:
      InstanceAgent *agent_1, *agent_2, *agent_3;
      Slot *slot_1, *slot_2, *slot_3;
    };
  }
}

#endif
