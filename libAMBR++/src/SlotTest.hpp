// ============================================================================
// Filename          : $RCSfile: SlotTest.hpp,v $
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
#ifndef libAMBR__SlotTest_hpp
#define libAMBR__SlotTest_hpp

#include <string>
#include "Slot.hpp"
#include "Agent.hpp"
#include "cppunit/extensions/HelperMacros.h"


using namespace std;
using namespace AMBR::Test;


namespace AMBR {
  namespace Test {
    
    class SlotTest : public CPPUNIT_NS::TestFixture {
      CPPUNIT_TEST_SUITE(SlotTest);
      CPPUNIT_TEST(testConstructor);
      CPPUNIT_TEST(testAddSuperClass);
      CPPUNIT_TEST(testAddInstanceOf);
      CPPUNIT_TEST(testAddCoreference);
      CPPUNIT_TEST(testAddAssociation);
      CPPUNIT_TEST(testResolveReferences);
      CPPUNIT_TEST(testPrintParse);
      CPPUNIT_TEST_SUITE_END();
      
    public:
      void setUp();
      void tearDown();
      
      void testConstructor();
      void testAddSuperClass();
      void testAddInstanceOf();
      void testAddCoreference();
      void testAddAssociation();
      void testResolveReferences();
      void testPrintParse();
      
      
      Slot *slot, *slot_1, *slot_2;
      Agent *agent_1, *agent_2;
    };
  }
}

#endif
