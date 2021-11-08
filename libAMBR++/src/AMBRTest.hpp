// ============================================================================
// Filename          : $RCSfile: AMBRTest.hpp,v $
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
#ifndef libAMBR__AMBRTest_hpp
#define libAMBR__AMBRTest_hpp

#include <stdlib.h>

#include "ConceptAgent.hpp"
#include "HypothesisAgent.hpp"
#include "InstanceAgent.hpp"
#include "cppunit/extensions/HelperMacros.h"


using namespace std;
using namespace AMBR::Test;


namespace AMBR {
  namespace Test {
    
    class AMBRTest : public CPPUNIT_NS::TestFixture {
      CPPUNIT_TEST_SUITE(AMBRTest);
      CPPUNIT_TEST(testSystem);
      CPPUNIT_TEST_SUITE_END();
      
    public:
      void setUp();
      void tearDown();
      
      void testSystem();
      
      
    protected:
      std::vector<ConceptAgent*>  conceptAgents;
      std::vector<InstanceAgent*> instanceAgents;
    };
  }
}

#endif
