// ============================================================================
// Filename          : $RCSfile: DistributedRepresentationTest.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 21-Jul-2008
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 06:06:34 $
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
#ifndef libFGREP__DistributedRepresentationTest_hpp
#define libFGREP__DistributedRepresentationTest_hpp

#include <stdio.h>

#include "DistributedRepresentation.hpp"
#include "cppunit/extensions/HelperMacros.h"



namespace ANN {
  
  namespace Test {
    
    class DistributedRepresentationTest : public CPPUNIT_NS::TestFixture {
      CPPUNIT_TEST_SUITE(DistributedRepresentationTest);
      CPPUNIT_TEST(testConstructorDestructor);
      CPPUNIT_TEST(testLoadSaveRepresentation);
      CPPUNIT_TEST(testSetErrorSignals);
      CPPUNIT_TEST(testDistributedSentenceRepresentation);
      CPPUNIT_TEST_SUITE_END();
      
    public:
      void setUp();
      void tearDown();
      
      void testConstructorDestructor();
      void testLoadSaveRepresentation();
      void testSetErrorSignals();
      void testDistributedSentenceRepresentation();
      
      
    protected:
      double m_learningRate;
      double m_momentum;

      std::vector<Perceptron*> m_layer;
      Sigmoid                  m_sigmoid;

      static const unsigned    LayerSize;
    };
  }
}

#endif

