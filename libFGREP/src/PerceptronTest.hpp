// ============================================================================
// Filename          : $RCSfile: PerceptronTest.hpp,v $
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
#ifndef libFGREP__PerceptronTest_hpp
#define libFGREP__PerceptronTest_hpp

#include "Perceptron.hpp"
#include "cppunit/extensions/HelperMacros.h"



namespace ANN {
  
  namespace Test {
    
    class PerceptronTest : public CPPUNIT_NS::TestFixture {
      CPPUNIT_TEST_SUITE(PerceptronTest);
      CPPUNIT_TEST(testConstructorDestructor);
      CPPUNIT_TEST(testAddInput);
      CPPUNIT_TEST(testUpdateActivation);
      CPPUNIT_TEST(testUpdateWeights);
      CPPUNIT_TEST(testPrint);
      CPPUNIT_TEST(testSaveLoad);
      CPPUNIT_TEST_SUITE_END();
      
    public:
      void setUp();
      void tearDown();
      
      void testConstructorDestructor();
      void testAddInput();
      void testUpdateActivation();
      void testUpdateWeights();
      void testPrint();
      void testSaveLoad();
      
      
    protected:
      double learningRate, momentum;
      Sigmoid *sigmoid;
      Perceptron *p_11, *p_12, *p_21, *p_31, *p_32;
    };
  }
}

#endif
