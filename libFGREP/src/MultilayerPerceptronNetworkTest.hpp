// ============================================================================
// Filename          : $RCSfile: MultilayerPerceptronNetworkTest.hpp,v $
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
#ifndef libFGREP__MultilayerPerceptronNetworkTest_hpp
#define libFGREP__MultilayerPerceptronNetworkTest_hpp

#include "MultilayerPerceptronNetwork.hpp"
#include "cppunit/extensions/HelperMacros.h"



namespace ANN {
  
  namespace Test {
    
    class MultilayerPerceptronNetworkTest : public CPPUNIT_NS::TestFixture {
      CPPUNIT_TEST_SUITE(MultilayerPerceptronNetworkTest);
      CPPUNIT_TEST(testConstructorDestructor);
      CPPUNIT_TEST(testAddTrainingData);
      CPPUNIT_TEST(testTrainNetworkIterative);
      CPPUNIT_TEST(testTrainNetworkErrorThreshold);
      CPPUNIT_TEST(testPrintNetwork);
      CPPUNIT_TEST(testSaveLoadNetwork);
      CPPUNIT_TEST_SUITE_END();
      
    public:
      void setUp();
      void tearDown();
      
      void testConstructorDestructor();
      void testAddTrainingData();
      void testTrainNetworkIterative();
      void testTrainNetworkErrorThreshold();
      void testPrintNetwork();
      void testSaveLoadNetwork();
      
      
    protected:
      MultilayerPerceptronNetwork *mlp;
    };
  }
}

#endif

