// ============================================================================
// Filename          : $RCSfile: FGREPNetworkTest.hpp,v $
// Version           : $Revision: 1.2 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 21-Jul-2008
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 11:22:59 $
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
#ifndef libFGREP__FGREPNetworkTest_hpp
#define libFGREP__FGREPNetworkTest_hpp

#include "FGREPNetwork.hpp"
#include "cppunit/extensions/HelperMacros.h"



namespace ANN {
  
  namespace Test {
    
    class FGREPNetworkTest : public CPPUNIT_NS::TestFixture {
      CPPUNIT_TEST_SUITE(FGREPNetworkTest);
      CPPUNIT_TEST(testConstructorDestructor);
      CPPUNIT_TEST(testDeclareTerm);
      CPPUNIT_TEST(testTrainNetworkByNumberOfIterationsIncremental);
      CPPUNIT_TEST(testTrainNetworkByAverageErrorSquaredIncremental);
      //CPPUNIT_TEST(testTrainNetworkByNumberOfIterationsBatch);
      //CPPUNIT_TEST(testTrainNetworkByAverageErrorSquaredBatch);
      CPPUNIT_TEST(testPrintNetwork);
      CPPUNIT_TEST(testSaveLoadNetwork);
      CPPUNIT_TEST_SUITE_END();
      
    public:
      void setUp();
      void tearDown();
      
      void testConstructorDestructor();
      void testDeclareTerm();
      void testTrainNetworkByNumberOfIterationsIncremental();
      void testTrainNetworkByAverageErrorSquaredIncremental();
      //void testTrainNetworkByNumberOfIterationsBatch();
      //void testTrainNetworkByAverageErrorSquaredBatch();
      void testPrintNetwork();
      void testSaveLoadNetwork();
      
      
    protected:
      FGREPNetwork *fgrep;
    };
  }
}

#endif

