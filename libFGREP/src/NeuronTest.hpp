// ============================================================================
// Filename          : $RCSfile: NeuronTest.hpp,v $
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
#ifndef libFGREP__NeuronTest_hpp
#define libFGREP__NeuronTest_hpp

#include "Neuron.hpp"
#include "cppunit/extensions/HelperMacros.h"



namespace ANN {
  
  namespace Test {
    
    class NeuronTest : public CPPUNIT_NS::TestFixture {
      CPPUNIT_TEST_SUITE(NeuronTest);
      CPPUNIT_TEST(testConstructorDestructor);
      CPPUNIT_TEST(testSaveLoad);
      CPPUNIT_TEST_SUITE_END();
      
    public:
      void setUp() { neuron = new Neuron("neuron"); };
      void tearDown() { delete neuron; };
      
      void testConstructorDestructor();
      void testSaveLoad();
      
      
    protected:
      Neuron *neuron;
    };
  }
}

#endif
