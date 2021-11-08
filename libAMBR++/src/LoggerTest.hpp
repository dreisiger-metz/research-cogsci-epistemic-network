// ============================================================================
// Filename          : $RCSfile: LoggerTest.hpp,v $
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
#ifndef LoggerTest_hpp
#define LoggerTest_hpp

#include "Logger.hpp"
#include "cppunit/extensions/HelperMacros.h"




class TestLogger : public Logger {
public:
  TestLogger(const char *programName, const char *categoryName[], 
	     Category categoryMask = 0xffffffff, Severity severityLimit = Logger::WARNING,
	     const FILE *ofp = stdout) : Logger(programName, categoryName, categoryMask, severityLimit, ofp) { };


  static const Logger::Category FIRST;
  static const Logger::Category SECOND;
  static const Logger::Category THIRD;
  static const Logger::Category FOURTH;
  static const Logger::Category FIFTH;
  static const Logger::Category SIXTH;
  static const Logger::Category SEVENTH;
  static const Logger::Category EIGHTH;

  friend class LoggerTest;
};




class LoggerTest : public CPPUNIT_NS::TestFixture {
  CPPUNIT_TEST_SUITE(LoggerTest);
  CPPUNIT_TEST(testConstructor);
  CPPUNIT_TEST(testLog);
  CPPUNIT_TEST_SUITE_END();

public:
  void setUp() { };
  void tearDown() { };

  void testConstructor();
  void testLog();

  void generateLogs();


  TestLogger *logger;

  static char *testCategories[];
};

#endif


/*
/usr/lib/gcc/i686-pc-linux-gnu/3.4.5/include/g++-v3/bits/locale_facets.tcc:2444: multiple definition of `TestLogger::FIRST'
LoggerTest.do:/usr/lib/gcc/i686-pc-linux-gnu/3.4.5/include/g++-v3/bits/locale_facets.tcc:2444: first defined here
TestRunner.do: In function `~LoggerTest':
/Network/Servers/smcs-wa.dsto.defence.gov.au/Volumes/Home/dreisigp/Work/Research/Projects/PhD/Code/libAMBR++/src/AgentTest.hpp:111: multiple definition of `TestLogger::SECOND'
LoggerTest.do:/usr/lib/gcc/i686-pc-linux-gnu/3.4.5/include/g++-v3/bits/stl_deque.h:130: first defined here
TestRunner.do: In function `~LoggerTest':
/usr/local/include/cppunit/TestCaller.h:158: multiple definition of `TestLogger::THIRD'
LoggerTest.do:/usr/lib/gcc/i686-pc-linux-gnu/3.4.5/include/g++-v3/bits/stl_algobase.h:155: first defined here
TestRunner.do: In function `~LoggerTest':
/usr/lib/gcc/i686-pc-linux-gnu/3.4.5/include/g++-v3/memory:301: multiple definition of `TestLogger::FOURTH'
LoggerTest.do:/usr/lib/gcc/i686-pc-linux-gnu/3.4.5/include/g++-v3/bits/stl_deque.h:134: first defined here
TestRunner.do: In function `~LoggerTest':
/usr/lib/gcc/i686-pc-linux-gnu/3.4.5/include/g++-v3/bits/stl_iterator.h:603: multiple definition of `TestLogger::FIFTH'
LoggerTest.do:/usr/lib/gcc/i686-pc-linux-gnu/3.4.5/include/g++-v3/bits/stl_deque.h:134: first defined here
TestRunner.do: In function `~LoggerTest':
/Network/Servers/smcs-wa.dsto.defence.gov.au/Volumes/Home/dreisigp/Work/Research/Projects/PhD/Code/libAMBR++/src/LoggerTest.hpp:66: multiple definition of `TestLogger::SIXTH'
LoggerTest.do:/usr/lib/gcc/i686-pc-linux-gnu/3.4.5/include/g++-v3/bits/stl_deque.h:230: first defined here
TestRunner.do: In function `~LoggerTest':
/usr/lib/gcc/i686-pc-linux-gnu/3.4.5/include/g++-v3/memory:319: multiple definition of `TestLogger::SEVENTH'
LoggerTest.do:/usr/lib/gcc/i686-pc-linux-gnu/3.4.5/include/g++-v3/bits/stl_deque.h:144: first defined here
TestRunner.do: In function `~LoggerTest':
/usr/local/include/cppunit/TestCaller.h:158: multiple definition of `TestLogger::EIGHTH'
LoggerTest.do:/usr/lib/gcc/i686-pc-linux-gnu/3.4.5/include/g++-v3/bits/stl_deque.h:425: first defined here

*/
