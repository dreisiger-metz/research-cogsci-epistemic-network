#include "LoggerTest.hpp"

const Logger::Category TestLogger::FIRST   = 0;
const Logger::Category TestLogger::SECOND  = 1;
const Logger::Category TestLogger::THIRD   = 2;
const Logger::Category TestLogger::FOURTH  = 3;
const Logger::Category TestLogger::FIFTH   = 4;
const Logger::Category TestLogger::SIXTH   = 5;
const Logger::Category TestLogger::SEVENTH = 6;
const Logger::Category TestLogger::EIGHTH  = 7;


char *LoggerTest::testCategories[] = {"FIRST_CATEGORY", "SECOND_CATEGORY", "THIRD_CATEGORY",
				      "FOURTH_CATEGORY", "FIFTH_CATEGORY", "SIXTH_CATEGORY",
				      "SEVENTH_CATEGORY", "EIGHT_CATEGORY"};


void LoggerTest::testConstructor() {
#ifdef __DEBUG__INTERACTIVE_TESTS
  char answer[32];

  // Test the full constructor
  answer[0] = 'y';
  printf("About to create a TestLogger with a default OFP of stderr --- the following informative message should go there\n");
  logger = new TestLogger("Full-Logger-Test", (const char **)testCategories, 0xffffffff, Logger::VERBOSE, stderr);
  logger->log(__FILE__, __LINE__, Logger::INFORMATIVE, TestLogger::FIRST, "Created an instance of TestLogger using the full constructor and a default OFP of stderr");
  delete logger;
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("TestLogger::log failed to send the correct message to stderr", answer[0] == 'y');


  answer[0] = 'y';
  printf("About to create a TestLogger without specifying the default OFP --- the following informative message should go to stdout\n");
  logger = new TestLogger("Logger-Test", (const char **)testCategories, 0xffffffff, Logger::VERBOSE);
  logger->log(__FILE__, __LINE__, Logger::INFORMATIVE, TestLogger::FIRST, "Created an instance of TestLogger without specifying the default OFP");
  delete logger;
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("TestLogger::log failed to send the correct message to stdout", answer[0] == 'y');


  answer[0] = 'y';
  printf("About to create a TestLogger without specifying the default OFP or severity limit --- the following informative message should /not/ be printed\n");
  logger = new TestLogger("Logger-Test", (const char **)testCategories, 0xffffffff);
  logger->log(__FILE__, __LINE__, Logger::INFORMATIVE, TestLogger::FIRST, "Created an instance of TestLogger without specifying the default OFP or severity limit");
  delete logger;
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("TestLogger::log incorrectly printed an informative message given the default severity limit", answer[0] == 'y');


  answer[0] = 'y';
  printf("About to create a TestLogger without specifying the default OFP or severity limit or category mask --- the following error message should be printed\n");
  logger = new TestLogger("Logger-Test", (const char **)testCategories);
  logger->log(__FILE__, __LINE__, Logger::ERROR, TestLogger::FIRST, "Created an instance of TestLogger without specifying the default OFP, severity limit, or category mask");
  delete logger;
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("TestLogger::log failed to print an error message given the default category mask", answer[0] == 'y');


  answer[0] = 'y';
  printf("About to create a TestLogger without specifying the default OFP or severity limit --- the following error message should /not/ be printed\n");
  logger = new TestLogger("Logger-Test", (const char **)testCategories, 254);
  logger->log(__FILE__, __LINE__, Logger::ERROR, TestLogger::FIRST, "Created an instance of TestLogger without specifying the default OFP or severity limit, and with a category mask of !TestLogger::FIRST");
  delete logger;
  printf("\nDoes this look right?  (y/n) ");
  scanf("%s", answer);
  CPPUNIT_ASSERT_MESSAGE("TestLogger::log incorrectly printed an error message despite its category mask", answer[0] == 'y');
#endif
}




void LoggerTest::testLog() {
}




void LoggerTest::generateLogs() {
  logger->log(__FILE__, __LINE__, Logger::FATAL,       TestLogger::FIRST,  "A fatal error of category %d", TestLogger::FIRST);
  logger->log(__FILE__, __LINE__, Logger::ERROR,       TestLogger::SECOND, "An error of category %d", TestLogger::SECOND);
  logger->log(__FILE__, __LINE__, Logger::WARNING,     TestLogger::THIRD,  "A warning of category %d", TestLogger::THIRD);
  logger->log(__FILE__, __LINE__, Logger::INFORMATIVE, TestLogger::FOURTH, "An informative message of category %d", TestLogger::FOURTH);
  logger->log(__FILE__, __LINE__, Logger::VERBOSE,     TestLogger::FIFTH,  "A verbose message of category %d", TestLogger::FIFTH);
  logger->log(__FILE__, __LINE__, Logger::LUDICROUS,   TestLogger::SIXTH,  "A ludicrously-verbose message of category %d", TestLogger::SIXTH);
}
