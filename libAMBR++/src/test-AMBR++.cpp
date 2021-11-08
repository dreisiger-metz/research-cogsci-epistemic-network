#include "cppunit/ui/text/TestRunner.h"
#include "SlotTest.hpp"
#include "AgentTest.hpp"
#include "AMBRTest.hpp"
#include "LoggerTest.hpp"


int main(int argc, char **argv) {
  CppUnit::TextUi::TestRunner runner;

  runner.addTest(SlotTest::suite());
  runner.addTest(AgentTest::suite());
  runner.addTest(ConceptAgentTest::suite());
  runner.addTest(InstanceAgentTest::suite());
  runner.addTest(AMBRTest::suite());
  runner.addTest(LoggerTest::suite());

  runner.run();

  return 0;
}
