#include "cppunit/ui/text/TestRunner.h"
#include "AgentTest.hpp"
#include "ManagerTest.hpp"


int main(int argc, char **argv) {
  CppUnit::TextUi::TestRunner runner;

  runner.addTest(VariableRateExecution::Test::AgentTest::suite());
  runner.addTest(VariableRateExecution::Test::ManagerTest::suite());

  runner.run();

  return 0;
}
