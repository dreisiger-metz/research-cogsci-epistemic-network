#include "LongTermMemory.hpp"

using namespace std;
using namespace AMBR;


void LongTermMemory::addToWorkingMemory(Agent *agent) throw (Exception::InvalidAgent) {
  if (m_workingMemory.find(agent) == m_workingMemory.end()) {
    m_workingMemory[agent] = 1;
    AMBR::logger->log(__FILE__, __LINE__, Logger::INFORMATIVE, AMBRLogger::WORKING_MEMORY,
                      "Agent '%s' entered working memory at time %d", agent->name.c_str(), agent->time());

  } else {
    Exception::InvalidAgent e(agent->name, "Agent already in Working Memory");
    throw e;
  }
}


void LongTermMemory::removeFromWorkingMemory(Agent *agent) throw (Exception::InvalidAgent) {
  map<Agent*, unsigned>::iterator i;
  
  if ((i = m_workingMemory.find(agent)) != m_workingMemory.end()) {
    m_workingMemory.erase(i);
    AMBR::logger->log(__FILE__, __LINE__, Logger::INFORMATIVE, AMBRLogger::WORKING_MEMORY,
                    "Agent '%s' left working memory at time %d", agent->name.c_str(), agent->time());
  } else {
    Exception::InvalidAgent e(agent->name, "Agent not in Working Memory");
    throw e;
  }
}
