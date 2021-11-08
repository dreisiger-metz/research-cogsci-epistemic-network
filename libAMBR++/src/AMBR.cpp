#include "AMBR.hpp"

using namespace AMBR;


AMBRLogger *AMBR::logger = new AMBRLogger("libAMBR++", 0xffffffff, Logger::LUDICROUS, stdout);

VariableRateExecution::Manager *AMBR::Manager = new VariableRateExecution::Manager(10, Agent::ActivationQuantum, Agent::ActivationMinimum, Agent::ActivationMaximum, Agent::ActivationThreshold);

LongTermMemory *AMBR::Memory = new LongTermMemory(AMBR::Manager);

unsigned    AMBR::MaxLineLength = 4096;

//WorkingMemoryManager *AMBR::WorkingMemory = new WorkingMemoryManager();
