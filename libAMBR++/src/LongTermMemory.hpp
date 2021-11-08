// ============================================================================
// Filename          : $RCSfile: LongTermMemory.hpp,v $
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
// Purpose           : This header file defines the interface of the DUAL and
//                     AMBR micro-agent.
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libAMBR__LongTermMemory_hpp
#define libAMBR__LongTermMemory_hpp

#include <map>

#include "VariableRateExecution/Manager.hpp"
#include "AMBR.hpp"
#include "Agent.hpp"


namespace AMBR {

  /// Forward declarations
  class Agent;


  //  =========================================================================
  /// @class    LongTermMemory
  /// @brief    The base class for all DUAL and AMBR micro-agents.
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  =========================================================================
  class LongTermMemory {
  public:
    LongTermMemory(VariableRateExecution::Manager *manager) { m_manager = manager; }
    virtual ~LongTermMemory() { };

    void addToWorkingMemory(Agent *agent) throw (Exception::InvalidAgent);
    void removeFromWorkingMemory(Agent *agent) throw (Exception::InvalidAgent);

    bool inWorkingMemory(Agent *agent) { return (m_workingMemory.find(agent) != m_workingMemory.end()); }

    unsigned createHypothesisAgent() { return 0; }


  protected:
    VariableRateExecution::Manager::Manager *m_manager;
    std::map<Agent*, unsigned>               m_workingMemory;
  };

}

#endif
