// ============================================================================
// Filename          : $RCSfile: Manager.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 30-Oct-2008
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 05:54:51 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines the interface of the agents'
//                     execution Manager
//
// Revision history  :
// ============================================================================
#ifndef libAMBR__Manager_hpp
#define libAMBR__Manager_hpp

#include "VariableRateExecution/Manager.hpp"
#include "Agent.hpp"


namespace AMBR {
  
  // Forward declaration
  class Agent;
 
 
  class Manager : public VariableRateExecution::Manager {
  public:
    //  =======================================================================
    /// The constructor
    ///
    /// This constructor creates a manager that oversees the execution and 
    /// activation of its agents
    ///
    /// @param priorityLevels the number of priority queues, or discrete rates
    ///            of execution to maintain
    //  =======================================================================
    Manager(unsigned priorityLevels);
    
    
    //  =======================================================================
    /// Requests the creation of a HypothesisAgent
    /// 
    /// This method allows a ConceptAgent to request the creation of a Hypothe-
    /// sisAgent
    ///
    /// @param agent the agent to add
    //  =======================================================================
    void requestHypothesisAgentCreation(Agent *agent) { };
    
    
    //  =======================================================================
    /// Print the managed agents
    /// 
    /// This method goes through all of the priorities queues and invokes the
    /// print(...) method of each of the managed agents.
    //  =======================================================================
    void printAgents();

    //friend class VariableRateExecution::ManagerConsole;    
  };
  
}

#endif

