// ============================================================================
// Filename          : $RCSfile: AMBR.hpp,v $
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
// Purpose           : This is the general, AMBR-wide header file.  It includes
//                     all of the essential header files;  it also declares all
//                     of the system-wide, or global, variables.
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libAMBR__AMBR_hpp
#define libAMBR__AMBR_hpp

#include <stdio.h>
#include <string.h>

#include <string>
#include <vector>
#include <map>


#include "Messages.hpp"
#include "Exceptions.hpp"
#include "AMBRLogger.hpp"
#include "LongTermMemory.hpp"


namespace AMBR {
  
  /// Forward declarations
  class Agent;
  //class HypothesisAgent;
  class AgentReference;
  class LongTermMemory;
  
  
  /// The AMBR-wide event logger
  extern AMBRLogger *logger;
  
  // The AMBR-wide VariableRateExecution manager
  extern VariableRateExecution::Manager *Manager;

  extern LongTermMemory *Memory;

  /// The AMBR-wide maximum line length (used when parsing agents and slots)
  extern unsigned    MaxLineLength;
  
  
} // namespace AMBR

#endif
