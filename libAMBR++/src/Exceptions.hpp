// ============================================================================
// Filename          : $RCSfile: Exceptions.hpp,v $
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
#ifndef libAMBR__Exceptions_hpp
#define libAMBR__Exceptions_hpp

#include <string>


namespace AMBR {
  
  namespace Exception {

    //  =========================================================================
    /// @class    BaseException
    /// @brief    The base class for all of AMBR's exceptions.
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class BaseException {
    public:
      BaseException(std::string comment) { this->comment = comment; }
      
      std::string comment;
    };
    
    
    class AgentException : public BaseException {
    public:
      AgentException(std::string agentName, std::string comment) : BaseException(comment) { 
        this->agentName = agentName;
      }
      
      std::string agentName;
    };
    
    
    class InvalidAgent : public AgentException {
    public:
      InvalidAgent(std::string agentName, std::string comment) : AgentException(agentName, comment) { };
    };
    
    
    class AgentNotFound : public AgentException {
    public:
      AgentNotFound(std::string agentName, std::string comment) : AgentException(agentName, comment) { };
    };
    
    
    class SlotException : public BaseException {
    public:
      SlotException(std::string slotName, std::string comment) : BaseException(comment) {
        this->slotName = slotName;
      }
      
      std::string slotName;
    };
    
    
    class InvalidSlot : public SlotException {
    public:
      InvalidSlot(std::string slotName, std::string comment) : SlotException(slotName, comment) { };
    };
    
    
    class SlotNotFound : public SlotException {
    public:
      SlotNotFound(std::string slotName, std::string comment) : SlotException(slotName, comment) { };
    };
    
    
  }  // namespace Exception
  
  
} // namespace AMBR

#endif
