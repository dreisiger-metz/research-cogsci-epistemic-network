// ============================================================================
// Filename          : $RCSfile: Message.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 30-Oct-2008
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 05:05:54 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This file defines the message base class and envelope.
//
// Revision history  : 
// ============================================================================
#ifndef libVariableRateExecution__Message_hpp
#define libVariableRateExecution__Message_hpp

#include "Agent.hpp"


namespace VariableRateExecution {
  
  /// Forward declarations
  class Agent;
  
  
  namespace Message {
    
    //  =========================================================================
    /// @class    BaseMessage
    /// @brief    The base class for all agent messages
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class BaseMessage {
    public:
      BaseMessage(unsigned type) { this->type = type; }
      
      unsigned type;
    };
    
    
    //  =========================================================================
    /// @class    Envelope
    /// @brief    The container used by the outgoing message queue
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class Envelope {
    public:
      Envelope(Agent *destination, BaseMessage *message) {
        this->destination = destination;
        this->message = message;
      }
      ~Envelope() { delete message; }
      
      Agent       *destination;
      BaseMessage *message;
    };
    
  }
  
  
} // namespace VariableRateExecution

#endif

