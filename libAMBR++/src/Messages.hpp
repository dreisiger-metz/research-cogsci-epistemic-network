// ============================================================================
// Filename          : $RCSfile: Messages.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 210-Nov-2008
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 05:54:51 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines the AMBR-specific messages
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libAMBR__Messages_hpp
#define libAMBR__Messages_hpp

#include "VariableRateExecution/Message.hpp"


namespace AMBR {
  
  /// Forward declarations
  class Agent;
  class HypothesisAgent;
  class AgentReference;
  
  
  namespace Message {
    
    /// The enumeration used to identify a message's type
    enum MessageType { 
      ActivationMessage,
      MarkerMessage,
      HypothesisRequestMessage,
      HypothesisResponseMessage
    };
    
    
    //  =========================================================================
    /// @class    BaseMessage
    /// @brief    The base class for all DUAL and AMBR messages
    ///
    /// @note by extending VariableRateExecution::Message::BaseMessage, this
    ///       AMBR message base-class will be able to take advantage of the
    ///       VariableRateExecution's manager's message delivery queue
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class BaseMessage : public VariableRateExecution::Message::BaseMessage {
    public:
      BaseMessage(MessageType type) : VariableRateExecution::Message::BaseMessage((unsigned) type) { };
    };
    
    
    //  =========================================================================
    /// @class    Activation
    /// @brief    The message class used to transmit activation
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class Activation : public BaseMessage {
    public:
      Activation(double activation) : BaseMessage(Message::ActivationMessage) { this->activation = activation; }
      
      double activation;
    };
    
    
    //  =========================================================================
    /// @class    Marker
    /// @brief    The message class used during marker passing
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class Marker : public BaseMessage {
    public:
      Marker(Marker *original) : BaseMessage(Message::MarkerMessage) {
        origin = original->origin;
        path = original->path;
        colour = original->colour;
        serial = original->serial;
      }
      Marker(Agent *that, Marker *original) : BaseMessage(Message::MarkerMessage) {
        origin = original->origin;
        path = original->path;
        path.push_back(that);
        colour = original->colour;
        serial = original->serial;
      }
      Marker(Agent *origin, unsigned serial) : BaseMessage(Message::MarkerMessage) {
        this->origin = origin;
        path.push_back(origin);
        colour = 0;
        this->serial = serial;
      }
      
      Agent               *origin;
      std::vector<Agent*>  path;
      unsigned             colour;
      unsigned             serial;
    };
    
    
    //  =========================================================================
    /// @class    HypothesisRequest
    /// @brief    The class used by hypothesis agents to announce their creation
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class HypothesisRequest : public BaseMessage {
    public:
      HypothesisRequest(HypothesisAgent *hypothesis, Agent *first, Agent *second) : BaseMessage(Message::HypothesisRequestMessage) {
        this->hypothesis = hypothesis;
        this->first = first;
        this->second = second;
      }
      
      HypothesisAgent *hypothesis;
      Agent           *first;
      Agent           *second;
    };
    
    
    //  =========================================================================
    /// @class    HypothesisResponse
    /// @brief    The class used to respond to hypothesis requests
    ///
    /// @author   Peter Dreisiger
    /// @version  0.1
    //  =========================================================================
    class HypothesisResponse : public BaseMessage {
    public:
      enum ResponseType {
        Resign,
        Establish
      };
      
      HypothesisResponse(ResponseType response, std::vector<AgentReference*> references) : BaseMessage(Message::HypothesisResponseMessage) {
        this->response = response;
        this->references = references;
      }
      
      ResponseType                 response;
      std::vector<AgentReference*> references;
    };
    
  } // namespace Message
  
} // namespace AMBR

#endif
