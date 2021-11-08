// ============================================================================
// Filename          : $RCSfile: Reference.hpp,v $
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
#ifndef libAMBR__Reference_hpp
#define libAMBR__Reference_hpp

#include "AMBR.hpp"
#include "Agent.hpp"
#include "Slot.hpp"


namespace AMBR {
  

  // Forward declarations
  class Agent;
  class Slot;


  //  =========================================================================
  /// @class    Reference
  /// @brief    The base class for the Agent and Slot references --- these
  ///           'fillers' appear in generic-slots and specific-slots' facets.
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  ///
  /// @note     These classes' constructors assume that the name parameters
  ///           have been validated elsewhere.
  //  =========================================================================
  class Reference {
  public:
    Reference(std::string name, double weight);
    virtual ~Reference() { };
    virtual unsigned resolveReference() = 0;

    std::string name;
    double weight;
  };




  //  =========================================================================
  /// @class    AgentReference
  /// @brief    The class that implements agent references
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  =========================================================================
  class AgentReference : public Reference {
  public:
    AgentReference(std::string name, double weight);
    AgentReference(Agent *agent, double weight);
    unsigned resolveReference() throw (Exception::AgentNotFound);

    Agent *agent;
  };




  //  =========================================================================
  /// @class    SlotReference
  /// @brief    The class that implements slot references
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  =========================================================================
  class SlotReference : public Reference {
  public:
    SlotReference(std::string name, double weight);
    unsigned resolveReference() throw (Exception::SlotNotFound, Exception::AgentNotFound);

    Slot *slot;
  };


} // namespace AMBR

#endif
