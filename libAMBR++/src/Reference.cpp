#include "Reference.hpp"

using namespace std;


namespace AMBR {


  //  =========================================================================
  /// @brief    The constructor
  ///
  /// @param  name    the name of the agent or slot being referred to
  /// @param  weight  the strength of the reference
  //  =========================================================================
  Reference::Reference(string name, double weight) {
    this->name = name;
    this->weight = weight;
  }






  //  =========================================================================
  /// @brief    The constructor
  ///
  /// @param  name    the name of the agent being referred to
  /// @param  weight  the strength of the reference
  //  =========================================================================
  AgentReference::AgentReference(string name, double weight) : Reference(name, weight) { 
    map<string, Agent*>::iterator i = Agent::AgentNameToPointerMap.find(name);

    if (i != Agent::AgentNameToPointerMap.end())
      this->agent = i->second;
    else
      this->agent = NULL;
  }


  //  =========================================================================
  /// @brief    The constructor
  ///
  /// @param  agent   pointer to the agent being referrenced
  /// @param  weight  the strength of the reference
  //  =========================================================================
  AgentReference::AgentReference(Agent *agent, double weight) : Reference(agent->name, weight) { 
    this->agent = agent;
  }
  
  
  //  =========================================================================
  /// @brief    Resolves the agent reference
  /// This method queries the Agent::AgentNameToPointerMap to determine the
  /// address of the Agent whose name is equal to this->name
  //  =========================================================================
  unsigned AgentReference::resolveReference() throw (Exception::AgentNotFound) {
    if (this->agent == NULL) {
      map<string, Agent*>::iterator i = Agent::AgentNameToPointerMap.find(this->name);

      if (i != Agent::AgentNameToPointerMap.end())
	this->agent = i->second;
      else {
	Exception::AgentNotFound e(this->name, "Agent not in [Agent::AgentNameToPointerMap]");
	throw e;
      }
    }
    
    return 1;  // we can only reach this point /if/ this reference has been resolved
  }






  //  =========================================================================
  /// @brief    The constructor
  ///
  /// @param  name    the name of the slot being referred to
  /// @param  weight  the strength of the reference
  //  =========================================================================
  SlotReference::SlotReference(string name, double weight) : Reference(name, weight) { 
    string agentName = this->name.substr(0, this->name.find('.')),
            slotName = this->name.substr(this->name.find('.') + 1);
    map<string, Agent*>::iterator ia = Agent::AgentNameToPointerMap.find(agentName);
    
    if (ia != Agent::AgentNameToPointerMap.end())
      this->slot = ia->second->getSlot(slotName);
    else
      this->slot = NULL;
  }


  //  =========================================================================
  /// @brief    Resolves the slot reference
  /// This method queries the relevant agent to determine the address of the
  /// Slot whose name is equal to this->name
  //  =========================================================================
  unsigned SlotReference::resolveReference() throw (Exception::SlotNotFound, Exception::AgentNotFound) {
    if (this->slot == NULL) {
      string agentName = this->name.substr(0, this->name.find('.')),
             slotName = this->name.substr(this->name.find('.') + 1);
      map<string, Agent*>::iterator ia = Agent::AgentNameToPointerMap.find(agentName);
    
      if (ia != Agent::AgentNameToPointerMap.end()) {
	this->slot = ia->second->getSlot(slotName);

	if (this->slot == NULL) {
	  Exception::SlotNotFound e(slotName, "Slot not in agent '" + agentName + "'");
	  throw e;
	}
      } else {
	Exception::AgentNotFound e(agentName, "Agent not in [Agent::AgentNameToPointerMap]");
	throw e;
      }
    }

    return 1;  // we can only reach this point /if/ this reference has been resolved
  }


} // namespace AMBR
