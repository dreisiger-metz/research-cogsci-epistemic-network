#include "ConceptAgent.hpp"


using namespace AMBR;
using namespace std;




//  =======================================================================
/// The constructor
///
/// This constructor creates an AMBR Concept micro-agent and sets its name,
/// type and comment.
///
/// @param name    the agent's mnemonic name
/// @param type    the agent's conceptual type
/// @param comment a description of the agent
/// =======================================================================
ConceptAgent::ConceptAgent(string name, AgentType type, string comment) : Agent(name, type, comment) {
  m_agentReferenceVectors.push_back(&superClasses);
  m_agentReferenceVectors.push_back(&subClasses);
  m_agentReferenceVectors.push_back(&instances);
}




//  =======================================================================
/// The destructor
///
/// This is the class' destructor.
//  =======================================================================
ConceptAgent::~ConceptAgent() {
  unsigned i;
  
  for (i = 0; i < superClasses.size(); i++)
    delete superClasses[i];
  for (i = 0; i < subClasses.size(); i++)
    delete subClasses[i];
  for (i = 0; i < instances.size(); i++)
    delete instances[i];
}




//  =======================================================================
/// Adds a superclass
///
/// This method adds the named Concept Agent to the current agent's list of
/// superclasses.
///
/// @param agentName the name of the agent that is the current agent's
///                  conceptual superclass
/// @param weight    the strength of the inheritence;  if no weight is 
///                  specified then the default of 1.0 is used
///
/// @retval zero if the agent was resolved; one otherwise
///
/// @throw Exception::InvalidAgent if [agentName] is not a valid agent name
//  =======================================================================
unsigned ConceptAgent::addSuperClass(string agentName, double weight = 1.0) throw (Exception::InvalidAgent) {
  AgentReference *temp;
  
  // Is [agentName] valid and unique?
  if (agentName.find('.') == string::npos) {
    for (unsigned i = 0; i < superClasses.size(); i++)
      if (superClasses[i]->name == agentName) {
        Exception::InvalidAgent e(agentName, "superclass already added");
        throw e;
      }
    
    temp = new AgentReference(agentName, weight);
    
    // If [agentName] could not be resolved then add it to superClasses and return.
    // Otherwise, make sure that the agent it points to is a ConceptAgent; if it
    // is, then add it and return, otherwise throw an exception
    if (temp->agent == NULL) {
      superClasses.push_back(temp);
      Agent::AgentsWithUnresolvedReferences[this] = 1;
      return 1;
    } else if (dynamic_cast<ConceptAgent*>(temp->agent)) {
      superClasses.push_back(temp);
      AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::REFERENCE,
                        "Superclass '%s' (%f) added to '%s' and resolved",
                        agentName.c_str(), weight, this->name.c_str());
      
      return 0;
    } else {
      Exception::InvalidAgent e(agentName, "only a ConceptAgent may be a superclass");
      throw e;
    }
  } else {
    Exception::InvalidAgent e(agentName, "not a valid agent name");
    throw e;
  }
}




//  =======================================================================
/// Adds a subclass
///
/// This method adds the named Concept Agent to the current agent's list of
/// subclasses.
///
/// @param agentName the name of the agent that is the current agent's
///                  conceptual subclass
/// @param weight    the strength of the inheritence;  if no weight is 
///                  specified then the default of 1.0 is used
///
/// @retval zero if the agent was resolved; one otherwise
///
/// @throw Exception::InvalidAgent if [agentName] is not a valid agent name
//  =======================================================================
unsigned ConceptAgent::addSubClass(string agentName, double weight = 1.0) throw (Exception::InvalidAgent) {
  AgentReference *temp;
  
  if (agentName.find('.') == string::npos) {
    for (unsigned i = 0; i < subClasses.size(); i++)
      if (subClasses[i]->name == agentName) {
        Exception::InvalidAgent e(agentName, "subclass already added");
        throw e;
      }
    
    temp = new AgentReference(agentName, weight);
    
    // if [agentName] could not be resolved then add it to subClasses and return.
    // otherwise, make sure that the agent it points to is a ConceptAgent; if it
    // is, then add it and return, otherwise thrown an exception
    if (temp->agent == NULL) {
      subClasses.push_back(temp);
      Agent::AgentsWithUnresolvedReferences[this] = 1;
      return 1;
    } else if (dynamic_cast<ConceptAgent*>(temp->agent)) {
      subClasses.push_back(temp);
      AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::REFERENCE,
                        "Subclass '%s' (%f) added to '%s' and resolved",
                        agentName.c_str(), weight, this->name.c_str());
      return 0;
    } else {
      Exception::InvalidAgent e(agentName, "only a ConceptAgent may be a subclass");
      throw e;
    }
  } else {
    Exception::InvalidAgent e(agentName, "not a valid agent name");
    throw e;
  }
}




//  =======================================================================
/// Adds an instance
///
/// This method adds the named Instance Agent to the current agent's list
/// of instances
///
/// @param agentName the name of the agent that is an instance of the
///                  current concept agent
/// @param weight    the strength of the inheritence;  if no weight is 
///                  specified then the default of 1.0 is used
///
/// @retval zero if the agent was resolved; one otherwise
///
/// @throw Exception::InvalidAgent if [agentName] is not a valid agent name
//  =======================================================================
unsigned ConceptAgent::addInstance(string agentName, double weight = 1.0) throw (Exception::InvalidAgent) {
  AgentReference *temp;
  
  if (agentName.find('.') == string::npos) {
    for (unsigned i = 0; i < instances.size(); i++)
      if (instances[i]->name == agentName) {
        Exception::InvalidAgent e(agentName, "instance already added");
        throw e;
      }
    
    temp = new AgentReference(agentName, weight);
    
    // if [agentName] could not be resolved then add it to instances and return.
    // otherwise, make sure that the agent it points to is an InstanceAgent; if it
    // is, then add it and return, otherwise thrown an exception
    if (temp->agent == NULL) {
      instances.push_back(temp);
      Agent::AgentsWithUnresolvedReferences[this] = 1;
      return 1;
    } else if (dynamic_cast<InstanceAgent*>(temp->agent)) {
      instances.push_back(temp);
      AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::REFERENCE,
                        "Instance '%s' (%f) added to '%s' and resolved",
                        agentName.c_str(), weight, this->name.c_str());
      
      return 0;
    } else {
      Exception::InvalidAgent e(agentName, "only an InstanceAgent may be an instance");
      throw e;
    }
  } else {
    Exception::InvalidAgent e(agentName, "not a valid agent name");
    throw e;
  }
}




//  =======================================================================
/// Prints the ConceptAgent's information
///
/// This method prints the details of the agent to the specified output
/// stream.
///
/// @param ostream the output stream to which the information will be
///                printed
/// @param prefix  the constant C-style string that will be prefixed
///                to each line of output; this allows for arbitrary 
///                levels of indenting
///
/// @retval the number of characters printed
///
/// @note lifted from Agent::print as it's not really practical to mix
///       output streams
//  =======================================================================
int ConceptAgent::print(FILE *ostream, const char *prefix) {
  char *subPrefix = new char[strlen(prefix) + 3];
  int count = 0;
  unsigned i;
  
  count += fprintf(ostream, "%s(defagent  %s  concept-agent \"%s\"\n", prefix, this->name.c_str(), this->comment.c_str());
  count += fprintf(ostream, "%s  :type        (:concept %s)\n", prefix, 
                   (this->type == Agent::ObjectAgent)?":object":(this->type == Agent::RelationAgent)?":relation":":situation");
  
  if (superClasses.size()) {
    count += fprintf(ostream, "%s  :superclasses (", prefix);
    for (i = 0; i < superClasses.size(); i++)
      count += fprintf(ostream, " (%s, %f)", superClasses[i]->name.c_str(), superClasses[i]->weight);
    count += fprintf(ostream, " )\n");
  }
  
  if (subClasses.size()) {
    count += fprintf(ostream, "%s  :subclasses   (", prefix);
    for (i = 0; i < subClasses.size(); i++)
      count += fprintf(ostream, " (%s, %f)", subClasses[i]->name.c_str(), subClasses[i]->weight);
    count += fprintf(ostream, " )\n");
  }
  
  if (instances.size()) {
    count += fprintf(ostream, "%s  :instances    (", prefix);
    for (i = 0; i < instances.size(); i++)
      count += fprintf(ostream, " (%s, %f)", instances[i]->name.c_str(), instances[i]->weight);
    count += fprintf(ostream, " )\n");
  }
  
  if (coreferences.size()) {
    count += fprintf(ostream, "%s  :coreference  (", prefix);
    for (i = 0; i < coreferences.size(); i++)
      count += fprintf(ostream, " (%s, %f)", coreferences[i]->name.c_str(), coreferences[i]->weight);
    count += fprintf(ostream, " )\n");
  }
  
  if (associations.size()) {
    count += fprintf(ostream, "%s  :association  (", prefix);
    for (i = 0; i < associations.size(); i++)
      count += fprintf(ostream, " (%s, %f)", associations[i]->name.c_str(), associations[i]->weight);
    count += fprintf(ostream, " )\n");
  }
  
  strcpy(subPrefix, prefix);
  strcat(subPrefix, "  ");
  for (i = 0; i < slots.size(); i++)
    count += slots[i]->print(ostream, subPrefix);
  
  count += fprintf(ostream, "%s)\n", prefix);
  delete[] subPrefix;
  
  return count;
}




//  =======================================================================
/// The hook method that parses sub-class specific tags
///
/// This virtual method is called by Agent::parse() upon encountering an
/// unrecognised tag, but before the tag is treated as an s-slot.
///
/// @retval true if something was done, false otherwise
//  =======================================================================
bool ConceptAgent::postParse(FILE *istream) throw (Exception::InvalidAgent, Exception::InvalidSlot) {
  char line[AMBR::MaxLineLength], tag[32], value[AMBR::MaxLineLength], *temp_s, *save;
  float weight;
  
  if (fgets(line, AMBR::MaxLineLength, istream) && strtok_r(line, ":", &save) && (temp_s = strtok_r(NULL, ":(\"", &save))) {
    sscanf(temp_s, "%s", tag);
    
    if (!strcmp("superclasses", tag)) {
      strtok_r(NULL, "(", &save);
      while ((temp_s = strtok_r(NULL, "(", &save))) {
        sscanf(temp_s, "%s %f", value, &weight);
        value[strlen(value) - 1] = 0;
        this->addSuperClass(value, (double) weight);
      }
      
    } else if (!strcmp("subclasses", tag)) {
      strtok_r(NULL, "(", &save);
      while ((temp_s = strtok_r(NULL, "(", &save))) {
        sscanf(temp_s, "%s %f", value, &weight);
        value[strlen(value) - 1] = 0;
        this->addSubClass(value, (double) weight);
      }
      
    } else if (!strcmp("instances", tag)) {
      strtok_r(NULL, "(", &save);
      while ((temp_s = strtok_r(NULL, "(", &save))) {
        sscanf(temp_s, "%s %f", value, &weight);
        value[strlen(value) - 1] = 0;
        this->addInstance(value, (double) weight);
      }
      
    } else
      return false;
  }
  
  return true;
}




//  =======================================================================
/// The hook method that performs sub-class specific pre-initialisation
/// tasks
///
/// This virtual method is called by Agent::initialise() after the refer-
/// ences have been resolved, but before any other general initialisation
/// tasks have been performed.
///
/// @retval true if something was done, false otherwise
//  =======================================================================
bool ConceptAgent::preInitialise() throw (Exception::SlotNotFound, Exception::AgentNotFound, Exception::InvalidAgent) {
  unsigned i;
  Agent *agent;
  
  /// At the moment, ::initialise() removes an invalidly-typed agents from
  /// the superClasses, subClasses and instances vectors just prior to
  /// throwing an error.  While this behaviour makes unit testing easier,
  /// it probably isn't necessary as this would indicate a fairly serious
  /// problem with the input data.
  try {
    for (i = 0; i < superClasses.size(); i++)
      if ((agent = superClasses[i]->agent))
        if (dynamic_cast<ConceptAgent*>(agent) == NULL) {
          // We've just found an InstanceAgent in ::superClasses --- remove
          // it and throw an exception
          superClasses.erase(superClasses.begin() + i);
          Exception::InvalidAgent e(superClasses[i]->agent->name, "a superclass resolved to something other than a ConceptAgent");
          throw e;
        }
    for (i = 0; i < subClasses.size(); i++)
      if ((agent = subClasses[i]->agent))
        if (dynamic_cast<ConceptAgent*>(agent) == NULL) {
          subClasses.erase(subClasses.begin() + i);
          Exception::InvalidAgent e(subClasses[i]->agent->name, "a subclass resolved to something other than a ConceptAgent");
          throw e;
        }
    for (i = 0; i < instances.size(); i++)
      if ((agent = instances[i]->agent))
        if (dynamic_cast<InstanceAgent*>(agent) == NULL) {
          instances.erase(instances.begin() + i);
          Exception::InvalidAgent e(instances[i]->agent->name, "an instance resolved to something other than an InstanceAgent");
          throw e;
        }
  } catch (...) {
    throw;
  }
  
  return 1;
}




//  =======================================================================
/// The virtual method that is called by tick to provide an upper-bound on
/// the number of operations that will be performed during the current
/// timestep
///
/// This virtual method is implemented by derived classes and called by 
/// tick();  it returns an upper-bound on the number of operations that 
/// the agent will perform during the current timestep.
///
/// @retval an upper-limit on the number of operations that will be 
///         performed during the current tick
//  =======================================================================
unsigned ConceptAgent::nopsThisTick() {
  if (m_incomingMarkers.size() == 0)
    return 0;
  
  else if ((m_incomingMarkers.size() == 1) && (m_originatingAgentToMarkerMap.size() == 0))
    return superClasses.size() + coreferences.size();
  
  else {
    // First of all, remove any duplicates and/or older markers with the
    // same originating agent...
    for (m_tick__i = 0; m_tick__i < m_incomingMarkers.size(); m_tick__i++)
      for (m_tick__j = 0; m_tick__j < m_tick__i; m_tick__j++)
        if (m_incomingMarkers[m_tick__i]->origin == m_incomingMarkers[m_tick__j]->origin)
          if (m_incomingMarkers[m_tick__i]->serial > m_incomingMarkers[m_tick__j]->serial) {
            m_incomingMarkers.erase(m_incomingMarkers.begin() + m_tick__j);
            m_tick__j--;  // we need to do this because we've just removed an item from the vector
          } else {
            m_incomingMarkers.erase(m_incomingMarkers.begin() + m_tick__i);
            m_tick__i--;
          } // we /could/ get pairs of identical markers arriving at the same time
    // via different paths, but even then we can't use path length to pick
    // the winner as instantaneous activation also determines their speed 
    
    // Next, work out the number of unique pairings (represented here by [m_tick__j].
    // Once we have it, return n-choose-2.
    for (m_tick__i = 0, m_tick__j = m_incomingMarkers.size(); m_tick__i < m_incomingMarkers.size(); m_tick__i++) {
      m_tick__markerMessage = m_incomingMarkers[m_tick__i];
      if (m_originatingAgentToMarkerMap.find(m_tick__markerMessage->origin) == m_originatingAgentToMarkerMap.end())
        m_tick__j++;
      else {
        // We already have a marker from (m_incomingMarkers[m_tick__i]->origin) ---
        // if the serial is the same, then we can erase the incoming marker;  if the
        // incoming marker's serial is greater than the one we already have then we
        // need to remove the one from m_originatingAgentToMarkerMap
        if (m_tick__markerMessage->serial > m_originatingAgentToMarkerMap[m_tick__markerMessage->origin]->serial) {
          m_tick__j++;
          m_originatingAgentToMarkerMap.erase(m_originatingAgentToMarkerMap.find(m_tick__markerMessage->origin));
        } else {
          m_incomingMarkers.erase(m_incomingMarkers.begin() + m_tick__i);
          m_tick__i--;
        }
      }
    }
    // After this loop, neither [m_incomingMarkers], [m_originatingAgent-
    // ToMarkerMap] or their union contain markers from the same origin.
    
    return m_tick__j * (m_tick__j - 1) / 2;   // n-choose-2
  } 
}




//  =======================================================================
/// The hook method that is called to perform sub-class agent operations
///
/// This virtual method is called by tick() to perform agent-specific
/// operations (such as marker passing or hypothesis formation);  it is
/// called after the activation has been calculated and spread, and after
/// the markers have been moved from their input zone to the main buffer.
/// In this method, we are going to look for marker intersections or, in
/// their absence, pass them on to our superclasses and coreferences.
///
/// @retval the number of activation quanta used
//  =======================================================================
unsigned ConceptAgent::postTick() {
  
  if ((m_incomingMarkers.size() == 1) && (m_originatingAgentToMarkerMap.size() == 0)) {
    // ================================================================
    // Nothing has intersected --- let's just pass the incoming message
    // on to our superclasses and coreferences
    if (nopsRemaining() < superClasses.size() + coreferences.size()) {
      // Because of limited resources, we need to alternate between 
      // sending markers to our superclasses and our coreferences (also
      // note that if we've made it into /this/ block, our reference
      // vectors will have been sorted by decreasing levels of activation)
      m_tick__i = 0;
      m_tick__j = 0;
      while (nopsRemaining() > 0) {
        if (consumeActivation() && m_tick__i < superClasses.size()) {
          m_manager->sendMessage(this, superClasses[m_tick__i]->agent, (Message::BaseMessage *) (new Message::Marker(this, m_incomingMarkers[0])));
          m_tick__i++;
        }
        if (consumeActivation() && m_tick__j < coreferences.size()) {
          m_manager->sendMessage(this, coreferences[m_tick__j]->slot->agent, (Message::BaseMessage *) (new Message::Marker(this, m_incomingMarkers[0])));
          m_tick__j++;
        }
      }
      
    } else {
      // We have enough activation, so we can simply sending the markers
      // off in the simplest way possible...
      consumeActivation(superClasses.size() + coreferences.size());
      // need to delete the messages too
      for (m_tick__i = 0; m_tick__i < superClasses.size(); m_tick__i++)
        m_manager->sendMessage(this, superClasses[m_tick__i]->agent, (Message::BaseMessage *) (new Message::Marker(this, m_incomingMarkers[0])));
      for (m_tick__i = 0; m_tick__i < coreferences.size(); m_tick__i++)
        m_manager->sendMessage(this, coreferences[m_tick__i]->slot->agent, (Message::BaseMessage *) (new Message::Marker(this, m_incomingMarkers[0])));
    }

    // Finally, we have to move marker[0] from the incoming marker queue to
    // m_originatingAgentToMarkerMap
    m_originatingAgentToMarkerMap[m_incomingMarkers[0]->origin] = m_incomingMarkers[0];
    
  } else if (m_incomingMarkers.size() + m_originatingAgentToMarkerMap.size() > 1) {
    // ====================================================================
    // We have an intersection (each pair of markers that (a) has different
    // originating agents, and (b) involves at least one agent from the 
    // incoming marker buffer constitutes an intersection).  We know, from
    // nopsThisTick() that neither the marker vector or the map contain
    // duplicates so we can just go ahead and enumerate all of the valid
    // pairs.
    map<Agent*, Message::Marker*>::iterator k;
    AMBR::HypothesisAgent *agent;
    
    for (m_tick__i = 0; m_tick__i < m_incomingMarkers.size(); m_tick__i++)
      for (m_tick__j = m_tick__i + 1; consumeActivation(Agent::AgentCreationCost) && (m_tick__j < m_incomingMarkers.size()); m_tick__j++) {
        // request the creation of a new hypothesis agent implying a
        // correspondence between the two originating agents, citing
        // (this) as justification.  Don't forget the cost of doing this!
        AMBR::logger->log(__FILE__, __LINE__, Logger::INFORMATIVE, AMBRLogger::MARKER_INTERSECTION,
                          "Markers %s:%d and %s:%d intersected at agent %s at time %d",
                          m_incomingMarkers[m_tick__i]->origin->name.c_str(), m_incomingMarkers[m_tick__i]->serial,
                          m_incomingMarkers[m_tick__j]->origin->name.c_str(), m_incomingMarkers[m_tick__j]->serial,
                          this->name.c_str(), time());
        agent = new AMBR::HypothesisAgent(m_incomingMarkers[m_tick__i]->origin->name + "<-->" + m_incomingMarkers[m_tick__j]->origin->name,
                                          m_incomingMarkers[m_tick__i]->origin, m_incomingMarkers[m_tick__j]->origin,
                                          this, "generated by " + name);
      }
    
    // We need to split this section into two loops otherwise we'd be
    // enumerating all pairs of markers in [m_incomingMarkers] /and/
    // removing elements from it at the same time
    for (m_tick__i = 0; m_tick__i < m_incomingMarkers.size(); m_tick__i++)
      for (k = m_originatingAgentToMarkerMap.begin(); consumeActivation(Agent::AgentCreationCost) && (k != m_originatingAgentToMarkerMap.end()); k++) {
        // request the creation of a new hypothesis agent
        AMBR::logger->log(__FILE__, __LINE__, Logger::INFORMATIVE, AMBRLogger::MARKER_INTERSECTION,
                          "Markers %s:%d and %s:%d intersected at agent %s at time %d",
                          m_incomingMarkers[m_tick__i]->origin->name.c_str(), m_incomingMarkers[m_tick__i]->serial,
                          k->first->name.c_str(), k->second->serial, this->name.c_str(), time());
        agent = new AMBR::HypothesisAgent(m_incomingMarkers[m_tick__i]->origin->name + "<-->" +k->first->name,
                                    m_incomingMarkers[m_tick__i]->origin, k->first,
                                    this, "generated by " + name);
      }
    
    // And move the elements of [m_incomingMarkers] to [m_originatingAgent-
    // toMarkerMap]
    for (m_tick__i = 0; m_tick__i < m_incomingMarkers.size(); m_tick__i++)
      m_originatingAgentToMarkerMap[m_incomingMarkers[m_tick__i]->origin] = m_incomingMarkers[m_tick__i];
  }

  return 1;
}




//  =======================================================================
/// The hook method that is called to handle unknown message types
///
/// This virtual method is called by Agent::receive() to handle any message
/// types that it does not know about.
///
/// @retval true if something was done, false otherwise
//  =======================================================================
bool ConceptAgent::postReceive(Message::BaseMessage *message) {
  if (message->type == Message::HypothesisRequest) {
    
    return true;
  } else
    return false;
}
