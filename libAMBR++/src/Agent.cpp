#include "Agent.hpp"


using namespace AMBR;
using namespace std;




// Instantiate the static member variables
map<string, Agent*>   Agent::AgentNameToPointerMap;
map<Agent*, unsigned> Agent::AgentsWithUnresolvedReferences;
map<Agent*, unsigned> Agent::AgentsInWorkingMemory;




// Constant values from DUAL/defs.lsp:43 (c.f. with *Kokinov-threshold*
// and *Kokinov-decay-rate* in defs.lsp:93, which are both 0.1)
const double   Agent::ActivationMaximum = 1.0;
const double   Agent::ActivationMinimum = 0.0;
const double   Agent::ActivationDecayRate = 2.0;
const double   Agent::ActivationExcitationRate = 1.0;
const double   Agent::ActivationThreshold = 0.1;
const double   Agent::ActivationMinimumIncoming = ActivationDecayRate * ActivationThreshold / (ActivationExcitationRate * (ActivationMaximum - ActivationThreshold));
const double   Agent::ActivationQuantum = 0.01;
const unsigned Agent::AgentCreationCost = 3;
const double   Agent::TimeQuantum = 0.01;




//  =======================================================================
/// The constructor
///
/// This constructor creates a generic DUAL micro-agent and sets its name,
/// type and comment.
///
/// @param name    the agent's mnemonic name
/// @param type    the agent's conceptual type
/// @param comment a description of the agent
/// =======================================================================
Agent::Agent(string name, AgentType type, string comment) : VariableRateExecution::Agent (name, 0.0, AMBR::Manager) {
  // Set the agent's identity,
  this->type = type;
  this->comment = comment;
  
  // initialise our member variables
  m_incomingActivation = 0.0;
  m_activationFixed = false;
  m_tick__activationMessage = new Message::Activation(0.0);
  
  m_agentReferenceVectors.push_back(&associations);
  m_slotReferenceVectors.push_back(&coreferences);
  
  // and add it to the static maps
  Agent::AgentNameToPointerMap[name] = this;
  
  
  AMBR::logger->log(__FILE__, __LINE__, Logger::INFORMATIVE, AMBRLogger::AGENT,
                    "%s agent '%s' created",
                    (type == Agent::ObjectAgent)?"Object":
                    (type == Agent::RelationAgent)?"Relation":
                    (type == Agent::HypothesisAgent)?"Hypothesis":"Situation", 
                    name.c_str());
}




//  =======================================================================
/// The destructor
///
/// This is the class' destructor.
//  =======================================================================
Agent::~Agent() {
  unsigned i;
  multimap<Agent*, Message::Marker*>::iterator j;
  map<Agent*, unsigned>::iterator l;
  
  // First, we remove the agent from the static maps
  Agent::AgentNameToPointerMap.erase(this->name);
  if ((l = Agent::AgentsWithUnresolvedReferences.find(this)) != Agent::AgentsWithUnresolvedReferences.end())
    Agent::AgentsWithUnresolvedReferences.erase(l);
  
  // Then we delete its references and slots,
  for (i = 0; i < coreferences.size(); i++)
    delete coreferences[i];
  for (i = 0; i < associations.size(); i++)
    delete associations[i];
  for (i = 0; i < slots.size(); i++)
    delete slots[i];
  
  // any markers and messages it may have stored locally,
  for (i = 0; i < m_incomingMarkers.size(); i++)
    delete m_incomingMarkers[i];
  for (j = m_originatingAgentToMarkerMap.begin(); j != m_originatingAgentToMarkerMap.end(); j++)
    delete j->second;
  
  delete m_tick__activationMessage;
}




//  =======================================================================
/// Adds a coreference
///
/// This method adds the named slot to the agent's list of coreferences.
///
/// @param slotName the slot's fully-qualified name (i.e. AgentName.SlotName)
/// @param weight   the strength of the coreference;  if no weight is
///                 specified then the default of 1.0 is used
///
/// @retval zero if the coreference was resolved; one otherwise
///
/// @throw Exception::InvalidSlot if [slotName] is not a valid slot name
//  =======================================================================
unsigned Agent::addCoreference(string slotName, double weight = 1.0) throw (Exception::InvalidSlot) {
  SlotReference *temp;
  
  // Is 'slotName' valid and unique?
  if (slotName.find('.') != string::npos) {
    for (unsigned i = 0; i < coreferences.size(); i++)
      if (coreferences[i]->name == slotName) {
        Exception::InvalidSlot e(slotName, "coreference already added");
        throw e;
      }
    // It is, so add it to this->coreferences
    coreferences.push_back(temp = new SlotReference(slotName, weight));
  } else {
    Exception::InvalidSlot e(slotName, "Not a valid slot name");
    throw e;
  }
  
  // See if the slot reference was resolved;  if it wasn't then add the
  // agent to Agent::AgentsWithUnresolvedReferences.
  if (temp->slot == NULL) {
    Agent::AgentsWithUnresolvedReferences[this] = 1;
    return 1;
  } else {
    AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::REFERENCE,
                      "Coreference from '%s' to '%s' (%f) added and resolved",
                      this->name.c_str(), slotName.c_str(), weight);
    return 0;
  }
}




//  =======================================================================
/// Adds an association
///
/// This method adds the named agent to the current agent's list of 
/// associations.
///
/// @param agentName the agent's name
/// @param weight    the strength of the coreference;  if no weight is
///                  specified then the default of 1.0 is used
///
/// @retval zero if the association was resolved; one otherwise
///
/// @throw Exception::InvalidAgent if [agentName] is not a valid agent name
//  =======================================================================
unsigned Agent::addAssociation(string agentName, double weight = 1.0) throw (Exception::InvalidAgent) {
  AgentReference *temp;
  
  // Is 'agentName' valid and unique?
  if (agentName.find('.') == string::npos) {
    for (unsigned i = 0; i < associations.size(); i++)
      if (associations[i]->name == agentName) {
        Exception::InvalidAgent e(agentName, "agent already added");
        throw e;
      }
    // It is, so add it to this->associations
    associations.push_back(temp = new AgentReference(agentName, weight));
  } else {
    Exception::InvalidAgent e(agentName, "not a valid agent name");
    throw e;
  }
  
  // See if the agent reference was resolved;  if it wasn't then add the 
  // agent to Agent::AgentsWithUnresolvedReferences.
  if (temp->agent == NULL) {
    Agent::AgentsWithUnresolvedReferences[this] = 1;
    return 1;
  } else {
    AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::REFERENCE,
                      "Association from '%s' to '%s' (%f) added and resolved",
                      this->name.c_str(), agentName.c_str(), weight);
    return 0;
  }
}




//  =======================================================================
/// Adds a slot.
///
/// This method adds a specific-slot to the agent.
///
/// @param slot a pointer to the specific-slot
///
/// @retval zero if all of the slot's associations were resolved; one 
///         otherwise
///
/// @throw Exception::InvalidSlot if [slot] is NULL
//  =======================================================================
unsigned Agent::addSlot(Slot *slot) throw (Exception::InvalidSlot) {
  // Is 'slot' valid and unique?
  if (slot) {
    if (m_slotNameToPointerMap.find(slot->name) == m_slotNameToPointerMap.end()) {
      // It is, so add it to our slot vector and map, and set slot->agent
      slots.push_back(slot);
      m_slotNameToPointerMap[slot->name] = slot;
      slot->agent = this;
    } else {
      Exception::InvalidSlot e(slot->name, "slot already added");
      throw e;
    }
  } else {
    Exception::InvalidSlot e("", "[slot] == NULL");
    throw e;
  }
  
  AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::AGENT,
                    "Slot '%s.%s' added", this->name.c_str(), slot->name.c_str());
  
  // should we return the slot's resolved-status or the index?
  return slots.size() - 1;
}




//  =======================================================================
/// Returns the named slot.
///
/// This method returns a pointer to the named slot.
///
/// @param name the name of the desired slot
///
/// @retval a pointer to the specific-slot, or NULL if no such slot exists.
//  =======================================================================
Slot *Agent::getSlot(string name) {
  map<string, Slot*>::iterator i = m_slotNameToPointerMap.find(name);
  
  if (i != m_slotNameToPointerMap.end())
    return i->second;
  else
    return NULL;
}




//  =======================================================================
/// Called whenever the agent becomes active
///
/// This virtual hook method is called by the agent's manager whenever its
/// level of activation goes from below, to above the activation-threshold.
///
/// @retval true if something was done; false otherwise
//  =======================================================================
bool Agent::activate() {
  AMBR::logger->log(__FILE__, __LINE__, Logger::VERBOSE, AMBRLogger::ACTIVATION,
                    "%s became active at time %d", name.c_str(), time());
  
  AMBR::Memory->addToWorkingMemory(this);
  
  if (postActivate())
    AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::ACTIVATION,
                      "postActivate() was called on %s", name.c_str());
  
  return true;
}




//  =======================================================================
/// Called whenever the agent becomes active
///
/// This virtual hook method is called by the agent's manager whenever its
/// level of activation goes from below, to above the activation-threshold.
///
/// @retval true if something was done; false otherwise
//  =======================================================================
bool Agent::deactivate() {
  AMBR::logger->log(__FILE__, __LINE__, Logger::VERBOSE, AMBRLogger::ACTIVATION,
                    "%s became inactive at time %d", name.c_str(), time());

  AMBR::Memory->removeFromWorkingMemory(this);
  
  if (preDeactivate())
    AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::ACTIVATION,
                      "preDeactivate() was called on %s", name.c_str());
  
  for (m_tick__i = 0; m_tick__i < m_incomingMarkers.size(); m_tick__i++)
    delete m_incomingMarkers[m_tick__i];
  m_incomingMarkers.clear();
  
  for (map<Agent*, Message::Marker*>::iterator j = m_originatingAgentToMarkerMap.begin(); j != m_originatingAgentToMarkerMap.end(); j++)
    delete j->second;
  m_originatingAgentToMarkerMap.clear();
  
  return true;
}




//  =======================================================================
/// Sets and fixes the agent's activation
///
/// This method sets the agent's activation to the value specified, and
/// prevents it from decaying during subsequent ticks.  It can be used to
/// represent percepts, or during testing
///
/// @param activation the level of activation
///
/// @note this specified activation is /not/ subject to the usual limits
//  =======================================================================
void Agent::lockActivation(double activation) {
  this->m_activationFixed = true;
  this->m_activation = activation;
  AMBR::logger->log(__FILE__, __LINE__, Logger::VERBOSE, AMBRLogger::ACTIVATION,
                    "%s's activation locked at time %d to %f", name.c_str(), time(), m_activation);
}




//  =======================================================================
/// Unlocks the agent's activation
///
/// When called on a previously-locked agent, this method will allow its
/// level of activation to decay, once again, during subsequent ticks.
//  =======================================================================
void Agent::unlockActivation() {
  this->m_activationFixed = false;
  AMBR::logger->log(__FILE__, __LINE__, Logger::VERBOSE, AMBRLogger::ACTIVATION,
                    "%s's activation unlocked at time %d", name.c_str(), time());
}




//  =======================================================================
/// Initialises the agent
///
/// This method resolves all of the slot and agent references, and 
/// normalises the link weights.
//  =======================================================================
void Agent::initialise() throw (Exception::SlotNotFound, Exception::AgentNotFound, Exception::InvalidAgent) {
  unsigned i, j;
  double scale = 0.0;
  map<Agent*, double>::iterator k;
  map<Agent*, unsigned>::iterator l;
  Agent *agent;
  Slot  *slot;
  AgentReference *agentReference;
  SlotReference  *slotReference;
  
  
  // ==========================================================================
  // Call resolveReferences on each of our coreferences, associations and slots
  try {
    for (i = 0; i < m_agentReferenceVectors.size(); i++)
      for (j = 0; j < m_agentReferenceVectors[i]->size(); j++)
        (*m_agentReferenceVectors[i])[j]->resolveReference();
    for (i = 0; i < m_slotReferenceVectors.size(); i++)
      for (j = 0; j < m_slotReferenceVectors[i]->size(); j++)
        (*m_slotReferenceVectors[i])[j]->resolveReference();
    
    for (i = 0; i < slots.size(); i++)
      slots[i]->resolveReferences();
  } catch (...) {
    // I should probably do something more useful here...
    throw;
  }
  
  if ((l = Agent::AgentsWithUnresolvedReferences.find(this)) != Agent::AgentsWithUnresolvedReferences.end())
    Agent::AgentsWithUnresolvedReferences.erase(l);
  
  
  // ========================================================================
  // Call preInitialise() to perform any sub-class specific preinitialisation
  try {
    if (preInitialise())
      AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::AGENT,
                        "preInitialise() called on agent '%s'", this->name.c_str());
  } catch (...) {
    throw;
  }
  
  
  // ========================================================================
  // If we've reached this point then all of the agent's references have been
  // resolved;  we can now update m_agentPointerToLinkWeightMap.
  m_agentPointerToLinkWeightMap.clear();   // Is there any reason why we wouldn't want to do this?
  for (i = 0; i < m_agentReferenceVectors.size(); i++)
    for (j = 0; j < m_agentReferenceVectors[i]->size(); j++) {
      agentReference = (*m_agentReferenceVectors[i])[j];
      if ((k = m_agentPointerToLinkWeightMap.find(agentReference->agent)) == m_agentPointerToLinkWeightMap.end())
        m_agentPointerToLinkWeightMap[agentReference->agent] = agentReference->weight;
      else if (agentReference->weight > k->second)
        k->second = agentReference->weight;
    }
  for (i = 0; i < m_slotReferenceVectors.size(); i++)
    for (j = 0; j < m_slotReferenceVectors[i]->size(); j++) {
      slotReference = (*m_slotReferenceVectors[i])[j];
      if ((k = m_agentPointerToLinkWeightMap.find(slotReference->slot->agent)) == m_agentPointerToLinkWeightMap.end())
        m_agentPointerToLinkWeightMap[slotReference->slot->agent] = slotReference->weight;
      else if (slotReference->weight > k->second)
        k->second = slotReference->weight;
    }
  
  for (i = 0; i < slots.size(); i++) {
    slot = slots[i];
    // For each slot, we need to go through its superclasses, instance-ofs,
    // coreferences and associations, and examine each of the associated
    // link weights
    for (j = 0; j < slot->superClasses.size(); j++)
      if (slot->superClasses[j]->slot) {
        // This superclass has been resolved --- because we found the other slot
        // via its owning agent, this means that its agent can also be resolved
        agent = slot->superClasses[j]->slot->agent;
        if ((k = m_agentPointerToLinkWeightMap.find(agent)) == m_agentPointerToLinkWeightMap.end())
          m_agentPointerToLinkWeightMap[agent] = slot->superClasses[j]->weight;
        else if (slot->superClasses[j]->weight > k->second)
          k->second = slot->superClasses[j]->weight;
      }
    for (j = 0; j < slot->instanceOf.size(); j++)
      if (slot->instanceOf[j]->slot) {   // This instance-of was resolved
        agent = slot->instanceOf[j]->slot->agent;
        if ((k = m_agentPointerToLinkWeightMap.find(agent)) == m_agentPointerToLinkWeightMap.end())
          m_agentPointerToLinkWeightMap[agent] = slot->instanceOf[j]->weight;
        else if (slot->instanceOf[j]->weight > k->second)
          k->second = slot->instanceOf[j]->weight;
      }
    for (j = 0; j < slot->coreferences.size(); j++)
      if ((agent = slot->coreferences[j]->agent)) {   // This coreference was resolved
        if ((k = m_agentPointerToLinkWeightMap.find(agent)) == m_agentPointerToLinkWeightMap.end())
          m_agentPointerToLinkWeightMap[agent] = slot->coreferences[j]->weight;
        else if (slot->coreferences[j]->weight > k->second)
          k->second = slot->coreferences[j]->weight;
      }
    for (j = 0; j < slot->associations.size(); j++)
      if ((agent = slot->associations[j]->agent)) {   // This association was resolved
        if ((k = m_agentPointerToLinkWeightMap.find(agent)) == m_agentPointerToLinkWeightMap.end())
          m_agentPointerToLinkWeightMap[agent] = slot->associations[j]->weight;
        else if (slot->associations[j]->weight > k->second)
          k->second = slot->associations[j]->weight;
      }
  }
  
  // =====================================================================
  // Having resolved all our links and stored the maximal, unique inter-
  // agent weights, we need to update 'agentPointerToNormalisedLinkWeight-
  // Map' --- doing so will allow us to spread our activation /much/ more
  // efficiently...
  m_agentPointerToNormalisedLinkWeightMap.clear();
  for (k = m_agentPointerToLinkWeightMap.begin(); k != m_agentPointerToLinkWeightMap.end(); k++)
    scale += k->second;
  if (scale != 0.0) {   // no one likes divide-by-zeros errors
    scale = (double) 1.0 / scale;
    for (k = m_agentPointerToLinkWeightMap.begin(); k != m_agentPointerToLinkWeightMap.end(); k++)
      m_agentPointerToNormalisedLinkWeightMap[k->first] = k->second * scale;
  }
  
  
  // ===========================================================================
  // Call postInitialise() to perform any sub-class specific post initialisation
  try {
    if (postInitialise())
      AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::AGENT,
                        "postInitialise() called on agent '%s'", this->name.c_str());
  } catch (...) {
    throw;
  }
  
  
  AMBR::logger->log(__FILE__, __LINE__, Logger::VERBOSE, AMBRLogger::AGENT,
                    "Agent '%s' resolved its references and updated the maximal, unique link weight maps", this->name.c_str());
}




//  =======================================================================
/// Prints the agent's information
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
//  =======================================================================
int Agent::print(FILE *ostream, const char *prefix) {
  char *subPrefix = new char[strlen(prefix) + 3];
  int count = 0;
  unsigned i;
  
  count += fprintf(ostream, "%s(defagent  %s  \"%s\"\n", prefix, this->name.c_str(), this->comment.c_str());
  count += fprintf(ostream, "%s  :type         %s\n", prefix, 
                   (this->type == Agent::ObjectAgent)?":object":(this->type == Agent::RelationAgent)?":relation":":situation");
  
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
/// Parses the textual representation of the agent's information
///
/// This method parses an earlier printout of the agent.
///
/// @param istream the input stream from which the information will be
///                obtained
///
/// @retval one if something was successfully parsed, zero otherwise
//  =======================================================================
int Agent::parse(FILE *istream) throw (Exception::InvalidAgent, Exception::InvalidSlot) {
  char line[AMBR::MaxLineLength], tag[32], value[AMBR::MaxLineLength], *temp_s, *save;
  long start = ftell(istream), rollback = start, temp_l;
  float weight;
  Slot *slot;
  
  while (fgets(line, AMBR::MaxLineLength, istream)) {
    // Does the line contain a LISP-style 'constant' definition?
    if (strtok_r(line, ":", &save) && (temp_s = strtok_r(NULL, ":(\"", &save))) {
      sscanf(temp_s, "%s", tag);
      
      if (!strcmp("type", tag)) {                 // a :type tag
        temp_s = strtok_r(NULL, ")", &save);
        if (strstr(temp_s, "object"))
          this->type = Agent::ObjectAgent;
        else if (strstr(temp_s, "relation"))
          this->type = Agent::RelationAgent;
        else if (strstr(temp_s, "situation"))
          this->type = Agent::SituationAgent;
        else {
          Exception::InvalidAgent e(name, "invalid agent type");
          //printf("\n\ntemp_s == '%s'\n\n", temp_s);
          throw e;
        }
        
      } else if (!strcmp("coreference", tag)) {   // a :coreference tag
        strtok_r(NULL, "(", &save);
        while ((temp_s = strtok_r(NULL, "(", &save))) {
          sscanf(temp_s, "%s %f", value, &weight);
          value[strlen(value) - 1] = 0;
          this->addCoreference(value, (double) weight);
        }
        
      } else if (!strcmp("association", tag)) {   // a :association tag
        strtok_r(NULL, "(", &save);
        while ((temp_s = strtok_r(NULL, "(", &save))) {
          sscanf(temp_s, "%s %f", value, &weight);
          value[strlen(value) - 1] = 0;
          this->addAssociation(value, (double) weight);
        }
        
      } else {
        // What we have here may be either an unknown tag (which can be
        // handled by our derived class' postParse(...) method), or an
        // agent-specific slot (s-slot).  We need to handle both situ-
        // ations.
        temp_l = ftell(istream);
        fseek(istream, rollback - ftell(istream), SEEK_CUR);  // seek back to the start of the line
        if (!postParse(istream)) {
          // since postParse(...) failed to recognise this tag, we've got
          // ourselves an s-slot;  before we can continue, we need to
          // advance [istream] back to its previous position
          fseek(istream, temp_l, SEEK_SET);  
          temp_s = strtok_r(NULL, "\"", &save);
          
          // Create a new slot instance, and call its parse method.
          slot = new Slot(tag, Slot::AspectSlot, temp_s);
          slot->parse(istream);
          this->addSlot(slot);
        }
      }
      
    } else if (strcspn(line, ")") != strlen(line)) {
      // The line contains neither a definition nor a blank --- instead
      // it contains a terminating ')'.
      break;
    }
    rollback = ftell(istream);
  }
  
  return ftell(istream) - start;
}




//  =======================================================================
/// Yields some CPU time to the agent
///
/// This method allows the agent to perform its symbolic operations; the
/// amount of processing that the agent can accomplish is proportional to
/// its current level of activation.  This method is called, periodically,
/// by VariableRateExecution::Manager.
///
/// @retval true if something was done, false otherwise
//  =======================================================================
bool Agent::tick() {
  AMBR::logger->log(__FILE__, __LINE__, Logger::VERBOSE, AMBRLogger::AGENT, 
                    "%s was ticked at time %d", this->name.c_str(), time());
  
  // Before we can perform any of the symbolic tasks, we need to (a) work
  // out how many operations we can perform at our current level of acti-
  // vation, and (b) if we have more operations than activation, generate
  // an activation-biased list of prospective 'operands', or agents, that
  // we will interact with or send messages to.
  // These steps embody three DUAL principles --- (a) the more active the
  // agent, the faster it can run, (b) the more active the agent, the more
  // work it can do per tick (this is, perhaps, a corollary, but it is
  // consistent with the DUAL philosophy), and (c) the more active the 
  // /receiving/ agent, the more likely it is to be chosen.  (See pp 29 and
  // 40 of the DUAL Architecture report for more details.)
  if (nopsThisTick() > nopsRemaining()) {
    // We don't, so we're going to have to sort each of the reference and
    // slot vectors by their associated agents' level of activation.
    sort(slots.begin(), slots.end(), Agent::sortSlotsByActivation);
    for (m_tick__i = 0; m_tick__i < m_agentReferenceVectors.size(); m_tick__i++)
      sort(m_agentReferenceVectors[m_tick__i]->begin(), m_agentReferenceVectors[m_tick__i]->end(), Agent::sortAgentReferencesByActivation);
    for (m_tick__i = 0; m_tick__i < m_slotReferenceVectors.size(); m_tick__i++)
      sort(m_slotReferenceVectors[m_tick__i]->begin(), m_slotReferenceVectors[m_tick__i]->end(), Agent::sortSlotReferencesByActivation);
  }
  
  // Next we call the derived class' post-tick method
  if (postTick())
    AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::AGENT, 
                      "postTick called on agent '%s'", this->name.c_str());
  
  // And finally, we clear the incoming markers queue
  m_incomingMarkers.clear();
  return true;
}




//  =======================================================================
/// Determines what the agent's new level of activation should be
///
/// This method is called by the VariableRateExecution::Manager, after
/// every iteration, to update m_activation;  depending on the nature of 
/// the derived class, the level may be related to the total amount of
/// /incoming/ activation, or the /fitness/ of the agent's algorithm or 
/// parameter values.
//  =======================================================================
void Agent::updateActivation() {
  // Firstly, we need to spread some activation to our neighbours (see pp.
  // 27 -- 29 of 'DUAL Architecture').  Normally, this would involve going
  // through /all/ of the slots and references, finding the maximal link
  // weight between this agent and each of its neighbours, normalising these
  // link weights, and then spreading the activation.  As the normalised
  // link weights are pre-computed in Agent::initialise(), all we need to do
  // here is the spreading.
  // Note that we're sending the messages /directly/, as the spreading of
  // activation is something that happens every tick, regardless of the 
  // agents' relative levels.
  if (m_activation > 0.0)
    for (m_tick__agentIter = m_agentPointerToNormalisedLinkWeightMap.begin(); m_tick__agentIter != m_agentPointerToNormalisedLinkWeightMap.end(); m_tick__agentIter++) {
      m_tick__activationMessage->activation = m_activation * m_tick__agentIter->second;
      m_tick__agentIter->first->receive((Message::BaseMessage *) m_tick__activationMessage);
    }

  
  // If our activation is not fixed, calculate what our new level of act-
  // ivation should be, and then clamp it to within our minimum and maxi-
  // mum limits.
  if (m_activationFixed == false) {
    if ((m_activation == 0.0) && (m_incomingActivation >= Agent::ActivationMinimumIncoming))
      m_activation = ActivationThreshold + (ActivationExcitationRate * m_incomingActivation * ActivationMaximum) * TimeQuantum;
    else
      m_activation += (ActivationExcitationRate * m_incomingActivation * (ActivationMaximum - m_activation) - ActivationDecayRate * m_activation) * TimeQuantum;
    
    if (m_activation > Agent::ActivationMaximum)
      m_activation = Agent::ActivationMaximum;
    if (m_activation < Agent::ActivationThreshold)
      m_activation = 0.0;
  }
  if (m_activation > 0.0) 
    AMBR::logger->log(__FILE__, __LINE__, Logger::VERBOSE, AMBRLogger::ACTIVATION,
                      "%s's activation at time %d is %f", name.c_str(), time(), m_activation);
  
  // Either way, we need to reset our incoming activation 'buffer'
  m_incomingActivation = 0.0;
}




//  =======================================================================
/// Handles an incoming Activation message
///
/// This method is called when an agent receives a packet of activation
/// from one of its neighbours.  All it does is update the agent's incoming
/// activation --- the actual calculations (to determine if the agent is
/// active, and to generate outgoing activation messages) is done in 
/// Agent::tick().
///
/// @param activation the activation message
///
/// @note DUAL's specification calls for a continuous/asynchronous exchange
///       of activation.  This implementation (like AMBR2, see DUAL/archit/
///       spread.lsp:33) approximates the continuous exchange by a sequence
///       of discrete cycles.  From spread.lsp, "During each cycle, the
///       activation levels of all agents are updated in parallel" --- i.e.
///       synchronously.  Also note that derived classes need to call
///       Agent::receive() in their own implementation of receive().
/// @note As [message] will be deleted by the calling agent, this method,
///       or the derived classes' implementation of postReceive() will
///       need to make its own copy if it needs to hold on to the infor-
///       mation outside of this method.
//  =======================================================================
bool Agent::receive(VariableRateExecution::Message::BaseMessage *incoming) {
  Message::BaseMessage *message = (Message::BaseMessage *) incoming;

  switch(message->type) {
    case Message::ActivationMessage:
      m_incomingActivation += ((Message::Activation *) message)->activation;
      AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, 
                        AMBRLogger::ACTIVATION_RECEIVED, 
                        "%s received %f units of activation at time %d", name.c_str(), ((Message::Activation *) message)->activation, time());
      break;
      
    case Message::MarkerMessage:
      if (m_activation >= ActivationThreshold) {
      //if (1) {
        unsigned i;
        m_incomingMarkers.push_back(new Message::Marker((Message::Marker *) message));

        AMBR::logger->log(__FILE__, __LINE__, Logger::INFORMATIVE, 
                          AMBRLogger::MARKER_RECEIVED, 
                          "%s received a marker from agent %s:%d at time %d", name.c_str(), ((Message::Marker *) message)->path[0]->name.c_str(), ((Message::Marker *) message)->serial, time());
        printf("  ");
        for (i = 0; i < ((Message::Marker *) message)->path.size() - 1; i++)
          printf("%s -> ", ((Message::Marker *) message)->path[i]->name.c_str());
        printf("%s\n", ((Message::Marker *) message)->path[i]->name.c_str());

      }
      break;
      
      default:
      // We don't know how to handle this message --- pass it to the derived
      // class' handler
      postReceive(message);
  }
  
  return true;
}




//  =======================================================================
/// A static function used to sort STL containers of AgentReferences
///
/// This static method is used by the STL sort(...) function to sort (nom-
/// inally) vectors of AgentReferences, in descending order, by the activ-
/// ation of the referenced agent.
///
/// @param a the first AgentReference
/// @param b the second AgentReference
///
/// @retval true if the agent pointed to by [a] is more active than [b]
//  =======================================================================
inline bool Agent::sortAgentReferencesByActivation(const AgentReference *a, const AgentReference *b) {
  return (a->agent->m_activation > b->agent->m_activation);
}




//  =======================================================================
/// A static function used to sort STL containers of SlotReferences
///
/// This static method is used by the STL sort(...) function to sort (nom-
/// inally) vectors of SlotReferences, in descending order, by the activ-
/// ation of the referenced agent.
///
/// @param a the first SlotReference
/// @param b the second SlotReference
///
/// @retval true if the agent pointed to by [a] is more active than [b]
//  =======================================================================
bool Agent::sortSlotReferencesByActivation(const SlotReference *a, const SlotReference *b) {
  return (a->slot->agent->m_activation > b->slot->agent->m_activation);
}




//  =======================================================================
/// A static function used to sort STL containers of Slots
///
/// This static method is used by the STL sort(...) function to sort (nom-
/// inally) vectors of Slots, in descending order, by the activation of the
/// referenced agent.
///
/// @param a the first Slot
/// @param b the second Slot
///
/// @retval true if the agent pointed to by [a] is more active than [b]
//  =======================================================================
inline bool Agent::sortSlotsByActivation(const Slot *a, const Slot *b) {
  return (a->agent->m_activation > b->agent->m_activation);
}
