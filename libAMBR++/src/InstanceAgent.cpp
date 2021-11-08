#include "InstanceAgent.hpp"


using namespace AMBR;
using namespace std;




//  =======================================================================
/// The constructor
///
/// This constructor creates an AMBR Instance micro-agent and sets its 
/// name, type and comment.
///
/// @param name    the agent's mnemonic name
/// @param type    the agent's conceptual type
/// @param comment a description of the agent
/// =======================================================================
InstanceAgent::InstanceAgent(string name, AgentType type, string comment) : Agent(name, type, comment) {
  m_becameActive = false;
  
  m_agentReferenceVectors.push_back(&instanceOf);
}




//  =======================================================================
/// The destructor
///
/// This is the class' destructor.
//  =======================================================================
InstanceAgent::~InstanceAgent() {
  unsigned i;
  
  for (i = 0; i < instanceOf.size(); i++)
    delete instanceOf[i];
}




//  =======================================================================
/// Adds an conceptual class
///
/// This method adds the named Concept Agent to the current agent's 
/// instance-of list.
///
/// @param agentName the name of the concept agent that the current agent
///                  instantiates
/// @param weight    the strength of the inheritence;  if no weight is 
///                  specified then the default of 1.0 is used
///
/// @retval the index of the slot reference into InstanceAgent#instanceOf
///
/// @throw Exception::InvalidAgent if [agentName] is not a valid agent name
//  =======================================================================
unsigned InstanceAgent::addInstanceOf(string agentName, double weight) throw (Exception::InvalidAgent) {
  AgentReference *temp;
  
  if (agentName.find('.') == string::npos) {
    for (unsigned i = 0; i < instanceOf.size(); i++)
      if (instanceOf[i]->name == agentName) {
        Exception::InvalidAgent e(agentName, "instanceOf already added");
        throw e;
      }
    
    temp = new AgentReference(agentName, weight);
    
    // if [agentName] could not be resolved then add it to instanceOf and
    // return; Otherwise, make sure that the agent it points to is a ConceptAgent;
    // if it is, then add it and return, otherwise throw an exception
    if (temp->agent == NULL) {
      instanceOf.push_back(temp);
      Agent::AgentsWithUnresolvedReferences[this] = 1;
      return 1;
    } else if (dynamic_cast<ConceptAgent*>(temp->agent)) {
      instanceOf.push_back(temp);
      AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::REFERENCE,
                        "Conceptual class '%s' added to '%s' and resolved (%f)",
                        agentName.c_str(), this->name.c_str(), weight);
      
      return 0;
    } else {
      Exception::InvalidAgent e(agentName, "only a ConceptAgent may be added to instanceOf");
      throw e;
    }
  } else {
    Exception::InvalidAgent e(agentName, "Not a valid agent name");
    throw e;
  }
}




//  =======================================================================
/// Prints the InstanceAgent's information
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
int InstanceAgent::print(FILE *ostream, const char *prefix) {
  char *subPrefix = new char[strlen(prefix) + 3];
  int count = 0;
  unsigned i;
  
  count += fprintf(ostream, "%s(defagent  %s  instance-agent \"%s\"\n", prefix, this->name.c_str(), this->comment.c_str());
  count += fprintf(ostream, "%s  :type        (:instance %s)\n", prefix, 
                   (this->type == Agent::ObjectAgent)?":object":(this->type == Agent::RelationAgent)?":relation":":situation");
  
  if (instanceOf.size()) {
    count += fprintf(ostream, "%s  :instanceOf  (", prefix);
    for (i = 0; i < instanceOf.size(); i++)
      count += fprintf(ostream, " (%s, %f)", instanceOf[i]->name.c_str(), instanceOf[i]->weight);
    count += fprintf(ostream, " )\n");
  }
  
  if (coreferences.size()) {
    count += fprintf(ostream, "%s  :coreference (", prefix);
    for (i = 0; i < coreferences.size(); i++)
      count += fprintf(ostream, " (%s, %f)", coreferences[i]->name.c_str(), coreferences[i]->weight);
    count += fprintf(ostream, " )\n");
  }
  
  if (associations.size()) {
    count += fprintf(ostream, "%s  :association (", prefix);
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
bool InstanceAgent::postParse(FILE *istream) throw (Exception::InvalidAgent, Exception::InvalidSlot) {
  char line[AMBR::MaxLineLength], tag[32], value[AMBR::MaxLineLength], *temp_s, *save;
  float weight;
  
  if (fgets(line, AMBR::MaxLineLength, istream) && strtok_r(line, ":", &save) && (temp_s = strtok_r(NULL, ":(\"", &save))) {
    sscanf(temp_s, "%s", tag);
    
    if (!strcmp("instanceOf", tag)) {
      strtok_r(NULL, "(", &save);
      while ((temp_s = strtok_r(NULL, "(", &save))) {
        sscanf(temp_s, "%s %f", value, &weight);
        value[strlen(value) - 1] = 0;
        this->addInstanceOf(value, (double) weight);
      }
      
    } else
      return false;
  }
  
  return true;
}




//  =======================================================================
/// Performs the InstanceAgent-specific pre-initialisation
///
/// This method checks the types of all linked-to agents
//  =======================================================================
bool InstanceAgent::preInitialise() throw (Exception::SlotNotFound, Exception::AgentNotFound, Exception::InvalidAgent) {
  unsigned i;
  Agent *agent;
  
  /// At the moment, we're remove the first invalidly-typed agent from
  /// the instanceOf vector just prior to throwing an error.  While this
  /// behaviour makes unit testing easier, it probably isn't necessary as
  /// this would indicate a fairly serious problem with the input data.
  try {
    for (i = 0; i < instanceOf.size(); i++)
      if ((agent = instanceOf[i]->agent))
        if (dynamic_cast<ConceptAgent*>(agent) == NULL) {
          // We've just found an InstanceAgent in ::superClasses --- remove
          // it and throw an exception
          instanceOf.erase(instanceOf.begin() + i);
          Exception::InvalidAgent e(instanceOf[i]->agent->name, "an instanceOf resolved to something other than a ConceptAgent");
          throw e;
        }
  } catch (...) {
    throw;
  }
  
  return true;
}




//  =======================================================================
/// Called by AMBR::Agent::activate() whenever the agent becomes active
///
/// This virtual hook method is called by AMBR::Agent::activate() whenever
/// its level of activation goes from below, to above the activation-
/// threshold;  this is where sub-class-specific activation tasks should
/// be implemented.
///
/// @retval true if something was done; false otherwise
//  =======================================================================
bool InstanceAgent::postActivate() {
  m_becameActive = true;
  
  return true;
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
unsigned InstanceAgent::nopsThisTick() {
  if (m_becameActive)
    return instanceOf.size();
  else
    return 0;
}




//  =======================================================================
/// The hook method that is called to perform sub-class agent operations
///
/// This virtual method is called by tick() to perform agent-specific
/// operations (such as marker passing or hypothesis formation);  it is
/// called after the activation has been calculated and spread, and after
/// the markers have been moved from their input zone to the main buffer.
///
/// @retval the number of activation quanta used
//  =======================================================================
unsigned InstanceAgent::postTick() {
  if (m_becameActive) {
    for (m_tick__i = 0; m_tick__i < instanceOf.size(); m_tick__i++)
      m_manager->sendMessage(this, instanceOf[m_tick__i]->agent, (Message::BaseMessage *) (new Message::Marker(this, time())));
    m_becameActive = false;
  }
  
  return instanceOf.size();
}




//  =======================================================================
/// The hook method that is called to handle unknown message types
///
/// This virtual method is called by Agent::receive() to handle any message
/// types that it does not know about.
///
/// @retval true if something was done, false otherwise
//  =======================================================================
bool InstanceAgent::postReceive(Message::BaseMessage *message) {
  // If we've received a hypothesis request, check to see if we already
  // have a hypothesis that relates 
  if (message->type == Message::HypothesisRequestMessage) {
    bool flag = false;
    Message::HypothesisRequest *request = (Message::HypothesisRequest *) message;
    for (m_tick__i = 0; m_tick__i < hypotheses.size(); m_tick__i++)
      if (hypotheses[m_tick__i]->agent == request) {
        flag = true;
        break;
      }

    if (flag) {
      vector<AgentReference*> references;
      references.push_back(hypotheses[m_tick__i]);
      m_manager->sendMessage(this, request->hypothesis, (VariableRateExecution::Message::BaseMessage *) new HypothesisResponse(HypothesisResponse::Resign, references));
      
    } else {
      AgentReference *reference = new AgentReference(request->hypotheses, 1.0);
      m_manager->sendMessage(this, request->hypothesis, (VariableRateExecution::Message::BaseMessage *) new HypothesisResponse(HypothesisResponse::Establish, hypotheses));
      
    }
    
    return true;
  } else
    return false;
}
