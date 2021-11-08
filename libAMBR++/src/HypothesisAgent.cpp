#include "HypothesisAgent.hpp"

using namespace std;
using namespace AMBR;


//  =======================================================================
/// The constructor
///
/// This constructor creates an AMBR Instance micro-agent and sets its 
/// name, type and comment.
///
/// @param name          the agent's mnemonic name
/// @param first         the first of the pair of 'analogous' agents
/// @param second        the second of the 'analogous' agents
/// @param justification the justifying agent that created the hypothesis
/// @param comment       a description of the agent
/// =======================================================================
HypothesisAgent::HypothesisAgent(string name, InstanceAgent *first, InstanceAgent *second, ConceptAgent *justification, string comment) : Agent(name, Agent::HypothesisAgent, comment) {
  this->first = first;
  this->second = second;
  justifications.push_back(new AgentReference(justification, 1.0));
  state = HypothesisAgent::Embryo;
  
  m_agentReferenceVectors.push_back(&relatedHypotheses);
  // justifications are not used during the spreading of activation
  
  // Next, we need to send each of our justifying agents a hypothesis 
  // request;  depending on the response (which will be handled in post-
  // Receive()), we will either mature or fizzle.
  m_manager->sendMessage(this, first,  (VariableRateExecution::Message::BaseMessage *) new Message::HypothesisRequest(this, first,  second));
  m_manager->sendMessage(this, second, (VariableRateExecution::Message::BaseMessage *) new Message::HypothesisRequest(this, second, first));
}




//  =======================================================================
/// The destructor
///
/// This is the class' destructor.
//  =======================================================================
HypothesisAgent::~HypothesisAgent() {
}




//  =======================================================================
/// Adds a justification
///
/// This method adds the given agent to the current hypothesis agent's list
/// justifications.
///
/// @param agent  pointer to an agent that justifies this hypothesis
/// @param weight the strength of the justification;  note that the weight
///               may be positive or negative
///
/// @retval zero if the agent was resolved; one otherwise
//  =======================================================================
unsigned HypothesisAgent::addJustification(Agent *agent, double weight) {
  return 0;
}




//  =======================================================================
/// Adds a related hypothesis
///
/// This method adds the specified agent to the current agent's list 
/// related hypotheses.
///
/// @param agent  pointer to a related hypothesis agent
/// @param weight the strength of the relation;  note that the weight may
///               be positive or negative
///
/// @retval zero if the agent was resolved; one otherwise
//  =======================================================================
unsigned HypothesisAgent::addRelatedHypothesis(HypothesisAgent *agent, double weight) { return 0; }




//  =======================================================================
/// Prints the HypothesisAgent's information
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
int HypothesisAgent::print(FILE *ostream, const char *prefix) { return 0; }




//  =======================================================================
/// The hook method that parses sub-class specific tags
///
/// This virtual method is called by Agent::parse() upon encountering an
/// unrecognised tag, but before the tag is treated as an s-slot.
///
/// @retval true if something was done, false otherwise
//  =======================================================================
bool HypothesisAgent::postParse(FILE *istream) throw (Exception::InvalidAgent, Exception::InvalidSlot) { return false; }




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
bool HypothesisAgent::preInitialise() throw (Exception::SlotNotFound, Exception::AgentNotFound, Exception::InvalidAgent) { return false; }




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
unsigned HypothesisAgent::nopsThisTick() { return 0; }




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
unsigned HypothesisAgent::postTick() { return 0; }
