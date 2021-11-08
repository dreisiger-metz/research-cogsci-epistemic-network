// ============================================================================
// Filename          : $RCSfile: ConceptAgent.hpp,v $
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
// Purpose           : This header file defines the interface of the Concept
//                     AMBR micro-agent.
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libAMBR__ConceptAgent_hpp
#define libAMBR__ConceptAgent_hpp

#include "Agent.hpp"
#include "HypothesisAgent.hpp"
#include "InstanceAgent.hpp"


namespace AMBR {


  //  =========================================================================
  /// @class    ConceptAgent
  /// @brief    The class that implements the AMBR concept agents.
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  =========================================================================
  class ConceptAgent : public Agent {
  public:

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
    ConceptAgent(std::string name, AgentType type, std::string comment);


    //  =======================================================================
    /// The destructor
    ///
    /// This is the class' destructor.
    //  =======================================================================
    virtual ~ConceptAgent();


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
    unsigned addSuperClass(std::string agentName, double weight) throw (Exception::InvalidAgent);


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
    unsigned addSubClass(std::string agentName, double weight) throw (Exception::InvalidAgent);


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
    unsigned addInstance(std::string agentName, double weight) throw (Exception::InvalidAgent);


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
    //  =======================================================================
    int print(FILE *ostream, const char *prefix);


    std::vector<AgentReference*> superClasses;  /// a list of the agent's superclasses
    std::vector<AgentReference*> subClasses;    /// a list of the agent's subclasses
    std::vector<AgentReference*> instances;     /// a list of the agent's instances
    
    
  protected:
    //  =======================================================================
    /// The hook method that parses sub-class specific tags
    ///
    /// This virtual method is called by Agent::parse() upon encountering an
    /// unrecognised tag, but before the tag is treated as an s-slot.
    ///
    /// @retval true if something was done, false otherwise
    //  =======================================================================
    bool postParse(FILE *istream) throw (Exception::InvalidAgent, Exception::InvalidSlot);


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
    bool preInitialise() throw (Exception::SlotNotFound, Exception::AgentNotFound, Exception::InvalidAgent);


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
    unsigned nopsThisTick();


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
    unsigned postTick();


    //  =======================================================================
    /// The hook method that is called to handle unknown message types
    ///
    /// This virtual method is called by Agent::receive() to handle any message
    /// types that it does not know about.
    ///
    /// @retval true if something was done, false otherwise
    //  =======================================================================
    bool postReceive(Message::BaseMessage *message);
  };


} // namespace AMBR

#endif
