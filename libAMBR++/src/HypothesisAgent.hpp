// ============================================================================
// Filename          : $RCSfile: HypothesisAgent.hpp,v $
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
// Purpose           : This header file defines the interface of the Instance
//                     AMBR micro-agent.
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libAMBR__HypothesisAgent_hpp
#define libAMBR__HypothesisAgent_hpp

#include "Agent.hpp"
#include "ConceptAgent.hpp"
//#include "Messages.hpp"


namespace AMBR {
  
  //  =========================================================================
  /// @class    HypothesisAgent
  /// @brief    The class that implements the AMBR instance agents.
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  =========================================================================
  class HypothesisAgent : public Agent {
  public:
    /// An enumeration of the hypothesis agents' states
    enum HypothesisState {
      Embryo,
      Mature,
      Winner
    };
    
    
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
    HypothesisAgent(std::string name, InstanceAgent *first, InstanceAgent *second, ConceptAgent *justification, std::string comment);
    
    
    //  =======================================================================
    /// The destructor
    ///
    /// This is the class' destructor.
    //  =======================================================================
    virtual ~HypothesisAgent();
    
    
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
    unsigned addJustification(Agent *agent, double weight);
    
    
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
    unsigned addRelatedHypothesis(HypothesisAgent *agent, double weight);
    
    
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
    int print(FILE *ostream, const char *prefix);
    
    
    HypothesisState              state;              /// the state of the agent (from its lifecycle)
    InstanceAgent               *first;              /// the first of the pair of 'analogous' agents
    InstanceAgent               *second;             /// the second of the analogous agents
    Agent                       *situation;          /// the driving situation 
    
    std::vector<AgentReference*> relatedHypotheses;  /// a list of supporting or conflicting hypotheses
    std::vector<AgentReference*> justifications;     /// a list of the agent's justifications
    
    
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
  };
  
  
} // namespace AMBR

#endif
