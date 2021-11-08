// ============================================================================
// Filename          : $RCSfile: Agent.hpp,v $
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
// Purpose           : This header file defines the interface of the DUAL and
//                     AMBR micro-agent.
//
// Linker options    : 
//
// Revision history  : 
// ---------------------------------------------------------------------------
// Notes:
//   While I've tried to follow Kokinov and Petrov's terminology, I have re-
//   named their generic slots --- partly because of C/C++'s language restric-
//   tions, partly to make them a bit more intuitive.  The mapping is:
//
//       Mine              DUAL/AMBR
//       superclass        :subc
//       subclass          :superc
//       instance          :instance
//       instanceOf        :inst-of
//       association       :a-link (later a-coref)
//       coreference       :c-coref
//       conceptualType    :type
//       situation         :situation
// ============================================================================
#ifndef libAMBR__Agent_hpp
#define libAMBR__Agent_hpp

//#include <stdlib.h>
#include <algorithm>

#include "VariableRateExecution/Agent.hpp"

#include "AMBR.hpp"
#include "Reference.hpp"
#include "Slot.hpp"


namespace AMBR {


  // Forward declarations
  class Slot;
  class Reference;
  class AgentReference;
  class SlotReference;
  

  namespace Test {
    class AgentTest;
    class ConceptAgentTest;
    class InstanceAgentTest;
  }
  



  //  =========================================================================
  /// @class    Agent
  /// @brief    The base class for all DUAL and AMBR micro-agents.
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  =========================================================================
  class Agent : public VariableRateExecution::Agent {
  public:
    /// An enumeration of the conceptual agent types
    enum AgentType {
      ObjectAgent,     /// the agent represents a physical or an abstract object
      RelationAgent,   /// the agent represents a relation
      SituationAgent,  /// the agent represents a specific situation
      HypothesisAgent  /// the agent represents a hypothesis
    };


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
    Agent(std::string name, AgentType type, std::string comment);


    //  =======================================================================
    /// The destructor
    ///
    /// This is the class' destructor.
    //  =======================================================================
    virtual ~Agent();


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
    unsigned addCoreference(std::string slotName, double weight) throw (Exception::InvalidSlot);


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
    unsigned addAssociation(std::string agentName, double weight) throw (Exception::InvalidAgent);


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
    unsigned addSlot(Slot *slot) throw (Exception::InvalidSlot);


    //  =======================================================================
    /// Returns the named slot.
    ///
    /// This method returns a pointer to the named slot.
    ///
    /// @param name the name of the desired slot
    ///
    /// @retval a pointer to the specific-slot, or NULL if no such slot exists.
    //  =======================================================================
    Slot* getSlot(std::string name);


    //  =======================================================================
    /// Sets and fixes the agent's activation
    ///
    /// This method sets the agent's activation to the value specified, and
    /// prevents it from decaying during subsequent ticks.  It can be used to
    /// represent percepts, or during testing
    ///
    /// @param activation the level of activation
    //  =======================================================================
    void lockActivation(double activation);
    
    //  =======================================================================
    /// Unlocks the agent's activation
    ///
    /// When called on a previously-locked agent, this method will allow its
    /// level of activation to decay, once again, during subsequent ticks.
    //  =======================================================================
    void unlockActivation();


    //  =======================================================================
    /// Initialises the agent
    ///
    /// This method resolves all of the slot and agent references, and 
    /// normalises the link weights.
    //  =======================================================================
    void initialise() throw (Exception::SlotNotFound, Exception::AgentNotFound, Exception::InvalidAgent);

    
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
    virtual int print(FILE *ostream, const char *prefix);
    
    
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
    virtual int parse(FILE *istream) throw (Exception::InvalidAgent, Exception::InvalidSlot);
    
    
    AgentType   type;      /// the agent's conceptual type
    std::string comment;   /// a description of the agent

    std::vector<SlotReference*>  coreferences;  /// a list of the agent's coreferences
    std::vector<AgentReference*> associations;  /// a list of the agent's associations
    std::vector<Slot*>           slots;         /// a list of the agent's slots


  protected:
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
    bool tick();


    //  =======================================================================
    /// Determines what the agent's new level of activation should be
    ///
    /// This method is called by the VariableRateExecution::Manager, after
    /// every iteration, to update m_activation;  depending on the nature of 
    /// the derived class, the level may be related to the total amount of
    /// /incoming/ activation, or the /fitness/ of the agent's algorithm or 
    /// parameter values.
    //  =======================================================================
    virtual void updateActivation();
    
    
    //  =======================================================================
    /// Called whenever the agent becomes active
    ///
    /// This virtual hook method is called by the agent's manager whenever its
    /// level of activation goes from below, to above the activation-threshold.
    ///
    /// @retval true if something was done; false otherwise
    //  =======================================================================
    bool activate();

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
    virtual bool postActivate() { return false; }


    //  =======================================================================
    /// Called whenever the agent becomes active
    ///
    /// This virtual hook method is called by the agent's manager whenever its
    /// level of activation goes from below, to above the activation-threshold.
    ///
    /// @retval true if something was done; false otherwise
    //  =======================================================================
    bool deactivate();

    //  =======================================================================
    /// Called by AMBR::Agent::deactivate() whenever the agent becomes inactive
    ///
    /// This virtual hook method is called by AMBR::Agent::deactivate() when-
    /// ever its level of activation goes from above, to below the activation-
    /// threshold;  this is where sub-class-specific deactivation tasks should
    /// be implemented.
    ///
    /// @retval true if something was done; false otherwise
    //  =======================================================================
    virtual bool preDeactivate() { return false; }


    //  =======================================================================
    /// Handles an incoming message
    ///
    /// This method is called by Agent::tick(), and forms the other half of the
    /// agents' message-passing framework.  If the message is one that Agent
    /// knows about, it will process it;  otherwise it will pass it on to
    /// postReceive().  The method is private to ensure that messages can't be
    /// sent immediately --- rather, the derived classes place messages into 
    /// the outgoing message queue by calling Agent::send(...).
    ///
    /// @param message the message itself
    //  =======================================================================
    virtual bool receive(VariableRateExecution::Message::BaseMessage *message);


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
    virtual unsigned nopsThisTick() { return 0; }


    //  =======================================================================
    /// The hook method that parses sub-class specific tags
    ///
    /// This virtual method is called by Agent::parse() upon encountering an
    /// unrecognised tag, but before the tag is treated as an s-slot.
    ///
    /// @retval true if something was done, false otherwise
    //  =======================================================================
    virtual bool postParse(FILE *istream) throw (Exception::InvalidAgent, Exception::InvalidSlot) { return 0; }


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
    virtual bool preInitialise() throw (Exception::SlotNotFound, Exception::AgentNotFound, Exception::InvalidAgent) { return 0; }


    //  =======================================================================
    /// The hook method that performs sub-class specific post-initialisation
    /// tasks
    ///
    /// This virtual method is called by Agent::initialise() after all other
    /// general initialisation tasks have been performed.
    ///
    /// @retval true if something was done, false otherwise
    //  =======================================================================
    virtual bool postInitialise() throw (Exception::SlotNotFound, Exception::AgentNotFound, Exception::InvalidAgent) { return 0; }


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
    virtual unsigned postTick() { return false; }
    

    //  =======================================================================
    /// The hook method that is called to handle unknown message types
    ///
    /// This virtual method is called by Agent::receive() to handle any message
    /// types that it does not know about.
    ///
    /// @retval true if something was done, false otherwise
    //  =======================================================================
    virtual bool postReceive(Message::BaseMessage *message) { return false; }
    
    
    bool     m_activationFixed;   /// is this agent's activation fixed or subject to change over time?

    /// A map between the slot names and pointers to the objects that represent them
    std::map<std::string, Slot*> m_slotNameToPointerMap;
    
    // = Activation-related member variables ==================================
    /// The total activation that has been received since the last tick
    double m_incomingActivation;
    /// A list of the agent's references (including coreferences, associations
    /// and other references defined in a derived class)
    std::map<Agent*, double> m_agentPointerToLinkWeightMap;
    /// A normalised version of [agentPointerToLinkWeightMap]
    std::map<Agent*, double> m_agentPointerToNormalisedLinkWeightMap;
    
    // = Marker-passing-related member variables ==============================
    /// A list of the markers that have been received since the last kick
    std::vector<Message::Marker*>      m_incomingMarkers;
    /// A map of the markers received since the last intersection
    std::map<Agent*, Message::Marker*> m_originatingAgentToMarkerMap;

    // = ::tick()-specific member variables ===================================
    // Variables beginning with 'm_tick__' are used in lieu of C/C++'s support for
    // static, but per-class-instance, variables (i.e. it avoids the need for
    // constant allocations and de-allocations)
    unsigned                           m_tick__i, m_tick__j;        /// generic iterators
    unsigned                           m_tick__nopsRemaining;       /// maximum number of operations left this tick
    Message::Activation               *m_tick__activationMessage;   /// pointer to a 'temporary' activation message
    Message::Marker                   *m_tick__markerMessage;       /// pointer to a 'temporary' marker message
    std::map<Agent*, double>::iterator m_tick__agentIter;           /// an iterator used to go through the agentPointerTo*LinkWeightMaps
    
    // = Housekeeping =========================================================
    /// A vector of pointers to the AgentReference vectors used by Agent and
    /// its derived classes --- used by initialise (to resolve /all/ of the
    /// references) and tick (to sort, if necessary, the references by the
    /// activation of the referenced agent)
    std::vector<std::vector<AgentReference*>*> m_agentReferenceVectors;
    /// A vector of pointers to the SlotReference vectors used by Agent and
    /// its derived classes
    std::vector<std::vector<SlotReference*>*> m_slotReferenceVectors;
    
    
  private:
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
    static inline bool sortAgentReferencesByActivation(const AgentReference *a, const AgentReference *b);

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
    static inline bool sortSlotReferencesByActivation(const SlotReference *a, const SlotReference *b);

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
    static inline bool sortSlotsByActivation(const Slot *a, const Slot *b);

    
  public:
    /// A static map between the agents' names and their pointers;  queries to
    /// this map should only be made via the std::map::find(...) method.
    static std::map<std::string, Agent*> AgentNameToPointerMap;
    
    /// A static map (that is essentially an unsorted std::set) containing
    /// pointers to those agents that have unresolved inter-agent references
    static std::map<Agent*, unsigned> AgentsWithUnresolvedReferences;
    
    /// A static map (that is essentially an unsorted std::set) containing
    /// pointers to those agents that are in the working memory set (i.e.
    /// whose level of activation is equal to, or exceeds, the threshold
    static std::map<Agent*, unsigned> AgentsInWorkingMemory;
    
    
    /// The activation threshold below which an agent becomes inactive
    static const double   ActivationMaximum;
    static const double   ActivationMinimum;
    static const double   ActivationDecayRate;
    static const double   ActivationExcitationRate;
    static const double   ActivationThreshold;
    static const double   ActivationMinimumIncoming;
    static const double   ActivationQuantum;
    static const unsigned AgentCreationCost;
    static const double   TimeQuantum;


    // Friend-class declarations for unit testing
    friend class Test::AgentTest;
    friend class Test::ConceptAgentTest;
    friend class Test::InstanceAgentTest;
  };


} // namespace AMBR

#endif
