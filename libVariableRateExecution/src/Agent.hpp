// ============================================================================
// Filename          : $RCSfile: Agent.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 30-Oct-2008
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 05:05:54 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines the interface of the Variable-
//                     rate-of-Execution agent
//
// Revision history  :
// ============================================================================
#ifndef libVariableRateExecution__Agent_hpp
#define libVariableRateExecution__Agent_hpp

#include "VariableRateExecution.hpp"
#include "Manager.hpp"
#include "Message.hpp"


namespace VariableRateExecution {

  // Forward declaration
  class Manager;
  
  namespace Test {
    class AgentTest;
    class ManagerTest;
  }



  class Agent {
  public:
    //  =======================================================================
    /// The constructor
    ///
    /// This constructor creates a generic variable-rate-of-execution agent
    /// and sets its initial level of activation.
    ///
    /// @param activation the agent's initial level of activation
    /// @param manager    a pointer to the responsible manager
    //  =======================================================================
    Agent(std::string name, double activation, Manager *manager);


    //  =======================================================================
    /// The destructor
    ///
    /// This is the class' destructor.
    //  =======================================================================
    virtual ~Agent();
    

    //  =======================================================================
    /// Returns the current system time
    ///
    /// This method returns the number of ticks since the agent's manager's
    /// epoch.
    ///
    /// @retval the current system time
    //  =======================================================================
    unsigned time();


    //  =======================================================================
    /// Returns the agent's level of activation
    ///
    /// @retval the agent's level of activation
    //  =======================================================================
    inline double activation() { return m_activation; }


    //  =======================================================================
    /// Returns the agent's priority level
    ///
    /// This method returns the agent's current priority level as set by its
    /// manager.
    ///
    /// @retval the current priority level
    //  =======================================================================
    inline unsigned priorityLevel() { return p_priorityLevel; }


    //  =======================================================================
    /// Returns the agent's tick count
    ///
    /// This method returns the number of times that the agent's tick() method
    /// has been called by its manager.
    ///
    /// @retval the current priority level
    //  =======================================================================
    inline unsigned totalNumberOfTicks() { return p_totalNumberOfTicks; }


    /// The agent's name
    std::string name;


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
    virtual bool tick() = 0;


    //  =======================================================================
    /// Determines what the agent's new level of activation should be
    ///
    /// This method is called by the VariableRateExecution::Manager, after
    /// every iteration, to update m_activation;  depending on the nature of 
    /// the derived class, the level may be related to the total amount of
    /// /incoming/ activation, or the /fitness/ of the agent's algorithm or 
    /// parameter values.
    //  =======================================================================
    virtual void updateActivation() = 0;


    //  =======================================================================
    /// Called whenever the agent becomes active
    ///
    /// This virtual hook method is called by the agent's manager whenever its
    /// level of activation goes from below, to above the activation-threshold.
    ///
    /// @retval true if something was done; false otherwise
    //  =======================================================================
    virtual bool activate() { return false; }


    //  =======================================================================
    /// Called whenever the agent becomes active
    ///
    /// This virtual hook method is called by the agent's manager whenever its
    /// level of activation goes from below, to above the activation-threshold.
    ///
    /// @retval true if something was done; false otherwise
    //  =======================================================================
    virtual bool deactivate() { return false; }


    //  =======================================================================
    /// Called to handle an incoming message
    ///
    /// This virtual hook method is called by the agent's manager to handle an
    /// incoming message.
    ///
    /// @param message the incoming message
    ///
    /// @retval true if something was done; false otherwise
    ///
    /// @note the receiving agent does /not/ own the message
    //  =======================================================================
    virtual bool receive(Message::BaseMessage *message) { return false; }
    
    
    //  =======================================================================
    /// Determines if we have enough energy to perform an atomic operation
    ///
    /// This method checks to see if we have can perform an atomic operation;
    /// if we can, it also decrements our number-of-operations-remaining 
    /// counter.
    ///
    /// @param the number of operations we wish to perform
    ///
    /// @retval true if we can and false if we can't
    //  =======================================================================
    bool consumeActivation();
    bool consumeActivation(unsigned nops);


    //  =======================================================================
    /// Returns the number of operations that we can perform during the 
    /// remainder of this tick
    ///
    /// @retval the number of operations remaining
    //  =======================================================================
    inline unsigned nopsRemaining() { return p_nopsRemaining; }


    /// The agent's current level of activation
    double m_activation;

    /// The manager responsible for this agent
    Manager *m_manager;


  private:
    /// A flag indicating whether the agent's level of activation was over
    /// the threshold last time
    bool p_activeLastTick;

    /// The maximum number of operations that the agent can perform /this/
    /// tick, given its current level of activation.
    unsigned p_totalNOPSThisTick;

    /// The number of (remaining) operations that can be performed by the
    /// agent this tick
    unsigned p_nopsRemaining;

    /// The agent's current priority level
    unsigned p_priorityLevel;

    /// The total number of times that this->tick() has been called
    unsigned p_totalNumberOfTicks;
    

    friend class VariableRateExecution::Manager;
    friend class VariableRateExecution::Test::AgentTest;
    friend class VariableRateExecution::Test::ManagerTest;
  };

}

#endif

