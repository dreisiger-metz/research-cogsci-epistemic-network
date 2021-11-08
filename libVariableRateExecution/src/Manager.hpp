// ============================================================================
// Filename          : $RCSfile: Manager.hpp,v $
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
// Purpose           : This header file defines the interface of the agents'
//                     execution Manager
//
// Revision history  :
// ============================================================================
#ifndef libVariableRateExecution__Manager_hpp
#define libVariableRateExecution__Manager_hpp

#include <pthread.h>

#include "VariableRateExecution.hpp"
#include "Agent.hpp"
#include "ManagerConsole.hpp"
#include "Message.hpp"


namespace VariableRateExecution {
  
  // Forward declaration
  class Agent;
  class Manager;
  class ManagerConsole;
  
  namespace Message {
    class BaseMessage;
    class Envelope;
  }
  
  namespace Test {
    class ManagerTest;
  }
  
  
  class Callback {
  public:
    Callback(Agent *agent, void (*callback)(Agent *)) {
      this->agent = agent;
      this->callback = callback;
    }
    
    Agent *agent;
    void (*callback)(Agent *);
  };
  
  
  class Manager {
  public:
    //  =======================================================================
    /// The constructor
    ///
    /// This constructor creates a manager that oversees the execution and 
    /// activation of its agents
    ///
    /// @param priorityLevels the number of priority queues, or discrete rates
    ///            of execution to maintain
    /// @param activationQuantum the smallest unit of activation that an agent
    ///            may consume
    /// @param minimumActivation minimum possible level of activation
    /// @param maximumActivation maximum possible level of activation
    /// @param activationThreshold the minimum level of activation than an 
    ///            /active/ agent can have
    //  =======================================================================
    Manager(unsigned priorityLevels, double activationQuantum,
            double minimumActivation, double maximumActivation,
            double activationThreshold);
    
    
    //  =======================================================================
    /// The destructor
    ///
    /// This is the class' destructor.
    //  =======================================================================
    virtual ~Manager();
    
    
    //  =======================================================================
    /// Adds an agent
    /// 
    /// This method adds an agent to the appropriate run queue.
    ///
    /// @param agent the agent to add
    //  =======================================================================
    void addAgent(Agent *agent);
    
    
    //  =======================================================================
    /// Gets an agent
    /// 
    /// This method returns a pointer to the named agent.
    ///
    /// @param agentName the name of the agent to retrieve
    ///
    /// @retval pointer to the named agent, or NULL if it could not be found
    //  =======================================================================
    Agent *getAgent(std::string agentName);
    
    
    //  =======================================================================
    /// Removes an agent
    /// 
    /// This method removes an agent from the manager's control and run queue.
    ///
    /// @param agent the agent to remove
    //  =======================================================================
    void removeAgent(Agent *agent);
    
    
    //  =======================================================================
    /// Start ticking
    /// 
    /// This method creates a new pthread and (indirectly) passes control of
    /// it to Manager::runLoop.
    //  =======================================================================
    void start();
    
    
    //  =======================================================================
    /// Stop ticking
    /// 
    /// This method sets the run flag to false which, in turn, causes the 
    /// manager to exit its run loop after it completes its current pass.
    //  =======================================================================
    void stop();
    
    
    //  =======================================================================
    /// Tick once
    /// 
    /// This method goes through all of the priorities queues and ticks each
    /// agent exactly once.
    //  =======================================================================
    void tick();

    
    //  =======================================================================
    /// Print a list of the managed agents
    /// 
    /// This method goes through all of the priorities queues and prints a
    /// summary of each of the managed agents.
    //  =======================================================================
    void printAgentSummaries();

    
    //  =======================================================================
    /// Queues a message for delivery
    /// 
    /// This method accepts a message, and places it into a time-delayed queue;
    /// the delay itself is a function of the sending and the receiving agents'
    /// level of activation
    ///
    /// @param sender   the agent sending the message
    /// @param receiver the receiving agent
    /// @param message  the message itself
    ///
    /// @retval true if the message will be delivered; false otherwise
    ///
    /// @note this method will only return false if the sender is inactive when
    ///       it calls Manager::sendMessage(...)
    //  =======================================================================
    bool sendMessage(Agent *sender, Agent *destination, Message::BaseMessage *message);
    
    
    //  =======================================================================
    /// Registers a callback
    /// 
    /// Through this method, an agent can ask the manager to 'wake' it by
    /// invoking a callback at some point in the future;  for example, it could
    /// be called by Agent::deactivate() to register a callback that will then
    /// delete an agent several ticks after it becomes inactive.
    ///
    /// @param agent    the agent to be woken
    /// @param delay    the receiving agent
    /// @param callback a pointer to the static callback method
    //  =======================================================================
    void registerCallback(Agent *agent, unsigned delay, void (*callback)(Agent *that));
    
    
    //  =======================================================================
    /// Returns the current system time
    /// 
    /// @retval the number of ticks since the manager's epoch
    //  =======================================================================
    inline unsigned time() { return m_time; }
    
    
    //  =======================================================================
    /// Returns the activation quantum
    /// 
    /// @retval the smallest unit of activation that an agent may consume
    //  =======================================================================
    inline double ActivationQuantum()  { return m_activationQuantum; }
    
    
    //  =======================================================================
    /// Returns the minimum level of activation
    /// 
    /// @retval the minimum possible level of activation
    //  =======================================================================
    inline double MinimumActivation()  { return m_minimumActivation; }
    
    
    //  =======================================================================
    /// Returns the maximum level of activation
    /// 
    /// @retval the maximum possible level of activation
    //  =======================================================================
    inline double MaximumActivation()  { return m_maximumActivation; }
    
    
    //  =======================================================================
    /// Returns the activation threshold
    /// 
    /// @retval the minimum level of activation than an /active/ agent can have
    //  =======================================================================
    inline double ActivationTreshold() { return m_activationThreshold; }
    
    
  protected:
    //  =======================================================================
    /// The 'function' that the newly-created pthread calls
    /// 
    /// A pointer to this static method is passed to pthread_create(...) by
    /// Agent::start();  its purpose is to pass control on to Manager::runLoop.
    /// 
    /// @param that the 'this' of the manager that created the new pthread
    ///
    /// @retval NULL
    //  =======================================================================
    static void *startThread(void *that);
    
    
    //  =======================================================================
    /// The main run loop
    /// 
    /// This method, which runs in its own thread, is the main run loop of the
    /// manager;  it is responsible for iterating over each priority queue,
    /// ticking the agents when appropriate, calling Agent::updateActivation,
    /// and moving agents between queues if necessary.
    //  =======================================================================
    void runLoop();
    
    
    //  =======================================================================
    /// Tick once
    /// 
    /// This method constitutes a single pass of the manager's main run loop.
    /// It may be called by runLoop() or tick(), and it is responsible for
    /// iterating over each priority queue, ticking the agents when 
    /// appropriate, calling Agent::updateActivation, and moving agents between
    /// queues if necessary.
    ///
    /// @param i     reference to a pre-allocated unsigned iterator;  it is 
    ///              passed in (rather than allocated locally) to avoid the 
    ///              memory-allocation overheads when called from runLoop()
    /// @param j     reference to a pre-allocated unsigned iterator
    /// @param i     reference to a pre-allocated unsigned iterator
    /// @param k     reference to a pre-allocated multimap iterator
    /// @param l     reference to a pre-allocated multimap iterator
    /// @param agent reference to a pre-allocated Agent pointer
    //  =======================================================================
    void Manager::doTick(unsigned &i, unsigned &j, unsigned &newQueue, 
                         std::multimap<unsigned, Message::Envelope*>::iterator &k,
                         std::multimap<unsigned, Callback*>::iterator &l, Agent* &agent);
    
    
    //  =======================================================================
    /// Determines if a queue should be ticked this time 'round
    /// 
    /// This method returns true if the agents in the specified priority queue 
    /// should be ticked at the current time-step.
    ///
    /// @retval true if it should; false otherwise
    //  =======================================================================
    virtual bool runQueueThisTick(unsigned priorityLevel);
    
    
    //  =======================================================================
    /// Determines which priority queue an agent should be in
    /// 
    /// This method determines which priority queue and agent should be in,
    /// based upon its current level of activation
    ///
    /// @retval the priority queue
    //  =======================================================================
    virtual unsigned queueByActivation(Agent *agent);
    
    
    //  =======================================================================
    /// Determines how many operations an agent can perform this time 'round
    /// 
    /// This method determines how many operations an agent can perform during
    /// the next tick, based upon its current level of activation;  it sets
    /// Agent::p_totalNOPSThisTick and Agent::p_nopsRemaining.
    //  =======================================================================
    virtual void updateNOPS(Agent *agent);
    
    
    unsigned m_priorityLevels;           /// The number of priority levels or queues to maintain
    unsigned m_priorityLevelsMinusOne;   /// m_priorityLevels - 1
    unsigned m_time;                     /// The number of ticks since the system's epoch
    double m_activationQuantum;          /// The smallest amount of activation that can be consumed
    double m_minimumActivation;          /// The minimum possible level of activation
    double m_maximumActivation;          /// The maximum possible level of activation
    double m_activationThreshold;        /// The minimum level of activation than an /active/ agent can have
    double m_activationRange;            /// m_maximumActivation - m_minimumActivation
    double m_activeRange;                /// m_maximumActivation - m_activationThreshold
    unsigned m_maximumMessageDelay;      /// The maximum message delivery delay, in ticks 
    
    bool                          m_running;
    pthread_t                     m_runThread;
    
    pthread_mutex_t               m_globalMutex;
    std::map<std::string, Agent*> m_agentNameToPointerMap;
    std::map<Agent*, unsigned>    m_agentPointerMap;
    std::vector<Agent*>          *m_agentPriorityQueue;
    unsigned                     *m_agentPriorityQueueTotalNumberOfTicks;
    unsigned                     *m_agentPriorityQueueSize;
    pthread_mutex_t              *m_agentPriorityQueueMutex;
    
    /// The multimap used to queue outgoing messages for delivery
    std::multimap<unsigned, Message::Envelope*> m_messageDeliveryQueue;
    
    /// The multimap used to queue callbacks
    std::multimap<unsigned, Callback*>  m_callbackQueue;

    friend class VariableRateExecution::ManagerConsole;    
    friend class VariableRateExecution::Test::ManagerTest;
  };
  
}

#endif

