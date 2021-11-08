#include "Manager.hpp"

using namespace std;
using namespace VariableRateExecution;


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
Manager::Manager(unsigned priorityLevels, 
                 double activationQuantum = 0.01,
                 double minimumActivation = 0.0,
                 double maximumActivation = 1.0,
                 double activationThreshold = 0.1) {
  unsigned i;
  
  // Set the instance-specific parameters,
  m_time = 0;
  m_priorityLevels = priorityLevels;
  m_activationQuantum = activationQuantum;
  m_minimumActivation = minimumActivation;
  m_maximumActivation = maximumActivation;
  m_activationThreshold = activationThreshold;
  m_maximumMessageDelay = 10;
  
  // set the derived constants,
  m_priorityLevelsMinusOne = priorityLevels - 1;
  m_activationRange = m_maximumActivation - m_minimumActivation;
  m_activeRange = m_maximumActivation - m_activationThreshold;
  
  // and initialise the queue- and execution-related variables
  m_running = false;
  pthread_mutex_init(&m_globalMutex, NULL);
  
  m_agentPriorityQueue = new vector<Agent*>[priorityLevels];
  m_agentPriorityQueueTotalNumberOfTicks = new unsigned[priorityLevels];
  m_agentPriorityQueueSize = new unsigned[priorityLevels];
  m_agentPriorityQueueMutex = new pthread_mutex_t[priorityLevels];
  for (i = 0; i < priorityLevels; i++) {
    m_agentPriorityQueueTotalNumberOfTicks[i] = 0;
    m_agentPriorityQueueSize[i] = 0;
    pthread_mutex_init(&m_agentPriorityQueueMutex[i], NULL);
  }
}


//  =======================================================================
/// The destructor
///
/// This is the class' destructor.
//  =======================================================================
Manager::~Manager() {
  unsigned i;
  multimap<unsigned, Message::Envelope*>::iterator j;
  map<Agent*, unsigned>::iterator k;
 
  for (j = m_messageDeliveryQueue.begin(); j != m_messageDeliveryQueue.end(); j++)
    delete j->second;
  
  while (m_agentPointerMap.size() > 0)
    delete (m_agentPointerMap.begin())->first;
  
  delete[] m_agentPriorityQueue;
  delete[] m_agentPriorityQueueTotalNumberOfTicks;
  delete[] m_agentPriorityQueueSize;
  
  for (i = 0; i < m_priorityLevels; i++)
    pthread_mutex_destroy(&m_agentPriorityQueueMutex[i]);
  delete[] m_agentPriorityQueueMutex;
  
  pthread_mutex_destroy(&m_globalMutex);
}




//  =======================================================================
/// Adds an agent
/// 
/// This method adds an agent to the appropriate run queue.
///
/// @param agent the agent to add
//  =======================================================================
void Manager::addAgent(Agent *agent) {
  unsigned queue;
  
  // If we haven't seen this agent before,
  if (m_agentPointerMap.find(agent) == m_agentPointerMap.end()) {
    // add it to the agent-pointer maps,
    m_agentPointerMap[agent] = 1;
    m_agentNameToPointerMap[agent->name] = agent;
    
    // working out where it should go, grab the appropriate mutex, add it
    // to the queue, and increment its corresponding size counter
    queue = queueByActivation(agent);
    agent->p_priorityLevel = queue;
    pthread_mutex_lock(&m_agentPriorityQueueMutex[queue]);
    m_agentPriorityQueue[queue].push_back(agent);
    m_agentPriorityQueueSize[queue]++;
    pthread_mutex_unlock(&m_agentPriorityQueueMutex[queue]);
  }
}




//  =======================================================================
/// Gets an agent
///
/// This method returns a pointer to the named agent.
///
/// @param agentName the name of the agent to retrieve
///
/// @retval pointer to the named agent, or NULL if it could not be found
//  =======================================================================
Agent *Manager::getAgent(string agentName) {
  if (m_agentNameToPointerMap.find(agentName) != m_agentNameToPointerMap.end())
    return m_agentNameToPointerMap[agentName];
  else
    return NULL;
}




//  =======================================================================
/// Removes an agent
/// 
/// This method removes an agent from the manager's control and run queue.
///
/// @param agent the agent to remove
//  =======================================================================
void Manager::removeAgent(Agent *agent) {
  unsigned queue, i;
  map<Agent*, unsigned>::iterator j;
  
  // If this agent was added earlier, find it, and remove it from its
  // current priority queue and the agent-pointer map.
  if ((j = m_agentPointerMap.find(agent)) != m_agentPointerMap.end()) {
    queue = agent->p_priorityLevel;
    for (i = 0; i < m_agentPriorityQueueSize[queue]; i++)
      if (m_agentPriorityQueue[queue][i] == agent) {
        pthread_mutex_lock(&m_agentPriorityQueueMutex[queue]);
        m_agentPriorityQueue[queue].erase(m_agentPriorityQueue[queue].begin() + i);
        m_agentPriorityQueueSize[queue]--;
        pthread_mutex_unlock(&m_agentPriorityQueueMutex[queue]);
        
        m_agentPointerMap.erase(j);
        m_agentNameToPointerMap.erase(m_agentNameToPointerMap.find(agent->name));
        break;
      }
  }
}




//  =======================================================================
/// Start ticking
/// 
/// This method creates a new pthread and (indirectly) passes control of
/// it to Manager::runLoop.
//  =======================================================================
void Manager::start() {
  pthread_mutex_lock(&m_globalMutex);
  
  if (m_running == false) {
    m_running = true;
    pthread_create(&m_runThread, NULL, (void *(*)(void*)) (startThread), (void *) this);
  }
  
  pthread_mutex_unlock(&m_globalMutex);
}




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
void *Manager::startThread(void *that) {
  ((Manager*) that)->runLoop();
  pthread_join(((Manager *) that)->m_runThread, NULL);
  
  return NULL;
}




//  =======================================================================
/// The main run loop
/// 
/// This method, which runs in its own thread, is the main run loop of the
/// manager;  it is responsible for iterating over each priority queue,
/// ticking the agents when appropriate, calling Agent::updateActivation,
/// and moving agents between queues if necessary.
//  =======================================================================
void Manager::runLoop() {
  unsigned i, j, newQueue;
  multimap<unsigned, Message::Envelope*>::iterator k;
  multimap<unsigned, Callback*>::iterator l;
  Agent *agent;
  
  while (m_running)
    doTick(i, j, newQueue, k, l, agent);
}




//  =======================================================================
/// Stop ticking
/// 
/// This method sets the run flag to false which, in turn, causes the 
/// manager to exit its run loop after it completes its current pass.
//  =======================================================================
void Manager::stop() {
  pthread_mutex_lock(&m_globalMutex);
  m_running = false;
  pthread_mutex_unlock(&m_globalMutex);
}




//  =======================================================================
/// Tick once
/// 
/// This method goes through all of the priorities queues and ticks each
/// agent exactly once.
//  =======================================================================
void Manager::tick() {
  unsigned i, j, newQueue;
  multimap<unsigned, Message::Envelope*>::iterator k;
  multimap<unsigned, Callback*>::iterator l;
  Agent *agent;
  
  doTick(i, j, newQueue, k, l, agent);
}
 


    
//  =======================================================================
/// Print a list of the managed agents
///
/// This method goes through all of the priorities queues and prints a
/// summary of each of the managed agents.
//  =======================================================================
void Manager::printAgentSummaries() {
  unsigned i, j;

  for (i = 0; i < m_priorityLevels; i++) {
    printf("Agents in priority level %d at time %d:\n", i, m_time);
    for (j = 0; j < m_agentPriorityQueueSize[i]; j++)
      printf("    %s (activation %.6g, address 0x%x)\n", m_agentPriorityQueue[i][j]->name.c_str(),
             m_agentPriorityQueue[i][j]->m_activation, (unsigned) m_agentPriorityQueue[i][j]);
  }
}




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
                      multimap<unsigned, Message::Envelope*>::iterator &k,
                      multimap<unsigned, Callback*>::iterator &l, Agent* &agent) {
  
  pthread_mutex_lock(&m_globalMutex);
  
  for (i = 0; i < m_priorityLevels; i++) {
    pthread_mutex_lock(&m_agentPriorityQueueMutex[i]);
    
    // Does this queue have a high enough priority to be run this tick?  If
    // it does, go through the queue and tick each agent.
    if (runQueueThisTick(i)) {
      for (j = 0; j < m_agentPriorityQueueSize[i]; j++) {
        agent = m_agentPriorityQueue[i][j];
        // call agent->activate() if necessary
        if ((agent->m_activation >= m_activationThreshold) && (agent->p_activeLastTick == false)) {
          agent->p_activeLastTick = true;
          agent->activate();
        }
        agent->tick();
        agent->p_totalNumberOfTicks++;
      }
      m_agentPriorityQueueTotalNumberOfTicks[i]++;
    }
    
    // Either way, go through it, updating each agent's activation and making
    // sure they're in the right queue.
    for (j = 0; j < m_agentPriorityQueueSize[i]; j++) {
      agent = m_agentPriorityQueue[i][j];
      // work out if the agent was active last tick, and update its activation levels
      agent->updateActivation();
      updateNOPS(agent);
      // call agent->deactivate() if necessary
      if ((agent->m_activation < m_activationThreshold) && (agent->p_activeLastTick == true)) {
        agent->deactivate();
        agent->p_activeLastTick = false;
      }
      
      // and move the agent to its new queue, if appropriate
      if ((newQueue = queueByActivation(agent)) != i) {
        agent->p_priorityLevel = newQueue;
        m_agentPriorityQueue[i].erase(m_agentPriorityQueue[i].begin() + j);
        m_agentPriorityQueueSize[i]--;
        m_agentPriorityQueue[newQueue].push_back(agent);
        m_agentPriorityQueueSize[newQueue]++;
        
        j--;  // because we've just removed the jth element
      }
    }
    pthread_mutex_unlock(&m_agentPriorityQueueMutex[i]);
  }
  
  // Deliver any messages that have fallen due this tick,
  for (k = m_messageDeliveryQueue.lower_bound(m_time); k != m_messageDeliveryQueue.upper_bound(m_time); k++) {
    k->second->destination->receive(k->second->message);
    delete k->second;
  }
  m_messageDeliveryQueue.erase(m_time);
  
  // and finally, invoke any callbacks that have fallen due
  // *** Need to think about this, especially wrt queued destruction ***
  for (l = m_callbackQueue.lower_bound(m_time); l != m_callbackQueue.upper_bound(m_time); l++) {
    l->second->callback(l->second->agent);
    delete l->second;
  }
  m_callbackQueue.erase(m_time);
  
  
  m_time++;
  pthread_mutex_unlock(&m_globalMutex);
}




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
bool Manager::sendMessage(Agent *sender, Agent *receiver, Message::BaseMessage *message) {
  unsigned delay = sender->p_priorityLevel + receiver->p_priorityLevel + 2;
  //unsigned delay = 1 + (unsigned) (4 * m_maximumActivation / (4 * sender->m_activation + receiver->m_activation));
  //if (delay > m_maximumMessageDelay)
  //  delay = m_maximumMessageDelay;
    
  m_messageDeliveryQueue.insert(make_pair(m_time + delay, new Message::Envelope(receiver, message)));

  return true;
}




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
void Manager::registerCallback(Agent *agent, unsigned delay, void (*callback)(Agent *that)) {
  if (delay > 0)
    m_callbackQueue.insert(make_pair(m_time + delay, new Callback(agent, callback)));
}




//  =======================================================================
/// Determines if a queue should be ticked this time 'round
/// 
/// This method returns true if the agents in the specified priority queue 
/// should be ticked at the current time-step.
///
/// @retval true if it should; false otherwise
//  =======================================================================
bool Manager::runQueueThisTick(unsigned priorityLevel) {
  if (priorityLevel == m_priorityLevels - 1)
    return false;
  else
    return (((double) (m_priorityLevelsMinusOne - priorityLevel) / m_priorityLevelsMinusOne * m_time) > m_agentPriorityQueueTotalNumberOfTicks[priorityLevel]);
  // return ((m_time % (priorityLevel + 1)) == 0);
}




//  =======================================================================
/// Determines which priority queue an agent should be in
/// 
/// This method determines which priority queue and agent should be in,
/// based upon its current level of activation
///
/// @retval the priority queue
//  =======================================================================
unsigned Manager::queueByActivation(Agent *agent) {
  unsigned queue;
  
  if (agent->m_activation < m_activationThreshold)
    queue = 0;
  else 
    queue = (unsigned) (((agent->m_activation - m_activationThreshold) / m_activeRange) * m_priorityLevelsMinusOne + 1);
  
  if (queue > m_priorityLevelsMinusOne)
    queue = m_priorityLevelsMinusOne;
  
  return m_priorityLevelsMinusOne - queue;
}




//  =======================================================================
/// Determines how many operations an agent can perform this time 'round
/// 
/// This method determines how many operations an agent can perform during
/// the next tick, based upon its current level of activation.
//  =======================================================================
void Manager::updateNOPS(Agent *agent) {
  if (agent->m_activation >= m_activationThreshold)
    agent->p_totalNOPSThisTick = (unsigned) (agent->m_activation / m_activationQuantum);
  else
    agent->p_totalNOPSThisTick = 0;
  
  agent->p_nopsRemaining = agent->p_totalNOPSThisTick;
}
