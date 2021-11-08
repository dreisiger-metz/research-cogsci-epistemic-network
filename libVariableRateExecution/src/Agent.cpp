#include "Agent.hpp"


using namespace std;
using namespace VariableRateExecution;



//  =======================================================================
/// The constructor
///
/// This constructor creates a generic variable-rate-of-execution agent
/// and sets its initial level of activation.
///
/// @param activation the agent's initial level of activation
/// @param manager    a pointer to the responsible manager
//  =======================================================================
Agent::Agent(string name, double activation, Manager *manager) {
  this->name = name;
  m_activation = activation;
  m_manager = manager;

  p_totalNumberOfTicks = 0;
  p_activeLastTick = false;

  m_manager->addAgent(this);
}




//  =======================================================================
/// The destructor
///
/// This is the class' destructor.
//  =======================================================================
Agent::~Agent() {
  m_manager->removeAgent(this);
}




//  =======================================================================
/// Determines if we have enough energy to perform an atomic operation
///
/// This method checks to see if we have can perform an atomic operation;
/// if we can, it also decrements our number-of-operations-remaining 
/// counter/
///
/// @retval true if we can and false if we can't
//  =======================================================================
bool Agent::consumeActivation() {
  if (p_nopsRemaining > 0) {
    p_nopsRemaining--;
    return true;
  } else
    return false;
}


bool Agent::consumeActivation(unsigned nops) {
  if (p_nopsRemaining >= nops) {
    p_nopsRemaining -= nops;
    return true;
  } else
    return false;
}




//  =======================================================================
/// Returns the current system time
///
/// This method returns the number of ticks since the agent's manager's
/// epoch.
///
/// @retval the current system time
//  =======================================================================
unsigned Agent::time() {
  return m_manager->time();
}
