// ============================================================================
// Filename          : $RCSfile: ManagerConsole.hpp,v $
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
// Purpose           : This header file defines the interface of the Manager's
//                     console or inspection tool
//
// Revision history  :
// ============================================================================
#ifndef libVariableRateExecution__ManagerConsole_hpp
#define libVariableRateExecution__ManagerConsole_hpp

#include "Manager.hpp"


namespace VariableRateExecution {
  
  // Forward declaration
  class Manager;
 
 
  class ManagerConsole {
  public:
    //  =======================================================================
    /// The constructor
    ///
    /// This constructor creates a 'console' that has access to all of the
    /// manager's protected and private variables.
    ///
    /// @param manager a pointer to the manager that this console will operate
    ///                upon
    //  =======================================================================
    ManagerConsole(Manager *manager) { m_manager = manager; }
    
    
    //  =======================================================================
    /// The destructor
    ///
    /// This is the class' destructor.
    //  =======================================================================
    virtual ~ManagerConsole() { };
    
    
  protected:
    Manager *m_manager;   /// The manager that this console will operate over
  };
  
}

#endif

