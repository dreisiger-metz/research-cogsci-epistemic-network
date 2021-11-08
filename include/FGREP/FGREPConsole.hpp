// ============================================================================
// Filename          : $RCSfile: FGREPConsole.hpp,v $
// Version           : $Revision: 1.2 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 15-Jan-2009
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 11:22:59 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines the interface of an FGREP
//                     console
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libFGREP__FGREPConsole_hpp
#define libFGREP__FGREPConsole_hpp

#include <sys/resource.h>
#include <sys/time.h>

#include "FGREPNetwork.hpp"


namespace ANN {

  // Forward declarations
  namespace Test {
    class FGREPConsoleTest;
  }
  

  //  ========================================================================80
  /// @class    FGREPConsole
  /// @brief    A class that implements an FGREP console
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  //  ========================================================================80
  class FGREPConsole {
  public:
    /// An enumeration of the types of training algorithms
    enum TrainingType {
      IncrementalTraining,   /// the incremental training algorithm 
      BatchTraining          /// the batch training algorithm
    };


    //  ======================================================================80
    /// The constructor
    ///
    /// This constructor creates an instance of an FGREP console.
    //  ======================================================================80
    FGREPConsole(unsigned representationSize, unsigned termsPerSentence, unsigned hiddenLayerSize, unsigned seed, FILE *ifp, FILE *ofp);


    //  ======================================================================80
    /// The destructor
    //  ======================================================================80
    virtual ~FGREPConsole();


    //  ======================================================================80
    /// Enters the console's run loop
    /// 
    /// @returns the total number of operations
    //  ======================================================================80
    unsigned runLoop();


    //  ======================================================================80
    /// Processes a command as if it was issued from the console
    ///
    /// This method passes the input to the command processor (which processes
    /// it as if it was issued by the user).
    ///
    /// @param command   the command to process
    ///
    /// @returns the (new) total number of terms
    //  ======================================================================80
    std::string processCommand(std::string command);


  protected:
    unsigned m_representationSize;
    unsigned m_termsPerSentence;
    unsigned m_layers;
    unsigned m_epochs;

    TrainingType m_trainingAlgorithm;

    std::map<std::string, unsigned>  m_variableNameUnsignedMap;
    std::map<std::string, double>  m_variableNameDoubleMap;

    FGREPNetwork *m_fgrep;

    FILE *m_ifp, *m_ofp, *m_defaultIFP;
    
    friend class Test::FGREPConsoleTest;
  };
  
}
  
#endif
