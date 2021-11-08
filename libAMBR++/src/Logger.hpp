// ============================================================================
// Filename          : $RCSfile: Logger.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 09-Oct-2008
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 05:54:51 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines a general purpose class that
//                     can be used to print out or log debugging information
//                     in a format not unlike that found on OpenVMS systems.
//
// Revision history  : 
//
// ---------------------------------------------------------------------------
// Notes:
//   These functions make use of the following preprocessor definitions:
//     __DEBUGF  If this directive is undefined, then these functions do
//              nothing (and will probably be stripped out by the compiler).
//     __DEBUGF__PRINT_LINE_NUMBERS  If this directive is defined then the
//              filename and line number arguments will be included in the
//              debug message.
//
//   These functions also make use of the following global variables (something
//   that may be frowned upon by some people, but it allows certain debugging
//   information to be available from any file or class in a program):
//
//     __DEBUGF_PROGRAM_NAME  The all-uppercase release name of the program.
//              This is not necessarily the name of the executable.
//     __DEBUGF_SEVERITY_LABEL  A predefined character array representing the
//              six levels of severity (Fatal, Error, Warning, Informative,
//              Verbose and Ludicrous corresponding to severity levels 0 to 5).
//     __DEBUGF_SEVERITY_NAME  A predefined array of C strings containing the
//              descriptive names of each severity level.
//     __DEBUGF_SEVERITY_MASK  An unsigned global variable that determines what
//              type of messages should be generated.
//     __DEBUGF_SEVERITY_MASK_MAX  The number of valid severity categories.
//     __DEBUGF_CATEGORY_LABEL  An array of all-uppercase C strings describing
//              the category of the message (e.g. "FILE_ACCESS", "SERIAL_IO",
//              "NETWORK_ACCESS").
//     __DEBUGF_CATEGORY_MASK  A bit-mapped mask that defines which message
//              categories should be generated.
//
//   For example, given
//
//     __DEBUGF_PROGRAM_NAME = "EXAMPLE"
//     __DEBUGF_SEVERITY_MASK = 4
//     __DEBUGF_CATEGORY_LABEL = {"FILE_ACCESS", "SERIAL_IO", "NETWORK_ACCESS"}
//     __DEBUGF_CATEGORY_MASK = 5
//
//   the function calls
//
//     debugf(__FILE__, __LINE__, 3, 0, "opened log file");
//     debugf(__FILE__, __LINE__, 1, 2, "cannot find server at %s:%d",
//            multicastIP, multicastPort);
//
//   would result in the messages
//
//     %EXAMPLE-I-FILE_ACCESS, opened log file
//     %EXAMPLE-E-NETWORK_ACCESS, cannot find server at 224.0.0.1:20000
//
//   appearing at stdout and stderr respectively.
// ============================================================================
#ifndef Logger_hpp
#define Logger_hpp

#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>


//  =========================================================================
/// @class    Logger
/// @brief    A general-purpose logger class.
///
/// @author   Peter Dreisiger
/// @version  0.1
//  =========================================================================
class Logger {
public:
  /// An enumeration of the six levels of severity
  enum Severity {
    FATAL,        /// a Very Bad Thing (TM) has occurred and we're about to core dump
    ERROR,        /// pretty bad, but we're able to handle this error
    WARNING,      /// a note-to-self or the user
    INFORMATIVE,  /// the most verbose level we should use for everyday use
    VERBOSE,      /// this will help us to debug the code
    LUDICROUS     /// for those hard-to-find bugs
  };


  /// The typedef used to define category-related constants;  note that
  /// derived classes should define a set of constants of type Category that
  /// correspond to the desired event categories.  Also note that the category
  /// constants should start from zero and be incremented by one
  typedef unsigned Category;

  
  //  =======================================================================
  /// The constructor
  ///
  /// This constructor instantiates a general-purpose logger, sets the program
  /// and category names and sets the category and severity masks.
  ///
  /// @param programName   the agent's mnemonic name
  /// @param categoryNames the agent's conceptual type
  /// @param categoryMask  a description of the agent
  /// @param severityLimit a description of the agent
  /// @param ofp           the default output stream that will be used by the
  ///                      simpler log(...) method
  /// =======================================================================
  Logger(const char *programName, const char *categoryNames[], Category categoryMask, Severity severityLimit, const FILE *ofp);


  //  =======================================================================
  /// The destructor
  ///
  /// This is the class' destructor.
  //  =======================================================================
  virtual ~Logger() { pthread_mutex_destroy(&m_mutex); };

 
  //  =======================================================================
  /// Logs a message to an output file stream
  ///
  /// This method constructs a message and sends it to the specified output
  /// stream
  ///
  /// @param ofp           the output stream to which 
  /// @param filename      the name of the calling source file
  /// @param lineNumber    the source line from which the method was called
  /// @param eventSeverity the severity of the message
  /// @param eventCategory the category of the message
  /// @param message       a C/printf style format string --- the message
  ///                      proper
  /// @param ...           any additional arguments referenced by the 
  ///                      message string
  //  =======================================================================
  void log(FILE *ofp, char *filename, unsigned lineNumber, Severity eventSeverity, Category eventCategory, char *message, ...);

  
  //  =======================================================================
  /// Logs a message to the default output file stream
  ///
  /// This method constructs a message and sends it to the default output
  /// stream (as specified in the constructor);  if the message's severity
  /// is fatal or error, a copy is also output to stderr
  ///
  /// @param filename      the name of the calling source file
  /// @param lineNumber    the source line from which the method was called
  /// @param eventSeverity the severity of the message
  /// @param eventCategory the category of the message
  /// @param message       a C/printf style format string --- the message
  ///                      proper
  /// @param ...           any additional arguments referenced by the 
  ///                      message string
  //  =======================================================================
  void log(char *filename, unsigned lineNumber, Severity eventSeverity, Category eventCategory, char *message, ...);
  
    
protected:
  const char           *m_programName;      ///
  const char          **m_categoryName;     ///
  Category              m_categoryMask;     ///
  static const char     m_severityLabel[];  ///
  static const char    *m_severityName[];   ///
  Severity              m_severityLimit;    ///
  static const Severity m_severityMaximum;  ///
  pthread_mutex_t       m_mutex;

  
  const FILE           *m_ofp;              ///
};

#endif
