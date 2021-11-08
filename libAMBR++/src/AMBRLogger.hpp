// ============================================================================
// Filename          : $RCSfile: AMBRLogger.hpp,v $
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
// ============================================================================
#ifndef libAMBR__AMBRLogger_hpp
#define libAMBR__AMBRLogger_hpp

#include "Logger.hpp"


//  =========================================================================
/// @class    AMBRLogger
/// @brief    An AMBR-specific implementation of the general-purpose logger
///           class.
///
/// @author   Peter Dreisiger
/// @version  0.1
//  =========================================================================
class AMBRLogger : public Logger {
public:
  AMBRLogger(const char *programName, Category categoryMask = 0xffffffff,
	     Severity severityLimit = Logger::INFORMATIVE, 
	     const FILE *ofp = stdout) : Logger(programName, (const char **) m_categories,
						categoryMask, severityLimit, ofp) { };


  static const Logger::Category AGENT;
  static const Logger::Category REFERENCE;
  static const Logger::Category MESSAGE_RECEIVED;
  static const Logger::Category MESSAGE_SENT;
  static const Logger::Category ACTIVATION;
  static const Logger::Category ACTIVATION_RECEIVED;
  static const Logger::Category ACTIVATION_SENT;
  static const Logger::Category SYMBOLIC;
  static const Logger::Category MARKER_RECEIVED;
  static const Logger::Category MARKER_SENT;
  static const Logger::Category MARKER_INTERSECTION;
  static const Logger::Category WORKING_MEMORY;

protected:
  static char *m_categories[];
};

#endif
