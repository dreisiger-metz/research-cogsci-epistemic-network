#include "AMBRLogger.hpp"

const Logger::Category AMBRLogger::AGENT               =  0;
const Logger::Category AMBRLogger::REFERENCE           =  1;
const Logger::Category AMBRLogger::MESSAGE_RECEIVED    =  2;
const Logger::Category AMBRLogger::MESSAGE_SENT        =  3;
const Logger::Category AMBRLogger::ACTIVATION          =  4;
const Logger::Category AMBRLogger::ACTIVATION_RECEIVED =  5;
const Logger::Category AMBRLogger::ACTIVATION_SENT     =  6;
const Logger::Category AMBRLogger::SYMBOLIC            =  7;
const Logger::Category AMBRLogger::MARKER_RECEIVED     =  8;
const Logger::Category AMBRLogger::MARKER_SENT         =  9;
const Logger::Category AMBRLogger::MARKER_INTERSECTION = 10;
const Logger::Category AMBRLogger::WORKING_MEMORY      = 11;


char *AMBRLogger::m_categories[] = {"AGENT", "REFERENCE", "MESSAGE_RECEIVED", "MESSAGE_SENT",
				    "ACTIVATION", "ACTIVATION_RECEIVED", "ACTIVATION_SENT",
				    "SYMBOLIC", "MARKER_RECEIVED", "MARKER_SENT", "MARKER_INTERSECTION",
				    "WORKING_MEMORY"};
