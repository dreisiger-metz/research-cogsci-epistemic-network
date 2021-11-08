#include "Logger.hpp"


const char     Logger::m_severityLabel[] = "FEWIVL";
const char    *Logger::m_severityName[]= {"Fatal", "Error", "Warning", "Informative", "Verbose", "Ludicrous"};
const Logger::Severity Logger::m_severityMaximum = Logger::LUDICROUS;


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
Logger::Logger(const char *programName, const char *categoryName[], Category categoryMask = 0xffffffff, Severity severityLimit = Logger::WARNING, const FILE *ofp = stdout) {
  m_programName = programName;
  m_categoryName = categoryName;
  m_categoryMask = categoryMask;
  m_severityLimit = severityLimit;
  m_ofp = ofp;
  
  pthread_mutex_init(&m_mutex, NULL);
}




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
void Logger::log(FILE *ofp, char* filename, unsigned lineNumber, Severity eventSeverity, Category eventCategory, char *message, ...) {
  #ifdef __LOGGER
  va_list ap;
  va_start(ap, message);
  
  if ((eventSeverity <= m_severityLimit) && ((1 << eventCategory) & m_categoryMask)) {
    pthread_mutex_lock(&m_mutex);
    
    #ifdef __LOGGER__PRINT_LINE_NUMBERS
    fprintf(ofp, "%c%s-%c-%s (%s:%d), ", '%', m_programName, m_severityLabel[eventSeverity],
            m_categoryName[eventCategory], filename, lineNumber);
    #else
    fprintf(ofp, "%c%s-%c-%s, ", '%', m_programName, m_severityLabel[eventSeverity],
            m_categoryName[eventCategory]);
    #endif
    vfprintf(ofp, message, ap);
    fprintf(ofp, "\n");
    fflush(ofp);

    pthread_mutex_unlock(&m_mutex);
  }
  #endif
}




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
void Logger::log(char* filename, unsigned lineNumber, Severity eventSeverity, Category eventCategory, char *message, ...) {
  #ifdef __LOGGER
  FILE *ofp = (eventSeverity < Logger::WARNING)?stderr:((FILE*) m_ofp);
  va_list ap;
  va_start(ap, message);
  
  if ((eventSeverity <= m_severityLimit) && ((1 << eventCategory) & m_categoryMask)) {
    pthread_mutex_lock(&m_mutex);

    #ifdef __LOGGER__PRINT_LINE_NUMBERS
    fprintf(ofp, "%c%s-%c-%s (%s:%d), ", '%', m_programName, m_severityLabel[eventSeverity],
            m_categoryName[eventCategory], filename, lineNumber);
    #else
    fprintf(ofp, "%c%s-%c-%s, ", '%', m_programName, m_severityLabel[eventSeverity],
            m_categoryName[eventCategory]);
    #endif
    vfprintf(ofp, message, ap);
    fprintf(ofp, "\n");
    fflush(ofp);

    pthread_mutex_unlock(&m_mutex);
  }
  #endif
}
