// ============================================================================
// Filename          : $RCSfile: Slot.hpp,v $
// Version           : $Revision: 1.1.1.1 $
// Release           : $Name:  $
//
// Original author   : Peter Dreisiger, MOD, DSTO Stirling
// Original date     : 21-Jul-2008
// Last modified by  : $Author: prd $
// Last modified on  : $Date: 2009/03/12 05:54:51 $
//
// Security class.   : UNCLASSIFIED
// Copyright         : DSTO
//
// Purpose           : This header file defines the agent-specific :s-slots
//
// Linker options    : 
//
// Revision history  : 
// ============================================================================
#ifndef libAMBR__Slot_hpp
#define libAMBR__Slot_hpp

#include "AMBR.hpp"
#include "Reference.hpp"


namespace AMBR {


  // Forward declarations
  class SlotReference;
  class AgentReference;
  class Agent;


  //  =========================================================================
  /// @class    Slot
  /// @brief    The class that implements the specific-slot--facet--filler 
  ///           structures found in the DUAL/AMBR agents.
  ///
  /// @author   Peter Dreisiger
  /// @version  0.1
  ///
  /// @note     A DUAL/AMBR slot must have a name, a type and a comment;  in
  ///           addition to these, it may also contain the following facets:
  ///             - a coreference             - an instance-of
  ///             - a sub-class               - a peer
  //  =========================================================================
  class Slot {
  public:
    /// An enumeration of the conceptual slot types
    enum SlotType  { AspectSlot, RelationSlot };


    //  =======================================================================
    /// The constructor
    ///
    /// This constructor creates an instance of a specific slot and sets its 
    /// name, type and comment.
    ///
    /// @param name    the slot's mnemonic name
    /// @param type    the slot's conceptual type
    /// @param comment a description of the slot
    /// =======================================================================
    Slot(std::string name, SlotType type, std::string comment);


    //  =======================================================================
    /// The destructor
    ///
    /// This is the class' destructor.
    //  =======================================================================
    ~Slot();


    //  =======================================================================
    /// Adds a superclass
    ///
    /// This method adds the named slot to the current slot's list of 
    /// superclasses.
    ///
    /// @param slotName the slot's fully-qualified name (i.e. AgentName.SlotName)
    /// @param weight   the strength of the coreference;  if no weight is
    ///                 specified then the default of 1.0 is used
    ///
    /// @retval zero if the slot reference was resolved; one otherwise
    ///
    /// @throw Exception::InvalidSlot if [slotName] is an invalid slot name,
    ///                 or if it has already been added 
    //  =======================================================================
    unsigned addSuperClass(std::string slotName, double weight) throw (Exception::InvalidSlot);


    //  =======================================================================
    /// Adds a conceptual class
    ///
    /// This method adds the named slot to the current slot's instance-of list.
    ///
    /// @param slotName the slot's fully-qualified name (i.e. AgentName.SlotName)
    /// @param weight   the strength of the coreference;  if no weight is
    ///                 specified then the default of 1.0 is used
    ///
    /// @retval zero if the slot reference was resolved; one otherwise
    ///
    /// @throw Exception::InvalidSlot if [slotName] is an invalid slot name,
    ///                 or if it has already been added 
    //  =======================================================================
    unsigned addInstanceOf(std::string slotName, double weight) throw (Exception::InvalidSlot);


    //  =======================================================================
    /// Adds a coreference
    ///
    /// This method adds the named agent to the slot's list of coreferences.
    ///
    /// @param agentName the agent's name
    /// @param weight    the strength of the coreference;  if no weight is
    ///                  specified then the default of 1.0 is used
    ///
    /// @retval zero if the agent reference was resolved; one otherwise
    ///
    /// @throw Exception::InvalidAgent if [agentName] is an invalid agent name,
    ///                 or if it has already been added 
    //  =======================================================================
    unsigned addCoreference(std::string agentName, double weight) throw (Exception::InvalidAgent);


    //  =======================================================================
    /// Adds an association
    ///
    /// This method adds the named agent to the slot's list of associations.
    ///
    /// @param agentName the agent's name
    /// @param weight    the strength of the coreference;  if no weight is
    ///                  specified then the default of 1.0 is used
    ///
    /// @retval zero if the agent reference was resolved; one otherwise
    ///
    /// @throw Exception::InvalidAgent if [agentName] is an invalid agent name,
    ///                 or if it has already been added 
    //  =======================================================================
    unsigned addAssociation(std::string agentName, double weight) throw (Exception::InvalidAgent);


    //  =======================================================================
    /// Resolves the slot's references
    ///
    /// This method resolves all of the slot and agent references.  It needs to
    /// be called during the second pass of an agent's construction.
    ///
    /// @retval zero if all of the references have been resolved; one otherwise
    ///
    /// @throw Exception::InvalidSlot if a slot reference could not be resolved
    /// @throw Exception::InvalidAgent if an agent reference could not be resolved
    //  =======================================================================
    unsigned resolveReferences() throw (Exception::InvalidSlot, Exception::InvalidAgent);


    //  =======================================================================
    /// Prints the slot's information
    ///
    /// This method prints the details of the slot to the specified output
    /// stream.
    ///
    /// @param ostream the output stream to which the information will be
    ///                printed
    /// @param prefix  the constant C-style string that will be prefixed
    ///                to each line of output; this allows for arbitrary 
    ///                levels of indenting
    ///
    /// @retval the number of characters printed
    //  =======================================================================
    int print(FILE *ostream, const char *prefix);


    //  =======================================================================
    /// Parses the textual representation of the slot's information
    ///
    /// This method parses an earlier printout of the slot.
    ///
    /// @param istream the input stream from which the information will be
    ///                obtained
    ///
    /// @retval one if something was successfully parsed, zero otherwise
    //  =======================================================================
    int parse(FILE *istream) throw (Exception::InvalidSlot, Exception::InvalidAgent);


    std::string                  name;          /// the slot's mnemonic name
    SlotType                     type;          /// the slot's conceptual type
    std::string                  comment;       /// a description of the specific-slot
    Agent                        *agent;        /// a pointer to the slot's owning agent

    std::vector<SlotReference*>  superClasses;  /// a list of the slot's superclasses
    std::vector<SlotReference*>  instanceOf;    /// a list of the slot's instances

    std::vector<AgentReference*> coreferences;  /// a list of the slot's coreferences
    std::vector<AgentReference*> associations;  /// a list of the slot's associations
  };


} // namespace AMBR

#endif
