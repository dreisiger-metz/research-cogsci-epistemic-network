#include "Slot.hpp"

using namespace std;
using namespace AMBR;


//  =========================================================================
/// @brief  The constructor
///
/// @param  name     the slot's name (relative, and unique, to its parent agent
/// @param  type     the conceptual type of the slot
/// @param  comment  an explanatory comment
//  =========================================================================
Slot::Slot(string name, SlotType type, string comment) {
  this->name = name;
  this->type = type;
  this->comment = comment;
  this->agent = NULL;

  AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::REFERENCE,
		    "%s slot '%s' created",
		    (type == Slot::AspectSlot)?"Aspect":"Relation", name.c_str());
}




//  =========================================================================
/// @brief  The destructor
//  =========================================================================
Slot::~Slot() {
  unsigned i;
  
  for (i = 0; i < this->superClasses.size(); i++)
    delete superClasses[i];
  for (i = 0; i < this->instanceOf.size(); i++)
    delete instanceOf[i];
  for (i = 0; i < this->coreferences.size(); i++)
    delete coreferences[i];
  for (i = 0; i < this->associations.size(); i++)
    delete associations[i];
}




//  ===========================================================================
/// @brief  Adds a superclass
/// This method adds a reference to the slot's conceptual superclass.  Note
/// that the new entry still needs to have its references resolved.
///
/// @param  slotName   name of the slot's conceptual superclass
/// @param  weight     the strength of the inheritence
///
/// @retval zero if the slot reference was resolved; one otherwise
///
/// @throw Exception::InvalidSlot if [slotName] is an invalid slot name,
///                 or if it has already been added 
//  ===========================================================================
unsigned Slot::addSuperClass(string slotName, double weight) throw (Exception::InvalidSlot) {
  SlotReference *temp;

  if (slotName.find('.') != string::npos) {
    for (unsigned i = 0; i < superClasses.size(); i++)
      if (superClasses[i]->name == slotName) {
	Exception::InvalidSlot e(slotName, "slot already added");
	throw e;
      }
    superClasses.push_back(temp = new SlotReference(slotName, weight));
  } else {
    Exception::InvalidSlot e(slotName, "not a valid slot name");
    throw e;
  }

  if (temp->slot == NULL)
    return 1;
  else {
    AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::REFERENCE,
		      "Superclass '%s' (%f) added to '%s' and resolved", 
		      slotName.c_str(), weight, this->name.c_str());

    return 0;
  }
}




//  ===========================================================================
/// @brief  Adds an instance-of
/// This method adds a reference to the conceptual class that this slot 
/// instantiates.  Note that the new entry still needs to have its reference 
/// resolved.
///
/// @param  slotName   name of the slot's conceptual class
/// @param  weight     the strength of the instantiation
///
/// @retval zero if the slot reference was resolved; one otherwise
///
/// @throw Exception::InvalidSlot if [slotName] is an invalid slot name,
///                 or if it has already been added 
//  ===========================================================================
unsigned Slot::addInstanceOf(string slotName, double weight) throw (Exception::InvalidSlot) {
  SlotReference *temp;

  if (slotName.find('.') != string::npos) {
    for (unsigned i = 0; i < instanceOf.size(); i++)
      if (instanceOf[i]->name == slotName) {
	Exception::InvalidSlot e(slotName, "slot already added");
	throw e;
      }
    instanceOf.push_back(temp = new SlotReference(slotName, weight));
  } else {
    Exception::InvalidSlot e(slotName, "not a valid slot name");
    throw e;
  }

  if (temp->slot == NULL)
    return 1;
  else {
    AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::REFERENCE,
		      "Instance-of '%s' (%f) added to '%s' and resolved", 
		      slotName.c_str(), weight, this->name.c_str());

    return 0;
  }
}




//  ===========================================================================
/// @brief  Adds a coreference
/// If a slot represents a specific aspect or facet of a larger concept, then
/// its coreferences are agents that describe those 'parts'.  This method adds
/// one of these coreferences.  Note that the new entry still needs to have
/// its reference resolved.
///
/// @param  agentName  name of the agent that this slot coreferences
/// @param  weight     the strength of the coreference
///
/// @retval zero if the agent reference was resolved; one otherwise
///
/// @throw Exception::InvalidAgent if [agentName] is an invalid agent name,
///                 or if it has already been added 
//  ===========================================================================
unsigned Slot::addCoreference(string agentName, double weight) throw (Exception::InvalidAgent) {
  AgentReference *temp;

  if (agentName.find('.') == string::npos) {
    for (unsigned i = 0; i < coreferences.size(); i++)
      if (coreferences[i]->name == agentName) {
	Exception::InvalidAgent e(agentName, "agent already added");
	throw e;
      }
    coreferences.push_back(temp = new AgentReference(agentName, weight));
  } else {
    Exception::InvalidAgent e(agentName, "not a valid agent name");
    throw e;
  }

  if (temp->agent == NULL)
    return 1;
  else {
    AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::REFERENCE,
		      "Coreference from '%s' to '%s' (%f) added and resolved", 
		      this->name.c_str(), agentName.c_str(), weight);

    return 0;
  }
}




//  ===========================================================================
/// @brief  Adds an association
/// This method associates an agent with the current slot.  Note that the new
/// entry still needs to have its reference resolved.
///
/// @param  agentName  name of the associated agent
/// @param  weight     the strength of the association
///
/// @retval zero if the agent reference was resolved; one otherwise
///
/// @throw Exception::InvalidAgent if [agentName] is an invalid agent name,
///                 or if it has already been added 
//  ===========================================================================
unsigned Slot::addAssociation(string agentName, double weight) throw (Exception::InvalidAgent) {
  AgentReference *temp;

  if (agentName.find('.') == string::npos) {
    for (unsigned i = 0; i < associations.size(); i++)
      if (associations[i]->name == agentName) {
	Exception::InvalidAgent e(agentName, "agent already added");
	throw e;
      }
    associations.push_back(temp = new AgentReference(agentName, weight));
  
  } else {
    Exception::InvalidAgent e(agentName, "not a valid agent name");
    throw e;
  }

  if (temp->agent == NULL)
    return 1;
  else {
    AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::REFERENCE,
		      "Association from '%s' to '%s' (%f) added and resolved", 
		      this->name.c_str(), agentName.c_str(), weight);

    return 0;
  }
}




//  ===========================================================================
/// @brief  Resolves the slot and agent references
/// This method goes through all of the current slot's references and invokes
/// their resolveReference() methods.  This, in turn, causes them to set their
/// agent or slot pointers based upon the reference's name.
///
/// @retval the number of references that have been resolved
//  ===========================================================================
unsigned Slot::resolveReferences() throw (Exception::InvalidSlot, Exception::InvalidAgent) {
  unsigned count = 0, i;

  try {
    for (i = 0; i < this->superClasses.size(); i++)
      count += superClasses[i]->resolveReference();
    for (i = 0; i < this->instanceOf.size(); i++)
      count += instanceOf[i]->resolveReference();
    for (i = 0; i < this->coreferences.size(); i++)
      count += coreferences[i]->resolveReference();
    for (i = 0; i < this->associations.size(); i++)
      count += associations[i]->resolveReference();
  } catch (...) {
    throw;
  }

  AMBR::logger->log(__FILE__, __LINE__, Logger::LUDICROUS, AMBRLogger::REFERENCE,
		    "Slot '%s' resolved %d more references", this->name.c_str(), count);
  
  return count;  // we can only reach this point if all of the references have been resolved
}



  
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
int Slot::print(FILE *ostream, const char *prefix) {
  int count = 0;
  unsigned i;

  count += fprintf(ostream, "%s:%s  \"%s\"\n", prefix, this->name.c_str(), this->comment.c_str());
  count += fprintf(ostream, "%s  :type        %s\n", prefix, (this->type == Slot::AspectSlot)?":aspect":":relation");
  
  if (superClasses.size()) {
    count += fprintf(ostream, "%s  :superclass  (", prefix);
    for (i = 0; i < superClasses.size(); i++)
      count += fprintf(ostream, " (%s, %f)", superClasses[i]->name.c_str(), superClasses[i]->weight);
    count += fprintf(ostream, " )\n");
  }

  if (instanceOf.size()) {
    count += fprintf(ostream, "%s  :instance-of (", prefix);
    for (i = 0; i < instanceOf.size(); i++)
      count += fprintf(ostream, " (%s, %f)", instanceOf[i]->name.c_str(), instanceOf[i]->weight);
    count += fprintf(ostream, " )\n");
  }

  if (coreferences.size()) {
    count += fprintf(ostream, "%s  :coreference (", prefix);
    for (i = 0; i < coreferences.size(); i++)
      count += fprintf(ostream, " (%s, %f)", coreferences[i]->name.c_str(), coreferences[i]->weight);
    count += fprintf(ostream, " )\n");
  }

  if (associations.size()) {
    count += fprintf(ostream, "%s  :association (", prefix);
    for (i = 0; i < associations.size(); i++)
      count += fprintf(ostream, " (%s, %f)", associations[i]->name.c_str(), associations[i]->weight);
    count += fprintf(ostream, " )\n");
  }

  return count;
}




//  =======================================================================
/// Parses the textual representation of the slot's information
///
/// This method parses an earlier printout of the slot.
///
/// @param istream the input stream from which the information will be
///                obtained
///
/// @retval one if something was successfully parsed, zero otherwise
///
/// @note in order for this method to be invoked, a slot definition will
///       have already been found in [istream] and the corresponding (but
///       hitherto blank and uninitialised) Slot instance created.
//  =======================================================================
int Slot::parse(FILE *istream) throw (Exception::InvalidSlot, Exception::InvalidAgent) {
  char  line[AMBR::MaxLineLength], tag[32], value[AMBR::MaxLineLength], *temp_s, *save;
  long  start = ftell(istream), rollback = start;
  float weight;

  while (fgets(line, AMBR::MaxLineLength, istream)) {
    if (strtok_r(line, ":", &save) && (temp_s = strtok_r(NULL, ":(", &save))) {
      sscanf(temp_s, "%s", tag);

      if (!strcmp("type", tag)) {
	sscanf(strtok_r(NULL, ":", &save), "%s", value);
	if (!strcmp("aspect", value))
	  this->type = Slot::AspectSlot;
	else if (!strcmp("relation", value))
	  this->type = Slot::RelationSlot;
	else {
	  Exception::InvalidSlot e(name, "invalid slot type");
	  throw e;
	}

      } else if (!strcmp("superclass", tag)) {            // we have a superclass ---
	strtok_r(NULL, "(", &save);
	while ((temp_s = strtok_r(NULL, "(", &save))) {   // loop until we have no more tuples,
	  sscanf(temp_s, "%s %f", value, &weight);
	  value[strlen(value) - 1] = 0;                   // remove the comma,
	  this->addSuperClass(value, (double) weight);    // and add the superclass
	}

      } else if (!strcmp("instance-of", tag)) {
	strtok_r(NULL, "(", &save);
	while ((temp_s = strtok_r(NULL, "(", &save))) {
	  sscanf(temp_s, "%s %f", value, &weight);
	  value[strlen(value) - 1] = 0;
	  this->addInstanceOf(value, (double) weight);
	}
	
      } else if (!strcmp("coreference", tag)) {
	strtok_r(NULL, "(", &save);
	while ((temp_s = strtok_r(NULL, "(", &save))) {
	  sscanf(temp_s, "%s %f", value, &weight);
	  value[strlen(value) - 1] = 0;
	  this->addCoreference(value, (double) weight);
	}
	
      } else if (!strcmp("association", tag)) {
	strtok_r(NULL, "(", &save);
	while ((temp_s = strtok_r(NULL, "(", &save))) {
	  sscanf(temp_s, "%s %f", value, &weight);
	  value[strlen(value) - 1] = 0;
	  this->addAssociation(value, (double) weight);
	}

      } else {
	// We've encountered an unknown tag; as we're assuming that the
	// input is valid, this either indicates the start of the next
	// slot, or the end of the containing agent's definition.  In
	// these situations, we need to return [istream] back one line
	// and break out of the parsing loop.
	fseek(istream, rollback - ftell(istream), SEEK_CUR);
	break;
      }
    } else if (strcspn(line, ")") != strlen(line)) {
      // The line contains the parent agent's terminating ')' --- rewind
      // and break out of the parsing loop.
      fseek(istream, rollback - ftell(istream), SEEK_CUR);
      break;
    }
    rollback = ftell(istream);
  }

  return (ftell(istream) != start);
}
