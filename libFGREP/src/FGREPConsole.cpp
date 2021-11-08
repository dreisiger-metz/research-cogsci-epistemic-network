#include "FGREPConsole.hpp"

using namespace std;
using namespace ANN;



//  ======================================================================80
/// The constructor
///
/// This constructor creates an instance of an FGREP console.
//  ======================================================================80
FGREPConsole::FGREPConsole(unsigned representationSize, unsigned termsPerSentence, unsigned hiddenLayerSize, unsigned seed, FILE *ifp, FILE *ofp) {
  struct timeval now;

  // Initialise our parameters and add them to m_variableNameValueMap
  m_representationSize = representationSize;
  m_termsPerSentence = termsPerSentence;
  m_layers = 3;
  m_trainingAlgorithm = FGREPConsole::IncrementalTraining;
  
  m_variableNameUnsignedMap["representation-size"] = m_representationSize;
  m_variableNameUnsignedMap["terms-per-sentence"] = m_termsPerSentence;
  m_variableNameUnsignedMap["number-of-layers"] = m_layers;
  m_variableNameUnsignedMap["quiet"] = 0;
  m_variableNameUnsignedMap["print-errors"] = 0;

  m_variableNameDoubleMap["learning-rate"] = 0.1;
  m_variableNameDoubleMap["momentum"] = 0.0;
  m_variableNameDoubleMap["cpu-time-last"] = 0.0;
  m_variableNameDoubleMap["cpu-time-total"] = 0.0;
  
  m_ifp = ifp;
  m_ofp = ofp;
  m_defaultIFP = ifp;

  // Unless we're given an explicit seed, we should pick a random(ish) one,
  // say, based on the current time.
  if (seed == 0) {
    gettimeofday(&now, NULL);
    seed = now.tv_sec;
  }
  Perceptron::Uniform.seed(seed);
  srand(seed);

  if (hiddenLayerSize == 0)
    hiddenLayerSize = m_representationSize * m_termsPerSentence / 2;
  m_variableNameUnsignedMap["hidden-layer-size"] = hiddenLayerSize;

  m_fgrep = new FGREPNetwork("interactive-fgrep", m_variableNameDoubleMap["learning-rate"],
			     m_variableNameDoubleMap["momentum"], 
			     m_representationSize, m_layers,
			     m_representationSize * m_termsPerSentence, hiddenLayerSize,
			     m_representationSize * m_termsPerSentence);
}




//  ======================================================================80
/// The destructor
//  ======================================================================80
FGREPConsole::~FGREPConsole() {
  delete m_fgrep;
}




//  ======================================================================80
/// Enters the console's run loop
/// 
/// @returns the total number of operations
//  ======================================================================80
unsigned FGREPConsole::runLoop() {
  char command[4096];
  unsigned ops = 0, quiet;
  double time;
  struct rusage then, now;
  string retVal;

  command[0] = 0;
  while (strcmp(command, "quit")) {
    quiet = m_variableNameUnsignedMap["quiet"];
    if (!quiet)
      fprintf(m_ofp, "FGREP> ");

    if (fgets(command, 4096, m_ifp)) {
      command[strcspn(command, "\n")] = 0;   // kill the \n

      if ((m_ifp != stdin) && (!quiet))
	fprintf(m_ofp, "%s\n", command);
      fflush(m_ofp);

      getrusage(RUSAGE_SELF, &then);
      retVal = processCommand(command);
      getrusage(RUSAGE_SELF, &now);
      time = (now.ru_utime.tv_sec + now.ru_stime.tv_sec) - (then.ru_utime.tv_sec + then.ru_stime.tv_sec) + ((now.ru_utime.tv_usec + now.ru_stime.tv_usec) - (then.ru_utime.tv_usec + then.ru_stime.tv_usec)) / 1000000.0;
      m_variableNameDoubleMap["cpu-time-last"] = time;
      m_variableNameDoubleMap["cpu-time-total"] += time;

      if (retVal.length() > 0)
	fprintf(m_ofp, "%s\n", retVal.c_str());

      ops++;
    } else {
      fclose(m_ifp);
      if (m_ifp != m_defaultIFP) {
	m_ifp = m_defaultIFP;
        if (!quiet)
	  fprintf(m_ofp, "\nend of command file reached\n");
      } else {
	fprintf(m_ofp, "\n");
	return ops;   // our constructor was given an input FILE* that wasn't stdin, so exit
      }
    }
  }
  
  printf("Exitting after %d operations\n", ops - 1);
  return ops;
}


//  ======================================================================80
/// Processes a command
///
/// This method passes the input to the command processor (which processes
/// it as if it was issued by the user).
///
/// @param command   the command to process
///
/// @returns the response to the command
///
/// @note The commands are:
///           load '<filename>'
///           save '<filename>'
///           load-commands '<filename>'
///           declare-term <term> [<value for fixed-representations>]
///           add-sentence <word1> <word2> ... <wordn>
///           train-network <iterations | max-error>
///           process-term <term>
///           inspect-term [<term>]
/// To add:   set seed, show cpu-time, set ofp [stdout | <filename>]
//  ======================================================================80
string FGREPConsole::processCommand(string command) {
  char temp_c[80];
  char *help[] = { "load <filename>",
		   "save <filename>",
		   "load-commands <filename>",
		   "declare-term <term-name> [<activation-value for fixed sources>]", 
		   "inspect-term [<term-name> ...]",
		   "add-sentence <term-1> ... <term-n> [<sentence-weight>]",
		   "weigh-sentence <weight> [<sentence-id-1> ...]",
		   "inspect-sentences",
		   "train-network [iterations | error ] <limit>",
		   "process-representations",
		   "set <variable-name> <value>",
		   "show [<variable-name> ...]",
		   "help",
		   "quit" };
  bool temp_b;
  unsigned i, oldPos = 0, newPos = command.find_first_of(" ", oldPos), temp_u;
  double temp_d;
  string temp_s;
  FILE *fp;
  vector<double> fixedVals;
  vector<string> token;
  map<string, unsigned>::iterator temp_ui;
  map<string, double>::iterator temp_di;
  
  // tokenise the command string
  do {
    token.push_back(command.substr(oldPos, newPos - oldPos));
    if (token[token.size() - 1] == "")
      token.pop_back();   // allow multiple spaces between tokens
    
    oldPos = newPos + 1;
    newPos = command.find_first_of(" ", oldPos);
  } while (oldPos != 0);
  

  // process them
  if (((token[0].c_str())[0] == 0) || ((token[0].c_str())[0] == '#') || ((token[0].c_str())[0] == '%')) {
    // comment -- just ignore
    return "";


  } else if (token[0] == "load") {
    // loads the network and distributed-representation states from the
    // specified file
    if (token.size() == 2) {
      fp = fopen(token[1].c_str(), "r");
      if (fp != NULL) {
	try {
	  m_fgrep->load(fp);
	  fclose(fp);
	  m_fgrep->processRepresentations(); // optional, but this probably makes sense
	  return "done.";
	} catch (Exception::IncompatibleNetworkGeometry &e) {
	  fclose(fp);
	  return "save file was generated from a network with a different geometry - " + e.comment;
	} catch (Exception::BaseException &e) {
	  fclose(fp);
	  return "unknown error - " + e.comment;
	}
      } else {
	return "error loading file";
      }
    } else 
      return "syntax error: usage is " + string(help[0]);

    
  } else if (token[0] == "save") {
    // saves the network and distributed-representation states to the
    // specified file
    if (token.size() == 2) {
      fp = fopen(token[1].c_str(), "w");
      if (fp != NULL) {
	m_fgrep->save(fp);
	fclose(fp);
	return "done.";
      } else {
	return "error opening file";
      }
    } else 
      return "syntax error: usage is " + string(help[1]);
    
    
  } else if (token[0] == "load-commands") {
    // loads command strings from the specified file and executes them
    // a line at a time
    if (token.size() == 2) {
      fp = fopen(token[1].c_str(), "r");
      if (fp != NULL) {
	m_ifp = fp;
	return "";
      } else {
	return "error opening file";
      }
    } else
      return "syntax error: usage is " + string(help[2]);

    
  } else if (token[0] == "declare-term") {
    // declares a new term --- a DistributedRepresentation if only the
    // name is given, or a FixedSource if an activation level is specified
    if (token.size() == 2) {
      try {
        m_fgrep->declareTerm(token[1]);  // let ::FGREP create the DR
      } catch (Exception::DuplicateTerm &e) {
        return "term has already been declared";
      } catch (Exception::BaseException &e) {
        return "unknown error" + e.comment;
      }
      return "";

    } else if (token.size() > 2) {
      fixedVals.clear();
      for (i = 2; i < token.size(); i++) {
	sscanf(token[i].c_str(), "%lf", &temp_d);
	fixedVals.push_back(temp_d);
      }
      FixedRepresentation *rep = new FixedRepresentation(token[1], m_representationSize, fixedVals);
      try {
        m_fgrep->addTerm(rep);
      } catch (Exception::DuplicateTerm &e) {
        return "term has already been declared";
      } catch (Exception::BaseException &e) {
        return "unknown error" + e.comment;
      }
      return "";

    } else
      return "syntax error: usage is " + string(help[3]);
    
    
  } else if ((token[0] == "inspect-term") || (token[0] == "inspect-terms")){
    // prints the term's pattern of activation
    temp_b = (m_variableNameUnsignedMap["print-errors"] != 0);
    if (token.size() > 1) {
      for (i = 1; i < token.size(); i++)
	if (m_fgrep->termRepresentationMap.find(token[i]) != m_fgrep->termRepresentationMap.end())
	  m_fgrep->termRepresentationMap[token[i]]->print(m_ofp, "  ", temp_b);
	else
	  return "unknown term";
    } else {
      for (map<string, DistributedRepresentationBase*>::iterator term = m_fgrep->termRepresentationMap.begin(); term != m_fgrep->termRepresentationMap.end(); term++)
	term->second->print(m_ofp, "  ", temp_b);
    }
    return "";
    
    
  } else if (token[0] == "add-sentence") {
    // adds a sentence to fgrep->sentence
    if (token.size() > m_termsPerSentence) {
      for (temp_u = 1; temp_u < m_termsPerSentence; temp_u++)
        temp_s += token[temp_u] + " ";
      temp_s += token[temp_u];
      if (token.size() == m_termsPerSentence + 2) {   // parse the sentence weight, if given
	sscanf(token[m_termsPerSentence + 1].c_str(), "%lf", &temp_d);
	if (temp_d < 0.0)
	  temp_d = 0.0;
	else if (temp_d > 1.0)
	  temp_d = 1.0;
      } else
	temp_d = 1.0;

      try {
        m_fgrep->addSentence(temp_s, temp_d);
      } catch (Exception::UnknownTerm &e) {
        if (m_ifp == stdin)
          return "unknown term: " + e.comment;
        else {
          fprintf(m_ofp, "unknown term: %s\n", e.comment.c_str());
          exit(1);
        }
      } catch (Exception::BaseException &e) {
        return "unknown error: " + e.comment;
      }
      return "";

    } else
      return "syntax error: incorrect number of terms.  usage is " + string(help[5]);
    
    
  } else if ((token[0] == "weigh-sentence") || (token[0] == "weigh-sentences")) {
    // set a sentence's weight
    if (token.size() == 1)
      return "syntax error:  usage is " + string(help[6]);
    else if (token.size() == 2) {
      sscanf(token[1].c_str(), "%lf", &temp_d);
      for (i = 0; i < m_fgrep->sentence.size(); i++)
	m_fgrep->sentence[i]->weight = temp_d;

    } else if (token.size() > 2) {
      sscanf(token[1].c_str(), "%lf", &temp_d);
      for (i = 2; i < token.size(); i++) {
	sscanf(token[i].c_str(), "%d", &temp_u);
	if ((temp_u >= 0) && (temp_u < m_fgrep->sentence.size()))
	  m_fgrep->sentence[temp_u]->weight = temp_d;
	else
	  return "invalid sentence-id";
      }
    }
    return "";
    
    
  } else if ((token[0] == "inspect-sentence") || (token[0] == "inspect-sentences")) {
    // prints the sentences
    //if (token.size() >= 1) {
    for (i = 0; i < m_fgrep->sentence.size(); i++) {
      fprintf(m_ofp, "  %d  ", i);
      m_fgrep->sentence[i]->print(m_ofp, "  ");
    }
    return "";
    
    
  } else if (token[0] == "train-network") {
    // calls fgrep->trainNetwork --- the iterative version if an integer
    // is given, or the error-based one if a floating point number is given
    // train-network [iterations=<number-of-iterations> | error-squared=<errorThreshold> | delta-representations=<deltaThreshold>]
    if (token.size() == 3) {
      sscanf(token[2].c_str(), "%lf", &temp_d);
      if (token[1] == "iterations") {
	if (m_trainingAlgorithm == FGREPConsole::IncrementalTraining)
	  temp_d = m_fgrep->trainNetworkByNumberOfIterationsIncremental((unsigned) temp_d);
	else if (m_trainingAlgorithm == FGREPConsole::BatchTraining)
	  temp_d = m_fgrep->trainNetworkByNumberOfIterationsBatch((unsigned) temp_d);

      } else if (token[1] == "error") {
	if (m_trainingAlgorithm == FGREPConsole::IncrementalTraining)
	  temp_d = m_fgrep->trainNetworkByAverageErrorSquaredIncremental(temp_d);
	else if (m_trainingAlgorithm == FGREPConsole::BatchTraining)
	  temp_d = m_fgrep->trainNetworkByAverageErrorSquaredBatch(temp_d);
      } else
	return "syntax error: usage is " + string(help[8]);

      m_fgrep->processRepresentations();
      sprintf(temp_c, "%1.12lg", temp_d);
      return "done.  average squared error is " + string(temp_c);

    } else
      return "syntax error: usage is " + string(help[8]);

    
  } else if (token[0] == "process-representations") {
    // calls fgrep->processRepresentations which, in turn, iterates over all
    // training sentences and terms, and works out the average errors squared
    // for each term /and/ sentence
    temp_d = m_fgrep->processRepresentations();

    sprintf(temp_c, "%1.12lg", temp_d);
    return "done.  average squared error is " + string(temp_c);


  } else if (token[0] == "set") {
    if (token.size() == 3) {
      if ((temp_ui = m_variableNameUnsignedMap.find(token[1])) != m_variableNameUnsignedMap.end()) {
	sscanf(token[2].c_str(), "%d", &temp_u);
	temp_ui->second = temp_u;
      } else if ((temp_di = m_variableNameDoubleMap.find(token[1])) != m_variableNameDoubleMap.end()) {
	sscanf(token[2].c_str(), "%lf", &temp_d);
	temp_di->second = temp_d;
      } else if (token[1] == "seed") {
	sscanf(token[2].c_str(), "%d", &temp_u);
	Perceptron::Uniform.seed(temp_u);
	srand(temp_u);
	return "";

      } else if (token[1] == "output-file") {
	if (token.size() == 3) {
	  if (token[2] == "stdout") {
	    if (m_ofp != stdout)
	      fclose(m_ofp);
	    m_ofp = stdout;
	  } else {
	    fp = fopen(token[2].c_str(), "w");
	    if (fp != NULL) {
	      m_ofp = fp;
	      return "";
	    } else
	      return "error opening file";
	  }
	  return "";
	} else
	  return "syntax error: usage is set output-file [stdout | <filename>]";

      } else if (token[1] == "learning-algorithm") {
	if (token.size() == 3) {
	  if (token[2] == "incremental")
	    m_trainingAlgorithm = FGREPConsole::IncrementalTraining;
	  else if (token[2] == "batch")
	    m_trainingAlgorithm = FGREPConsole::BatchTraining;
	  else
	    return "unknown algorithm type";
	} else
	  return "syntax error: usage is set learning-algorithm [incremental | batch]";

      } else
	return "unknown variable";
      return "";
    } else
      return "syntax error: usage is " + string(help[10]);


  } else if (token[0] == "show") {
    if (token.size() == 1) {
      for (temp_ui = m_variableNameUnsignedMap.begin(); temp_ui != m_variableNameUnsignedMap.end(); temp_ui++)
	fprintf(m_ofp, "  %20s  %d\n", temp_ui->first.c_str(), temp_ui->second);
      for (temp_di = m_variableNameDoubleMap.begin(); temp_di != m_variableNameDoubleMap.end(); temp_di++)
	fprintf(m_ofp, "  %20s  %lf\n", temp_di->first.c_str(), temp_di->second);
      fprintf(m_ofp, "  %20s  %d\n", "seed", Perceptron::Uniform.seed());
      fprintf(m_ofp, "  %20s  %s\n", "output-file", (m_ofp == stdout)?"stdout":"output-file");
      fprintf(m_ofp, "  %20s  %d\n", "epochs", m_fgrep->epochs());
      fprintf(m_ofp, "  %20s  %s\n", "learning-algorithm", (m_trainingAlgorithm == FGREPConsole::IncrementalTraining)?"incremental":"batch");

    } else {
      for (i = 1; i < token.size(); i++)
	if ((temp_ui = m_variableNameUnsignedMap.find(token[i])) != m_variableNameUnsignedMap.end())
	  fprintf(m_ofp, "  %20s  %d\n", temp_ui->first.c_str(), temp_ui->second);
	else if ((temp_di = m_variableNameDoubleMap.find(token[i])) != m_variableNameDoubleMap.end())
	  fprintf(m_ofp, "  %20s  %lf\n", temp_di->first.c_str(), temp_di->second);
	else if (token[i] == "seed")
	  fprintf(m_ofp, "  %20s  %d\n", "seed", Perceptron::Uniform.seed());
	else if (token[i] == "output-file")
	  fprintf(m_ofp, "  %20s  %s\n", "output-file", (m_ofp == stdout)?"stdout":"output-file");
	else if (token[i] == "epochs")
	  fprintf(m_ofp, "  %20s  %d\n", "epochs", m_fgrep->epochs());
	else if (token[i] == "learning-algorithm")
	  fprintf(m_ofp, "  %20s  %s\n", "learning-algorithm", (m_trainingAlgorithm == FGREPConsole::IncrementalTraining)?"incremental":"batch");
	else
	  return "unknown variable";
    }
    return "";


  } else if (token[0] == "help") {
    for (i = 0; i < 13; i++)
      fprintf(m_ofp, "  %s\n", help[i]);
    return "";


  } else if (token[0] == "quit") {
    return "";


  } else
    return "syntax error: unknown command";
}
