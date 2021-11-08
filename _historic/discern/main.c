/* File: main.c
 *
 * Main story processing loop, initializations, and statistics
 * Risto Miikkulainen 5/7/93
 */

#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "discerndefs.h"
#define DEFINE_GLOBALS    /* so that the global variables get defined here */
#include "globals.c"
#define DEFINE_XGLOBALS   /* so that the global X variables get defined */
#include "xglobals.c"

/* Function prototypes */
void process_command(FILE *fp, String commandstr, String rest);
void init_nets(int argc, char **argv);
int init_inp(String inpfile, FILE **fp);
void voc_init(char *lexsem, char *vocfile, struct WORDSTRUCT words[], int *nwords);
void read_story_data(FILE *fp);
void read_qa_data(FILE *fp);
void readfun(FILE *fp, float *place, float par1, float par2);
void printwordout(char s[], char ss[]);
float distance(float v1[], float v2[], int nrep);
float seldistance(int indeces[], float v1[], float v2[], int ncomp);
void init_stats();
void collect_stats(int modi, float outrep[], int nas, int target[], struct WORDSTRUCT words[], int nrep, int nwords);
void print_stats();
void print_task_stats(char taskname[], int taski);
int find_nearest_s(float rep[], struct WORDSTRUCT words[], int nrep, int nwords);
void process_inpfile(FILE *fp);
void loop_stories();
void read_int_par(String rest, String commandstr, int *variable);
void read_float_par(String rest, String commandstr, float *variable);
void reps_init(char *lexsem, char *repfile, char *vocfile, struct WORDSTRUCT words[], int *nwords, int *nrep);
void printcomment(char *beginning, char *s, char *ending);
int fgetline(FILE *fp, char *s, int lim);
void fgl(FILE *fp);
void list_params();
void read_text_question(String rest);

void display_init();
void display_cleanup();
void qa_init();
void trace_init();
void hfm_init();
void lex_init();
void clear_traces();
void clear_networks();
void parse_story();
void gener_story();
void formcue();
void produceanswer();
void presentmem(int mod, int slots[]);
void read_inpfilename();
void wait_for_run();
void start_running();
void close_display();
void init_hfm_display_params(int neti, Widget w);

void main(int argc, char **argv)
{
  main_widget = XtAppInitialize(&app_con, "Discern", NULL, 0, &argc, argv,
				fallback_resources, NULL, 0);
  XtGetApplicationResources(main_widget, &data, resources,
			    XtNumber(resources), NULL, 0);
  setbuf(stdout,NULL);
  init_nets(argc, argv);
  if (displaying) display_init();
  printf("System initialization complete.\n");
  loop_stories();
  exit(0);
}

void loop_stories()
{
  printwordout(DISCERNPROMPT, " ");
  if (displaying)
    while(1)
      {
	wait_for_run();
	process_command(NULL, "file", current_inpfile);
      }
  else
    process_inpfile(stdin);
}

void process_command(FILE *fp, String commandstr, String rest)
{
  char s[MAXSTRL], ss[MAXSTRL];
  int i;
  FILE *fp2;

  if(fp==NULL && (!strcmp(commandstr, "read") ||
		  !strcmp(commandstr, "read-and-paraphrase") ||
		  !strcmp(commandstr, "question")))
    {
      sprintf(s, "command %s ignored in display mode, sorry", commandstr);
      printcomment("", s, "\n");
    }    
  else if(!strcmp(commandstr, "read"))
    {
      task=PARATASK;
      printcomment("", "Read story", "\n");
      read_story_data(fp);
      parse_story();
      if(withhfm)
	presentmem(STOREMOD,story.slots);
      if(babbling) printf("\n");
    }
  else if(!strcmp(commandstr, "read-and-paraphrase"))
    {
      task=PARATASK;
      printcomment("", "Read and paraphrase story", "\n");
      read_story_data(fp);
      parse_story();
      if(withhfm)
	presentmem(STOREMOD,story.slots);
      gener_story();
      if(babbling) printf("\n");
    }
  else if(!strcmp(commandstr, "question"))
    {
      task=QATASK;
      printcomment("", "Answer question", "\n");
      for(i=0; i<nslot; i++)
	fscanf(fp, "%d",&qa.slots[i]);
      fgl(fp);
      read_qa_data(fp);
      if(qa.question.included>0)
	{
	  formcue();
	  if(withhfm)
	    presentmem(RETMOD,qa.slots);
	}
      if(!(qa.answer.included<=0 || (withhfm && bestvalue<aliveact)))
	produceanswer();
      if(babbling) printf("\n");
    }
  else if(!strcmp(commandstr, "text-question"))
    {
      task=QATASK;
      printcomment("", "Answer text question", "\n");
      read_text_question(rest);
      text_question=1;
      formcue();
      if(withhfm) presentmem(RETMOD,qa.slots);
      if(!(withhfm && bestvalue<aliveact)) produceanswer();
      text_question=0;
      if(babbling) printf("\n");
    }
  else if(!strcmp(commandstr, "clear-networks"))
    {
      printcomment("", "Clearing networks", "\n");
      if (displaying) clear_networks();
      else clear_traces();
    }
  else if(!strcmp(commandstr, "stop"))
    {
      if (!ignore_stops)
	{
	  if(displaying)
	    printcomment("","Stopping (click Run to continue)", "\n");
	  else printcomment("", "Stopping (hit Return to continue)", "");
	  if (displaying) wait_for_run();
	  else while(getchar()!='\n');
	}
      else
	  printcomment("","Stop command ignored", "\n");
    }
  else if(!strcmp(commandstr, "file"))
    {
      sscanf(rest, "%s", s);
      if(init_inp(s, &fp2))
	{
	  if(fp==NULL || fp==stdin)
	    {
	      if (displaying) start_running();
	      sprintf(current_inpfile, "%s", s);
	    }
	  sprintf(ss, "Reading input from %s...", s);
	  printcomment("", ss, "\n");
	  process_inpfile(fp2);
	  fclose(fp2);
	  sprintf(ss, "Finished reading input from %s.", s);
	  printcomment("", ss, "\n");
	  if (displaying && fp==NULL) simulator_running=0;
	}
    }
  else if(!strcmp(commandstr, "echo"))
    {
      if(babbling || print_mistakes)
	printf("%s%s%s\n", BEGIN_COMMENT, rest+1, END_COMMENT);
    }
  else if(!strcmp(commandstr, "init-stats"))
    {
      printcomment("", "Initializing statistics", "\n");
      init_stats();
    }
  else if(!strcmp(commandstr, "print-stats"))
    {
      print_stats();
    }
  else if(!strcmp(commandstr, "list-params"))
    {
      list_params();
    }
  else if(!strcmp(commandstr, "quit"))
    {
      if(displaying) close_display();
      exit(0);
    }
  else if(!strcmp(commandstr, "lvocfile"))
    {
      sscanf(rest, "%s", lvocfile);
      voc_init("lexical", lvocfile, lwords, &nlwords);
    }
  else if(!strcmp(commandstr, "svocfile"))
    {
      sscanf(rest, "%s", svocfile);
      voc_init("semantic", svocfile, swords, &nswords);
    }
  else if(!strcmp(commandstr, "lrepfile"))
    {
      sscanf(rest, "%s", lrepfile);
      reps_init("lexical", lrepfile, lvocfile, lwords, &nlwords, &nlrep);
    }
  else if(!strcmp(commandstr, "srepfile"))
    {
      sscanf(rest, "%s", srepfile);
      reps_init("semantic", srepfile, svocfile, swords, &nswords, &nsrep);
    }
  else if(!strcmp(commandstr, "qafile"))
    {
      sscanf(rest, "%s", qafile);
      qa_init();
    }
  else if(!strcmp(commandstr, "hfmfile"))
    {
      sscanf(rest, "%s", hfmfile);
      hfm_init();
    }
  else if(!strcmp(commandstr, "hfminpfile"))
    {
      sscanf(rest, "%s", hfminpfile);
      if(displaying)
	{
	  init_hfm_display_params(HFMWINMOD, hfm);
	  XClearArea(theDisplay, Win[HFMWINMOD], 0,0,0,0, True);
	}
    }
  else if(!strcmp(commandstr, "lexfile"))
    {
      sscanf(rest, "%s", lexfile);
      lex_init();
    }
  else if(!strcmp(commandstr, "displaying"))
    {
      read_int_par(rest, commandstr, &displaying);
      if(displaying)
	{
	  if(form==NULL)
	    {
	      printcomment("", "Clearing networks", "\n");
	      display_init();
	    }
	  else
	    XtMapWidget(main_widget);
	  loop_stories();
	}
      else if(!displaying && form!=NULL)
	{
	  XtUnmapWidget(main_widget);
	  loop_stories();
	}
    }
  else if(commandstr[0]=='#');
  else if(!strcmp(commandstr, "nslot"))
    read_int_par(rest, commandstr, &nslot);
  else if(!strcmp(commandstr, "nword"))
    read_int_par(rest, commandstr, &nslot);
  else if(!strcmp(commandstr, "ncase"))
    read_int_par(rest, commandstr, &nslot);
  else if(!strcmp(commandstr, "nsent"))
    read_int_par(rest, commandstr, &nslot);
  else if(!strcmp(commandstr, "chained"))
    read_int_par(rest, commandstr, &chained);
  else if(!strcmp(commandstr, "withlex"))
    read_int_par(rest, commandstr, &withlex);
  else if(!strcmp(commandstr, "withhfm"))
    read_int_par(rest, commandstr, &withhfm);
  else if(!strcmp(commandstr, "babbling"))
    read_int_par(rest, commandstr, &babbling);
  else if(!strcmp(commandstr, "print_mistakes"))
    read_int_par(rest, commandstr, &print_mistakes);
  else if(!strcmp(commandstr, "log_lexicon"))
    read_int_par(rest, commandstr, &log_lexicon);
  else if(!strcmp(commandstr, "ignore_stops"))
    read_int_par(rest, commandstr, &ignore_stops);
  else if(!strcmp(commandstr, "hfmsearch"))
    read_float_par(rest, commandstr, &hfmsearch);
  else if(!strcmp(commandstr, "subsearch"))
    read_float_par(rest, commandstr, &subsearch);
  else if(!strcmp(commandstr, "tracenc"))
    read_int_par(rest, commandstr, &tracenc);
  else if(!strcmp(commandstr, "tsettle"))
    read_int_par(rest, commandstr, &tsettle);
  else if(!strcmp(commandstr, "epsilon"))
    read_float_par(rest, commandstr, &epsilon);
  else if(!strcmp(commandstr, "aliveact"))
    read_float_par(rest, commandstr, &aliveact);
  else if(!strcmp(commandstr, "minact"))
    read_float_par(rest, commandstr, &minact);
  else if(!strcmp(commandstr, "maxact"))
    read_float_par(rest, commandstr, &maxact);
  else if(!strcmp(commandstr, "gammaexc"))
    read_float_par(rest, commandstr, &gammaexc);
  else if(!strcmp(commandstr, "gammainh"))
    read_float_par(rest, commandstr, &gammainh);
  else if(!strcmp(commandstr, "withinerr"))
    read_float_par(rest, commandstr, &withinerr);
  else
    {
      sprintf(s, "Command %s not recognized", commandstr);
      printcomment("", s, "\n");
    }
  if(fp==stdin || fp==NULL) printwordout(DISCERNPROMPT, " ");
}

/*********************  initializations ******************************/

void init_nets(int argc, char **argv)
{
  int i, j;
  FILE *fp;
  
  nmodules=MAXMODULES;
  lwords = lwordsarray+1;
  swords = swordsarray+1;
  for(i=0; i<MAXTASK; i++)
    for(j=0; j<MAXMODULES; j++)
      {
	all[i][j] = allarray[i][j]+1;
	corr[i][j] = corrarray[i][j]+1;
      }

  /* initialization file */
  if(argc > 1) sprintf(data.initfile, "%s", argv[1]);
  if((fp=fopen(data.initfile,"r"))==NULL)
     {
       fprintf(stderr, "cannot open %s, exiting\n", data.initfile);
       exit(1);
     }
  printf("Initializing discern from %s:\n", data.initfile);
  fscanf(fp,"%s", lrepfile); fgl(fp);
  fscanf(fp,"%s", srepfile); fgl(fp);
  fscanf(fp,"%s", qafile); fgl(fp);
  fscanf(fp,"%s", hfmfile); fgl(fp);
  fscanf(fp,"%s", lexfile); fgl(fp);
  fscanf(fp,"%d %d %d %d", &nslot,&nword,&ncase,&nsent);  fgl(fp);
  fscanf(fp,"%d %d %d %d", &chained, &withlex, &withhfm, &displaying); fgl(fp);
  fscanf(fp,"%d %d %d %d", &babbling, &print_mistakes,
	 &log_lexicon, &ignore_stops); fgl(fp);
  fscanf(fp,"%f %f", &hfmsearch, &subsearch); fgl(fp);
  fscanf(fp, "%d %d %f %f", &tracenc, &tsettle, &epsilon,
	 &aliveact); fgl(fp);
  fscanf(fp, "%f %f %f %f", &minact, &maxact, &gammaexc, &gammainh); fgl(fp);
  fscanf(fp, "%f", &withinerr); fgl(fp);
  fclose(fp);

  sprintf(current_inpfile, "%s", data.inpfile);

  /* representations */
  reps_init("lexical", lrepfile, lvocfile, lwords, &nlwords, &nlrep);
  reps_init("semantic", srepfile, svocfile, swords, &nswords, &nsrep);

  nslotrep=nslot*nsrep;
  ncaserep=ncase*nsrep;

  /* networks */
  qa_init();
  hfm_init();
  lex_init();
  clear_traces();
}      


int init_inp(String inpfile, FILE **fp)
{
  /* input parameters */
  if((*fp=fopen(inpfile,"r"))==NULL)
     {
       fprintf(stderr, "cannot open %s\n", inpfile);
       return(0);
     }
  return(1);
}
 

void reps_init(char *lexsem, char *repfile, char *vocfile,
	       struct WORDSTRUCT words[], int *nwords, int *nrep)
{
  int i,j;
  FILE *fp;

  if((fp=fopen(repfile,"r"))==NULL)
     {
       fprintf(stderr, "cannot open %s, exiting\n", repfile);
       exit(1);
     }
  fscanf(fp,"%s", vocfile); fgl(fp);

  voc_init(lexsem, vocfile, words, nwords);

  printf("Reading %s representations from %s...", lexsem, repfile);
  fscanf(fp,"%d", nrep); fgl(fp);
  for(i=0; i< *nwords; i++)
    for(j=0; j< *nrep; j++)
      fscanf(fp, "%f", &words[i].rep[j]);
  fclose(fp);
  printf("Done.\n");
}


void voc_init(char *lexsem, char *vocfile,
	      struct WORDSTRUCT words[], int *nwords)
{
  FILE *fp;
  int i;
  /* read the words */
  if((fp=fopen(vocfile,"r"))==NULL)
     {
       fprintf(stderr, "cannot open %s, exiting\n", vocfile);
       exit(1);
     }
  printf("Reading %s labels from %s...", lexsem, vocfile);
  fscanf(fp,"%s", fillerword); fgl(fp);
  fscanf(fp,"%c", &internal_symbol); fgl(fp);
  fscanf(fp,"%d", &firsttokword); fgl(fp);
  for(i=0; fscanf(fp,"%s", words[i].chars)!=EOF; i++)
    if(!strcmp(words[i].chars, ".")) periodindex=i;
  *nwords=i;
  fclose(fp);
  sprintf(words[-1].chars, "%s", "_");
  printf("Done.\n");
}


/********************* I/O routines ******************************/

void process_inpfile(FILE *fp)
{
  char commandstr[MAXSTRL], rest[MAXSTRL];
  while(fscanf(fp,"%s", commandstr)!=EOF)
    {
      if (!fgetline(fp, rest, MAXSTRL)) fgl(fp);
      process_command(fp, commandstr, rest);
    }
}

void read_story_data(FILE *fp)
{
  register int i,j;

  for(i=0; i<nslot; i++)
    fscanf(fp, "%d",&story.slots[i]); fgl(fp);
  for(i=0; i<nsent; i++)
    {
      fscanf(fp, "%d", &story.sents[i].included);
      for(j=0; j<nword; j++)
	fscanf(fp, "%d", &story.sents[i].words[j]);
      for(j=0; j<ncase; j++)
	fscanf(fp, "%d", &story.sents[i].caseroles[j]);
      fgl(fp);
    }
}

void read_qa_data(FILE *fp)
{
  register int j;

  fscanf(fp, "%d", &qa.question.included);
  for(j=0; j<nword; j++)
    fscanf(fp, "%d", &qa.question.words[j]);
  for(j=0; j<ncase; j++)
    fscanf(fp, "%d", &qa.question.caseroles[j]);
  fgl(fp);

  fscanf(fp, "%d", &qa.answer.included);
  for(j=0; j<nword; j++)
    fscanf(fp, "%d", &qa.answer.words[j]);
  for(j=0; j<ncase; j++)
    fscanf(fp, "%d", &qa.answer.caseroles[j]);
  fgl(fp);
}

void read_text_question(String rest)
{
  char wordstring[MAXSTRL];
  int i=0, j=0;

  for(i=0; i<nslot; i++) qa.slots[i]=-1;
  qa.question.included=1;
  while(j<nword)
    {
      i=0;
      while(i<strlen(rest) && (rest[i] == ' ' || rest[i] == '\t' )) i++;
      if (i<strlen(rest))
	{
	  rest += i;
	  sscanf(rest, "%s", wordstring);
	  rest += strlen(wordstring);
	  for(i=0; i<nswords; i++)
	    if(!strcasecmp(wordstring, swords[i].chars))
	      {
		qa.question.words[j++]=i;
		break;
	      }
	}
      else break;
    }
  for(i=j; i<nword; i++) qa.question.words[i]=-1;
  for(i=0; i<ncase; i++) qa.question.caseroles[i]=-1;
  qa.answer.included=1;
  for(i=0; i<nword; i++) qa.answer.words[i]=-1;
  for(i=0; i<ncase; i++) qa.answer.caseroles[i]=-1;
}

void readfun(FILE *fp, float *place, float par1, float par2)
{
  fscanf(fp,"%f", place);
} 

void read_int_par(String rest, String commandstr, int *variable)
{
  char s[MAXSTRL];
  sscanf(rest, "%d", variable);
  sprintf(s, "Set %s = %d", commandstr, *variable);
  printcomment("", s, "\n");
}

void read_float_par(String rest, String commandstr, float *variable)
{
  char s[MAXSTRL];
  sscanf(rest, "%f", variable);
  sprintf(s, "Set %s = %f", commandstr, *variable);
  printcomment("", s, "\n");
}

void printcomment(char *beginning, char *s, char *ending)
{
  if (babbling)
    printf("%s%s%s%s%s", beginning, BEGIN_COMMENT, s, END_COMMENT, ending);
}

void printwordout(char s[], char ss[])
{
  printf("%s%s",s,ss);
}

void fgl(FILE *fp)
{
  char c;
  while((c=getc(fp)) != EOF && c != '\n'){}
}

int fgetline(FILE *fp, char *s, int lim)
{
  char c;
  int i=0;
  while(--lim>0 && (c=getc(fp)) != EOF && c != '\n')
    s[i++]=c;
  s[i]='\0';
  if(lim==0 && c != EOF && c != '\n') return(0);
  else return(1);
}

void list_params()
{
  printf("lvocfile   = %s\n", lvocfile);
  printf("lrepfile   = %s\n", lrepfile);
  printf("svocfile   = %s\n", svocfile);
  printf("srepfile   = %s\n", srepfile);
  printf("qafile     = %s\n", qafile);
  printf("hfmfile    = %s\n", hfmfile);
  printf("hfminpfile = %s\n", hfminpfile);
  printf("lexfile    = %s\n", lexfile);
  printf("inputfile  = %s\n", current_inpfile);

  printf("nslot=%d, nword=%d, ncase=%d, nsent=%d\n",
	 nslot, nword, ncase, nsent);
  printf("chained=%d, withlex=%d, withhfm=%d\n",
	 chained, withlex, withhfm);
  printf("displaying=%d, babbling=%d, print_mistakes=%d, log_lexicon=%d\n",
	 displaying, babbling, print_mistakes, log_lexicon);
  printf("hfmsearch=%.3f, subsearch=%.3f\n",
	 hfmsearch, subsearch);
  printf("tracenc=%d, tsettle=%d, epsilon=%f, aliveact=%.3f\n",
	 tracenc, tsettle, epsilon, aliveact);
  printf("minact=%.3f, maxact=%.3f, gammaexc=%.3f, gammainh=%.3f\n",
	 minact, maxact, gammaexc, gammainh);
  printf("withinerr=%.3f\n", withinerr);
}

/*********************  math routines ******************************/

float distance(float v1[], float v2[], int nrep)
{
  float sum=0.0;
  register int i;
  for(i=0; i<nrep; i++)
    sum += (v1[i]-v2[i])*(v1[i]-v2[i]);
  return(sqrt(sum));
}

float seldistance(int indeces[], float v1[], float v2[], int ncomp)
{
  float sum=0.0;
  register int i;
  /* count only selected components in the distance */
  for(i=0; i<ncomp; i++)
    sum=sum+(v1[indeces[i]]-v2[i])*(v1[indeces[i]]-v2[i]);
  return(sqrt(sum));
}

/********************* stats routines ******************************/

void init_stats()
{
  int modi, i, taski;
  for(taski=0; taski<MAXTASK; taski++)
    {
      for(modi=0; modi<nmodules; modi++)
	{
	  within[taski][modi]=0;
	  deltasum[taski][modi]=0.0;
	  for(i=(-1); i<((modi==LINPMOD|| modi==LOUTMOD)?nlwords:nswords); i++)
	    corr[taski][modi][i] = all[taski][modi][i]=0;
	}
    }
}

void collect_stats(int modi, float outrep[], int nas, int target[], struct WORDSTRUCT words[], int nrep, int nwords)
{
  int i,nearest,j;
  float uniterror;
  char s[MAXSTRL], ss[MAXSTRL];

  for(i=0; i<nas; i++)
    for(j=0; j<nrep; j++)
    {
      uniterror = fabs(words[target[i]].rep[j] - outrep[i*nrep+j]);
      deltasum[task][modi] += uniterror;
      if (uniterror < withinerr) within[task][modi]++;
    }

  if(babbling && modi!=SENTGENMOD && modi<LINPMOD) printf("| ");
  for(i=0; i<nas; i++)
    {
      all[task][modi][ target[i] ]++;
      nearest = find_nearest_s(&outrep[i*nrep], words, nrep, nwords);
      if(nearest ==  target[i]) corr[task][modi][ target[i] ]++;
      
      if ((babbling && !(!log_lexicon && /* nearest==target[i] &&*/
			 (modi==SOUTMOD || modi==SENTGENMOD || modi==SINPMOD)))
	   || (print_mistakes && nearest!=target[i]))
	{
	  if(nearest==target[i] || text_question)
	    sprintf(s, "%s", words[nearest].chars);
	  else
	    {
	      if(print_mistakes && !babbling) sprintf(ss, "%d:", modi);
	      else sprintf(ss, "%s", "");
	      sprintf(s, "*%s%s(%s)*",
		      ss, words[nearest].chars, words[target[i]].chars);
	    }
	  sprintf(ss, "%s%s", s, (print_mistakes && !babbling) ? "\n" : "");
	  printwordout(ss,
		       ((log_lexicon /*|| nearest!=target[i]*/) &&
			(modi==LINPMOD || modi==SENTGENMOD || modi==SINPMOD)) ?
		       ">" : " ");
	}
    }      
  if(babbling && modi!=SENTGENMOD && modi<LINPMOD) printf("|\n");
}


void print_stats()
{
  print_task_stats("Paraphrasing", PARATASK);
  print_task_stats("Question answering", QATASK);
}

void print_task_stats(char taskname[], int taski)
{
  int modi, nwords, nrep, i, sum_corr, sum_all, sum_corrtok, sum_alltok;

  printf("\n%s performance:\n", taskname);
  printf("Module       All    Syn   <%.2f    Err:\n",withinerr);
  for(modi=0; modi<nmodules; modi++)
    {
      nwords=(modi==LINPMOD || modi==LOUTMOD)?nlwords:nswords;
      nrep=(modi==LINPMOD || modi==LOUTMOD)?nlrep:nsrep;
      sum_corr = sum_all = sum_corrtok = sum_alltok = 0;
      for(i=(-1); i<nwords; i++)
	{
	  sum_corr += corr[taski][modi][i];
	  sum_all += all[taski][modi][i];
	}
      for(i=firsttokword; i<nwords; i++)
	{
	  sum_corrtok += corr[taski][modi][i];
	  sum_alltok += all[taski][modi][i];
	}      
      if(sum_alltok>0)
	{
	  printf("Module %2d: ", modi);
	  printf("%6.1f %6.1f %6.1f %9.4f\n",
		 100.0 * sum_corr / sum_all,
		 100.0 * sum_corrtok / sum_alltok,
		 100.0 * within[taski][modi] / (sum_all * nrep),
		 deltasum[taski][modi] / (sum_all * nrep));
	}
    }
}

int find_nearest_s(float rep[], struct WORDSTRUCT words[], int nrep, int nwords)
{
  int i,bestindex;
  float lbest, dist;
  lbest=LARGEFLOAT;
  for(i=(-1); i<nwords; i++)
    if(strcmp(words[i].chars, fillerword))
      {
	dist= distance(rep,words[i].rep,nrep);
	if(dist<lbest)
	  {
	    bestindex=i;
	    lbest=dist;
	  }
      }
  return(bestindex);
}
