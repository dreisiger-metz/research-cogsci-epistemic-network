/* File: globals.c
 *
 * Defines global variables for the DISCERN system.
 * Risto Miikkulainen 5/7/93
 */

#ifndef INCLUDED_GLOBALS  /* to make sure this file is included only once */
#define INCLUDED_GLOBALS

#ifdef DEFINE_GLOBALS  /* Defined in the main.c file */

/************ general stuff *************/

int nlwords, nlrep, nswords, nsrep, nmodules;
int chained, withlex, withhfm, displaying, babbling, print_mistakes,
  log_lexicon, ignore_stops, text_question, task;

/* word reps */
WORDSTRUCT lwordsarray[MAXWORDS+1], *lwords, swordsarray[MAXWORDS+1], *swords;

/* story data */
STORYSTRUCT story;
QASTRUCT qa;

/* active reps on the pathways */
float swordrep[MAXREP], caserep[MAXCASE*MAXREP], slotrep[MAXSLOTREP];
int nslotrep, ncaserep;

/* statistics */
int within[MAXTASK][MAXMODULES],
  allarray[MAXTASK][MAXMODULES][MAXWORDS+1], *all[MAXTASK][MAXMODULES],
  corrarray[MAXTASK][MAXMODULES][MAXWORDS+1], *corr[MAXTASK][MAXMODULES];
float deltasum[MAXTASK][MAXMODULES], withinerr;

/* files */
char lrepfile[MAXFILENAMEL], lvocfile[MAXFILENAMEL],
  srepfile[MAXFILENAMEL], svocfile[MAXFILENAMEL],
  qafile[MAXFILENAMEL], hfmfile[MAXFILENAMEL],
  hfminpfile[MAXFILENAMEL], lexfile[MAXFILENAMEL],
  current_inpfile[MAXFILENAMEL];
int simulator_running, stepping;


/************ qa stuff *************/

int nqanets, nslot, nword, ncase, nsent;

/* actual table dimensions */
int nhidrep[MAXQAMODULES], ninpunits[MAXQAMODULES], noutunits[MAXQAMODULES];

/* units and weights */
float inprep[MAXQAMODULES][MAXQAUNITS], outrep[MAXQAMODULES][MAXQAUNITS],
  tchrep[MAXQAMODULES][MAXQAUNITS],
  prevhidrep[MAXQAMODULES][MAXHIDREP], hidrep[MAXQAMODULES][MAXHIDREP],
  hidbias[MAXQAMODULES][MAXHIDREP], outbias[MAXQAMODULES][MAXQAUNITS],
  wih[MAXQAMODULES][MAXQAUNITS][MAXHIDREP],
  who[MAXQAMODULES][MAXQAUNITS][MAXHIDREP],
  wph[MAXQAMODULES][MAXHIDREP][MAXHIDREP];

int inputs[MAXQAMODULES][MAXAS], targets[MAXQAMODULES][MAXAS];

/************ lexicon stuff *************/

int lnets, snets, abesti, abestj;
int firsttokword, periodindex;

LEXUNIT lunits[MAXLSNETS][MAXLSNETS], sunits[MAXLSNETS][MAXLSNETS];

float lsassoc[MAXLSNETS][MAXLSNETS][MAXLSNETS][MAXLSNETS],
      slassoc[MAXLSNETS][MAXLSNETS][MAXLSNETS][MAXLSNETS];

char fillerword[MAXWORDL], internal_symbol;

/************ hfm stuff *************/

int hfmnets, subnets, bnets, besti, bestj,
  besti0, bestj0, besti1, bestj1, besti2, bestj2;
float inpvector[MAXSLOTREP], outvector[MAXSLOTREP];

float hfmsearch, subsearch;

int indeces[MAXHFMNETS][MAXHFMNETS][MAXSLOTREP],
  nsublines[MAXHFMNETS][MAXHFMNETS],
  indeces2[MAXHFMNETS][MAXHFMNETS][MAXHFMNETS][MAXHFMNETS][MAXSLOTREP],
  nblines[MAXHFMNETS][MAXHFMNETS][MAXHFMNETS][MAXHFMNETS];

HFMUNITDATA hfmunits[MAXHFMNETS][MAXHFMNETS],
  subunits[MAXHFMNETS][MAXHFMNETS][MAXHFMNETS][MAXHFMNETS];
TRACEUNITDATA bunits[MAXHFMNETS][MAXHFMNETS][MAXHFMNETS][MAXHFMNETS][MAXTRACENETS][MAXTRACENETS];

/************ trace stuff *************/

int tracenc, tsettle;
float minact, maxact, gammaexc, gammainh, bestvalue, aliveact, epsilon;

/************ end definitions *************/

#else

extern int nlwords, nlrep, nswords, nsrep, nmodules;
extern int chained, withlex, withhfm, displaying, babbling, print_mistakes,
  log_lexicon, ignore_stops, text_question, task;

extern WORDSTRUCT lwordsarray[MAXWORDS+1], *lwords, swordsarray[MAXWORDS+1], *swords;
extern STORYSTRUCT story;
extern QASTRUCT qa;
extern float swordrep[MAXREP], caserep[MAXCASE*MAXREP], slotrep[MAXSLOTREP];
extern int nslotrep, ncaserep;
extern int inputs[MAXQAMODULES][MAXAS], targets[MAXQAMODULES][MAXAS];
extern int within[MAXTASK][MAXMODULES],
  allarray[MAXTASK][MAXMODULES][MAXWORDS+1], *all[MAXTASK][MAXMODULES],
  corrarray[MAXTASK][MAXMODULES][MAXWORDS+1], *corr[MAXTASK][MAXMODULES];
extern float deltasum[MAXTASK][MAXMODULES], withinerr;
extern char lrepfile[], lvocfile[],
  srepfile[], svocfile[], qafile[], hfmfile[],
  hfminpfile[], lexfile[],  current_inpfile[MAXFILENAMEL];
extern int simulator_running, stepping;
extern int nqanets, nslot, nword, ncase, nsent;
extern int nhidrep[MAXQAMODULES], ninpunits[MAXQAMODULES],
  noutunits[MAXQAMODULES];
extern float inprep[MAXQAMODULES][MAXQAUNITS],
  outrep[MAXQAMODULES][MAXQAUNITS],
  tchrep[MAXQAMODULES][MAXQAUNITS],
  prevhidrep[MAXQAMODULES][MAXHIDREP], hidrep[MAXQAMODULES][MAXHIDREP],
  hidbias[MAXQAMODULES][MAXHIDREP], outbias[MAXQAMODULES][MAXQAUNITS],
  wih[MAXQAMODULES][MAXQAUNITS][MAXHIDREP],
  who[MAXQAMODULES][MAXQAUNITS][MAXHIDREP],
  wph[MAXQAMODULES][MAXHIDREP][MAXHIDREP];
extern int lnets, snets, abesti, abestj;
extern int firsttokword, periodindex;
extern LEXUNIT lunits[MAXLSNETS][MAXLSNETS], sunits[MAXLSNETS][MAXLSNETS];
extern float lsassoc[MAXLSNETS][MAXLSNETS][MAXLSNETS][MAXLSNETS],
      slassoc[MAXLSNETS][MAXLSNETS][MAXLSNETS][MAXLSNETS];
extern char fillerword[MAXWORDL], internal_symbol;
extern int hfmnets, subnets, bnets, besti, bestj,
  besti0, bestj0, besti1, bestj1, besti2, bestj2;
extern float hfmsearch, subsearch;
extern float inpvector[MAXSLOTREP], outvector[MAXSLOTREP];
extern int indeces[MAXHFMNETS][MAXHFMNETS][MAXSLOTREP],
  nsublines[MAXHFMNETS][MAXHFMNETS],
  indeces2[MAXHFMNETS][MAXHFMNETS][MAXHFMNETS][MAXHFMNETS][MAXSLOTREP],
  nblines[MAXHFMNETS][MAXHFMNETS][MAXHFMNETS][MAXHFMNETS];
extern HFMUNITDATA hfmunits[MAXHFMNETS][MAXHFMNETS],
  subunits[MAXHFMNETS][MAXHFMNETS][MAXHFMNETS][MAXHFMNETS];
extern TRACEUNITDATA bunits[MAXHFMNETS][MAXHFMNETS][MAXHFMNETS][MAXHFMNETS][MAXTRACENETS][MAXTRACENETS];
extern int tracenc, tsettle;
extern float minact, maxact, gammaexc, gammainh, bestvalue,
  aliveact, epsilon;

#endif   /*  #ifdef DEFINE_GLOBALS */
#endif   /*  #ifndef INCLUDED_GLOBALS */
