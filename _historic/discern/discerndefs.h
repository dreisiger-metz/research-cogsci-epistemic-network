/* File: discerndefs.h
 *
 * Defines parameters and data structures for the DISCERN system.
 * Risto Miikkulainen 5/7/93
 */

/*********** general stuff *************/

#define LARGEINT 999999999
#define LARGEFLOAT 999999999.9
#define SNAPSHOTEND LARGEINT	/* token that indicates last snapshot in file*/

#define MAXMODULES 12           /* 6 qanets + store + ret + l + s + ls + sl */
#define MAXQAMODULES 6          /* 6 qanets */
#define MAXREP 12		/* maximum size of lexical & semantic reps */
#define MAXWORDS 300		/* maximum number of lexical & semantic words*/
#define MAXWORDL 30		/* maximum length of input words (chars) */
#define MAXSENT 7
#define MAXWORD 8
#define MAXCASE 6
#define MAXSLOT 7
#define MAXAS (MAXCASE+MAXSLOT)	/* maximum number of assemblies */
#define MAXSLOTREP (MAXSLOT*MAXREP)
#define MAXSNAPS 50		/* maximum number of snapshots */
#define MAXINPSTORIES 100	/* maximum number of stories in epoch */
#define MAXFILENAMEL 100
#define MAXSTRL 100

#define MAXTASK 2		/* maximum number of tasks (para, qa..) */
#define PARATASK 0		/* index to task table */
#define QATASK 1		/* index to task table */

#define SENTPARSMOD 0		/* module # for sentence parser */
#define STORYPARSMOD 1		/* module # for story parser */
#define STORYGENMOD 2		/* module # for story generator */
#define SENTGENMOD 3		/* module # for sentence generator */
#define CUEFORMMOD 4		/* module # for cue former */
#define ANSWERPRODMOD 5		/* module # for answer producer */
#define STOREMOD 6		/* module # for storing into episodic mem */
#define RETMOD 7		/* module # for retrieving from episodic mem */
#define LINPMOD 8		/* module # for lexical input map  */
#define SOUTMOD 9		/* module # for semantic output map  */
#define SINPMOD 10		/* module # for semantic input map  */
#define LOUTMOD 11		/* module # for lexical output map  */

#define HFMWINMOD STOREMOD	/* module # for hfm window */
#define LEXWINMOD LINPMOD	/* module # for lex window */
#define SEMWINMOD SINPMOD	/* module # for sem window */

#define FIRSTQAMOD SENTPARSMOD
#define FIRSTMOD FIRSTQAMOD
#define DISCERNPROMPT "DISCERN>"
#define BEGIN_COMMENT "[ "
#define END_COMMENT " ]"


/* word reps */
typedef struct WORDSTRUCT
{
  char chars[MAXWORDL];
  float rep[MAXREP];
} WORDSTRUCT;

/* story data */
typedef struct SENTSTRUCT
{
  int words[MAXWORD],caseroles[MAXCASE], included;
} SENTSTRUCT;

typedef struct STORYSTRUCT
{
  int slots[MAXSLOT];
  struct SENTSTRUCT sents[MAXSENT];
} STORYSTRUCT;

typedef struct QASTRUCT
{
  int slots[MAXSLOT];
  struct SENTSTRUCT question,answer;
} QASTRUCT;

/************ qa stuff *************/

#define MAXQAUNITS (MAXAS*MAXREP)
#define MAXHIDREP 100              /* maximum size of the representations */

#define HIDREPSTEP 5         /* this must divide the number of hidden units */
#define INPUNITSTEP 4        /* this must divide the number of output units */

/************ lexicon stuff *************/

#define MAXLSNETS 20
#define MAXLEXLABELS 2
#define ANC 1

typedef struct LEXUNIT
{
  float value, prevvalue;
  float comp[MAXREP];
  int   labelcount;
  char  labels[MAXLEXLABELS][MAXWORDL];
} LEXUNIT;

/************ hfm stuff *************/

#define MAXHFMNETS 2		/* maximum network size is n*n units */
#define MAXTRACENETS 8		/* maximum network size is n*n units */

#define MAXHFMLABELS 2
#define MAXTRACELABELS 2
#define MORELABEL "ETC"

typedef struct HFMUNITDATA
{
  float value, prevvalue;
  float comp[MAXSLOTREP];
  int   labelcount;
  char  labels[MAXHFMLABELS][MAXWORDL];
} HFMUNITDATA;

typedef struct TRACEUNITDATA
{
  float value, prevvalue;
  float comp[MAXSLOTREP];
  int   labelcount;
  char  labels[MAXTRACELABELS][MAXWORDL];
  float latweights[MAXTRACENETS][MAXTRACENETS];
} TRACEUNITDATA;

/************ graphics stuff *************/

/* graphics parameters */
#define UNITCOLORS 0           /* chooses bryw colorscale */
#define WEIGHTCOLORS 1         /* chooses gbr colorscale */

#define HIDSP 1
#define VERSP 3
#define HORSP 3
#define BOXSP 1
#define HFMSP 3
#define LEXSP 1
#define PREVSP (3*HORSP)
#define ABOVE  (-qaboxhght)
#define ABOVE2 (-net[neti].uhght-qaboxhght)
#define BELOW  net[neti].uhght
#define BELOW2 (2*net[neti].uhght)

#define MAXWINS MAXMODULES

#define MAXDEPTH 8
#define MINCOLORS 10
#define MAXCOLORS 256

typedef struct NETSTRUCT
{
  int
    height, width,
    uwidth, uhght, u1width, u1hght, u2width, u2hght,
    marg, inpx, inp1x, prevx, hidx, outx, tchx,
    inpy, inp1y, prevy, hidy, outy, tchy,
    hsp, columns, ninpitem, noutitem;

  char sequence[MAXWORD*(2*MAXWORDL+5)], newitem[2*MAXWORDL+5];
  char log[MAXSTRL];
} NETSTRUCT;

typedef struct RESOURCE_DATA
{
  Dimension netwidth, qanetheight, hfmnetheight, lexnetheight;
  String inpfile, initfile;
  Pixel textColor, latweightColor, netColor;
  String commandfont, titlefont, logfont, qafont, qaerrorfont,
  hfmfont, tracefont, lexfont, semfont;
  float tracelinescale, tracewidthscale, reversevalue;
} RESOURCE_DATA, *RESOURCE_DATA_PTR;


