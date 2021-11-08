/*  File: xglobals.c
 * 
 * Declares the common global variables for X graphics
 * Risto Miikkulainen 5/7/93
 *
 */

#ifndef INCLUDED_XGLOBALS  /* to ensure that it is included only once */
#define INCLUDED_XGLOBALS

#ifdef DEFINE_XGLOBALS  /* Defined in the initapp.c file */

Display *theDisplay;   /* Connection to X display */
XtAppContext app_con;
Widget  main_widget, form, runstop, clear, quit, step, command, sentpars,
  storypars, storygen, sentgen, cueformer, answerprod, hfm, lex, sem;
Window  theMain, runstopWin, commandWin, Win[MAXWINS];

/* graphics outline */
NETSTRUCT net[MAXMODULES];

char
  *caselabels[] =
{"AGENT", "ACT", "RECIPIENT", "PAT-ATTR", "PATIENT", "LOCATION"}, 
  *slotlabels[] =
{"SCRIPT", "TRACK", "ACTOR", "ITEM", "PLACE", "ROLE1", "ROLE2"},
  *titles[] =
{"SENTENCE PARSER", "STORY PARSER", "STORY GENERATOR", "SENTENCE GENERATOR",
 "CUE FORMER", "ANSWER PRODUCER", "EPISODIC MEMORY", "unused", "LEXICAL MAP",
 "unused", "SEMANTIC MAP", "unused"};

GC titleGC, logGC, qaGC, qaerrorGC,
  hfmfGC, tracefGC, lexfGC, semfGC,
  hfmbGC, tracebGC, lexbGC, sembGC,
  linefGC, linebGC, clearGC, boxGC, activityGC;

XFontStruct *titlefontStruct, *logfontStruct,
  *qafontStruct, *qaerrorfontStruct,
  *hfmfontStruct, *tracefontStruct,
  *lexfontStruct, *semfontStruct;

int qaboxhght, titleboxhght;

XColor colors[MAXCOLORS];
int Actual_Color_Range;

/* The EV_MASK sets the kind of events one need to respond to, such as
 * mousebutton press, window expose etc. 
 */

int hfmi, hfmj;

XtResource resources[] =
{
  {"netwidth", "Netwidth", XtRDimension, sizeof(Dimension),
  XtOffset(RESOURCE_DATA_PTR, netwidth), XtRString, "512"},
  {"qanetheight", "Qanetheight", XtRDimension, sizeof(Dimension),
  XtOffset(RESOURCE_DATA_PTR, qanetheight), XtRString, "128"},
  {"hfmnetheight", "Hfmnetheight", XtRDimension, sizeof(Dimension),
  XtOffset(RESOURCE_DATA_PTR,hfmnetheight),XtRString, "512"},
  {"lexnetheight", "Lexnetheight", XtRDimension, sizeof(Dimension),
  XtOffset(RESOURCE_DATA_PTR, lexnetheight),XtRString, "255"},

  {"defaultinitfile", "Defaultinitfile", XtRString, sizeof(String),
  XtOffset(RESOURCE_DATA_PTR, initfile), XtRString, "init"},
  {"defaultInputfile", "DefaultInputfile", XtRString, sizeof(String),
  XtOffset(RESOURCE_DATA_PTR, inpfile), XtRString, "input-example"},

  {"tracelinescale", "Tracelinescale", XtRFloat, sizeof(float),
  XtOffset(RESOURCE_DATA_PTR, tracelinescale),XtRString, "1.5"},
  {"tracewidthscale", "Tracewidthscale", XtRFloat, sizeof(float),
  XtOffset(RESOURCE_DATA_PTR, tracewidthscale),XtRString, "0.01"},
  {"reversevalue", "Reversevalue", XtRFloat, sizeof(float),
  XtOffset(RESOURCE_DATA_PTR, reversevalue),XtRString, "0.3"},

  {"textColor", "TextColor", XtRPixel, sizeof(Pixel),
  XtOffset(RESOURCE_DATA_PTR, textColor), XtRString, "white"},
  {"latweightColor", "LatweightColor", XtRPixel, sizeof(Pixel),
  XtOffset(RESOURCE_DATA_PTR, latweightColor), XtRString, "white"},
  {"netColor", "NetColor", XtRPixel, sizeof(Pixel),
  XtOffset(RESOURCE_DATA_PTR, netColor), XtRString, "white"},

  {"commandfont", "Commandfont", XtRString, sizeof(String),
  XtOffset(RESOURCE_DATA_PTR, commandfont), XtRString, "7x13bold"},
  {"titlefont", "Titlefont", XtRString, sizeof(String),
  XtOffset(RESOURCE_DATA_PTR, titlefont), XtRString, "8x13bold"},
  {"logfont", "Logfont", XtRString, sizeof(String),
  XtOffset(RESOURCE_DATA_PTR, logfont), XtRString, "6x10"},
  {"qafont", "Qafont", XtRString, sizeof(String),
  XtOffset(RESOURCE_DATA_PTR, qafont), XtRString, "6x10"},
  {"qaerrorfont", "Qaerrorfont", XtRString, sizeof(String),
  XtOffset(RESOURCE_DATA_PTR, qaerrorfont), XtRString, "5x8"},
  {"hfmfont", "Hfmfont", XtRString, sizeof(String),
  XtOffset(RESOURCE_DATA_PTR, hfmfont), XtRString, "7x13"},
  {"tracefont", "Tracefont", XtRString, sizeof(String),
  XtOffset(RESOURCE_DATA_PTR, tracefont), XtRString, "5x8"},
  {"lexfont", "Lexfont", XtRString, sizeof(String),
  XtOffset(RESOURCE_DATA_PTR, lexfont), XtRString, "5x8"},
  {"semfont", "Semfont", XtRString, sizeof(String),
  XtOffset(RESOURCE_DATA_PTR, semfont), XtRString, "5x8"},
};

RESOURCE_DATA data;

String fallback_resources[] = { 
  "*runstop.left: 	ChainLeft",
  "*runstop.right: 	ChainLeft",
  "*runstop.top:	ChainTop",
  "*runstop.bottom:	ChainTop",
  "*clear.fromHoriz: 	runstop",
  "*clear.left: 	ChainLeft",
  "*clear.right: 	ChainLeft",
  "*clear.top:		ChainTop",
  "*clear.bottom:	ChainTop",
  "*quit.fromHoriz: 	clear",
  "*quit.left:	 	ChainLeft",
  "*quit.right: 	ChainLeft",
  "*quit.top:		ChainTop",
  "*quit.bottom:	ChainTop",
  "*step.fromHoriz: 	quit",
  "*step.left:	 	ChainLeft",
  "*step.right: 	ChainLeft",
  "*step.top:		ChainTop",
  "*step.bottom:	ChainTop",
  "*command.fromHoriz: 	step",
  "*command.left: 	ChainLeft",
  "*command.right: 	ChainRight",
  "*command.top:	ChainTop",
  "*command.bottom:	ChainTop",
  "*sentpars.fromVert: 	runstop",
  "*sentpars.top:	ChainTop",
  "*storypars.fromVert:	sentpars",
  "*cueformer.fromVert:	storypars",
  "*lex.fromVert: 	cueformer",
  "*sem.fromVert: 	lex",
  "*sentgen.fromHoriz:	sentpars",
  "*sentgen.fromVert:	runstop",
  "*sentgen.top:	ChainTop",
  "*storygen.fromHoriz:	storypars",
  "*storygen.fromVert:	sentgen",
  "*answerprod.fromHoriz: cueformer",
  "*answerprod.fromVert: storygen",
  "*hfm.fromHoriz: 	lex",
  "*hfm.fromVert: 	answerprod",

  "*foreground:	        white",
  "*background:		black",
  "*borderColor:	white",

  "*command*translations: #override\n\
	<Key>Return: read_command()",
  "*command*editType:	edit",
  NULL,
};
 
#else

extern Display  *theDisplay;
extern XtAppContext app_con;
extern Widget  main_widget, form, runstop, clear, quit, step, command,
  sentpars, storypars, storygen, sentgen, cueformer, answerprod, hfm, lex, sem;
extern Window   theMain, runstopWin, commandWin, Win[];
extern NETSTRUCT net[];

extern char *caselabels[], *slotlabels[], *titles[];

extern GC titleGC, logGC, qaGC, qaerrorGC,
  hfmfGC, tracefGC, lexfGC, semfGC,
  hfmbGC, tracebGC, lexbGC, sembGC,
  linefGC, linebGC, clearGC, boxGC, activityGC;

extern XFontStruct *titlefontStruct, *logfontStruct,
  *qafontStruct, *qaerrorfontStruct,
  *hfmfontStruct, *tracefontStruct,
  *lexfontStruct, *semfontStruct;

extern int qaboxhght, titleboxhght;

extern XColor colors[];
extern int Actual_Color_Range;
extern int hfmi, hfmj;
extern XtResource resources[];
extern RESOURCE_DATA data;
extern String fallback_resources[];

#endif   /*  #ifdef DEFINE_XGLOBALS */
#endif   /*  #ifndef INCLUDED_XGLOBALS  */
