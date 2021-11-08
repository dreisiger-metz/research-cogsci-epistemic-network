/* File: graph.c
 *
 * X interface for DISCERN
 * Risto Miikkulainen 5/7/93
 */

#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/AsciiText.h>

#include "discerndefs.h"
#include "Gwin.h"
#include "globals.c"
#include "xglobals.c"

/* Function prototypes */

void frameRectangle(int neti, int x, int y, int width, int height, int colorindex);
void labelbox(int neti, int x, int y, int width, int height, float value, char labels[][MAXWORDL], int labelcount, XFontStruct *fontstruct, GC fGC, GC bGC);
void collect_uniq_labels(char labels[][MAXWORDL], int *count, char label[], int maxlabels);
void collect_labels(char labels[][MAXWORDL], int *count, char label[], int maxlabels);
void display_init();
void display_cleanup();
void display_init_net(int neti);
void display_lex(int neti, int nets, struct LEXUNIT units[MAXLSNETS][MAXLSNETS]);
void display_lex_error(int neti, int index, struct LEXUNIT units[MAXLSNETS][MAXLSNETS], int besti, int bestj, int nwords, struct WORDSTRUCT words[], int nrep);
void display_hfm_values();
void display_hfm_error(int neti, float outvector[], int slots[]);
void display_hfm_box(int neti, int nets, int x, int y, int width, int height, XFontStruct *fontstruct, HFMUNITDATA units[MAXHFMNETS][MAXHFMNETS]);
void display_trace_box(int neti, int nets, int x, int y, int width, int height, XFontStruct *fontstruct, TRACEUNITDATA units[MAXTRACENETS][MAXTRACENETS]);
void display_hfm_top();
void display_hfm_sub(int i, int j);
void display_hfm_b(int i, int j, int ii, int jj);
int newlabel(char labels[][MAXWORDL], int count, char *label);
void find_max_resp(float inpvector[], char inplabel[], int dim, int nets, struct LEXUNIT units[MAXLSNETS][MAXLSNETS]);
int imin(int a, int b);
int imax(int a, int b);
void display_labels(int neti, int x, int y, char *labels[], int nitem);
void display_title(int neti, char name[]);
void display_log(int neti);
void handle_events();
Window OpenWindow(int x, int y, int width, int height);
int createGC(Window New_win, GC *New_GC, Font fid, Pixel FGpix,Pixel theBGpix);
void drawLine(int neti, int x1, int y1, int x2, int y2, GC currGC, int width);
void fillRectangle(int neti, int x, int y, int width, int height, int colorindex);
void clearRectangle(int neti, int x, int y, int width, int height);
void drawRectangle(int neti, int x, int y, int width, int height);
void drawText(int neti, int x, int y, char text[], GC currGC);
void drawoverText(int neti, int x, int y, char text[], GC currGC);
XFontStruct *loadFont(char fontName[]);
void create_colormap(XColor colors[]);
void display_labeled_layer(int neti, int nas, float rep[], int nums[], int x, int y, int labeloffset);
void display_sequence(int neti, int x, int y);
void display_error(int neti, float outrep[], int nas, int target[], struct WORDSTRUCT words[], int nrep, int step);
void display_nearest(int neti, float rep[], int nrep);
void display_layer(int neti, int nas, float layer[], int layerx, int layery, int nrep);
void display_assembly(int neti, int startx, int starty, float assembly[], int nrep);
void display_boxword(int neti, int x, int y, int width, int height, char wordchars[], int index, XFontStruct *fontStruct, GC currGC);
int trans_to_color(float value, int map);
static void toggle_callback(Widget w, XtPointer client_data, XtPointer call_data);
static void quit_callback(Widget w,XtPointer client_data, XtPointer call_data);
void close_display();
static void runstop_callback(Widget w,XtPointer client_data,XtPointer call_data);
static void clear_callback(Widget w,XtPointer client_data,XtPointer call_data);
void clear_networks();
static void expose_sentpars(Widget w, XtPointer client_data, XtPointer call_data);
static void expose_storypars(Widget w, XtPointer client_data, XtPointer call_data);
static void expose_sentgen(Widget w, XtPointer client_data, XtPointer call_data);
static void expose_storygen(Widget w, XtPointer client_data, XtPointer call_data);
static void expose_cueformer(Widget w, XtPointer client_data, XtPointer call_data);
static void expose_answerprod(Widget w, XtPointer client_data, XtPointer call_data);
static void expose_hfm(Widget w, XtPointer client_data, XtPointer call_data);
static void expose_lex(Widget w, XtPointer units, XtPointer call_data);
void wait_for_run();
void start_running();
static void Read_command(Widget w, XEvent *ev, String *params, Cardinal *num_params);
int nfree_color_cells(Display *theDisplay, Colormap colormap, int ncolors);
void init_common_qa_display_params(int neti, Widget w);
void init_hfm_display_params(int neti, Widget w);
void init_lex_display_params(int neti, Widget w, int nets, int nwords, struct WORDSTRUCT words[], int nrep, struct LEXUNIT units[MAXLSNETS][MAXLSNETS]);
void common_resize(int neti, Widget w);
void common_qa_resize(int neti);
static void resize_sentpars(Widget w, XtPointer client_data,
			    XtPointer call_data);
static void resize_storypars(Widget w, XtPointer client_data,
			     XtPointer call_data);
static void resize_storygen(Widget w, XtPointer client_data,
			     XtPointer call_data);
static void resize_sentgen(Widget w, XtPointer client_data,
			     XtPointer call_data);
static void resize_cueformer(Widget w, XtPointer client_data,
			     XtPointer call_data);
static void resize_answerprod(Widget w, XtPointer client_data,
			     XtPointer call_data);
static void resize_hfm(Widget w, XtPointer client_data,
		       XtPointer call_data);
static void resize_lex(Widget w, XtPointer client_data, XtPointer call_data);


float distance(float v1[], float v2[], int nrep);
int find_nearest_s(float rep[], struct WORDSTRUCT words[], int nrep, int nwords);
void classify();
void hfm_null_values();
void hfm_null_prevvalues();
void clear_traces();
void lex_clear_values(struct LEXUNIT units[MAXLSNETS][MAXLSNETS], int nets);
void loop_stories();
void lex_clear_prevvalues(struct LEXUNIT units[MAXLSNETS][MAXLSNETS], int nets);
void printcomment(char *beginning, char *s, char *ending);
void fgl(FILE *fp);
void process_command(FILE *fp, String commandstr, String rest);

/********************* general initialization ***********************/

void display_init()
{
  Arg args[3];
  char s[MAXSTRL];
  Pixel theBGpix;
  Dimension borderwidth, height, width, tot_width;
  static XtActionsRec command_actions[] =
    {
      {"read_command", Read_command},
    };

  printf("Initializing graphics...\n");

  XtSetArg(args[0], XtNborderWidth, &borderwidth);
  XtGetValues(main_widget, args, 1);
  XtSetArg(args[0], XtNdefaultDistance, -borderwidth);
  form = XtCreateManagedWidget("form",formWidgetClass, main_widget,args,1);

  runstop = XtCreateManagedWidget("runstop",commandWidgetClass, form, args, 1);
  clear = XtCreateManagedWidget("clear", commandWidgetClass, form, args, 1);
  quit = XtCreateManagedWidget("quit", commandWidgetClass, form, args, 1);
  step = XtCreateManagedWidget("step", toggleWidgetClass, form, args, 1);

  XtSetArg(args[0], XtNheight, &height);
  XtSetArg(args[1], XtNwidth, &width);
  XtGetValues(runstop, args, 2); tot_width=width+borderwidth;
  XtSetArg(args[0], XtNwidth, &width);
  XtGetValues(clear, args, 1); tot_width+=width+borderwidth;
  XtSetArg(args[0], XtNwidth, &width);
  XtGetValues(quit, args, 1); tot_width+=width+borderwidth;
  XtSetArg(args[0], XtNwidth, &width);
  XtGetValues(step, args, 1); tot_width+=width+borderwidth;
  XtSetArg(args[0], XtNheight, height);
  XtSetArg(args[1], XtNwidth, 2*data.netwidth+borderwidth-tot_width);
  sprintf(s, "file %s", current_inpfile);
  XtSetArg(args[2], XtNstring, s);
  command=XtCreateManagedWidget("command",asciiTextWidgetClass,form,args,3);

  XtSetArg(args[0], XtNwidth, data.netwidth);
  XtSetArg(args[1], XtNheight, data.qanetheight);
  sentpars  = XtCreateManagedWidget("sentpars",  gwinWidgetClass,form,args,2);
  storypars = XtCreateManagedWidget("storypars", gwinWidgetClass,form,args,2);
  cueformer = XtCreateManagedWidget("cueformer", gwinWidgetClass,form,args,2);
  sentgen   = XtCreateManagedWidget("sentgen",   gwinWidgetClass,form,args,2);
  storygen  = XtCreateManagedWidget("storygen",  gwinWidgetClass,form,args,2);
  answerprod= XtCreateManagedWidget("answerprod",gwinWidgetClass,form,args,2);

  XtSetArg(args[1], XtNheight, data.lexnetheight);
  lex=XtCreateManagedWidget("lex",gwinWidgetClass,form,args,2);
  sem=XtCreateManagedWidget("sem",gwinWidgetClass,form,args,2);
  XtSetArg(args[1], XtNheight, data.hfmnetheight);
  hfm=XtCreateManagedWidget("hfm",gwinWidgetClass,form,args,2);

  XtAddCallback(runstop, XtNcallback, runstop_callback, NULL);
  XtAddCallback(clear, XtNcallback, clear_callback, NULL);
  XtAddCallback(quit, XtNcallback, quit_callback, NULL);
  XtAddCallback(step, XtNcallback, toggle_callback, NULL);

  XtAddCallback(sentpars, XtNexposeCallback, expose_sentpars, NULL);
  XtAddCallback(storypars, XtNexposeCallback, expose_storypars, NULL);
  XtAddCallback(cueformer, XtNexposeCallback, expose_cueformer, NULL);
  XtAddCallback(sentgen, XtNexposeCallback, expose_sentgen, NULL);
  XtAddCallback(storygen, XtNexposeCallback, expose_storygen, NULL);
  XtAddCallback(answerprod, XtNexposeCallback, expose_answerprod, NULL);
  XtAddCallback(hfm, XtNexposeCallback, expose_hfm, NULL);
  XtAddCallback(lex, XtNexposeCallback, expose_lex, lunits);
  XtAddCallback(sem, XtNexposeCallback, expose_lex, sunits);

  XtAddCallback(sentpars, XtNresizeCallback, resize_sentpars, NULL);
  XtAddCallback(storypars, XtNresizeCallback, resize_storypars, NULL);
  XtAddCallback(cueformer, XtNresizeCallback, resize_cueformer, NULL);
  XtAddCallback(sentgen, XtNresizeCallback, resize_sentgen, NULL);
  XtAddCallback(storygen, XtNresizeCallback, resize_storygen, NULL);
  XtAddCallback(answerprod, XtNresizeCallback, resize_answerprod, NULL);
  XtAddCallback(hfm, XtNresizeCallback, resize_hfm, NULL);
  XtAddCallback(lex, XtNresizeCallback, resize_lex, NULL);
  XtAddCallback(sem, XtNresizeCallback, resize_lex, NULL);

  XtAppAddActions(app_con, command_actions, XtNumber(command_actions));
  
  XtRealizeWidget(main_widget);
  theDisplay = XtDisplay(main_widget);
  theMain = XtWindow(main_widget);
  runstopWin=XtWindow(runstop);
  commandWin=XtWindow(command);
  XtSetKeyboardFocus(main_widget, command);
  
  init_common_qa_display_params(SENTPARSMOD, sentpars);
  init_common_qa_display_params(STORYPARSMOD, storypars);
  init_common_qa_display_params(STORYGENMOD, storygen);
  init_common_qa_display_params(SENTGENMOD, sentgen);
  init_common_qa_display_params(CUEFORMMOD, cueformer);
  init_common_qa_display_params(ANSWERPRODMOD, answerprod);
  init_hfm_display_params(HFMWINMOD, hfm);
  init_lex_display_params(LEXWINMOD,lex,lnets, nlwords, lwords, nlrep, lunits);
  init_lex_display_params(SEMWINMOD,sem,snets, nswords, swords, nsrep, sunits);

  create_colormap(colors);
  XtSetArg(args[0], XtNbackground, &theBGpix);
  XtGetValues(main_widget, args, 1);
  
  XtSetArg(args[0], XtNfont, loadFont(data.commandfont));
  XtSetValues(runstop, args, 1);
  XtSetValues(clear, args, 1);
  XtSetValues(quit, args, 1);
  XtSetValues(step, args, 1);
  XtSetValues(command, args, 1);

  titlefontStruct = loadFont(data.titlefont);
  logfontStruct = loadFont(data.logfont);
  qafontStruct = loadFont(data.qafont);
  qaerrorfontStruct = loadFont(data.qaerrorfont);
  hfmfontStruct = loadFont(data.hfmfont);
  tracefontStruct = loadFont(data.tracefont);
  lexfontStruct = loadFont(data.lexfont);
  semfontStruct = loadFont(data.semfont);

  titleboxhght = titlefontStruct->ascent+titlefontStruct->descent+2*BOXSP;
  qaboxhght = qafontStruct->ascent+qafontStruct->descent+2*BOXSP;

  createGC(theMain, &titleGC, titlefontStruct->fid, data.textColor, theBGpix);
  createGC(theMain, &logGC, logfontStruct->fid, data.textColor, theBGpix);
  createGC(theMain, &qaGC, qafontStruct->fid, data.textColor, theBGpix);
  createGC(theMain, &qaerrorGC,qaerrorfontStruct->fid,data.textColor,theBGpix);

  createGC(theMain, &hfmfGC, hfmfontStruct->fid, data.textColor, theBGpix);
  createGC(theMain, &tracefGC, tracefontStruct->fid, data.textColor, theBGpix);
  createGC(theMain, &lexfGC, lexfontStruct->fid, data.textColor, theBGpix);
  createGC(theMain, &semfGC, semfontStruct->fid, data.textColor, theBGpix);

  createGC(theMain, &hfmbGC, hfmfontStruct->fid, theBGpix, theBGpix);
  createGC(theMain, &tracebGC, tracefontStruct->fid, theBGpix, theBGpix);
  createGC(theMain, &lexbGC, lexfontStruct->fid, theBGpix, theBGpix);
  createGC(theMain, &sembGC, semfontStruct->fid, theBGpix, theBGpix);

  createGC(theMain, &linefGC, logfontStruct->fid,data.latweightColor,theBGpix);
  createGC(theMain, &linebGC, logfontStruct->fid, theBGpix, theBGpix);
  createGC(theMain, &clearGC, logfontStruct->fid, theBGpix, theBGpix);
  createGC(theMain, &boxGC, logfontStruct->fid, data.netColor, theBGpix);
  createGC(theMain, &activityGC, logfontStruct->fid, theBGpix, theBGpix);

  resize_sentpars(sentpars, NULL, NULL);
  resize_storypars(storypars, NULL, NULL);
  resize_storygen(storygen, NULL, NULL);
  resize_sentgen(sentgen, NULL, NULL);
  resize_cueformer(cueformer, NULL, NULL);
  resize_answerprod(answerprod, NULL, NULL);
  resize_hfm(hfm, NULL, NULL);
  resize_lex(lex, NULL, NULL);
  resize_lex(sem, NULL, NULL);
  printf("Graphics initialization complete.\n");
}

void common_resize(int neti, Widget w)
{
  Arg args[2];
  Dimension width, height;
  XtSetArg(args[0], XtNwidth, &width);
  XtSetArg(args[1], XtNheight, &height);
  XtGetValues(w, args, 2);
  net[neti].width=width;
  net[neti].height=height;
}



void handle_events()
  /* Event Handling loop */
{
  XEvent theEvent;
  while(XtAppPending(app_con))
    {
      XtAppNextEvent(app_con, &theEvent);
      if(!(theEvent.type==Expose && theEvent.xexpose.count>0))
	{
	  XtDispatchEvent(&theEvent);
	  XFlush(theDisplay);
	}
    }
}

static void runstop_callback(Widget w,
			     XtPointer client_data, XtPointer call_data)
{
  if (simulator_running==1) wait_for_run();
  else start_running();
}

static void toggle_callback(Widget w,
			    XtPointer client_data, XtPointer call_data)
{
  stepping = !stepping;
}

void wait_for_run()
{
  Arg args[1];
  simulator_running=0;
  XtSetArg(args[0], XtNlabel, "Run");
  XtSetValues(runstop, args, 1);
  XFlush(theDisplay);
  while (simulator_running==0)
    handle_events();
}

void start_running()
{
  Arg args[1];
  simulator_running=1;
  XtSetArg(args[0], XtNlabel, "Stop");
  XtSetValues(runstop, args, 1);
}


static void Read_command(Widget w, XEvent *ev, String *params,
			      Cardinal *num_params)
{
    Arg args[1];
    String str;
    char commandstr[MAXSTRL], rest[MAXSTRL];

    XtSetArg(args[0], XtNstring, &str);
    XtGetValues(command, args, 1);

    printf("%s\n", str);
    sscanf(str, "%s", commandstr);
    strcpy(rest, str+strlen(commandstr));

    XtSetArg(args[0], XtNstring, "");
    XtSetValues(command, args, 1);

    process_command(NULL, commandstr, rest);
}

static void clear_callback(Widget w,XtPointer client_data, XtPointer call_data)
{
  printcomment("\n", "Clearing networks", "\n");
  clear_networks();
  loop_stories();
}

void clear_networks()
{
  int neti,i;

  for(neti=FIRSTQAMOD; neti<FIRSTQAMOD+nqanets; neti++)
    {
      for(i=0; i<net[neti].ninpitem; i++) inputs[neti][i]=(-1.0);
      for(i=0; i<net[neti].noutitem; i++) targets[neti][i]=(-1.0);
      for(i=0; i<ninpunits[neti]; i++) inprep[neti][i]=0.0;
      for(i=0; i<noutunits[neti]; i++) outrep[neti][i]=0.0;
      for(i=0; i<noutunits[neti]; i++) tchrep[neti][i]=0.0;
      for(i=0; i<nhidrep[neti]; i++) hidrep[neti][i]=0.0;
      for(i=0; i<nhidrep[neti]; i++) prevhidrep[neti][i]=0.0;
      sprintf(net[neti].sequence, "%s", "");
      sprintf(net[neti].log, "%s", "");
      XClearArea(theDisplay, Win[neti], 0,0,0,0, True);
    }
  hfm_null_values();
  hfm_null_prevvalues();
  clear_traces();
  sprintf(net[HFMWINMOD].log, "%s", "");
  XClearArea(theDisplay, Win[HFMWINMOD], 0,0,0,0, True);

  lex_clear_values(lunits, lnets);
  lex_clear_prevvalues(lunits, lnets);
  sprintf(net[LEXWINMOD].log, "%s", "");
  XClearArea(theDisplay, Win[LEXWINMOD], 0,0,0,0, True);

  lex_clear_values(sunits, snets);
  lex_clear_prevvalues(sunits, snets);
  sprintf(net[SEMWINMOD].log, "%s", "");
  XClearArea(theDisplay, Win[SEMWINMOD], 0,0,0,0, True);

}

static void quit_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
  close_display();
  exit(0);
}

void close_display()
{
  XFreeFont(theDisplay, titlefontStruct);
  XFreeFont(theDisplay, logfontStruct);
  XFreeFont(theDisplay, qafontStruct);
  XFreeFont(theDisplay, qaerrorfontStruct);
  XFreeFont(theDisplay, hfmfontStruct);
  XFreeFont(theDisplay, tracefontStruct);
  XFreeFont(theDisplay, lexfontStruct);
  XFreeFont(theDisplay, semfontStruct);
  XCloseDisplay(theDisplay);
}

void create_colormap(XColor colors[])
/* Allocates colors in the colormap for the display passed as a parameter.*/
{
  Colormap colormap;
  int i, rangeby3, value, depth, maxcolors;
  float multiplier;
  Visual *visual;

  visual=DefaultVisual(theDisplay, DefaultScreen(theDisplay));
  depth= DefaultDepth(theDisplay,  DefaultScreen(theDisplay));
  colormap = DefaultColormap(theDisplay,DefaultScreen(theDisplay));

  maxcolors = imin( (int)pow(2.0,(double)depth), MAXCOLORS);
  Actual_Color_Range = nfree_color_cells(theDisplay, colormap, maxcolors);
  if(Actual_Color_Range<MINCOLORS)
    Actual_Color_Range = maxcolors;
  Actual_Color_Range--;

  if(!(visual->class==GrayScale || visual->class==StaticGray))
    {
      rangeby3   = Actual_Color_Range/3;
      Actual_Color_Range = (Actual_Color_Range/3)*3;
      multiplier = 65535.0/rangeby3;
      for(i=0; i<=Actual_Color_Range; i++ )
	{
	  colors[i].green = 0;
	  colors[i].blue  = 0;
	  if ( i<rangeby3 )
	    colors[i].red = (int) (i*multiplier);
	  else
	    {
	      colors[i].red = 65535;
	      if ( i<2*rangeby3)
		colors[i].green = (int) ((i-rangeby3) * multiplier);
	      else
		{
		  colors[i].green = 65535;
		  colors[i].blue  = (int) ((i-2*rangeby3)*multiplier);
		}
	    }
	  colors[i].flags = DoRed | DoGreen | DoBlue;
	  if ( XAllocColor(theDisplay, colormap, &(colors[i]) )==0)
	    {
	      fprintf(stderr, "Color %d not allocated \n", i);
	      Actual_Color_Range--;
	    }
	}
    }
  else
    {
      multiplier = 65535.0/Actual_Color_Range;
      for(i=0; i<=Actual_Color_Range; i++ )
	{
	  value = (int) (i*multiplier);
	  colors[i].red   = value;
	  colors[i].green = value;
	  colors[i].blue  = value;
	  colors[i].flags = DoRed | DoGreen | DoBlue;
	  if ( XAllocColor(theDisplay, colormap, &(colors[i]) )==0)
	    {
	      fprintf(stderr, "Color %d not allocated \n", i);
	      Actual_Color_Range--;
	    }
	}
    }
  
}

int nfree_color_cells(Display *theDisplay, Colormap colormap, int npixels)
{
  unsigned long planes[1], pixels[MAXCOLORS];
  while(npixels>=MINCOLORS)
    if(!XAllocColorCells(theDisplay,colormap,False,planes,0,pixels,npixels))
      npixels--;
    else
      {
	XFreeColors(theDisplay, colormap, pixels, npixels, 0);
	break;
      }
  return(npixels);
}


int createGC(Window New_win, GC *New_GC, Font fid,
	     Pixel theFGpix, Pixel theBGpix)
{
  XGCValues GCValues;  /* Put in the graphics context values in here */

  *New_GC = XCreateGC(theDisplay, New_win, (unsigned long)0, &GCValues);

  if (*New_GC == 0)
    return(0);
  else
    {
      XSetFont(theDisplay, *New_GC, fid);
      XSetForeground(theDisplay, *New_GC, theFGpix);
      XSetBackground(theDisplay, *New_GC, theBGpix);
      return(1);
    }
}


XFontStruct *loadFont(char fontName[])
{
  XFontStruct *fontStruct;

  if ((fontStruct = XLoadQueryFont(theDisplay, fontName))==NULL)
     {
       fprintf(stderr, "Cannot load font: %s, using fixed\n", fontName);
       if ((fontStruct = XLoadQueryFont(theDisplay, "fixed"))==NULL)
	 {
	   fprintf(stderr, "Cannot load fixed font, exiting\n");
	   exit(1);
	 }
     }
  return(fontStruct);
}

/********************* qa module operations ***********************/

void init_common_qa_display_params(int neti, Widget w)
{
  int i;
  Win[neti] = XtWindow(w);
  net[neti].ninpitem=ninpunits[neti]/nsrep;
  net[neti].noutitem=noutunits[neti]/nsrep;
  for(i=0; i< net[neti].ninpitem; i++) inputs[neti][i]=(-1.0);
  for(i=0; i< net[neti].noutitem; i++) targets[neti][i]=(-1.0);

  if (neti==ANSWERPRODMOD) net[neti].columns=nslot;
  else net[neti].columns = imax(net[neti].ninpitem, net[neti].noutitem);
}

static void expose_sentpars(Widget w, XtPointer client_data,
			    XtPointer call_data)
{
  int neti=SENTPARSMOD;
  XClearWindow(theDisplay, Win[neti]);
  display_title(neti, titles[neti]);
  display_labels(neti, net[neti].tchx,
		 net[neti].tchy+net[neti].uhght+qaboxhght,
		 caselabels, ncase);
  display_labeled_layer(neti, net[neti].noutitem,
			tchrep[neti], targets[neti],
			net[neti].tchx, net[neti].tchy,
			BELOW);
  display_sequence(neti, net[neti].inpx, net[neti].inpy);
  display_labeled_layer(neti, net[neti].ninpitem,
			inprep[neti], inputs[neti],
			net[neti].inpx, net[neti].inpy,
			ABOVE);
  display_assembly(neti, net[neti].prevx, net[neti].prevy,
		   prevhidrep[neti], nhidrep[neti]);
  display_assembly(neti, net[neti].hidx, net[neti].hidy,
		   hidrep[neti], nhidrep[neti]);
  display_labeled_layer(neti, net[neti].noutitem,
			outrep[neti], targets[neti],
			net[neti].outx, net[neti].outy,
			BELOW2);
  display_log(neti);
}
		  
static void expose_storypars(Widget w,
			    XtPointer client_data, XtPointer call_data)
{
  int neti= STORYPARSMOD;
  XClearWindow(theDisplay, Win[neti]);
  display_title(neti, titles[neti]);
  display_labels(neti, net[neti].inpx, titleboxhght,
		 caselabels, ncase);
  display_labels(neti, net[neti].tchx,
		 net[neti].tchy+net[neti].uhght+qaboxhght,
		 slotlabels, nslot);
  display_labeled_layer(neti, net[neti].noutitem,
			tchrep[neti],  targets[neti],
			net[neti].tchx, net[neti].tchy,
			BELOW);
  display_labeled_layer(neti, net[neti].ninpitem,
			inprep[neti], inputs[neti],
			net[neti].inpx, net[neti].inpy,
			ABOVE);
  display_assembly(neti, net[neti].prevx, net[neti].prevy,
		   prevhidrep[neti], nhidrep[neti]);
  display_assembly(neti, net[neti].hidx, net[neti].hidy,
		   hidrep[neti], nhidrep[neti]);
  display_labeled_layer(neti, net[neti].noutitem,
			outrep[neti], targets[neti],
			net[neti].outx, net[neti].outy,
			BELOW2);
  display_log(neti);
}
		  
static void expose_storygen(Widget w,
			    XtPointer client_data, XtPointer call_data)
{
  int neti=STORYGENMOD;
  XClearWindow(theDisplay, Win[neti]);
  display_title(neti, titles[neti]);
  display_labels(neti, net[neti].inpx,
		 net[neti].inpy+net[neti].uhght+qaboxhght,
		 slotlabels, nslot);
  display_labels(neti, net[neti].outx, titleboxhght,
		 caselabels, ncase);
  display_labeled_layer(neti, net[neti].ninpitem,
			inprep[neti], inputs[neti],
			net[neti].inpx, net[neti].inpy,
			BELOW);
  display_assembly(neti, net[neti].prevx, net[neti].prevy,
		   prevhidrep[neti], nhidrep[neti]);
  display_assembly(neti, net[neti].hidx, net[neti].hidy,
		   hidrep[neti], nhidrep[neti]);
  display_labeled_layer(neti, net[neti].noutitem,
			outrep[neti], targets[neti],
			net[neti].outx, net[neti].outy,
			ABOVE2);
  display_layer(neti, net[neti].noutitem, tchrep[neti],
		net[neti].tchx, net[neti].tchy, nsrep);
  display_log(neti);
}
		  
static void expose_sentgen(Widget w, XtPointer client_data,
			   XtPointer call_data)
{
  int neti=SENTGENMOD;
  XClearWindow(theDisplay, Win[neti]);
  display_title(neti, titles[neti]);
  display_labels(neti, net[neti].inpx,
		 net[neti].inpy+net[neti].uhght+qaboxhght,
		 caselabels, ncase);
  display_labeled_layer(neti, net[neti].ninpitem,
			inprep[neti], inputs[neti],
			net[neti].inpx, net[neti].inpy,
			BELOW);
  display_assembly(neti, net[neti].prevx, net[neti].prevy,
		   prevhidrep[neti], nhidrep[neti]);
  display_assembly(neti, net[neti].hidx, net[neti].hidy,
		   hidrep[neti], nhidrep[neti]);
  display_sequence(neti, net[neti].tchx, net[neti].tchy);
  display_labeled_layer(neti, net[neti].noutitem,
			outrep[neti], targets[neti],
			net[neti].outx, net[neti].outy,
			ABOVE2);
  display_layer(neti, net[neti].noutitem, tchrep[neti],
		net[neti].tchx, net[neti].tchy, nsrep);
  display_log(neti);
}

		  
static void expose_cueformer(Widget w, XtPointer client_data,
			     XtPointer call_data)
{
  int neti=CUEFORMMOD;
  XClearWindow(theDisplay, Win[neti]);
  display_title(neti, titles[neti]);
  display_labels(neti, net[neti].inpx, titleboxhght,
		 caselabels, ncase);
  display_labels(neti, net[neti].tchx,
		 net[neti].tchy+net[neti].uhght+qaboxhght,
		 slotlabels, nslot);
  display_labeled_layer(neti, net[neti].ninpitem,
			inprep[neti], inputs[neti],
			net[neti].inpx, net[neti].inpy,
			ABOVE);
  display_assembly(neti, net[neti].hidx, net[neti].hidy,
		   hidrep[neti], nhidrep[neti]);
  display_labeled_layer(neti, net[neti].noutitem,
			outrep[neti], targets[neti],
			net[neti].outx, net[neti].outy,
			BELOW2);
  display_layer(neti, net[neti].noutitem, tchrep[neti],
		net[neti].tchx, net[neti].tchy, nsrep);
  display_log(neti);
}

		  
static void expose_answerprod(Widget w, XtPointer client_data,
			      XtPointer call_data)
{
  int neti=ANSWERPRODMOD;
  XClearWindow(theDisplay, Win[neti]);
  display_title(neti, titles[neti]);
  display_labels(neti, net[neti].inpx,
		 net[neti].inpy+net[neti].uhght+qaboxhght,
		 caselabels, ncase);
  display_labels(neti, net[neti].inp1x,
		 net[neti].inp1y+net[neti].uhght+qaboxhght,
		 slotlabels, nslot);
  display_labels(neti, net[neti].outx, titleboxhght,
		 caselabels, ncase);
  display_labeled_layer(neti, ncase,
			inprep[neti], inputs[neti],
			net[neti].inpx, net[neti].inpy,
			BELOW);
  display_labeled_layer(neti, nslot,
			&inprep[neti][ncase*nsrep],
			&inputs[neti][ncase],
			net[neti].inp1x, net[neti].inp1y,
			BELOW);
  display_assembly(neti, net[neti].hidx, net[neti].hidy,
		   hidrep[neti], nhidrep[neti]);
  display_labeled_layer(neti, net[neti].noutitem,
			outrep[neti], targets[neti],
			net[neti].outx, net[neti].outy,
			ABOVE2);
  display_layer(neti, net[neti].noutitem, tchrep[neti],
		net[neti].tchx, net[neti].tchy, nsrep);
  display_log(neti);
}

		  
void common_qa_resize(int neti)
{
  net[neti].uwidth =
    imin((net[neti].width-2*HORSP)/(net[neti].columns*nsrep),
	 (net[neti].width-2*HORSP-PREVSP)/nhidrep[neti]);
  net[neti].hsp = nsrep*net[neti].uwidth;
  net[neti].marg = HORSP;

  if (neti==ANSWERPRODMOD)
    {
      net[neti].inpx = (net[neti].width-net[neti].hsp*ncase)/2;
      net[neti].inp1x = (net[neti].width-net[neti].hsp*nslot)/2;
    }
  else net[neti].inpx =
    (net[neti].width-net[neti].hsp*net[neti].ninpitem)/2;
  net[neti].hidx  =
    (net[neti].width-nhidrep[neti]*net[neti].uwidth)/2;
  net[neti].prevx =  net[neti].hidx+PREVSP;
  net[neti].outx  = (net[neti].width-
			   net[neti].hsp*net[neti].noutitem)/2;
  net[neti].tchx  = net[neti].outx;
}

static void resize_sentpars(Widget w, XtPointer client_data,
			    XtPointer call_data)
{
  int neti=SENTPARSMOD;
  common_resize(neti, sentpars);
  common_qa_resize(neti);
  net[neti].uhght =
    imax(0, (net[neti].height-titleboxhght-3*qaboxhght-HIDSP-3*VERSP)/5);
  net[neti].inpy  = titleboxhght+qaboxhght;
  net[neti].prevy = net[neti].inpy+net[neti].uhght+VERSP;
  net[neti].hidy  = net[neti].prevy+net[neti].uhght+HIDSP;
  net[neti].outy  = net[neti].hidy+net[neti].uhght+VERSP;
  net[neti].tchy  = net[neti].outy+net[neti].uhght;
}

static void resize_storypars(Widget w, XtPointer client_data,
			     XtPointer call_data)
{
  int neti= STORYPARSMOD;
  common_resize(neti, storypars);
  common_qa_resize(neti);
  net[neti].uhght =
    imax(0, (net[neti].height-titleboxhght-4*qaboxhght-HIDSP-3*VERSP)/5);
  net[neti].inpy  = titleboxhght+2*qaboxhght;
  net[neti].prevy = net[neti].inpy+net[neti].uhght+VERSP;
  net[neti].hidy  = net[neti].prevy+net[neti].uhght+HIDSP;
  net[neti].outy  = net[neti].hidy+net[neti].uhght+VERSP;
  net[neti].tchy  = net[neti].outy+net[neti].uhght;
}

static void resize_storygen(Widget w, XtPointer client_data,
			     XtPointer call_data)
{
  int neti=STORYGENMOD;
  common_resize(neti, storygen);
  common_qa_resize(neti);
  net[neti].uhght =
    imax(0, (net[neti].height-titleboxhght-4*qaboxhght-HIDSP-3*VERSP)/5);
  net[neti].tchy  = titleboxhght+2*qaboxhght;
  net[neti].outy  = net[neti].tchy+net[neti].uhght;
  net[neti].hidy  = net[neti].outy+net[neti].uhght+VERSP;
  net[neti].prevy = net[neti].hidy+net[neti].uhght+HIDSP;
  net[neti].inpy  = net[neti].prevy+net[neti].uhght+VERSP;
}

static void resize_sentgen(Widget w, XtPointer client_data,
			     XtPointer call_data)
{
  int neti=SENTGENMOD;
  common_resize(neti, sentgen);
  common_qa_resize(neti);
  net[neti].uhght =
    imax(0, (net[neti].height-titleboxhght-3*qaboxhght-HIDSP-3*VERSP)/5);
  net[neti].tchy  = titleboxhght+qaboxhght;
  net[neti].outy  = net[neti].tchy+net[neti].uhght;
  net[neti].hidy  = net[neti].outy+net[neti].uhght+VERSP;
  net[neti].prevy = net[neti].hidy+net[neti].uhght+HIDSP;
  net[neti].inpy  = net[neti].prevy+net[neti].uhght+VERSP;
}

static void resize_cueformer(Widget w, XtPointer client_data,
			     XtPointer call_data)
{
  int neti=CUEFORMMOD;
  common_resize(neti, cueformer);
  common_qa_resize(neti);
  net[neti].uhght =
    imax(0, (net[neti].height-titleboxhght-4*qaboxhght-3*VERSP)/4);
  net[neti].inpy  = titleboxhght+2*qaboxhght;
  net[neti].hidy  = net[neti].inpy+net[neti].uhght+VERSP;
  net[neti].outy  = net[neti].hidy+net[neti].uhght+VERSP;
  net[neti].tchy  = net[neti].outy+net[neti].uhght;
}


static void resize_answerprod(Widget w, XtPointer client_data,
			     XtPointer call_data)
{
  int neti=ANSWERPRODMOD;
  common_resize(neti, answerprod);
  common_qa_resize(neti);
  net[neti].uhght = 
    imax(0, (net[neti].height-titleboxhght-6*qaboxhght-HIDSP-3*VERSP)/5);
  net[neti].tchy  = titleboxhght+2*qaboxhght;
  net[neti].outy  = net[neti].tchy+net[neti].uhght;
  net[neti].hidy  = net[neti].outy+net[neti].uhght+VERSP;
  net[neti].inp1y = net[neti].hidy+net[neti].uhght+VERSP;
  net[neti].inpy  =
    net[neti].inp1y+net[neti].uhght+2*qaboxhght+HIDSP;
}


int imin(int a, int b)
{
  return((a > b) ? b : a);
}

int imax(int a, int b)
{
  return((a > b) ? a : b);
}


void display_labels(int neti, int x, int y, char *labels[], int nitem)
{
  int i;
  for (i=0; i<nitem; i++)
    display_boxword(neti, x+i*net[neti].hsp, y,
		    net[neti].hsp, qaboxhght,
		    labels[i], 0, qafontStruct, qaGC);
}  

void display_sequence(int neti, int x, int y)
{
  /* write out the sequence */
  clearRectangle(neti, 0, titleboxhght, x, qaboxhght);
  drawText(neti,
	   x-net[neti].marg-XTextWidth(qafontStruct, net[neti].sequence,
			      strlen(net[neti].sequence)),
	   y-qaboxhght+qafontStruct->ascent+BOXSP,
	   net[neti].sequence, qaGC);
  XFlush(theDisplay);
}	


void display_error(int neti, float outrep[], int nas, int target[], struct WORDSTRUCT words[], int nrep, int step)
{
  register int i,j;
  float sum=0.0;
  for(i=0; i<nas; i++)
    for(j=0; j<nrep; j++)
      sum += fabs(words[target[i]].rep[j] - outrep[i*nrep+j]);
  sprintf(net[neti].log,"Step %d: Eavg %.3f",step,sum/(nas*nrep));
  display_log(neti);
  XFlush(theDisplay);
}

void display_labeled_layer(int neti, int nas, float rep[], int nums[], int x, int y, int labeloffset)
{
  register int i;
  char s[2*MAXWORDL+4];
  int nearest;

  display_layer(neti, nas, rep, x, y, nsrep);
  for(i=0; i<nas; i++)
    {
      nearest = find_nearest_s(&rep[i*nsrep], swords, nsrep, nswords);
      if(nearest==nums[i] || text_question)
	{
	  sprintf(s,"%s", swords[nearest].chars);
	  display_boxword(neti, x+i*net[neti].hsp, y+labeloffset,
			  net[neti].hsp,  qaboxhght,
			  swords[nearest].chars, nearest,
			  qafontStruct, qaGC);
	}
      else
	{
	  sprintf(s,"*%s(%s)*",swords[nearest].chars,
		  swords[nums[i]].chars);
	  display_boxword(neti, x+i*net[neti].hsp, y+labeloffset,
			  net[neti].hsp, qaboxhght,
			  s, nums[i],
			  qaerrorfontStruct, qaerrorGC);
	}
    }      
  if ((neti==SENTPARSMOD || neti==SENTGENMOD) && nas==1)
    sprintf(net[neti].newitem,"%s", s);
  XFlush(theDisplay);
}


void display_layer(int neti, int nas, float layer[], int layerx, int layery, int nrep)
{
  register int i;
  for(i=0; i<nas; i++)
    display_assembly(neti, layerx+i*net[neti].hsp, layery, &layer[i*nrep], nrep);
}	


void display_assembly(int neti, int startx, int starty, float assembly[], int nrep)
{
  register int i;
  for(i=0; i<nrep; i++)
    fillRectangle(neti,startx+i*net[neti].uwidth, starty,
		  net[neti].uwidth, net[neti].uhght,
		  trans_to_color(assembly[i], UNITCOLORS));
  drawRectangle(neti,startx, starty,
		nrep*net[neti].uwidth, net[neti].uhght);
  XFlush(theDisplay);
}  


void display_boxword(int neti, int x, int y, int width, int height, char wordchars[], int index, XFontStruct *fontStruct, GC currGC)
{
  clearRectangle(neti, x, y, width, height);
  if (index>=0)
    drawoverText(neti,
		 x+(width-XTextWidth(fontStruct, wordchars,
				     strlen(wordchars)))/2,
		 y+height-BOXSP-qafontStruct->descent,
		 wordchars, currGC);
  drawRectangle(neti, x, y, width, height);
  XFlush(theDisplay);
}

/********************* hfm operations ***********************/

void init_hfm_display_params(int neti, Widget w)
{
  int displaying_save=displaying, babbling_save=babbling,
  print_mistakes_save=print_mistakes;
  int i,j, k, wordindex, fewest;
  char hfmlabel[MAXWORDL], sublabel[MAXWORDL], blabel[MAXWORDL];
  FILE *fp;

  Win[neti] = XtWindow(w);

  if((fp=fopen(hfminpfile,"r"))==NULL)
     {
       fprintf(stderr, "cannot open %s, exiting\n", hfminpfile);
       exit(1);
     }

  printf("Reading hfmlabels from %s...", hfminpfile);
  displaying=0; babbling=0; print_mistakes=0;
  fgl(fp);
  for(i=0; fscanf(fp,"%s %s %s", hfmlabel, sublabel, blabel)!=EOF; i++)
    {
      for(j=0; j<nslot; j++)
	{
	  fscanf(fp,"%d", &wordindex);
	  for(k=0; k<nsrep; k++)
	    inpvector[j*nsrep+k] = swords[wordindex].rep[k];
	}
      classify();

      collect_uniq_labels(hfmunits[besti0][bestj0].labels,
			  &hfmunits[besti0][bestj0].labelcount,
			  hfmlabel, MAXHFMLABELS);
      collect_uniq_labels(subunits[besti0][bestj0][besti1][bestj1].labels,
			  &subunits[besti0][bestj0][besti1][bestj1].labelcount,
			  sublabel, MAXHFMLABELS);
      collect_labels(bunits[besti0][bestj0][besti1][bestj1][besti2][bestj2]
		     .labels,
		     &bunits[besti0][bestj0][besti1][bestj1][besti2][bestj2]
		     .labelcount,
		     blabel, MAXTRACELABELS);
    }      
  fclose(fp);
  printf("Done.\n");

  displaying=displaying_save;
  babbling=babbling_save;
  print_mistakes=print_mistakes_save;
  hfm_null_values();
  hfm_null_prevvalues();

  fewest=LARGEINT;
  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      if (hfmunits[i][j].labelcount<fewest)
	{
	  hfmi=i;
	  hfmj=j;
	  fewest=hfmunits[i][j].labelcount<fewest;
	}	  
}

static void expose_hfm(Widget w, XtPointer client_data, XtPointer call_data)
{
  int neti=HFMWINMOD;
  XClearWindow(theDisplay, Win[neti]);
  display_title(neti, titles[neti]);
  hfm_null_prevvalues();
  display_hfm_values();
  display_log(neti);
}

static void resize_hfm(Widget w, XtPointer client_data,
		       XtPointer call_data)
{
  int neti=HFMWINMOD;
  common_resize(neti, hfm);
  net[neti].uhght  =
    (net[neti].height-titleboxhght-VERSP-(hfmnets*subnets-1)*HFMSP)
      /(hfmnets*subnets*hfmnets);
  net[neti].u1hght = net[neti].uhght*hfmnets/subnets;
  net[neti].u2hght = net[neti].u1hght*subnets/bnets;
  net[neti].uwidth =
    (net[neti].width-2*HORSP-(hfmnets*subnets-1)*HFMSP)
      /(hfmnets*subnets*hfmnets);
  net[neti].u1width= net[neti].uwidth*hfmnets/subnets;
  net[neti].u2width= net[neti].u1width*subnets/bnets; 
  net[neti].marg= (net[neti].width-hfmnets*subnets*hfmnets*net[neti].uwidth-
		   (hfmnets*subnets-1)*HFMSP)/2;
}
		  
void display_hfm_values()
{
  register int i,j,ii,jj;
  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      for(ii=0; ii<subnets; ii++)
	for(jj=0; jj<subnets; jj++)
	  display_hfm_b(i,j,ii,jj);
  for(i=0; i<hfmnets; i++)
    for(j=0; j<hfmnets; j++)
      display_hfm_sub(i,j);
  display_hfm_top();
}

void display_hfm_top()
{
  int neti=HFMWINMOD;
  int hfm2j=(hfmnets-1)-hfmj; /* inverts the vertical axis */
  display_hfm_box(neti, hfmnets, 
		  net[neti].marg+(hfmi*subnets+hfmi)*hfmnets*net[neti].uwidth +
		  (hfmi*subnets+hfmi)*HFMSP,
		  titleboxhght+ (hfm2j*subnets+hfm2j)*hfmnets*net[neti].uhght +
		  (hfm2j*subnets+hfm2j)*HFMSP,
		  net[neti].uwidth, net[neti].uhght, hfmfontStruct, hfmunits);
}

void display_hfm_sub(int i, int j)
{
  int neti=HFMWINMOD;
  int hfm2j=(hfmnets-1)-hfmj, j2=(hfmnets-1)-j; /* inverts the vertical axis */
  display_hfm_box(neti, subnets,
		  net[neti].marg+(hfmi*subnets+i)*hfmnets*net[neti].uwidth +
		  (hfmi*subnets+i)*HFMSP,
		  titleboxhght+(hfm2j*subnets+j2)*hfmnets*net[neti].uhght+
		  (hfm2j*subnets+j2)*HFMSP,
		  net[neti].u1width, net[neti].u1hght, hfmfontStruct,
		  subunits[i][j]);
}
  
void display_hfm_b(int i, int j, int ii, int jj)
{
  int neti=HFMWINMOD;
  int j2=(hfmnets-1)-j, jj2=(subnets-1)-jj; /* inverts the vertical axis */

  display_trace_box(neti, bnets,
		    net[neti].marg+(i*subnets+ii)*hfmnets*net[neti].uwidth
		    +(i*subnets+ii)*HFMSP,
		    titleboxhght+(j2*subnets+jj2)*hfmnets*net[neti].uhght
		    +(j2*subnets+jj2)*HFMSP,
		    net[neti].u2width, net[neti].u2hght, tracefontStruct,
		    bunits[i][j][ii][jj]);
}
  

void display_hfm_box(int neti, int nets, int x, int y, int width, int height,
		     XFontStruct *fontstruct,
		     HFMUNITDATA units[MAXHFMNETS][MAXHFMNETS])
{
  int i, j;
  int j2; /* inverts the vertical axis */

  for(i=0; i<nets; i++)
    for(j=0; j<nets; j++)
      if (units[i][j].value != units[i][j].prevvalue)
	{
	  j2=(nets-1)-j;
	  frameRectangle(neti, x+i*width, y+j2*height, width, height,
			 trans_to_color(units[i][j].value, UNITCOLORS));
	  labelbox(neti, x+i*width, y+j2*height, width, height,
		   units[i][j].value, units[i][j].labels,
		   units[i][j].labelcount, fontstruct,
		   hfmfGC, hfmbGC);
	}
  XFlush(theDisplay);
}




void display_trace_box(int neti, int nets, int x, int y, int width, int height,
		       XFontStruct *fontstruct,
		       TRACEUNITDATA units[MAXTRACENETS][MAXTRACENETS])
{
  int i, j, lll, mmm;
  int i2, j2, lll2, mmm2; /* flips the coordinate axes */
  float x0,x1,y0,y1,length;
  GC currGC;

  for(i=0; i<nets; i++)
    for(j=0; j<nets; j++)
      if (units[i][j].value != units[i][j].prevvalue)
	{
	  i2=j; j2=i;
	  frameRectangle(neti, x+i2*width, y+j2*height, width, height,
			 trans_to_color(units[i][j].value, UNITCOLORS));
	}
	  
  for(i=0; i<nets; i++)
    for(j=0; j<nets; j++)
      if (units[i][j].value != units[i][j].prevvalue)
	{
	  i2=j; j2=i;
	  labelbox(neti, x+i2*width, y+j2*height, width, height,
		   units[i][j].value, units[i][j].labels,
		   units[i][j].labelcount, fontstruct,
		   tracefGC, tracebGC);
	}

  for(i=0; i<nets; i++)
    for(j=0; j<nets; j++)
      if (units[i][j].value != units[i][j].prevvalue)
	{
	  if(units[i][j].value>data.reversevalue) currGC=linebGC;
	  else currGC=linefGC;
	  i2=j; j2=i;
	  for(lll=0; lll<nets; lll++)
	    for(mmm=0; mmm<nets; mmm++)
	      if(units[i][j].latweights[lll][mmm] > 0.0)
		{
		  lll2=mmm; mmm2=lll;
		  x0 = x+(i2+0.5)*net[neti].u2width;
		  x1 = x+(lll2+0.5)*net[neti].u2width;
		  y0 = y+(j2+0.5)*net[neti].u2hght;
		  y1 = y+(mmm2+0.5)*net[neti].u2hght;

		  length = sqrt((x1 -x0)*(x1 -x0) + (y1 -y0)*(y1 -y0));
		  if (length>0.0)
		    drawLine(neti, x0, y0,
			     x0+units[i][j].latweights[lll][mmm]/gammaexc
			     *data.tracelinescale*0.5*net[neti].u2width*
			     (x1-x0)/length,
			     y0+units[i][j].latweights[lll][mmm]/gammaexc
			     *data.tracelinescale*0.5*net[neti].u2hght*
			     (y1-y0)/length,
			     currGC,
			     (int) (units[i][j].latweights[lll][mmm]
				    *data.tracewidthscale*net[neti].width));
		}
	}
  XFlush(theDisplay);
}

void display_hfm_error(int neti, float outvector[], int slots[])
{
  char bunitlabel[MAXSTRL];
  register int i,j;
  float sum=0.0;

  if(neti==RETMOD && bestvalue<aliveact)
    sprintf(net[HFMWINMOD].log, "No image found");
  else 
    {
      if(neti==STOREMOD) sprintf(net[HFMWINMOD].log, "S");
      else sprintf(net[HFMWINMOD].log, "R");
      if(bunits[besti0][bestj0][besti1][bestj1][besti2][bestj2].labelcount==0)
	sprintf(bunitlabel, "(%d,%d)", besti2, bestj2);
      else
	sprintf(bunitlabel, "%s",
		bunits[besti0][bestj0][besti1][bestj1][besti2][bestj2]
		.labels[0]);
      sprintf(net[HFMWINMOD].log, "%s %s-%s-%s", net[HFMWINMOD].log,
	      hfmunits[besti0][bestj0].labels[0],
	      subunits[besti0][bestj0][besti1][bestj1].labels[0],
	      bunitlabel);
      
      for(i=0; i<nslot; i++)
	for(j=0; j<nsrep; j++)
	  sum += fabs(swords[slots[i]].rep[j] - outvector[i*nsrep+j]);
      
      sprintf(net[HFMWINMOD].log,"%s: Eavg %.3f",
	      net[HFMWINMOD].log, sum/(nslot*nsrep));
    }
  display_log(HFMWINMOD);
  XFlush(theDisplay);
}


/********************* lexicon operations ***********************/

void init_lex_display_params(int neti, Widget w, int nets, int nwords, struct WORDSTRUCT words[], int nrep, struct LEXUNIT units[MAXLSNETS][MAXLSNETS])
{
  int i;

  Win[neti] = XtWindow(w);
  
  for(i=0; i<nwords; i++)
    if(strcmp(words[i].chars, fillerword) &&
       (words[i].chars[0]!=internal_symbol))
      find_max_resp(words[i].rep, words[i].chars, nrep, nets, units);

  lex_clear_values(units, nets);
  lex_clear_prevvalues(units, nets);
}

static void expose_lex(Widget w, XtPointer units, XtPointer call_data)
{
  int
    neti= ((w==lex) ? LEXWINMOD : SEMWINMOD),
    nets= ((w==lex) ? lnets : snets);

  XClearWindow(theDisplay, Win[neti]);
  display_title(neti, titles[neti]);
  lex_clear_prevvalues(units, nets);
  display_lex(neti, nets, units);
  display_log(neti);
}

		  
static void resize_lex(Widget w, XtPointer client_data, XtPointer call_data)
{
  int
    neti= ((w==lex) ? LEXWINMOD : SEMWINMOD),
    nets= ((w==lex) ? lnets : snets);

  common_resize(neti, w);
  net[neti].uhght= (net[neti].height-titleboxhght-VERSP)/nets;
  net[neti].uwidth= (net[neti].width-2*HORSP)/nets;
  net[neti].marg= (net[neti].width-nets*net[neti].uwidth)/2;
}

void display_lex(int neti, int nets, struct LEXUNIT units[MAXLSNETS][MAXLSNETS])
{

  int i,j, jj;
  XFontStruct *fontstruct;
  GC currfGC, currbGC;
  if(neti==SINPMOD || neti == SOUTMOD) neti=SEMWINMOD;
  if(neti==LINPMOD || neti == LOUTMOD) neti=LEXWINMOD;
 
  if(neti==LEXWINMOD)
    {
      fontstruct=lexfontStruct;
      currfGC=lexfGC;
      currbGC=lexbGC;
    }
  else
    {
      fontstruct=semfontStruct;
      currfGC=semfGC;
      currbGC=sembGC;
    }

  /* i and j are switched to match the figure in the book */
  for(i=0; i<nets; i++)
    for(j=0; j<nets; j++)
      if (units[i][j].value != units[i][j].prevvalue)
	{
	  frameRectangle(neti, net[neti].marg+j*net[neti].uwidth,
			 titleboxhght+i*net[neti].uhght,
			 net[neti].uwidth, net[neti].uhght,
			 trans_to_color(units[i][j].value, UNITCOLORS));
	  for (jj=j-1; jj<=j+1; jj++)
	    if(jj>=0 && jj<nets)
	      labelbox(neti, net[neti].marg+jj*net[neti].uwidth,
		       titleboxhght+i*net[neti].uhght,
		       net[neti].uwidth, net[neti].uhght,
		       units[i][jj].value, units[i][jj].labels,
		       units[i][jj].labelcount, fontstruct,
		       currfGC, currbGC);
	}
  XFlush(theDisplay);
}

void display_lex_error(int neti, int index, struct LEXUNIT units[MAXLSNETS][MAXLSNETS], int besti, int bestj, int nwords, struct WORDSTRUCT words[], int nrep)
{
  int i,nearest,real_neti;
  float sum=0.0;

  real_neti=neti;
  if(neti==LINPMOD || neti == LOUTMOD) neti=LEXWINMOD;
  if(neti==SINPMOD || neti == SOUTMOD) neti=SEMWINMOD;
  if(real_neti==LINPMOD || real_neti==SINPMOD) sprintf(net[neti].log, "Input");
  else sprintf(net[neti].log, "Assoc");

  for(i=0; i<nrep; i++)
    sum += fabs(words[index].rep[i] - units[besti][bestj].comp[i]);

  nearest = find_nearest_s(units[besti][bestj].comp, words, nrep, nwords);

  if(nearest==index || text_question)
    sprintf(net[neti].log,"%s %s: Eavg %.3f",
	    net[neti].log, words[nearest].chars, sum/nrep);
  else
    sprintf(net[neti].log,"%s *%s(%s)*: Eavg %.3f",
	    net[neti].log,
	    words[nearest].chars, words[index].chars,
	    sum/nrep);

  display_log(neti);
  XFlush(theDisplay);
}

void find_max_resp(float inpvector[], char inplabel[], int dim, int nets, struct LEXUNIT units[MAXLSNETS][MAXLSNETS])
/* find the maximally responding unit */
{
  int i,j, besti, bestj;
  float best=LARGEFLOAT, second=(-1.0);

  /* present it and find the image unit */
  for(i=0; i<nets; i++)
    for(j=0; j<nets; j++)
      {
	units[i][j].value = distance(inpvector,units[i][j].comp,dim);
	/* check if this unit's response is best so far encountered */
	if (units[i][j].value == best) second=best;
	if (units[i][j].value < best)
	  {
	    besti=i; bestj=j; best=units[i][j].value;
	  }
      }
  if (second == best)
    fprintf(stderr, "Warning at init: image for \"%s\" is not unique\n",
	    inplabel);

  collect_labels(units[besti][bestj].labels,
		 &units[besti][bestj].labelcount,
		 inplabel, MAXLEXLABELS);
}


/********************* general routines ***********************/

void display_title(int neti, char name[])
{
  drawText(neti,
	   (net[neti].width-XTextWidth(titlefontStruct, name, strlen(name)))/2,
	   BOXSP+titlefontStruct->ascent,
	   name, titleGC);
}

void display_log(int neti)
{
  
  clearRectangle(neti,0,0,
		(net[neti].width-XTextWidth(titlefontStruct,
					    titles[neti],
					    strlen(titles[neti])))/2,
		titleboxhght);
  drawText(neti, net[neti].marg, BOXSP+titlefontStruct->ascent,
	   net[neti].log, logGC);
}
   

void frameRectangle(int neti, int x, int y, int width, int height, int colorindex)
{
  fillRectangle(neti, x, y, width, height, colorindex);
  drawRectangle(neti, x, y, width, height);
}


void labelbox(int neti, int x, int y, int width, int height, float value, char labels[][MAXWORDL], int labelcount, XFontStruct *fontstruct, GC fGC, GC bGC)
{
  int k;
  GC currGC;

  if(value>data.reversevalue) currGC=bGC;
  else currGC=fGC;
  for(k=0; k<labelcount; k++)
    drawoverText(neti,
		 x + 0.5*(width-XTextWidth(fontstruct,
					   labels[k],
					   strlen(labels[k]))),
		 y + 0.5*height + (k+1-0.5*labelcount)*fontstruct->ascent,
		 labels[k],
		 currGC);
}

void collect_uniq_labels(char labels[][MAXWORDL], int *count, char label[], int maxlabels)
{
  if (newlabel(labels, *count, label))
    collect_labels(labels, count, label, maxlabels);
}

void collect_labels(char labels[][MAXWORDL], int *count, char label[], int maxlabels)
{
  if (*count==maxlabels)
    sprintf(labels[maxlabels-1], MORELABEL);
  else
    sprintf(labels[(*count)++], label);
}

int newlabel(char labels[][MAXWORDL], int count, char label[])
{
  register int i;
  for(i=0; i<count; i++)
    if(!strcmp(labels[i], label))
      return(0);
  return(1);
}

int trans_to_color(float value, int map)
{
  if (map==UNITCOLORS)
    /* map the number [0,1] to corresponding color */
    return((int) ((Actual_Color_Range*value)+0.499999));
  else
    {
      /* map [-1,1] to corresponding color, clip <-1 to -1, >1 to 1 */
      if (value<-1.0) return(0);
      else if (value > 1.0) return(Actual_Color_Range);
      else return((int) ((Actual_Color_Range*(0.5-value/2.0)))+0.499999);
    }
}

/********************* low-level operations ***********************/

void drawLine(int neti, int x1, int y1, int x2, int y2, GC currGC, int width)
{
  XSetLineAttributes(theDisplay, currGC, width, LineSolid, CapRound,JoinRound);
  XDrawLine(theDisplay, Win[neti], currGC, x1, y1, x2, y2);
}

void clearRectangle(int neti, int x, int y, int width, int height)
{
 XFillRectangle(theDisplay, Win[neti], clearGC, x, y, width, height);
} 

void fillRectangle(int neti, int x, int y, int width, int height, int colorindex)
{
 XSetForeground(theDisplay, activityGC, colors[colorindex].pixel);
 XFillRectangle(theDisplay, Win[neti], activityGC, x, y, width, height);
} 


void drawRectangle(int neti, int x, int y, int width, int height)
{
 XDrawRectangle(theDisplay, Win[neti], boxGC, x, y, width, height);
} 


void drawText(int neti, int x, int y, char text[], GC currGC)
{
  XDrawImageString(theDisplay, Win[neti], currGC, x, y, text, strlen(text));
}


void drawoverText(int neti, int x, int y, char text[], GC currGC)
{
  XDrawString(theDisplay, Win[neti], currGC, x, y, text, strlen(text));
}

