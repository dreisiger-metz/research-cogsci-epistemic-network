/* File: Gwin.h
 *
 * Graphics window widget; modified from template
 * Risto Miikkulainen 5/7/93
 */

#ifndef _Gwin_h
#define _Gwin_h

/****************************************************************
 *
 * Gwin widget
 *
 ****************************************************************/

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 callback	     Callback		Callback	NULL
 exposeCallback	     Callback		Callback	NULL
 resizeCallback	     Callback		Callback	NULL
 font		     Font		XFontStruct*	XtDefaultFont
 drawingColor1	     Color		Pixel		XtDefaultForeground
 drawingColor2	     Color		Pixel		XtDefaultForeground
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	0
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0

*/

/* define any special resource names here that are not in <X11/StringDefs.h> */

#define XtNGwinResource "gwinResource"

#define XtCGwinResource "GwinResource"

#define XtNdrawingColor1	"drawingColor1"
#define XtNdrawingColor2	"drawingColor2"
#define XtNexposeCallback	"exposeCallback"
#define XtNresizeCallback	"resizeCallback"

extern Pixel GwinColor1(/* Widget */);
extern Pixel GwinColor2(/* Widget */);
extern Font  GwinFont(/* Widget */);

/* declare specific GwinWidget class and instance datatypes */

typedef struct _GwinClassRec*		GwinWidgetClass;
typedef struct _GwinRec*		GwinWidget;

/* declare the class constant */

extern WidgetClass gwinWidgetClass;

#endif /* _Gwin_h */


