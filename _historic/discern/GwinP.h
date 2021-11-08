/* File: GwinP.h
 *
 * Graphics window widget; modified from template
 * Risto Miikkulainen 5/7/93
 */

#ifndef _GwinP_h
#define _GwinP_h

#include "Gwin.h"
/* include superclass private header file */
#include <X11/CoreP.h>

/* define unique representation types not found in <X11/StringDefs.h> */

#define XtRGwinResource "GwinResource"

typedef struct {
    int empty;
} GwinClassPart;

typedef struct _GwinClassRec {
    CoreClassPart	core_class;
    GwinClassPart	gwin_class;
} GwinClassRec;

extern GwinClassRec gwinClassRec;

typedef struct {
	/* resources */
	Pixel color_1;
	Pixel color_2;
	XFontStruct* font;
	XtCallbackList expose_callback;
	XtCallbackList resize_callback;
	XtCallbackList input_callback;
	/* private state */
	/* (none) */
} GwinPart;

typedef struct _GwinRec {
    CorePart	core;
    GwinPart	gwin;
} GwinRec;

#endif /* _GwinP_h */
