/* File: gwin.c
 *
 * Graphics window widget; modified from template
 * Risto Miikkulainen 5/7/93
 */

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "GwinP.h"

static XtResource resources[] = {
#define offset(field) XtOffset(GwinWidget, gwin.field)
	/* {name, class, type, size, offset, default_type, default_addr}, */
	{ XtNdrawingColor1, XtCColor, XtRPixel, sizeof(Pixel),
		  offset(color_1), XtRString, XtDefaultForeground },
	{ XtNdrawingColor2, XtCColor, XtRPixel, sizeof(Pixel),
		  offset(color_2), XtRString, XtDefaultForeground },
	{ XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct*),
		  offset(font), XtRString, XtDefaultFont },
	{ XtNexposeCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
		  offset(expose_callback), XtRCallback, NULL },
	{ XtNresizeCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
		  offset(resize_callback), XtRCallback, NULL },
	{ XtNcallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
		  offset(input_callback), XtRCallback, NULL },
#undef offset
};

/* ARGSUSED */
static void InputAction(w, event, params, num_params)
	Widget w;
	XEvent *event;
	String *params;		/* unused */
	Cardinal *num_params;	/* unused */
{
	XtCallCallbacks(w, XtNcallback, (XtPointer)event);
}

static XtActionsRec actions[] =
{
	/* {name,	procedure}, */
	{"input",	InputAction},
};

static char translations[] =
"	<Key>:	input() \n\
 	<BtnDown>:	input() \
";

/* ARGSUSED */
static void Redisplay(w, event, region)
	Widget w;
	XEvent *event;	/* unused */
	Region region;
{
	XtCallCallbacks(w, XtNexposeCallback, (XtPointer)region);
}

/* ARGSUSED */
static void Resize(w)
	Widget w;
{
	XtCallCallbacks(w, XtNresizeCallback, NULL);
}

Pixel GwinColor1(w)
	Widget w;
{
	return ((GwinWidget)w)->gwin.color_1;
}

Pixel GwinColor2(w)
	Widget w;
{
	return ((GwinWidget)w)->gwin.color_2;
}

Font GwinFont(w)
	Widget w;
{
	return ((GwinWidget)w)->gwin.font->fid;
}

GwinClassRec gwinClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &widgetClassRec,
    /* class_name		*/	"Gwin",
    /* widget_size		*/	sizeof(GwinRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	NULL,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	actions,
    /* num_actions		*/	XtNumber(actions),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	NULL,
    /* resize			*/	Resize,
    /* expose			*/	Redisplay,
    /* set_values		*/	NULL,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	translations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* template fields */
    /* empty			*/	0
  }
};

WidgetClass gwinWidgetClass = (WidgetClass)&gwinClassRec;
