/* xlib.c -- libx11 helpers for Nightshade Xlib. */

#include <X11/Xlib.h>

#if 1
#include <signal.h>
#ifdef mach
#ifdef mips
#include <mips/cpu.h>
#endif
#endif

/* For printf. */
#include <stdio.h>

#include "nightshade.h"
#include "internals.h"
#include "globals.h"
#include "vars.h"
#include "parse.h"
#include "monitor.h"
#include "arch.h"
#include "search.h"
#include "interrupt.h"
#include "globals.h"
#include "lispregs.h"
#include "validate.h"
#include "monitor.h"
#include "alloc.h"
#include "dynbind.h"
#include "interr.h"
#endif

/* Define variables for values in order to access the numbers. */

const int true = True;
const int false = False;
const int none = None;

const int badAlloc = BadAlloc;
const int badName = BadName;
const int badWindow = BadWindow;
const int badMatch = BadMatch;
const int badValue = BadValue;

const long gCBackground = GCBackground;
const long gCFillStyle = GCFillStyle;
const long gCFont = GCFont;
const long gCForeground = GCForeground;
const long gCFunction = GCFunction;
const long gCGraphicsExposures = GCGraphicsExposures;
const long gCLineStyle = GCLineStyle;
const long gCLineWidth = GCLineWidth;
const long gCPlaneMask = GCPlaneMask;
const long gCStipple = GCStipple;

const int fillOpaqueStippled = FillOpaqueStippled;
const int fillSolid = FillSolid;
const int fillStippled = FillStippled;
const int fillTiled = FillTiled;

const int lineSolid = LineSolid;
const int lineOnOffDash = LineOnOffDash;
const int lineDoubleDash = LineDoubleDash;

int keyPress = KeyPress;
int keyRelease = KeyRelease;
int buttonPress = ButtonPress;
int buttonRelease = ButtonRelease;
int motionNotify = MotionNotify;
int enterNotify = EnterNotify;
int leaveNotify = LeaveNotify;
int exposure = Expose;
int graphicsExposure = GraphicsExpose;
int noExposure = NoExpose;
int focusIn = FocusIn;
int focusOut = FocusOut;
int keymapNotify = KeymapNotify;
int visibilityNotify = VisibilityNotify;
int createNotify = CreateNotify;
int destroyNotify = DestroyNotify;
int unmapNotify = UnmapNotify;
int mapNotify = MapNotify;
int mapRequest = MapRequest;
int reparentNotify = ReparentNotify;
int configureNotify = ConfigureNotify;
int circulateNotify = CirculateNotify;
int circulateRequest = CirculateRequest;
int gravityNotify = GravityNotify;
int resizeRequest = ResizeRequest;
int configureRequest = ConfigureRequest;
int propertyRequest = PropertyNotify;
int selectionClear = SelectionClear;
int selectionRequest = SelectionRequest;
int selectionNotify = SelectionNotify;
int colormapNotify = ColormapNotify;
int mappingNotify = MappingNotify;
int clientMessage = ClientMessage;

unsigned int inputOutput = InputOutput;
unsigned int inputOnly = InputOnly;
unsigned int copyFromParent = CopyFromParent;

long cWBackPixel = CWBackPixel;
long cWBackPixmap = CWBackPixmap;
long cWBorderPixmap = CWBorderPixmap;
long cWBorderPixel = CWBorderPixel;
long cWOverrideRedirect = CWOverrideRedirect;
long cWCursor = CWCursor;

int isUnmapped = IsUnmapped;
int isUnviewable = IsUnviewable;
int isViewable = IsViewable;

int coordModeOrigin = CoordModeOrigin;
int coordModePrevious = CoordModePrevious;

int complex = Complex;
int convex = Convex;
int nonconvex = Nonconvex;

#if 0
// FIX where are these?
long pAllHints = PAllHints;
long uSPosition = USPosition;
long uSSize = USSize;
#endif

int xYBitmap = XYBitmap;
int xYPixmap = XYPixmap;

// xlib.c:85: error: `BitmapSuccess' undeclared here (not in a function)
//int bitmapSuccess = BitmapSuccess;

int button1Mask = Button1Mask;
int button2Mask = Button2Mask;
int button3Mask = Button3Mask;
int button4Mask = Button4Mask;
int button5Mask = Button5Mask;
int shiftMask = ShiftMask;
int lockMask = LockMask;
int controlMask = ControlMask;
int mod1Mask = Mod1Mask;
int mod2Mask = Mod2Mask;
int mod3Mask = Mod3Mask;
int mod4Mask = Mod4Mask;
int mod5Mask = Mod5Mask;

int queuedAlready = QueuedAlready;
int queuedAfterFlush = QueuedAfterFlush;
int queuedAfterReading = QueuedAfterReading;

/* Event masks. */

/* Guessed after seeing an event name somewhere. */
int keyPressMask = KeyPressMask;
int keyReleaseMask = KeyReleaseMask;
int buttonPressMask = ButtonPressMask;
int buttonReleaseMask = ButtonReleaseMask;
//int motionNotifyMask = MotionNotifyMask;
int enterWindowMask = EnterWindowMask;
int leaveWindowMask = LeaveWindowMask;
int exposureMask = ExposureMask;
//int graphicsExposureMask = GraphicsExposureMask;

/* Found by searching manual pages. */
int resizeRedirectMask = ResizeRedirectMask;
//int shapeNotifyMask = ShapeNotifyMask;
int substructureRedirectMask = SubstructureRedirectMask;
int substructureNotifyMask = SubstructureNotifyMask;
//int structureRedirectMask = StructureRedirectMask;
int structureNotifyMask = StructureNotifyMask;

#if 0
/* FIX translated directly from keywords */

//int noExposureMask = NoExposureMask;
int configureNotifyMask = ConfigureWindowMask;

/* Passed over by editor. */
int destroyNotifyMask = DestroyWindowMask;
int unmapNotifyMask = UnmapWindowMask;
int mapNotifyMask = MapWindowMask;
int reparentNotifyMask = ReparentNotifyMask;
int gravityNotifyMask = GravityNotifyMask; // FIX missing from clx-ext.lisp
int circulateNotifyMask = CirculateNotifyMask;
int clientMessageMask = ClientMessageMask;

/* Needed only by clx-ext.lisp. */
int focusInMask = FocusInMask;
int focusOutMask = FocusOutMask;
int keymapNotifyMask = KeymapMask;
int visibilityNotifyMask = VisibilityNotifyMask;
int createNotifyMask = CreateNotifyMask;
int mapRequestMask = MapWindowMask;
int circulateRequestMask = CirculateRequestMask;
int propertyNotifyMask = PropertyNotifyMask;
int selectionClearMask = SelectionClearMask;
int selectionRequestMask = SelectionRequestMask;
int selectionNotifyMask = SelectionNotifyMask;
int colormapNotifyMask = ColormapNotifyMask;
int mappingNotifyMask = MappingNotifyMask;
#endif

/* Printing event queue. */

Bool
print_event (Display* display, XEvent *event, XPointer arg)
{
  printf ("  %i\n", event->type);
  return False;
}

void
x_print_events (Display* display)
{
  XEvent event;
  XPointer arg;
  printf ("event queue:\n");
  XCheckIfEvent (display, &event, print_event, arg);
}

/* Attempts at error handling. */

#if 0
int call_serve_alien_caller_2 (void *one, void *two) {

    char *ptr = "SERVE-ALIEN-CALLER-2";
    lispobj thing = parse_lispobj(&ptr), function, cons;
    // from monitor.c call_cmd
    if (LowtagOf(thing) == type_OtherPointer) {
	switch (TypeOf(*(lispobj *)(thing-type_OtherPointer))) {
	  case type_SymbolHeader:
	    for (cons = SymbolValue(INITIAL_FDEFN_OBJECTS);
		 cons != NIL;
		 cons = CONS(cons)->cdr) {
		if (FDEFN(CONS(cons)->car)->name == thing) {
		    thing = CONS(cons)->car;
		    goto fdefn;
		}
	    }
	    abort();
#if 0
	    printf("symbol 0x%08lx is undefined.\n", thing);
	    return;
#endif

	  case type_Fdefn:
	  fdefn:
	    function = FDEFN(thing)->function;
	    if (function == NIL) {
	        abort();
#if 0
		printf("fdefn 0x%08lx is undefined.\n", thing);
		return;
#endif
	    }
	    break;
	  default:
	    abort();
#if 0
	    printf(
	      "0x%08lx is not a function pointer, symbol, or fdefn object.\n",
		   thing);
	    return;
#endif
	}
    }
    else if (LowtagOf(thing) != type_FunctionPointer) {
	abort();
#if 0
        printf("0x%08lx is not a function pointer, symbol, or fdefn object.\n",
	       thing);
        return;
#endif
    }
    else
	function = thing;


  // FIX function is going to call `error', is that going to
  // return from this function?
  funcall2(function,
#if 0
           // seems to be what parse_lispobj in monitor does
           ((long) one) | ~3,
	   ((long) two) | ~3);
#else
           make_fixnum((lispobj)one),
	   make_fixnum((lispobj)two));
#endif
  return 0;
}
#endif


#if 0
int call_serve_alien_caller_0 (void *one, void *two) {

    char *ptr = "SERVE-ALIEN-CALLER-0";
    lispobj thing = parse_lispobj(&ptr), function, cons;
    // from monitor.c call_cmd
    if (LowtagOf(thing) == type_OtherPointer) {
	switch (TypeOf(*(lispobj *)(thing-type_OtherPointer))) {
	  case type_SymbolHeader:
	    for (cons = SymbolValue(INITIAL_FDEFN_OBJECTS);
		 cons != NIL;
		 cons = CONS(cons)->cdr) {
		if (FDEFN(CONS(cons)->car)->name == thing) {
		    thing = CONS(cons)->car;
		    goto fdefn;
		}
	    }
	    abort();
#if 0
	    printf("symbol 0x%08lx is undefined.\n", thing);
	    return;
#endif

	  case type_Fdefn:
	  fdefn:
	    function = FDEFN(thing)->function;
	    if (function == NIL) {
	        abort();
#if 0
		printf("fdefn 0x%08lx is undefined.\n", thing);
		return;
#endif
	    }
	    break;
	  default:
	    abort();
#if 0
	    printf(
	      "0x%08lx is not a function pointer, symbol, or fdefn object.\n",
		   thing);
	    return;
#endif
	}
    }
    else if (LowtagOf(thing) != type_FunctionPointer) {
	abort();
#if 0
        printf("0x%08lx is not a function pointer, symbol, or fdefn object.\n",
	       thing);
        return;
#endif
    }
    else
	function = thing;


  funcall0(function);
  return 0;
}
#endif

#if 1
#include <stdio.h>

int handle_xlib_error (Display* display, XErrorEvent* event) {
  char message[512];
  if (XGetErrorText (display, event->error_code,
		     (char*) &message, 512)) {
    printf ("Xlib error\n");
    printf (message);
    printf ("\n");
  }
  return 1;
}
#endif

#if 0
int handle_xlib_error_2 (Display* display, XErrorEvent* event) {

    lispobj context_sap;
#if ( defined( __linux__ ) && defined( i386 ) )
    GET_CONTEXT
#endif

    fake_foreign_function_call(context);

#if 0
    /* Allocate the SAP. */
    context_sap = alloc_sap(context);
#endif

    funcall2(SymbolFunction(INTERNAL_ERROR), context_sap, T);
    undo_fake_foreign_function_call(context);

    arch_skip_instruction(context);

    return 1;
}
#endif
