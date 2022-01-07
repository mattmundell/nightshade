;;; Initialization code and random debugging stuff.

(in-package "EDI")

(export '(*global-variable-names* *mode-names* *buffer-names*
	  *character-attribute-names* *command-names* *buffer-list*
	  *window-list* *last-key-event-typed* after-editor-initializations))

(in-package "EXTENSIONS")
(export '(save-all-buffers))
(in-package "EDI")


;;;; Higher level documentation nodes.

#[ Editor

This document describes the editor, which is the primary user interface to
Nightshade.

At its core the editor is a text editor, that is, a program for editing
plain text files.  Various extensions to the basic editing functionality
provide extensive features and a growing number of applications.

The editor is based on the CMUCL editor, Hemlock, which resembles the
ITS/TOPS-20 Emacs.  More recent additions have been heavily influenced by
GNU Emacs (as at 2006).

{function:ed}

[ Editor Introduction         ]
[ Basic Commands              ]
[ Files, Buffers, and Windows ]
[ Editing Documents           ]
[ Managing Large Systems      ]
[ Special Modes               ]
[ Editing Programs            ]
[ Interacting With Lisp       ]
[ Applications                ]        Mail, News and more.
[ Menu                        ]
[ System Interface            ]
[ Simple Customization        ]
]#

#[ Applications

The editor is extended with a growing number of applications.  The
"Applications" submenu of the [menu] lists the applications.  Links to
documentation for some of them follow.

[ Mail                 ]  Read and send electronic mail.
[ Netnews              ]  Read and post to NNTP newsgroups.
[ IRC                  ]  Instant chat.
[ Gopher               ]  Browse Gopher sites.
[ Dired Mode           ]  Manage files and directories.
[ The Package Manager  ]  Install and manage software.
]#

#[ Editor Introduction

This manual describes the editor commands and other user visible features.
It also describes simple customizations.  The [Editor Extension] manual is
available for complete documentation of editor customization and extension.

The editor follows in the tradition of Emacs and the Lisp Machine editor
ZWEI.  In its basic form, it has almost the same command set as ITS/TOPS-20
Emacs, and similar features such as multiple windows and extended commands,
as well as built in documentation.  Whenever some powerful feature of the
editor is described, it is likely to have been directly inspired by Emacs.

[ The Point and The Cursor ]
[ Notation                 ]
[ Invoking Commands        ]
[ The Prefix Argument      ]
[ Modes                    ]
[ Display Conventions      ]
[ Use with X Windows       ]
[ Use With Terminals       ]
[ The Echo Area            ]
[ Online Help              ]
[ Entering and Exiting     ]
[ Helpful Information      ]
[ Recursive Edits          ]
[ User Errors              ]
[ Internal Errors          ]
]#

#[ Notation

There are a number of notational conventions used in this manual which need
some explanation.

[ Key-events       ]
[ Commands         ]
[ Editor Variables ]
]#

#[ Files, Buffers, and Windows

Three abstractions combine at the editing interface.

  File
     A file provides permanent storage of text.  the editor has commands
     to read files into buffers and write buffers out into files.

  Buffer
     A buffer provides temporary storage of text and a capability to
     edit it.  A buffer may or may not have a file associated with it; if it
     does, the text in the buffer need bear no particular relation to the text
     in the file.  In addition, text in a buffer may be displayed in any number
     of windows, or may not be displayed at all.

  Window
     A window displays some portion of a buffer on the screen.  There
     may be any number of windows on the screen, each of which may display any
     position in any buffer.  It is thus possible, and often useful, to have
     several windows displaying different places in the same buffer.

[ Buffers ]
[ Files   ]
[ Windows ]
]#

#[ Managing Large Systems

The editor provides three tools which help to manage large systems:

  1) File groups, which provide several commands that operate on all the files
     in a possibly large collection, instead of merely on a single buffer.

  2) A source comparison facility with semi-automatic merging, which can be used
     to compare and merge divergent versions of a source file.

  3) A change log facility, which maintains a single file containing a record of the
     edits done on a system.

[ File Groups       ]
[ Source Comparison ]
[ Change Logs       ]
]#

#[ Special Modes

[ Dired Mode        ]         Directory editing; file manager.
[ View Mode         ]
[ Process Mode      ]
[ Bufed Mode        ]
[ Completion        ]
[ Caps Lock Mode    ]         CAPS LOCK simulation.
[ Overwrite Mode    ]
[ Word Abbreviation ]
[ Refresh Mode      ]         Automatic buffer refreshing.
]#
;; FIX add packagem above

#[ Editing Programs

[ Comment Manipulation    ]
[ Indentation             ]
[ Editing Lisp            ]
[ Editing Other Languages ]
]#

#[ Interacting With Lisp

Lisp encourages highly interactive programming environments by requiring
decisions about object type and function definition to be postponed until
run time.  The editor supports interactive programming in Lisp by providing
incremental redefinition and environment examination commands.  The editor
also uses Unix TCP sockets to support multiple Lisp processes, each of
which may be on any machine.

[ Eval Servers                       ]
[ Typescripts                        ]
[ The Current Package                ]
[ Compiling and Evaluating Lisp Code ]
[ Compiling Files                    ]
[ Querying the Environment           ]
[ Editing Definitions                ]
[ Inspecting Running Programs        ]
[ Manipulating the Editor Process    ]
[ Editor Command Line Options        ]
]#

#[ System Interface

The editor provides a number of commands that access operating system resources
such as the filesystem and print servers.  These commands offer an alternative
to leaving the editor and using the normal operating system command language
(such as the Unix shell), but they are implementation dependent.  Therefore,
they might not even exist in some implementations.

[ File Utility Commands ]
[ Printing              ]
[ Scribe                ]
[ Miscellaneous         ]
]#

#[ Simple Customization

The editor can be customized and extended to a very large degree, but in
order to do much of this a knowledge of Lisp is required.  These advanced
aspects of customization are discussed in the [ Editor Extension ] Manual,
while simpler methods of customization are discussed here.

[ Keyboard Macros  ]
[ Binding Keys     ]
[ Editor Variables ]
[ Init Files       ]
]#



#[ Editor Extension

This document describes how to write commands for the editor, which is the
primary Nightshade human interface.  The editor is based on the CMUCL
editor, Hemlock, which resembles the ITS/TOPS-20 Emacs.  More recent
additions have been influenced by GNU Emacs.

[ Editor Extension Introduction ]
[ Representation of Text        ]
[ Buffers (extension)           ]
[ Altering and Searching Text   ]
[ The Current Environment       ]
[ Editor Variables (extension)  ]
[ Commands                      ]
[ Modes                         ]
[ Character Attributes          ]
[ Controlling the Display       ]
[ Logical Key-Events            ]
[ The Echo Area (extension)     ]
[ Files (extension)             ]
[ The Editor Lisp Environment   ]
[ High-Level Text Primitives    ]
[ Utilities                     ]
[ Miscellaneous Extension       ]
[ Auxiliary Systems             ]
]#

#[ Editor Extension Introduction

The Nightshade editor is a text editor which follows in the tradition of
editors such as Emacs and the Lisp Machine editor ZWEI.  In its basic form,
it has almost the same command set as Emacs, and similar features such as
multiple buffers and windows, extended commands, and built in
documentation.

Both user extensions and the original commands are written in Lisp,
therefore a command implementor will have a working knowledge of this
language.  Users new to Lisp need not despair however.  Many users of
Multics Emacs, another text editor written in Lisp, came to learn Lisp
simply for the purpose of writing their own editor extensions, and found,
to their surprise, that it was really pretty easy to write simple commands.

This document describes the Lisp functions, macros and data structures that
are used to implement new commands.  The basic editor consists of a set of
Lisp utility functions for manipulating buffers and the other data
structures of the editor as well as handling the display.  All user level
commands are written in terms of these functions.  The chapter [commands]
explains how to define commands.
]#

#[ The Editor Lisp Environment

This chapter is sort of a catch all for any functions and variables
which concern interaction between the editor and the outside world.

[ Entering and Leaving the Editor  ]
[ Keyboard Input                   ]
[ Editor Streams                   ]
[ Interface to the Error System    ]
[ Definition Editing               ]
[ Event Scheduling                 ]
[ Miscellaneous Editor Environment ]
]#

#[ High-Level Text Primitives

This chapter discusses primitives that operate on higher level text forms than
characters and words.  For English text, there are functions that know about
sentence and paragraph structures, and for Lisp sources, there are functions
that understand this language.  This chapter also describes mechanisms for
organizing file sections into logical pages and for formatting text forms.

[ Indenting Text       ]
[ Lisp Text Buffers    ]
[ English Text Buffers ]
[ Logical Pages        ]
[ Filling              ]
]#

#[ Utilities

This chapter describes utilities for manipulating some types of objects
the editor uses to record information.  String-tables are used to store names
of variables, commands, modes, and buffers.  Ring lists can be used to
provide a kill ring, recent command history, or other user-visible
features.

[ Undoing commands ]
]#

#[ Miscellaneous Extension

This chapter is somewhat of a catch-all for comments and features that don't
fit well anywhere else.

[ Generic Pointer Up ]
[ Using View Mode    ]
]#

#[ Auxiliary Systems

This chapter describes auxillary utilities.

[ CLX Interface  ]
[ Slave Lisps    ]
[ Spelling       ]
[ File Utilities ]
[ Beeping        ]
]#


;;;; Setup editor herald.

(pushnew :ed *features*)
(setf (getf ext:*herald-items* :ed) `("    Editor"))


;;;; %init-editor.

(defvar *editor-initialized* ())

(defun %init-editor ()
  "Initialize data structures internal to the editor."
  (if *editor-initialized*
      (warn "%init-editor called with *editor-initialized* true."))
  ;;
  ;; This function is defined in buffer.lisp.  It creates fundamental mode
  ;; and the buffer main.  This must be done in order to define or use
  ;; editor variables.
  (setup-initial-buffer)
  ;;
  ;; Define some of the system variables.
  (define-some-variables)
  ;;
  ;; FIX also in `site-init'
  (setf (ext:search-list "ginfo:") '("/usr/share/info/"))
  ;;
  ;; Site initializations such as window system variables.
  (site-init)
  ;;
  ;; Set up syntax table data structures.
  (%init-syntax-table)
  ;;
  ;; Define print representations for funny characters.
  (%init-line-image)
  (setq *editor-initialized* t))


;;;; DEFINE-SOME-VARIABLES.

;;; This is necessary to define "Default Status Line Fields" which belongs
;;; beside the other modeline variables.  This DEFVAR would live in
;;; morecoms.lisp, but it is compiled and loaded after this file.
;;;
(proclaim '(special ed::*recursive-edit-count*))
;;;
(make-modeline-field
 :name :edit-level :width 15
 :function #'(lambda (buffer window)
	       (declare (ignore buffer window))
	       (if (zerop ed::*recursive-edit-count*)
		   ""
		   (format nil "Edit Level: ~2,'0D "
			   ed::*recursive-edit-count*))))

;;; This is necessary to define "Default Status Line Fields" which belongs
;;; beside the other modeline variables.  This DEFVAR would live in
;;; completion.lisp, but it is compiled and loaded after this file.
;;;
(proclaim '(special ed::*completion-mode-possibility*))
;;; Hack for now until completion mode is added.
(defvar ed::*completion-mode-possibility* "")
;;;
(make-modeline-field
 :name :completion :width 40
 :function #'(lambda (buffer window)
	       (declare (ignore buffer window))
	       ed::*completion-mode-possibility*))

(defun define-some-variables ()
  (defevar "Default Modes"
    "This variable contains the fallback list of modes for new buffers."
    :value '("Fundamental" "Save"))
  (defevar "Echo Area Height"
    "The height in lines of the echo area window."
    :value 3)
  (defevar "Make Buffer Hook"
    "A hook called with the new buffer whenever a buffer is created.
     Called after the modes have been setup on the buffer, and before the
     buffer pathname is set.")
  (defevar "Delete Buffer Hook"
    "A hook called with the buffer whenever a buffer is deleted.")
  (defevar "Enter Recursive Edit Hook"
    "A hook called when a recursive edit is entered.")
  (defevar "Exit Recursive Edit Hook"
    "A hook called with the value returned when a recursive edit is
     exited.")
  (defevar "Abort Recursive Edit Hook"
    "A hook called with the `editor-error' args when a recursive edit is
     terminated.")
  (defevar "Buffer Major Mode Hook"
    "A hook called with the buffer and the new mode when a buffer's major
     mode is changed.")
  (defevar "Buffer Minor Mode Hook"
    "A hook called when the presence of a minor mode in a buffer changes.
     The arguments are the buffer, the mode affected and t or () depending
     on when the mode is being turned on or off.")
  (defevar "Buffer Writable Hook"
    "This hook is called whenever the writable state of buffer changes.  It
     takes the buffer and the new value of the writable flag.  It is called
     after the flag is set.")
  (defevar "Buffer Name Hook"
    "A hook called with the buffer and the new name when the name of a
     buffer is changed.")
  (defevar "Buffer Pathname Hook"
    "A hook called with the buffer and the new pathname when the pathname
     associated with the buffer is changed.")
  (defevar "Buffer Modified Hook"
    "A hook called whenever the modification status of a buffer changes.
     It takes the buffer and the new value for modification flag.")
  (defevar "Set Buffer Hook"
    "A hook called with the new buffer when the current buffer is set.")
  (defevar "After Set Buffer Hook"
    "A hook invoked with the old buffer after the current buffer has been
     changed.")
  (defevar "Set Window Hook"
    "This hook is called with the new window when the current window
     is set.")
  (defevar "Make Window Hook"
    "A hook called with the new window when a window is created.")
  (defevar "Delete Window Hook"
    "A hook called with a window before it is deleted.")
  (defevar "Window Buffer Hook"
    "A hook invoked with the window and new buffer when a window's buffer
     is changed.")
  (defevar "Delete Variable Hook"
    "A hook called when a variable is deleted with the args to
     `delete-variable'.")
  (defevar "Entry Hook"
    "This hook is called when the editor is entered.")
  (defevar "Exit Hook"
    "This hook is called when the editor is exited.  Signalling an
     editor-error, as with (editor-error \"Exit halted.\") will halt the
     exit.")
  (defevar "After Change Hook"
    "This is called on a buffer whenever the buffer text is changed.")
  (defevar "After Command Hook"
    "This is called after every command.")
  (defevar "Redisplay Hook"
    "This is called on the current window from REDISPLAY and REDISPLAY-ALL
     after checking the window display start, window image, and
     recentering.  The smart/dumb redisplay method for the device is called
     after calling the functions in this hook.")
  (defevar "Key Echo Delay"
    "A key binding may be composed of several key-events, especially when
     you enter it using modifier-prefix key-events.  The editor provides
     feedback for partially entered keys by displaying the typed key-events
     in the echo area.  In order to neaten output and clearing of the echo
     area, this display is stalled for `Key Echo Delay' seconds.  If this
     variable is set to (), then the editor foregoes the display of initial
     subsequences of keys."
    :value 1.0)
  (defevar "Input Hook"
    "The command interpreter invokes the functions in `Input Hook' each
     time it reads a key-event from `real-editor-input'.")
  (defevar "Abort Hook"
    "These functions are invoked when ^G is typed.  No arguments are passed.")
  (defevar "Command Abort Hook"
    "These functions get called when commands are aborted, such as with
     `editor-error'.")
  (defevar "Character Attribute Hook"
    "A hook called with the attribute, character and new value when the
     value of a character attribute is changed.")
  (defevar "Shadow Attribute Hook"
    "A hook called when a mode character attribute is made.")
  (defevar "Unshadow Attribute Hook"
    "A hook called when a mode character attribute is deleted.")
  (defevar "Default Modeline Fields"
    "The fallback list of modeline-fields for `make-window'."
    :value (list (modeline-field :buffer-state)
		 (modeline-field :package)
		 (modeline-field :buffer-short-name)
		 ; or (modeline-field :buffer-name)
		 ;; :vc-status goes here.
		 (modeline-field :modes)
		 (modeline-field :space)
		 (modeline-field :space)
		 ; FIX combine these into one col,line,% for convenience
		 (modeline-field :column)
		 (modeline-field :space)
		 (modeline-field :line)
		 (modeline-field :space)
		 (modeline-field :%)
		 (modeline-field :space)
		 ;(modeline-field :position)
		 (modeline-field :buffer-directory)))
  (defevar "Default Status Line Fields"
    "The initial list of modeline-fields for the echo area window
     modeline."
    :value (list (modeline-field :busy)
		 (modeline-field :space)
		 (modeline-field :space)
		 (modeline-field :edit-level)
		 (modeline-field :space)
		 (modeline-field :time)
		 (modeline-field :space)
		 (modeline-field :date)
		 (modeline-field :space)
		 (modeline-field :mail)
		 (modeline-field :space)
		 (modeline-field :machine-instance)
		 (modeline-field :space)
		 (modeline-field :user)
		 (modeline-field :space)
		 (modeline-field :completion)))
  (defevar "Maximum Modeline Pathname Length"
    "When set, this variable is the maximum length of the display of a pathname
     in a modeline.  When the pathname is too long, the :buffer-pathname
     modeline-field function chops off leading directory specifications until
     the pathname fits.  \"...\" indicates a truncated pathname."
    :hooks (list 'maximum-modeline-pathname-length-hook))
  (defevar "Maximum Modeline Short Name Length"
    "When set, this variable is the maximum length of the display of a
     short buffer name in a modeline.  When the buffer name is too long,
     the :buffer-short-name modeline-field function shortens the name, ending
     it in a '!'."
    :hooks (list 'maximum-modeline-pathname-length-hook))
  (defevar "Quit on Exit"
    "If true then Nightshade exits on editor exit."
    :value t)
  (defevar "Inspect on Error"
    "If t inspect when an error occurs, otherwise only if a #\? is typed
     immediately after the error."
    :value ())
  ;; FIX depends on rompsite; here for top-level schedule-event calls
  (defevar "Schedule Event Hook"
    "Invoked on the event when an event is scheduled.")
  (defevar "Notify GC"
    "If true print a message before and after garbage collection.")
  (defevar "Load and Save Histories"
    "If true, load saved history on start and save histories on exit."
    :value t)
  (defevar "Prompt Guide"
    "If true, guide prompting when the prompted value must exist."
    :value t))


;;;; Site init.
;;;
;;; FIX is this really init for a particular site?

;;; *key-event-history* is defined in input.lisp, but it needs to be set in
;;; SITE-INIT, since MAKE-RING doesn't exist at load time for this file.
;;;
(proclaim '(special *key-event-history*))

;;; SITE-INIT  --  Internal
;;;
;;; This function is called at init time to set up any site stuff.
;;;
(defun site-init ()
  (defevar "Beep Border Width"
    "Width in pixels of the border area flashed by border flash beep
     styles."
    :value 20)
  (defevar "Default Window Width"
    "This is the width used to make a window when prompting the user.  The
     value is in characters."
    :value 80)
  (defevar "Default Window Height"
    "This is the height used to make a window when prompting the user.  The
     value is in characters."
    :value 24)
  (defevar "Default Initial Window Width"
    "This is used when the editor first starts up to make its first window.
     The value is in characters."
    :value 80)
  (defevar "Default Initial Window Height"
    "This is used when the editor first starts up to make its first window.
     The value is in characters."
    :value 24)
  (defevar "Default Initial Window X"
    "This is used when the editor first starts up to make its first window.
     The value is in pixels."
    :value 0)
  (defevar "Default Initial Window Y"
    "This is used when the editor first starts up to make its first window.
     The value is in pixels."
    :value 0)
  (defevar "Bell Style"
    "This controls what beeps do in the editor.  Acceptable values are
     :border-flash, :feep, :border-flash-and-feep, :flash, :flash-and-feep,
     and () (carry on in silence)."
    :value :border-flash)
  (defevar "Cursor Bitmap File"
    "The file from which the mouse cursor bitmap is read on startup.  The
     mask is found by merging this name with \".mask\".  This has to be a
     full pathname."
    :value "library:ed11.cursor")
  (defevar "Enter Window Hook"
    "Invoked when the mouse enters an editor window.  These functions take
     the editor window as an argument.")
  (defevar "Exit Window Hook"
    "Invoked when the mouse exits an editor window.  These functions take
     the editor window as an argument.")
  (defevar "Set Window Autoraise"
    "When true, setting the current window will automatically raise that
     window via a function on \"Set Window Hook\".  If the value is
     :echo-only, then only the echo area window will be raised
     automatically upon becoming current."
    :value :echo-only)
  (defevar "Default Font"
    "The string name of the font to be used for normal text display: buffer
     text, modelines, random typeout, etc.  The font is loaded at
     initializing time, so this variable must be set before entering the
     editor.  When (), the display type is used to choose a font."
    :value "*-fixed-medium-r-normal--*-120-*")
  ;; FIX for term?
  (defevar "Active Region Highlighting Font"
    "The string name of the font to be used for highlighting active
     regions.  The font is loaded when initializing the editor."
    :value "*-fixed-medium-o-normal--*-120-*")
  (defevar "Open Paren Highlighting Font"
    "The string name of the font to be used for highlighting open parens.
     If null, then a reasonable default is chosen.  The font is loaded when
     initializing the editor, so this variable must be set before the
     editor is first entered to have any effect."
    :value "*-fixed-bold-r-normal--*-120-*")
  (defevar "Thumb Bar Meter"
    "If true, then X11 windows display a ruler in the bottom border of the
     window."
    :value t)
  (defevar "Initial Foreground Color"
    "The initial foreground color for windows.  Either () or an RGB list (a
     list of a red, a blue and a green value, each between 0.0 and 1.0).
     If () then the terminal-based editor uses the original foreground
     color and the X-based editor uses black.")
  (defevar "Initial Background Color"
    "The initial background color for windows.  Either () or an RGB list (a
     list of a red, a blue and a green value, each between 0.0 and 1.0).
     If () then the terminal-based editor uses the original background
     color and the X-based editor uses white.")
  ;; TODO On terminal, if () use reversed original pair, via standout.
  (defevar "Initial Modeline Foreground Color"
    "The initial foreground color for window modelines.  Either () or an
     RGB list (a list of a red, a blue and a green value, each between 0.0
     and 1.0).  If () then the terminal-based editor uses window background
     (if set, else white), and the X-based editor uses white.")
  (defevar "Initial Modeline Background Color"
    "The initial background color for window modelines.  Either () or an
     RGB list (a list of a red, a blue and a green value, each between 0.0
     and 1.0).  If () then the terminal-based editor uses the window
     foreground (if set, else black) and the X-based editor uses black.")
  (defevar "Initial Border Color"
    "The initial color for border of X11 windows.  A list of a red, a blue
     and a green value, each between 0.0 and 1.0.")
  ;; Reverse Video must come after the colors, as the hook can set them.
  (defevar "Reverse Video"
    "If true, then the editor swaps the foreground and background colors.

     When running in a terminal the original pair is always drawn in the
     original way, even if *Reverse Video* is true.  This means that if one
     of *Initial Foreground Color* and *Initial Background Color* are (),
     then the foreground and background colors of certain text may the
     same, and so the text will be hidden."
    :hooks '(reverse-video-hook-fun))
  (defevar "Kill Ring Hook"
    "Function called with the region whenever a region is killed or saved.")
  (defevar "Activate Region Hook"
    "Function called whenever the region is activated.")
  (defevar "Pacify Region Hook"
    "Function called whenever the region is pacified.")

  (setf *key-event-history* (make-ring 60))
  (setf (ext:search-list "ginfo:") '("/usr/share/info/"))
  ())


#[ Init Files

The editor customizations are normally put in the editor's initialization
file, "nightshade-ed.lisp", or when compiled "nightshade-ed.fasl".  When
starting up Lisp, use the -einit switch to indicate a particular file.  The
contents of the init file must be Lisp code, but there is a fairly
straightforward correspondence between the basic customization commands and
the equivalent Lisp code.  Rather than describe these functions in depth
here, a brief example follows:

;;; -*- Mode: Lisp; Package: Ed -*-

;;; It is necessary to specify that the customizations go in
;;; the editor package.
(in-package 'ed)

;;; Bind `Kill Previous Word' to M-h.
(bind-key "Kill Previous Word" '#(#\m-h))
;;;
;;; Bind `Extract List' to C-M-? when in `Lisp' mode.
(bind-key "Extract List" '#(#\c-m-?) :mode "Lisp")

;;; Make C-w globally unbound.
(delete-key-binding '#(#\c-w))

;;; Make string searches case-sensitive.
(setv string-search-ignore-case ())
;;;
;;; Make "Query Replace" replace strings literally.
(setv case-replace ())

The [ Editor Extension ] Manual describes these functions in detail.
]#


;;;; ED.

(defvar *editor-has-been-entered* ()
  "True if and only if the editor has been entered.")

(defvar *in-the-editor* ()
  "True if we are inside the editor.  This is used to prevent ill-advised
   \"recursive\" edits.")

(defvar *after-editor-initializations-funs* nil
  "A list of functions to be called after the editor has been initialized
   (including loading the initialization file) upon entering the first
   time.")

(defmacro after-editor-initializations (&rest forms)
  "Cause $forms to be executed after the editor has been initialized
   (including loading the initialization file).  Execute forms supplied
   with successive uses of this macro after forms supplied with previous
   uses."
  `(push #'(lambda () ,@forms) *after-editor-initializations-funs*))

#[ Entering and Leaving the Editor

{function:ed:ed}
{evariable:Entry Hook}
{function:ed:exit}
{function:ed:pause}
]#

(defswitch "xoff")

(defun ed (&optional arg
	   &key (init t)
	        (xoff (or (find "xoff" ext:*command-line-switches*
				:test #'string=
				:key #'ext:cmd-switch-name)
			  (member (file-namestring
				   ext:*command-line-utility-name*)
				  '("net" "nit")
				  :test #'string=)))
	        (display (cdr (assoc :display ext:*environment-list*))))
  "Invoke the editor.

   If $arg is supplied and is a symbol, put the definition of $arg into a
   buffer, and select that buffer.  If $arg is a pathname, visit the file
   specified by $arg in a new buffer.  If $arg is (), enter the editor in
   the state from which it last exited.

   When $init is t, load either the file \"ed.lisp\" from the config
   directory (which is usually \"home:.nightshade/\"), or the file
   specified by the command line switch -einit.  A compiled version of the
   source is preferred when choosing the file to load.  If $init is a true
   value other than #t, then merge that value with the home directory.

   If $xoff is true then load the terminal version of the editor instead of
   trying to load the windowed version.

   Invoke *Entry Hook* before accepting commands."
  (if *in-the-editor* (error "The editor is already running."))
  (let ((*in-the-editor* t)
	(*busy* t)
	(display (unless *editor-has-been-entered*
		   (to-file (out "conf:clean-exit")
		     (write () :stream out) (terpri out))
		   (setf (ext:search-list "ginfo:") '("/usr/share/info/"))
		   ;; Hack the busy modeline field for X.
		   #+clx
		   (or xoff
		       (if display
			   (setf (car (value ed::default-status-line-fields))
				 (modeline-field :busy-or-menu))))
		   ;; Make `windowed-monitor-p' work for init.
		   (setf *editor-windowed-input* (fi xoff display))
		   (maybe-load-init init)
		   (maybe-load-histories init)
		   ;; Device dependent initializaiton.
		   (init-raw-io display xoff))))
    (handler-bind
	((editor-top-level-catcher #'(lambda (condition)
				       (declare (ignore condition))
				       (if (value ed::quit-on-exit)
					   (quit)
					   (return-from ed)))))
      (site-wrapper-macro
       (let ((continue t)) ; Flag for stopping exit in an exit-hook.
	 (while () (continue)
	   (catch 'editor-exit
	     (or *editor-has-been-entered*
		 (block ed-init
		   ;; Make an initial window, and set up redisplay's
		   ;; internal data structures.
		   (%init-redisplay display)
		   (setq *editor-has-been-entered* t)
		   (handler-bind
		       ((editor-top-level-catcher
			 #'(lambda (condition)
			     (declare (ignore condition))
			     (return-from ed-init))))
		     ;; Pick up user initializations to be done after
		     ;; initialization.
		     (invoke-hook
		      (reverse *after-editor-initializations-funs*)))))

	     (block ed-x
	       (handler-bind
		   ((editor-top-level-catcher
		     #'(lambda (condition)
			 (declare (ignore condition))
			 (return-from ed-x))))
		 (typecase arg
		   (null)
		   (symbol
		    (let* ((name (nstring-capitalize
				  (concatenate 'simple-string
					       "Edit "
					       (string arg))))
			   (buffer (or (getstring name *buffer-names*)
				       (make-buffer name)))
			   (*print-case* :downcase))
		      (delete-region (buffer-region buffer))
		      (with-output-to-mark
			  (*standard-output* (buffer-point buffer))
			(eval `(describe ,arg))
			(terpri)
			(ed::change-to-buffer buffer)
			(buffer-start (buffer-point buffer)))))
		   ((or string pathname)
		    (ed::find-command () arg))
		   (t
		    (error
		     "~S must be a symbol or pathname." arg)))))

	     (invoke-hook ed::entry-hook)
	     (unwind-protect
		 (loop
		   (with-simple-restart (ed "Return to editor.")
		     (block ed-command-loop
		       (handler-case
			   ;(handler-case
			   (handler-bind ((type-error #'lisp-error-error-handler)
					  (error #'lisp-error-error-handler))
			     (progn
			       (invoke-hook ed::abort-hook)  ; control-g
			       (%command-loop)))
			 ;; FIX how to pass :internal to handler-bind?
			 ;(error (condition)
			 ;	(lisp-error-error-handler
			 ;	 condition :internal)))
			 (editor-top-level-catcher ()
						   (return-from ed-command-loop))))))
	       (setq continue ())
	       (handler-case
		   (invoke-hook ed::exit-hook)
		 (editor-error () (setq continue t))
		 (editor-top-level-catcher () (setq continue t))
		 (error (condition)
			(let ((device (device-hunk-device
				       (window-hunk (current-window)))))
			  (funcall (device-exit device) device))
			(invoke-debugger condition))))))))))
  (to-file (out "conf:clean-exit") (write t :stream out) (terpri out))
  (if (value ed::quit-on-exit) (quit)))

(defun maybe-load-init (init)
  (when init
    (let* ((switch (find "einit" *command-line-switches*
			 :test #'string-equal
			 :key #'cmd-switch-name))
	   (spec-name
	    (if (not (eq init t))
		init
		(and switch
		     (or (cmd-switch-value switch)
			 (car (cmd-switch-words switch)))))))
      (if spec-name
	  (load (merge-pathnames spec-name (user-homedir-pathname))
		:if-does-not-exist nil)
	  (load (config:config-pathname "ed") :if-does-not-exist nil)))))


;;;; SAVE-ALL-BUFFERS.

;;; SAVE-ALL-BUFFERS -- Public.
;;;
(defun save-all-buffers (&optional (list-unmodified-buffers nil))
  "This prompts users with each modified buffer as to whether they want to
   write it out.  If the buffer has no associated file, this will also
   prompt for a file name.  Supplying the optional argument true causes
   this to prompt for every buffer."
  (dolist (buffer *buffer-list*)
    (when (or list-unmodified-buffers (buffer-modified buffer))
      (maybe-save-buffer buffer))))

(defun maybe-save-buffer (buffer)
  (let* ((modified (buffer-modified buffer))
	 (pathname (buffer-pathname buffer))
	 (name (buffer-name buffer))
	 (string (if pathname (namestring pathname))))
    (format t "Buffer ~S is ~:[UNmodified~;modified~], Save it? "
	    name modified)
    (force-output)
    (when (y-or-n-p)
      (let ((name (read-line-default "File to write" string)))
	(format t "Writing file ~A..." name)
	(force-output)
	(write-file (buffer-region buffer) name)
	(write-line "written.")))))

(defun read-line-default (prompt default)
  (format t "~A:~@[ [~A]~] " prompt default)
  (force-output)
  (do ((result (read-line) (read-line)))
      (())
    (declare (simple-string result))
    (when (plusp (length result)) (return result))
    (when default (return default))
    (format t "~A:~@[ [~A]~] " prompt default)
    (force-output)))

(or *editor-initialized* (%init-editor))
