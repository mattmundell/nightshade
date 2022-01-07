;;; Key bindings.

;; FIX lay these out nicely

(in-package "ED")

(export '(frob-[-keys frob-help-key frob-hyper-key))


#[ Key Bindings

A key is a sequence of key-events (see section [Key Events]) typed on the
keyboard, usually only one or two in length.  Sections [Using X] and
[Using Terminals] contain information on particular input devices.

When a command is bound to a key, typing the key causes the editor to invoke the
command.  When the command completes its job, the editor returns to reading
another key, and this continually repeats.

Some commands read key-events interpreting them however each command desires.
When commands do this, key bindings have no effect, but you can usually abort
the editor whenever it is waiting for input by typing C-g (see section
[Aborting]).  You can usually find out what options are available by typing
C-_ or Home (see section [Help]).

The user can easily rebind keys to different commands, bind new keys to
commands, or establish bindings for commands never bound before (see section
[Binding Keys]).

In addition to the key bindings explicitly listed with each command, there
are some implicit bindings created by using key translationsfootKey
translations are documented in the [Editor Extension] Manual.  These
bindings are not displayed by documentation commands such as `Where Is'.
By default, there are only a few key translations.  The modifier-prefix
characters C-^, Escape, C-z, or C-c may be used when typing keys to convert
the following key-event to a control, meta, control-meta, or hyper
key-event.  For example, C-x Escape b invokes the same commands as C-x M-b,
and C-z u is the same as C-M-u.  This allows user to type more interesting
keys on limited keyboards that lack control, meta, and hyper keys.

{evariable:Key Echo Delay}
]#

;; FIX other x apps support shift-tab


;;;; Default key translations.

;;; This page defines prefix characters that set specified modifier bits on
;;; the next character typed.
;;;
(setf (key-translation #k"escape") '(:bits :meta))
; control-z is for Pause.
;(setf (key-translation #k"control-z") '(:bits :control :meta))
;(setf (key-translation #k"control-Z") '(:bits :control :meta))
(setf (key-translation #k"control-^") '(:bits :control))
; control-c is for the user.
;(setf (key-translation #k"control-c") '(:bits :hyper))
;(setf (key-translation #k"control-C") '(:bits :hyper))


;;;; User

;;; control-c <alphanum> is for personal bindings.  Perhaps punctuation
;;; symbols should be too.

(bind-key "Comment Region" #k"control-c ;")


;;;; Most global bindings.

;;; Self insert letters:
;;;
(do-alpha-key-events (key-event :both)
  (bind-key "Self Insert" key-event))

(bind-key "Beginning of Line" #k"control-a")
(bind-key "Delete Next Character" #k"control-d")
(bind-key "End of Line" #k"control-e")
(bind-key "Forward Character" #k"control-f")
(bind-key "Forward Character" #k"rightarrow")
(bind-key "Backward Character" #k"control-b")
(bind-key "Backward Character" #k"leftarrow")
(bind-key "Kill Line" #k"control-k")
(bind-key "Refresh Screen" #k"control-l")
(bind-key "Next Line" #k"control-n")
(bind-key "Next Line" #k"downarrow")
(bind-key "Previous Line" #k"control-p")
(bind-key "Previous Line" #k"uparrow")
(bind-key "Query Replace" #k"meta-%")
(bind-key "Reverse Incremental Search" #k"control-r")
(bind-key "Incremental Search" #k"control-s")
(bind-key "Forward Search" #k"meta-s")
(bind-key "Reverse Search" #k"meta-r")
(bind-key "Transpose Characters" #k"control-t")
(bind-key "Universal Argument" #k"control-u")
(bind-key "Scroll Window Down" #k"control-v")
(bind-key "Scroll Window Up" #k"meta-v")
(bind-key "Scroll Next Window Down" #k"control-meta-v")
(bind-key "Scroll Next Window Up" #k"control-meta-V")
(bind-key "Scroll Window Down" #k"pagedown")
(bind-key "Scroll Window Up" #k"pageup")
(bind-key "Scroll Next Window Down" #k"control-pagedown")
(bind-key "Scroll Next Window Up" #k"control-pageup")

(bind-key "Help"         #k"f1")
(bind-key "Undo"         #k"control-_")

;; Mouse
(bind-key "Point to Here"            #k"leftdown")
(bind-key "Generic Pointer Up"       #k"leftup")
(bind-key "Previous Line"            #k"fourdown")
(bind-key "Do Nothing"               #k"fourup")
(bind-key "Next Line"                #k"fivedown")
(bind-key "Do Nothing"               #k"fiveup")
(bind-key "Menu"                     #k"rightdown")
(bind-key "Do Nothing"               #k"rightup")
#+clx
(bind-key "Insert Selection"         #k"middledown")
(bind-key "Do Nothing"               #k"middleup")
#|
(bind-key "Here to Top of Window" #k"leftdown")
(bind-key "Do Nothing" #k"leftup")
(bind-key "Top Line to Here" #k"rightdown")
(bind-key "Do Nothing" #k"rightup")
(bind-key "Point to Here" #k"middledown")
(bind-key "Point to Here" #k"super-leftdown")
(bind-key "Generic Pointer Up" #k"middleup")
(bind-key "Generic Pointer Up" #k"super-leftup")
(bind-key "Do Nothing" #k"super-rightup")
(bind-key "Insert Kill Buffer" #k"super-rightdown")
|#

(bind-key "Insert File" #k"control-x control-r")
(bind-key "Save File" #k"control-x control-s")
;(bind-key "Visit File" #k"control-x control-v")
(bind-key "Write File" #k"control-x control-w")
;(bind-key "Find File" #k"control-x control-f")
(bind-key "Find" #k"control-x control-f")
(bind-key "Backup File" #k"control-x meta-b")
(bind-key "Save All Files" #k"control-x control-m")
(bind-key "Save All Files" #k"control-x return")
(bind-key "Save All Files and Exit" #k"control-x control-z")
(bind-key "Revert File" #k"control-c meta-r")


;(bind-key "List Buffers"            #k"control-x control-b")
(bind-key "Clear Buffer Modified"   #k"meta-~")
(bind-key "Check Buffer Modified"   #k"control-x ~")
(bind-key "Set Buffer Read Only"    #k"control-x control-q")
;(bind-key "Select Buffer"           #k"control-x b")
;(bind-key "Select Or Create Buffer" #k"control-x b")
(bind-key "Switch to Buffer"        #k"control-x b")
(bind-key "Select Previous Buffer"  #k"control-meta-l")
(bind-key "Circulate Buffers"       #k"control-meta-L")
(bind-key "Rotate Buffers Forward"  #k"control-x control-b")
(bind-key "Rotate Buffers Backward" #k"control-x control-v")
;(bind-key "Create Buffer"           #k"control-x meta-b")
; FIX clumbsy, maybe add shift/alt modifier to return in Switch To Buffer
(bind-key "Create Buffer" #k"control-x control-meta-b")
(bind-key "Copy Buffer"             #k"control-x n")
(bind-key "Copy Buffer Next Window" #k"control-x 4 n")
(bind-key "Switch to Next Copy"     #k"control-x control-n")
(bind-key "Kill Buffer"             #k"control-x k")
(bind-key "Select Random Typeout Buffer" #k"hyper-t")

;(bind-key "Next Window" #k"control-x o")
(bind-key "Next Window" #k"control-o")
(bind-key "Previous Window" #k"control-x p")
(bind-key "Split Window" #k"control-x 2")
;(bind-key "New Window" #k"control-x control-n")
(bind-key "Go to one window" #k"control-x 1")
;(bind-key "Delete Next Window" #k"control-x 1")
(bind-key "Delete Window" #k"control-x 0")
;(bind-key "Line to Top of Window" #k"meta-!") ; FIX should be where then?
(bind-key "Saving Shell Command Line in Buffer" #k"meta-!")
(bind-key "Line to Center of Window" #k"meta-#")
(bind-key "Top of Window" #k"meta-,")
(bind-key "Bottom of Window" #k"meta-.")

(bind-key "Pause" #k"control-z")
(bind-key "Exit" #k"control-x meta-z")
;(bind-key "Exit Recursive Edit" #k"control-meta-z")
(bind-key "Exit Recursive Edit" #k"control-c control-c")
(bind-key "Abort Recursive Edit" #k"control-]")

(bind-key "Delete Previous Character" #k"delete")
(bind-key "Delete Previous Character" #k"backspace")
(bind-key "Kill Next Word" #k"meta-d")
(bind-key "Kill Previous Word" #k"meta-delete")
(bind-key "Kill Previous Word" #k"meta-backspace")
(bind-key "Exchange Point and Mark" #k"control-x control-x")
(bind-key "Mark Whole Buffer" #k"control-x h")
(bind-key "Set/Pop Mark" #k"control-@")
(bind-key "Set/Pop Mark" #k"control-space")
(bind-key "Pop and Goto Mark" #k"meta-space")
(bind-key "Pop and Goto Mark" #k"meta-@")
(bind-key "Pop Mark" #k"control-meta-space")  ;#k"control-meta-@" = "Mark Form".
(bind-key "Kill Region" #k"control-w")
(bind-key "Save Region" #k"meta-w")
(bind-key "Yank" #k"control-y")
(bind-key "Rotate Kill Ring" #k"meta-y")
(bind-key "Browse Kill Ring" #k"control-meta-y")

(bind-key "Forward Word" #k"meta-f")
(bind-key "Backward Word" #k"meta-b")

(bind-key "Forward Paragraph" #k"meta-]")
(bind-key "Forward Sentence" #k"meta-e")
(bind-key "Backward Paragraph" #k"meta-[")
(bind-key "Backward Sentence" #k"meta-a")

(bind-key "Mark Paragraph" #k"meta-h")

(bind-key "Forward Kill Sentence" #k"meta-k")
(bind-key "Backward Kill Sentence" #k"control-x delete")
(bind-key "Backward Kill Sentence" #k"control-x backspace")

(bind-key "Beginning of Buffer" #k"meta-\<")
(bind-key "Beginning of Buffer" #k"home")
(bind-key "End of Buffer" #k"meta-\>")
(bind-key "End of Buffer" #k"end")
(bind-key "Mark to Beginning of Buffer" #k"control-\<")
(bind-key "Mark to Beginning of Buffer" #k"control-home")
(bind-key "Mark to End of Buffer" #k"control-\<")
(bind-key "Mark to End of Buffer" #k"control-end")

(bind-key "Extended Command" #k"meta-x")

(bind-key "Uppercase Word" #k"meta-u")
(bind-key "Lowercase Word" #k"meta-l")
(bind-key "Capitalize Word" #k"meta-c")

; FIX Goto Previous Page
(bind-key "Previous Page" #k"control-x [")
(bind-key "Goto Next Page" #k"control-x ]")
(bind-key "Mark Page" #k"control-x control-p")
(bind-key "Count Lines Page" #k"control-x l")
(bind-key "Count Lines and Characters" #k"meta-=")
(bind-key "Describe Cursor Position" #k"control-x =")

(bind-key "Next Search Match" #k"meta-`")

(bind-key "Overwrite Mode" #k"insert")


;;;; Argument Digit and Negative Argument.

(bind-key "Negative Argument" #k"meta-\-")
(bind-key "Argument Digit" #k"meta-0")
(bind-key "Argument Digit" #k"meta-1")
(bind-key "Argument Digit" #k"meta-2")
(bind-key "Argument Digit" #k"meta-3")
(bind-key "Argument Digit" #k"meta-4")
(bind-key "Argument Digit" #k"meta-5")
(bind-key "Argument Digit" #k"meta-6")
(bind-key "Argument Digit" #k"meta-7")
(bind-key "Argument Digit" #k"meta-8")
(bind-key "Argument Digit" #k"meta-9")
(bind-key "Negative Argument" #k"control-\-")
(bind-key "Argument Digit" #k"control-0")
(bind-key "Argument Digit" #k"control-1")
(bind-key "Argument Digit" #k"control-2")
(bind-key "Argument Digit" #k"control-3")
(bind-key "Argument Digit" #k"control-4")
(bind-key "Argument Digit" #k"control-5")
(bind-key "Argument Digit" #k"control-6")
(bind-key "Argument Digit" #k"control-7")
(bind-key "Argument Digit" #k"control-8")
(bind-key "Argument Digit" #k"control-9")
(bind-key "Negative Argument" #k"control-meta-\-")
(bind-key "Argument Digit" #k"control-meta-0")
(bind-key "Argument Digit" #k"control-meta-1")
(bind-key "Argument Digit" #k"control-meta-2")
(bind-key "Argument Digit" #k"control-meta-3")
(bind-key "Argument Digit" #k"control-meta-4")
(bind-key "Argument Digit" #k"control-meta-5")
(bind-key "Argument Digit" #k"control-meta-6")
(bind-key "Argument Digit" #k"control-meta-7")
(bind-key "Argument Digit" #k"control-meta-8")
(bind-key "Argument Digit" #k"control-meta-9")


;;;; Self Insert and Quoted Insert.

(bind-key "Quoted Insert" #k"control-q")

(bind-key "Self Insert" #k"space")
(bind-key "Self Insert" #k"!")
(bind-key "Self Insert" #k"@")
(bind-key "Self Insert" #k"#")
(bind-key "Self Insert" #k"$")
(bind-key "Self Insert" #k"%")
(bind-key "Self Insert" #k"^")
(bind-key "Self Insert" #k"&")
(bind-key "Self Insert" #k"*")
(bind-key "Self Insert" #k"(")
(bind-key "Self Insert" #k")")
(bind-key "Self Insert" #k"_")
(bind-key "Self Insert" #k"+")
(bind-key "Self Insert" #k"~")
(bind-key "Self Insert" #k"1")
(bind-key "Self Insert" #k"2")
(bind-key "Self Insert" #k"3")
(bind-key "Self Insert" #k"4")
(bind-key "Self Insert" #k"5")
(bind-key "Self Insert" #k"6")
(bind-key "Self Insert" #k"7")
(bind-key "Self Insert" #k"8")
(bind-key "Self Insert" #k"9")
(bind-key "Self Insert" #k"0")
(bind-key "Self Insert or Previous Page" #k"[")
(bind-key "Self Insert or Next Page" #k"]")
(bind-key "Self Insert" #k"\\")
(bind-key "Self Insert" #k"|")
(bind-key "Self Insert" #k":")
(bind-key "Self Insert" #k";")
(bind-key "Self Insert" #k"\"")
(bind-key "Self Insert" #k"'")
(bind-key "Self Insert" #k"\-")
(bind-key "Self Insert" #k"=")
(bind-key "Self Insert" #k"`")
(bind-key "Self Insert" #k"\<")
(bind-key "Self Insert" #k"\>")
(bind-key "Self Insert" #k",")
(bind-key "Self Insert" #k".")
(bind-key "Self Insert" #k"?")
(bind-key "Self Insert" #k"/")
(bind-key "Self Insert" #k"{")
(bind-key "Self Insert" #k"}")


;;;; Goto.

(bind-key "Edit Command Definition"          #k"meta-g meta-c")
;; FIX should be analogous to c-h d (~describe generic def)
;; Go Directly
(bind-key "Goto Definition"                  #k"meta-g meta-d")
(bind-key "Edit Editor Variable Definition"  #k"meta-g meta-e")
(bind-key "Edit Definition"                  #k"meta-g meta-f") ; f for function
(bind-key "Goto Absolute Line"               #k"meta-g meta-g")
(bind-key "Goto Absolute Character"          #k"meta-g meta-k")
(bind-key "Goto Absolute Line"               #k"meta-g meta-l")
;(bind-key "Edit Variable Definition"        #k"meta-g meta-v")
(bind-key "Edit Command Definition"          #k"meta-g c")
(bind-key "Goto Definition"                  #k"meta-g d")
(bind-key "Edit Editor Variable Definition"  #k"meta-g e")
(bind-key "Edit Definition"                  #k"meta-g f") ; f for function
; FIX meta-< control-u <num> control-n
(bind-key "Goto Absolute Line"               #k"meta-g g")
(bind-key "Goto Absolute Character"          #k"meta-g k")
(bind-key "Goto Absolute Line"               #k"meta-g l")
(bind-key "Goto Page"                        #k"meta-g p")
(bind-key "Edit TODO"                        #k"meta-g t")
;(bind-key "Edit Variable Definition"        #k"meta-g v")


;;;; Echo Area.

;;; Basic echo-area commands.
;;;
(bind-key "Help on Parse" #k"home" :mode "Echo Area")

(bind-key "Complete Field" #k"space" :mode "Echo Area")
(bind-key "Confirm Parse" #k"return" :mode "Echo Area")

;;; Rebind some standard commands to behave better.
;;;
(bind-key "Insert Parse Default" #k"meta-i" :mode "Echo Area")
(bind-key "Complete Field" #k"tab" :mode "Echo Area")
(bind-key "Echo Area Delete Previous Character" #k"delete" :mode "Echo Area")
(bind-key "Echo Area Delete Previous Character" #k"backspace" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"meta-h" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"meta-delete" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"meta-backspace" :mode "Echo Area")
(bind-key "Kill Region" #k"control-w" :mode "Echo Area")
(bind-key "Beginning of Parse" #k"control-a" :mode "Echo Area")
(bind-key "Beginning of Parse" #k"meta-\<" :mode "Echo Area")
(bind-key "Echo Area Backward Character" #k"control-b" :mode "Echo Area")
(bind-key "Echo Area Backward Word" #k"meta-b" :mode "Echo Area")
(bind-key "Next Parse" #k"meta-n" :mode "Echo Area")
(bind-key "Previous Parse" #k"meta-p" :mode "Echo Area")
(bind-key "Next Line" #k"control-n" :mode "Echo Area")
(bind-key "Previous Line" #k"control-p" :mode "Echo Area")
(bind-key "Kill Parse" #k"control-x backspace" :mode "Echo Area") ;; FIX
(bind-key "Undo" #k"control-_" :mode "Echo Area")

;;; Remove some dangerous standard bindings.
;;;
(bind-key "Editor Error" #k"control-x" :mode "Echo Area")
(bind-key "Editor Error" #k"control-meta-c" :mode "Echo Area")
(bind-key "Editor Error" #k"control-meta-s" :mode "Echo Area")
(bind-key "Editor Error" #k"control-meta-l" :mode "Echo Area")
(bind-key "Editor Error" #k"meta-x" :mode "Echo Area")
(bind-key "Editor Error" #k"control-s" :mode "Echo Area")
(bind-key "Editor Error" #k"control-r" :mode "Echo Area")
(bind-key "Editor Error" #k"hyper-t" :mode "Echo Area")
(bind-key "Editor Error" #k"middledown" :mode "Echo Area")
(bind-key "Do Nothing" #k"middleup" :mode "Echo Area")
(bind-key "Editor Error" #k"super-leftdown" :mode "Echo Area")
(bind-key "Do Nothing" #k"super-leftup" :mode "Echo Area")
(bind-key "Editor Error" #k"super-rightdown" :mode "Echo Area")
(bind-key "Do Nothing" #k"super-rightup" :mode "Echo Area")


;;;; Eval and Editor Modes.

(bind-key "Confirm Eval Input" #k"return" :mode "Eval")
(bind-key "Previous Interactive Input" #k"meta-p" :mode "Eval")
(bind-key "Search Previous Interactive Input" #k"meta-P" :mode "Eval")
(bind-key "Next Interactive Input" #k"meta-n" :mode "Eval")
(bind-key "Kill Interactive Input" #k"meta-i" :mode "Eval")
(bind-key "Abort Eval Input" #k"control-meta-i" :mode "Eval")
(bind-key "Interactive Beginning of Line" #k"control-a" :mode "Eval")
(bind-key "Reenter Interactive Input" #k"control-return" :mode "Eval")

(bind-key "Editor Evaluate Expression" #k"control-meta-escape")
(bind-key "Editor Evaluate Expression" #k"meta-escape"  :mode "Editor")
(bind-key "Editor Evaluate Defun" #k"control-x control-e" :mode "Editor")
(bind-key "Editor Compile Defun" #k"control-x control-c" :mode "Editor")
(bind-key "Editor Compile Defun" #k"control-x control-C" :mode "Editor")
(bind-key "Editor Macroexpand Expression" #k"control-m" :mode "Editor")
(bind-key "Editor Describe Function Call" #k"control-meta-A" :mode "Editor")
(bind-key "Editor Describe Symbol" #k"control-meta-S" :mode "Editor")


;;;; Typescript.

(bind-key "Confirm Typescript Input" #k"return" :mode "Typescript")
(bind-key "Interactive Beginning of Line" #k"control-a" :mode "Typescript")
(bind-key "Kill Interactive Input" #k"meta-i" :mode "Typescript")
(bind-key "Previous Interactive Input" #k"meta-p" :mode "Typescript")
(bind-key "Search Previous Interactive Input" #k"meta-P" :mode "Typescript")
(bind-key "Next Interactive Input" #k"meta-n" :mode "Typescript")
(bind-key "Reenter Interactive Input" #k"control-return" :mode "Typescript")
(bind-key "Typescript Slave Break" #k"hyper-b" :mode "Typescript")
(bind-key "Typescript Slave to Top Level" #k"hyper-g" :mode "Typescript")
(bind-key "Typescript Slave Status" #k"hyper-s" :mode "Typescript")
(bind-key "Select Slave" #k"control-meta-\c")
(bind-key "Select Background" #k"control-meta-C")

(bind-key "Abort Operations" #k"hyper-a")
(bind-key "List Operations" #k"hyper-l")

(bind-key "Next Compiler Error" #k"hyper-n")
(bind-key "Previous Compiler Error" #k"hyper-p")


;;;; Lisp (some).

(bind-key "Indent Form" #k"control-meta-q")
(bind-key "Fill Lisp Comment Paragraph" #k"meta-q" :mode "Lisp")
(bind-key "Defindent" #k"control-meta-#")
(bind-key "Beginning of Defun" #k"control-meta-[")
(bind-key "End of Defun" #k"control-meta-]")
(bind-key "Beginning of Defun" #k"control-meta-a")
(bind-key "End of Defun" #k"control-meta-e")
(bind-key "Forward Form" #k"control-meta-f")
(bind-key "Backward Form" #k"control-meta-b")
(bind-key "Forward List" #k"control-meta-n")
(bind-key "Backward List" #k"control-meta-p")
(bind-key "Transpose Forms" #k"control-meta-t")
(bind-key "Forward Kill Form" #k"control-meta-k")
(bind-key "Backward Kill Form" #k"control-meta-backspace")
(bind-key "Backward Kill Form" #k"control-meta-delete")
(bind-key "Mark Form" #k"control-meta-@")
(bind-key "Mark Defun" #k"control-meta-h")
(bind-key "Insert ()" #k"meta-(")
(bind-key "Move over )" #k"meta-)")
(bind-key "Backward Up List" #k"control-meta-(")
(bind-key "Backward Up List" #k"control-meta-u")
(bind-key "Forward Up List" #k"control-meta-)")
; FIX should be ~"forward delete form" to match meta-d?
(bind-key "Down List" #k"control-meta-d")
(bind-key "Extract List" #k"control-meta-x")
(bind-key "Lisp Insert )" #k")" :mode "Lisp")
(bind-key "Delete Previous Character Expanding Tabs" #k"backspace" :mode "Lisp")
(bind-key "Delete Previous Character Expanding Tabs" #k"delete" :mode "Lisp")

(bind-key "Evaluate Expression" #k"meta-escape")
(bind-key "Evaluate Defun" #k"control-x control-e")
(bind-key "Compile Defun" #k"control-x control-c")
(bind-key "Compile Buffer File" #k"control-x c")
(bind-key "Macroexpand Expression" #k"control-M")

(bind-key "Describe Function Call" #k"control-meta-A")
(bind-key "Describe Symbol" #k"control-meta-S")

(bind-key "Goto Definition" #k"control-meta-F")

(bind-key "Inspect Up" #k"control-meta-hyper-u")
(bind-key "Inspect Down" #k"control-meta-hyper-d")
(bind-key "Inspect Top" #k"control-meta-hyper-t")
(bind-key "Inspect Bottom" #k"control-meta-hyper-b")
(bind-key "Inspect Frame" #k"control-meta-hyper-f")
(bind-key "Inspect Quit" #k"control-meta-hyper-q")
(bind-key "Inspect Go" #k"control-meta-hyper-g")
(bind-key "Inspect Abort" #k"control-meta-hyper-a")
(bind-key "Inspect Restart" #k"control-meta-hyper-r")
(bind-key "Inspect Help" #k"control-meta-hyper-h")
(bind-key "Inspect Error" #k"control-meta-hyper-e")
(bind-key "Inspect Backtrace" #k"control-meta-hyper-B")
(bind-key "Inspect Print" #k"control-meta-hyper-p")
(bind-key "Inspect Verbose Print" #k"control-meta-hyper-P")
(bind-key "Inspect List Locals" #k"control-meta-hyper-l")
(bind-key "Inspect Source" #k"control-meta-hyper-s")
(bind-key "Inspect Edit Source" #k"control-meta-hyper-S")
(bind-key "Inspect Flush Errors" #k"control-meta-hyper-F")


;;;; More Miscellaneous bindings.

;(bind-key "Open Line" #k"control-o")
(bind-key "Open Line" #k"control-x o")
(bind-key "New Line" #k"return")

(bind-key "Transpose Words" #k"meta-t")
(bind-key "Transpose Lines" #k"control-x control-t")
(bind-key "Transpose Regions" #k"control-x t")

(bind-key "Uppercase Region" #k"control-x control-u")
(bind-key "Lowercase Region" #k"control-x control-l")

(bind-key "Delete Indentation" #k"meta-^")
(bind-key "Delete Indentation" #k"control-meta-^")
(bind-key "Delete Horizontal Space" #k"meta-\\")
(bind-key "Delete Blank Lines" #k"control-x control-o" :global)
(bind-key "Just One Space" #k"meta-\|")
(bind-key "Back to Indentation" #k"meta-m")
(bind-key "Back to Indentation" #k"control-meta-m")
(bind-key "Indent Rigidly" #k"control-x tab")
(bind-key "Indent Rigidly" #k"control-x control-i")

(bind-key "Indent New Line" #k"linefeed")
(bind-key "Indent" #k"tab")
(bind-key "Indent" #k"control-i")
(bind-key "Indent Region" #k"control-meta-\\")
(bind-key "Quote Tab" #k"meta-tab")

(bind-key "Directory" #k"control-x control-\d")
(bind-key "Verbose Directory" #k"control-x control-D")

(bind-key "Activate Region" #k"control-x control-@")
(bind-key "Activate Region" #k"control-x control-space")

(bind-key "Save Position" #k"control-x s")
(bind-key "Jump to Saved Position" #k"control-x j")
(bind-key "Put Register" #k"control-x x")
(bind-key "Get Register" #k"control-x g")

(bind-key "Delete Previous Character Expanding Tabs" #k"backspace"
	  :mode "Pascal")
(bind-key "Delete Previous Character Expanding Tabs" #k"delete" :mode "Pascal")
(bind-key "Scribe Insert Bracket" #k")" :mode "Pascal")
(bind-key "Scribe Insert Bracket" #k"]" :mode "Pascal")
(bind-key "Scribe Insert Bracket" #k"}" :mode "Pascal")


;;;; Auto Fill Mode.

(bind-key "Fill Paragraph" #k"meta-q")
; FIX used for goto
;(bind-key "Fill Region" #k"meta-g")
(bind-key "Set Fill Prefix" #k"control-x .")
(bind-key "Set Fill Column" #k"control-x f")
(bind-key "Auto Fill Return" #k"return" :mode "Fill")
(bind-key "Auto Fill Space" #k"space" :mode "Fill")
(bind-key "Auto Fill Linefeed" #k"linefeed" :mode "Fill")


;;;; Keyboard macro bindings.

(bind-key "Define Keyboard Macro" #k"control-x (")
(bind-key "Define Keyboard Macro Key" #k"control-x meta-(")
(bind-key "End Keyboard Macro" #k"control-x )")
(bind-key "End Keyboard Macro" #k"control-x hyper-)")
(bind-key "Last Keyboard Macro" #k"control-x e")
(bind-key "Keyboard Macro Query" #k"control-x q")


;;;; Spell bindings.

(bind-key "Check Word Spelling" #k"meta-$")
(bind-key "Add Word to Spelling Dictionary" #k"control-x $")

(dolist (info (command-bindings (getstring "Self Insert" *command-names*)))
  (let* ((key (car info))
	 (key-event (svref key 0))
	 (character (key-event-char key-event)))
    (or (alpha-char-p character)
	(eq key-event #k"'"))
	(bind-key "Auto Check Word Spelling" key :mode "Spell")))
(bind-key "Auto Check Word Spelling" #k"return" :mode "Spell")
(bind-key "Auto Check Word Spelling" #k"tab" :mode "Spell")
(bind-key "Auto Check Word Spelling" #k"linefeed" :mode "Spell")
(bind-key "Correct Last Spelling Error" #k"meta-:")
(bind-key "Undo Last Spelling Correction" #k"control-x a")


;;;; Overwrite Mode.

(bind-key "Overwrite Delete Previous Character" #k"delete" :mode "Overwrite")
(bind-key "Overwrite Delete Previous Character" #k"backspace" :mode "Overwrite")

;;; Do up the printing characters ...
(do ((i 33 (1+ i)))
    ((= i 126))
  (let ((key-event (char-key-event (code-char i))))
    (bind-key "Self Overwrite" key-event :mode "Overwrite")))

(bind-key "Self Overwrite" #k"space" :mode "Overwrite")


;;;; Comment bindings.

(bind-key "Indent for Comment" #k"meta-;")
(bind-key "Set Comment Column" #k"control-x ;")
(bind-key "Kill Comment" #k"control-meta-;")
(bind-key "Down Comment Line" #k"meta-n")
(bind-key "Up Comment Line" #k"meta-p")
(bind-key "Indent New Comment Line" #k"meta-j")
(bind-key "Indent New Comment Line" #k"meta-linefeed")


;;;; Word Abbrev Mode.

(bind-key "Add Mode Word Abbrev" #k"control-x control-a")
(bind-key "Add Global Word Abbrev" #k"control-x +")
(bind-key "Inverse Add Mode Word Abbrev" #k"control-x control-h")
(bind-key "Inverse Add Global Word Abbrev" #k"control-x \-")
;; Used for "Pop and Goto Mark" instead.
;;(bind-key "Abbrev Expand Only" #k"meta-space")
(bind-key "Word Abbrev Prefix Mark" #k"meta-\"")
(bind-key "Unexpand Last Word" #k"control-x u")

(dolist (key (list #k"!" #k"~" #k"@" #k"#" #k";" #k"$" #k"%" #k"^" #k"&" #k"*"
		   #k"\-" #k"_" #k"=" #k"+" #k"[" #k"]" #k"(" #k")" #k"/" #k"|"
		   #k":" #k"'" #k"\"" #k"{" #k"}" #k"," #k"\<" #k"." #k"\>"
		   #k"`" #k"\\" #k"?" #k"return" #k"newline" #k"tab" #k"space"))
  (bind-key "Abbrev Expand Only" key :mode "Abbrev"))

(bind-key "Dabbrev Expand" #k"meta-/")


;;;; Inspect Mode.

(bind-key "Exit Recursive Edit" #k"q" :mode "Inspect")
(bind-key "Scroll Window Up" #k"delete" :mode "Inspect")
(bind-key "Scroll Window Down" #k"space" :mode "Inspect")
(bind-key "Enter Break Loop" #k"e" :mode "Inspect")
(bind-key "Restart from Error" #k"r" :mode "Inspect")
(bind-key "Restart from Error Self" #k"0" :mode "Inspect")
(bind-key "Restart from Error Self" #k"1" :mode "Inspect")
(bind-key "Restart from Error Self" #k"2" :mode "Inspect")
(bind-key "Restart from Error Self" #k"3" :mode "Inspect")
(bind-key "Restart from Error Self" #k"4" :mode "Inspect")
(bind-key "Restart from Error Self" #k"5" :mode "Inspect")
(bind-key "Restart from Error Self" #k"6" :mode "Inspect")
(bind-key "Restart from Error Self" #k"7" :mode "Inspect")
(bind-key "Restart from Error Self" #k"8" :mode "Inspect")
(bind-key "Restart from Error Self" #k"9" :mode "Inspect")
(bind-key "Next Line" #k"n" :mode "Inspect")
(bind-key "Previous Line" #k"p" :mode "Inspect")
(bind-key "Step Forward" #k"f" :mode "Inspect")
;(bind-key "Step Backward" #k"b" :mode "Inspect")
(bind-key "Edit Frame Source" #k"return" :mode "Inspect")
;(bind-key "Step Backward" #k"o" :mode "Inspect")
(bind-key "Forward Frame" #k"tab" :mode "Inspect")
(bind-key "Backward Frame" #k"meta-tab" :mode "Inspect")
(bind-key "Inspect Refresh" #k"g" :mode "Inspect")
(bind-key "Inspect Show Backtrace" #k"s b" :mode "Inspect")
(bind-key "Inspect Show Locals" #k"s l" :mode "Inspect")
(bind-key "Refresh Screen" #k"l" :mode "Inspect")


;;;; Scribe Mode.

(dolist (key (list #k"]" #k")" #k"}" #k"\>"))
  (bind-key "Scribe Insert Bracket" key :mode "Scribe"))

(bind-key "Scribe Buffer File" #k"control-x c" :mode "Scribe")
(bind-key "Select Scribe Warnings" #k"control-meta-C" :mode "Scribe")

(bind-key "Insert Scribe Directive" #k"hyper-i" :mode "Scribe")


;;;; X.

#|
#+clx (bind-key "Insert Cut Buffer" #k"insert")
#+clx (bind-key "Region to Cut Buffer" #k"meta-insert")
|#

;; FIX these could be a permanent calculator

(bind-key "Insert /"      #k"numpad/")
(bind-key "Insert *"      #k"numpad*")
(bind-key "Insert Minus"  #k"numpad\-")
(bind-key "Insert +"      #k"numpad+")
(bind-key "New Line"      #k"numpad\-return") ; FIX update in other modes
(bind-key "Continue"      #k"numlock")

(bind-key "Screenshot"    #k"printscreen")
(bind-key "Pause"         #k"pause")


;;;; Telnet.

(bind-key "Confirm Telnet Input" #k"return" :mode "Telnet")
(bind-key "Previous Interactive Input" #k"meta-p" :mode "Telnet")
(bind-key "Search Previous Interactive Input" #k"meta-P" :mode "Telnet")
(bind-key "Interactive Beginning of Line" #k"control-a" :mode "Telnet")
(bind-key "Kill Interactive Input" #k"meta-i" :mode "Telnet")
(bind-key "Next Interactive Input" #k"meta-n" :mode "Telnet")
(bind-key "Reenter Interactive Input" #k"control-return" :mode "Telnet")


;;;; IRC.


;;;; Gopher.

(bind-key "Next Gopher Reference" #k"n" :mode "Gopher")
(bind-key "Next Gopher Reference" #k"tab" :mode "Gopher")
(bind-key "Previous Gopher Reference" #k"p" :mode "Gopher")
(bind-key "Previous Gopher Reference" #k"meta-tab" :mode "Gopher")
(bind-key "Rotate Buffers Forward" #k"q" :mode "Gopher")
(bind-key "Gopher Resource from Point" #k"return" :mode "Gopher")
(bind-key "Refresh Gopher Buffer" #k"g" :mode "Gopher")
(bind-key "Forward Gopher Page" #k"f" :mode "Gopher")
(bind-key "Backward Gopher Page" #k"b" :mode "Gopher") ; FIX Scroll Win Up in View Mode.
(bind-key "Backward Gopher Page" #k"l" :mode "Gopher")


;;;; Mailer (MH) commands.

;;; Global.
;;;
(bind-key "Incorporate and Read New Mail" #k"control-x i")
;(bind-key "Browse Folders" #k"hyper-x m")?
(bind-key "Send Message" #k"control-x m")
;(bind-key "Message Headers" #k"control-x r")
(bind-key "Last Message Headers" #k"control-x r")

;;; Both Headers and Message modes.
;;;
;;; The bindings in these two blocks should be the same, one for "Message" mode
;;; and one for "Headers" mode.
;;;
(bind-key "Next Message" #k"meta-n" :mode "Message")
(bind-key "Previous Message" #k"meta-p" :mode "Message")
(bind-key "Next Undeleted Message" #k"n" :mode "Message")
(bind-key "Previous Undeleted Message" #k"p" :mode "Message")
(bind-key "Forward Message" #k"f" :mode "Message")
(bind-key "Headers Delete Message" #k"k" :mode "Message")
(bind-key "Headers Undelete Message" #k"u" :mode "Message")
(bind-key "Headers Refile Message" #k"r" :mode "Message")
(bind-key "Headers Mark as Read" #k"c" :mode "Message")
(bind-key "List Mail Buffers" #k"l" :mode "Message")
(bind-key "Incorporate and Read New Mail" #k"i" :mode "Message")
(bind-key "Beginning of Buffer" #k"\<" :mode "Message")
(bind-key "End of Buffer" #k"\>" :mode "Message")
(bind-key "Mark Message" #k"M" :mode "Message")
;;;
(bind-key "Next Message" #k"meta-n" :mode "Headers")
(bind-key "Previous Message" #k"meta-p" :mode "Headers")
(bind-key "Next Undeleted Message" #k"n" :mode "Headers")
(bind-key "Previous Undeleted Message" #k"p" :mode "Headers")
(bind-key "Forward Message" #k"f" :mode "Headers")
(bind-key "Headers Delete Message" #k"k" :mode "Headers")
(bind-key "Headers Undelete Message and Down Line" #k"u" :mode "Headers")
(bind-key "Headers Refile Message" #k"r" :mode "Headers")
(bind-key "Headers Mark as Read" #k"c" :mode "Headers")
(bind-key "List Mail Buffers" #k"l" :mode "Headers")
(bind-key "Incorporate and Read New Mail" #k"i" :mode "Headers")
(bind-key "Beginning of Buffer" #k"\<" :mode "Headers")
(bind-key "End of Buffer" #k"\>" :mode "Headers")
(bind-key "Mark Message" #k"M" :mode "Headers")

;;; Headers mode.
;;;
(bind-key "Headers Delete Message and Down Line" #k"d" :mode "Headers")
(bind-key "Pick Headers" #k"h" :mode "Headers")
(bind-key "Show Message" #k"." :mode "Headers")
(bind-key "Show Message" #k"return" :mode "Headers")
(bind-key "Show Message Next Window" #k"o" :mode "Headers")
(bind-key "Reply to Message" #k"a" :mode "Headers")
(bind-key "Reply to Message with Message" #k"A" :mode "Headers")
(bind-key "Expunge Messages" #k"!" :mode "Headers")
(bind-key "Expunge Messages" #k"x" :mode "Headers")
(bind-key "Headers Help" #k"?" :mode "Headers")
(bind-key "Refresh Headers" #k"g" :mode "Headers")
;(bind-key "Refresh All Headers" #k"g" :mode "Headers")
(bind-key "Sort Headers" #k"s" :mode "Headers")
(bind-key "Browse Folders" #k"^" :mode "Headers")
(bind-key "Scroll Window Down" #k"space" :mode "Headers")
(bind-key "Scroll Headers Window Up" #k"rubout" :mode "Headers")
(bind-key "Scroll Headers Window Up" #k"b" :mode "Headers")
(bind-key "Next Line" #k"tab" :mode "Headers")
(bind-key "Next Line" #k"n" :mode "Headers")
(bind-key "Previous Headers Line" #k"meta-tab" :mode "Headers")
(bind-key "Previous Headers Line" #k"p" :mode "Headers")
(bind-key "Previous Headers Line" #k"control-p" :mode "Headers")
(bind-key "Quit Headers" #k"q" :mode "Headers")

;;; Message mode.
;;;
(bind-key "Delete Message and Show Next" #k"d" :mode "Message")
(bind-key "Goto Headers Buffer" #k"^" :mode "Message")
(bind-key "Goto Headers Buffer" #k"q" :mode "Message")
(bind-key "Scroll Message" #k"space" :mode "Message")
(bind-key "Scroll Message" #k"control-v" :mode "Message")
(bind-key "Scroll Window Up" #k"backspace" :mode "Message")
(bind-key "Scroll Window Up" #k"delete" :mode "Message")
;(bind-key "Reply to Message in Other Window" #k"a" :mode "Message")
(bind-key "Reply to Message" #k"a" :mode "Message")
(bind-key "Reply to Message with Message" #k"A" :mode "Message")
(bind-key "Expunge Messages" #k"!" :mode "Message")
(bind-key "Expunge Messages" #k"x" :mode "Message")
(bind-key "Edit Message Buffer" #k"e" :mode "Message")
(bind-key "Insert Message Region" #k"hyper-y" :mode "Message")
(bind-key "Message Help" #k"?" :mode "Message")
(bind-key "Fill Paragraph Respect Comment" #k"meta-q" :mode "Message")
;; FIX Headers sounds like headers buffer (rename all fields?)
(bind-key "View MIME Part" #k"return" :mode "Message")
(bind-key "Next MIME Part" #k"tab" :mode "Message")
(bind-key "Previous MIME Part" #k"meta-tab" :mode "Message")
(bind-key "Save MIME Part" #k"s" :mode "Message")
(bind-key "Toggle Message Headers" #k"hyper-s h" :mode "Message")
(bind-key "Toggle Message Headers" #k"w" :mode "Message")
(bind-key "Check and Deliver Message" #k"hyper-s" :mode "Message")
(bind-key "Check and Deliver Message" #k"hyper-c" :mode "Message")

;;; Draft mode.
;;;
(bind-key "Goto Headers Buffer" #k"hyper-^" :mode "Draft")
(bind-key "Goto Message Buffer" #k"hyper-m" :mode "Draft")
;(bind-key "Goto Message Buffer" #k"m" :mode "Draft")
(bind-key "Check and Deliver Message" #k"hyper-s" :mode "Draft")
(bind-key "Check and Deliver Message" #k"hyper-c" :mode "Draft")
(bind-key "Insert Message Buffer" #k"hyper-y" :mode "Draft")
(bind-key "Delete Draft and Buffer" #k"hyper-q" :mode "Draft")
(bind-key "List Mail Buffers" #k"hyper-l" :mode "Draft")
(bind-key "Draft Help" #k"hyper-?" :mode "Draft")
(bind-key "Fill Paragraph Respect Comment" #k"meta-q" :mode "Draft")
(bind-key "Append File" #k"hyper-r" :mode "Draft")
(bind-key "Attach File" #k"hyper-a" :mode "Draft")
;(bind-key "Inline File" #k"hyper-i" :mode "Draft")
;(bind-key "Draft Insert Space" #k"space" :mode "Draft")
;(bind-key "Draft New Line" #k"return" :mode "Draft")
(bind-key "Draft Save File" #k"control-x control-s" :mode "Draft")

;;; Mail Browse.

(bind-key "Scroll Window Up" #k"delete" :mode "Mail Browse")
(bind-key "Scroll Window Down" #k"space" :mode "Mail Browse")
(bind-key "Rotate Buffers Forward" #k"q" :mode "Mail Browse")
(bind-key "Next Line" #k"n" :mode "Mail Browse")
(bind-key "Previous Line" #k"p" :mode "Mail Browse")
(bind-key "Mail Browse Browse Folder" #k"return" :mode "Mail Browse")
(bind-key "Mail Browse Browse Folder in Other Window" #k"o" :mode "Mail Browse")
(bind-key "Refresh Folders" #k"g" :mode "Mail Browse")
(bind-key "Mail Browse Toggle New" #k"N" :mode "Mail Browse")
(bind-key "Mail Browse Toggle New" #k"s n" :mode "Mail Browse")
(bind-key "Mail Browse Toggle New" #k"s a" :mode "Mail Browse")
(bind-key "Mail Browse Delete Folder and Down Line" #k"d" :mode "Mail Browse")
(bind-key "Mail Browse Delete Folder" #k"control-d" :mode "Mail Browse")
(bind-key "Mail Browse Mark Folder" #k"m" :mode "Mail Browse")
(bind-key "Mail Browse Clear Folder Marks and Down Line" #k"u" :mode "Mail Browse")
(bind-key "Mail Browse Rename Folder" #k"r" :mode "Mail Browse")
(bind-key "Expunge Folders" #k"x" :mode "Mail Browse")
(bind-key "Create Folder" #k"c" :mode "Mail Browse")

;;; Kill Browse.

(bind-key "Scroll Window Up" #k"delete" :mode "Kill Browse")
(bind-key "Scroll Window Down" #k"space" :mode "Kill Browse")
(bind-key "Rotate Buffers Forward" #k"q" :mode "Kill Browse")
(bind-key "Next Kill" #k"n" :mode "Kill Browse")
(bind-key "Previous Kill" #k"p" :mode "Kill Browse")
(bind-key "Refresh Kill Browse" #k"g" :mode "Kill Browse")
(bind-key "Kill Browse Insert Kill" #k"return" :mode "Kill Browse")


;;;; Netnews.

;; FIX standardise
;;; Clear everything user might hit to avoid getting the internal error
;;; message about modifying read-only buffers.
;;;
(do-alpha-key-events (key-event :both)
  (bind-key "Editor Error" key-event :mode "News Headers")
  (bind-key "Editor Error" key-event :mode "News Message"))

;;; Global Netnews bindings
;;;
(bind-key "Netnews Post Message" #k"C-x P")

;;; Both News Headers and News Message modes.
;;;
;;; The bindings in these two blocks should be the same, one for "News Message"
;;; mode and one for "News Headers" mode.
;;;
(bind-key "List All Groups" #k"l" :mode "News Headers")
(bind-key "Netnews Append to File" #k"i" :mode "News Headers") ;; FIX i for insert (was 'a')
(bind-key "Netnews Forward Message" #k"f" :mode "News Headers")
(bind-key "Netnews Go to Next Group" #k"g" :mode "News Headers") ;; FIX refresh
(bind-key "Netnews Next Article" #k"n" :mode "News Headers")
(bind-key "Netnews Previous Article" #k"p" :mode "News Headers")
(bind-key "Netnews Quit Starting Here" #k"." :mode "News Headers")
(bind-key "Netnews Group Punt Messages" #k"G" :mode "News Headers")
(bind-key "Netnews Show Whole Header" #k"w" :mode "News Headers")
(bind-key "Netnews Reply to Sender in Other Window" #k"a" :mode "News Headers")
(bind-key "Netnews Reply to Group in Other Window" #k"A" :mode "News Headers")
;;;
(bind-key "List All Groups" #k"l" :mode "News Message")
(bind-key "Netnews Append to File" #k"i" :mode "News Message")  ;; FIX i for insert (was 'a')
(bind-key "Netnews Forward Message" #k"f" :mode "News Message")
(bind-key "Netnews Go to Next Group" #k"g" :mode "News Message") ;; FIX refresh
(bind-key "Netnews Next Article" #k"n" :mode "News Message")
(bind-key "Netnews Previous Article" #k"p" :mode "News Message")
(bind-key "Netnews Quit Starting Here" #k"." :mode "News Message")
(bind-key "Netnews Group Punt Messages" #k"G" :mode "News Message")
(bind-key "Netnews Show Whole Header" #k"w" :mode "News Message")
(bind-key "Netnews Reply to Sender in Other Window" #k"a" :mode "News Message")
(bind-key "Netnews Reply to Group in Other Window" #k"A" :mode "News Message")

;;; News Headers.
;;;
(bind-key "Netnews Exit" #k"q" :mode "News Headers")
(bind-key "Netnews Headers File Message" #k"r" :mode "News Headers")
(bind-key "Netnews Headers Scroll Window Down" #k"c-v" :mode "News Headers")
(bind-key "Netnews Headers Scroll Window Up" #k"m-v" :mode "News Headers")
(bind-key "Netnews Headers Scroll Window Down" #k"space" :mode "News Headers")
(bind-key "Netnews Headers Scroll Window Up" #k"delete" :mode "News Headers")
(bind-key "Netnews Headers Scroll Window Up" #k"backspace" :mode "News Headers")
(bind-key "Netnews Next Line" #k"c-n" :mode "News Headers")
(bind-key "Netnews Next Line" #k"n" :mode "News Headers")
(bind-key "Netnews Next Line" #k"downarrow" :mode "News Headers")
(bind-key "Netnews Previous Line" #k"c-p" :mode "News Headers")
(bind-key "Netnews Previous Line" #k"p" :mode "News Headers")
(bind-key "Netnews Previous Line" #k"uparrow" :mode "News Headers")
(bind-key "Netnews Select Message Buffer" #k"hyper-m" :mode "News Headers")
(bind-key "Netnews Show Article" #k"return" :mode "News Headers")
(bind-key "Netnews Show Article in Other Window" #k"o" :mode "News Headers")

;;; News Message.
;;;
(bind-key "Insert Message Region" #k"Hyper-y" :mode "News Message")
(bind-key "Netnews Message File Message" #k"r" :mode "News Message")
(bind-key "Netnews Message Keep Buffer" #k"k" :mode "News Message")
(bind-key "Netnews Message Quit" #k"q" :mode "News Message")
(bind-key "Netnews Message Scroll Down"  #k"space" :mode "News Message")
(bind-key "Scroll Window Up" #k"delete" :mode "News Message")
(bind-key "Scroll Window Up" #k"backspace" :mode "News Message")
(bind-key "Netnews Goto Draft Buffer" #k"hyper-d" :mode "News Message")
(bind-key "Netnews Goto Headers Buffer" #k"^" :mode "News Message")
(bind-key "Netnews Goto Headers Buffer" #k"hyper-h" :mode "News Message")
(bind-key "Netnews Goto Post Buffer" #k"hyper-p" :mode "News Message")

;;; Post.
;;;
(bind-key "Netnews Select Message Buffer" #k"hyper-m" :mode "Post")
(bind-key "Netnews Deliver Post" #k"hyper-s" :mode "Post")
(bind-key "Netnews Abort Post" #k"hyper-q" :mode "Post")
(bind-key "Insert Message Buffer" #k"Hyper-y" :mode "Post")

;;; News Browse.

(bind-key "Netnews Quit Browse" #k"q" :mode "News Browse")
(bind-key "Netnews Browse Add Group To File" #k"a" :mode "News Browse")
(bind-key "Scroll Window Up" #k"delete" :mode "News Browse")
(bind-key "Scroll Window Up" #k"backspace" :mode "News Browse")
(bind-key "Scroll Window Down" #k"space" :mode "News Browse")
(bind-key "Netnews Browse Read Group" #k"enter" :mode "News Browse")
(bind-key "Next Line" #k"n" :mode "News Browse")
(bind-key "Previous Line" #k"p" :mode "News Browse")


;;;; Version Control.

(bind-key "VC Buffer File Log Entry" #k"control-x v l")
(bind-key "VC Commit Buffer File" #k"control-x v c")
(bind-key "VC Update Buffer File" #k"control-x v u")
(bind-key "VC Toggle Buffer File Lock" #k"control-x v k")  ; k for locK.
(bind-key "VC Compare Buffer File" #k"control-x v =")
;(bind-key "VC Annotate" #k"control-x v a")

;;;; Viewing version control logs (VC Log).

(bind-key "Scroll Window Up" #k"delete" :mode "VC Log")
(bind-key "Scroll Window Down" #k"space" :mode "VC Log")
(bind-key "Rotate Buffers Forward" #k"q" :mode "VC Log")

;;;; Entering version control logs (VC Log Entry).

(bind-key "Previous Parse" #k"meta-p" :mode "VC Log Entry")
(bind-key "Next Parse" #k"meta-n" :mode "VC Log Entry")

;;;; Viewing version control file comparisons (VC Comparison).

(bind-key "Scroll Window Up"       #k"delete" :mode "VC Comparison")
(bind-key "Scroll Window Down"     #k"space" :mode "VC Comparison")
(bind-key "Rotate Buffers Forward" #k"q" :mode "VC Comparison")
(bind-key "Next VC Comparison"     #k"n" :mode "VC Comparison")
(bind-key "Previous VC Comparison" #k"p" :mode "VC Comparison")
(bind-key "Next VC File"           #k"tab" :mode "VC Comparison")
(bind-key "Previous VC File"       #k"meta-tab" :mode "VC Comparison")
(bind-key "Edit VC Comparison"     #k"enter" :mode "VC Comparison")
(bind-key "Edit VC Comparison"     #k"e" :mode "VC Comparison")


;;;; Process (Shell).

(bind-key "Shell" #k"control-meta-s")
(bind-key "Confirm Process Input" #k"return" :mode "Process")
(bind-key "Shell Complete Filename" #k"M-escape" :mode "Process")
(bind-key "Interrupt Buffer Subprocess" #k"hyper-c" :mode "Process")
(bind-key "Stop Buffer Subprocess" #k"hyper-z" :mode "Process")
(bind-key "Quit Buffer Subprocess" #k"hyper-\\" :mode "Process")
(bind-key "Send EOF to Process" #k"hyper-d" :mode "Process")
;; Shadow global revert file.
(bind-key "Update Shell Buffer" #k"control-c meta-r" :mode "Process")

(bind-key "Previous Interactive Input" #k"meta-p" :mode "Process")
(bind-key "Search Previous Interactive Input" #k"meta-P" :mode "Process")
(bind-key "Interactive Beginning of Line" #k"control-a" :mode "Process")
(bind-key "Kill Interactive Input" #k"meta-i" :mode "Process")
(bind-key "Next Interactive Input" #k"meta-n" :mode "Process")
(bind-key "Reenter Interactive Input" #k"control-return" :mode "Process")


;;;; Compile.

(bind-key "Switch to Next Compile Reference" #k"control-x `")
(bind-key "Next Compile Reference" #k"tab" :mode "Compile")
(bind-key "Previous Compile Reference" #k"meta-tab" :mode "Compile")
(bind-key "Next Compile Reference" #k"n" :mode "Compile")
(bind-key "Previous Compile Reference" #k"p" :mode "Compile")
(bind-key "Follow Compile Reference" #k"return" :mode "Compile")
(bind-key "Follow Compile Reference in Other Window" #k"o" :mode "Compile")
(bind-key "Scroll Window Down" #k"space" :mode "Compile")
(bind-key "Scroll Window Up" #k"delete" :mode "Compile")
(bind-key "Update Compile Buffer" #k"g" :mode "Compile")


;;;; Objed.

(bind-key "Save Object" #k"control-x control-s" :mode "Objed")


;;;; DB.

; FIX then how to insert an s?
;(bind-key "Save DB" #k"s" :mode "DB")


;;;; Page.

(bind-key "Select Page" #k"enter" :mode "Page")
(bind-key "Scroll Window Up" #k"delete" :mode "Page")
(bind-key "Scroll Window Down" #k"space" :mode "Page")
(bind-key "Rotate Buffers Forward" #k"q" :mode "Page")
(bind-key "Next Line" #k"n" :mode "Page")
(bind-key "Previous Line" #k"p" :mode "Page")


;;;; Messages.

(bind-key "Rotate Buffers Forward" #k"q" :mode "Messages")
(bind-key "Scroll Window Up" #k"delete" :mode "Messages")
(bind-key "Scroll Window Down" #k"space" :mode "Messages")


;;;; Menu.

(bind-key "Select Menu Item" #k"enter" :mode "Menu")
(bind-key "Scroll Window Up" #k"delete" :mode "Menu")
(bind-key "Scroll Window Down" #k"space" :mode "Menu")
(bind-key "Exit Menu" #k"q" :mode "Menu")
(bind-key "Next Line" #k"n" :mode "Menu")
(bind-key "Previous Line" #k"p" :mode "Menu")
(bind-key "Next Line" #k"tab" :mode "Menu")
(bind-key "Previous Line" #k"meta-tab" :mode "Menu")


;;;; Record.

(bind-key "Refresh Record Buffer" #k"g" :mode "Record")
(bind-key "Scroll Window Up" #k"delete" :mode "Record")
(bind-key "Scroll Window Down" #k"space" :mode "Record")
(bind-key "Rotate Buffers Forward" #k"q" :mode "Record")
(bind-key "Create Contact" #k"c" :mode "Record")
(bind-key "Delete Contact" #k"d" :mode "Record")
(bind-key "Next Record" #k"n" :mode "Record")
(bind-key "Previous Record" #k"p" :mode "Record")
(bind-key "Next Record" #k"tab" :mode "Record")
(bind-key "Previous Record" #k"meta-tab" :mode "Record")
(bind-key "Edit Contact" #k"E" :mode "Record")
(bind-key "Edit Field" #k"e" :mode "Record")
(bind-key "Add Field" #k"a" :mode "Record")
(bind-key "Remove Field" #k"r" :mode "Record")
(bind-key "Mail Contact" #k"m" :mode "Record")
(bind-key "WWW Contact" #k"w" :mode "Record")
(bind-key "Save DB" #k"s" :mode "Record")


;;;; Diary entry.

;; FIX q should also close the window if the diary was poped up
(bind-key "Rotate Buffers Forward" #k"q" :mode "Diary Entry")
(bind-key "Scroll Window Up" #k"delete" :mode "Diary Entry")
(bind-key "Scroll Window Down" #k"space" :mode "Diary Entry")


;;;; Diary.

;(bind-key "Goto Date in Diary" #k"meta-g d" :mode "Diary")


;;;; Bufed.

;(bind-key "Bufed" #k"control-x control-meta-b")
(bind-key "Bufed" #k"control-x meta-b")
;(bind-key "Bufed Delete" #k"d" :mode "Bufed")
(bind-key "Bufed Delete and Next" #k"d" :mode "Bufed")
(bind-key "Bufed Delete" #k"control-d" :mode "Bufed")
(bind-key "Bufed Undelete" #k"u" :mode "Bufed")
(bind-key "Bufed Expunge" #k"!" :mode "Bufed")
(bind-key "Bufed Expunge" #k"x" :mode "Bufed")
(bind-key "Bufed Quit" #k"q" :mode "Bufed")
(bind-key "Bufed Goto" #k"return" :mode "Bufed")
(bind-key "Bufed Goto and Quit" #k"super-leftdown" :mode "Bufed")
(bind-key "Bufed Goto in Next Window" #k"o" :mode "Bufed")
(bind-key "Bufed Save File" #k"s" :mode "Bufed")
(bind-key "Bufed Refresh" #k"g" :mode "Bufed")
(bind-key "Next Line" #k"n" :mode "Bufed")
(bind-key "Previous Line" #k"p" :mode "Bufed")
(bind-key "Scroll Window Down" #k"space" :mode "Bufed")
(bind-key "Scroll Window Up" #k"delete" :mode "Bufed")

(bind-key "Bufed Help" #k"?" :mode "Bufed")


;;;; Packdired, Packed, Packman.

(bind-key "Packdired" #k"control-x p")
(bind-key "Packdired Quit" #k"q" :mode "Packdired")
(bind-key "Packdired Refresh" #k"g" :mode "Packdired")
(bind-key "Packdired Edit Package" #k"return" :mode "Packdired")
(bind-key "Packdired Edit Package" #k"v" :mode "Packdired")
(bind-key "Packdired Edit Package in Other Window" #k"o" :mode "Packdired")
(bind-key "Next Line" #k"n" :mode "Packdired")
(bind-key "Previous Line" #k"p" :mode "Packdired")
(bind-key "Scroll Window Down" #k"space" :mode "Packdired")
(bind-key "Scroll Window Up" #k"delete" :mode "Packdired")

(bind-key "Packdired Help" #k"?" :mode "Packdired")

(bind-key "Packdired" #k"^" :mode "Packed")
(bind-key "Packdired" #k"d" :mode "Packed")
(bind-key "Packed Quit" #k"q" :mode "Packed")
(bind-key "Packed Refresh" #k"g" :mode "Packed")
(bind-key "Next Line" #k"n" :mode "Packed")
(bind-key "Previous Line" #k"p" :mode "Packed")
(bind-key "Scroll Window Down" #k"space" :mode "Packed")
(bind-key "Scroll Window Up" #k"delete" :mode "Packed")

(bind-key "Packed Help" #k"?" :mode "Packed")

;(bind-key "Manage Packages" #k"control-x p")
(bind-key "Rotate Buffers Forward" #k"q" :mode "PackMan")
(bind-key "PackMan Refresh" #k"g" :mode "PackMan")
(bind-key "Describe Package" #k"return" :mode "PackMan")
(bind-key "Describe Package" #k"v" :mode "PackMan")
(bind-key "Describe Package in Other Window" #k"o" :mode "PackMan")
(bind-key "Editor Load Package" #k"l" :mode "PackMan")
(bind-key "Install Package" #k"i" :mode "PackMan")
(bind-key "Describe Package" #k"d" :mode "PackMan")
(bind-key "Update Meta Info" #k"u" :mode "PackMan")
(bind-key "Flush Package" #k"f" :mode "PackMan")
(bind-key "Commit Package" #k"c" :mode "PackMan")
(bind-key "Edit Package" #k"e" :mode "PackMan")
(bind-key "Test Package" #k"t" :mode "PackMan")

(bind-key "Next Line" #k"n" :mode "PackMan")
(bind-key "Previous Line" #k"p" :mode "PackMan")
(bind-key "Scroll Window Down" #k"space" :mode "PackMan")
(bind-key "Scroll Window Up" #k"delete" :mode "PackMan")

(bind-key "PackMan Help" #k"?" :mode "PackMan")


;;;; Evented.

(bind-key "Evented Quit" #k"q" :mode "Evented")
(bind-key "Evented Refresh" #k"g" :mode "Evented")
(bind-key "Evented Cancel Event" #k"D" :mode "Evented")
(bind-key "Next Line" #k"n" :mode "Evented")
(bind-key "Previous Line" #k"p" :mode "Evented")
(bind-key "Scroll Window Down" #k"space" :mode "Evented")
(bind-key "Scroll Window Up" #k"delete" :mode "Evented")

(bind-key "Evented Help" #k"?" :mode "Evented")


;;;; Dired.

(bind-key "Dired" #k"control-x control-meta-d")
(bind-key "Dired" #k"control-x d")
;; FIX dired of current dir w point on current file
(bind-key "Dired from Buffer Pathname" #k"control-x linefeed")
(bind-key "Dired from Buffer Pathname" #k"control-x control-j")

(bind-key "Scroll Window Down" #k"space"  :mode "Dired")
(bind-key "Scroll Window Up"   #k"delete" :mode "Dired")

(bind-key "Dired Delete File and Down Line" #k"d" :mode "Dired")
(bind-key "Dired Delete File with Pattern" #k"% d" :mode "Dired")
(bind-key "Dired Delete File" #k"control-d" :mode "Dired")
;(bind-key "Dired Delete File" #k"k" :mode "Dired")
(bind-key "Dired Delete and Expunge" #k"D" :mode "Dired")

(bind-key "Dired Undelete File and Down Line" #k"u" :mode "Dired")
(bind-key "Dired Undelete File with Pattern" #k"U" :mode "Dired")

(bind-key "Dired Mark File and Down Line" #k"m" :mode "Dired")
(bind-key "Dired Mark File with Pattern" #k"% m" :mode "Dired")
(bind-key "Dired Clear File Marks and Down Line" #k"u" :mode "Dired")
(bind-key "Dired Clear File Marks with Pattern" #k"% u" :mode "Dired")
(bind-key "Dired Toggle Marks" #k"t" :mode "Dired")

(bind-key "Dired Clear All Marks" #k"* !" :mode "Dired")

(bind-key "Dired Toggle Recurse" #k"s r" :mode "Dired")
(bind-key "Dired Toggle Backups" #k"s b" :mode "Dired")
(bind-key "Dired Toggle Hidden Files" #k"s h" :mode "Dired")
(bind-key "Dired Toggle All Files" #k"s a" :mode "Dired")
(bind-key "Dired Toggle Version Control" #k"s v" :mode "Dired")

(bind-key "Dired Expunge Files" #k"x" :mode "Dired")
(bind-key "Dired Update Buffer" #k"g" :mode "Dired")
(bind-key "Dired View File" #k"v" :mode "Dired")
(bind-key "Dired Edit File" #k"e" :mode "Dired")
(bind-key "Dired Edit File" #k"return" :mode "Dired")
(bind-key "Dired Edit File Next Window" #k"o" :mode "Dired")
(bind-key "Dired Up Directory" #k"^" :mode "Dired")
(bind-key "Rotate Buffers Forward" #k"q" :mode "Dired")
(bind-key "Dired Help" #k"?" :mode "Dired")

(bind-key "Dired Make Directory" #k"+" :mode "Dired")
(bind-key "Dired Copy File" #k"c" :mode "Dired")
(bind-key "Dired Copy with Wildcard" #k"% c" :mode "Dired")
(bind-key "Dired Rename File" #k"r" :mode "Dired")
(bind-key "Dired Rename with Wildcard" #k"% r" :mode "Dired")
(bind-key "Dired Symlink File" #k"l" :mode "Dired")
(bind-key "Dired Touch File" #k"T" :mode "Dired")
(bind-key "Dired Toggle File Compression" #k"Z" :mode "Dired")
(bind-key "Dired Compare Files" #k"=" :mode "Dired")
(bind-key "Dired Shell Command on File" #k"!" :mode "Dired")
;(bind-key "Dired Shell Command on File with Pattern" #k"% !" :mode "Dired")
(bind-key "Dired WWW File" #k"W" :mode "Dired")
(bind-key "Dired Change File Mode" #k"M" :mode "Dired")

(bind-key "Dired VC Update Directory" #k"meta-u" :mode "Dired")
(bind-key "Dired VC Update File" #k"hyper-u" :mode "Dired")
(bind-key "Dired VC Commit File" #k"hyper-c" :mode "Dired")
(bind-key "Dired VC Log Entry" #k"hyper-l" :mode "Dired")
(bind-key "Dired VC Compare File" #k"hyper-=" :mode "Dired")

(bind-key "Next Line"     #k"n" :mode "Dired")
(bind-key "Previous Line" #k"p" :mode "Dired")


;;;; View Mode.

(bind-key "View Scroll Down" #k"space" :mode "View")
(bind-key "Scroll Window Up" #k"b" :mode "View")
(bind-key "Scroll Window Up" #k"backspace" :mode "View")
(bind-key "Scroll Window Up" #k"delete" :mode "View")
(bind-key "View Return" #k"^" :mode "View")
(bind-key "View Quit" #k"q" :mode "View")
(bind-key "View Edit File" #k"e" :mode "View")
(bind-key "View Help" #k"?" :mode "View")
(bind-key "Beginning of Buffer" #k"\<" :mode "View")
(bind-key "End of Buffer" #k"\>" :mode "View")


;;;; Manual (Page) Mode.

(bind-key "Manual Page from Point" #k"return" :mode "Manual")
(bind-key "Next Manual Part" #k"n" :mode "Manual")
(bind-key "Previous Manual Part" #k"p" :mode "Manual")
(bind-key "Manual Page" #k"m" :mode "Manual")
(bind-key "Refresh Manual Page" #k"g" :mode "Manual")
(bind-key "Next Manual Reference" #k"tab" :mode "Manual")
(bind-key "Previous Manual Reference" #k"M-tab" :mode "Manual")


;;;; System Apropos Mode.

(bind-key "Manual Page from Point" #k"return" :mode "SysApropos")
(bind-key "Manual Page" #k"m" :mode "SysApropos")
(bind-key "Next Manual Reference" #k"tab" :mode "SysApropos")
(bind-key "Previous Manual Reference" #k"M-tab" :mode "SysApropos")
(bind-key "Next Line" #k"n" :mode "SysApropos")
(bind-key "Previous Line" #k"p" :mode "SysApropos")
;(bind-key "Manual Page in Other Window" #k"m" :mode "SysApropos")


;;;; System Process Listing mode.

(bind-key "Next Line" #k"n" :mode "SysProc")
(bind-key "Previous Line" #k"p" :mode "SysProc")
(bind-key "Refresh System Processes" #k"g" :mode "SysProc")
(bind-key "Set Current User" #k"u" :mode "SysProc")
(bind-key "Signal Process" #k"D" :mode "SysProc")
(bind-key "Signal Process" #k"k" :mode "SysProc")


;;;; Info Mode.

(bind-key "Info Directory" #k"d" :mode "Info")
(bind-key "Read Info Node" #k"i" :mode "Info")
;(bind-key "Next Info Node" #k"n" :mode "Info")
;(bind-key "Previous Info Node" #k"p" :mode "Info")
;(bind-key "Parent Info Node" #k"u" :mode "Info")
;(bind-key "Parent Info Node" #k"^" :mode "Info")
(bind-key "Top Info Node" #k"t" :mode "Info")
(bind-key "Info Node from Point" #k"return" :mode "Info")
(bind-key "Forward Info Node" #k"f" :mode "Info")
(bind-key "Backward Info Node" #k"b" :mode "Info") ; FIX Scroll Win Up in View Mode.
(bind-key "Backward Info Node" #k"l" :mode "Info")
(bind-key "Next Info Reference" #k"tab" :mode "Info")
(bind-key "Previous Info Reference" #k"meta-tab" :mode "Info")
(bind-key "Edit Node Source" #k"e" :mode "Info")
(bind-key "Refresh Info" #k"g" :mode "Info")


;;;; GNU Info Mode.

(bind-key "Next Ginfo Node" #k"n" :mode "Ginfo")
(bind-key "Previous Ginfo Node" #k"p" :mode "Ginfo")
(bind-key "Parent Ginfo Node" #k"u" :mode "Ginfo")
(bind-key "Parent Ginfo Node" #k"^" :mode "Ginfo")
(bind-key "Top Ginfo Node" #k"t" :mode "Ginfo")
(bind-key "Ginfo Node from Point" #k"return" :mode "Ginfo")
(bind-key "Ginfo Node from Menu" #k"m" :mode "Ginfo")
(bind-key "Forward Ginfo Node" #k"f" :mode "Ginfo")
(bind-key "Backward Ginfo Node" #k"b" :mode "Ginfo")
(bind-key "Backward Ginfo Node" #k"l" :mode "Ginfo")
(bind-key "Next Ginfo Reference" #k"tab" :mode "Ginfo")
(bind-key "Previous Ginfo Reference" #k"meta-tab" :mode "Ginfo")


;;;; Calendar and Diary.

(bind-key "Goto Today" #k"." :mode "Calendar")
(bind-key "Goto Date" #k"meta-g d" :mode "Calendar")
(bind-key "Refresh Calendar" #k"g" :mode "Calendar")
(bind-key "Calendar Show Year" #k"y" :mode "Calendar")
(bind-key "Calendar Show 3 Months" #k"3" :mode "Calendar")
(bind-key "Goto Current Date in Diary" #k"d" :mode "Calendar")
(bind-key "Show Diary Entries" #k"D" :mode "Calendar")
(bind-key "Insert Diary Entry" #k"i d" :mode "Calendar")
(bind-key "Rotate Buffers Forward" #k"q" :mode "Calendar")
(bind-key "Scroll Window Up" #k"backspace" :mode "Calendar")
(bind-key "Scroll Window Up" #k"delete" :mode "Calendar")
(bind-key "Scroll Window Down" #k"space" :mode "Calendar")


;;;; WWW.

(bind-key "WWW Resource from Point" #k"return" :mode "WWW")
(bind-key "WWW Resource from Point in New Buffer" #k"meta-return" :mode "WWW")
(bind-key "Forward WWW Page" #k"f" :mode "WWW")
(bind-key "Backward WWW Page" #k"b" :mode "WWW") ; FIX Scroll Win Up in View Mode.
(bind-key "Backward WWW Page" #k"l" :mode "WWW")
(bind-key "Next WWW Reference" #k"tab" :mode "WWW")
(bind-key "WWW Refresh" #k"g" :mode "WWW")
(bind-key "WWW Home" #k"h" :mode "WWW")
(bind-key "Save WWW URL" #k"c" :mode "WWW")
(bind-key "WWW URL at Point" #k"u" :mode "WWW")
(bind-key "Next Line" #k"n" :mode "WWW")
(bind-key "Previous Line" #k"p" :mode "WWW")
(bind-key "WWW in Current Buffer" #k"w" :mode "WWW")
(bind-key "Copy WWW Buffer" #k"control-x n" :mode "WWW")
(bind-key "WWW Page Info" #k"=" :mode "WWW")
(bind-key "WWW Toggle Source" #k"s s" :mode "WWW")


;;;; Links.

(bind-key "Rotate Buffers Forward"  #k"q"         :mode "Links")
(bind-key "Scroll Window Up"        #k"backspace" :mode "Links")
(bind-key "Scroll Window Up"        #k"delete"    :mode "Links")
(bind-key "Scroll Window Down"      #k"space"     :mode "Links")
(bind-key "Links WWW"               #k"w"         :mode "Links")
(bind-key "Links WWW Externally"    #k"W"         :mode "Links")
(bind-key "Links Edit"              #k"e"         :mode "Links")
(bind-key "Links Find"              #k"enter"     :mode "Links")
(bind-key "Links Next Link"         #k"tab"       :mode "Links")
(bind-key "Links Next Link"         #k"n"         :mode "Links")
(bind-key "Links Previous Link"     #k"meta-tab"  :mode "Links")
(bind-key "Links Previous Link"     #k"p"         :mode "Links")


;;;; Completion mode.

(dolist (c (command-bindings (getstring "Self Insert" *command-names*)))
  (bind-key "Completion Self Insert" (car c) :mode "Completion"))

(bind-key "Completion Self Insert" #k"space" :mode "Completion")
(bind-key "Completion Self Insert" #k"tab" :mode "Completion")
(bind-key "Completion Self Insert" #k"return" :mode "Completion")
(bind-key "Completion Self Insert" #k"linefeed" :mode "Completion")

#|
(bind-key "Completion Complete Word" #k"end")
(bind-key "Completion Rotate Completions" #k"meta-end")
|#


;;;; Caps Lock mode.

(do-alpha-key-events (key-event :lower)
  (bind-key "Self Insert Caps Lock" key-event :mode "CAPS LOCK"))


;;;; Enriched mode.

(bind-key "Do Nothing" #k"control-x control-s" :mode "Enriched")


;;;; SGML mode.

(bind-key "SGML Insert >" #k"\>" :mode "SGML")


;;;; Hex mode.

(do-alpha-key-events (key-event :both)
  (bind-key "Self Insert Hex" key-event :mode "Hex"))

(bind-key "Hex g or Refresh" #k"g" :mode "Hex")
(bind-key "Hex Space or Scroll Window Down" #k"space" :mode "Hex")
(bind-key "Scroll Window Up" #k"backspace" :mode "Hex")
(bind-key "Scroll Window Up" #k"delete" :mode "Hex")
(bind-key "Hex Next Item" #k"tab" :mode "Hex")
;(bind-key "Hex Previous Item" #k"meta-tab" :mode "Hex")
(bind-key "Hex q or Quit" #k"q" :mode "Hex")

(bind-key "Editor Error" #k"control-q" :mode "Hex")

(bind-key "Self Insert Hex" #k"!" :mode "Hex")
(bind-key "Self Insert Hex" #k"@" :mode "Hex")
(bind-key "Self Insert Hex" #k"#" :mode "Hex")
(bind-key "Self Insert Hex" #k"$" :mode "Hex")
(bind-key "Self Insert Hex" #k"%" :mode "Hex")
(bind-key "Self Insert Hex" #k"^" :mode "Hex")
(bind-key "Self Insert Hex" #k"&" :mode "Hex")
(bind-key "Self Insert Hex" #k"*" :mode "Hex")
(bind-key "Self Insert Hex" #k"(" :mode "Hex")
(bind-key "Self Insert Hex" #k")" :mode "Hex")
(bind-key "Self Insert Hex" #k"_" :mode "Hex")
(bind-key "Self Insert Hex" #k"+" :mode "Hex")
(bind-key "Self Insert Hex" #k"~" :mode "Hex")
(bind-key "Self Insert Hex" #k"1" :mode "Hex")
(bind-key "Self Insert Hex" #k"2" :mode "Hex")
(bind-key "Self Insert Hex" #k"3" :mode "Hex")
(bind-key "Self Insert Hex" #k"4" :mode "Hex")
(bind-key "Self Insert Hex" #k"5" :mode "Hex")
(bind-key "Self Insert Hex" #k"6" :mode "Hex")
(bind-key "Self Insert Hex" #k"7" :mode "Hex")
(bind-key "Self Insert Hex" #k"8" :mode "Hex")
(bind-key "Self Insert Hex" #k"9" :mode "Hex")
(bind-key "Self Insert Hex" #k"0" :mode "Hex")
(bind-key "Self Insert Hex" #k"[" :mode "Hex")
(bind-key "Self Insert Hex" #k"]" :mode "Hex")
(bind-key "Self Insert Hex" #k"\\" :mode "Hex")
(bind-key "Self Insert Hex" #k"|" :mode "Hex")
(bind-key "Self Insert Hex" #k":" :mode "Hex")
(bind-key "Self Insert Hex" #k";" :mode "Hex")
(bind-key "Self Insert Hex" #k"\"" :mode "Hex")
(bind-key "Self Insert Hex" #k"'" :mode "Hex")
(bind-key "Self Insert Hex" #k"\-" :mode "Hex")
(bind-key "Self Insert Hex" #k"=" :mode "Hex")
(bind-key "Self Insert Hex" #k"`" :mode "Hex")
(bind-key "Self Insert Hex" #k"\<" :mode "Hex")
(bind-key "Self Insert Hex" #k"\>" :mode "Hex")
(bind-key "Self Insert Hex" #k"," :mode "Hex")
(bind-key "Self Insert Hex" #k"." :mode "Hex")
(bind-key "Self Insert Hex" #k"?" :mode "Hex")
(bind-key "Self Insert Hex" #k"/" :mode "Hex")
(bind-key "Self Insert Hex" #k"{" :mode "Hex")
(bind-key "Self Insert Hex" #k"}" :mode "Hex")


;;;; Logical characters.

#[ System Defined Logical Key-Events

There are many default logical key-events, some of which are used by functions
documented in this manual.  If a command wants to read a single key-event
command that fits one of these descriptions then the key-event read should be
compared to the corresponding logical key-event instead of explicitly
mentioning the particular key-event in the code.  In many cases you can use the
macrefcommand-case macro.  It makes logical key-events easy to use and takes
care of prompting and displaying help messages.

  :yes
      Indicates the prompter should take the action under consideration.

  :no
      Indicates the prompter should NOT take the action under consideration.

  :do-all
      Indicates the prompter should repeat the action under consideration as many
     times as possible.

  :do-once
      Indicates the prompter should execute the action under consideration once and
     then exit.

  :exit
      Indicates the prompter should terminate its activity in a normal fashion.

  :abort
      Indicates the prompter should terminate its activity without performing any
     closing actions of convenience, for example.

  :keep
      Indicates the prompter should preserve something.

  :help
      Indicates the prompter should display some help information.

  :confirm
      Indicates the prompter should take any input provided or use the default if
     the user entered nothing.

  :quote
      Indicates the prompter should take the following key-event as itself without
     any sort of command interpretation.

  :recursive-edit
      Indicates the prompter should enter a recursive edit in the current context.

  :cancel
      Indicates the prompter should cancel the effect of a previous key-event input.

  :forward-search
      Indicates the prompter should search forward in the current context.

  :backward-search
      Indicates the prompter should search backward in the current context.

Define a new logical key-event whenever:

  1) The key-event concerned represents a general class of actions, and
     several commands may want to take a similar action of this type.

  2) The exact key-event a command implementor chooses may generate violent taste
     disputes among users, and then the users can trivially change the command in
     their init files.

  3) You are using command-case which prevents implementors from specifying
     non-standard characters for dispatching in otherwise possibly portable code,
     and you can define and set the logical key-event in a site dependent file where
     you can mention implementation dependent characters.
]#

(setf (logical-key-event-p #k"control-s" :forward-search) t)
(setf (logical-key-event-p #k"control-r" :backward-search) t)
(setf (logical-key-event-p #k"control-r" :recursive-edit) t)
(setf (logical-key-event-p #k"delete" :cancel) t)
(setf (logical-key-event-p #k"backspace" :cancel) t)
(setf (logical-key-event-p #k"control-g" :abort) t)
(setf (logical-key-event-p #k"escape" :exit) t)
(setf (logical-key-event-p #k"y" :yes) t)
(setf (logical-key-event-p #k"space" :yes) t)
(setf (logical-key-event-p #k"n" :no) t)
(setf (logical-key-event-p #k"backspace" :no) t)
(setf (logical-key-event-p #k"delete" :no) t)
(setf (logical-key-event-p #k"!" :do-all) t)
(setf (logical-key-event-p #k"." :do-once) t)
(setf (logical-key-event-p #k"home" :help) t)
(setf (logical-key-event-p #k"h" :help) t)
(setf (logical-key-event-p #k"?" :help) t)
(setf (logical-key-event-p #k"control-_" :help) t)
(setf (logical-key-event-p #k"return" :confirm) t)
(setf (logical-key-event-p #k"control-q" :quote) t)
(setf (logical-key-event-p #k"k" :keep) t)
(setf (logical-key-event-p #k"s" :switch-to-reference) t)


;;; Help key.

(defun frob-help-key ()
  "Setup a control-h to `Help' binding for the standard keyboard."
  (if (windowed-monitor-p)
      (progn
	(bind-key "Help" #k"control-h")
	;; FIX as below
	(bind-key "Help" #k"control-h" :mode "Echo Area"))
      (progn
	;;; FIX Separate control-h and Backspace.
	(in-package "EDI")
	(setf (ext:char-key-event (code-char 8)) (ext::make-key-event #k"F35"))
	(in-package "ED")

	(bind-key "Help" #k"F35")

	;; FIX alternative binding?
	;;;(bind-key "Help on Parse" #k"F35" #| control-h |# :mode "Echo Area")
	;;; Rather, to allow help on echo area:
	(bind-key "Help" #k"F35" #| control-h |# :mode "Echo Area"))))


;;;; Hyper.

(defun frob-hyper-key ()
  "Setup the OS key (on a standard keyboard) to set the hyper bit on the
   following key.

   This overrides meta-[, which is normally bound to `Backward Paragraph'."
  ;; FIX Almost Hyper.
  ;;     Maybe hack something to allow one-shot keys to modify, so then F13
  ;;     bound in linux/x11 to the OS key could be Hyper and the repeat of
  ;;     holding down the key would be absorbed by the editor.
  (setf (key-translation #k"meta-[ 2 5 ~") '(:bits :hyper)))


;;; Arrow, page and function keys.
;;;
;;; These keys send "escape [ <char>*" (i.e. "meta-[ <char>*").

#|
(defun print-input (&optional (fd 0))
  (alien:with-alien ((buf (alien:array c-call:unsigned-char 256)))
    (multiple-value-bind
	(len errno)
	(unix:unix-read fd (alien:alien-sap buf) 256)
      (declare (type (or null fixnum) len))
      (or len
	  (error "Problem with tty input: ~S"
		 (unix:get-unix-error-msg errno)))
      (dotimes (i len t)
	(message "~A code: ~A ch: ~A, ev: ~A"
		 i
		 (alien:deref buf i)
		 (code-char (alien:deref buf i))
		 (ext:char-key-event (code-char (alien:deref buf i))))))))
|#

(defun frob-[-keys ()
  "Bind the arrow, page and function keys for the standard keyboard.

   This overrides meta-[, which is normally bound to `Backward Paragraph'."

  (delete-key-binding #k"meta-[")

  ;;; Function keys.
  ;;;

  ;; Aterm (X11).
  (bind-key "Continue" #k"meta-[ 1 1 ~")       ; F1
  (bind-key "Continue" #k"meta-[ 1 2 ~")       ; ...
  (bind-key "Continue" #k"meta-[ 1 3 ~")
  (bind-key "Continue" #k"meta-[ 1 4 ~")
  (bind-key "Continue" #k"meta-[ 1 5 ~")
  (bind-key "Continue" #k"meta-[ 1 6 ~")       ;; FIX check to F12
  (bind-key "Continue" #k"meta-[ 1 7 ~")
  (bind-key "Continue" #k"meta-[ 1 8 ~")
  (bind-key "Continue" #k"meta-[ 1 9 ~")
  (bind-key "Continue" #k"meta-[ 2 1 ~")
  (bind-key "Continue" #k"meta-[ 2 3 ~")
  (bind-key "Continue" #k"meta-[ 2 4 ~")       ; F12

  ;; Linux term.
  (bind-key "Continue" #k"meta-[ [ [ A ~")     ; F1
  (bind-key "Continue" #k"meta-[ [ [ B ~")     ; ...
  (bind-key "Continue" #k"meta-[ [ [ C ~")
  (bind-key "Continue" #k"meta-[ [ [ D ~")
  (bind-key "Continue" #k"meta-[ [ [ E ~")
  (bind-key "Continue" #k"meta-[ [ 1 7 ~")
  (bind-key "Continue" #k"meta-[ [ 1 8 ~")
  (bind-key "Continue" #k"meta-[ [ 1 9 ~")
  (bind-key "Continue" #k"meta-[ 2 0 ~")
  (bind-key "Continue" #k"meta-[ 2 1 ~")
  (bind-key "Continue" #k"meta-[ 2 3 ~")
  (bind-key "Continue" #k"meta-[ 2 4 ~")       ; F12

  ;;; Function keys for the Sun (and other keyboards) -- L1-L10 and R1-R15.
  ;;;
  ;;; ...

  ;;; Upper right key bank.
  ;;;
  ;;; FIX Maybe being handled before editor?
  ;;; ; printscreen/sysrq
  ;;; ; type scroll lock.
  ;;; ; pause

  ;;; Middle right key bank.
  ;;;
  (bind-key "Overwrite Mode"        #k"meta-[ 2 ~") ; insert
  (bind-key "Delete Next Character" #k"meta-[ 3 ~") ; rubout
  (bind-key "Beginning of Buffer"   #k"meta-[ 7 ~") ; home     Aterm (x11)
  (bind-key "Beginning of Buffer"   #k"meta-[ 1 ~") ; home     Linux term
  (bind-key "End of Buffer"         #k"meta-[ 8 ~") ; end      Aterm (x11)
  (bind-key "End of Buffer"         #k"meta-[ 4 ~") ; end      Linux term
  (bind-key "Scroll Window Up"      #k"meta-[ 5 ~") ; page up
  (bind-key "Scroll Window Down"    #k"meta-[ 6 ~") ; page down

  ;;; Arrows.
  ;;;
  (bind-key "Previous Line"      #k"meta-[ A") ; up arrow
  (bind-key "Next Line"          #k"meta-[ B") ; down arrow
  (bind-key "Forward Character"  #k"meta-[ C") ; right arrow
  (bind-key "Backward Character" #k"meta-[ D") ; left arrow

  (defcommand "Insert Keypad Operation" ()
    "Insert character for keypad operation (\"/\", \"+\", etc.)."
    (let ((char (ext:key-event-char *last-key-event-typed*)))
      (or char (editor-error "Failed to translate key-event to character."))
      (insert-character (current-point) (code-char (- (char-code char) 64)))))

  ;;; Number pad.
  ;;;
  ;;; ; numlock
  (bind-key "Continue"                #k"meta-O M") ; enter
  (bind-key "Insert Keypad Operation" #k"meta-O o") ; /
  (bind-key "Insert Keypad Operation" #k"meta-O j") ; *
  (bind-key "Insert Keypad Operation" #k"meta-O m") ; -
  (bind-key "Insert Keypad Operation" #k"meta-O k") ; +
  (bind-key "Overwrite Mode"          #k"meta-O p") ; 0  ins
  (bind-key "End of Buffer"           #k"meta-O q") ; 1  end
  (bind-key "Next Line"               #k"meta-O r") ; 2  down arrow
  (bind-key "Scroll Window Down"      #k"meta-O s") ; 3  page down
  (bind-key "Backward Character"      #k"meta-O t") ; 4  left arrow
  (bind-key "Continue"                #k"meta-O u") ; 5
  (bind-key "Forward Character"       #k"meta-O v") ; 6  right arrow
  (bind-key "Beginning of Buffer"     #k"meta-O w") ; 7
  (bind-key "Previous Line"           #k"meta-O x") ; 8
  (bind-key "Scroll Window Up"        #k"meta-O y") ; 9
  (bind-key "Delete Next Character "  #k"meta-O n")) ; .  del


;; Initialization of these keys depends on the whether the editor is
;; running in a terminal or under X, so it must be done after the editor
;; has started.
(after-editor-initializations
 (frob-help-key)
 (or (windowed-monitor-p) (frob-[-keys)))
