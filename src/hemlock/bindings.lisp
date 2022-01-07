;;; -*- Log: hemlock.log; Package: Hemlock -*-
;;;
;;; Key bindings.

(in-package "HEMLOCK")



;;;; Default key translations:

;;; This page defines prefix characters that set specified modifier bits on
;;; the next character typed.
;;;
(setf (key-translation #k"escape") '(:bits :meta))
(setf (key-translation #k"control-z") '(:bits :control :meta))
(setf (key-translation #k"control-Z") '(:bits :control :meta))
(setf (key-translation #k"control-^") '(:bits :control))
(setf (key-translation #k"control-c") '(:bits :hyper))
(setf (key-translation #k"control-C") '(:bits :hyper))



;;;; Most every binding.

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

(bind-key "Help" #k"home")
(bind-key "Help" #k"control-_")
(bind-key "Describe Key" #k"meta-?")


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


(bind-key "Insert File" #k"control-x control-r")
(bind-key "Save File" #k"control-x control-s")
(bind-key "Visit File" #k"control-x control-v")
(bind-key "Write File" #k"control-x control-w")
(bind-key "Find File" #k"control-x control-f")
(bind-key "Backup File" #k"control-x meta-b")
(bind-key "Save All Files" #k"control-x control-m")
(bind-key "Save All Files" #k"control-x return")
(bind-key "Save All Files and Exit" #k"control-x meta-z")

(bind-key "List Buffers" #k"control-x control-b")
(bind-key "Buffer Not Modified" #k"meta-~")
(bind-key "Check Buffer Modified" #k"control-x ~")
(bind-key "Select Buffer" #k"control-x b")
(bind-key "Select Previous Buffer" #k"control-meta-l")
(bind-key "Circulate Buffers" #k"control-meta-L")
(bind-key "Create Buffer" #k"control-x meta-b")
(bind-key "Copy Buffer" #k"control-x n")
(bind-key "Copy Buffer Next Window" #k"control-x 4 n")
(bind-key "Switch to Next Copy" #k"control-x control-n")
(bind-key "Kill Buffer" #k"control-x k")
(bind-key "Select Random Typeout Buffer" #k"hyper-t")

(bind-key "Next Window" #k"control-x o")
(bind-key "Previous Window" #k"control-x p")
(bind-key "Split Window" #k"control-x 2")
;(bind-key "New Window" #k"control-x control-n")
(bind-key "Delete Window" #k"control-x d")
(bind-key "Go to one window" #k"control-x 1")
;(bind-key "Delete Next Window" #k"control-x 1")
(bind-key "Delete Window" #k"control-x 0")
(bind-key "Line to Top of Window" #k"meta-!")
(bind-key "Line to Center of Window" #k"meta-#")
(bind-key "Top of Window" #k"meta-,")
(bind-key "Bottom of Window" #k"meta-.")

(bind-key "Exit Hemlock" #k"control-x control-z")
(bind-key "Exit Recursive Edit" #k"control-meta-z")
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
(bind-key "Un-Kill" #k"control-y")
(bind-key "Rotate Kill Ring" #k"meta-y")

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
(bind-key "End of Buffer" #k"meta-\>")
(bind-key "Mark to Beginning of Buffer" #k"control-\<")
(bind-key "Mark to End of Buffer" #k"control-\>")

(bind-key "Extended Command" #k"meta-x")

(bind-key "Uppercase Word" #k"meta-u")
(bind-key "Lowercase Word" #k"meta-l")
(bind-key "Capitalize Word" #k"meta-c")

(bind-key "Previous Page" #k"control-x [")
(bind-key "Next Page" #k"control-x ]")
(bind-key "Mark Page" #k"control-x control-p")
(bind-key "Count Lines Page" #k"control-x l")
(bind-key "Count Lines and Characters" #k"meta-=")
(bind-key "Describe Cursor Position" #k"control-x =")



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
(bind-key "Self Insert" #k"[")
(bind-key "Self Insert" #k"]")
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



;;;; Echo Area.

;;; Basic echo-area commands.
;;;
(bind-key "Help on Parse" #k"home" :mode "Echo Area")
(bind-key "Help on Parse" #k"control-_" :mode "Echo Area")

(bind-key "Complete Keyword" #k"escape" :mode "Echo Area")
(bind-key "Complete Field" #k"space" :mode "Echo Area")
(bind-key "Confirm Parse" #k"return" :mode "Echo Area")

;;; Rebind some standard commands to behave better.
;;;
(bind-key "Kill Parse" #k"control-u" :mode "Echo Area")
(bind-key "Insert Parse Default" #k"control-i" :mode "Echo Area")
(bind-key "Insert Parse Default" #k"tab" :mode "Echo Area")
(bind-key "Echo Area Delete Previous Character" #k"delete" :mode "Echo Area")
(bind-key "Echo Area Delete Previous Character" #k"backspace" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"meta-h" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"meta-delete" :mode "Echo Area")
(bind-key "Echo Area Kill Previous Word" #k"meta-backspace" :mode "Echo Area")
;;; FIX Then how to kill the region?
(bind-key "Echo Area Kill Previous Word" #k"control-w" :mode "Echo Area")
(bind-key "Beginning of Parse" #k"control-a" :mode "Echo Area")
(bind-key "Beginning of Parse" #k"meta-\<" :mode "Echo Area")
(bind-key "Echo Area Backward Character" #k"control-b" :mode "Echo Area")
(bind-key "Echo Area Backward Word" #k"meta-b" :mode "Echo Area")
(bind-key "Next Parse" #k"control-n" :mode "Echo Area")
(bind-key "Previous Parse" #k"control-p" :mode "Echo Area")

;;; Remove some dangerous standard bindings.
;;;
(bind-key "Illegal" #k"control-x" :mode "Echo Area")
(bind-key "Illegal" #k"control-meta-c" :mode "Echo Area")
(bind-key "Illegal" #k"control-meta-s" :mode "Echo Area")
(bind-key "Illegal" #k"control-meta-l" :mode "Echo Area")
(bind-key "Illegal" #k"meta-x" :mode "Echo Area")
(bind-key "Illegal" #k"control-s" :mode "Echo Area")
(bind-key "Illegal" #k"control-r" :mode "Echo Area")
(bind-key "Illegal" #k"hyper-t" :mode "Echo Area")
(bind-key "Illegal" #k"middledown" :mode "Echo Area")
(bind-key "Do Nothing" #k"middleup" :mode "Echo Area")
(bind-key "Illegal" #k"super-leftdown" :mode "Echo Area")
(bind-key "Do Nothing" #k"super-leftup" :mode "Echo Area")
(bind-key "Illegal" #k"super-rightdown" :mode "Echo Area")
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

(bind-key "Debug Up" #k"control-meta-hyper-u")
(bind-key "Debug Down" #k"control-meta-hyper-d")
(bind-key "Debug Top" #k"control-meta-hyper-t")
(bind-key "Debug Bottom" #k"control-meta-hyper-b")
(bind-key "Debug Frame" #k"control-meta-hyper-f")
(bind-key "Debug Quit" #k"control-meta-hyper-q")
(bind-key "Debug Go" #k"control-meta-hyper-g")
(bind-key "Debug Abort" #k"control-meta-hyper-a")
(bind-key "Debug Restart" #k"control-meta-hyper-r")
(bind-key "Debug Help" #k"control-meta-hyper-h")
(bind-key "Debug Error" #k"control-meta-hyper-e")
(bind-key "Debug Backtrace" #k"control-meta-hyper-B")
(bind-key "Debug Print" #k"control-meta-hyper-p")
(bind-key "Debug Verbose Print" #k"control-meta-hyper-P")
(bind-key "Debug List Locals" #k"control-meta-hyper-l")
(bind-key "Debug Source" #k"control-meta-hyper-s")
(bind-key "Debug Edit Source" #k"control-meta-hyper-S")
(bind-key "Debug Flush Errors" #k"control-meta-hyper-F")



;;;; More Miscellaneous bindings.

(bind-key "Open Line" #k"Control-o")
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
(bind-key "Fill Region" #k"meta-g")
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
    (unless (or (alpha-char-p character) (eq key-event #k"'"))
      (bind-key "Auto Check Word Spelling" key :mode "Spell"))))
(bind-key "Auto Check Word Spelling" #k"return" :mode "Spell")
(bind-key "Auto Check Word Spelling" #k"tab" :mode "Spell")
(bind-key "Auto Check Word Spelling" #k"linefeed" :mode "Spell")
(bind-key "Correct Last Misspelled Word" #k"meta-:")
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
;; Removed in lieu of "Pop and Goto Mark".
;;(bind-key "Abbrev Expand Only" #k"meta-space")
(bind-key "Word Abbrev Prefix Mark" #k"meta-\"")
(bind-key "Unexpand Last Word" #k"control-x u")

(dolist (key (list #k"!" #k"~" #k"@" #k"#" #k";" #k"$" #k"%" #k"^" #k"&" #k"*"
		   #k"\-" #k"_" #k"=" #k"+" #k"[" #k"]" #k"(" #k")" #k"/" #k"|"
		   #k":" #k"'" #k"\"" #k"{" #k"}" #k"," #k"\<" #k"." #k"\>"
		   #k"`" #k"\\" #k"?" #k"return" #k"newline" #k"tab" #k"space"))
  (bind-key "Abbrev Expand Only" key :mode "Abbrev"))

(bind-key "Dabbrev Expand" #k"meta-/")



;;;; Scribe Mode.

(dolist (key (list #k"]" #k")" #k"}" #k"\>"))
  (bind-key "Scribe Insert Bracket" key :mode "Scribe"))

(bind-key "Scribe Buffer File" #k"control-x c" :mode "Scribe")
(bind-key "Select Scribe Warnings" #k"control-meta-C" :mode "Scribe")

(bind-key "Insert Scribe Directive" #k"hyper-i" :mode "Scribe")



;;;; X commands:

#+clx (bind-key "Insert Cut Buffer" #k"insert")
#+clx (bind-key "Region to Cut Buffer" #k"meta-insert")



;;;; Mailer commands.

;;; Clear everything user might hit to avoid getting the internal error
;;; message about modifying read-only buffers.
;;;
(do-alpha-key-events (key-event :both)
  (bind-key "Illegal" key-event :mode "Headers")
  (bind-key "Illegal" key-event :mode "Message"))


;;; Global.
;;;
(bind-key "Incorporate and Read New Mail" #k"control-x i")
(bind-key "Send Message" #k"control-x m")
(bind-key "Message Headers" #k"control-x r")


;;; Both Headers and Message modes.
;;;
;;; The bindings in these two blocks should be the same, one for "Message" mode
;;; and one for "Headers" mode.
;;;
(bind-key "Next Message" #k"meta-n" :mode "Message")
(bind-key "Previous Message" #k"meta-p" :mode "Message")
(bind-key "Next Undeleted Message" #k"n" :mode "Message")
(bind-key "Previous Undeleted Message" #k"p" :mode "Message")
(bind-key "Send Message" #k"s" :mode "Message")
(bind-key "Send Message" #k"m" :mode "Message")
(bind-key "Forward Message" #k"f" :mode "Message")
(bind-key "Headers Delete Message" #k"k" :mode "Message")
(bind-key "Headers Undelete Message" #k"u" :mode "Message")
(bind-key "Headers Refile Message" #k"o" :mode "Message")
(bind-key "List Mail Buffers" #k"l" :mode "Message")
(bind-key "Quit Headers" #k"q" :mode "Message")
(bind-key "Incorporate and Read New Mail" #k"i" :mode "Message")
(bind-key "Beginning of Buffer" #k"\<" :mode "Message")
(bind-key "End of Buffer" #k"\>" :mode "Message")
;;;
(bind-key "Next Message" #k"meta-n" :mode "Headers")
(bind-key "Previous Message" #k"meta-p" :mode "Headers")
(bind-key "Next Undeleted Message" #k"n" :mode "Headers")
(bind-key "Previous Undeleted Message" #k"p" :mode "Headers")
(bind-key "Send Message" #k"s" :mode "Headers")
(bind-key "Send Message" #k"m" :mode "Headers")
(bind-key "Forward Message" #k"f" :mode "Headers")
(bind-key "Headers Delete Message" #k"k" :mode "Headers")
(bind-key "Headers Undelete Message" #k"u" :mode "Headers")
(bind-key "Headers Refile Message" #k"o" :mode "Headers")
(bind-key "List Mail Buffers" #k"l" :mode "Headers")
(bind-key "Quit Headers" #k"q" :mode "Headers")
(bind-key "Incorporate and Read New Mail" #k"i" :mode "Headers")
(bind-key "Beginning of Buffer" #k"\<" :mode "Headers")
(bind-key "End of Buffer" #k"\>" :mode "Headers")


;;; Headers mode.
;;;
(bind-key "Delete Message and Down Line" #k"d" :mode "Headers")
(bind-key "Pick Headers" #k"h" :mode "Headers")
(bind-key "Show Message" #k"space" :mode "Headers")
(bind-key "Show Message" #k"." :mode "Headers")
(bind-key "Reply to Message" #k"r" :mode "Headers")
(bind-key "Expunge Messages" #k"!" :mode "Headers")
(bind-key "Headers Help" #k"?" :mode "Headers")
(bind-key "Refresh Headers" #k"g" :mode "Headers")
(bind-key "Sort Headers" #k"s" :mode "Headers")

;;; Message mode.
;;;
(bind-key "Delete Message and Show Next" #k"d" :mode "Message")
(bind-key "Goto Headers Buffer" #k"^" :mode "Message")
(bind-key "Scroll Message" #k"space" :mode "Message")
(bind-key "Scroll Message" #k"control-v" :mode "Message")
(bind-key "Scroll Window Up" #k"backspace" :mode "Message")
(bind-key "Scroll Window Up" #k"delete" :mode "Message")
(bind-key "Reply to Message in Other Window" #k"r" :mode "Message")
(bind-key "Edit Message Buffer" #k"e" :mode "Message")
(bind-key "Insert Message Region" #k"hyper-y" :mode "Message")
(bind-key "Message Help" #k"?" :mode "Message")
(bind-key "Fill Paragraph Respect Comment" #k"meta-q" :mode "Message")
(bind-key "Toggle Message Headers" #k"hyper-s h" :mode "Message")

;;; Draft mode.
;;;
(bind-key "Goto Headers Buffer" #k"hyper-^" :mode "Draft")
(bind-key "Goto Message Buffer" #k"hyper-m" :mode "Draft")
;(bind-key "Deliver Message" #k"hyper-s" :mode "Draft")
(bind-key "Deliver Message" #k"hyper-c" :mode "Draft")
(bind-key "Insert Message Buffer" #k"hyper-y" :mode "Draft")
(bind-key "Delete Draft and Buffer" #k"hyper-q" :mode "Draft")
(bind-key "List Mail Buffers" #k"hyper-l" :mode "Draft")
(bind-key "Draft Help" #k"hyper-?" :mode "Draft")
(bind-key "Fill Paragraph Respect Comment" #k"meta-q" :mode "Draft")
(bind-key "Append File" #k"hyper-r" :mode "Draft")
(bind-key "Attach File" #k"hyper-a" :mode "Draft")
(bind-key "Draft Insert Space" #k"space" :mode "Draft")
(bind-key "Draft New Line" #k"return" :mode "Draft")


;;;; Netnews.

;;; Clear everything user might hit to avoid getting the internal error
;;; message about modifying read-only buffers.
;;;
(do-alpha-key-events (key-event :both)
  (bind-key "Illegal" key-event :mode "News-Headers")
  (bind-key "Illegal" key-event :mode "News-Message"))


;;; Global Netnews bindings
;;;
(bind-key "Netnews Post Message" #k"C-x P")


;;; Both News-Headers and News-Message modes.
;;;
;;; The bindings in these two blocks should be the same, one for "News-Message"
;;; mode and one for "News-Headers" mode.
;;;
(bind-key "List All Groups" #k"l" :mode "News-Headers")
(bind-key "Netnews Append to File" #k"a" :mode "News-Headers")
(bind-key "Netnews Forward Message" #k"f" :mode "News-Headers")
(bind-key "Netnews Go to Next Group" #k"g" :mode "News-Headers")
(bind-key "Netnews Next Article" #k"n" :mode "News-Headers")
(bind-key "Netnews Previous Article" #k"p" :mode "News-Headers")
(bind-key "Netnews Quit Starting Here" #k"." :mode "News-Headers")
(bind-key "Netnews Group Punt Messages" #k"G" :mode "News-Headers")
(bind-key "Netnews Show Whole Header" #k"w" :mode "News-Headers")
(bind-key "Netnews Reply to Sender in Other Window" #k"r" :mode "News-Headers")
(bind-key "Netnews Reply to Group in Other Window" #k"R" :mode "News-Headers")
;;;
(bind-key "List All Groups" #k"l" :mode "News-Message")
(bind-key "Netnews Append to File" #k"a" :mode "News-Message")
(bind-key "Netnews Forward Message" #k"f" :mode "News-Message")
(bind-key "Netnews Go to Next Group" #k"g" :mode "News-Message")
(bind-key "Netnews Next Article" #k"n" :mode "News-Message")
(bind-key "Netnews Previous Article" #k"p" :mode "News-Message")
(bind-key "Netnews Quit Starting Here" #k"." :mode "News-Message")
(bind-key "Netnews Group Punt Messages" #k"G" :mode "News-Message")
(bind-key "Netnews Show Whole Header" #k"w" :mode "News-Message")
(bind-key "Netnews Reply to Sender in Other Window" #k"r" :mode "News-Message")
(bind-key "Netnews Reply to Group in Other Window" #k"R" :mode "News-Message")


;;; News-Headers.
;;;
(bind-key "Netnews Exit" #k"q" :mode "News-Headers")
(bind-key "Netnews Headers File Message" #k"o" :mode "News-Headers")
(bind-key "Netnews Headers Scroll Window Down" #k"c-v" :mode "News-Headers")
(bind-key "Netnews Headers Scroll Window Up" #k"m-v" :mode "News-Headers")
(bind-key "Netnews Next Line" #k"C-n" :mode "News-Headers")
(bind-key "Netnews Next Line" #k"Downarrow" :mode "News-Headers")
(bind-key "Netnews Previous Line" #k"C-p" :mode "News-Headers")
(bind-key "Netnews Previous Line" #k"Uparrow" :mode "News-Headers")
(bind-key "Netnews Select Message Buffer" #k"hyper-m" :mode "News-Headers")
(bind-key "Netnews Show Article" #k"space" :mode "News-Headers")


;;; News-Message.
;;;
(bind-key "Insert Message Region" #k"Hyper-y" :mode "News-Message")
(bind-key "Netnews Message File Message" #k"o" :mode "News-Message")
(bind-key "Netnews Message Keep Buffer" #k"k" :mode "News-Message")
(bind-key "Netnews Message Quit" #k"q" :mode "News-Message")
(bind-key "Netnews Message Scroll Down"  #k"space" :mode "News-Message")
(bind-key "Netnews Goto Draft Buffer" #k"hyper-d" :mode "News-Message")
(bind-key "Netnews Goto Headers Buffer" #k"^" :mode "News-Message")
(bind-key "Netnews Goto Headers Buffer" #k"hyper-h" :mode "News-Message")
(bind-key "Netnews Goto Post Buffer" #k"hyper-p" :mode "News-Message")
(bind-key "Scroll Window Up" #k"backspace" :mode "News-Message")


;;; Post.
;;;
(bind-key "Netnews Select Message Buffer" #k"hyper-m" :mode "Post")
(bind-key "Netnews Deliver Post" #k"hyper-s" :mode "Post")
(bind-key "Netnews Abort Post" #k"hyper-q" :mode "Post")
(bind-key "Insert Message Buffer" #k"Hyper-y" :mode "Post")


;;; News-Browse.

(bind-key "Netnews Quit Browse" #k"q" :mode "News-Browse")
(bind-key "Netnews Browse Add Group To File" #k"a" :mode "News-Browse")
(bind-key "Netnews Browse Read Group" #k"space" :mode "News-Browse")
(bind-key "Next Line" #k"n" :mode "News-Browse")
(bind-key "Previous Line" #k"p" :mode "News-Browse")



;;;; Version Control.

(bind-key "VC Buffer File Log Entry" #k"control-x v l")
(bind-key "VC Commit Buffer File" #k"control-x v c")
(bind-key "VC Update Buffer File" #k"control-x v u")
(bind-key "VC Toggle Buffer File Lock" #k"control-x v k")  ; k for locK.
(bind-key "VC Compare Buffer File" #k"control-x v =")
;(bind-key "VC Annotate" #k"control-x v a")

;;;; Viewing version control logs (VC-Log).

(bind-key "Scroll Window Up" #k"delete" :mode "VC-Log")
(bind-key "Scroll Window Down" #k"space" :mode "VC-Log")
(bind-key "Rotate Buffers Forward" #k"q" :mode "VC-Log")

;;;; Entering version control logs (VC-Log-Entry).

(bind-key "Previous Parse" #k"meta-p" :mode "VC-Log-Entry")
(bind-key "Next Parse" #k"meta-n" :mode "VC-Log-Entry")


;;;; Process (Shell).

(bind-key "Shell" #k"control-meta-s")
(bind-key "Confirm Process Input" #k"return" :mode "Process")
(bind-key "Shell Complete Filename" #k"M-escape" :mode "Process")
(bind-key "Interrupt Buffer Subprocess" #k"hyper-c" :mode "Process")
(bind-key "Stop Buffer Subprocess" #k"hyper-z" :mode "Process")
(bind-key "Quit Buffer Subprocess" #k"hyper-\\")
(bind-key "Send EOF to Process" #k"hyper-d")

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



;;;; Bufed.

(bind-key "Bufed" #k"control-x control-meta-b")
(bind-key "Bufed Delete" #k"d" :mode "Bufed")
(bind-key "Bufed Delete" #k"control-d" :mode "Bufed")
(bind-key "Bufed Undelete" #k"u" :mode "Bufed")
(bind-key "Bufed Expunge" #k"!" :mode "Bufed")
(bind-key "Bufed Quit" #k"q" :mode "Bufed")
(bind-key "Bufed Goto" #k"space" :mode "Bufed")
(bind-key "Bufed Goto and Quit" #k"super-leftdown" :mode "Bufed")
(bind-key "Bufed Save File" #k"s" :mode "Bufed")
(bind-key "Next Line" #k"n" :mode "Bufed")
(bind-key "Previous Line" #k"p" :mode "Bufed")


(bind-key "Bufed Help" #k"?" :mode "Bufed")



;;;; Dired.

(bind-key "Dired" #k"control-x control-meta-d")

(bind-key "Dired Delete File and Down Line" #k"d" :mode "Dired")
(bind-key "Dired Delete File with Pattern" #k"D" :mode "Dired")
(bind-key "Dired Delete File" #k"control-d" :mode "Dired")
(bind-key "Dired Delete File" #k"k" :mode "Dired")

(bind-key "Dired Undelete File and Down Line" #k"u" :mode "Dired")
(bind-key "Dired Undelete File with Pattern" #k"U" :mode "Dired")

(bind-key "Dired Mark File and Down Line" #k"m" :mode "Dired")
(bind-key "Dired Mark File with Pattern" #k"M" :mode "Dired")
(bind-key "Dired Mark File" #k"control-m" :mode "Dired")
(bind-key "Dired Clear File Marks and Down Line" #k"u" :mode "Dired")
(bind-key "Dired Clear File Marks with Pattern" #k"U" :mode "Dired")
(bind-key "Dired Toggle Marks" #k"t" :mode "Dired")

(bind-key "Dired Clear All Marks" #k"* !" :mode "Dired")

(bind-key "Dired Toggle Backups" #k"s b" :mode "Dired")
(bind-key "Dired Toggle Hidden Files" #k"s h" :mode "Dired")
(bind-key "Dired Toggle Version Control" #k"s v" :mode "Dired")

(bind-key "Dired Expunge Files" #k"!" :mode "Dired")
(bind-key "Dired Update Buffer" #k"g" :mode "Dired")
(bind-key "Dired View File" #k"space" :mode "Dired")
(bind-key "Dired Edit File" #k"e" :mode "Dired")
(bind-key "Dired Up Directory" #k"^" :mode "Dired")
(bind-key "Dired Quit" #k"q" :mode "Dired")
(bind-key "Dired Help" #k"?" :mode "Dired")

(bind-key "Dired Copy File" #k"c" :mode "Dired")
(bind-key "Dired Copy with Wildcard" #k"C" :mode "Dired")
(bind-key "Dired Rename File" #k"r" :mode "Dired")
(bind-key "Dired Rename with Wildcard" #k"R" :mode "Dired")
(bind-key "Dired Symlink File" #k"l" :mode "Dired")
(bind-key "Dired Compare Files" #k"=" :mode "Dired")
(bind-key "Dired Shell Command on File" #k"!" :mode "Dired")
(bind-key "Dired WWW File" #k"W" :mode "Dired")

(bind-key "Dired VC Update Directory" #k"meta-u" :mode "Dired")
(bind-key "Dired VC Update File" #k"hyper-u" :mode "Dired")
(bind-key "Dired VC Commit File" #k"hyper-c" :mode "Dired")
(bind-key "Dired VC Log Entry" #k"hyper-l" :mode "Dired")
(bind-key "Dired VC Compare File" #k"hyper-=" :mode "Dired")

(bind-key "Next Line" #k"n" :mode "Dired")
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
;; TODO
;;(bind-key "Next Manual Reference" #k"tab" :mode "Manual")
;;(bind-key "Previous Manual Reference" #k"M-tab" :mode "Manual")



;;;; System Process Listing mode.

(bind-key "Next Line" #k"n" :mode "SysProc")
(bind-key "Previous Line" #k"p" :mode "SysProc")
(bind-key "Refresh System Processes" #k"g" :mode "SysProc")
(bind-key "Set Current User" #k"u" :mode "SysProc")
(bind-key "Kill Process" #k"k" :mode "SysProc")



;;;; Info Mode.

(bind-key "Info Directory" #k"d" :mode "Info")
(bind-key "Next Info Node" #k"n" :mode "Info")
(bind-key "Previous Info Node" #k"p" :mode "Info")
(bind-key "Parent Info Node" #k"u" :mode "Info")
(bind-key "Parent Info Node" #k"^" :mode "Info")
(bind-key "Top Info Node" #k"t" :mode "Info")
(bind-key "Info Node from Point" #k"return" :mode "Info")
(bind-key "Forward Info Node" #k"f" :mode "Info")
(bind-key "Backward Info Node" #k"b" :mode "Info") ; FIX Scroll Win Up in View Mode.
(bind-key "Backward Info Node" #k"l" :mode "Info")
(bind-key "Next Info Reference" #k"tab" :mode "Info")
(bind-key "Previous Info Reference" #k"M-tab" :mode "Info")



;;;; GNU Info Mode.

(bind-key "Next Ginfo Node" #k"n" :mode "Ginfo")
(bind-key "Previous Ginfo Node" #k"p" :mode "Ginfo")
(bind-key "Parent Ginfo Node" #k"u" :mode "Ginfo")
(bind-key "Parent Ginfo Node" #k"^" :mode "Ginfo")
(bind-key "Top Ginfo Node" #k"t" :mode "Ginfo")
(bind-key "Ginfo Node from Point" #k"return" :mode "Ginfo")
(bind-key "Forward Ginfo Node" #k"f" :mode "Ginfo")
(bind-key "Backward Ginfo Node" #k"b" :mode "Ginfo")
(bind-key "Backward Ginfo Node" #k"l" :mode "Ginfo")
(bind-key "Next Ginfo Reference" #k"tab" :mode "Ginfo")
(bind-key "Previous Ginfo Reference" #k"M-tab" :mode "Ginfo")



;;;; Calendar and Diary.

(bind-key "Goto Today" #k"." :mode "Calendar")
(bind-key "Refresh Calendar" #k"g" :mode "Calendar")
(bind-key "Calendar Show Year" #k"y" :mode "Calendar")
(bind-key "Calendar Show 3 Months" #k"3" :mode "Calendar")
(bind-key "Show Diary Entries" #k"d" :mode "Calendar")
(bind-key "Insert Diary Entry" #k"i d" :mode "Calendar")
(bind-key "Rotate Buffers Forward" #k"q" :mode "Calendar")



;;;; WWW.

(bind-key "WWW Resource from Point" #k"return" :mode "WWW")
(bind-key "WWW Resource from Point in New Buffer" #k"meta-return" :mode "WWW")
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



;;;; Lisp Library.


(bind-key "Describe Pointer Library Entry" #k"leftdown" :mode "Lisp-Lib")
(bind-key "Load Pointer Library Entry" #k"rightdown" :mode "Lisp-Lib")
(bind-key "Describe Library Entry" #k"space" :mode "Lisp-Lib")
(bind-key "Load Library Entry" #k"l" :mode "Lisp-Lib")
(bind-key "Exit Lisp Library" #k"q" :mode "Lisp-Lib")
(bind-key "Lisp Library Help" #k"?" :mode "Lisp-Lib")



;;;; Completion mode.

(dolist (c (command-bindings (getstring "Self Insert" *command-names*)))
  (bind-key "Completion Self Insert" (car c) :mode "Completion"))

(bind-key "Completion Self Insert" #k"space" :mode "Completion")
(bind-key "Completion Self Insert" #k"tab" :mode "Completion")
(bind-key "Completion Self Insert" #k"return" :mode "Completion")
(bind-key "Completion Self Insert" #k"linefeed" :mode "Completion")

(bind-key "Completion Complete Word" #k"end")
(bind-key "Completion Rotate Completions" #k"meta-end")



;;;; Caps-Lock mode.

(do-alpha-key-events (key-event :lower)
  (bind-key "Self Insert Caps Lock" key-event :mode "CAPS-LOCK"))



;;;; Logical characters.

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
