;;; All the definitions of keysyms (see key-event.lisp).  These keysyms
;;; match those for X11.

(in-package "EDI")

;;; The IBM RT keyboard has X11 keysyms defined for the following modifier
;;; keys, but we leave them mapped to nil indicating that they are non-events
;;; to be ignored:
;;;    ctrl		65507
;;;    meta (left)	65513
;;;    meta (right)	65514
;;;    shift (left)	65505
;;;    shift (right)	65506
;;;    lock		65509
;;;

#[ Editor Key-events

The canonical representation of editor input is a key-event.  When you type
on the keyboard, the editor receives key-events.  Key-events have names for
their basic form, and we refer to this name as a keysym.  This manual
displays keysyms in a Bold font.  For example, a and b are the keys that
normally cause the editor to insert the characters a and b.

Key-events have modifiers or bits indicating a special interpretation of
the root key-event.  Although the keyboard places limitations on what
key-events you can actually type, the editor understands arbitrary
combinations of the following modifiers: Control, Meta, Super, Hyper,
Shift, and Lock.  This manual represents the bits in a key-event by
prefixing the keysym with combinations of C-, M-, S-, H-, Shift-, and Lock.
For example, a with both the control and meta bits set appears as C-M-a.
In general, ignore the shift and lock modifiers since this manual never
talks about keysyms that explicitly have these bits set; that is, it may
talk about the key-event A, but it would never mention Shift-a.  These are
actually distinct key-events, but typical input coercion turns presents the
editor with the former, not the latter.

Key-event modifiers are totally independent of the keysym.  This may be new
to you if you are used to thinking in terms of ASCII character codes.  For
example, with key-events you can distinctly identify both uppercase and
lowercase keysyms with the control bit set; therefore, C-a and C-A may have
different meanings to the editor.

Some keysyms' names consist of more than a single character, and these
usually correspond to the legend on the keyboard.  For example, some
keyboards let you enter Home, Return, F9, etc.

In addition to a keyboard, you may have a mouse or pointer device.
Key-events also represent this kind of input.  For example, the down and up
transitions of the left button correspond to the Leftdown and Leftup
keysyms.

See sections [ Key Bindings ], [ Using X ] and [ Using Terminals ].
]#

#[ Editor Input

The canonical representation of editor input is a key-event structure.
Users can bind commands to keys (see section refkey-bindings), which are
non-zero length sequences of key-events.  A key-event consists of an
identifying token known as a keysym and a field of bits representing
modifiers.  Users define keysyms, integers between 0 and 65535 inclusively,
by supplying names that reflect the legends on their keyboard's keys.
Users define modifier names similarly, but the system chooses the bit and
mask for recognizing the modifier.  You can use keysym and modifier names
to textually specify key-events and editor keys in a #k syntax.  The
following are some examples:

    #k"C-u"
    #k"Control-u"
    #k"c-m-z"
    #k"control-x meta-d"
    #k"a"
    #k"A"
    #k"Linefeed"

This is convenient for use within code and in init files containing
bind-key calls.

The #k syntax is delimited by double quotes, but the system parses the
contents rather than reading it as a Lisp string.  Within the double
quotes, spaces separate multiple key-events.  A single key-event optionally
starts with modifier names terminated by hyphens.  Modifier names are
alphabetic sequences of characters which the system uses
case-insensitively.  Following modifiers is a keysym name, which is
case-insensitive if it consists of multiple characters, but if the name
consists of only a single character, then it is case-sensitive.

You can escape special characters -- hyphen, double quote, open angle
bracket, close angle bracket, and space -- with a backslash, and you can
specify a backslash by using two contiguously.  You can use angle brackets to
enclose a keysym name with many special characters in it.  Between angle
brackets appearing in a keysym name position, there are only two special
characters, the closing angle bracket and backslash.

The [Editor Key-events] section details key-events.
]#

;;; Function keys for the RT.
;;;
(ext:define-keysym 65470 "F1")
(ext:define-keysym 65471 "F2")
(ext:define-keysym 65472 "F3")
(ext:define-keysym 65473 "F4")
(ext:define-keysym 65474 "F5")
(ext:define-keysym 65475 "F6")
(ext:define-keysym 65476 "F7")
(ext:define-keysym 65477 "F8")
(ext:define-keysym 65478 "F9")
(ext:define-keysym 65479 "F10")
(ext:define-keysym 65480 "F11" "L1")
(ext:define-keysym 65481 "F12" "L2")

;;; Function keys for the Sun (and other keyboards) -- L1-L10 and R1-R15.
;;;
(ext:define-keysym 65482 "F13" "L3")
(ext:define-keysym 65483 "F14" "L4")
(ext:define-keysym 65484 "F15" "L5")
(ext:define-keysym 65485 "F16" "L6")
(ext:define-keysym 65486 "F17" "L7")
(ext:define-keysym 65487 "F18" "L8")
(ext:define-keysym 65488 "F19" "L9")
(ext:define-keysym 65489 "F20" "L10")
(ext:define-keysym 65490 "F21" "R1")
(ext:define-keysym 65491 "F22" "R2")
(ext:define-keysym 65492 "F23" "R3")
(ext:define-keysym 65493 "F24" "R4")
(ext:define-keysym 65494 "F25" "R5")
(ext:define-keysym 65495 "F26" "R6")
(ext:define-keysym 65496 "F27" "R7")
(ext:define-keysym 65497 "F28" "R8")
(ext:define-keysym 65498 "F29" "R9")
(ext:define-keysym 65499 "F30" "R10")
(ext:define-keysym 65500 "F31" "R11")
(ext:define-keysym 65501 "F32" "R12")
(ext:define-keysym 65502 "F33" "R13")
(ext:define-keysym 65503 "F34" "R14")
(ext:define-keysym 65504 "F35" "R15")

;;; Upper right key bank.
;;;
(ext:define-keysym 65377 "Printscreen")
;; Couldn't type scroll lock.
(ext:define-keysym 65299 "Pause")

;;; Middle right key bank.
;;;
(ext:define-keysym 65379 "Insert")
(ext:define-keysym 65535 "Delete" "Rubout" (string (code-char 127)))
(ext:define-keysym 65360 "Home")
(ext:define-keysym 65365 "Pageup")
(ext:define-keysym 65367 "End")
(ext:define-keysym 65366 "Pagedown")

;;; Arrows.
;;;
(ext:define-keysym 65361 "Leftarrow")
(ext:define-keysym 65362 "Uparrow")
(ext:define-keysym 65364 "Downarrow")
(ext:define-keysym 65363 "Rightarrow")

;;; Number pad.
;;;
(ext:define-keysym 65407 "Numlock")
(ext:define-keysym 65421 "Numpad\-Return" "Numpad\-Enter")	;num-pad-enter
(ext:define-keysym 65455 "Numpad/") 				;num-pad-/
(ext:define-keysym 65450 "Numpad*")				;num-pad-*
(ext:define-keysym 65453 "Numpad-")				;num-pad--
(ext:define-keysym 65451 "Numpad+")				;num-pad-+
(ext:define-keysym 65456 "Numpad0")				;num-pad-0
(ext:define-keysym 65457 "Numpad1")				;num-pad-1
(ext:define-keysym 65458 "Numpad2")				;num-pad-2
(ext:define-keysym 65459 "Numpad3")				;num-pad-3
(ext:define-keysym 65460 "Numpad4")				;num-pad-4
(ext:define-keysym 65461 "Numpad5")				;num-pad-5
(ext:define-keysym 65462 "Numpad6")				;num-pad-6
(ext:define-keysym 65463 "Numpad7")				;num-pad-7
(ext:define-keysym 65464 "Numpad8")				;num-pad-8
(ext:define-keysym 65465 "Numpad9")				;num-pad-9
(ext:define-keysym 65454 "Numpad.")				;num-pad-.

;;; "Named" keys.
;;;
(ext:define-keysym 65289 "Tab")
(ext:define-keysym 65307 "Escape" "Altmode" "Alt")		;escape
(ext:define-keysym 65288 "Backspace")				;backspace
(ext:define-keysym 65293 "Return" "Enter")			;enter
(ext:define-keysym 65512 "Linefeed" "Action" "Newline")		;action
(ext:define-keysym 32 "Space" " ")

;;; Letters.
;;;
(ext:define-keysym 97 "a") (ext:define-keysym 65 "A")
(ext:define-keysym 98 "b") (ext:define-keysym 66 "B")
(ext:define-keysym 99 "c") (ext:define-keysym 67 "C")
(ext:define-keysym 100 "d") (ext:define-keysym 68 "D")
(ext:define-keysym 101 "e") (ext:define-keysym 69 "E")
(ext:define-keysym 102 "f") (ext:define-keysym 70 "F")
(ext:define-keysym 103 "g") (ext:define-keysym 71 "G")
(ext:define-keysym 104 "h") (ext:define-keysym 72 "H")
(ext:define-keysym 105 "i") (ext:define-keysym 73 "I")
(ext:define-keysym 106 "j") (ext:define-keysym 74 "J")
(ext:define-keysym 107 "k") (ext:define-keysym 75 "K")
(ext:define-keysym 108 "l") (ext:define-keysym 76 "L")
(ext:define-keysym 109 "m") (ext:define-keysym 77 "M")
(ext:define-keysym 110 "n") (ext:define-keysym 78 "N")
(ext:define-keysym 111 "o") (ext:define-keysym 79 "O")
(ext:define-keysym 112 "p") (ext:define-keysym 80 "P")
(ext:define-keysym 113 "q") (ext:define-keysym 81 "Q")
(ext:define-keysym 114 "r") (ext:define-keysym 82 "R")
(ext:define-keysym 115 "s") (ext:define-keysym 83 "S")
(ext:define-keysym 116 "t") (ext:define-keysym 84 "T")
(ext:define-keysym 117 "u") (ext:define-keysym 85 "U")
(ext:define-keysym 118 "v") (ext:define-keysym 86 "V")
(ext:define-keysym 119 "w") (ext:define-keysym 87 "W")
(ext:define-keysym 120 "x") (ext:define-keysym 88 "X")
(ext:define-keysym 121 "y") (ext:define-keysym 89 "Y")
(ext:define-keysym 122 "z") (ext:define-keysym 90 "Z")

;;; Standard number keys.
;;;
(ext:define-keysym 49 "1") (ext:define-keysym 33 "!")
(ext:define-keysym 50 "2") (ext:define-keysym 64 "@")
(ext:define-keysym 51 "3") (ext:define-keysym 35 "#")
(ext:define-keysym 52 "4") (ext:define-keysym 36 "$")
(ext:define-keysym 53 "5") (ext:define-keysym 37 "%")
(ext:define-keysym 54 "6") (ext:define-keysym 94 "^")
(ext:define-keysym 55 "7") (ext:define-keysym 38 "&")
(ext:define-keysym 56 "8") (ext:define-keysym 42 "*")
(ext:define-keysym 57 "9") (ext:define-keysym 40 "(")
(ext:define-keysym 48 "0") (ext:define-keysym 41 ")")

;;; "Standard" symbol keys.
;;;
(ext:define-keysym 96 "`") (ext:define-keysym 126 "~")
(ext:define-keysym 45 "-") (ext:define-keysym 95 "_")
(ext:define-keysym 61 "=") (ext:define-keysym 43 "+")
(ext:define-keysym 91 "[") (ext:define-keysym 123 "{")
(ext:define-keysym 93 "]") (ext:define-keysym 125 "}")
(ext:define-keysym 92 "\\") (ext:define-keysym 124 "|")
(ext:define-keysym 59 ";") (ext:define-keysym 58 ":")
(ext:define-keysym 39 "'") (ext:define-keysym 34 "\"")
(ext:define-keysym 44 ",") (ext:define-keysym 60 "<")
(ext:define-keysym 46 ".") (ext:define-keysym 62 ">")
(ext:define-keysym 47 "/") (ext:define-keysym 63 "?")

;;; Standard Mouse keysyms.
;;;
(ext::define-mouse-keysym 1 25601 "Leftdown" "Super" :button-press)
(ext::define-mouse-keysym 1 25602 "Leftup" "Super" :button-release)

(ext::define-mouse-keysym 2 25603 "Middledown" "Super" :button-press)
(ext::define-mouse-keysym 2 25604 "Middleup" "Super" :button-release)

(ext::define-mouse-keysym 3 25605 "Rightdown" "Super" :button-press)
(ext::define-mouse-keysym 3 25606 "Rightup" "Super" :button-release)

(ext::define-mouse-keysym 4 25607 "Fourdown" "Super" :button-press)
(ext::define-mouse-keysym 4 25608 "Fourup" "Super" :button-release)

(ext::define-mouse-keysym 5 25609 "Fivedown" "Super" :button-press)
(ext::define-mouse-keysym 5 25610 "Fiveup" "Super" :button-release)

;;; Sun keyboard.
;;;
(ext:define-keysym 65387 "break")			;alternate (Sun).
;(ext:define-keysym 65290 "linefeed")



;;;; SETFs of KEY-EVENT-CHAR and CHAR-KEY-EVENT.

;;; Converting ASCII control characters to Lisp control characters: ASCII
;;; control character codes are separated from the codes of the
;;; "non-controlified" characters by the code of atsign.  The ASCII control
;;; character codes range from ^@ (0) through ^_ (one less than the code of
;;; space).  We iterate over this range adding the ASCII code of atsign to
;;; get the "non-controlified" character code.  With each of these, we turn
;;; the code into a Lisp character and set its :control bit.  Certain ASCII
;;; control characters have to be translated to special Lisp characters
;;; outside of the loop.  With the advent of the editor running under X,
;;; and all the key bindings changing, we also downcase each Lisp character
;;; (where normally control characters come in upcased) in an effort to
;;; obtain normal command bindings.  Commands bound to uppercase modified
;;; characters will not be accessible to terminal interaction.
;;;
(let ((@-code (char-code #\@)))
  (dotimes (i (char-code #\space))
    (setf (ext:char-key-event (code-char i))
	  (ext::make-key-event (string (char-downcase (code-char (+ i @-code))))
			       (key-event-modifier-mask "control")))))
(setf (ext:char-key-event (code-char 9)) (ext::make-key-event #k"Tab"))
(setf (ext:char-key-event (code-char 10)) (ext::make-key-event #k"Linefeed"))
(setf (ext:char-key-event (code-char 13)) (ext::make-key-event #k"Return"))
(setf (ext:char-key-event (code-char 27)) (ext::make-key-event #k"Alt"))
(setf (ext:char-key-event (code-char 8)) (ext::make-key-event #k"Backspace"))
;;;
;;; Other ASCII codes are exactly the same as the Lisp codes.
;;;
(do ((i (char-code #\space) (1+ i)))
    ((= i 128))
  (setf (ext:char-key-event (code-char i))
	(ext::make-key-event (string (code-char i)))))

;;; This makes KEY-EVENT-CHAR the inverse of CHAR-KEY-EVENT from the start.
;;; It need not be this way, but it is.
;;;
(dotimes (i 128)
  (let ((character (code-char i)))
    (setf (ext::key-event-char (ext:char-key-event character)) character)))

;;; Since we treated these characters specially above when setting
;;; EXT:CHAR-KEY-EVENT above, we must set these EXT:KEY-EVENT-CHAR's
;;; specially to make quoting characters into editor buffers more obvious
;;; for users.
;;;
(setf (ext:key-event-char #k"C-h") #\backspace)
(setf (ext:key-event-char #k"C-i") #\tab)
(setf (ext:key-event-char #k"C-j") #\linefeed)
(setf (ext:key-event-char #k"C-m") #\return)
