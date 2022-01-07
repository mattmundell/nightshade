;;; Terminal Capability
;;;
;;; Parses a Termcap file and returns a data structure suitable for
;;; initializing a redisplay methods device.

;; FIX move to code:

(in-package "EDI")

(export '(*termcap-file*))

#|
(to-file (out ":tmp/tc")
  (format out "~A" (get-termcap "pcansi")))
|#


#[ Use With Terminals

The editor can also be used with ASCII terminals and terminal emulators.
Capabilities that depend on X Windows (such as mouse commands) are not
available, but nearly everything else can be done.

[ Terminal Initialization ]  Speed and capabilities (the termcap file).
[ Terminal Input          ]
[ Terminal Redisplay      ]
[ Terminal Emulators      ]  Notes on setting up various emulators.
]#

#[ Terminal Initialization

== Speed ==

For best redisplay performance, it is very important to set the terminal
speed:

   stty 2400

Often when running the editor using TTY redisplay, the editor will actually
be talking to a PTY whose speed is initialized to infinity.  In reality,
the terminal will be much slower, resulting in the editor's output getting
way ahead of the terminal.  This prevents the editor from briefly stopping
redisplay to allow the terminal to catch up.  This is also affected by
*Scroll Redraw Ratio*, as described in [Terminal Redisplay].

== Capabilities ==

The editor reads the capabilities of the terminal (mostly the terminal
control sequences) from the termcap database, according to the old Unix
conventions.  The "TERM" environment variable holds the type of the current
terminal.  The editor locates the termcap database, in order, by searching
for

  * the file or data specified by the TERMCAP environment variable

  * the file specified by edi::*termcap-file*

  * the file conf:termcap

  * the file library:termcap

  * the file /etc/termcap.


A prepared termcap file is available at

    http://www.mundell.ukfsn.org/nightshade/termcap

Alternatively the same file can be generated with a command such as

    for f in `find /lib/terminfo/`; do infocmp -Cr `basename $f` 2> /dev/null; done > termcap


Example commands that start the editor with a TERMCAP variable are

    TERMCAP=/home/ni/etc/termcap nightshade -edit

and

    export TERMCAP=/home/ni/etc/termcap
    nightshade -edit


The following command alters the size of the terminal from the termcap
value:

    stty rows height columns width
]#

#[ Terminal Input

The most important limitation of a terminal is its input capabilities.  On a
workstation with function keys and independent control, meta, and shift
modifiers, it is possible to type 800 or so distinct single keystrokes.
Although by default, the editor uses only a fraction of these combinations, there
are many more than the 128 key-events available in ASCII.

On a terminal, the editor attempts to translate ASCII control characters into the
most useful key-event:

  - On a terminal, control does not compose with shift.  If the control key is down
     when you type a letter keys, the terminal always sends one code regardless of
     whether the shift key is held.  Since the editor primarily binds commands to
     key-events with keysyms representing lowercase letters regardless of what bits
     are set in the key-event, the system translates the ASCII control codes to a
     keysym representing the appropriate lowercase characters.  This keysym then
     forms a key-event with the control bit set.  Users can type C-c followed
     by an uppercase character to form a key-event with a keysym representing an
     uppercase character and bits with the control bit set.

  - On a terminal, some of the named keys generate an ASCII control code.  For
     example, Return usually sends a C-m.  The system translates these ASCII
     codes to a key-event with an appropriate keysym instead of the keysym named by
     the character which names the ASCII code.  In the above example, typing the
     Return key would generate a key-event with the Return keysym and no
     bits.  It would NOT translate to a key-event with the m keysym and the
     control bit.

Since terminals have no meta key, you must use the Escape and C-Z
modifier-prefix key-events to invoke commands bound to key-events with the meta
bit or meta and control bits set.  ASCII terminals cannot generate all
key-events which have the control bit on, so you can use the C-^
modifier-prefix.  The C-c prefix sets the hyper bit on the next key-event
typed.

When running the editor from a terminal ^\ is the interrupt key-event.
Typing this will place you in the Lisp debugger.

When using a terminal, pop-up output windows cannot be retained after the
completion of the command.
]#

#[ Terminal Emulators

                      TERM
    aterm             xterm
    Eterm             Eterm        -- use TERM=xterm
    fbiterm                        -- fails to find font file
    gnome-terminal    xterm
    konsole           xterm
    kterm             kterm        -- FIX paints fore,back same; bit better w xterm
    linux             linux        -- smooth scroll needs *Scroll Redraw Ratio* (FIX)
                                   -- use TERM=xterm for color and reverse modeline
    mrxvt             rxvt
    powershell        xterm-debian
    pterm             xterm
    putty (ssh)       xterm
    pyqonsole         xterm
    rxvt              rxvt
    wterm             rxvt
    wy60              wyse60
    xfce4-terminal    xterm        -- C-h sends backspace
    xiterm            xterm        -- background painting
                                          (setf (tty-device-clear-to-eol
                                                 (device-hunk-device
                                                  (window-hunk (current-window))))
                                                #'space-to-eol)
    xterm             xterm        -- unset TERMCAP
                                   -- how FIX Alt keys?
                                          xmodmap -e "keycode 64 = Meta_L"
                                          xmodmap -e "keycode 113 = Meta_R"
                                          XTerm*eightBitInput: false
                                          XTerm*eightBitOutput: false
                                   -- smooth scroll needs *Scroll Redraw Ratio* (FIX)
                                   -- FIX modeline jump on enter
    xvt               xterm-r6
    yakuake           xterm        -- (drop down kde terminal, F12 to drop/raise)

FIX check special keys
]#


;;;; Interface for device creating code.

(defvar *termcap-file* ()
  "File of terminal capability descriptions.  Overridden if there is a
   TERMCAP environment variable.")

#|
(defun generate-termcap ()
  (ignore-errors
   (run-program
    "/bin/sh"
    (list "-c"
	  (format ()
		  "for f in `find /lib/terminfo/`; do infocmp -Cr `basename $f` 2> /dev/null; done > ~A/termcap && sync"
		  (namify
		   (namestring (car (search-list
				     "library:"))))))
    :output *standard-output*))
  (unix:unix-sync)
  (shell:cat "library:termcap")
  (probe-file "library:termcap"))
|#

(defun generate-termcap (&optional (dest "library:termcap"))
  (to-file (out dest :if-exists :error)
    (do-files (file "/lib/terminfo/"
		    :recurse t :check-for-subdirs t :all ())
      (or (directory-name-p file)
	  (run-program "infocmp"
		       (list "-Cr" (file-namestring file))
		       :output out
		       :error ())))
    (finish-output out))
  (probe-file dest))

(defconstant termcap-last-resort
  "Failed to find, read or generate a termcap file.

   The terminal version of the editor needs a termcap file to run.
   For some reason the editor has failed to find, read or create such a
   file.  This could be due to restrictions on reading an existing termcap
   file, restrictions on writing to the directories conf: and library:, or
   the lack of an infocmp program (provided by packages like ncurses-bin).

   There is a termcap file available at

       http://www.mundell.ukfsn.org/nightshade/termcap

   for copying to conf: (which is usually ~~/.nightshade/), library: or
   /etc/.")

(defun find-termcap-file (name)
  (cond (*termcap-file*
	 (with-open-file (s *termcap-file*)
	   (if (find-termcap-entry name s)
	       *termcap-file*
	       (error "Unknown Terminal ~S in file ~S."
		      name *termcap-file*))))
	((file-readable "conf:termcap")
	 (with-open-file (s "conf:termcap")
	   (if (find-termcap-entry name s)
	       "conf:termcap"
	       (error "Unknown Terminal ~S in file ~S."
		      name "conf:termcap"))))
	((file-readable "library:termcap")
	 (with-open-file (s "library:termcap")
	   (if (find-termcap-entry name s)
	       "library:termcap"
	       (error "Unknown Terminal ~S in file ~S."
		      name "library:termcap"))))
	((file-readable termcap-file)
	 (with-open-file (s termcap-file)
	   (if (find-termcap-entry name s)
	       termcap-file
	       (error "Unknown Terminal ~S in file ~S."
		      name termcap-file))))
	((ignore-errors (generate-termcap))
	 (with-open-file (s "library:termcap")
	   (if (find-termcap-entry name s)
	       "library:termcap"
	       (error "Unknown Terminal ~S in file ~S."
		      name "library:termcap"))))
	((ignore-errors (generate-termcap "conf:termcap"))
	 (with-open-file (s "conf:termcap")
	   (if (find-termcap-entry name s)
	       "conf:termcap"
	       (error "Unknown Terminal ~S in file ~S."
		      name "conf:termcap"))))
	(t
	 (error termcap-last-resort))))

(defun get-termcap (name)
  ;; FIX
  "Look in TERMCAP environment variable for terminal capabilities or a file
   to use.  If it is a file, look for $name in it.  If it is a description
   of the capabilities, load the capabilities of $name and add those in
   TERMCAP.  If TERMCAP is undefined, look for $name in termcap-file and
   various standard locations.  Signal an error on failure to find the
   terminal capabilities."
  (let ((termcap-env-var (get-termcap-env-var)))
    (flet ((load-termcap ()
	     (cond (*termcap-file*
		    (with-open-file (s *termcap-file*)
		      (if (find-termcap-entry name s)
			  (parse-fields s)
			  (error "Unknown Terminal ~S in file ~S."
				 name *termcap-file*))))
		   ((file-readable "conf:termcap")
		    (with-open-file (s "conf:termcap")
		      (if (find-termcap-entry name s)
			  (parse-fields s)
			  (error "Unknown Terminal ~S in file ~S."
				 name "conf:termcap"))))
		   ((file-readable "library:termcap")
		    (with-open-file (s "library:termcap")
		      (if (find-termcap-entry name s)
			  (parse-fields s)
			  (error "Unknown Terminal ~S in file ~S."
				 name "library:termcap"))))
		   ((file-readable termcap-file)
		    (with-open-file (s termcap-file)
		      (if (find-termcap-entry name s)
			  (parse-fields s)
			  (error "Unknown Terminal ~S in file ~S."
				 name termcap-file))))
		   ((ignore-errors (generate-termcap))
		    (with-open-file (s "library:termcap")
		      (if (find-termcap-entry name s)
			  (parse-fields s)
			  (error "Unknown Terminal ~S in file ~S."
				 name "library:termcap"))))
		   ((ignore-errors (generate-termcap "conf:termcap"))
		    (with-open-file (s "conf:termcap")
		      (if (find-termcap-entry name s)
			  (parse-fields s)
			  (error "Unknown Terminal ~S in file ~S."
				 name "conf:termcap"))))
		   (t
		    (error termcap-last-resort)))))
      (if (and termcap-env-var
	       (> (length termcap-env-var) 0))
	  (if (char= (schar termcap-env-var 0) #\/) ; hack for filenamep
	      (with-open-file (s termcap-env-var)
		(if (find-termcap-entry name s)
		    (parse-fields s)
		    (error "Unknown Terminal ~S in file ~S."
			   name termcap-env-var)))
	      (with-open-file (termcap (find-termcap-file name))
		(with-input-from-string (s termcap-env-var)
		  (skip-termcap-names s)
		  (parse-fields s termcap))))
	  (load-termcap)))))

(proclaim '(inline termcap))
(defun termcap (name termcap)
  (cdr (assoc name termcap :test #'eq)))


;;;; Finding the termcap entry

(defun find-termcap-entry (name stream)
  (loop
   (let ((end-of-names (lex-termcap-name stream)))
     (when (termcap-found-p name)
       (or end-of-names (skip-termcap-names stream))
       (return t))
     (if end-of-names
	 (unless (skip-termcap-fields stream)
	   (return nil))))))

;;; This buffer is used in LEX-TERMCAP-NAME and PARSE-FIELDS to
;;; do string comparisons and build strings from interpreted termcap
;;; characters, respectively.
;;;
(defvar *termcap-string-buffer* (make-string 300))
(defvar *termcap-string-index* 0)

(eval-when (compile eval load)
;
(defmacro init-termcap-string-buffer ()
  `(setf *termcap-string-index* 0))

(defmacro store-char (char)
  `(progn
    (setf (schar *termcap-string-buffer* *termcap-string-index*) ,char)
    (incf *termcap-string-index*)))

(defmacro termcap-string-buffer-string ()
  `(subseq (the simple-string *termcap-string-buffer*)
	   0 *termcap-string-index*))

) ;eval-when


;;; LEX-TERMCAP-NAME gathers characters until the next #\|, which separate
;;; terminal names, or #\:, which terminate terminal names for an entry.
;;; T is returned if the end of the names is reached for the entry.
;;; If we hit and EOF, act like we found a :.
;;;
(defun lex-termcap-name (stream)
  (init-termcap-string-buffer)
  (loop
   (let ((char (read-char stream nil #\:)))
     (case char
       (#\Linefeed (init-termcap-string-buffer))
       (#\# (read-line stream nil))
       (#\| (return nil))
       (#\: (return t))
       (t (store-char char))))))

(defun termcap-found-p (name)
  (string= name *termcap-string-buffer* :end2 *termcap-string-index*))

;;; SKIP-TERMCAP-NAMES eats characters until the next #\: which terminates
;;; terminal names for an entry.  Stop also at EOF.
;;;
(defun skip-termcap-names (stream)
  (loop
   (when (char= (read-char stream nil #\:) #\:)
     (return))))

;;; SKIP-TERMCAP-FIELDS skips the rest of an entry, returning nil if there
;;; are no more entries in the file.  An entry is terminated by a #\:
;;; followed by a #\newline (possibly by eof).
;;;
(defun skip-termcap-fields (stream)
  (loop
   (multiple-value-bind (line eofp)
			(read-line stream nil)
     (if eofp
	 (return nil)
	 (let ((len (length line)))
	   (declare (simple-string line))
	   (when (and (not (zerop len))
		      (not (char= (schar line 0) #\#))
		      (char= (schar line (1- len)) #\:))
	     (let ((char (read-char stream nil :eof)))
	       (if (eq char :eof)
		   (return nil)
		   (unread-char char stream))
	       (return t))))))))



;;;; Defining known capabilities for parsing purposes.

(eval-when (compile load eval)
(defvar *known-termcaps* ())
) ;eval-when


(eval-when (compile eval load)

;;; DEFTERMCAP makes a terminal capability known for parsing purposes.
;;; Type is one of :string, :number, or :boolean.  Cl-name is an EQ
;;; identifier for the capability.
;;;
(defmacro deftermcap (name type cl-name)
  `(progn (push (list ,name ,type ,cl-name) *known-termcaps*)))

(defmacro termcap-def (name)
  `(cdr (assoc ,name *known-termcaps* :test #'string=)))

(defmacro termcap-def-type (termcap-def)
  `(car ,termcap-def))

(defmacro termcap-def-cl-name (termcap-def)
  `(cadr ,termcap-def))

) ;eval-when

(deftermcap "is" :string :init-string)
(deftermcap "if" :string :init-file)
(deftermcap "ti" :string :init-cursor-motion)
(deftermcap "te" :string :end-cursor-motion)
(deftermcap "al" :string :open-line)
(deftermcap "am" :boolean :auto-margins-p)
(deftermcap "ce" :string :clear-to-eol)
(deftermcap "cl" :string :clear-display)
(deftermcap "cm" :string :cursor-motion)
(deftermcap "co" :number :columns)
(deftermcap "dc" :string :delete-char)
(deftermcap "dm" :string :init-delete-mode)
(deftermcap "ed" :string :end-delete-mode)
(deftermcap "dl" :string :delete-line)
(deftermcap "im" :string :init-insert-mode)
(deftermcap "ic" :string :init-insert-char)
(deftermcap "ip" :string :end-insert-char)
(deftermcap "ei" :string :end-insert-mode)
(deftermcap "li" :number :lines)
(deftermcap "so" :string :init-standout-mode)
(deftermcap "se" :string :end-standout-mode)
(deftermcap "tc" :string :similar-terminal)
(deftermcap "os" :boolean :overstrikes)
(deftermcap "ul" :boolean :underlines)

;;; font related fields
(deftermcap "ae" :string :end-alternate-char-set)
(deftermcap "as" :string :start-alternate-char-set)
(deftermcap "mb" :string :start-blinking-attribute)
(deftermcap "md" :string :start-bold-attribute)
(deftermcap "me" :string :end-all-attributes)
(deftermcap "mh" :string :start-half-bright-attribute)
(deftermcap "mk" :string :start-blank-attribute)
(deftermcap "mp" :string :start-protected-attribute)
(deftermcap "mr" :string :start-reverse-video-attribute)
(deftermcap "ue" :string :end-underscore-mode)
(deftermcap "us" :string :start-underscore-mode)
(deftermcap "Co" :number :colors)
(deftermcap "op" :string :original-pair)
(deftermcap "AF" :string :adjust-fg)
(deftermcap "AB" :string :adjust-bg)

;; FIX where these from? termcap?
#|
rxvt|rxvt terminal emulator (X Window System):\
	:am:eo:km:mi:ms:xn:xo:\
	:co#80:it#8:li#24:\
	:AL=\E[%dL:DC=\E[%dP:DL=\E[%dM:DO=\E[%dB:IC=\E[%d@:\
	:K1=\EOw:K2=\EOu:K3=\EOy:K4=\EOq:K5=\EOs:LE=\E[%dD:\
	:RI=\E[%dC:UP=\E[%dA:ae=^O:al=\E[L:as=^N:bl=^G:cd=\E[J:\
	:ce=\E[K:cl=\E[H\E[2J:cm=\E[%i%d;%dH:cr=^M:\
	:cs=\E[%i%d;%dr:ct=\E[3g:dc=\E[P:dl=\E[M:do=^J:ei=\E[?4l:\
	:ho=\E[H:i1=\E[?47l\E=\E[?1l:ic=\E[@:im=\E[?4h:\
	:is=\E[r\E[m\E[2J\E[H\E[?7h\E[?1;3;4;6l\E[4l:\
	:k0=\E[21~:k1=\E[11~:k2=\E[12~:k3=\E[13~:k4=\E[14~:\
	:k5=\E[15~:k6=\E[17~:k7=\E[18~:k8=\E[19~:k9=\E[20~:\
	:kD=\E[3~:kI=\E[2~:kN=\E[6~:kP=\E[5~:kb=\177:kd=\E[B:\
	:ke=\E>:kh=\E[7~:kl=\E[D:kr=\E[C:ks=\E=:ku=\E[A:le=^H:\
	:mb=\E[5m:md=\E[1m:me=\E[m\017:mr=\E[7m:nd=\E[C:rc=\E8:\
	:sc=\E7:se=\E[27m:sf=^J:so=\E[7m:sr=\EM:st=\EH:ta=^I:\
	:te=\E[?47l\E8:ti=\E7\E[?47h:ue=\E[24m:up=\E[A:us=\E[4m:\
	:vb=\E[?5h\E[?5l:ve=\E[?25h:vi=\E[?25l:vs=\E[?25h:

   added
	:Co#8:op=\E[39;49m:AB=\E[4%dm:AF=\E[3%dm:

xterm-xfree86|XFree86 xterm:\
	:is=\E[!p\E[?3;4l\E[4l\E>:\
	:rs=\E[!p\E[?3;4l\E[4l\E>:\
	:AL=\E[%dL:DL=\E[%dM:DC=\E[%dP:DO=\E[%dB:UP=\E[%dA:\
	:LE=\E[%dD:RI=\E[%dC:\
	:al=\E[L:am:bl=^G:\
	:cd=\E[J:ce=\E[K:cl=\E[H\E[2J:cm=\E[%i%d;%dH:co#80:\
	:cs=\E[%i%d;%dr:ct=\E[3g:\
	:dc=\E[P:dl=\E[M:ho=\E[H:\
	:im=\E[4h:ei=\E[4l:mi:\
	:ks=\E[?1h\E=:ke=\E[?1l\E>:\
	:k1=\EOP:k2=\EOQ:k3=\EOR:k4=\EOS:\
	:k5=\E[15~:k6=\E[17~:k7=\E[18~:k8=\E[19~:k9=\E[20~:\
	:k;=\E[21~:F1=\E[23~:F2=\E[24~:\
	:kn#12:\
	:kH=\E[4~::@7=\E[4~:kh=\E[1~:\
	:@0=\E[1~:kI=\E[2~:kD=^?:\
	:*6=\E[4~:kP=\E[5~:kN=\E[6~:\
	:km:\
	:kb=^H:ku=\EOA:kd=\EOB:kr=\EOC:kl=\EOD:\
	:li#24:md=\E[1m:me=\E[m^O:mr=\E[7m:ms:nd=\E[C:\
	:eA=\E)0:as=^N:ae=^O:ml=\El:mu=\Em:\
	:sc=\E7:rc=\E8:sf=\n:so=\E[7m:se=\E[27m:sr=\EM:st=\EH:\
	:ti=\E7\E[?47h:te=\E[2J\E[?47l\E8:\
	:vi=\E[?25l:ve=\E[?25h:\
	:up=\E[A:us=\E[4m:ue=\E[24m:xn:\
	:ut:Co#8:op=\E[39;49m:AB=\E[4%dm:AF=\E[3%dm:\
	:pa#64:Sf=\E[3%dm:Sb=\E[4%dm:
|#


;;;; Parsing an entry.

(defvar *getchar-ungetchar-buffer* nil)

(eval-when (compile eval load)

;;; UNGETCHAR  --  Internal.
;;;
;;; We need this to be able to peek ahead more than one character.
;;; This is used in PARSE-FIELDS and GET-TERMCAP-STRING-CHAR.
;;;
(defmacro ungetchar (char)
  `(push ,char *getchar-ungetchar-buffer*))

;;; GETCHAR  --  Internal.
;;;
;;; This is used in PARSE-FIELDS and GET-TERMCAP-STRING-CHAR.
;;;
(defmacro getchar ()
  `(loop
    (setf char
	  (if *getchar-ungetchar-buffer*
	      (pop *getchar-ungetchar-buffer*)
	      (read-char stream nil :eof)))
    (if (and (characterp char) (char= char #\\))
	(let ((temp (if *getchar-ungetchar-buffer*
			(pop *getchar-ungetchar-buffer*)
			(read-char stream))))
	  (when (char/= temp #\newline)
	    (ungetchar temp)
	    (return char)))
	(return char))))


;;; STORE-FIELD used in PARSE-FIELDS.
;;;
(defmacro store-field (cl-name value)
  (let ((name (gensym)))
    `(let ((,name ,cl-name))
       ; Allow multiple fields, for :similar-terminal.
       ;(unless (cdr (assoc ,name termcap :test #'eq))
	 (push (cons ,name ,value) termcap)
       ;)
       )))

) ;eval-when

;(defvar *out* t)

;;; PARSE-FIELDS parses a termcap entry.  We start out in the state get-name.
;;; Each name is looked up in *known-termcaps*, and if it is of interest, then
;;; we dispatch to a state to pick up the value of the field; otherwise, eat
;;; the rest of the field to get to the next name.  The name could be present
;;; simply to have the capability negated before the entry indirects to a
;;; similar terminal's capabilities, in which case it is followed by an #\@.
;;; Negated fields are stored with the value :negated since we only store a
;;; field if it does not already have a value -- this is the intent of the
;;; sequencing built into the termcap file.  When we are done, we see if there
;;; is a similar terminal to be parsed, and when we are really done, we replace
;;; all the :negated's with nil's.
;;;
(defun parse-fields (stream &optional other-stream) ; FIX explain other-stream
  (prog ((termcap-name (make-string 2))
	 (termcap ())
	 char termcap-def)
  GET-NAME
    ;;
    ;; This state expects char to be a #\:.
    (case (getchar)
      ((#\space #\tab)
       (go GET-NAME))
      (#\:
       ;; This is an empty field.
       (go GET-NAME))
      ((#\newline :eof)
       (go MAYBE-DONE))
      (t
       (setf (schar termcap-name 0) char)))
    (setf (schar termcap-name 1) (getchar))
    (setf termcap-def (termcap-def termcap-name))
    (or termcap-def (go EAT-FIELD))
    ;(format *out* "def: ~A~%" termcap-def)
    (when (char= (getchar) #\@)
      ;; Negation of a capability to be inherited from a similar terminal.
      (store-field (termcap-def-cl-name termcap-def) :negated)
      (go EAT-FIELD))
    (case (termcap-def-type termcap-def)
      (:number (go NUMBER))
      (:boolean (go BOOLEAN))
      (:string (go STRING)))
  NUMBER
    (unless (char= char #\#)
      (error "Bad termcap format -- number field '#' missing."))
    (let ((number 0)
	  digit)
      (loop
       (setf digit (digit-char-p (getchar)))
       (if digit
	   (setf number (+ digit (* number 10)))
	   (if (char= char #\:)
	       (return)
	       (error "Bad termcap format -- number field not : terminated."))))
      (store-field (termcap-def-cl-name termcap-def) number)
      (go GET-NAME))
  BOOLEAN
    (store-field (termcap-def-cl-name termcap-def) t)
    (if (char= char #\:)
	(go GET-NAME)
	(error "Bad termcap format -- boolean field not : terminated."))
  STRING
    (unless (char= char #\=)
      (error "Bad termcap format -- string field '=' missing."))
    ;;
    ;; Eat up any cost of the capability.
    (when (digit-char-p (getchar))
      (let ((dotp nil))
	(loop
	 (case (getchar)
	   ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	   (#\.
	    (when dotp (return))
	    (setf dotp t))
	   (t (when (char= char #\*) (getchar)) ; '*' means a per line cost
	      (return))))))
    ;;
    ;; Collect the characters.
    (let ((normal-string-p (not (eq (termcap-def-cl-name termcap-def)
				    :cursor-motion)))
	  xp cm-info)
      (init-termcap-string-buffer)
      (loop
	;(format *out* "char: ~A~%" char)
       (case (setf char (get-termcap-string-char stream char))
	 (#\%
	  (if normal-string-p
	      (store-char #\%)
	      (case (getchar)
		(#\% (store-char #\%))
		((#\d #\2 #\3)
		 (push (if (char= char #\d) 0 (digit-char-p char))
		       cm-info)
		 (push (if xp :y-pad :x-pad) cm-info)
		 (push (termcap-string-buffer-string) cm-info)
		 (push (if xp :string2 :string1) cm-info)
		 (init-termcap-string-buffer)
		 (setf xp t))
		(#\.
		 (push (termcap-string-buffer-string) cm-info)
		 (push (if xp :string2 :string1) cm-info)
		 (init-termcap-string-buffer)
		 (setf xp t))
		(#\+
		 (push (termcap-string-buffer-string) cm-info)
		 (push (if xp :string2 :string1) cm-info)
		 (push (get-termcap-string-char stream (getchar)) cm-info)
		 (push (if xp :y-add-char :x-add-char) cm-info)
		 (init-termcap-string-buffer)
		 (setf xp t))
		(#\>
		 (push (get-termcap-string-char stream (getchar)) cm-info)
		 (push (if xp :y-condx-char :x-condx-char) cm-info)
		 (push (get-termcap-string-char stream (getchar)) cm-info)
		 (push (if xp :y-condx-add-char :x-condx-add-char) cm-info))
		(#\r
		 (push t cm-info)
		 (push :reversep cm-info))
		(#\i
		 (push t cm-info)
		 (push :one-origin cm-info)))))
	 (#\:
	  (store-field (termcap-def-cl-name termcap-def)
		       (cond (normal-string-p (termcap-string-buffer-string))
			     (t (push (termcap-string-buffer-string) cm-info)
				(cons :string3 cm-info))))
	  ;(format *out* "at string stroe: ~A~%" termcap)
	  (return))
	 (t (store-char char)))
       (getchar))
      (go GET-NAME))
  EAT-FIELD
    (loop (when (char= (getchar) #\:) (return)))
    (go GET-NAME)
  MAYBE-DONE
    ;(format *out* "maybe-done: ~A~%" termcap)
    (let* ((similar-terminal (assoc :similar-terminal termcap :test #'eq))
	   (name (cdr similar-terminal)))
      (when name
	(setf (car similar-terminal) :was-similar-terminal)
	(if other-stream
	    (progn
	      (setq stream other-stream)
	      (file-position stream :start)
	      (if (find-termcap-entry name stream)
		  (go GET-NAME)
		  (error "Unknown similar terminal name (other) -- ~S."
			 name)))
	    (progn
	      (file-position stream :start)
	      (if (find-termcap-entry name stream)
		  (go GET-NAME)
		  (error "Unknown similar terminal name -- ~S." name))))))
    (dolist (ele termcap)
      (when (eq (cdr ele) :negated)
	(setf (cdr ele) nil)))
    (return termcap)))

;;; GET-TERMCAP-STRING-CHAR -- Internal.
;;;
;;; This parses/lexes an ASCII character out of the termcap file and
;;; converts it into the appropriate Lisp character.  This is a Lisp
;;; character with the same CHAR-CODE code as the ASCII code, so writing
;;; the character to the tty will have the desired effect.  If this
;;; function needs to look ahead to determine any characters, it unreads
;;; the character.
;;;
(defun get-termcap-string-char (stream char)
  (case char
    (#\\
     (case (getchar)
       (#\E (code-char 27))
       (#\n (code-char 10))
       (#\r (code-char 13))
       (#\t (code-char 9))
       (#\b (code-char 8))
       (#\f (code-char 12))
       (#\^ #\^)
       (#\\ #\\)
       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	(let ((result 0)
	      (digit (digit-char-p char)))
	  (loop (setf result (+ digit (* 8 result)))
	    (unless (setf digit (digit-char-p (getchar)))
	      (ungetchar char)
	      (return (code-char (ldb (byte 7 0) result)))))))
       (t (error "Bad termcap format -- unknown backslash character."))))
    (#\^
     (code-char (- (char-code (char-upcase (getchar))) 64)))
    (t char)))


;;;; Initialization file string.

(defun get-init-file-string (f)
  (unless (probe-file f)
    (error "File containing terminal initialization string does not exist -- ~S."
	   f))
  (with-open-file (s f)
    (let* ((len (file-length s))
	   (string (make-string len)))
      (dotimes (i len string)
	(setf (schar string i) (read-char s))))))
