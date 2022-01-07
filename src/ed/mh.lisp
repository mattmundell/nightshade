;;; A mailer interface to the mail handler in the MH package.

(in-package "ED")

#[ Mail

[ Introduction to Editor Mail                        ]
[ Setting up Mail                                    ]
[ Profile Components and Customized Files            ]
[ Introduction to Commands and Variables             ]
[ Scanning and Picking Messages                      ]
[ Reading New Mail                                   ]
[ Reading Mail Messages                              ]
[ Sending Messages                                   ]
[ Convenience Commands for Message and Draft Buffers ]
[ Deleting Messages                                  ]
[ Folder Operations                                  ]
[ Refiling Messages                                  ]
[ Marking Messages                                   ]
[ Terminating Headers Buffers                        ]
[ Miscellaneous Commands                             ]
[ Styles of Usage                                    ]
[ Mail Bindings Wallchart                            ]
]#

#[ Introduction to Editor Mail

The editor provides an electronic mail handling facility via the Nightshade
mail handler library.  The library is based on the Rand MH Message Handling
System.

The editor mail interface provides a means for generating summary lines for
messages and displaying these headers in a `Headers' buffer.  The current
message is indicated by the position of the point in the `Headers' buffer.
The `Headers' mode commands modify the current message.  The commands
`Message Headers' and `Incorporate and Read New Mail' both generate
`Headers' buffers.  Multiple headers buffers may exist at the same time.

Reading a message places its text in a `Message' buffer.  In a manner
similar to a `Headers' buffer, the `Message' mode commands allow
modification of that message.  Most `Headers' buffer commands behave the
same in a `Message' buffer.  For example, the `Reply to Message' command
has the same effect in both `Headers' mode and `Message' mode.  It creates
a `Draft' buffer as a reply to the message, and makes it the current buffer
for editing.

The `Send Message' command originates outgoing mail.  It generates a new
message in a `Draft' buffer for editing.  Each `Draft' buffer has an
associated pathname.  Saving the buffer saves the message to the associated
file.  Invoking `Send Message' or one of the reply commands in a `Headers'
or `Message' buffer associates the `Draft' buffer with a `Message' buffer.
This allows the `Goto Message Buffer' command in the `Draft' buffer to
easily refer to the original message.  The `Deliver Message' delivers the
message when in the `Draft' buffer.  Alternatively, keeping the draft
allows further editing and delivery later, even across editing sessions.
Invoking `Deliver Message' in any other type of buffer causes it to prompt
for a draft message ID, allowing retrieval of previously saved messages.

The editor mail interface provides a mechanism for virtual message
deletion.  That is, the `Delete Message' command flags the message for
future deletion instead of deleting the message immediately.  This enables
the `Undelete Message' command, which can cancel the deletion of the
messages later.  The `Expunge Messages' command actually removes the
flagged messages, moving them to the "trash" folder.  Commands that read
messages by sequencing through a `Headers' buffer typically skip over those
flagged for deletion, which makes for more fluid reading when some of the
messages are flagged.

After handling messages in a `Headers' buffer, there may be messages
flagged for deletion and possibly multiple `Message' buffers lying around.
There is a variety of commands that help terminate a mail session.
`Expunge Messages' will flush the messages to be deleted, leaving the
buffer in an updated state.  `Delete Headers Buffer and Message Buffers'
will delete the `Headers' buffer and its corresponding `Message'
buffers.  `Quit Headers' is a combination of these two commands in that it
first expunges messages and then deletes all the appropriate buffers.

Operating on messages represented in a `Headers' buffer is the nominal mode
of interaction.  There are also commands that prompt for a folder, an MH
message specification (for example, "1 3 6 last", "1-3 5 6", "all",
"unseen"), and possibly a pick expression.  Pick expressions allow messages
to be selected based on header field pattern matching, body text searching,
and date comparisons; these can be specified using either a Unix
shell-like/switch notation or a Lisp syntax, according to one's preference.
See section [ Scanning ] for more details.

A mail-drop is a file where a Unix-based mail system stores all messages a
user receives.  The user's mail handling program then fetches these from the
mail-drop, allowing the user to operate on them.
]#

#[ Setting up Mail

mail drops
rest auto   path :Mail/
]#

#[ Profile Components and Customized Files

[ Profile Components ]
[ Components Files   ]
]#

#[ Profile Components

The following are short descriptions about profile components that are either
necessary to using the editor's interface to MH or convenient for using MH in
general:

  Path:
     This specifies the user's mail directory.  It can be either a full pathname or
     a pathname relative to the user's home directory.  This component is
     necessary for using MH.

  MailDrop:
     This is used to specify one's remote mail-drop.  It is necessary for
     the editor only when using a mail-drop other than "/usr/spool/mail/<user>" on
     the local machine.

  Folder-Protect:, Msg-Protect:
     These are set to 700 and 600 respectively to keep others from reading one's
     mail.  At one time the default values were set for public visibility of mail
     folders.  Though this is no longer true, these can be set for certainty.  The
     700 protection allows only user read, write, and execute (list access for
     directories), and 600 allows only user read and write.  These are not necessary
     for either MH or the the editor interface.

  Unseen-Sequence:
     When mail is incorporated, new messages are added to this sequence, and as
     these messages are read they are removed from it.  This allows the user at any
     time to invoke an MH program on all the unseen messges of a folder easily.  An
     example definition is:

         Unseen-Sequence: unseen

     Specifying an unseen-sequence is necessary to use the editor's
     interface to MH.

  Alternate-Mailboxes:
     This is not necessary for either MH or the the editor interface.  This
     component tells MH which addresses that it should recognize as the user.  This
     is used for scan output formatting when the mail was sent by the user.  It
     is also used by repl when it sets up headers to know who the user is for
     inclusion or exclusion from cc: lists.  This is case sensitive and takes
     wildcards.  One example is:

         Alternate-Mailboxes: *FRED*, *Fred*, *fred*

  Draft-Folder:
     This makes multiple draft creation possible and trivial to use.  Just supply a
     folder name (for example, "drafts").  Specifying a draft-folder is
     necessary to use the editor's interface to MH.

  repl: -cc all -nocc me -fcc out-copy
     This tells the repl utility to include everyone but the user in the
     cc: list when replying to mail.  It also makes repl keep an copy of the
     message the user sends.  This is mentioned because one probably wants to reply
     to everyone receiving a piece of mail except oneself.  Unlike other utilities
     that send mail, repl stores personal copies of outgoing mail based on a
     command line switch.  Other MH utilities use different mechanisms.  This line
     is not necessary to use either MH or the the editor interface.

  rmmproc: /usr/cs/bin/rm
     This is not necessary to use the editor's interface to MH, but due to
     the editor's virtual message deletion feature, this causes messages to be deleted
     from folder directories in a cleaner fashion when they actually get removed.
     Note that setting this makes rmm more treacherous if used in the Unix
     shell.
]#

#[ Components Files

Components files are templates for outgoing mail header fields that specify
position and sometimes values for specified fields.  Example files are shown
for each one discussed here.  These should exist in the user's mail directory.

For originating mail there is a components file named "components", and it
is used by the MH utility comp.  An example follows:

   To:
   cc:
   fcc: out-copy
   Subject:
   --------

This example file differs from the default by including the fcc: line.
This causes MH to keep a copy of the outgoing draft message.  Also, though it
isn't visible here, the To:, cc:, and Subject: lines have a space
at the end.

The "forwcomps" components file is a template for the header fields of any
forwarded message.  Though it may be different, our example is the same as the
previous one.  These are distinct files for MH's purposes, and it is more
flexible since the user might not want to keep copies of forwarded messages.

The "replcomps" components file is a template for the header fields of any
draft message composed when replying to a message.  An example
follows:

   %(lit)%(formataddr %<{reply-to}%|%<{from}%|%{sender}%>%>)\
   %<(nonnull)%(void(width))%(putaddr To: )\n%>\
   %(lit)%(formataddr{to})%(formataddr{cc})%(formataddr(me))\
   %(formataddr{resent-to})\
   %<(nonnull)%(void(width))%(putaddr cc: )\n%>\
   %<{fcc}Fcc: %{fcc}\n%>\
   %<{subject}Subject: Re: %{subject}\n%>\
   %<{date}In-reply-to: Your message of \
   %<(nodate{date})%{date}%|%(tws{date})%>.%<{message-id}
		%{message-id}%>\n%>\
   --------

This example file differs from the default by including the resent-to:
field (in addition to the to: and cc: fields) of the message being
replied to in the cc: field of the draft.  This is necessary for replying
to all recipients of a distributed message.  Keeping a copy of the outgoing
draft message works a little differently with reply components.  MH expects a
switch which the user can put in his profile (see section [Profile] of this
chapter), and using the MH formatting language, this file tests for the
fcc value as does the standard file.
]#

#[ Scanning and Picking Messages

As pointed out in the introduction of this chapter, users typically generate
headers or scan listings of messages with `Message Headers', using
commands that operate on the messages represented by the headers.  `Pick
Headers' (bound to h in `Headers' mode) can be used to narrow down (or
further select over) the headers in the buffer.

A pick expression may be entered using either a Lisp syntax or a Unix
shell-like/switch notation as described in the MH documentation.  The Lisp
syntax is as follows:



   <exp>       ::=  {(not <exp>) | (and <exp>*) | (or <exp>*)
		    | (cc <pattern>) | (date <pattern>)
		    | (from <pattern>) | (search <pattern>)
		    | (subject <pattern>) | (to <pattern>)
		    | (-- <component> <pattern>)
		    | (before <date>) | (after <date>)
		    | (datefield <field>)}


   <pattern>   ::=  {<string> | <symbol>}


   <component> ::=  {<string> | <symbol>}


   <date>      ::=  {<string> | <symbol> | <number>}


   <field>     ::=  <string>


Anywhere the user enters a <symbol>, its symbol name is used as a string.
Since the editor reads the expression without evaluating it, single quotes
("'") are unnecessary.  From the MH documentation,


  -    A <pattern> is a Unix ed regular expression.  When using a string to
        input these, remember that \ is an escape character in Common Lisp.

  -    A <component> is a header field name (for example, reply-to or
        resent-to).

  -    A <date> is an 822-style specification, a day of the week,
        "today", "yesterday", "tomorrow", or a number indicating n
        days ago.  The 822 standard is basically:


   dd mmm yy hh:mm:ss zzz




        which is a two digit day, three letter month (first letter capitalized), two
        digit year, two digit hour (00 through 23), two digit minute, two
        digit second (this is optional), and a three letter zone (all capitalized).
        For
        example:


   21 Mar 88 16:00 EST





        A <field> is an alternate Date: field to use with (before
   <date>) and (after <date>) such as BB-Posted: or
        Delivery-Date:.

  -    Using (before <date>) and (after <date>) causes date field parsing,
        while (date <pattern>) does string pattern matching.

Since a <pattern> may be a symbol or string, it should be noted that the
symbol name is probably all uppercase characters, and MH will match these
only against upper case.  MH will match lowercase characters against lower
and upper case.  Some examples are:

   ;;; All messages to Gripe.
   (to "gripe")

   ;;; All messages to Gripe or about Nightshade.
   (or (to "gripe") (subject "nightshade"))

   ;;; All messages to Gripe with "Nightshade" in the body.
   (and (to "gripe") (search "nightshade"))


Matching of <component> fields is case sensitive, so this example will
pick over all messages that have been replied to.

(or (-- "replied" "") (-- "Replied" ""))

{command:Message Headers}
{command:Pick Headers}
{command:Headers Help}
]#


;;;; General stuff.

(defevar "Trash Folder"
  "Name of trash folder, with leading +."
  :value "+trash")

(defvar *new-mail-buffer* ())

(defvar *mh-utility-bit-bucket* (make-broadcast-stream))

(defattribute "Digit"
  "This is just a (mod 2) attribute for base 10 digit characters.")
;;;
(dotimes (i 10)
  (setf (character-attribute :digit (digit-char i)) 1))


(defvar *boundary-pattern*
  (new-search-pattern :string-sensitive :forward "boundary=\""))
(defvar *two-nl-pattern*
  (new-search-pattern :string-sensitive :forward "

"))

(defmacro do-headers-buffers ((buffer-var folder &optional hinfo-var)
			      &rest forms)
  "The Forms are evaluated with Buffer-Var bound to each buffer containing
   headers lines for folder.  Optionally Hinfo-Var is bound to the
   headers-information structure."
  (let ((folder-var (gensym))
	(hinfo (gensym)))
    `(let ((,folder-var ,folder))
       (declare (simple-string ,folder-var))
       (dolist (,buffer-var *buffer-list*)
	 (when (editor-bound-p 'headers-information :buffer ,buffer-var)
	   (let ((,hinfo (variable-value 'headers-information
					 :buffer ,buffer-var)))
	     (when (string= (the simple-string (headers-info-folder ,hinfo))
			    ,folder-var)
	       ,@(if hinfo-var
		     `((let ((,hinfo-var ,hinfo))
			 ,@forms))
		     forms))))))))

(defmacro do-headers-lines ((hbuffer-region &key line-var mark-var) &rest forms)
  "Forms are evaluated for each non-blank line.  When supplied Line-Var and
   Mark-Var are to the line and a :left-inserting mark at the beginning of the
   line.  This works with DELETE-HEADERS-BUFFER-LINE, but one should be careful
   using this to modify the hbuffer."
  (let ((line-var (or line-var (gensym)))
	(mark-var (or mark-var (gensym)))
	(id (gensym)))
    `(with-mark ((,mark-var (region-start ,hbuffer-region) :left-inserting))
       (loop
	 (let* ((,line-var (mark-line ,mark-var))
		(,id (line-message-id ,line-var)))
	   (or (blank-line-p ,line-var)
	       ,@forms)
	   (if (or (not (eq ,line-var (mark-line ,mark-var)))
		   (string/= ,id (line-message-id ,line-var)))
	       (line-start ,mark-var)
	       (or (line-offset ,mark-var 1 0) (return))))))))

(defmacro with-headers-mark ((mark-var hbuffer msg) &rest forms)
  "Forms are executed with Mark-Var bound to a :left-inserting mark at the
   beginning of the headers line representing msg.  If no such line exists,
   no execution occurs."
  (let ((line (gensym)))
    `(do-headers-lines ((buffer-region ,hbuffer) :line-var ,line :mark-var ,mark-var)
       (when (string= (the simple-string (line-message-id ,line))
		      (the simple-string ,msg))
	 ,@forms
	 (return)))))


;;;; Headers Mode.

(declaim (special *mode-highlighters*))

(defun setup-headers-mode (buffer)
  (highlight-visible-headers-buffer buffer)
  (pushnew '("Headers" t highlight-visible-headers-buffer)
	   *mode-highlighters*))

(defmode "Headers" :major-p t
  :setup-function #'setup-headers-mode)

(defevar "Headers Information"
  "This holds the information about the current headers buffer.")

(defstruct (headers-info (:print-function print-headers-info))
  buffer	 ; Buffer for these headers.
  folder	 ; String name of folder with leading "+".
  msg-seq	 ; MH sequence of messages in buffer.
  msg-strings	 ; List of strings representing msg-seq.
  other-msg-bufs ; List of message buffers referencing this headers buffer.
  draft-bufs	 ; List of draft buffers referencing this headers buffer.
  pick           ; Pick expression that generated the headers.
  msgs           ; Messages picked from.
  msg-buffer)

(defun print-headers-info (obj str n)
  (declare (ignore n))
  (format str "#<Headers Info ~S>" (headers-info-folder obj)))

(defmacro line-message-deleted (line)
  `(getf (line-plist ,line) 'mh-msg-deleted))

(defmacro line-message-id (line)
  `(getf (line-plist ,line) 'mh-msg-id))

(defun headers-current-message (hinfo)
  (let* ((point (buffer-point (headers-info-buffer hinfo)))
	 (line (mark-line point)))
    (fi (blank-line-p line)
	(values (line-message-id line)
		(copy-mark point)))))

(defcommand "Message Headers" (p)
  "Prompt for a folder and messages, and display the message headers in a
   Headers in the current window.  With an argument, also prompt for a pick
   expression."
  (let ((folder (prompt-for-folder)))
    (new-message-headers
     folder
     (prompt-for-message :prompt (if p
				     "MH messages to pick from: "
				     "MH messages: ")
			 :folder folder
			 :messages "all")
			 p)))

(defcommand "All Message Headers" ()
  "Prompt for a folder, displaying headers of all messages in a buffer in
   the current window."
  (new-message-headers (prompt-for-folder) (breakup-message-spec "all")))

(defcommand "Last Message Headers" ()
  "Prompt for a folder, displaying enough of the last headers from the
   folder to fill the current window."
  (new-message-headers (prompt-for-folder) '("highest")
		       () (1+ (- (window-height (current-window))))))

(defcommand "Refresh Headers" ()
  "If the current buffer is a headers buffer then refresh the headers."
  (let ((hinfo (value headers-information)))
    (when hinfo
      (let ((current-id (line-message-id (current-line))))
	(new-message-headers (headers-info-folder hinfo)
			     (headers-info-msgs hinfo)
			     () ; pickp
			     (1+ (- (window-height (current-window))))
			     (headers-info-pick hinfo))
	(catch 'moved
	  (if current-id
	      ;; Try move point to the message it was on before.
	      (while ((line (mark-line (buffer-start-mark (current-buffer)))
			    (line-next line)))
		     (line)
		(when (equal current-id (line-message-id line))
		  (move-mark (current-point) (mark line 0))
		  (throw 'moved ()))))
	  ;; Move to the first new message.
	  (let ((in (mh:sequence-list (headers-info-folder hinfo)
				      (mh:profile-component
				       "unseen-sequence"))))
	    (while ((line (mark-line (current-point)) (line-next line)))
		   (line)
	      (when (mh:sequence-member-p (line-message-id line) in)
		(move-mark (current-point) (mark line 0))
		(return))))))
      (message "Messages refreshed."))))

(defcommand "Refresh All Headers" ()
  "If in a headers buffer then refresh the buffer, showing all headers."
  (let ((hinfo (value headers-information)))
    (when hinfo
      (new-message-headers (headers-info-folder hinfo) '("all"))
      (message "Messages refreshed (all)."))))

(defcommand "Sort Headers" ()
  "Sort the folder associated with the headers, date-major subject-minor."
  (let ((hinfo (value headers-information)))
    (when hinfo
      (mh:sort-folder (headers-info-folder hinfo))
      (message "Messages sorted.")
      (refresh-headers-command))))

(defcommand "Headers Mark as Read" ()
  "Mark the current message as read."
  (flet ((mark-as-read (folder cur-msg)
	   (let ((in-seq (mh:profile-component "unseen-sequence")))
	     (when (and in-seq
			(prompt-for-y-or-n
			 :prompt (format () "Mark ~A as read? " cur-msg)
			 :default t
			 :default-string "Y")
			(mh:sequence-member-p
			 cur-msg
			 (mh:sequence-list folder in-seq)))
	       (mh:mark-message folder cur-msg in-seq :delete)
	       (folders-incr folder -1)
	       ;; FIX must refresh "in" msgs if showing "in" msgs
	       (refresh-headers-command)))))
    (let ((hinfo (value headers-information)))
      (if hinfo
	  (multiple-value-bind (cur-msg)
			       (headers-current-message hinfo)
	    (or cur-msg (editor-error "Point must be on a header line."))
	    (mark-as-read (headers-info-folder hinfo) cur-msg))
	  ;; Daft.
	  (let ((minfo (value message-information)))
	    (or (message-info-folder minfo)
		(editor-error "Attempt to mark a temporary message as read."))
	    (or minfo (editor-error "Must be in a header or message buffer."))
	    (mark-as-read (message-info-folder minfo)
			  (message-info-msgs minfo)))))))

(defcommand "Pick Headers" ()
  "In a `Headers' buffer narrow the selection of headers.  Prompt for a
   pick expression and pick over the headers in the current buffer.  If an
   empty expression is supplied display all the headers for that folder."
  (pick-message-headers
   (or (value headers-information)
       (editor-error "Pick Headers only works in a headers buffer."))))

;;; PICK-MESSAGE-HEADERS picks messages from info's messages based on an
;;; expression provided by the user.  If the expression is empty, we do
;;; headers on all the messages in folder.  The buffer's name is changed to
;;; reflect the messages picked over and the expression used.
;;;
(defun pick-message-headers (hinfo)
  (let ((folder (headers-info-folder hinfo))
	(msgs (headers-info-msg-strings hinfo)))
    (let* ((pick (prompt-for-pick-expression))
	   (hbuffer (headers-info-buffer hinfo))
	   (new-mail-buf-p (eq hbuffer *new-mail-buffer*))
	   (region (cond (pick
			  (message-headers-to-region
			   folder (mh:pick-messages folder msgs pick)))
			 (new-mail-buf-p
			  (maybe-get-new-mail-msg-hdrs folder))
			 (t (message-headers-to-region folder
						       (list "all"))))))
      (with-writable-buffer (hbuffer)
	(revamp-headers-buffer hbuffer hinfo)
	(when region (insert-message-headers hbuffer hinfo region)))
      (setf (buffer-modified hbuffer) ())
      (buffer-start (buffer-point hbuffer))
      (setf (buffer-name hbuffer)
	    (cond (pick (format () "Headers ~A ~A ~A" folder msgs pick))
		  (new-mail-buf-p (format () "Unseen Headers ~A" folder))
		  (t (format () "Headers ~A (all)" folder)))))))

;;; NEW-MESSAGE-HEADERS picks over msgs if pickp is true, or it just scans
;;; msgs.  It is important to pick and get the message headers region
;;; before making the buffer and info structures since
;;; MESSAGE-HEADERS-TO-REGION will call EDITOR-ERROR if they fail.  The
;;; buffer name is chosen based on folder, msgs, and an optional pick
;;; expression.
;;;
(defun new-message-headers (folder msgs &optional pickp range pick-exp)
  (let* ((pick-exp (or pick-exp
		       (if pickp (prompt-for-pick-expression))))
	 (pick (if pick-exp (mh:pick-messages folder msgs pick-exp)))
	 (region (message-headers-to-region folder (or pick msgs)
					    :range range))
	 (hbuffer (maybe-make-mh-buffer (format () "Headers ~A ~A~:[~; ~S~]"
						folder msgs pick pick-exp)
					:headers))
	 (hinfo (make-headers-info :buffer hbuffer :folder folder
				   :pick pick-exp
				   :msgs msgs)))
    (insert-message-headers hbuffer hinfo region)
    (defevar "Headers Information"
      "This holds the information about the current headers buffer."
      :value hinfo :buffer hbuffer)
    (setf (buffer-modified hbuffer) ())
    (setf (buffer-writable hbuffer) ())
    (buffer-start (buffer-point hbuffer))
    (setf (buffer-pathname hbuffer) (mh:folder-pathname folder))
    (change-to-buffer hbuffer)))

(defun extend-message-headers (folder hbuffer direction &key range)
  "Extend the headers of $folder in $hbuffer by $range messages in
   $direction.

   $direction can be :backward or :forward."
  (ecase direction
    (:backward
     (let* ((point (buffer-point hbuffer))
	    (mark (buffer-start (copy-mark point :left-inserting)))
	    (hinfo (variable-value 'headers-information :buffer hbuffer))
	    (msg (line-message-id (mark-line mark)))
	    (region (message-headers-to-region folder (list msg)
					       :range range))
	    (seq (set-message-headers-ids region
					  :return-seq
					  (headers-info-msg-seq hinfo))))
       ;; Set the message ID on the line the region is to replace.
       (setf (line-message-id (mark-line mark))
	     (line-message-id (mark-line (region-start region))))
       (ninsert-region mark region)
       ;; Release the line following the inserted region, as it is
       ;; repeated in the region.
       (line-start mark)
       (let ((mark2 (copy-mark mark)))
	 (line-offset mark2 1 0)
	 (delete-region (region mark mark2))
	 (let* ((line-str (line-string (mark-line mark2)))
		(num (parse-integer line-str :junk-allowed t)))
	   (declare (simple-string line-str))
	   (setf (line-message-id (mark-line mark2)) (string num))))
       (delete-mark mark)
       (setf (headers-info-msg-seq hinfo) seq)
       (setf (headers-info-msg-strings hinfo) (mh:sequence-strings seq))
       (when (value virtual-message-deletion)
	 (note-deleted-headers region
			       (mh:sequence-list (headers-info-folder hinfo)
						 "edtrash")))))))

;; FIX probably better way to do this
(defevar "Message Headers Fill Column"
  "The width of the text in a Message Headers buffer.  If () then \"Fill
   Column\" will be used instead."
  :value ())

;;; MESSAGE-HEADERS-TO-REGION uses the MH interface to output headers into
;;; buffer for folder and msgs.
;;;
;;; (value fill-column) should really be done as if the buffer were
;;; current, but the editor doesn't let you do this without the buffer
;;; being current.
;;;
(defun message-headers-to-region (folder msgs &key width range)
  (let ((region (make-empty-region)))
    (with-output-to-mark (stream (region-end region) :full)
      (mh:summarize-messages folder msgs stream
			     :width (or width
					(value message-headers-fill-column)
					(value fill-column))
			     :range (or range
					#|
					;; FIX first fi a hack to stop
					;; range if msgs is a sequence
					(fi (and (listp msgs)
						 (stringp (car msgs))
						 (fi (string= (car msgs) "all")))
					    (- (if (or (eq msgs :all)
						       (eq (car msgs) :all)
						       (and (stringp msgs)
							    (string= msgs "all"))
						       (and (stringp (car msgs))
							    (string= (car msgs)
								     "all")))
						   (window-height (current-window))
						   (1+ (/ (window-height (current-window))
							  2)))))
					|#
					)))
    region))

(defun insert-message-headers (hbuffer hinfo region)
  (ninsert-region (buffer-point hbuffer) region)
  (let ((seq (set-message-headers-ids (buffer-region hbuffer) :return-seq)))
    (setf (headers-info-msg-seq hinfo) seq)
    (setf (headers-info-msg-strings hinfo) (mh:sequence-strings seq)))
  (when (value virtual-message-deletion)
    (note-deleted-headers (buffer-region hbuffer)
			  (mh:sequence-list (headers-info-folder hinfo)
					    "edtrash"))))

(defun set-message-headers-ids (hbuffer-region &optional return-seq seq)
  (let ((msgs seq))
    (do-headers-lines (hbuffer-region :line-var line)
      (let* ((line-str (line-string line))
	     (num (parse-integer line-str :junk-allowed t)))
	(declare (simple-string line-str))
	(or num
	    ;; FIX
	    (editor-error "MH scan lines must contain the message id as the ~
			   first thing on the line for the editor interface."))
	(setf (line-message-id line) (string num))
	(if return-seq (setf msgs (mh:sequence-insert num msgs)))))
    msgs))

(defun note-deleted-headers (hbuffer-region deleted-seq)
  (when deleted-seq
    (do-headers-lines (hbuffer-region :line-var line :mark-var hmark)
      (if (mh:sequence-member-p (line-message-id line) deleted-seq)
	  (note-deleted-message-at-mark hmark)
	  (setf (line-message-deleted line) nil)))))

#[ Terminating Headers Buffers

The user never actually exits the mailer.  He can leave mail buffers lying
around while conducting other editing tasks, selecting them and continuing his
mail handling whenever.  There still is a need for various methods of
terminating or cleaning up `Headers' buffers.  The two most useful commands
in this section are `Expunge Messages' and `Quit Headers'.

{evariable:Expunge Messages Confirm}
{evariable:Temporary Draft Folder}
{command:Expunge Messages}
{command:Quit Headers}
{command:Delete Headers Buffer and Message Buffers}
]#

(defcommand "Delete Headers Buffer and Message Buffers" (p buffer)
  "Delete a prompted `Headers' buffer and all associated `Message' buffers.
   Leave any associated `Draft' buffers in tact."
  (declare (ignore p))
  (let* ((default (cond ((value headers-information) (current-buffer))
			((value message-information) (value headers-buffer))))
	 (buffer (or buffer
		     (prompt-for-buffer :default default
					:default-string
					(if default (buffer-name default))))))
    (or (editor-bound-p 'headers-information :buffer buffer)
	(editor-error "Not a headers buffer -- ~A" (buffer-name buffer)))
    (let* ((hinfo (variable-value 'headers-information :buffer buffer))
	   ;; Copy list since buffer cleanup hook is destructive.
	   (other-bufs (copy-list (headers-info-other-msg-bufs hinfo)))
	   (msg-buf (headers-info-msg-buffer hinfo)))
      (when msg-buf (delete-buffer-if-possible msg-buf))
      (dolist (b other-bufs) (delete-buffer-if-possible b))
      (delete-buffer-if-possible (headers-info-buffer hinfo)))))

(defevar "Expunge Messages Confirm"
  "When set, `Expunge Messages' and `Quit Headers' ask for confirmation
   before expunging messages and packing the folder's message ID's."
  :value t)

(defevar "Temporary Draft Folder"
  "The folder name where MH fcc: messages are kept that are intended to be
   deleted and expunged whenever messages from any folder are expunged (for
   example, when `Expunge Messages' or `Quit Headers' are invoked).")

(defun delete-messages (folder messages &optional (verbose t))
  "Move the list of $messages from $folder to the trash folder or if
   $folder is the trash folder clear the $messages out the trash folder."
  (if (string= folder (value trash-folder))
      (progn
	(if verbose (message "Deleting messages ..."))
	(if (prompt-for-y-or-n
	     :prompt "Really delete files? "
	     :default t
	     :default-string "Y")
	    (mh:delete-messages folder messages)
	    (if verbose (message "   ...canceled."))))
      (let* ((in-seq (mh:sequence-list folder
				       (mh:profile-component
					"unseen-sequence")))
	     (ins (mh:pick-messages
		   folder messages
		   `(mh:sequence-member-p (car mh::*entry*) ',in-seq))))
	(if verbose (message "Moving messages to trash ..."))
	(mh:move-messages folder
			  (value trash-folder)
			  messages)
	(folders-incr folder (- (length ins))))))

;;; "Quit Headers" doesn't expunge or compact unless there is a deleted
;;; sequence.  This collapses other headers buffers into the same folder
;;; differently than "Expunge Messages" since the latter assumes there will
;;; always be one remaining headers buffer.  This command folds all headers
;;; buffers into the folder that are not the current buffer or the new mail
;;; buffer into one buffer.  When the current buffer is the new mail buffer
;;; we do not check for more unseen headers since we are about to delete
;;; the buffer anyway.  The other headers buffers must be deleted before
;;; making the new one due to aliasing the buffer structure and
;;; MAYBE-MAKE-MH-BUFFER.
;;;
(defcommand "Quit Headers" ()
  "Quit the current `Headers' buffer, possibly expunging deleted messages.

   When *Expunge Message Confirm* is set then confirm before expunging.  Kill
   the buffer and all associated message buffers.  When *Temporary Draft
   Folder* is bound to that folder instead."
  (let* ((hinfo (value headers-information))
	 (minfo (value message-information))
	 (hdrs-buf (cond (hinfo (current-buffer))
			 (minfo (value headers-buffer)))))
    (or hdrs-buf
	(editor-error "Must be in or associated with a headers buffer."))
    (let* ((folder (cond (hinfo (headers-info-folder hinfo))
			 (minfo (message-info-folder minfo))))
	   (deleted-seq (mh:sequence-list folder "edtrash")))
      (when (and deleted-seq
		 (or (not (value expunge-messages-confirm))
		     (prompt-for-y-or-n
		      :prompt (list "Expunge messages and pack folder ~A? "
				    folder)
		      :default t
		      :default-string "Y")))
	(delete-messages folder '("trash"))
	(let ((*standard-output* *mh-utility-bit-bucket*))
	  (message "Compacting folder ...")
	  (mh:pack-folder folder))
	(message "Maintaining consistency ...")
	(let (hbufs)
	  (declare (list hbufs))
	  (do-headers-buffers (b folder)
	    (or (eq b hdrs-buf) (eq b *new-mail-buffer*)
		(push b hbufs)))
	  (dolist (b hbufs)
	    (delete-headers-buffer-and-message-buffers-command nil b))
	  (when hbufs
	    (new-message-headers folder (list "all"))))
	(expunge-messages-fix-draft-buffers folder)
	(or (eq hdrs-buf *new-mail-buffer*)
	    (expunge-messages-fix-unseen-headers folder))
	(delete-and-expunge-temp-drafts)))
    (delete-headers-buffer-and-message-buffers-command nil hdrs-buf)))

;;; DELETE-AND-EXPUNGE-TEMP-DRAFTS deletes all the messages in the
;;; temporary draft folder if there is one defined.  Any headers buffers
;;; into this folder are deleted with their message buffers.  We have to
;;; create a list of buffers to delete since buffer deletion destructively
;;; modifies the same list DO-HEADERS-BUFFERS uses.  `delete-messages' is
;;; run with error reporting off, to prevent it from signalling an error.
;;; This function must return; for example, "Quit Headers" would not
;;; complete successfully if this ended up calling EDITOR-ERROR.
;;;
(defun delete-and-expunge-temp-drafts ()
  (let ((temp-draft-folder (value temporary-draft-folder)))
    (when temp-draft-folder
      (setf temp-draft-folder (mh:coerce-folder-name temp-draft-folder))
      (message "Deleting and expunging temporary drafts ...")
      ;; FIX move to trash?
      (when (mh:delete-messages temp-draft-folder '(:all))
	(let (hdrs)
	  (declare (list hdrs))
	  (do-headers-buffers (b temp-draft-folder)
	    (push b hdrs))
	  (dolist (b hdrs)
	    (delete-headers-buffer-and-message-buffers-command nil b)))))))

;; FIX add p as in Previous Line
;;
(defcommand "Previous Headers Line" ()
  "Move point to the previous header."
  (let ((hinfo (value headers-information))
	(window (current-window)))
    (or hinfo (editor-error "Must be in a headers buffer."))
    (if (eq (mark-line (current-point))
	    (mark-line (window-display-start window)))
	;; First line in window.  Check if enough lines above
	;; window.
	(let* ((half-window (1+ (/ (window-height window) 2)))
	       (required half-window))
	  (iterate iter ((line (mark-line (current-point))))
	    (when (and line (plusp required))
	      (decf required)
	      (iter (line-previous line))))
	  (if (zerop required)
	      ;; Enough lines.
	      (previous-line-command)
	      (multiple-value-bind (cur-msg cur-mark)
				   (headers-current-message hinfo)
		(declare (ignore cur-mark))
		(or cur-msg (editor-error "First line."))
		(if (mh:maybe-messages-before-p
		     (headers-info-folder hinfo)
		     cur-msg)
		    (with-writable-buffer ((current-buffer))
		      (message "Fetching more headers...")
		      (extend-message-headers (headers-info-folder hinfo)
					      (current-buffer)
					      :backward
					      :range (- half-window))
		      (previous-line-command))
		    (editor-error "First line.")))))
	(previous-line-command))))

(defcommand "Scroll Headers Window Up" ()
  "Move point to the previous header."
  (let* ((hinfo (or (value headers-information)
		    (editor-error "Must be in a headers buffer.")))
	 (window (current-window))
	 (range (window-height window))
	 (required range))
    ;; Check for enough lines above window.
    (iterate iter ((line (mark-line (window-display-start window))))
      (when (and line (plusp required))
	(decf required)
	(iter (line-previous line))))
    (if (plusp required)
	;; Too few lines in the buffer to scroll a full window.  This may
	;; mean that there are too few messages left at the beginning of
	;; the folder, so check if there are more messages before the first
	;; message in the buffer.
	(let ((first-msg (line-message-id
			  (mark-line (buffer-start-mark
				      (current-buffer))))))
	  (and first-msg
	       (mh:maybe-messages-before-p (headers-info-folder hinfo)
					   first-msg)
	       (with-writable-buffer ((current-buffer))
		 (message "Requesting backward...")
		 (extend-message-headers (headers-info-folder hinfo)
					 (current-buffer)
					 :backward
					 :range (- range))))))
    (scroll-window-up-command)))


#[ Reading Mail Messages

This section describes basic commands that show the current, next, and
previous messages, as well as a couple of advanced commands.  `Show
Message' (bound to Return in `Headers' mode) will display the message
represented by the summary line the the editor cursor is on.  Deleted
messages are considered special, and the more conveniently bound commands
for viewing the next and previous messages (`Next Undeleted Message' bound
to n and `Previous Undeleted Message' bound to p, both in `Headers' and
`Message' modes) will ignore them.  `Next Message' and `Previous Message'
(bound to M-n and M-p in `Headers' and `Message' modes) may be invoked if
reading a message is desired regardless of whether it has been deleted.

{command:Show Message}
{command:Next Message}
{command:Previous Message}
{command:Next Undeleted Message}
{command:Previous Undeleted Message}
{command:Scroll Message}
{evariable:Scroll Message Showing Next}
{command:Keep Message}
{command:Message Help}
]#


;;;; Message Mode.

(defmode "Message" :major-p t :setup-function 'setup-message-mode)

(declaim (special *mode-highlighters*))

(defun setup-message-mode (buffer)
  (highlight-visible-message-buffer buffer)
  (pushnew '("Message" t highlight-visible-message-buffer)
	   *mode-highlighters*))

(defevar "Message Information"
  "This holds the information about the current message buffer.")

(defstruct message/draft-info
  headers-mark)		; Mark pointing to a headers line in a headers buffer.

(defstruct (message-info (:include message/draft-info)
			 (:print-function print-message-info))
  folder	; String name of folder with leading "+".  () for temp msgs.
  msgs		; List of strings representing messages to be shown.
  attachments   ; TODO (in read-message) List of attachments for each of "msgs".
  draft-buf	; Possible draft buffer reference.
  keep)		; Whether message buffer may be re-used.

(defun print-message-info (obj str n)
  (declare (ignore n))
  (format str "#<Message Info ~S ~S>"
	  (message-info-folder obj) (message-info-msgs obj)))

;; FIX sort resulting headers
(defun filter-message-headers (mbuffer)
  "Filter out most headers from the message in Buffer.  Return the mime
   boundary string if there is one, else nil."
  (let* ((point (buffer-point mbuffer))
	 (end (copy-mark point))
	 mime-boundary mime-subtype)
    (when (find-pattern end *two-nl-pattern*)
      (mark-after end)
      (setf (variable-value 'message-headers :buffer mbuffer)
	    (copy-region (region point end)))
      (setf (variable-value 'message-headers-short :buffer mbuffer) nil)
      (let ((line (mark-line point)))
	;; FIX handle multi-line fields
	(loop for keep = () do
	  (if (mark= (mark line 0) end) (return))
	  (let* ((chars (line-string line))
		 (len (length chars)))
	    (if (and (>= len 13)
		     (string= chars "Content-Type:"
			      :end1 13 :end2 13))
		;; Content type field.
		(let* ((start (mark line 14))
		       (end (mark line 14))
		       type subtype)
		  ;; Parse types. ; parse mime type
		  (find-attribute end :word-delimiter)
		  (setq type (region-to-string (region start end)))
		  (mark-after end)
		  (move-mark start end)
		  (find-attribute end :word-delimiter)
		  (setq subtype (region-to-string (region start end)))
		  ;; Check for multipart boundary.
		  (when (and (string= (string-downcase type) "multipart")
			     (find-pattern start *boundary-pattern*))
		    (setq mime-subtype subtype)
		    (character-offset start 10)
		    (let ((end (copy-mark start)))
		      ;; FIX (find-character end #\")
		      (find-pattern end
				    (new-search-pattern :character
							:forward #\"))
		      (setq mime-boundary
			    (region-to-string (region start end))))))
		;; Other fields.
		(dolist (field
			 '("Subject:" "To:" "From:"
			   "Cc:" "Reply-To:" "Date:"
			   "Resent-To:" "Resent-From:"
			   "Resent-Cc:" "Resent-Date:"))
		  (let ((field-len (length field)))
		    (when (and (>= len (length field))
			       (string= chars field
					:end1 field-len :end2 field-len))
		      (setq keep t)
		      (return)))))
	    (if keep
		(progn
		  (setq line (line-next line))
; FIX loop over rest lines in multi-line field
; 		  (loop for chars = (line-string line)
; 		    for len = (length chars)
; 		    while (and line
; 			       (plusp len)
; 			       (member (char chars 0)
; 				       '(#\space #\newline)))
; 		    do
; 		    (delete-region (region (mark line 0)
; 					   (mark-after (mark line len)))))
		  )
		(let ((region (region (mark line 0)
				      (mark-after (mark line len)))))
		  (delete-region region)))))))
    (values mime-boundary (string-downcase mime-subtype))))

(defun get-mime-type (mark)
  "Return the MIME type and subtype at Mark."
  ; FIX test
  ; text/plain text/x-csrc text-eg/plain?
  (let ((start (copy-mark mark))
	(type))
    (when (find-attribute mark :word-delimiter)
      (setq type
	    (string-downcase (region-to-string (region start
						       mark))))
      (mark-after mark)
      (move-mark start mark)
      (when (find-attribute mark :whitespace)
	(values type (string-downcase (region-to-string
				       (region start mark))))))))

(defcommand "Next Message" ()
  "Show the next message.

   When in a message buffer, show the next message from the associated
   headers buffer in the current buffer.  If the `Message' buffer is
   associated with a `Draft' buffer, invoking this command breaks that
   association.  Using `Keep Message' preserves the current buffer and
   any association with a `Draft' buffer.

   When in a headers buffer, move point to the next message and show that
   message."
  (show-message-offset 1))

(defcommand "Previous Message" ()
  "Show the previous message.

   When in a message buffer, show the previous message from the associated
   headers buffer in the current buffer.  If the `Message' buffer is
   associated with a `Draft' buffer, invoking this command breaks that
   association.  Using `Keep Message' preserves the current buffer and any
   association with a `Draft' buffer.

   When in a headers buffer, move point to the previous message and show that
   message."
  (show-message-offset -1))

(defcommand "Next Undeleted Message" ()
  "Show the next undeleted message.

   When in a message buffer, show the next undeleted message from the
   associated headers buffer in the current buffer.  If the `Message'
   buffer is associated with a `Draft' buffer, invoking this command breaks
   that association.  Using `Keep Message' preserves the current buffer and
   any association with a `Draft' buffer.

   When in a headers buffer, move point to the next undeleted message and
   show that message."
  (show-message-offset 1 :undeleted))

(defcommand "Previous Undeleted Message" ()
  "Show the previous undeleted message.

   When in a message buffer, show the previous undeleted message from the
   associated headers buffer in the current buffer.  If the `Message'
   buffer is associated with a `Draft' buffer, invoking this command breaks
   that association.  Using `Keep Message' preserves the current buffer and
   any association with a `Draft' buffer.

   When in a headers buffer, move point to the previous undeleted message
   and show that message."
  (show-message-offset -1 :undeleted))

(defun show-message-offset (offset &optional undeleted)
  (let ((minfo (value message-information)))
    (cond
     ((not minfo)
      (let ((hinfo (value headers-information)))
	(or hinfo (editor-error "Not in a message or headers buffer."))
	(show-message-offset-hdrs-buf hinfo offset undeleted)))
     ((message-info-keep minfo)
      (let ((hbuf (value headers-buffer)))
	(or hbuf (editor-error "Not associated with a headers buffer."))
	(let ((hinfo (variable-value 'headers-information :buffer hbuf))
	      (point (buffer-point hbuf)))
	  (move-mark point (message-info-headers-mark minfo))
	  (show-message-offset-hdrs-buf hinfo offset undeleted))))
     (t
      (show-message-offset-msg-buf minfo offset undeleted)))))

(defun show-message-offset-hdrs-buf (hinfo offset undeleted)
  (or hinfo (editor-error "Not in a message or headers buffer."))
  (or (show-message-offset-mark (buffer-point (headers-info-buffer hinfo))
				offset undeleted)
      (editor-error "No ~:[previous~;next~] ~:[~;undeleted ~]message."
		    (plusp offset) undeleted))
  (show-headers-message hinfo))

(defun show-message-offset-msg-buf (minfo offset undeleted)
  (let ((msg-mark (message-info-headers-mark minfo)))
    (or msg-mark (editor-error "Not associated with a headers buffer."))
    (unless (show-message-offset-mark msg-mark offset undeleted)
      (let ((hbuf (value headers-buffer))
	    (mbuf (current-buffer)))
	(or (buffer-windows hbuf)
	    (progn
	      (setf (current-buffer) hbuf)
	      (setf (window-buffer (current-window)) hbuf)
	      (delete-buffer-if-possible mbuf))))
      (editor-error "~:[First~;Last~] ~:[~;undeleted ~]message."
		    (plusp offset) undeleted))
    (move-mark (buffer-point (line-buffer (mark-line msg-mark))) msg-mark)
    (let* ((next-msg (line-message-id (mark-line msg-mark)))
	   (folder (message-info-folder minfo))
	   (mbuffer (current-buffer)))
      (with-writable-buffer (mbuffer)
	(delete-region (buffer-region mbuffer))
	(setf (buffer-name mbuffer)
	      (get-storable-msg-buf-name folder next-msg))
	(setf (message-info-msgs minfo) next-msg)
	(let ((path (merge-pathnames next-msg (mh:folder-pathname folder))))
	  (read-message folder next-msg mbuffer
			(if (string= (mh:strip-folder-name folder)
				     (mh:draft-folder))
			    t
			    (value message-headers)))
	  (setf (buffer-pathname mbuffer) path))
	(let ((in-seq (mh:profile-component "unseen-sequence")))
	  (and in-seq
	       (mh:sequence-member-p next-msg
				     (mh:sequence-list folder in-seq))
	       (progn
		 (mh:mark-message folder next-msg in-seq :delete)
		 (folders-incr folder -1)))))))
  (let ((dbuffer (message-info-draft-buf minfo)))
    (when dbuffer
      (delete-variable 'message-buffer :buffer dbuffer)
      (setf (message-info-draft-buf minfo) nil))))

(defun get-storable-msg-buf-name (folder msg)
  (let ((name (format () "Message ~A ~A" folder msg)))
    (if (not (getstring name *buffer-names*))
	name
	(let ((n 2))
	  (loop
	    (setf name (format () "Message ~A ~A copy ~D" folder msg n))
	    (or (getstring name *buffer-names*) (return name))
	    (incf n))))))

(defun show-message-offset-mark (msg-mark offset undeleted)
  (with-mark ((temp msg-mark))
    (let ((winp
	   (cond (undeleted
		  (loop
		    (or (and (line-offset temp offset 0)
			     (fi (blank-line-p (mark-line temp))))  ;; FIX
			(return nil))
		    (or (line-message-deleted (mark-line temp))
			(return t))))
		 ((and (line-offset temp offset 0)
		       (not (blank-line-p (mark-line temp)))))
		 (t nil))))
      (if winp (move-mark msg-mark temp)))))

; FIX Read Message?
(defcommand "Show Message" ()
  "Show messages.  Try replace a message in an existing message buffer.  If
   all existing messages have been marked for preservation with `Keep
   Message' then create a new message buffer for the message.

   When in a `Headers' buffer use the current message, otherwise prompt for
   a message.  If multiple messages are requested inserts all the messages
   into the same buffer."
  (let ((hinfo (value headers-information)))
    (if hinfo
	;; FIX orig just called show-h-m here
	(if (string= (mh:strip-folder-name (headers-info-folder hinfo))
		     (mh:draft-folder))
	    (show-draft-message hinfo)
	    (progn
	      ;; Force line rehighlight.
	      (clear-chi-signatures (region (mark (current-line) 0)
					    (mark (current-line) 0)))
	      (show-headers-message hinfo)))
	(let ((folder (prompt-for-folder)))
	  (show-prompted-message folder (prompt-for-message :folder folder))))))

(defcommand "Show Message Next Window" ()
  "Shows the current message in the next window.  Prompts for a folder and
   message(s), displaying this in the current window.  When invoked in a
   headers buffer, shows the message on the current line."
  "Show a message."
  (let ((hinfo (value headers-information)))
    (if (<= (length *window-list*) 2)
	(split-window-command)
	(setf (current-window) (next-window (current-window))))
    (if hinfo
	(if (string= (mh:strip-folder-name (headers-info-folder hinfo))
		     (mh:draft-folder))
	    (show-draft-message hinfo)
	    (show-headers-message hinfo))
	(progn
	  (let ((folder (prompt-for-folder)))
	    (show-prompted-message folder (prompt-for-message :folder folder)))))))

(defcommand "Show Message Nicely" ()
  "Shows the current message in the next window shrinking the headers
   windows.  Prompts for a folder and message(s), displaying this in the
   current window.  When invoked in a headers buffer, shows the message on
   the current line."
  "Show a message."
  (let ((hinfo (value headers-information)))
    (if hinfo
	(progn
	  ;; FIX flickers
	  (go-to-one-window-command)
	  (split-window-command)
	  (next-window-command)
	  (split-window-command)
	  (next-window-command)
	  (delete-window-command)
	  ;; FIX orig just called show-h-m here
	  (if (string= (mh:strip-folder-name (headers-info-folder hinfo))
		       (mh:draft-folder))
	      (show-draft-message hinfo)
	      (show-headers-message hinfo)))
	(progn
	  (if (<= (length *window-list*) 2)
	      (split-window-command)
	      (setf (current-window) (next-window (current-window))))
	  (let ((folder (prompt-for-folder)))
	    (show-prompted-message folder (prompt-for-message :folder folder)))))))

;;; SHOW-HEADERS-MESSAGE shows the current message for hinfo.  If there is a
;;; main message buffer, clobber it, and we don't have to deal with kept
;;; messages or draft associations since those operations should have moved
;;; the message buffer into the others list.  Remove the message from the
;;; unseen sequence, and make sure the message buffer is displayed in some
;;; window.
;;;
(defun show-headers-message (hinfo)
  (multiple-value-bind (cur-msg cur-mark)
		       (headers-current-message hinfo)
    (or cur-msg (editor-error "Point must be on a header line."))
    (let* ((mbuffer (headers-info-msg-buffer hinfo))
	   (folder (headers-info-folder hinfo))
	   (buf-name (get-storable-msg-buf-name folder cur-msg))
	   (writable nil))
      (cond (mbuffer
	     (setf (buffer-name mbuffer) buf-name)
	     (setf writable (buffer-writable mbuffer))
	     (setf (buffer-writable mbuffer) t)
	     (delete-region (buffer-region mbuffer))
	     (let ((minfo (variable-value 'message-information :buffer mbuffer)))
	       (move-mark (message-info-headers-mark minfo) cur-mark)
	       (delete-mark cur-mark)
	       (setf (message-info-msgs minfo) cur-msg)))
	    (t (setf mbuffer (maybe-make-mh-buffer buf-name :message))
	       (setf (headers-info-msg-buffer hinfo) mbuffer)
	       (defevar "Message Information"
		 "This holds the information about the current headers buffer."
		 :value (make-message-info :folder folder
					   :msgs cur-msg
					   :headers-mark cur-mark)
		 :buffer mbuffer)
	       (defevar "Headers Buffer"
		 "This is bound in message and draft buffers to their
		  associated headers buffer."
		 :value (headers-info-buffer hinfo) :buffer mbuffer)
	       (defevar "Message Headers"
		 "List of message headers."
		 :value (value message-headers) :buffer mbuffer)
	       (defevar "Message Headers Alternate"
		 "List of message headers for message header toggling."
		 :value t :buffer mbuffer)))
      (let ((path (merge-pathnames cur-msg (mh:folder-pathname folder))))
	(read-message folder cur-msg mbuffer
		      (if (string= (mh:strip-folder-name
				    (headers-info-folder hinfo))
				   (mh:draft-folder))
			  t
			  (value message-headers)))
	(setf (buffer-pathname mbuffer) path))
      (setf (buffer-writable mbuffer) writable)
      (let ((in-seq (mh:profile-component "unseen-sequence")))
	(and in-seq
	     (mh:sequence-member-p cur-msg
				   (mh:sequence-list folder in-seq))
	     (progn
	       (mh:mark-message folder cur-msg in-seq :delete)
	       (folders-incr folder -1))))
      (get-message-buffer-window mbuffer))))

;;; SHOW-PROMPTED-MESSAGE takes an arbitrary message spec and blasts those
;;; messages into a message buffer.  First we pick the message to get them
;;; individually specified as normalized message ID's -- all integers and
;;; no funny names such as "last".
;;;
(defun show-prompted-message (folder msgs)
  (let* ((draft-p (string= folder (mh:draft-folder)))
	 (msgs (mh:pick-messages folder msgs ()))
	 (mbuffer (if draft-p
		      (maybe-make-mh-buffer
		       (get-storable-msg-buf-name folder msgs) ; FIX OK?
		       :draft)
		      (maybe-make-mh-buffer
		       (format () "Message ~A ~A" folder msgs)
		       :message))))
    (defevar "Message Information"
      "The information about the current message buffer."
      :value (fi draft-p (make-message-info :folder folder :msgs msgs))
      :buffer mbuffer)
    (let ((stream (make-editor-output-stream (buffer-point mbuffer) :full)))
      ;; FIX should handle headers,MIME  (loop write-message?)
      (mh:write-messages stream folder msgs)
      (setf (mh:current-message folder) (car msgs))
      (setf (buffer-modified mbuffer) ()))
    (buffer-start (buffer-point mbuffer))
    (setf (buffer-writable mbuffer) draft-p)
    (when draft-p
      (let ((msg-pathname (merge-pathnames (string (car msgs))
					   (mh:folder-pathname
					    (mh:draft-folder)))))
	(defevar "Draft Information"
	  "This holds the information about the current draft buffer."
	  :value (make-draft-info :folder folder
				  :message msgs
				  :pathname msg-pathname)
	  :buffer mbuffer)
	(setf (buffer-pathname mbuffer) msg-pathname)))
    (get-message-buffer-window mbuffer)))

;;; GET-MESSAGE-BUFFER-WINDOW currently just changes to buffer, unless
;;; buffer has any windows, in which case it uses the first one.  It could
;;; prompt for a window, split the current window or use the next one if
;;; there is one, funcall an Hvar.  It could take a couple of arguments to
;;; control its behaviour.  Whatever.
;;;
(defun get-message-buffer-window (mbuffer)
  (let ((wins (buffer-windows mbuffer)))
    (cond (wins
	   (setf (current-buffer) mbuffer)
	   (setf (current-window) (car wins)))
	  (t (change-to-buffer mbuffer)))))


(defevar "Scroll Message Showing Next"
  "When this is set, `Scroll Message' shows the next message when the end
   of the current message is visible."
  :value t)

(defcommand "Scroll Message" (p)
  "Scroll the current window down through the current message.  If the end
   of the message is visible and if *Scroll Message Showing Next* is set
   then show the next undeleted message."
  (if (fi p
	  (and (displayed-p (buffer-end-mark (current-buffer)) (current-window))
	       (value scroll-message-showing-next)))
      (show-message-offset 1 :undeleted)
      (scroll-window-down-command p)))

(defcommand "Keep Message" ()
  "In a `Message' buffer keep the current message buffer from being
   re-used.  Also, the buffer is kept if an associated draft is completed."
  (let ((minfo (value message-information)))
    (or minfo (editor-error "Must be in a message buffer."))
    (let ((hbuf (value headers-buffer)))
      (when hbuf
	(let ((mbuf (current-buffer))
	      (hinfo (variable-value 'headers-information :buffer hbuf)))
	  (when (eq (headers-info-msg-buffer hinfo) mbuf)
	    (setf (headers-info-msg-buffer hinfo) nil)
	    (push mbuf (headers-info-other-msg-bufs hinfo))))))
    (setf (message-info-keep minfo) t)))

(defcommand "Edit Message Buffer" ()
  "Recursively edit the current `Message' buffer.  Put the buffer into
   \"Text\" mode and make the buffer writable.  Associate the buffer with
   the pathname of the message for the duration of the recursive edit, so
   that saving the file is possible."
  (let ((minfo (value message-information)))
    (or minfo (editor-error "Must be in a message buffer."))
    (let* ((msg (message-info-msgs minfo))
	   (mbuf (current-buffer))
	   (mbuf-name (buffer-name mbuf))
	   (writable (buffer-writable mbuf))
	   (abortp t))
      (when (consp msg)
	(editor-error
	 "There appears to be more than one message in this buffer."))
      (or (string= (message-info-folder minfo)
		   (mh:coerce-folder-name (mh:draft-folder)))
	  (editor-error "Only drafts can be edited."))
      (unwind-protect
	  (progn
	    (setf (buffer-writable mbuf) t)
	    (setf (buffer-pathname mbuf)
		  (merge-pathnames
		   msg
		   (mh:folder-pathname (message-info-folder minfo))))
	    (delete-region (buffer-region mbuf))
	    (with-output-to-mark (stream (buffer-mark mbuf))
	      (mh:write-messages stream (mh:draft-folder) (list msg)))
	    (setf (buffer-modified mbuf) nil)
	    (setf (buffer-major-mode mbuf) "Text")
	    (setf (buffer-minor-mode mbuf "Draft") t)
	    (do-recursive-edit)
	    (setf abortp nil))
	(or abortp
	    (when (and (buffer-modified mbuf)
		       (prompt-for-y-or-n
			:prompt "Message buffer modified, save it? "
			:default t))
	      (save-file-command nil mbuf)))
	(delete-region (buffer-region mbuf))
	(read-message (mh:draft-folder) msg mbuf t)
	;; "Save File", which the user may have used, changes the buffer's
	;; name.
	(or (getstring mbuf-name *buffer-names*)
	    (setf (buffer-name mbuf) mbuf-name))
	(setf (buffer-writable mbuf) writable)
	(setf (buffer-pathname mbuf) nil)
	(setf (buffer-major-mode mbuf) "Message")
	(setf (buffer-minor-mode mbuf "Draft") nil)))))

(defcommand "Toggle Message Headers" ()
  "Toggle message headers display between all and some."
  (let ((buffer (current-buffer))
	(minfo (value message-information)))
    (or minfo
	(editor-error "The current buffer must be a message buffer."))
    (or (message-info-folder minfo)
	(editor-error "Attempt to toggle headers in a temporary message."))
    (with-writable-buffer (buffer)
      (let* ((mark (copy-mark (buffer-start-mark buffer)))
	     (end (copy-mark mark)))
	(when (find-pattern end *two-nl-pattern*)
	  (mark-after end)
	  (mark-after end)
	  (delete-region (region mark end))
	  (with-output-to-mark (stream mark)
	    (mh:write-headers (message-info-folder minfo)
			      (message-info-msgs minfo)
			      stream
			      (swap (value message-headers)
				    (value message-headers-alternate)))))))))

(defun hide-wildcards (name)
  (substitute
   #\_ #\[
   (substitute
    #\_ #\]
    (substitute
     #\_ #\*
     (substitute
      #\_ #\?
      name)))))

(defcommand "View MIME Part" ()
  "View any MIME part described under point."
  (let ((minfo (value message-information)))
    (or minfo
	(editor-error "The current buffer must be a message buffer."))
    (let ((mark (copy-mark (current-point))))
      (line-start mark)
      (if (eq (next-character mark) #\#)
	  (multiple-value-bind
	      (part content-type)
	      (if (message-info-folder minfo)
		  (mh:get-part (message-info-folder minfo)
			       (message-info-msgs minfo)
			       (count-characters
				(region (buffer-start-mark
					 (current-buffer))
					mark))
			       (value message-attachments))
		  (mh:get-headers-part
		   (value message-fields)
		   (count-characters
		    (region (buffer-start-mark (current-buffer))
			    mark))))
	    (when (and part content-type)
	      (multiple-value-bind (type subtype params)
				   (with-input-from-string
				       (in content-type)
				     (mh:parse-content-type in))
		;; FIX (mh:get-param "name")?
		(let* ((param-name (cdr (assoc "name" params
					       :test #'string=)))
		       (name (if param-name (hide-wildcards param-name)))
		       (pathname (pick-new-file
				  (concatenate
				   'string
				   (if name
				       "/tmp/tmp~D-~D-"
				       "/tmp/tmp~D-~D.")
				   (if name
				       (string
					(if (pathname-type name)
					    ;; The given name has a type.
					    name
					    (if subtype
						;; There is a subtype, use
						;; it as the pathname type.
						(file-namestring
						 (make-pathname
						  :type (string-downcase
							 subtype)
						  :defaults name))
						;; Just use the given name.
						name)))
				       ;; Just append the subtype to the
				       ;; temporary name.
				       subtype)))))
		  (with-open-file (stream pathname :direction :output)
		    (with-input-from-string (in part)
		      (transfer in stream)))
		  (view pathname type subtype params)))))))))

(defcommand "Save MIME Part" ()
  "Save any MIME part described under point."
  (let ((minfo (value message-information)))
    (or minfo
	(editor-error "The current buffer must be a message buffer."))
    (let ((mark (copy-mark (current-point))))
      (line-start mark)
      (if (eq (next-character mark) #\#)
	  (multiple-value-bind
	      (part content-type)
	      (if (message-info-folder minfo)
		  (mh:get-part (message-info-folder minfo)
			       (message-info-msgs minfo)
			       (count-characters
				(region (buffer-start-mark
					 (current-buffer))
					mark))
			       (value message-attachments))
		  (mh:get-headers-part (value message-fields)
				       (count-characters
					(region (buffer-start-mark
						 (current-buffer))
						mark))))
	    (if part
		(let* ((pre-name
			(or (and content-type
				 (multiple-value-bind
				     (type subtype params)
				     (with-input-from-string
					 (in content-type)
				       (mh:parse-content-type in))
				   (declare (ignore type subtype))
				   (cdr (assoc "name" params
					       :test #'string=))))
			    (directory-namestring
			     (buffer-default-pathname
			      (current-buffer)))))
		       (name (if pre-name (hide-wildcards pre-name))))
		  (with-open-file
		    (stream
		     (flet ((prompt ()
			      (prompt-for-file
			       :must-exist ()
			       :default name
			       :prompt "Save MIME part to file: "
			       :help "Name under which to file MIME part")))
		       (while ((file (prompt)
				     (prompt)))
			      (t)
			 (if (probe-file file)
			     (if (prompt-for-y-or-n
				  :prompt (format () "File ~A exists, overwrite? "
						  file)
				  :default ())
				 (return file))
			     (return file))))
		     :direction :output
		     :if-exists :overwrite
		     :if-does-not-exist :create)
		    (write-string part stream)))))))))

(defcommand "Next MIME Part" (p)
  "Move to the next MIME part in the current buffer."
  (or p (setq p 1))
  (let* ((mark (copy-mark (current-point)))
	 (buffer (current-buffer))
	 (end (if (plusp p)
		  (buffer-end-mark buffer)
		  (buffer-start-mark buffer))))
    (line-offset mark p 0)
    (loop
      (if (mark= mark end) (return))
      (when (and (next-character mark)
		 (char= (next-character mark) #\#))
	(move-mark (current-point) mark)
	(return))
      (line-offset mark p))))

(defcommand "Previous MIME Part" (p)
  "Move to the previous MIME part in the current buffer."
  (next-mime-part-command (- (or p 1))))


#[ Sending Messages

The most useful commands for sending mail are `Send Message' (bound to
m and s in `Headers' and `Message' modes), `Reply to
Message' (bound to r in `Headers' mode), and `Reply to Message in
Other Window' (bound to r in `Message' mode).  These commands set up a
`Draft' buffer and associate a `Message' buffer with the draft when
possible.  To actually deliver the message to its recipient(s), use
`Deliver Message' (bound to H-s in `Draft' mode).  To abort
sending mail, use `Delete Draft and Buffer' (bound to H-q in
`Draft' mode).  If one wants to temporarily stop composing a draft with the
intention of finishing it later, then the `Save File' command (bound to
C-x C-s) will save the draft to the user's draft folder.

`Draft' buffers have a special minor mode called `Draft' mode.
   FIX opened in Message Mode, e to edit
The major
mode of a `Draft' buffer is taken from the `Default Modes' variable.  The
user may wish to arrange that `Text' mode (and possibly `Fill' mode or
`Save' mode) be turned on whenever `Draft' mode is set.  FIX ref to how

{command:Send Message}
{command:Reply to Message}
{command:Reply to Message in Other Window}
{evariable:Reply to Message Prefix CC}

[Styles of Usage] exemplifies the use of Reply to Message Prefix CC.

{command:Forward Message}
{command:Deliver Message}
{evariable:Deliver Message Confirm}
{command:Delete Draft and Buffer}
{command:Remail Message}
{command:Draft Help}
]#


;;;; Draft Mode.

(defun setup-draft-mode (buffer)
  (highlight-visible-message-buffer buffer)
  (pushnew '("Draft" () highlight-visible-message-buffer)
	   *mode-highlighters*))

(defmode "Draft" :setup-function #'setup-draft-mode)

(defevar "Draft Information"
  "This holds the information about the current draft buffer.")

(defstruct (draft-info (:include message/draft-info)
		       (:print-function print-draft-info))
  folder		;String name of draft folder with leading "+".
  message		;String id of draft folder message.
  pathname		;Pathname of draft in the draft folder directory.
  delivered		;This is set when the draft was really sent.
  replied-to-folder	;Folder of message draft is in reply to.
  replied-to-msg)	;Message draft is in reply to.

(defun print-draft-info (obj str n)
  (declare (ignore n))
  (format str "#<Draft Info ~A>" (draft-info-message obj)))


(defevar "Reply to Message CC"
  "One of :all, :others and ().  Determines which addresses the Reply to
   Message commands add to the CC field."
  :value :others)

(defevar "Reply to Message Prefix CC"
  "One of :all, :others and ().  Determines which addresses the Reply to
   Message commands add to the CC field when they are called with prefix
   arguments."
  :value ())

(defcommand "Reply to Message" (p)
  "Draft a reply to a message.

   Store the draft in the draft folder so that it can be saved, edited and
   sent at any time.

   When in a headers or message buffer the current message is made
   available in a buffer, the draft is associated with the curent message
   and any association to a `Headers' buffer is propogated to the `Draft'
   buffer.  Showing other messages while in the `Headers' buffer leaves the
   current message buffer intact.

   Regard *Reply to Message CC* for carbon copy behaviour; with a prefix
   regard *Reply to Message Prefix CC*.  The value of these variables is
   one of :all, :others, or ().

   `Deliver Message' delivers the draft to its intended recipient(s)."
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (if (string= (mh:strip-folder-name (headers-info-folder hinfo))
			(mh:draft-folder))
	       (editor-error "Attempt to reply to a draft."))
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Point must be on a header line."))
	     (setup-reply-draft (headers-info-folder hinfo)
				cur-msg hinfo cur-mark p)))
	  (minfo
	   (or (message-info-folder minfo)
	       (editor-error "Attempt to replay to a temporary message."))
	   (setup-message-buffer-draft (current-buffer) minfo :reply p))
	  (t
	   (let ((folder (prompt-for-folder)))
	     (setup-reply-draft folder
				(car (prompt-for-message :folder folder))
				nil nil p))))))

;;; SETUP-REPLY-DRAFT takes a folder and msg to draft a reply to.  Optionally,
;;; a headers buffer and mark are associated with the draft.  First, the draft
;;; buffer is associated with the headers buffer if there is one.  Then the
;;; message buffer is created and associated with the drafter buffer and
;;; headers buffer.  Argument may be used to pass in the argument from the
;;; command.
;;;
(defun setup-reply-draft (folder msg &optional hinfo hmark argument)
  (let* ((dbuffer (sub-setup-message-draft
		   :repl :end-of-buffer
		   `(,folder ,msg ,argument)))
	 (dinfo (variable-value 'draft-information :buffer dbuffer))
	 (h-buf (if hinfo (headers-info-buffer hinfo))))
    (setf (draft-info-replied-to-folder dinfo) folder)
    (setf (draft-info-replied-to-msg dinfo) msg)
    (when h-buf
      (defevar "Headers Buffer"
	"This is bound in message and draft buffers to their associated
	 headers buffer."
	:value h-buf :buffer dbuffer)
      (setf (draft-info-headers-mark dinfo) hmark)
      (push dbuffer (headers-info-draft-bufs hinfo)))
    (let ((msg-buf (maybe-make-mh-buffer (format nil "Message ~A ~A"
						 folder msg)
					 :message)))
      (defevar "Message Information"
	"This holds the information about the current headers buffer."
	:value (make-message-info :folder folder :msgs msg
				  :headers-mark
				  (if h-buf (copy-mark hmark) hmark)
				  :draft-buf dbuffer)
	:buffer msg-buf)
      (when h-buf
	(defevar "Headers Buffer"
	  "This is bound in message and draft buffers to their associated
	   headers buffer."
	  :value h-buf :buffer msg-buf)
	(push msg-buf (headers-info-other-msg-bufs hinfo)))
      (read-message folder msg msg-buf)
      ;(setf (buffer-writable msg-buf) nil)
      (defevar "Message Buffer"
	"This is bound in draft buffers to their associated message buffer."
	:value msg-buf :buffer dbuffer))
    (get-draft-buffer-window dbuffer)))

(defcommand "Forward Message" ()
  "Draft a forwarding of a message.

   In a `Headers' or `Message' buffer use the current message, otherwise
   prompt for a message.

   Creates a unique `Draft' buffer, inserting the message in the draft as a
   forward, and showing the `Draft' buffer.  The `Draft' buffer is
   associated with the `Headers' buffer.  Propagate any association with a
   `Headers' buffer to the `Draft' buffer.

   `Deliver Message' delivers the draft to its intended recipient(s)."
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Not on a header line."))
	     (setup-forward-draft (headers-info-folder hinfo)
				  cur-msg hinfo cur-mark)))
	  (minfo
	   (or (message-info-folder minfo)
	       (editor-error "Attempt to forward a temporary message."))
	   (setup-message-buffer-draft (current-buffer) minfo :forward))
	  (t
	   (let ((folder (prompt-for-folder)))
	     (setup-forward-draft folder
				  (car (prompt-for-message
					:folder folder))))))))

;;; SETUP-FORWARD-DRAFT sets up a draft forwarding folder's msg.  When there
;;; is a headers buffer involved (hinfo and hmark), the draft is associated
;;; with it.
;;;
;;; This function is like SETUP-REPLY-DRAFT (in addition to "forw" and
;;; :to-field), but it does not setup a message buffer.  If this is added as
;;; something forward drafts want, then SETUP-REPLY-DRAFT should be
;;; parameterized and renamed.
;;;
(defun setup-forward-draft (folder msg &optional hinfo hmark)
  (let* ((dbuffer (sub-setup-message-draft :forw :to-field
					   (list folder msg)))
	 (dinfo (variable-value 'draft-information :buffer dbuffer))
	 (h-buf (if hinfo (headers-info-buffer hinfo))))
    (when h-buf
      (defevar "Headers Buffer"
	"This is bound in message and draft buffers to their associated
	headers buffer."
	:value h-buf :buffer dbuffer)
      (setf (draft-info-headers-mark dinfo) hmark)
      (push dbuffer (headers-info-draft-bufs hinfo)))
    (get-draft-buffer-window dbuffer)))

(defcommand "Send Message" ()
  "Draft a new message.

   Store the draft in the draft folder so that it can be saved, edited and
   sent at any time.

   When in a headers or message buffer the current message is made
   available in a buffer, the draft is associated with the current message
   and any association to a `Headers' buffer is propogated to the `Draft'
   buffer.  Showing other messages while in the `Headers' buffer leaves the
   current message buffer intact.

   `Deliver Message' delivers the draft to its intended recipient(s)."
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo (setup-headers-message-draft hinfo))
	  (minfo (setup-message-buffer-draft (current-buffer) minfo :compose))
	  (t (setup-message-draft)))))

(defun setup-message-draft ()
  (get-draft-buffer-window (sub-setup-message-draft :comp :to-field)))

;;; SETUP-HEADERS-MESSAGE-DRAFT sets up a draft buffer associated with a
;;; headers buffer and a message buffer.  The headers current message is
;;; inserted in the message buffer which is also associated with the headers
;;; buffer.  The draft buffer is associated with the message buffer.
;;;
(defun setup-headers-message-draft (hinfo)
  (multiple-value-bind (cur-msg cur-mark)
		       (headers-current-message hinfo)
    (or cur-msg (message "Draft not associated with any message."))
    (let* ((dbuffer (sub-setup-message-draft :comp :to-field))
	   (dinfo (variable-value 'draft-information :buffer dbuffer))
	   (h-buf (headers-info-buffer hinfo)))
      (when cur-msg
	(defevar "Headers Buffer"
	  "This is bound in message and draft buffers to their associated
	   headers buffer."
	  :value h-buf :buffer dbuffer)
	(push dbuffer (headers-info-draft-bufs hinfo)))
      (when cur-msg
	(setf (draft-info-headers-mark dinfo) cur-mark)
	(let* ((folder (headers-info-folder hinfo))
	       (msg-buf (maybe-make-mh-buffer
			 (format nil "Message ~A ~A" folder cur-msg)
			 :message)))
	  (defevar "Message Information"
	    "This holds the information about the current message."
	    :value (make-message-info :folder folder :msgs cur-msg
				      :headers-mark (copy-mark cur-mark)
				      :draft-buf dbuffer)
	    :buffer msg-buf)
	  (defevar "Headers Buffer"
	    "This is bound in message and draft buffers to their associated
	     headers buffer."
	    :value h-buf :buffer msg-buf)
	  (push msg-buf (headers-info-other-msg-bufs hinfo))
	  (read-message folder cur-msg msg-buf)
	  (setf (buffer-writable msg-buf) ())
	  (defevar "Message Buffer"
	    "This is bound in draft buffers to their associated message buffer."
	    :value msg-buf :buffer dbuffer)))
      (get-draft-buffer-window dbuffer))))

;;; SETUP-MESSAGE-BUFFER-DRAFT takes a message buffer and its message
;;; information.  A draft buffer is created according to type, and the two
;;; buffers are associated.  Any previous association of the message buffer and
;;; a draft buffer is dropped.  Any association between the message buffer and
;;; a headers buffer is propagated to the draft buffer, and if the message
;;; buffer is the headers buffer's main message buffer, it is moved to "other"
;;; status.  Argument may be used to pass in the argument from the command.
;;;
(defun setup-message-buffer-draft (msg-buf minfo type &optional argument)
  (let* ((msgs (message-info-msgs minfo))
	 (cur-msg (if (consp msgs) (car msgs) msgs))
	 (folder (message-info-folder minfo))
	 (dbuffer
	  (ecase type
	    (:reply
	     (sub-setup-message-draft :repl
				      :end-of-buffer
				      (list folder
					    cur-msg
					    argument)))
	    (:compose
	     (sub-setup-message-draft :comp :to-field))
	    (:forward
	     (sub-setup-message-draft :forw :to-field
				      (list folder cur-msg)))))
	 (dinfo (variable-value 'draft-information :buffer dbuffer)))
    (when (message-info-draft-buf minfo)
      (delete-variable 'message-buffer :buffer (message-info-draft-buf minfo)))
    (setf (message-info-draft-buf minfo) dbuffer)
    (when (eq type :reply)
      (setf (draft-info-replied-to-folder dinfo) folder)
      (setf (draft-info-replied-to-msg dinfo) cur-msg))
    (when (editor-bound-p 'headers-buffer :buffer msg-buf)
      (let* ((hbuf (variable-value 'headers-buffer :buffer msg-buf))
	     (hinfo (variable-value 'headers-information :buffer hbuf)))
	(defevar "Headers Buffer"
	  "This is bound in message and draft buffers to their associated
	  headers buffer."
	  :value hbuf :buffer dbuffer)
	(setf (draft-info-headers-mark dinfo)
	      (copy-mark (message-info-headers-mark minfo)))
	(push dbuffer (headers-info-draft-bufs hinfo))
	(when (eq (headers-info-msg-buffer hinfo) msg-buf)
	  (setf (headers-info-msg-buffer hinfo) nil)
	  (push msg-buf (headers-info-other-msg-bufs hinfo)))))
    (defevar "Message Buffer"
      "This is bound in draft buffers to their associated message buffer."
      :value msg-buf :buffer dbuffer)
    (get-draft-buffer-window dbuffer)))

(defvar *draft-to-pattern*
  (new-search-pattern :string-insensitive :forward "To:"))

(defun sub-setup-message-draft (utility point-action &optional args)
  (ecase utility
    (:comp (mh:draft-new))
    (:forw (mh:draft-forward (car args) (cadr args)))
    (:repl (mh:draft-reply (car args) (cadr args)
			   (if (caddr args)
			       (value reply-to-message-prefix-cc)
			       (value reply-to-message-cc)))))
  (let* ((folder (mh:draft-folder))
	 (draft-msg (string (mh:current-message folder)))
	 (msg-pn (merge-pathnames draft-msg
				  (mh:folder-pathname
				   (mh:draft-folder))))
	 (dbuffer (maybe-make-mh-buffer (format () "Draft ~A" draft-msg)
					:draft)))
    (read-message (mh:draft-folder) draft-msg dbuffer :draft)
    (setf (buffer-pathname dbuffer) msg-pn)
    (defevar "Draft Information"
      "This holds the information about the current draft buffer."
      :value (make-draft-info :folder (mh:coerce-folder-name folder)
			      :message draft-msg
			      :pathname msg-pn)
      :buffer dbuffer)
    (let ((point (buffer-point dbuffer)))
      (ecase point-action
	(:to-field
	 (when (find-pattern point *draft-to-pattern*)
	   (line-end point)))
	(:end-of-buffer (buffer-end point))))
#|
    (setf (buffer-modified dbuffer) ())
    (setf (buffer-writable dbuffer) ())
|#
    dbuffer))

(defun convert-html (in html-file)
  "Render the HTML from $html-file into the buffer associated with editor
   stream stream $in."
  (let* ((point (edi::editor-output-stream-mark in))
	 (buffer (line-buffer (mark-line point))))
    (www-buffer buffer point html-file)))

;; FIX write-message?
(defun read-message (folder message buffer &optional (headers t))
  (with-output-to-mark (stream (buffer-point buffer))
    (if (eq headers :draft)
	(progn
	  (mh:write-messages stream folder (list message))
	  (setf (mh:current-message folder) message))
	(let ((mh:*html-handler* #'convert-html))
	  (multiple-value-bind (ok err parts)
			       (mh:write-message folder message
						 stream headers)
	    (if ok
		;; FIX guess should go in minfo (eg for multiple msgs in buf)
		(defevar "Message Attachments"
		  "List of attachments to message."
		  :buffer buffer
		  :value parts)
		(message "Error reading message ~A in folder ~A: ~A."
			 message (mh:strip-folder-name folder) err))))))
  (setf (buffer-write-date buffer)
	(file-write-date (merge-pathnames message
					  (mh:folder-pathname folder))))
  (buffer-start (buffer-point buffer))
  (setf (buffer-modified buffer) ()))

(defvar *draft-buffer-window-fun* 'change-to-buffer
  "This is called by GET-DRAFT-BUFFER-WINDOW to display a new draft buffer.
   The default is CHANGE-TO-BUFFER which uses the current window.")

;;; GET-DRAFT-BUFFER-WINDOW is called to display a new draft buffer.
;;;
(defun get-draft-buffer-window (dbuffer)
  (funcall *draft-buffer-window-fun* dbuffer))

(defcommand "Reply to Message in Other Window" (p)
  "Draft a reply to a message in the other window, splitting the window if
   there is only one window.

   Store the draft in the draft folder so that it can be saved, edited and
   sent at any time.

   When in a headers or message buffer the current message is made
   available in a buffer, the draft is associated with the curent message
   and any association to a `Headers' buffer is propogated to the `Draft'
   buffer.  Showing other messages while in the `Headers' buffer leaves the
   current message buffer intact.

   Regard *Reply to Message CC* for carbon copy behaviour; with a prefix
   regard *Reply to Message Prefix CC*.  The value of these variables is
   one of :all, :others, or ().

   `Deliver Message' delivers the draft to its intended recipient(s)."
  (let ((*draft-buffer-window-fun* #'draft-buffer-in-other-window))
    (reply-to-message-command p)))

(defun draft-buffer-in-other-window (dbuffer)
  (when (editor-bound-p 'message-buffer :buffer dbuffer)
    (let ((mbuf (variable-value 'message-buffer :buffer dbuffer)))
      (when (not (eq (current-buffer) mbuf))
	(change-to-buffer mbuf))))
  (setf (current-buffer) dbuffer)
  (setf (current-window) (make-window (buffer-start-mark dbuffer)))
  (defevar "Split Window Draft"
    "Indicates window needs to be cleaned up for draft."
    :value t :buffer dbuffer))

(defcommand "Reply to Message with Message" (p)
  "Set up a draft in reply to the current message, quoting the message.

   Regard *Reply to Message CC* for carbon copy behaviour; with a prefix
   regard *Reply to Message Prefix CC*."
  (reply-to-message-command p)
  (end-of-buffer-command)
  (setf (buffer-writable (current-buffer)) t)
  ;; FIX maybe if region in message body then insert region
  (insert-message-buffer-command)
  (exchange-point-and-mark-command)
  ;; Filter out headers and leading and trailing blank lines.
  (let* ((point (copy-mark (current-point)))
	 (mark1 (copy-mark point))
	 (mbip (value message-buffer-insertion-prefix))
	 (pattern (new-search-pattern :string-sensitive
				      :forward
				      (concatenate 'simple-string
						   (string #\newline)
						   mbip
						   (string #\newline))))
	 (chars (find-pattern mark1 pattern)))
    (when chars
      (character-offset mark1 chars)
      (let ((mark2 (copy-mark mark1)))
	(block find
	  (loop
	    (setq chars (find-pattern mark1 pattern))
	    (or chars (return-from find))
	    (character-offset mark1 chars)
	    (or (mark= mark1 mark2)
		(return-from find))
	    (move-mark mark2 mark1)))
	(delete-region (region point mark2))))
    ;; Trailing.
    (move-mark mark1 (buffer-end-mark (current-buffer)))
    (let ((pattern (new-search-pattern
		    :string-sensitive
		    :backward
		    (concatenate 'simple-string
				 (string #\newline)
				 mbip
				 (string #\newline)))))
      (let ((mark2 (copy-mark mark1)))
	(block find
	  (loop
	    (setq chars (find-pattern mark1 pattern))
	    (or chars (return-from find))
	    (character-offset mark1 chars)
	    (or (mark= mark1 mark2)
		(return-from find))
	    (character-offset mark1 (1+ (- chars)))
	    (move-mark mark2 mark1)))
	(delete-region (region mark2
			       (buffer-end-mark
				(current-buffer)))))))
  (save-file-command)
  ;; FIX why region still highlighted?
  (pacify-region))

(defevar "SMTP Account"
  "SMTP account, created with make-smtp-account."
  :value ;(internet:fill-from-netrc FIX home: error
  (internet:make-inet-account "localhost"));)

(defevar "Deliver Message Confirm"
  "When set, `Deliver Message' will ask for confirmation before sending the
   draft."
  :value t)

(defcommand "Deliver Message" ()
  "In a `Draft' buffer, save the file and deliver the draft.  If `Deliver
   Message Confirm' is set then confirm that the message must be delivered.
   If the draft is a reply to some message, then annotate that message with
   a \"replied\" component.  Update any `Headers' buffers containing the
   replied-to message with an \"A\" placed in the appropriate headers line
   two characters after the message ID.

   In any other mode of buffer, prompt for a draft message ID and deliver
   that draft.  Sending a draft in this way severs any association that
   draft may have had with a message being replied to, forfeiting any
   annotation."
  (let ((dinfo (value draft-information)))
    (cond (dinfo
	   (deliver-draft-buffer-message dinfo))
	  (t
	   (let ((folder (mh:coerce-folder-name (mh:draft-folder)))
		 (msg-info (value message-information))
		 (msg)
		 (recovery-region))
	     (if (and msg-info
		      (string= (message-info-folder msg-info) folder))
		 (let ((msgs (message-info-msgs msg-info)))
		   (or (message-info-folder msg-info)
		       (editor-error "Attempt to deliver a temporary message."))
		   (setq msg
			 (if (if msgs (consp msgs) t)
			     (prog1
			       (prompt-for-message :folder folder)
			       (editor-error "FIX insert attachments"))
			     ;; FIX fix somehow handle the updated buffer
			     ;; if deliver-messages fails
			     (let ((buf (current-buffer)))
			       (setf (buffer-writable buf) t)
			       (setq recovery-region
				     (copy-region (buffer-region buf)))
			       (insert-attachments buf)
			       (when (buffer-modified buf)
				 (write-buffer-file
				  buf
				  (buffer-pathname buf)))
			       (breakup-message-spec msgs)))))
		  (setq msg (prompt-for-message :folder folder)))
	      ;; FIX Other case annotates message to which draft replies.
	     (let ((buf (current-buffer)))
	       (multiple-value-bind
		   (success error)
		   (unwind-protect
		       (handler-bind
			   ((error (lambda (error) (values () error))))
			 (msg "send")
			 (mh:deliver-messages (value smtp-account) msg folder))
		     (when recovery-region
		       (delete-region (buffer-region buf))
		       (insert-region (buffer-point buf)
				      recovery-region)
		       (write-buffer-file buf (buffer-pathname buf))))
		 (or success
		     (progn
		       (when recovery-region
			 (delete-region (buffer-region buf))
			 (insert-region (buffer-point buf)
					recovery-region)
			 (write-buffer-file buf (buffer-pathname buf)))
		       (editor-error "Delivery failed: ~A" error))))))))))

(defun deliver-draft-buffer-message (dinfo)
  (if (draft-info-delivered dinfo)
      (editor-error "This draft has already been delivered."))
  (when (if (value deliver-message-confirm)
	    (prompt-for-y-or-n :prompt "Deliver message? " :default t)
	    t)
    (let* ((dbuffer (current-buffer))
	   (recovery-region (copy-region (buffer-region dbuffer))))
      (setf (buffer-writable dbuffer) t)
      (insert-attachments dbuffer)
      (if (buffer-modified dbuffer)
	  (write-buffer-file dbuffer (buffer-pathname dbuffer)))
      (message "Delivering draft ...")
      (multiple-value-bind
	  (success error)
	  (handler-case
	      (mh:deliver-messages (value smtp-account)
				   (list (draft-info-message dinfo))
				   (draft-info-folder dinfo))
	    (error (error) (values () error)))
	(or success
	    (progn
	      (when recovery-region
		(delete-region (buffer-region dbuffer))
		(insert-region (buffer-point dbuffer)
			       recovery-region)
		(write-buffer-file dbuffer (buffer-pathname dbuffer)))
	      (editor-error "Delivery failed: ~A" error))))
      (setf (draft-info-delivered dinfo) t)
      (let ((replied-folder (draft-info-replied-to-folder dinfo))
	    (replied-msg (draft-info-replied-to-msg dinfo)))
	(when replied-folder
	  (message "Annotating message being replied to ...")
	  (mh:annotate-message replied-folder replied-msg "replied")
	  (do-headers-buffers (hbuf replied-folder)
	    (with-headers-mark (hmark hbuf replied-msg)
	      (mark-to-note-replied-msg hmark)
	      (with-writable-buffer (hbuf)
		(setf (next-character hmark) #\A))))
	  (dolist (b *buffer-list*)
	    (and (editor-bound-p 'message-information :buffer b)
		 (buffer-modeline-field-p b :replied-to-message)
		 (dolist (w (buffer-windows b))
		   (update-modeline-field b w :replied-to-message))))))
      (maybe-delete-extra-draft-window dbuffer (current-window))
      (let ((mbuf (value message-buffer)))
	(and mbuf
	     (or (editor-bound-p 'netnews-message-info :buffer mbuf)
		 (let ((minfo (variable-value 'message-information :buffer mbuf)))
		   (if minfo (or (message-info-keep minfo)
				 (delete-buffer-if-possible mbuf)))))))
      (delete-buffer-if-possible dbuffer))))

(defun make-boundary (buffer mark)
  "Return a boundary string for Buffer that is unique after Mark."
  (declare (ignore buffer))
  (let* ((boundary "=-=-=")
	 (m2 (copy-mark mark))
	 (pattern (new-search-pattern :string-sensitive :forward
				      boundary)))
    (loop while (find-pattern m2 pattern)
      do
      (move-mark m2 mark)
      (setq boundary (format nil "~A-=" boundary))
      (setq pattern (new-search-pattern :string-sensitive :forward
					boundary)))
    boundary))

 ;#define CPERLIN 76              chars per output line (= 57 chars per input line)
 ;#define BPERLIN (CPERLIN / 4)   = 19 bytes per line of output
 (defvar *base64-chars-per-line* 76
   "Characters per line of base64 output in MIME attachments.")

 ; FIX uip/mhn.c h/mhn.h
 (defun insert-mime-part (mark boundary type subtype file
			       &optional description inline-p)
   "Insert a MIME part at $mark."
   (with-output-to-mark (out mark)
     (format out "--~A~%Content-Type: ~A/~A~%" boundary type subtype)
     (if inline-p
	 (format out "Content-Disposition: inline~%")
	 (progn
	   (format out "Content-Disposition: attachment; filename=\"~A\"~%"
		   (file-namestring file))
	   ;; FIX when to use base64? uip/mhn.c
	   (format out "Content-Transfer-Encoding: base64~%")))
     (if description
	 (format out "Content-Description: ~A" description))
     (format out "~%~%")
     (with-open-file (in file :direction :input
			      :if-does-not-exist :error)
       (if inline-p
	   (transfer in out)
	   (with-temp-buffer (buffer file)
	     (let* ((coded (base64:base64-encode
			    (region-to-string (buffer-region buffer))))
		    (len (length coded))
		    (start 0))
	       (while ((dist (min (- len start) *base64-chars-per-line*)
			     (min (- len start) *base64-chars-per-line*)))
		      ((< start len))
		 ;(message "write ~A from ~A to ~A" coded start (+ start dist))
		 (write-string coded out :start start :end (+ start dist))
		 (terpri out)
		 ;(or (eq dist *base64-chars-per-line*) (return))
		 ;(if (eq dist *base64-chars-per-line*)
		 ;  (terpri out)
		 ;  ;; FIX always terminate w an =? right to add an extra one?
		 ;  (progn
		 ;    (if (eq (previous-character mark) #\=)
		 ;      (terpri out)
		 ;      (format out "=~%"))
		 ;    (return)))
		 (incf start dist))))))))

 (defvar *attach-start-pattern*
   (new-search-pattern :string-sensitive :forward "--[Attached: \""))

    (defevar "MH Attach in Lisp"
   "If true process attachments before calling the send program, otherwise
    attaching is left to MH (which requires an \"automhnproc: mhn\" line in
    :.mh_profile."
   :value t)

    (defun insert-attachments (buffer)
      "Insert the files named by any attachment clauses in $buffer."
      (when (value mh-attach-in-lisp)
	(buffer-start (buffer-point buffer))
	(let ((mark (copy-mark (buffer-point buffer))))
	  ;; FIX ensure sep alone on line
	  (when (find-string mark (value message-header-body-separator))
	    (let ((sep-start (copy-mark mark))
		  (end (copy-mark mark))
		  (start (copy-mark mark))
		  (boundary (make-boundary buffer mark))
		  (found))
	      (move-mark mark sep-start) ; In case make-boundary moved mark.
	      (line-offset mark 1 0)
	      ; #text/plain [Todo list] /home/ram/todo
	      (while (inline-p)
		     ((find-character mark #\#))
		(if (plusp (mark-charpos mark))
		;; The # is offset into the line, keep searching.
		(or (line-offset mark 1 0)
		    ;; End of buffer.
		    (progn
		      (line-start mark)
		      (return)))
		;; The # starts the line, look for an attachment.
		(catch 'found-attachment
		  (with-mark ((tem mark))
		    (mark-after tem)
		    (when (char= (next-character tem) #\:)
		      (setq inline-p t)
		      (mark-after tem))
		    (multiple-value-bind (type subtype)
					 (get-mime-type tem)
		      (mark-after tem)
		      (when (and type subtype
				 (eq (previous-character tem) #\space)
				 (eq (next-character tem) #\[))
			(mark-after tem)
			(move-mark start tem)
			(when (find-character tem #\])
			  ;; Found the description.
			  (let ((descr (region-to-string (region start tem))))
			    (mark-after (mark-after tem))
			    (when (eq (previous-character tem) #\space)
			      ;; Found the space after the description.
			      (move-mark start tem)
			      (line-end tem)
			      (let ((file (region-to-string (region start tem))))
				(mark-after tem)
				(delete-region (region mark tem))
				(and found (mark> mark end)
				     ;; There is text between the part and
				     ;; the previous parts, insert a
				     ;; boundary before the text.
				     (with-output-to-mark (out end)
				       (format out "--~A~%~%" boundary)))
				(insert-mime-part mark boundary type subtype
						  file descr inline-p)
				(move-mark end mark) ; For first when in loop.
				(setq found t)
				;; Found attachment, throw to skip
				;; line-offset.
				(throw 'found-attachment ()))))))))
		  (or (line-offset mark 1 0)
		      ;; End of buffer.
		      (progn
			(line-start mark)
			(return))))))
	  (when found
	    ;; FIX what usually deals with the message/body separator?
	    ;;       maybe send/post blanks the line following the headers
	    (let ((sep-line (mark-line sep-start)))
	      (delete-region (region sep-start
				     (mark sep-line (line-length sep-line)))))
	    ;; Insert the MIME headers at the separator mark.
	    (with-output-to-mark (stream sep-start)
	      (format stream
  "MIME-Version: 1.0~%Content-Type: multipart/mixed; boundary=\"~A\"~%"
                      boundary)
	      (mark-after sep-start)
	      (or (string= (line-string (mark-line sep-start))
			   (format () "--~A" boundary))
		  ;; First part is text.
		  (format stream "--~A~%~%" boundary)))
	    ;; Make a part for any trailing text.
	    (or (mark= (buffer-end-mark buffer) end)
		(progn
		  (with-output-to-mark (out end)
		    (format out "--~A~%~%" boundary))
		  (buffer-end end)))
	    ;; Insert the final boundary.
	    (or (char= (previous-character end) #\newline)
		(insert-character end #\newline))
	    (with-output-to-mark (stream end)
	      (format stream "--~A--~%" boundary))))))))

(defcommand "Delete Draft and Buffer" ()
  "When invoked in a `Draft' buffer, delete the draft message file and the
   buffer.  Also delete any associated message buffer unless the user
   preserved it with `Keep Message'."
  (let ((dinfo (value draft-information))
	(dbuffer (current-buffer)))
    (or dinfo (editor-error "No draft associated with buffer."))
    (maybe-delete-extra-draft-window dbuffer (current-window))
    (delete-file (draft-info-pathname dinfo))
    (let ((mbuf (value message-buffer)))
      (when (and mbuf
		 (not (editor-bound-p 'netnews-message-info :buffer mbuf)))
	(let ((minfo (variable-value 'message-information :buffer mbuf)))
	  (when (and minfo (not (message-info-keep minfo)))
	    (delete-buffer-if-possible mbuf)))))
    (delete-buffer-if-possible dbuffer)))

;;; MAYBE-DELETE-EXTRA-DRAFT-WINDOW -- Internal.
;;;
;;; This takes a draft buffer and a window into it that should not be deleted.
;;; If "Split Window Draft" is bound in the buffer, and there are at least two
;;; windows in dbuffer-window's group, then we delete some window.  Blow away
;;; the variable, so we don't think this is still a split window draft buffer.
;;;
(defun maybe-delete-extra-draft-window (dbuffer dbuffer-window)
  (when (and (editor-bound-p 'split-window-draft :buffer dbuffer)
	     ;; Since we know bitmap devices have window groups, this loop is
	     ;; more correct than testing the length of *window-list* and
	     ;; accounting for *echo-area-window* being in there.
	     (do ((start dbuffer-window)
		  (count 1 (1+ count))
		  (w (next-window dbuffer-window) (next-window w)))
		 ((eq start w) (> count 1))))
    (delete-window (next-window dbuffer-window))
    (delete-variable 'split-window-draft :buffer dbuffer)))

(defcommand "Remail Message" ()
  "In a `Headers' or `Message' buffer, prompt for resend To: and resend Cc:
   addresses and remail the current message.  In any other kind of buffer,
   prompt for a folder and message as well.

   Message can only be annotated when they are initially mailed." ; FIX ~~
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Not on a header line."))
	     (delete-mark cur-mark)
	     (remail-message (headers-info-folder hinfo) cur-msg
			     (prompt-for-string :prompt "Resend To: ")
			     (prompt-for-string :prompt "Resend Cc: "))))
	  (minfo
	   (remail-message (message-info-folder minfo)
			   (message-info-msgs minfo)
			   (prompt-for-string :prompt "Resend To: ")
			   (prompt-for-string :prompt "Resend Cc: ")))
	  (t
	   (let ((folder (prompt-for-folder)))
	     (remail-message folder
			     (car (prompt-for-message :folder folder))
			     (prompt-for-string :prompt "Resend To: ")
			     (prompt-for-string :prompt "Resend Cc: "))))))
  (message "Message remailed."))

;;; REMAIL-MESSAGE claims a draft folder message with mh:draft-message.
;;; This is then sucked into a buffer and modified by inserting the
;;; supplied addresses.  "send" is used to deliver the draft, but it
;;; requires certain environment variables to make it do the right thing.
;;; "mhdist" says the draft is only remailing information, and "mhaltmsg"
;;; is the message to send.  "mhannotate" must be set due to a bug in MH's
;;; "send"; it will not notice the "mhdist" flag unless there is some
;;; message to be annotated.  This command does not provide for annotation
;;; of the remailed message.
;;;
(defun remail-message (folder msg resend-to resend-cc)
  (mh:draft-resend)
  (let* ((draft-folder (mh:draft-folder))
	 (draft-msg (string (mh:current-message draft-folder))))
    (setup-remail-draft-message draft-msg resend-to resend-cc)
    (mh:resend-message (value smtp-account) msg folder draft-msg draft-folder))
;     (mh "send" `("-draftfolder" ,draft-folder "-draftmessage" ,draft-msg)
; 	:environment
; 	`((:|mhdist| . "1")
; 	  (:|mhannotate| . "1")
; 	  (:|mhaltmsg| . ,(namestring
; 			   (merge-pathnames msg (mh:folder-pathname folder)))))))
  )

;;; SETUP-REMAIL-DRAFT-MESSAGE takes a draft folder and message that have
;;; been created with mh:draft-message.  A buffer is created with this
;;; message's pathname, searching for "resent-to:" and "resent-cc:",
;;; filling in the supplied argument values.  After writing out the
;;; results, the buffer is deleted.
;;;
(defvar *draft-resent-to-pattern*
  (new-search-pattern :string-insensitive :forward "resent-to:"))
(defvar *draft-resent-cc-pattern*
  (new-search-pattern :string-insensitive :forward "resent-cc:"))

(defun setup-remail-draft-message (msg resend-to resend-cc)
  (let* ((msg-pn (merge-pathnames msg (mh:folder-pathname
				       (mh:draft-folder))))
	 (dbuffer (maybe-make-mh-buffer (format () "Draft ~A" msg)
					:draft))
	 (point (buffer-point dbuffer)))
    (read-message (mh:draft-folder) msg dbuffer)
    (when (find-pattern point *draft-resent-to-pattern*)
      (line-end point)
      (insert-string point resend-to))
    (buffer-start point)
    (when (find-pattern point *draft-resent-cc-pattern*)
      (line-end point)
      (insert-string point resend-cc))
    (write-file (buffer-region dbuffer) msg-pn :keep-backup nil)
    ;; Touch the draft directory to ensure that the cache is updated.
    (touch-file (mh:folder-pathname (mh:draft-folder)))
    ;; The draft buffer delete hook expects this to be bound.
    (defevar "Draft Information"
      "This holds the information about the current draft buffer."
      :value :ignore
      :buffer dbuffer)
    (delete-buffer dbuffer)))

;;; SHOW-DRAFT-MESSAGE shows the current message for hinfo.  If there is a
;;; main message buffer, clobber it, and we don't have to deal with kept
;;; messages since that operation should have moved the message buffer into
;;; the others list.  Make sure the message buffer is displayed in some
;;; window.
;;;
(defun show-draft-message (hinfo)
  (multiple-value-bind (cur-msg cur-mark)
		       (headers-current-message hinfo)
    (or cur-msg (editor-error "Point must be on a header line."))
    (let* ((mbuffer (headers-info-msg-buffer hinfo))
	   (folder (headers-info-folder hinfo))
	   (buf-name (get-storable-msg-buf-name folder cur-msg))
	   (writable t)
	   (msg-pathname (merge-pathnames cur-msg
					  (mh:folder-pathname
					   (mh:draft-folder)))))
      (cond (mbuffer
	     (setf (buffer-name mbuffer) buf-name)
	     (setf writable (buffer-writable mbuffer))
	     (setf (buffer-writable mbuffer) t)
	     (let ((minfo (variable-value 'message-information
					  :buffer mbuffer)))
	       (move-mark (message-info-headers-mark minfo) cur-mark)
	       (delete-mark cur-mark)
	       (or (eq (message-info-msgs minfo) cur-msg)
		   (progn
		     (delete-region (buffer-region mbuffer))
		     (setf (message-info-msgs minfo) cur-msg)
		     (read-message (mh:draft-folder) cur-msg mbuffer :draft)))))
	    (t (setf mbuffer (maybe-make-mh-buffer buf-name :draft))
	       (setf (headers-info-msg-buffer hinfo) mbuffer)
	       (defevar "Draft Information"
		 "This holds the information about the current draft buffer."
		 :value (make-draft-info :folder folder
					 :message cur-msg
					 :pathname msg-pathname)
		 :buffer mbuffer)
	       (defevar "Message Information"
		 "This holds the information about the current headers buffer."
		 :value (make-message-info :folder folder
					   :msgs cur-msg
					   :headers-mark cur-mark)
		 :buffer mbuffer)
	       (defevar "Headers Buffer"
		 "This is bound in message and draft buffers to their
		  associated headers buffer."
		 :value (headers-info-buffer hinfo) :buffer mbuffer)
	       (read-message (mh:draft-folder) cur-msg mbuffer :draft)))
      (setf (buffer-writable mbuffer) writable)
      (setf (buffer-pathname mbuffer) msg-pathname)
      (get-message-buffer-window mbuffer))))

(defcommand "Edit Draft Buffer" ()
  "Edit draft buffer."
  (let* ((minfo (value message-information)))
    (or minfo (editor-error "Point must be in a message buffer."))
    (let* ((msgs (message-info-msgs minfo))
	   (mbuf (current-buffer))
	   (mbuf-name (buffer-name mbuf))
	   (writable (buffer-writable mbuf))
	   (abortp t))
      (when (consp msgs)
	(editor-error
	 "There appears to be more than one message in this buffer."))
      (unwind-protect
	  (progn
	    (setf (buffer-writable mbuf) t)
	    (setf (buffer-pathname mbuf)
		  (merge-pathnames
		   msgs
		   (mh:folder-pathname (message-info-folder minfo))))
	    (setf (buffer-major-mode mbuf) "Text")
	    (do-recursive-edit)
	    (setf abortp nil))
	(when (and (not abortp)
		   (buffer-modified mbuf)
		   (prompt-for-y-or-n
		    :prompt "Message buffer modified, save it? "
		    :default t))
	  (save-file-command nil mbuf))
	(setf (buffer-modified mbuf) nil)
	;; "Save File", which the user may have used, changes the buffer's
	;; name.
	(or (getstring mbuf-name *buffer-names*)
	    (setf (buffer-name mbuf) mbuf-name))
	(setf (buffer-writable mbuf) writable)
	(setf (buffer-pathname mbuf) nil)
	(setf (buffer-major-mode mbuf) "Message")))))

#| FIX first attempt, completes manually on single word
(defcommand "Complete or Space" ()
  "Complete address when point is on first line, else indent."
  (let ((point (current-point)))
    (if (line-previous (mark-line point))
	(insert-character point #\ )
	(let ((mark (copy-mark (current-point))))
	  (if (plusp (character-attribute :whitespace (next-character mark)))
	      (mark-before mark))
	  (if (plusp (character-attribute :word-delimiter
					  (next-character mark)))
	      (insert-character point #\ )
	      (let ((record (db:find-record (word-at-point mark))))
		(when record
		  (let ((emails (db:db-record-emails record))
			(name (db:db-record-full-name record)))
		    (when emails
		      (word-start mark)
		      (word-end point)
		      (kill-region (region mark point) :kill-forward)
		      (push-buffer-mark (copy-mark point) t)
		      (with-output-to-mark (stream mark)
			(if name
			    (format stream "\"~A\" <~A>" name (car emails))
			    (format stream "~A" (car emails)))))))))
	  (delete-mark mark)))))
|#

(defun insert-email (point record)
  "Insert at Point the email address in Record."
  (when record
    (let ((emails (db:db-record-emails record))
	  (name (db:db-record-full-name record)))
      (or emails (editor-error "FIX Add email."))
      (when emails
	(push-buffer-mark (copy-mark point) t)
	(with-output-to-mark (stream point)
	  (if (string= name "")
	      (format stream "~A" (car emails))
	      (format stream "\"~A\" <~A>" name (car emails))))))))

(defcommand "Draft Insert Space" ()
  "Complete address when point is on first line, else insert a space."
  (let ((point (current-point)))
    (if (or (line-previous (mark-line point))
	    (eq (previous-character point) #\"))
	(insert-character point #\ )
	(let ((mark (copy-mark point)))
	  (when (or (find-character mark #\; :backward t)
		    (find-character mark #\: :backward t))
	    (find-attribute mark :word-delimiter 'zerop)
	    (if (let ((pre (region-to-string (region mark point))))
		  (or (position #\" pre)
		      (position #\< pre)
		      (position #\> pre)))
		(insert-character point #\ )
		(progn
		  (or (find-character point #\;) (line-end point))
		  (when (mark< mark point)
		    (let ((string (region-to-string (region mark point))))
		      (multiple-value-bind
			  (prefix key record field ambig); last-field-p)
			  (complete-string string (list (db:get-db-table)))
			(declare (ignore field key))
			(if (eq (char string (1- (length string))) #\ )
			    (let ((*parse-help* "Email address")
				  (*parse-type* :keyword)
				  (*parse-string-tables* (list (db:get-db-table)))
				  (*parse-input-region* (region mark point)))
			      (help-on-parse-command)))
			(when prefix
			  (word-start mark)
			  (delete-region (region mark point))
			  (if ambig
			      (progn
				(insert-string mark prefix)
				(insert-character mark #\ ))
			      (insert-email point record))))))
		  (delete-mark mark))))))))

(defcommand "Draft New Line" ()
  "Confirm address when point is on first line, else call New Line."
  (let ((point (current-point)))
    (if (line-previous (mark-line point))
	(new-line-command)
	(let ((mark (copy-mark point)))
	  (when (or (find-character mark #\; :backward t)
		    (find-character mark #\: :backward t))
	    (find-attribute mark :word-delimiter 'zerop)
	    (or (find-character point #\;) (line-end point))
	    (setf (mark-kind mark) :right-inserting)
	    (let* ((*parse-string-tables* (list (db:get-db-table)))
		   (*parse-value-must-exist* t)
		   (*parse-input-region* (region mark
						 (copy-mark point)))
		   (string (region-to-string *parse-input-region*)))
	      (let ((ret (edi::keyword-verification-function string)))
		(if (if ret
			(if (cadr ret)
			    (progn
			      (delete-region *parse-input-region*)
			      (insert-email point (cadr ret))
			      nil)
			    (if (eq (last-command-type)
				    :draft-addr-confirm)
				(string= (car ret) string)))
			(eq (last-command-type) :draft-addr-confirm))
		    ;; Ambiguous or completes (key :complete) to same
		    ;; string (FIX with point at same posn?)
		    (let ((mark2 (copy-mark point)))
		      (or (find-character mark2 #\;) (line-end mark2))
		      (let ((*parse-help* "Email address")
			    (*parse-type* :keyword)
			    (*parse-string-tables* (list (db:get-db-table)))
			    (*parse-input-region* (region mark mark2)))
			(help-on-parse-command)))))
	      (setf (last-command-type) :draft-addr-confirm)))))))

(defevar "Message Header Body Separator"
  "String that separates the header and body of a draft message."
  :value "--------")

(defcommand "Correct Message Spelling" ()
  "Correct spelling over mail, skipping cited lines."
  (let ((start (copy-mark (buffer-start-mark (current-buffer)))))
    (or (find-pattern start
		      (get-search-pattern
		       (value message-header-body-separator)
		       :forward))
	(editor-error "Failed to find start of message body."))
    (correct-buffer-spelling-command t start)))

(defcommand "Check and Deliver Message" ()
  "Correct message spelling, then call Deliver Message."
  (let ((dinfo (value draft-information)))
    (or dinfo (editor-error "Point must be in a draft buffer.")))
  (correct-message-spelling-command)
  (deliver-message-command)
  (rotate-buffers-forward-command)
  (refresh-all-headers-command) ;; In case in draft buffer.
  (message "Message checked and sent."))

(defcommand "Edit Draft" ()
  "Edit a prompted draft, prompting with the current draft."
  (show-prompted-message (mh:draft-folder)
			 (prompt-for-message
			  :folder (mh:draft-folder)
			  :prompt "Edit Draft: ")))

(defcommand "Draft Save File" ()
  "Save the "
  (if (probe-file (buffer-pathname (current-buffer)))
      (save-file-command)
      ;; FIX offer to save to a new draft.
      (error "Draft file missing.")))


#[ Convenience Commands for Message and Draft Buffers

This section describes how to switch from a `Message' or `Draft' buffer
to its associated `Headers' buffer, or from a `Draft' buffer to its
associated `Message' buffer.  There are also commands for various styles of
inserting text from a `Message' buffer into a `Draft' buffer.

{command:Goto Headers Buffer}
{command:Goto Message Buffer}
{command:Insert Message Region}
{evariable:Message Insertion Prefix}
{evariable:Message Insertion Column}
{command:Insert Message Buffer}
{evariable:Message Buffer Insertion Prefix}
{command:Edit Message Buffer}
]#


;;;; Message and Draft Stuff.

;; FIX other var (command?) same name
(defevar "Message Headers"
  "List of message headers to display in message buffers, or T for all."
  :value '("Resent-To" "To" "Resent-Cc" "Cc"
	   "Resent-From" "From" "Reply-To" "Date" "Subject"
	   "Resent-Date"))

(defevar "Message Headers Alternate"
  "List of alternate message headers to display in message buffers, or T
   for all."
  :value t)

(defevar "Headers Buffer"
  "This is bound in message and draft buffers to their associated headers
   buffer.")

(defcommand "Goto Headers Buffer" ()
  "When invoked in a `Message' or `Draft' buffer with an associated
   `Headers' buffer, select the associated `Headers' buffer, else rotate
   buffers.  Move the `Header' buffer's point to the appropriate line,
   pushing a buffer mark where point was."
  (let ((h-buf (value headers-buffer)))
    (if h-buf
	(if (buffer-windows h-buf)
	    (delete-window-command)
	    (let ((info (or (value message-information) (value draft-information))))
	      (change-to-buffer h-buf)
	      (push-buffer-mark (copy-mark (current-point)))
	      (move-mark (current-point) (message/draft-info-headers-mark info))))
	(rotate-buffers-forward-command))))

(defevar "Message Buffer"
  "This is bound in draft buffers to their associated message buffer.")

(defcommand "Goto Message Buffer" ()
  "When invoked in a `Draft' buffer with an associated `Message' buffer,
   select the associated `Message' buffer in the current window."
  (let ((msg-buf (value message-buffer)))
    (or msg-buf (editor-error "No associated message buffer."))
    (change-to-buffer msg-buf)))

(defevar "Message Insertion Prefix"
  "This is a fill prefix that is used when inserting text from a message
   buffer into a draft buffer by `Insert Message Region'.  It defaults to
   three spaces."
  :value "   ")

(defevar "Message Insertion Column"
  "This is a fill column that is used when inserting text from a message
   buffer into a draft buffer by `Insert Message Region'."
  :value 75)

(defcommand "Insert Message Region" (p)
  "Copy the current region into the associated draft or post buffer.  When
   in a message buffer that has an associated draft or post buffer, copy
   the current active region into the draft or post buffer.  Fill the
   region using *Message Insertion Prefix* and *Message Insertion Column*.
   If an argument is supplied, the filling is turned off.  If both a draft
   buffer and post buffer are associated with this, then insert it into the
   draft buffer."
  (let* ((minfo (value message-information))
	 (nm-info (if (editor-bound-p 'netnews-message-info)
		      (value netnews-message-info)))
	 (post-buffer (and nm-info (nm-info-post-buffer nm-info)))
	 (post-info (and post-buffer
			 (variable-value 'post-info :buffer post-buffer)))
	 dbuf kind)
    (cond (minfo
	   (setf kind :mail)
	   (setf dbuf (message-info-draft-buf minfo)))
	  (nm-info
	   (setf kind :netnews)
	   (setf dbuf (or (nm-info-draft-buffer nm-info)
			  (nm-info-post-buffer nm-info))))
	  (t (editor-error "Not in a netnews message or message buffer.")))
    (or dbuf
	(editor-error "Message buffer not associated with any draft or post ~
		       buffer."))
    (let* ((region (copy-region (current-region)))
	   (dbuf-point (buffer-point dbuf))
	   (dbuf-mark (copy-mark dbuf-point)))
      (cond ((and (eq kind :mail)
		  (editor-bound-p 'split-window-draft :buffer dbuf)
		  (> (length (the list *window-list*)) 2)
		  (buffer-windows dbuf))
	     (setf (current-buffer) dbuf
		   (current-window) (car (buffer-windows dbuf))))
	    ((and (eq kind :netnews)
		  (and (member (post-info-message-window post-info)
			       *window-list*)
		       (member (post-info-reply-window post-info)
			       *window-list*)))
	     (setf (current-buffer) dbuf
		   (current-window) (post-info-reply-window post-info)))
	    (t (change-to-buffer dbuf)))
      (push-buffer-mark dbuf-mark)
      (ninsert-region dbuf-point region)
      (or p
	  (fill-region-by-paragraphs (region dbuf-mark dbuf-point)
				     (value message-insertion-prefix)
				     (value message-insertion-column)))))
  (setf (last-command-type) :ephemerally-active))


(defevar "Message Buffer Insertion Prefix"
  "A line prefix that is inserted at the beginning of every line in a
   message buffer when inserting those lines into a draft buffer with
   \"Insert Message Buffer\"."
  :value "    ")

(defcommand "Insert Message Buffer" (p)
  "Insert the entire associated message buffer into a draft or post buffer.
   When in a draft or post buffer with an associated message buffer, or
   when in a message buffer that has an associated draft or post buffer,
   insert the message buffer into the draft buffer.  When there are both an
   associated draft and post buffer, insert the text into the draft buffer.
   Prefix each inserted line with *Message Buffer Insertion Prefix*.  If an
   argument is supplied then the prefix is \"\"."
  (let ((minfo (value message-information))
	(dinfo (value draft-information))
	mbuf dbuf message-kind)
    (cond (minfo
	   (setf message-kind :mail)
	   (setf dbuf (message-info-draft-buf minfo))
	   (or dbuf
	       (editor-error
		"Message buffer not associated with any draft buffer."))
	   (setf mbuf (current-buffer))
	   (change-to-buffer dbuf))
	  (dinfo
	   (setf message-kind :mail)
	   (setf mbuf (value message-buffer))
	   (or mbuf
	       (editor-error
		"Draft buffer not associated with any message buffer."))
	   (setf dbuf (current-buffer)))
	  ((editor-bound-p 'netnews-message-info)
	   (setf message-kind :netnews)
	   (setf mbuf (current-buffer))
	   (let ((nm-info (value netnews-message-info)))
	     (setf dbuf (or (nm-info-draft-buffer nm-info)
			    (nm-info-post-buffer nm-info)))
	     (or dbuf
		 (editor-error "Message buffer not associated with any draft ~
				or post buffer.")))
	   (change-to-buffer dbuf))
	  ((editor-bound-p 'post-info)
	   (setf message-kind :netnews)
	   (let ((post-info (value post-info)))
	     (setf mbuf (post-info-message-buffer post-info))
	     (or mbuf
		 (editor-error "Post buffer not associated with any message ~
				buffer.")))
	   (setf dbuf (current-buffer)))
	  (t (editor-error "Not in a draft, message, news-message, or post ~
	                    buffer.")))
    (let* ((dbuf-point (buffer-point dbuf))
	   (dbuf-mark (copy-mark dbuf-point)))
      (push-buffer-mark dbuf-mark)
      (insert-region dbuf-point (buffer-region mbuf))
      (or p
	  (let ((prefix (value message-buffer-insertion-prefix)))
	    (with-mark ((temp dbuf-mark :left-inserting))
	      (loop
		(when (mark>= temp dbuf-point) (return))
		(insert-string temp prefix)
		(or (line-offset temp 1 0) (return)))))))
    (ecase message-kind
      (:mail
       (insert-message-buffer-cleanup-split-draft dbuf mbuf))
      (:netnews
       (nn-reply-cleanup-split-windows dbuf))))
  (setf (last-command-type) :ephemerally-active))

;;; INSERT-MESSAGE-BUFFER-CLEANUP-SPLIT-DRAFT tries to delete an extra window
;;; due to "Reply to Message in Other Window".  Since we just inserted the
;;; message buffer in the draft buffer, we don't need the other window into
;;; the message buffer.
;;;
(defun insert-message-buffer-cleanup-split-draft (dbuf mbuf)
  (when (and (editor-bound-p 'split-window-draft :buffer dbuf)
	     (> (length (the list *window-list*)) 2))
    (let ((win (car (buffer-windows mbuf))))
      (cond
       (win
	(when (eq win (current-window))
	  (let ((dwin (car (buffer-windows dbuf))))
	    (or dwin
		(editor-error "Couldn't fix windows for split window draft."))
	    (setf (current-buffer) dbuf)
	    (setf (current-window) dwin)))
	(delete-window win))
       (t ;; This happens when invoked with the message buffer current.
	(let ((dwins (buffer-windows dbuf)))
	  (when (> (length (the list dwins)) 1)
	    (delete-window (find-if #'(lambda (w)
					(not (eq w (current-window))))
				    dwins)))))))
    (delete-variable 'split-window-draft :buffer dbuf)))


;;; CLEANUP-MESSAGE-BUFFER is called when a buffer gets deleted.  It cleans
;;; up references to a message buffer.
;;;
(defun cleanup-message-buffer (buffer)
  (let ((minfo (variable-value 'message-information :buffer buffer)))
    (when (editor-bound-p 'headers-buffer :buffer buffer)
      (let* ((hinfo (variable-value 'headers-information
				    :buffer (variable-value 'headers-buffer
							    :buffer buffer)))
	     (msg-buf (headers-info-msg-buffer hinfo)))
	(if (eq msg-buf buffer)
	    (setf (headers-info-msg-buffer hinfo) nil)
	    (setf (headers-info-other-msg-bufs hinfo)
		  (delete buffer (headers-info-other-msg-bufs hinfo)
			  :test #'eq))))
      (delete-mark (message-info-headers-mark minfo))
      ;;
      ;; Do this for MAYBE-MAKE-MH-BUFFER since it isn't necessary for GC.
      (delete-variable 'headers-buffer :buffer buffer))
    (when (and (message-info-draft-buf minfo)
	       (editor-bound-p 'message-buffer
			       :buffer (message-info-draft-buf minfo)))
      (delete-variable 'message-buffer
		       :buffer (message-info-draft-buf minfo)))))

;;; CLEANUP-DRAFT-BUFFER is called when a buffer gets deleted.  It cleans
;;; up references to a draft buffer.
;;;
(defun cleanup-draft-buffer (buffer)
  (let ((dinfo (variable-value 'draft-information :buffer buffer)))
    (when (editor-bound-p 'headers-buffer :buffer buffer)
      (let* ((hinfo (variable-value 'headers-information
				    :buffer (variable-value 'headers-buffer
							    :buffer buffer))))
	(setf (headers-info-draft-bufs hinfo)
	      (delete buffer (headers-info-draft-bufs hinfo) :test #'eq))
	(and dinfo
	     (draft-info-headers-mark dinfo)
	     (delete-mark (draft-info-headers-mark dinfo)))))
    (when (editor-bound-p 'message-buffer :buffer buffer)
      (setf (message-info-draft-buf
	     (variable-value 'message-information
			     :buffer (variable-value 'message-buffer
						     :buffer buffer)))
	    nil))))

;;; CLEANUP-HEADERS-BUFFER is called when a buffer gets deleted.  It cleans
;;; up references to a headers buffer.
;;;
(defun cleanup-headers-buffer (buffer)
  (if (editor-bound-p 'headers-information :buffer buffer)
      (let* ((hinfo (variable-value 'headers-information :buffer buffer))
	     (msg-buf (headers-info-msg-buffer hinfo)))
	(when msg-buf
	  (cleanup-headers-reference
	   msg-buf (variable-value 'message-information :buffer msg-buf)))
	(dolist (b (headers-info-other-msg-bufs hinfo))
	  (cleanup-headers-reference
	   b (variable-value 'message-information :buffer b)))
	(dolist (b (headers-info-draft-bufs hinfo))
	  (cleanup-headers-reference
	   b (variable-value 'draft-information :buffer b))))))

(defun cleanup-headers-reference (buffer info)
  (delete-mark (message/draft-info-headers-mark info))
  (setf (message/draft-info-headers-mark info) nil)
  (delete-variable 'headers-buffer :buffer buffer)
  (when (typep info 'draft-info)
    (setf (draft-info-replied-to-folder info) nil)
    (setf (draft-info-replied-to-msg info) nil)))

;;; REVAMP-HEADERS-BUFFER cleans up a headers buffer for immediate re-use.
;;; After deleting the buffer's region, there will be one line in the
;;; buffer because of how the editor regions work, so we have to delete
;;; that line's plist.  Then we clean up any references to the buffer and
;;; delete the main message buffer.  The other message buffers are left
;;; alone assuming they are on the "others" list because they are being
;;; used in some particular way (for example, a draft buffer refers to one
;;; or the user has kept it).  Then some slots of the info structure are
;;; set to nil.
;;;
(defun revamp-headers-buffer (hbuffer hinfo)
  (delete-region (buffer-region hbuffer))
  (setf (line-plist (mark-line (buffer-point hbuffer))) nil)
  (let ((msg-buf (headers-info-msg-buffer hinfo)))
    ;; Deleting the buffer sets the slot to nil.
    (when msg-buf (delete-buffer-if-possible msg-buf))
    (cleanup-headers-buffer hbuffer))
  (setf (headers-info-other-msg-bufs hinfo) nil)
  (setf (headers-info-draft-bufs hinfo) nil)
  (setf (headers-info-msg-seq hinfo) nil)
  (setf (headers-info-msg-strings hinfo) nil))

(defcommand "Append File" ()
  "Append a file at the end of the buffer."
  (let ((name (prompt-for-file
	       :default (buffer-default-pathname (current-buffer))
	       :prompt "Append file: "
	       :help "Name of file to append to message"))
	(point (current-point)))
    (buffer-end point)
    (mark-before point)
    (or (blank-line-p (mark-line point))
	(insert-character point #\newline))
    (mark-after point)
    (with-output-to-mark (stream point)
      (format stream
	      "~&== File ~A ====================================================~%"
	      name)
      ;; FIX insert file manually, and add undo
      (insert-file-command nil name)
      (format stream
	      "== End of file ~A =============================================~%"
	      name))))

(defcommand "Attach File" ()
  "Insert file attachment clause at point."
  (let ((name (prompt-for-file
	       :default (buffer-default-pathname (current-buffer))
	       :prompt "Attach file: "
	       :help "Name of file to attach to message."))
	(type (prompt-for-string
	       :default "text/plain"
	       :prompt "Content type: "
	       :help "MIME type of the attachment."))
	(description (prompt-for-string
		      :prompt "Description: "
		      :help "One line description of the attachment."))
	(mark (copy-mark (current-point))))
    (or (blank-line-p (mark-line mark))
	(insert-character mark #\newline))
    (with-output-to-mark (stream mark)
      (format stream "#~A [~A] ~A" type description name))))


#[ Reading New Mail

{command:Incorporate and Read New Mail}
{command:Incorporate New Mail}
{evariable:New Mail Folder}
{evariable:Unseen Headers Message Spec}
{evariable:Incorporate New Mail Hook}
]#

#|
FIX
{evariable:Store Password}
     When this is set, the user is only prompted once for his password, and the
     password is stored for future use.
{evariable:Authenticate Incorporation}
{evariable:Authentication User Name}
     When `Authenticate Incorporation' is set, incorporating new mail prompts
     for a password to access a remote mail-drop.

     When incorporating new mail accesses a remote mail-drop, `Authentication
User Name' is the user name supplied for authentication on the remote machine.
     If this is nil, the editor uses the local name.
|#


;;;; Periodic mail checking.

;; FIX a whole new process just to check for mail

(defvar *mail-checker* ()
  "Slave that checks for mail.")

(defun update-new-mail-status (arg)
  (declare (ignore arg))
  (when *mail-checker*
    (or (server-info-notes *mail-checker*)
	(if (eval-form-in-server-1 *mail-checker*
				   "ed::*new-mail-p*"
				   "USER")
	    (progn
	      (setq *new-mail-p* t)
	      (if (modeline-field :mail)
		  (update-modeline-field *echo-area-buffer*
					 *echo-area-window*
					 (modeline-field :mail))))))))

(defun check-new-mail-p (seconds)
  "Call new-mail-p."
  (declare (ignore seconds))
  (when *mail-checker*
    (or (server-info-notes *mail-checker*)
	(if (eval-form-in-server-1 *mail-checker*
				   "ed::*new-mail-p*"
				   "USER")
	    (progn
	      (setq *new-mail-p* t)
	      (if (modeline-field :mail)
		  (update-modeline-field *echo-area-buffer*
					 *echo-area-window*
					 (modeline-field :mail))))
	    (string-eval (format () "(ignore-errors
				       (or ed::*new-mail-p*
				           (setq ed::*new-mail-p*
				                 (mh:new-mail-p (mh:make-drops '~S)))))"
				 (value mail-drops))
			 :server *mail-checker*
			 :package "USER"
			 :complete-hook '(update-new-mail-status)
			 :quiet t)))))

(defun update-mail-check-period (name &optional (kind :current)
				      where value)
  (declare (ignore name kind where))
  (remove-scheduled-function #'check-new-mail-p)
  (when value
    (or *mail-checker*
	(setq *mail-checker*
	      (elet ((confirm-slave-creation)
		     (input-wait-alarm))
		(let ((eval-server (variable-value 'current-eval-server
						   :global)))
		  ;; FIX check if such a buffer exists
		  (prog1 (create-slave "Mail Checker" t)
		    (setf (variable-value 'current-eval-server :global)
			  eval-server))))))
    (schedule-event t #'check-new-mail-p value t)))

(defevar "Mail Check Period"
  "The number of seconds between checks for new mail.  () to turn off
   periodic mail checking."
  :hooks '(update-mail-check-period))


;;;; Incorporating new mail.

(defevar "New Mail Folder"
  "This is the folder new mail is incorporated into."
  :value "+inbox")

(defcommand "Incorporate New Mail" ()
  "Incorporate new mail into *New Mail Folder*, showing any mail protocol
   specific output in a pop-up window."
  (with-pop-up-display (s)
    (incorporate-new-mail s)))

(defevar "Unseen Headers Message Spec"
  "This is an MH message specification that is suitable for any message
   prompt.  It is used to supply headers for the unseen headers buffer, in
   addition to the unseen-sequence name that is taken from the user's MH
   profile, when incorporating new mail and after expunging.  This value is
   a string.")

(defevar "Read New Mail Style"
  "What happens after incorporating mail in `Incorporate and Read New
   Mail'.

   If :folders, then change to the folders buffer, otherwise change to a
   headers buffer listing all the new mail in *New Mail Folder*.

   :folders is suited to use with a *Split Mail Rule*.")

(defcommand "Incorporate and Read New Mail" ()
  "Incorporate new mail into `New Mail Folder' and generate a headers
   buffer with the new messages.  Require an MH profile unseen-sequence.
   Add the new messages to the unseen-sequence.  Position the buffer's
   point on the headers line representing the first newly incorporated
   message."
  (let ((unseen-seq (or (mh:profile-component "unseen-sequence")
			(mh:add-profile-component "unseen-sequence"
						  "in"))))
    (incorporate-new-mail)
    (case (value read-new-mail-style)
      (:folders
       (browse-folders-command)
       (refresh-folders-command)
       (if (eq (value folders-contents) :all)
	   (mail-browse-toggle-new-command)))
      (t
       ;; FIX update *folders-buffer* to match
       (let* ((folder (value new-mail-folder))
	      ;; Stash current message before fetching unseen headers.
	      (cur-msg (mh:current-message folder))
	      (region (get-new-mail-msg-hdrs folder unseen-seq)))
	 ;; Fetch message headers before possibly making buffer in case we
	 ;; error.
	 (or (and *new-mail-buffer*
		  (member *new-mail-buffer* *buffer-list* :test #'eq))
	     (let ((name (format () "Unseen Headers ~A" folder)))
	       (if (getstring name *buffer-names*)
		   (editor-error "There already is a buffer named ~S." name))
	       (setf *new-mail-buffer*
		     (make-buffer name :modes (list "Headers")
				  :delete-hook '(new-mail-buf-delete-hook)))
	       (setf (buffer-writable *new-mail-buffer*) ())))
	 (cond ((editor-bound-p 'headers-information
				:buffer *new-mail-buffer*)
		(let ((hinfo (variable-value 'headers-information
					     :buffer *new-mail-buffer*)))
		  (or (string= (headers-info-folder hinfo) folder)
		      (editor-error
		       "An unseen headers buffer already exists but into another ~
			folder.  Your mail has already been incorporated into the ~
			specified folder."))
		  (with-writable-buffer (*new-mail-buffer*)
		    (revamp-headers-buffer *new-mail-buffer* hinfo))
		  ;; Restore the name in case someone used "Pick Headers".
		  (setf (buffer-name *new-mail-buffer*)
			(format () "Unseen Headers ~A" folder))
		  (insert-new-mail-message-headers hinfo region (string cur-msg))))
	       (t
		(let ((hinfo (make-headers-info :buffer *new-mail-buffer*
						:folder folder)))
		  (defevar "Headers Information"
		    "This holds the information about the current headers buffer."
		    :value hinfo :buffer *new-mail-buffer*)
		  (insert-new-mail-message-headers hinfo region (string cur-msg))))))))))

;;; NEW-MAIL-BUF-DELETE-HOOK is invoked whenever the new mail buffer is
;;; deleted.
;;;
(defun new-mail-buf-delete-hook (buffer)
  (declare (ignore buffer))
  (setf *new-mail-buffer* nil))

;;; GET-NEW-MAIL-MSG-HDRS takes a folder and the unseen-sequence name.  It
;;; returns a region with the unseen message headers and any headers due to
;;; the "Unseen Headers Message Spec" variable.
;;;
(defun get-new-mail-msg-hdrs (folder unseen-seq)
  (let* ((unseen-headers-message-spec (value unseen-headers-message-spec))
	 (other-msgs (if unseen-headers-message-spec
			 (breakup-message-spec
			  (string-trim '(#\space #\tab)
				       unseen-headers-message-spec))))
	 (msg-spec (cond ((null other-msgs)
			  (list unseen-seq))
			 ((member unseen-seq other-msgs :test #'string=)
			  other-msgs)
			 (t (cons unseen-seq other-msgs)))))
    (message-headers-to-region folder msg-spec)))

;;; INSERT-NEW-MAIL-MESSAGE-HEADERS inserts region in the new mail buffer.
;;; Then we look for the header line with cur-msg id, moving point there.
;;; There may have been unseen messages before incorporating new mail, and
;;; cur-msg should be the first new message.  Then we either switch to the
;;; new mail headers, or show the current message.
;;;
(defun insert-new-mail-message-headers (hinfo region cur-msg)
  (declare (simple-string cur-msg))
  (with-writable-buffer (*new-mail-buffer*)
    (insert-message-headers *new-mail-buffer* hinfo region))
  (let ((point (buffer-point *new-mail-buffer*)))
    (buffer-start point)
    (with-headers-mark (cur-mark *new-mail-buffer* cur-msg)
      (move-mark point cur-mark)))
  (change-to-buffer *new-mail-buffer*))

(defevar "Incorporate New Mail Hook"
  "A list of functions which are invoked immediately after new mail is
   incorporated.")

(defevar "Mail Drops"
  "The list of mail drops.") ;; FIX explain adding

(defvar *new-mail-p* ()
  "Set to t in `new-mail-p' when there is mail.  Cleared when mail is
   incorporated.  Used for :mail modeline field.")

(defun reset-new-mail-flags ()
  (setq *new-mail-p* ())
  (if *mail-checker*
      (or (server-info-notes *mail-checker*)
	  (eval-form-in-server-1 *mail-checker*
				 "(setq ed::*new-mail-p* ())"
				 "USER"))))

(defun incorporate-new-mail (&optional stream)
  "Incorporate new mail, passing any protocol output to $stream.  When
   $stream is (), flush any output."
  (or (new-mail-p)
      (progn
	(reset-new-mail-flags)
	(when (modeline-field :mail)
	  (update-modeline-field *echo-area-buffer* *echo-area-window*
				 (modeline-field :mail)))
	(editor-error "Any mail already in.")))
  (message "Incorporating new mail ...")
  (if (mh:incorporate (mh:make-drops (value mail-drops))
			(value new-mail-folder)
			stream)
      (progn
	;; FIX inc should only ret t if there was mail and it failed
	;;       so if some drop fails in new-mail-p too it should ret ()
	(reset-new-mail-flags)
	(when (modeline-field :mail)
	  (update-modeline-field *echo-area-buffer* *echo-area-window*
				 (modeline-field :mail))))
      (message "Failed to incorporate some mail."))
  (when (value incorporate-new-mail-hook)
    (message "Invoking new mail hooks ...")
    (invoke-hook incorporate-new-mail-hook)))

;; FIX ~ Auto Refile Mail Rule? Refile Headers rule.
(defevar "Split Mail Rule"
  "A list of (\"folder\" pick-expression) lists, to control mail
   splitting."
  :value ())

(defun symbols-to-keywords (list)
  "Returns a list like List with symbols converted to keywords."
  (typecase list
    (symbol (lisp::make-keyword list))
    (string list)
    (list  (mapcar 'symbols-to-keywords list))
    (t list)))

(defcommand "Split Mail" (p (folder (value new-mail-folder))
			    (pick (list (mh:profile-component
					 "unseen-sequence"))))
  "Split the messages specified by $pick in $folder into folders, according
   to *Split Mail Rule*."
  (declare (ignore p))
  ;; FIX update *folders-buffer* to match new msgs
  ;;       redundant for some configs
  ;;           eg incorp, split, refresh Folders (refr F does it too)
  (let ((rules (value split-mail-rule)))
    (when rules
      (message "Splitting mail from folder ~A..." folder)
      (mh:split-messages folder pick rules #'refile-message)
      (message "... done."))))


#[ Deleting Messages

The main command described in this section is `Headers Delete Message'
(bound to k in `Headers' and `Message' modes).  A useful command
for reading new mail is `Delete Message and Show Next' (bound to d in
`Message' mode) which deletes the current message and shows the next
undeleted message.

Since messages are by default deleted using a virtual message deletion
mechanism, `Expunge Messages' (bound to ! in `Headers' mode)
should be mentioned here.  This is described in section [terminating].

{evariable:Virtual Message Deletion}
{command:Delete Message}
{command:Headers Delete Message}
{command:Delete Message and Show Next}
{command:Delete Message and Down Line}
{command:Undelete Message}
{command:Headers Undelete Message}
]#


;;;; Deletion.

(defevar "Virtual Message Deletion"
  "When set, `Delete Message' merely mark's a message into the \"edtrash\"
   sequence; otherwise, it deletes the message."
  :value t)

(defcommand "Delete Message and Show Next" ()
  "Delete the current message and show the next undeleted message.  This
   command is only valid in a headers buffer or a message buffer associated
   with some headers buffer."
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Not on a header line."))
	     (delete-mark cur-mark)
	     (delete-message (headers-info-folder hinfo) cur-msg)))
	  (minfo
	   (or (message-info-folder minfo)
	       (editor-error "Attempt to delete temporary message."))
	   (delete-message (message-info-folder minfo)
			   (message-info-msgs minfo)))
	  (t
	   (editor-error "Not in a headers or message buffer."))))
  (show-message-offset 1 :undeleted))

;;; "Delete Message" unlike "Headers Delete Message" cannot know for sure
;;; which message id's have been deleted, so when virtual message deletion
;;; is not used, we cannot use DELETE-HEADERS-BUFFER-LINE to keep headers
;;; buffers consistent.  However, the message id's in the buffer (if deleted)
;;; will generate MH errors if operations are attempted with them, and
;;; if the user ever packs the folder with "Expunge Messages", the headers
;;; buffer will be updated.
;;;
(defcommand "Delete Message" ()
  "Delete prompted messages.

   When `Virtual Message Deletion' is set, merely flags the messages for
   deletion by adding them to the \"edtrash\" sequence and updates
   any `Headers' buffers representing the folder.  Notate each headers line
   referring to a deleted message with a \"D\" in the third character
   position after the message ID.  `Expunge Messages' can be invoked to
   delete these messages.

   When `Virtual Message Deletion' is (), delete each message, updating the
   headers buffer.  In this case headers and message buffers message id's
   may go out of sync with MH."  ;; FIX
  (let* ((folder (prompt-for-folder))
	 (hinfo (value headers-information))
	 (temp-msgs (prompt-for-message
		     :folder folder
		     :messages
		     (if (and hinfo
			      (string= folder
				       (the simple-string
					    (headers-info-folder hinfo))))
			 (headers-info-msg-strings hinfo))
		     :prompt "MH messages to pick from: "))
	 (pick-exp (prompt-for-pick-expression))
	 (msgs (mh:pick-messages folder temp-msgs pick-exp))
	 (virtually (value virtual-message-deletion)))
    (declare (simple-string folder))
    (if virtually
	(mh:mark-messages folder msgs "edtrash" :add)
	(delete-messages folder msgs ()))
    (if virtually
	(let ((deleted-seq (mh:sequence-list folder "edtrash")))
	  (when deleted-seq
	    (do-headers-buffers (hbuf folder)
	      (with-writable-buffer (hbuf)
		(note-deleted-headers (buffer-region hbuf) deleted-seq)))))
	(do-headers-buffers (hbuf folder hinfo)
	  (do-headers-lines ((buffer-region hbuf) :line-var line :mark-var hmark)
	    (when (member (parse-integer (line-message-id line)) msgs)
	      (delete-headers-buffer-line hinfo hmark)))))))

(defcommand "Headers Delete Message" ()
  "Delete current message.  When in a headers buffer, deletes the message
   on the current line.  When in a message buffer, deletes the message in
   the buffer.  When *Virtual Message Deletion* is set, messages are only
   MARK'ed for deletion.  These messages can then be deleted with `Expunge
   Messages'."
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Point must be on a header line."))
	     (delete-mark cur-mark)
	     (delete-message (headers-info-folder hinfo) cur-msg)))
	  (minfo
	   (or (message-info-folder minfo)
	       (editor-error "Attempt to delete temporary message."))
	   (let ((msgs (message-info-msgs minfo)))
	     (delete-message (message-info-folder minfo)
			     (if (consp msgs) (car msgs) msgs)))
	   (message "Message deleted."))
	  (t (editor-error "Point must be in a headers or message buffer.")))))

(defcommand "Headers Delete Message and Down Line" ()
  "Delete the current message, moving point to the next line."
  (let ((hinfo (value headers-information)))
    (or hinfo (editor-error "Point must be in a headers buffer."))
    (multiple-value-bind (cur-msg cur-mark)
			 (headers-current-message hinfo)
      (or cur-msg (editor-error "Point must be on a header line."))
      (delete-message (headers-info-folder hinfo) cur-msg)
      (if (line-offset cur-mark 1)
	  (or (blank-line-p (mark-line cur-mark))
	      (move-mark (current-point) cur-mark)))
      (delete-mark cur-mark))))

;;; DELETE-MESSAGE takes a folder and message id and either flags this message
;;; for deletion or deletes it.  All headers buffers into folder are updated,
;;; either by flagging a headers line or deleting it.
;;;
(defun delete-message (folder msg)
  (cond ((value virtual-message-deletion)
	 (mh:mark-message folder msg "edtrash" :add)
	 (do-headers-buffers (hbuf folder)
	   (with-headers-mark (hmark hbuf msg)
	     (with-writable-buffer (hbuf)
	       (note-deleted-message-at-mark hmark)))))
	(t (delete-messages folder (list msg) ())
	   (do-headers-buffers (hbuf folder hinfo)
	     (with-headers-mark (hmark hbuf msg)
	       (delete-headers-buffer-line hinfo hmark)))))
  (dolist (b *buffer-list*)
    (when (and (editor-bound-p 'message-information :buffer b)
	       (buffer-modeline-field-p b :deleted-message))
      (dolist (w (buffer-windows b))
	(update-modeline-field b w :deleted-message)))))

;;; NOTE-DELETED-MESSAGE-AT-MARK takes a mark at the beginning of a valid
;;; headers line, sticks a "D" on the line, and frobs the line's deleted
;;; property.  This assumes the headers buffer is modifiable.
;;;
(defun note-deleted-message-at-mark (mark)
  (find-attribute mark :digit)
  (find-attribute mark :digit #'zerop)
  (character-offset mark 2)
  (setf (next-character mark) #\D)
  (setf (line-message-deleted (mark-line mark)) t))

;;; DELETE-HEADERS-BUFFER-LINE takes a headers information and a mark on the
;;; line to be deleted.  Before deleting the line, we check to see if any
;;; message or draft buffers refer to the buffer because of the line.  Due
;;; to how regions are deleted, line plists get messed up, so they have to
;;; be regenerated.  We regenerate them for the whole buffer, so we don't have
;;; to hack the code to know which lines got messed up.
;;;
(defun delete-headers-buffer-line (hinfo hmark)
  (delete-headers-line-references hinfo hmark)
  (let ((id (line-message-id (mark-line hmark)))
	(hbuf (headers-info-buffer hinfo)))
    (with-writable-buffer (hbuf)
      (with-mark ((end (line-start hmark) :left-inserting))
	(or (line-offset end 1 0) (buffer-end end))
	(delete-region (region hmark end))))
    (let ((seq (mh:sequence-delete id (headers-info-msg-seq hinfo))))
      (setf (headers-info-msg-seq hinfo) seq)
      (setf (headers-info-msg-strings hinfo) (mh:sequence-strings seq)))
    (set-message-headers-ids (buffer-region hbuf))
    (when (value virtual-message-deletion)
      (let ((deleted-seq (mh:sequence-list (headers-info-folder hinfo)
					   "edtrash")))
	(do-headers-lines ((buffer-region hbuf) :line-var line)
	  (setf (line-message-deleted line)
		(mh:sequence-member-p (line-message-id line) deleted-seq)))))))


;;; DELETE-HEADERS-LINE-REFERENCES removes any message buffer or draft buffer
;;; pointers to a headers buffer or marks into the headers buffer.  Currently
;;; message buffers and draft buffers are identified differently for no good
;;; reason; probably message buffers should be located in the same way draft
;;; buffers are.  Also, we currently assume only one of other-msg-bufs could
;;; refer to the line (similarly for draft-bufs), but this might be bug
;;; prone.  The message buffer case couldn't happen since the buffer name
;;; would cause MAYBE-MAKE-MH-BUFFER to re-use the buffer, but you could reply
;;; to the same message twice simultaneously.
;;;
(defun delete-headers-line-references (hinfo hmark)
  (let ((msg-id (line-message-id (mark-line hmark)))
	(main-msg-buf (headers-info-msg-buffer hinfo)))
    (declare (simple-string msg-id))
    (when main-msg-buf
      (let ((minfo (variable-value 'message-information :buffer main-msg-buf)))
	(when (string= (the simple-string (message-info-msgs minfo))
		       msg-id)
	  (cond ((message-info-draft-buf minfo)
		 (cleanup-headers-reference main-msg-buf minfo)
		 (setf (headers-info-msg-buffer hinfo) nil))
		(t (delete-buffer-if-possible main-msg-buf))))))
    (dolist (mbuf (headers-info-other-msg-bufs hinfo))
      (let ((minfo (variable-value 'message-information :buffer mbuf)))
	(when (string= (the simple-string (message-info-msgs minfo))
		       msg-id)
	  (cond ((message-info-draft-buf minfo)
		 (cleanup-headers-reference mbuf minfo)
		 (setf (headers-info-other-msg-bufs hinfo)
		       (delete mbuf (headers-info-other-msg-bufs hinfo)
			       :test #'eq)))
		(t (delete-buffer-if-possible mbuf)))
	  (return)))))
  (dolist (dbuf (headers-info-draft-bufs hinfo))
    (let ((dinfo (variable-value 'draft-information :buffer dbuf)))
      (when (same-line-p (draft-info-headers-mark dinfo) hmark)
	(cleanup-headers-reference dbuf dinfo)
	(setf (headers-info-draft-bufs hinfo)
	      (delete dbuf (headers-info-draft-bufs hinfo) :test #'eq))
	(return)))))

(defcommand "Undelete Message" (p)  ;; Restore?
  "If `Virtual Message Deletion' is set then clear the delete flag on a set
   of prompted messages, updating all the messages `Headers' buffers."
  (declare (ignore p))
  (or (value virtual-message-deletion)
      (editor-error "Only possible with virtual message deletion."))
  (let* ((folder (prompt-for-folder))
	 (hinfo (value headers-information))
	 (temp-msgs (prompt-for-message
		     :folder folder
		     :messages
		     (if (and hinfo
			      (string= folder
				       (the simple-string
					    (headers-info-folder hinfo))))
			 (headers-info-msg-strings hinfo))
		     :prompt "Messages to pick from: "))
	 (pick-exp (prompt-for-pick-expression))
	 (msgs (or (if pick-exp
		       (mh:pick-messages folder temp-msgs pick-exp))
		   temp-msgs)))
    (declare (simple-string folder))
    (mh:mark-messages folder msgs "edtrash" :delete)
    (let ((deleted-seq (mh:sequence-list folder "edtrash")))
      (do-headers-buffers (hbuf folder)
	(with-writable-buffer (hbuf)
	  (do-headers-lines ((buffer-region hbuf) :line-var line :mark-var hmark)
	    (when (and (line-message-deleted line)
		       (not (mh:sequence-member-p (line-message-id line)
						  deleted-seq)))
	      (note-undeleted-message-at-mark hmark))))))))

(defcommand "Headers Undelete Message" ()
  "If `Virtual Message Deletion' is set then clear the delete the current
   message.  When in a headers buffer, undeletes the message on the current
   line.  When in a message buffer, undeletes the message in the buffer."
  (or (value virtual-message-deletion)
      (editor-error "You don't use virtual message deletion."))
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Not on a header line."))
	     (delete-mark cur-mark)
	     (undelete-message (headers-info-folder hinfo) cur-msg)))
	  (minfo
	   (or (message-info-folder minfo)
	       (editor-error "Attempt to undelete temporary message."))
	   (undelete-message (message-info-folder minfo)
			     (message-info-msgs minfo))
	   (message "Message undeleted."))
	  (t (editor-error "Not in a headers or message buffer.")))))

(defcommand "Headers Undelete Message and Down Line" ()
  "Undelete the current message.  When in a headers buffer, undeletes the
   message on the current line and moves down a line.  When in a message
   buffer, undeletes that message.  This command is only meaningful if you
   have *Virtual Message Deletion* set."
  "When in a headers buffer, undeletes the message on the current line.
   When in a message buffer, undeletes that message.  This command is only
   meaningful if you have *Virtual Message Deletion* set."
  (or (value virtual-message-deletion)
      (editor-error "Only fitting with virtual message deletion."))
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Point must be on a header line."))
	     (undelete-message (headers-info-folder hinfo) cur-msg)
	     (when (line-offset cur-mark 1)
	       (or (blank-line-p (mark-line cur-mark))
		   (move-mark (current-point) cur-mark)))
	     (delete-mark cur-mark)))
	  (minfo
	   (undelete-message (message-info-folder minfo)
			     (message-info-msgs minfo))
	   (message "Message undeleted."))
	  (t (editor-error "Point must be in a headers or message buffer.")))))

;;; UNDELETE-MESSAGE takes a folder and a message id.  All headers buffers into
;;; folder are updated.
;;;
(defun undelete-message (folder msg)
  (mh:mark-message folder msg "edtrash" :delete)
  (do-headers-buffers (hbuf folder)
    (with-headers-mark (hmark hbuf msg)
      (with-writable-buffer (hbuf)
	(note-undeleted-message-at-mark hmark))))
  (dolist (b *buffer-list*)
    (when (and (editor-bound-p 'message-information :buffer b)
	       (buffer-modeline-field-p b :deleted-message))
      (dolist (w (buffer-windows b))
	(update-modeline-field b w :deleted-message)))))

;;; NOTE-UNDELETED-MESSAGE-AT-MARK takes a mark at the beginning of a valid
;;; headers line, sticks a space on the line in place of a "D", and frobs the
;;; line's deleted property.  This assumes the headers buffer is modifiable.
;;;
(defun note-undeleted-message-at-mark (hmark)
  (find-attribute hmark :digit)
  (find-attribute hmark :digit #'zerop)
  (character-offset hmark 2)
  (setf (next-character hmark) #\space)
  (setf (line-message-deleted (mark-line hmark)) nil))

(defcommand "Expunge Messages" ()
  "Expunge messages marked for deletion and compact (pack) the message IDs
   in the current folder.  If `Expunge Messages Confirm' is set then ask
   for confirmation before expunging.  If `Temporary Draft Folder' is set
   then expunge messages in that folder.

   Regenerate all the headers in the current folder.  Collapse multiple
   `Headers' buffers for the same folder into one buffer.  Kill any
   `Message' buffers associated with these `Headers' buffers."
  (let* ((hinfo (value headers-information))
	 (minfo (value message-information))
	 (folder (cond (hinfo (headers-info-folder hinfo))
		       (minfo (or (message-info-folder minfo)
				  (prompt-for-folder)))
		       (t (prompt-for-folder))))
	 (deleted-seq (mh:sequence-list folder "edtrash")))
    ;;
    ;; Delete the messages if there are any.
    ;; This deletes "edtrash" from sequence file; we don't have to. (FIX ?)
    (when (and deleted-seq
	       (or (not (value expunge-messages-confirm))
		   (prompt-for-y-or-n
		    :prompt (list "Expunge messages and pack folder ~A? "
				  folder)
		    :default t
		    :default-string "Y")))
      (delete-messages folder '("edtrash"))
      ;;
      ;; Compact the message id's after deletion.
      (let ((*standard-output* *mh-utility-bit-bucket*))
	(message "Compacting folder ...")
	(mh:pack-folder folder))
      ;;
      ;; Maintain much consistency.
      (let ((new-buf-p (eq (current-buffer) *new-mail-buffer*)))
	(message "Maintaining consistency ...")
	(expunge-messages-fold-headers-buffers folder)
	(expunge-messages-fix-draft-buffers folder)
	(expunge-messages-fix-unseen-headers folder)
	(when new-buf-p (change-to-buffer *new-mail-buffer*)))
      (delete-and-expunge-temp-drafts))))

;;; EXPUNGE-MESSAGES-FOLD-HEADERS-BUFFERS deletes all headers buffers into the
;;; compacted folder.  We can only update the headers buffers by installing all
;;; headers, so there may as well be only one such buffer.  First we get a list
;;; of the buffers since DO-HEADERS-BUFFERS is trying to iterate over a list
;;; being destructively modified by buffer deletions.
;;;
(defun expunge-messages-fold-headers-buffers (folder)
  (let (hbufs)
    (declare (list hbufs))
    (do-headers-buffers (b folder)
      (or (eq b *new-mail-buffer*)
	  (push b hbufs)))
    (unless (zerop (length hbufs))
      (dolist (b hbufs)
	(delete-headers-buffer-and-message-buffers-command nil b))
      (new-message-headers folder (list "highest")
			   () (1+ (- (window-height (current-window))))))))

;;; EXPUNGE-MESSAGES-FIX-DRAFT-BUFFERS finds any draft buffer that was set up
;;; as a reply to some message in folder, removing this relationship in case
;;; that message id does not exist after expunge folder compaction.
;;;
(defun expunge-messages-fix-draft-buffers (folder)
  (declare (simple-string folder))
  (dolist (b *buffer-list*)
    (when (editor-bound-p 'draft-information :buffer b)
      (let* ((dinfo (variable-value 'draft-information :buffer b))
	     (reply-folder (draft-info-replied-to-folder dinfo)))
	(when (and reply-folder
		   (string= (the simple-string reply-folder) folder))
	  (setf (draft-info-replied-to-folder dinfo) nil)
	  (setf (draft-info-replied-to-msg dinfo) nil))))))

;;; EXPUNGE-MESSAGES-FIX-UNSEEN-HEADERS specially handles the unseen headers
;;; buffer apart from the other headers buffers into the same folder when
;;; messages have been expunged.  We must delete the associated message buffers
;;; since REVAMP-HEADERS-BUFFER does not, and these potentially reference bad
;;; message id's.  When doing this we must copy the other-msg-bufs list since
;;; the delete buffer cleanup hook for them is destructive.  Then we check for
;;; more unseen messages.
;;;
(defun expunge-messages-fix-unseen-headers (folder)
  (declare (simple-string folder))
  (when *new-mail-buffer*
    (let ((hinfo (variable-value 'headers-information
				 :buffer *new-mail-buffer*)))
      (when (string= (the simple-string (headers-info-folder hinfo))
		     folder)
	(let ((other-bufs (copy-list (headers-info-other-msg-bufs hinfo))))
	  (dolist (b other-bufs) (delete-buffer-if-possible b)))
	(with-writable-buffer (*new-mail-buffer*)
	  (revamp-headers-buffer *new-mail-buffer* hinfo)
	  ;; Restore the name in case someone used "Pick Headers".
	  (setf (buffer-name *new-mail-buffer*)
		(format nil "Unseen Headers ~A" folder))
	  (let ((region (maybe-get-new-mail-msg-hdrs folder)))
	    (when region
	      (insert-message-headers *new-mail-buffer* hinfo region))))))))

;;; MAYBE-GET-NEW-MAIL-MSG-HDRS returns a region suitable for a new mail buffer
;;; or nil.  Folder is probed for unseen headers, and if there are some, then
;;; we call GET-NEW-MAIL-MSG-HDRS which also uses "Unseen Headers Message Spec".
;;; If there are no unseen headers, we only look for "Unseen Headers Message
;;; Spec" messages.  We go through these contortions to keep MH from outputting
;;; errors.
;;;
(defun maybe-get-new-mail-msg-hdrs (folder)
  (let ((unseen-seq-name (mh:profile-component "unseen-sequence")))
    (multiple-value-bind (unseen-seq foundp)
			 (mh:sequence-list folder unseen-seq-name)
      (if (and foundp unseen-seq)
	  (get-new-mail-msg-hdrs folder unseen-seq-name)
	  (let ((spec (value unseen-headers-message-spec)))
	    (when spec
	      (message-headers-to-region
	       folder
	       (breakup-message-spec (string-trim '(#\space #\tab) spec)))))))))

(defcommand "Empty Trash" ()
  "Empty the trash folder."
  (if (prompt-for-yes-or-no
       :prompt (list "Release all messages in folder \"~A\"? (yes or no) "
		     (mh:strip-folder-name (value trash-folder)))
       :help "Confirm release of all messages in folder."
       :default-string "No"
       :default ())
      (progn
	(message "Emptying trash folder...")
	(delete-messages (value trash-folder) '(:all))
	(message "Maintaining consistency ...")
	(let (refresh)
	  (do-headers-buffers (buffer "trash")
	    (setq refresh t))
	  (new-message-headers "trash" '("all"))))
      (message "Cancelled.")))


#[ Folder Operations

{command:List Folders}
{command:Create Folder}
{command:Delete Folder}
]#


;;;; Folders.

(defcommand "List Folders" ()
  "Pop up a list of all current mail folders in the top-level mail
   directory."
  (with-pop-up-display (stream)
    (mh:print-folders stream)))

(defcommand "Update Folders" ()
  "Update folder names, clearing any cache folder information."
  ;; FIX refresh an browse folders buffers
  (mh:update-folder-table))

(defcommand "Create Folder" ()
  "Create a prompted folder.  If the folder already exists signal an
   error."
  (let ((folder (prompt-for-folder :must-exist nil)))
    (if (mh:folder-exists-p folder)
	(editor-error "Folder already exists -- ~S." folder))
    (mh:create-folder folder)))

(defcommand "Delete Folder" ()
  "Delete a prompted folder."
  (let* ((folder (prompt-for-folder)))
    (if (prompt-for-yes-or-no
	 :prompt (list "Delete folder ~A? (yes or no) "
		       (mh:strip-folder-name folder))
	 :help "Confirm deletion of folder.")
	(multiple-value-bind (success files)
			     (mh:delete-folder folder)
	  (if success
	      (message "Done.")
	      (dolist (file files)
		(message "Skipped ~A." file)))))))

#[ Refiling Messages

{command:Refile Message}
{command:Headers Refile Message}
]#

(defcommand "Refile Message" ()
  "Refile a set of prompted messages from a prompted folder in a prompted
   destination folder.  Update all `Headers' buffers for the folder." ; FIX which folder?
  (let* ((src-folder (prompt-for-folder :prompt "Source folder: "))
	 (hinfo (value headers-information))
	 (temp-msgs (prompt-for-message
		     :folder src-folder
		     :messages
		     (if (and hinfo
			      (string= src-folder
				       (the simple-string
					    (headers-info-folder hinfo))))
			 (headers-info-msg-strings hinfo))
		     :prompt "Messages to pick from: "))
	 ;; Return pick result or temp-msgs individually specified in a list.
	 (msgs (mh:pick-messages src-folder temp-msgs
				 (prompt-for-pick-expression))))
    (declare (simple-string src-folder))
    (refile-message src-folder
		    (prompt-for-folder :must-exist nil
				       :prompt "Destination folder: ")
		    msgs)))

(defhistory *refile-history* *refile-history-pointer* 30)

(defcommand "Headers Refile Message" ()
  "Refile the current message in a prompted destination folder.  When in a
   headers buffer, refile the message on the current line, and when in a
   message buffer, refile the message in the buffer.  Update any `Headers'
   buffers containing messages for that folder." ; FIX which folder?
  (let ((hinfo (value headers-information))
	(minfo (value message-information)))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg (editor-error "Point must be on a header line."))
	     (delete-mark cur-mark)
	     (refile-message (headers-info-folder hinfo)
			     (prompt-for-folder
			      :must-exist nil
			      :prompt "Destination folder: "
			      :history *refile-history*
			      :history-pointer '*refile-history-pointer*)
			     cur-msg)))
	  (minfo
	   (or (message-info-folder minfo)
	       (editor-error "Attempt to refile temporary message."))
	   (refile-message
	    (message-info-folder minfo)
	    (prompt-for-folder :must-exist nil
			       :prompt "Destination folder: "
			       :history *refile-history*
			       :history-pointer '*refile-history-pointer*)
	    (message-info-msgs minfo))
	   (message "Message refiled."))
	  (t
	   (editor-error "Not in a headers or message buffer.")))))

;;; REFILE-MESSAGE refiles msg from src-folder to dst-folder.  If
;;; dst-buffer doesn't exist, prompt to create it.  If $msg is a list
;;; refile the listed messages.  All headers buffers concerning src-folder
;;; are updated.  When msg is a list, we did a general message prompt, and
;;; we cannot know which headers lines to delete.
;;;
(defun refile-message (src-folder dst-folder msg)
  (or (mh:folder-exists-p dst-folder)
      (cond ((prompt-for-y-or-n
	      :prompt (format nil
			      "Destination folder ~A doesn't exist.  Create it? "
			      (mh:strip-folder-name dst-folder))
	      :default t :default-string "Y")
	     (mh:create-folder dst-folder))
	    (t (editor-error "Refile cancelled."))))
  (mh:move-messages src-folder dst-folder (if (listp msg) msg (list msg)))
  (if (listp msg)
      (do-headers-buffers (hbuf src-folder hinfo)
	(do-headers-lines ((buffer-region hbuf) :line-var line :mark-var hmark)
	  (when (member (parse-integer (line-message-id line)) msg)
	    (delete-headers-buffer-line hinfo hmark))))
      (do-headers-buffers (hbuf src-folder hinfo)
	(with-headers-mark (hmark hbuf msg)
	  (delete-headers-buffer-line hinfo hmark)))))


;;;; Miscellaneous commands.

#[ Marking Messages

{command:Mark Message}
]#

(defcommand "Mark Message" (p)
  "Add a message to a sequence.  In a headers or message buffer, add to the
   current message.  With an argument delete the message from the
   sequence."
  (let* ((hinfo (value headers-information))
	 (minfo (value message-information))
	 (in-sequence (mh:profile-component "unseen-sequence")))
    (cond (hinfo
	   (multiple-value-bind (cur-msg cur-mark)
				(headers-current-message hinfo)
	     (or cur-msg
		 (editor-error "Point must be on a header line."))
	     (delete-mark cur-mark)
	     (let ((seq-name (prompt-for-string :prompt "Sequence name: "
						:trim t))
		   (folder (headers-info-folder hinfo)))
	       (declare (simple-string seq-name))
	       (when (string= "" seq-name)
		 (editor-error "Sequence name cannot be empty."))
	       (mh:mark-message folder
				cur-msg seq-name (if p :delete :add))
	       ;; Force line rehighlight.
	       (clear-chi-signatures (region (mark (current-line) 0)
					     (mark (current-line) 0)))
	       (if (string= seq-name in-sequence)
		   (folders-incr folder (if p -1 1))))))
	  (minfo
	   (let ((msgs (message-info-msgs minfo))
		 (seq-name (prompt-for-string :prompt "Sequence name: "
					      :trim t))
		 (folder (message-info-folder minfo)))
	     (declare (simple-string seq-name))
	     (when (string= "" seq-name)
	       (editor-error "Sequence name cannot be empty."))
	     (mh:mark-message folder
			      (if (consp msgs) (car msgs) msgs)
			      seq-name (if p :delete :add))
	     (if (string= seq-name in-sequence)
		 (folders-incr folder (if p -1 1)))))
	  (t
	   (let ((folder (prompt-for-folder))
		 (seq-name (prompt-for-string :prompt "Sequence name: "
					      :trim t)))
	     (declare (simple-string seq-name))
	     (if (string= "" seq-name)
		 (editor-error "Sequence name cannot be empty."))
	     (mh:mark-messages folder (prompt-for-message :folder folder)
			       seq-name (if p :delete :add))
	     (if (string= seq-name in-sequence)
		 (folders-incr folder (if p -1 1))))))))

#[ Miscellaneous Commands

{command:List Mail Buffers}
]#

(defcommand "List Mail Buffers" ()
  "Show a list of all mail `Message', `Headers', and `Draft' buffers.

   If a `Message' buffer has an associated `Headers' buffer, display it
   to the right of the `Message' buffer name.

   If a `Draft' buffer has an associated `Message' buffer, display it to
   the right of the `Draft' buffer's name, otherwise if the buffer is
   associated with a `Headers' buffer, then display the name of the
   `Headers' buffer to the right of the `Draft' buffer.

   For each buffer listed, if it is modified, then display an asterisk
   before the name of the buffer."
  (let ((buffers nil))
    (declare (list buffers))
    (do-strings (n b *buffer-names*)
      (declare (ignore n))
      (or (eq b *echo-area-buffer*)
	  (cond ((editor-bound-p 'message-buffer :buffer b)
		 ;; Catches draft buffers associated with message buffers first.
		 (push (cons b (variable-value 'message-buffer :buffer b))
		       buffers))
		((editor-bound-p 'headers-buffer :buffer b)
		 ;; Then draft or message buffers associated with headers buffers.
		 (push (cons b (variable-value 'headers-buffer :buffer b))
		       buffers))
		((or (editor-bound-p 'draft-information :buffer b)
		     (editor-bound-p 'message-information :buffer b)
		     (editor-bound-p 'headers-information :buffer b))
		 (push b buffers)))))
    (with-pop-up-display (s :height (length buffers))
      (dolist (ele (nreverse buffers))
	(let* ((association (if (consp ele) (cdr ele)))
	       (b (if association (car ele) ele))
	       (buffer-pathname (buffer-pathname b))
	       (buffer-name (buffer-name b)))
	  (write-char (if (buffer-modified b) #\* #\space) s)
	  (if buffer-pathname
	      (format s "~A  ~A~:[~;~50T~:*~A~]~%"
		      (file-namestring buffer-pathname)
		      (directory-namestring buffer-pathname)
		      (if association (buffer-name association)))
	      (format s "~A~:[~;~50T~:*~A~]~%"
		      buffer-name
		      (if association (buffer-name association)))))))))


(defcommand "Message Help" ()
  "Show documentation on `Message' mode."
  (describe-mode-command nil "Message"))

(defcommand "Headers Help" ()
  "Show documentation on `Headers' mode."
  (describe-mode-command nil "Headers"))

(defcommand "Draft Help" ()
  "Show documentation on `Draft' mode."
  (describe-mode-command nil "Draft"))

(defcommand "Fill Paragraph Respect Comment" () ;; FIX comment?
  "Fill the region with *Fill Prefix* set to the comment char if the line
   starts with comment char."
;; FIX lispmode similar "Fill Lisp Comment Para"
;; (to-line-command (line-start mark) (value comment-start))

;; FIX message-[buffer-]insertion-prefix

  ;; FIX empty cited line
  (let ((fill-prefix (concatenate 'simple-string
				  (or (value comment-start) ">")
				  " "))
	(mark (copy-mark (current-point))))
    (line-start mark)
    (let ((chars (region-to-string (region (copy-mark mark :temporary)
					   (line-end mark)))))
      (if (and (>= (length chars) (length fill-prefix))
	       (string= fill-prefix chars :end2 (length fill-prefix)))
	  (elet ((fill-prefix fill-prefix))
	    (fill-paragraph-command))
	  (fill-paragraph-command)))))


;;;; Prompting.

(defhistory *folder-input-history* *folder-input-history-pointer* 50)

;;; Folder prompting.
;;;

(defun prompt-for-folder (&key (must-exist t) (prompt "MH Folder: ")
			       default
			       (history *folder-input-history*)
			       (history-pointer '*folder-input-history-pointer*))
  "Return a prompted folder, suggesting the head of any history else the
   current folder.  Ensure there is a leading + on the result."
  (let ((folders (mh:get-folder-table)))
    (if must-exist
	(or (plusp (string-table-length folders))
	    (editor-error
	     "Empty folder table.  (`Create Folder' adds folders.)")))
    (let ((folder
	   (prompt-for-keyword (list folders)
			       :must-exist must-exist :prompt prompt
			       :default (if (if history
						(zerop (ring-length history))
						t)
					    (or default (mh:current-folder)))
			       :default-string (if (if history
						       (zerop (ring-length history))
						       t)
						   (or default
						       (mh:current-folder)))
			       :history history
			       :history-pointer history-pointer
			       :help "Enter folder name.")))
      (declare (simple-string folder))
      (if (string= folder "") (editor-error "Must supply a folder."))
      (let ((name (mh:coerce-folder-name folder)))
	(if must-exist
	    (or (mh:folder-exists-p name)
		(editor-error "Folder must exist -- ~S." name)))
	name))))

;;; Message prompting.
;;;

(defun prompt-for-message (&key (folder (mh:current-folder))
				(prompt "MH messages: ")
				messages)
   "Prompts for a message spec, using messages as a default.  If messages is
    not supplied, then the current message for folder is used.  The result is
    a list of strings which are the message ids, intervals, and/or sequence
    names the user entered."
  (let* ((cur-msg (cond
		   ((not messages) (string (mh:current-message folder)))
		   ((stringp messages) messages)
		   ((consp messages)
		    (if (= (length (the list messages)) 1)
			(car messages)
			(format nil "~{~A~^ ~}" messages))))))
    (breakup-message-spec (prompt-for-string :prompt prompt
					     :default cur-msg
					     :default-string cur-msg
					     :trim t
					     :help "Enter MH message id(s)."))))

(defun breakup-message-spec (msgs)
  (declare (simple-string msgs))
  (let ((start 0)
	(result nil))
    (loop
      (let ((end (position #\space msgs :start start :test #'char=)))
	(or end
	    (return (if (zerop start)
			(list msgs)
			(nreverse (cons (subseq msgs start) result)))))
	(push (subseq msgs start end) result)
	(setf start (1+ end))))))


;;; PICK expression prompting.
;;;

(defun prompt-for-pick-expression ()
  "Prompt for a pick expression."
  (let ((*package* (find-package "MH")))
    (prompt-for-expression
     :prompt "Message pick expression: "
     :help "Expression to pick over mail messages.")))


;;; Password prompting.
;;;

(defun prompt-for-password (&optional (prompt "Password: "))
  "Prompts for password with prompt."
  (let ((edi::*parse-verification-function* #'(lambda (string) (list string))))
    (let ((edi::*parse-prompt* prompt))
      (edi::display-prompt-nicely))
    (let ((start-window (current-window)))
      (move-mark *parse-starting-mark* (buffer-point *echo-area-buffer*))
      (setf (current-window) *echo-area-window*)
      (unwind-protect
	  (use-buffer *echo-area-buffer*
	    (let ((result ()))
	      (declare (list result))
	      (loop
		(let ((key-event (get-key-event *editor-input*)))
		  (ring-pop edi::*key-event-history*)
		  (cond ((eq key-event #k"return")
			 (return (prog1 (coerce (nreverse result)
						'simple-string)
				   (fill result nil))))
			((or (eq key-event #k"control-u")
			     (eq key-event #k"control-U"))
			 (setf result nil))
			(t (push (ext:key-event-char key-event) result)))))))
	(setf (current-window) start-window)))))


;;;; Making mail buffers.

(defun maybe-make-mh-buffer (name use)
  "Look up buffer with NAME, returning it if it exists after cleaning it up
   to a state \"good as new\".  Currently, we don't believe it is possible
   to try to make two draft buffers with the same name since that would
   mean that composition, draft folder interaction, and draft folder
   current message didn't do what we expected -- or some user was modifying
   the draft folder in some evil way."
  (let ((buf (getstring name *buffer-names*)))
    ;; FIX loop until buffer made, in case buffer name created some other way?
    (cond ((not buf)
	   (let ((new-buf
		  (ecase use
		    (:headers
		     (make-buffer name
				  :modes '("Headers")
				  :delete-hook '(cleanup-headers-buffer)))

		    (:message
		     (make-buffer name :modes '("Message")
				  :modeline-fields
				  (value default-message-modeline-fields)
				  :delete-hook '(cleanup-message-buffer)))

		    (:draft
		     (let ((buf (make-buffer
				 name :modes '("Text")
				 :delete-hook '(cleanup-draft-buffer))))
		       (setf (buffer-minor-mode buf "Draft") t)
		       buf)))))
	     new-buf))
	  ((editor-bound-p 'headers-information :buffer buf)
	   (setf (buffer-writable buf) t)
	   (delete-region (buffer-region buf))
	   (cleanup-headers-buffer buf)
	   (delete-variable 'headers-information :buffer buf)
	   buf)
	  ((editor-bound-p 'message-information :buffer buf)
	   (setf (buffer-writable buf) t)
	   (delete-region (buffer-region buf))
	   (cleanup-message-buffer buf)
	   (delete-variable 'message-information :buffer buf)
	   buf)
	  ((editor-bound-p 'draft-information :buffer buf)
	   (error "Attempt to create multiple draft buffers to same draft ~
	           folder message -- ~S"
		  name)))))


;;;; Message buffer modeline fields.

(make-modeline-field
 :name :deleted-message :width 2 :replace t
 :function
 #'(lambda (buffer window)
     "Return \"D \" when message in $buffer is deleted."
     (declare (ignore window))
     (let* ((minfo (variable-value 'message-information :buffer buffer))
	    (hmark (message-info-headers-mark minfo)))
       (cond ((not hmark)
	      (let ((msgs (message-info-msgs minfo)))
		(if (and (value virtual-message-deletion)
			 (mh:sequence-member-p
			  (if (consp msgs) (car msgs) msgs)
			  (mh:sequence-list (message-info-folder minfo)
					    "edtrash")))
		    "D "
		    "")))
	     ((line-message-deleted (mark-line hmark))
	      "D ")
	     (t "")))))

(make-modeline-field
 :name :replied-to-message :width 1 :replace t
 :function
 #'(lambda (buffer window)
     "Return \"A\" when message in $buffer is deleted."
     (declare (ignore window))
     (let* ((minfo (variable-value 'message-information :buffer buffer))
	    (hmark (message-info-headers-mark minfo)))
       (cond ((not hmark)
	      ;; Could do something nasty here to figure out the right value.
	      "")
	     (t
	      (mark-to-note-replied-msg hmark)
	      (if (char= (next-character hmark) #\A)
		  "A"
		  ""))))))

;;; MARK-TO-NOTE-REPLIED-MSG moves the headers-buffer mark to a line position
;;; suitable for checking or setting the next character with respect to noting
;;; that a message has been replied to.
;;;
(defun mark-to-note-replied-msg (hmark)
  (line-start hmark)
  (find-attribute hmark :digit)
  (find-attribute hmark :digit #'zerop)
  (character-offset hmark 1))


(defevar "Default Message Modeline Fields"
  "This is the default list of modeline-field objects for message buffers."
  :value
  (list (modeline-field :buffer-state)
	(modeline-field :package)
	(modeline-field :buffer-short-name)
	(modeline-field :modes)
	(modeline-field :replied-to-message)
	(modeline-field :deleted-message)
	(modeline-field :column)
	(modeline-field :space)
	(modeline-field :line)
	(modeline-field :space)
	(modeline-field :%)
	;;(modeline-field :position)
	(modeline-field :space)
	(modeline-field :buffer-pathname)))


;;;; MH interface.

#[ Introduction to Commands and Variables

Unless otherwise specified, any command which prompts for a folder name will
offer the user a default.  Usually this is MH's idea of the current folder,
but sometimes it is the folder name associated with the current buffer if there
is one.  When prompting for a message, any valid MH message expression may be
entered (for example, "1 3 6", "1-3 5 6", "unseen", "all").
Unless otherwise specified, a default will be offered (usually the current
message).

Some commands mention specific MH utilities, so the user knows how the
the editor command affects the state of MH and what profile components and
special formatting files will be used.  the editor runs the MH utility programs
from a directory indicated by the following variable:

{evariable:MH Utility Pathname}
]#

;;; Running an MH utility.
;;;

(defevar "MH Utility Pathname"
  "MH utility names are merged with this."
  :value (pathname "/usr/misc/.mh/bin/"))

(defvar *mh-error-output* (make-string-output-stream))

(defun mh (utility args &key (errorp t) environment)
  "Runs the MH utility with the list of args (suitable for EXT:RUN-PROGRAM),
   outputting to *standard-output*.  Environment is a list of strings
   appended with ext:*environment-list*.  This returns t, unless there is
   an error.  When errorp, this reports any MH errors in the echo area as
   an editor error, and this does not return; otherwise, nil and the error
   output from the MH utility are returned."
  (fresh-line)
  (let* ((utility
	  (namestring
	   (or (probe-file (merge-pathnames utility
					    (value mh-utility-pathname)))
	       utility)))
	 (proc (ext:run-program
		utility args
		:output *standard-output*
		:error *mh-error-output*
		:env (append environment ext:*environment-list*))))
    (fresh-line)
    (ext:process-close proc)
    (cond ((zerop (ext:process-exit-code proc))
	   (values t nil))
	  (errorp
	   (editor-error "MH Error -- ~A"
			 (get-output-stream-string *mh-error-output*)))
	  (t (values nil (get-output-stream-string *mh-error-output*))))))


;;;; Checking for mail.

(defvar *mailbox* nil)

(defun new-mail-p ()
  (when (mh:new-mail-p (mh:make-drops (value mail-drops)))
    (setq *new-mail-p* t)))


;;;; Folder browsing.

(defun setup-mail-browse-mode (buffer)
  (highlight-visible-mail-browse-buffer buffer)
  (pushnew '("Mail Browse" t highlight-visible-mail-browse-buffer)
	   *mode-highlighters*))

(defmode "Mail Browse" :major-p t
  :setup-function #'setup-mail-browse-mode
  :short-name "Mail-Browse"
  :documentation
  "Mail folder browsing mode.")

(defmacro line-folder (line)
  `(getf (line-plist ,line) 'mh-folder))

(defmacro line-folder-count (line)
  `(getf (line-plist ,line) 'mh-folder-count))

(defun refresh-folders (buffer)
  "Refresh the folders listed in $buffer."
  (let ((pos (count-lines (region (buffer-start-mark buffer)
				  (buffer-point buffer)))))
    (setf (buffer-writable buffer) t)
    (delete-region (buffer-region buffer))
    (or (editor-bound-p 'folders-contents :buffer buffer)
	(defevar "Folders Contents"
	  "What is displayed in this folders buffer: :all for all folder, else
	   only folders with new messages."
	  :value t ; :all
	  :buffer buffer))
    (case (variable-value 'folders-contents :buffer buffer)
      (:all
       (let ((mark (copy-mark (buffer-point buffer)))
	     (in-sequence (mh:profile-component "unseen-sequence")))
	 (mh:do-folders (folder)
	   (let ((seqs (mh:sequence-list folder in-sequence))
		 (count 0))
	     (dolist (seq seqs)
	       (etypecase seq
		 (integer (incf count))
		 (list (incf count (1+ (- (cdr seq) (car seq)))))))
	     (setf (line-folder (mark-line mark)) folder)
	     (setf (line-folder-count (mark-line mark)) count)
	     (insert-string mark
			    (format () "   ~@3<~A~> ~A~%"
				    (if (zerop count) "" count)
				    folder))))
	 (delete-mark mark)))
      (t
       (let ((mark (copy-mark (buffer-point buffer)))
	     (in-sequence (mh:profile-component "unseen-sequence")))
	 (mh:do-folders (folder)
	   (if (string= folder (mh:draft-folder))
	       (let ((count (length (mh:pick-messages folder '(:all)))))
		 (setf (line-folder (mark-line mark)) folder)
		 (setf (line-folder-count (mark-line mark)) count)
		 (insert-string mark
				(format () "   ~@3<~A~> ~A~%"
					count folder)))
	       (let ((seqs (mh:sequence-list folder in-sequence))
		     (count 0))
		 (dolist (seq seqs)
		   (etypecase seq
		     (integer (incf count))
		     (list (incf count (1+ (- (cdr seq) (car seq)))))))
		 (when (plusp count)
		   (setf (line-folder (mark-line mark)) folder)
		   (setf (line-folder-count (mark-line mark)) count)
		   (insert-string mark
				  (format () "   ~@3<~A~> ~A~%"
					  count folder))))))
	 (delete-mark mark))))
    (setf (buffer-major-mode buffer) "Mail Browse")
    (setf (buffer-writable buffer) ())
    (setf (buffer-modified buffer) ())
    (buffer-start (buffer-point buffer))
    (line-offset (buffer-point buffer) (1- pos))))

(defvar *folders-buffer* ())

(defun get-folders-buffer ()
  (or *folders-buffer*
      (setq *folders-buffer* (make-unique-buffer "Folders"))))

(defun folders-incr (folder increment)
  (when *folders-buffer*
    (with-writable-buffer (*folders-buffer*)
      (while ((line (mark-line (buffer-start-mark *folders-buffer*))
		    (line-next line)))
	     (line
	      ;; Refresh, to add a line for the folder.
	      (refresh-folders *folders-buffer*)
	      ;; FIX force rehighlight
	      ;(redisplay-all)
	      )
	(when (string= folder (line-folder line))
	  (setf (line-string line)
		(format () "   ~@3<~A~> ~A"
			(setf (line-folder-count line)
			      (+ (line-folder-count line) increment))
			(line-folder line)))
	  (clear-chi-signatures (region (mark line 0) (mark line 0)))
	  ;; FIX force rehighlight
	  #|
	  (dolist (window (buffer-windows *folders-buffer*))
	    (edi::force-redisplay window)) ; display.lisp
	  |#
	  (return))))))

(defcommand "Browse Folders" ()
  "Browse mail folders."
  (let ((buffer (get-folders-buffer)))
    (change-to-buffer buffer)
    (refresh-folders buffer)))

#|
(defun line-folder (line)
  "Return the folder on $line."
  (if (blank-line-p line)
      (editor-error "Point must be on a folder line."))
  (let ((string (line-string line)))
    (or (> (length string) 3)
	(error "Folder line less than four characters."))
    (setq string (subseq string 3))
    (let ((pos (nth-value 1 (read-from-string string))))
      (subseq string pos (position #\space string :start pos)))))
|#

(defun line-of-folder (folder buffer)
  "Return the line on which FOLDER resides in BUFFER."
  (do-buffer-lines (line buffer)
    (or (blank-line-p line)
	(if (string= folder
		     (subseq (line-string line)
			     7 (position #\space (line-string line)
					 :start 7)))
	    (return-from line-of-folder line)))))

(defcommand "Mail Browse Browse Folder" (p folder)
  "Browse the headers of the folder at point."
  (declare (ignore p))
  (let ((folder (or folder
		    (if (blank-line-p (current-line))
			(editor-error "Point must be on a folder line.")
			(line-folder (mark-line (current-point)))))))
    (new-message-headers folder
			 (list "highest")
			 () (1+ (- (window-height (current-window)))))
    ;; Move to the first new message.
    (let ((in (mh:sequence-list folder (mh:profile-component
					"unseen-sequence"))))
      (while ((line (mark-line (current-point)) (line-next line)))
	     (line)
	(when (mh:sequence-member-p (line-message-id line) in)
	  (move-mark (current-point) (mark line 0))
	  (return))))))

(defcommand "Mail Browse Browse Folder in Other Window" ()
  "Browse the headers of the folder at point in the other window."
  (let ((folder (line-folder (mark-line (current-point)))))
    (if (eq (next-window (current-window)) (current-window))
	(split-window-command)
	(next-window-command))
    (mail-browse-browse-folder-command () folder)))

(defcommand "Refresh Folders" ()
  "Refresh the browsed folders."
  (refresh-folders (current-buffer)))

(defcommand "Mail Browse Toggle New" ()
  "Toggle whether the buffer holds all folders or only folders with new
   mail."
  (or (editor-bound-p 'folders-contents :buffer (current-buffer))
      (editor-error "Must be in a Folders buffer."))
  (setv folders-contents (fi (eq (value folders-contents) :all) :all))
  ;; FIX refresh marks too?
  (refresh-folders (current-buffer)))

(defcommand "Mail Browse Delete Folder" ()
  "Mark the folder at point for deletion."
  (if (blank-line-p (mark-line (current-point)))
      (editor-error "Point must be on a folder line."))
  (let ((mark (copy-mark (current-point)))
	(buffer (current-buffer)))
    (line-start mark)
    (setf (buffer-writable buffer) t)
    (delete-characters mark)
    (insert-character mark #\D)
    (setf (buffer-writable buffer) ())))

(defcommand "Mail Browse Delete Folder and Down Line" ()
  "Mark the folder at point for deletion, and move down a line."
  (mail-browse-delete-folder-command)
  (next-line-command))

(defcommand "Mail Browse Rename Folder" ()
  "Rename the folder at point."
  (if (blank-line-p (mark-line (current-point)))
      (editor-error "Point must be on a folder line."))
  (let ((line-folder (line-folder (mark-line (current-point)))))
    (mh:rename-folder line-folder
		      (prompt-for-folder :must-exist ()
					 ;; FIX uses head of history
					 :default line-folder)))
  ;; FIX refresh marks too
  (refresh-folders (current-buffer)))

(defcommand "Mail Browse Mark Folder" ()
  "Mark the folder at point."
  (if (blank-line-p (mark-line (current-point)))
      (editor-error "Point must be on a folder line."))
  (let ((mark (copy-mark (current-point)))
	(buffer (current-buffer)))
    (line-start mark)
    (mark-after mark)
    (setf (buffer-writable buffer) t)
    (delete-characters mark)
    (insert-character mark #\*)
    (setf (buffer-writable buffer) ())
    (next-line-command)))

(defcommand "Mail Browse Clear Folder Marks" ()
  "Clear any marks on the folder at point."
  (if (blank-line-p (mark-line (current-point)))
      (editor-error "Point must be on a folder line."))
  (let ((mark (copy-mark (current-point)))
	(buffer (current-buffer)))
    (line-start mark)
    (setf (buffer-writable buffer) t)
    (delete-characters mark 2)
    (insert-string mark "  ")
    (setf (buffer-writable buffer) ())))

(defcommand "Mail Browse Clear Folder Marks and Down Line" ()
  "Clear any marks on the folder at point."
  (mail-browse-clear-folder-marks-command)
  (next-line-command))

(defcommand "Expunge Folders" ()
  "Expunge any folders marked for deletion."
  (let* ((buffer (current-buffer))
	 (mark (copy-mark (buffer-start-mark buffer)))
	 (remain))
    (collect ((folders)
	      (generic-remains))  ; *-marked folders to remain.
      (line-start mark)
      (setf (buffer-writable buffer) t)
      (loop for line = (mark-line mark) do
	(if (blank-line-p line) (return))
	(let ((string (line-string (mark-line mark))))
	  (if (char= (aref string 0) #\D)
	      (folders (line-folder line))
	      (if (char= (aref string 1) #\*)
		  (generic-remains (line-folder line)))))
	(line-offset mark 1))
      (or (folders)
	  (progn
	    (setf (buffer-writable buffer) ())
	    (return-from expunge-folders-command)))
      (let ((folder-count (length (folders))))
	(flet ((prompt-and-do (prompt)
		 (if (prompt-for-yes-or-no
		      :prompt prompt
		      :help "Confirm deletion of folders.")
		     (dolist (folder (folders))
		       ;; FIX prompt again if folder contains messages
		       (or (mh:delete-folder folder)
			   (progn
			     (message "Failed to delete ~A." folder)
			     (push folder remain))))
		     (editor-error "Expunge canceled."))))
	  (if (> folder-count 1)
	      ;; FIX with-pop-up-list?
	      (with-pop-up-window (buffer window)
		(with-output-to-mark (stream (buffer-point buffer))
		  (let* ((pos 0)
			 (width (/ (value fill-column) 3))
			 (format-0 (format () "~~@~D<~~A~~>" width))
			 (format-1 (format () " ~~@~D<~~A~~>" width)))
		    (dolist (folder (folders))
		      (case pos
			(0 (format stream format-0 folder)
			   (setq pos 1))
			(1 (format stream format-1 folder)
			   (setq pos 2))
			(2 (format stream " ~A~%" folder)
			   (setq pos 0))))))
		(prompt-and-do
		 (list "Delete ~D folders, including any messages? (yes or no) "
		       folder-count)))
	      (prompt-and-do
	       (list "Delete ~A, including any messages? (yes or no) "
		     (car (folders)))))))
      (refresh-folders buffer)
      ;; Remark failures.
      (setf (buffer-writable buffer) t)
      (dolist (folder remain)
	(let* ((line (line-of-folder folder buffer))
	       (mark (mark line 0)))
	  (line-start mark)
	  (delete-characters mark)
	  (insert-character mark #\D)))
      ;; Remark generic marks (*'s).
      (dolist (folder (generic-remains))
	(let* ((line (line-of-folder folder buffer))
	       (mark (mark line 0)))
	  (line-start mark)
	  (mark-after mark)
	  (delete-characters mark)
	  (insert-character mark #\*)))
      (setf (buffer-writable buffer) ()))))


;;;; Highlighting.

(declaim (special *context*))

(defun highlight-message-line (line chi-info)
  (when (next-character (mark line 0))
    (let ((chars (line-string line)))
      (case *context*
	((:field ())
	 (if (plusp (length chars))
	     (if (char= (char chars 0) #\-)
		 (progn
		   (setq *context* :body)
		   (chi-mark line 0 *string-font* :string chi-info))
		 (progn
		   (setq *context* :field)
		   (chi-mark line 0 *string-font* :string chi-info)))
	     (progn
	       (setq *context* :body)
	       (chi-mark line 0 *original-font*
			 :window-foreground chi-info))))
	(:body
	 (if (and (plusp (length chars))
		  (char= (char chars 0) #\>))
	     ;; Mark a citation.
	     (chi-mark
	      line 0
	      *original-font*
	      (nth (mod (let ((index 1)
			      (length (length chars)))
			  (block find-font
			    (while ((font 0 (1+ font))) (t)
			      (if (>= index length) (return font))
			      (let ((char (char chars index)))
				;; Skip over whitespace in chars.
				(while () ((member char '(#\space #\tab)))
				  (incf index)
				  (if (>= index length)
				      (return-from find-font font))
				  (setq char (char chars index)))
				(or (char= char #\>)
				    (return font))
				(incf index)))))
			5)
		   '(:comment
		     :variable
		     :function
		     :preprocessor
		     :special-form))
	      chi-info)))))))

(defun highlight-message-buffer (buffer)
  (highlight-chi-buffer buffer highlight-message-line))

(defun highlight-visible-message-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-message-line))

(defun highlight-headers-line (line chi-info)
  (let ((line-length (line-length line)))
    (when (> line-length 3)
      (when (> line-length 20)
	(let ((hinfo (value headers-information)))
	  (if hinfo
	      (progn
		(chi-mark line 21 *special-form-font*
			  :special-form chi-info)
		(when (> line-length 38)
		  (if (mh:sequence-member-p
		       (line-message-id line)
		       (mh:sequence-list (headers-info-folder hinfo)
					 (mh:profile-component
					  "unseen-sequence")))
		      (chi-mark line 39 *variable-font*
				:variable chi-info)
		      (chi-mark line 39 *string-font*
				:string chi-info))))
	      (chi-mark line 21 *special-form-font*
			:special-form chi-info)))
	(when (> line-length 77)
	  (chi-mark line 78 *original-font* :window-foreground
		    chi-info))))))

(defun highlight-headers-buffer (buffer)
  (highlight-chi-buffer buffer highlight-headers-line))

(defun highlight-visible-headers-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-headers-line))

(defun highlight-mail-browse-line (line chi-info)
  (let ((line-length (line-length line)))
    (when (> line-length 3)
      (chi-mark line 3 *variable-font*
		:variable chi-info)
      (when (> line-length 7)
	(chi-mark line 7 *string-font* :string chi-info)))))

(defun highlight-mail-browse-buffer (buffer)
  (highlight-chi-buffer buffer highlight-mail-browse-line))

(defun highlight-visible-mail-browse-buffer (buffer)
  (highlight-visible-chi-buffer buffer highlight-mail-browse-line))


#[ Styles of Usage

This section discusses some styles of usage or ways to make use of some of the
features of the editor's interface to MH that might not be obvious.  In each
case, setting some variables and/or remembering an extra side effect of a
command will lend greater flexibility and functionality to the user.

[ Unseen Headers Message Spec ]
[ Temporary Draft Folder      ]
[ Reply to Message Prefix CC  ]

FIX split mail, Read New Mail Style
]#

#[ Unseen Headers Message Spec

The unseen `Headers' buffer by default only shows unseen headers which is
adequate for one folder, simple mail handling.  Some people use their `New
Mail Folder' only for incoming mail, refiling or otherwise dispatching a
message immediately.  Under this mode it is easy to conceive of the user not
having time to respond to a message, but he would like to leave it in this
folder to remind him to take care of it.  Using the `Unseen Headers Message
Spec' variable, the user can cause all the messages the `New Mail Folder' to
be inserted into the unseen `Headers' buffer whenever just unseen headers
would be.  This way he sees all the messages that require immediate attention.

To achieve the above effect, `Unseen Headers Message Spec' should be set to
the string "all".  This variable can be set to any general MH message
specification (see section [mhcommands] of this chapter), so the user can
include headers of messages other than those that have not been seen without
having to insert all of them.  For example, the user could set the variable to
"flagged" and use the `Mark Message' command to add messages he's
concerned about to the "flagged" sequence.  Then the user would see new
mail and interesting mail in his unseen `Headers' buffer, but he doesn't
have to see everything in his `New Mail Folder'.
]#

#[ Temporary Draft Folder

Section [components-files] of this chapter discusses how to make MH keep
personal copies of outgoing mail.  The method described will cause a copy of
every outgoing message to be saved forever and requires the user to go through
his Fcc: folder, weeding out those he does not need.  The `Temporary
Draft Folder' variable can name a folder whose messages will be deleted and
expunged whenever any folder's messages are expunged.  By naming this folder in
the MH profile and components files, copies of outgoing messages can be saved
temporarily.  They will be cleaned up automatically, but the user still has a
time frame in which he can permanently save a copy of an outgoing message.
This folder can be visited with `Message Headers', and messages can be
refiled just like any other folder.
]#

#[ Reply to Message Prefix CC

Depending on the kinds of messages one tends to handle, the user may find
himself usually replying to everyone who receives a certain message, or he may
find that this is only desired occasionally.  In either case, the user
can set up his MH profile to do one thing by default, using the `Reply
to Message Prefix Action' variable in combination with a prefix argument to the
`Reply to Message' command to get the other effect.

For example, the following line in one's MH profile will cause MH to reply to
everyone receiving a certain message (except for the user himself since he
saves personal copies with the -fcc switch):

repl: -cc all -nocc me -fcc out-copy

This user can set `Reply to Message Prefix CC' to be :no-cc-all.
Then whenever he invokes `Reply to Message' with a prefix argument, instead
of replying to everyone, the draft will be set up in reply only to the person
who sent the mail.

As an alternative example, not specifying anything in one's MH profile and
setting this variable to :cc-all will have a default effect of replying
only to the sender of a piece of mail.  Then invoking `Reply to Message'
with a prefix argument will cause everyone who received the mail to get a copy
of the reply.  If the user does not want a cc: copy, then he can add
-nocc me as a default switch and value in his MH profile.
]#

;; FIX generate the bindings
#[ Mail Bindings Wallchart

Key bindings for the editor [Mail] interface.

== Global bindings ==

    Incorporate and Read New Mail     C-x i
    Send Message                      C-x m
    Message Headers                   C-x r

== Headers and Message modes bindings ==

    Next Undeleted Message            n
    Previous Undeleted Message        p
    Send Message                      s, m
    Forward Message                   f
    Headers Delete Message            k
    Headers Undelete Message          u
    Headers Refile Message            o
    List Mail Buffers                 l
    Quit Headers                      q
    Incorporate and Read New Mail     i
    Next Message                      M-n
    Previous Message                  M-p
    Beginning of Buffer               <
    End of Buffer                     >

== Headers mode bindings ==

    Delete Message and Down Line      d
    Pick Headers                      h
    Show Message                      space, .
    Reply to Message                  r
    Expunge Messages                  !

== Message mode bindings ==

    Delete Message and Show Next      d
    Goto Headers Buffer               ^
    Scroll Message                    space
    Scroll Message                    C-v
    Scroll Window Up                  backspace, delete
    Reply to Message in Other Window  r
    Edit Message Buffer               e
    Insert Message Region             H-y

== Draft mode bindings ==

    Goto Headers Buffer               H-^
    Goto Message Buffer               H-m
    Deliver Message                   H-s, H-c
    Insert Message Buffer             H-y
    Delete Draft and Buffer           H-q
    List Mail Buffers                 H-l
]#
