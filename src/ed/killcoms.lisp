;;; Killing and reviving things.

(in-package "ED")

(export '(*ephemerally-active-command-types* *kill-ring* activate-region
	  buffer-mark check-region-active current-mark current-region
	  pacify-region kill-characters kill-region pop-buffer-mark
	  push-buffer-mark push-kill region-active-p))

(defvar *kill-ring* (make-ring 1000)
  "A ring (as in [Rings]) of regions saved from buffers.  Some commands
   save affected regions on the kill ring before performing modifications.")


#[ Killing and Deleting

The editor has many commands which kill text.  Killing is a variety of
deletion which saves the deleted text for later retrieval.  The killed text
is saved in a ring buffer known as the kill ring.  Killing has two main
advantages over deletion:

  1) If text is accidentally killed, a not uncommon occurrence, then it can be
     restored.

  2) Text can be moved from one place to another by killing it and then
     restoring it in the new location.

Killing is not the same as deleting.  When a command is said to delete
text, the text is permanently gone and is not pushed on the kill ring.
Commands which delete text generally only delete things of little
importance, such as single characters or whitespace.
]#


;;;; Active Regions.

#[ Active Regions

Every buffer has a mark stack (page pagerefmark-stack) and a mark known as
the point where most text altering nominally occurs.  Between the top of the
mark stack, the current-mark, and the current-buffer's point, the
current-point, is what is known as the current-region.  Certain
commands signal errors when the user tries to operate on the current-region
without its having been activated.  If the user turns off this feature, then
the current-region is effectively always active.

When writing a command that marks a region of text, the programmer should
make sure to activate the region.  This typically occurs naturally from the
primitives that you use to mark regions, but sometimes you must explicitly
activate the region.  These commands should be written this way, so they do
not require the user to separately mark an area and then activate it.
Commands that modify regions do not have to worry about making the region
passive since modifying a buffer automatically does it.  Commands that
insert text often activate the region ephemerally; that is, the region is
active for the immediately following command, allowing the user wants to
delete the region inserted, fill it, or whatever.

Once a marking command makes the region active, it remains active until:

  - a command uses the region,

  - a command modifies the buffer,

  - a command changes the current window or buffer,

  - a command signals an editor-error,

  - or the user types C-g.

{evariable:Active Regions Enabled}
{variable:ed:*ephemerally-active-command-types*}
{function:ed:activate-region}
{function:ed:pacify-region}
{function:ed:region-active-p}
{function:ed:check-region-active}
{function:ed:current-region}
]#

(defevar "Active Regions Enabled"
  "When true, some commands that affect the current region only work when
   the region is active.  May be set to () for more traditional Emacs
   region behaviour.")

(defevar "Highlight Active Region"
  "When true, display the text in the region in a special font whenever the
   region is active.  This provides a visible indication of what text will
   be manipulated by a region command."
  :value t)

(defvar *active-region-p* nil)
(defvar *active-region-buffer* nil)
(defvar *ephemerally-active-command-types* (list :ephemerally-active :yank)
  "A list of [command types] that permit the current region to be active
   for the immediately following command.

   When the previous command type is one of these, the current region is
   active for the currently executing command only, regardless of whether
   it does something to pacify the region.  However, the current command
   may activate the region for future commands.

   :ephemerally-active is a default command type that may be used to
   ephemerally activate the region, and :yank is the type used by two
   commands, `Yank' and `Rotate Kill Ring' (what users typically think of
   as C-y and M-y)")

(proclaim '(inline activate-region pacify-region region-active-p))

(defun activate-region ()
  "Make the current region active."
  (let ((buffer (current-buffer)))
    (setf *active-region-p* (buffer-signature buffer))
    (setf *active-region-buffer* buffer)
    (invoke-hook activate-region-hook)))

(defun pacify-region ()
  "Make the current region passive."
  (setf *active-region-p* ())
  (setf *active-region-buffer* ())
  (invoke-hook pacify-region-hook))

(defun region-active-p ()
  "Return whether the current-region is active, including ephemerally.
   Leave the active state of the region as it is."
  (or (and *active-region-buffer*
	   (eql *active-region-p* (buffer-signature *active-region-buffer*)))
      (member (last-command-type) *ephemerally-active-command-types*
	      :test #'equal)))

(defun check-region-active ()
  "Signal an editor error when active regions are enabled and the current
   region is passive."
  (if (value active-regions-enabled)
      (fi (region-active-p)
	  (editor-error "The current region is passive."))))

(defun current-region (&optional (error-if-passive t)
				 (pacify-region t))
  "Return a region formed with the current mark and the current point,
   optionally signaling an editor error if the current region is passive.

   Each call returns a newly allocated region.

   Pacify the current region if $pacify-region is true.

   The editor primitives can modify the region if it is passive or active,
   so a command that modifies the active state can pacify the region
   whenever it is convenient."
  (if error-if-passive (check-region-active))
  (if pacify-region (pacify-region))
  (let ((point (current-point))
	(mark (current-mark)))
    (if (mark< mark point) (region mark point) (region point mark))))

(defcommand "Activate Region" ()
  "Make the current region active, using the current positions of the point
   and mark.  ^G pacifies the region."
  (activate-region))

;;; The following are hook functions for keeping things righteous.
;;;

(defun set-buffer-pacify-region (buffer)
  (declare (ignore buffer))
  (pacify-region))
;;;
(add-hook set-buffer-hook 'set-buffer-pacify-region)

(defun set-window-pacify-region (window)
  (unless (or (eq window *echo-area-window*)
	      (eq (current-window) *echo-area-window*))
    (pacify-region)))
;;;
(add-hook set-window-hook 'set-window-pacify-region)

(defun control-g-pacify-region ()
  (pacify-region))
;;;
(add-hook abort-hook 'control-g-pacify-region)


#[ The Mark and The Region

Each buffer has a distinguished position known as the mark.  The mark
initially points to the beginning of the buffer.  The area between the mark
and the point is known as the region.  Many the editor commands which
manipulate large pieces of text use the text in the region.  To use these
commands, one must first use some command to mark the region.

Although the mark is always pointing somewhere (initially
to the beginning of the buffer), region commands insist that the region be made
active before it can be used.  This prevents accidental use of a region
command from mysteriously mangling large amounts of text.

{evariable:Active Regions Enabled}

Once a marking command makes the region active, it remains active until:

  - a command uses the region,

  - a command modifies the buffer,

  - a command changes the current window or buffer,

  - a command signals an editor error,

  - or the user types C-g.

Motion commands have the effect of redefining the region, since they move the
point and leave the region active.

Commands that insert a large chunk of text into the buffer usually set an
ephemerally active region around the inserted text.  An ephemerally active
region is always pacified by the next command, regardless of the kind of
command.  The ephemerally active region allows an immediately following
region command to manipulate the inserted text, but doesn't persist
annoyingly.  This is also very useful with active region highlighting,
since it visibly marks the inserted text.

{evariable:Highlight Active Region}
{evariable:Active Region Highlighting Font}
{command:Set/Pop Mark}
{command:Mark Whole Buffer}
{command:Mark to Beginning of Buffer}
{command:Mark to End of Buffer}
{command:Activate Region}

[ The Mark Stack ]
[ Using The Mouse ]
]#

#[ The Mark Stack

Each buffer has a mark stack, providing a history of positions in that
buffer.  The current mark is the mark on the top of the stack; earlier
values are recovered by popping the stack.  Since commands that move a long
distance save the old position on the mark stack, the mark stack commands
are useful for jumping to interesting places in a buffer without having to
do a search.

{command:Pop Mark}
{command:Pop and Goto Mark}
{command:Exchange Point and Mark}

`Exchange Point and Mark' can be used to switch between two positions in a
buffer, since repeating it reverts to the previous setup.
]#


;;;; Buffer-Mark primitives and commands.

;;; See command.lisp for #'hcmd-make-buffer-hook-fun which makes the stack
;;; for each buffer.

(defun current-mark ()
  "Return the top of the current buffer's mark stack.

   There is always at least one mark at the beginning of the buffer's
   region, and all marks returned are right-inserting."
  (ring-ref (value buffer-mark-ring) 0))

(defun buffer-mark (buffer)
  "Return the top of $buffer's mark stack.  There is always at least one
   mark at the beginning of $buffer's region.  All marks returned are
   right-inserting."
  (ring-ref (variable-value 'buffer-mark-ring :buffer buffer) 0))

(defun pop-buffer-mark ()
  "Pop the current buffer's mark stack, returning the mark.  If the stack
   becomes empty, pushes a new mark on the stack pointing to the buffer's
   start.  This always pacifies the current region (as in [active
   regions])."
  (let* ((ring (value buffer-mark-ring))
	 (mark (ring-pop ring)))
    (pacify-region)
    (if (zerop (ring-length ring))
	(ring-push (copy-mark
		    (buffer-start-mark (current-buffer)) :right-inserting)
		   ring))
    mark))

(defun push-buffer-mark (mark &optional (activate-region nil))
  "Push $mark into the current buffer's mark stack, ensuring that the mark
   is right-inserting, and return $mark.  If $mark points into some other
   buffer, signal an error.

   Make the current region active if $active-region is true.  Always leave
   the current region active if it is active."
  (cond ((eq (line-buffer (mark-line mark)) (current-buffer))
	 (setf (mark-kind mark) :right-inserting)
	 (ring-push mark (value buffer-mark-ring)))
	(t (error "Mark must be in the current buffer.")))
  (if activate-region (activate-region))
  mark)

(defcommand "Set/Pop Mark" (p)
  "Set or Pop the mark ring.

   Push point as the mark, activating the current region.

   With one prefix do a pop-and-goto, i.e. pop the mark, replace the point
   with the popped mark, and make the current region passive.

   With two prefixes, pop the mark and throw it away, pacifying the current
   region."
  (cond ((fi p)
	 (if (eq (last-command-type) :pop-and-goto-mark)
	     (progn
	       (pop-and-goto-mark-command)
	       (setf (last-command-type) :pop-and-goto-mark))
	     (progn
	       (push-buffer-mark (copy-mark (current-point)) t)
	       (when (interactive)
		 (message "Mark pushed.")))))
	((= p (value universal-argument-fallback))
	 (pop-and-goto-mark-command)
	 (setf (last-command-type) :pop-and-goto-mark))
	((= p (expt (value universal-argument-fallback) 2))
	 (delete-mark (pop-buffer-mark)))
	(t (editor-error "Prefix argument out of range."))))

(defcommand "Pop and Goto Mark" ()
  "Pop mark into point, making the current region passive."
  (let ((mark (pop-buffer-mark)))
    (move-mark (current-point) mark)
    (delete-mark mark)))

(defcommand "Pop Mark" ()
  "Pop the mark stack, throwing away the mark on the top of the stack,
   restoring the current mark to the next most recent value, and making the
   current region passive."
  (delete-mark (pop-buffer-mark)))

(defcommand "Exchange Point and Mark" ()
  "Swap the positions of the point and the mark, thus moving to the
   location of the mark, and leaving the mark at the location of the
   point."
  (let ((point (current-point))
	(mark (current-mark)))
    (with-mark ((temp point))
      (move-mark point mark)
      (move-mark mark temp))))

(defcommand "Mark Whole Buffer"  (p)
  "Set the region around the whole buffer, activating the region.  Push the
   point on the mark ring beforehand, so that popping the mark stack twice
   get it back.  With a prefix argument, put mark at the beginning and
   point at end."
  (let* ((region (buffer-region (current-buffer)))
	 (start (region-start region))
	 (end (region-end region))
	 (point (current-point)))
    (push-buffer-mark (copy-mark point))
    (cond (p (push-buffer-mark (copy-mark start) t)
	     (move-mark point end))
	  (t (push-buffer-mark (copy-mark end) t)
	     (move-mark point start)))))


#[ Kill Ring

{variable:ed:*kill-ring*}
{function:ed:kill-region}
{function:ed:kill-characters}
{evariable:Character Deletion Threshold}
]#


;;;; KILL-REGION and KILL-CHARACTERS primitives.

(proclaim '(special *delete-char-region*))

;;; KILL-REGION first checks for any characters that may need to be added to
;;; the region.  If there are some, we possibly push a region onto *kill-ring*,
;;; and we use the top of *kill-ring*.  If there are no characters to deal
;;; with, then we make sure the ring isn't empty; if it is, just push our
;;; region.  If there is some region in *kill-ring*, then see if the last
;;; command type was a region kill.  Otherwise, just push the region.
;;;
(defun kill-region (region current-type)
  "Kill $region, saving it in *kill-ring*.

   $current-type is either :kill-forward or :kill-backward.

   When the `last-command-type' is :kill-forward or :kill-backward, add
   region to the beginning or end, respectively, of *kill-ring*.  Always
   set `last-command-type' to $current-type afterwards.

   Interact with `kill-characters'."
  (let ((last-type (last-command-type))
	(insert-mark (copy-mark (region-start region) :left-inserting)))
    (cond ((or (eq last-type :char-kill-forward)
	       (eq last-type :char-kill-backward))
	   (when *delete-char-region*
	     (ring-push *delete-char-region* *kill-ring*)
	     (setf *delete-char-region* nil))
	   (setf region (kill-region-top-of-ring region current-type))
	   (update-kill-browse)
	   (invoke-hook kill-ring-hook region))
	  ((zerop (ring-length *kill-ring*))
	   (setf region (delete-and-save-region region))
	   (push-kill region))
	  ((or (eq last-type :kill-forward) (eq last-type :kill-backward))
	   (setf region (kill-region-top-of-ring region current-type))
	   (update-kill-browse)
	   (invoke-hook kill-ring-hook region))
	  (t
	   (setf region (delete-and-save-region region))
	   (push-kill region)))
    (make-region-undo :insert "kill" (copy-region region) insert-mark)
    (setf (last-command-type) current-type)))

(defun kill-region-top-of-ring (region current-type)
  (let ((r (ring-ref *kill-ring* 0)))
    (ninsert-region (if (eq current-type :kill-forward)
			(region-end r)
			(region-start r))
		    (delete-and-save-region region))
    r))

(defevar "Character Deletion Threshold"
  "If more than this many characters are deleted by a character deletion
   command (via `kill-characters'), then the characters are saved on the
   kill ring.  Example commands are `Delete Next Character', `Delete
   Previous Character', or `Delete Previous Character Expanding Tabs'."
  :value 5)

(defvar *delete-char-region* nil)
(defvar *delete-char-count* 0)

;;; KILL-CHARACTERS makes sure there are count characters with CHARACTER-OFFSET.
;;; If the last command type was a region kill, we just use the top region
;;; in *kill-ring* by making KILL-CHAR-REGION believe *delete-char-count* is
;;; over the threshold.  We don't call KILL-REGION in this case to save making
;;; undo's -- no good reason.  If we were just called, then increment our
;;; global counter.  Otherwise, make an empty region to keep KILL-CHAR-REGION
;;; happy and increment the global counter.
;;;
(defun kill-characters (mark count)
  "Kill $count characters after $mark if $count is positive, otherwise
   before $mark if $count is negative.

   When $count is greater than or equal to *Character Deletion Threshold*,
   the save the killed characters on *kill-ring*.  If this is called
   multiple times between the setting of `last-command-type', then the
   count accumulates for comparison with the threshold.

   Set `last-command-type', and interacts with `kill-region'.  When adding
   a new region to *kill-ring*, set `last-command-type' to :kill-forward
   (if count is positive) or :kill-backward (if count is negative).  When
   last-command-type is :kill-forward or :kill-backward, add the killed
   characters to the beginning (if count is negative) or the end (if count
   is positive) of *kill-ring*, and set `last-command-type' as if it added
   a new region to *kill-ring*.  When the characters are simply deleted,
   set `last-command-type' to :char-kill-forward or :char-kill-backward
   depending on whether count is positive or negative, respectively.

   If there are too few characters in the appropriate direction, return (),
   otherwise return $mark."
  (if (zerop count)
      mark
      (with-mark ((temp mark :left-inserting))
	(if (character-offset temp count)
	    (let ((current-type (if (plusp count)
				    :char-kill-forward
				    :char-kill-backward))
		  (last-type (last-command-type))
		  (del-region (if (mark< temp mark)
				  (region temp mark)
				  (region mark temp))))
	      (cond ((or (eq last-type :kill-forward)
			 (eq last-type :kill-backward))
		     (setf *delete-char-count*
			   (value character-deletion-threshold))
		     (setf *delete-char-region* nil))
		    ((or (eq last-type :char-kill-backward)
			 (eq last-type :char-kill-forward))
		     (incf *delete-char-count* (abs count)))
		    (t
		     (setf *delete-char-region* (make-empty-region))
		     (setf *delete-char-count* (abs count))))
	      (kill-char-region del-region current-type)
	      mark)
	    nil))))

(defun kill-char-region (region current-type)
  (let ((deleted-region (delete-and-save-region region)))
    (cond ((< *delete-char-count* (value character-deletion-threshold))
	   (ninsert-region (if (eq current-type :char-kill-forward)
			       (region-end *delete-char-region*)
			       (region-start *delete-char-region*))
			   deleted-region)
	   (setf (last-command-type) current-type))
	  (t
	   (when *delete-char-region*
	     (ring-push *delete-char-region* *kill-ring*)
	     (setf *delete-char-region* nil))
	   (let ((r (ring-ref *kill-ring* 0)))
	     (ninsert-region (if (eq current-type :char-kill-forward)
				 (region-end r)
				 (region-start r))
			     deleted-region))
	   (setf (last-command-type)
		 (if (eq current-type :char-kill-forward)
		     :kill-forward
		     :kill-backward))))
    (update-kill-browse)
    (invoke-hook kill-ring-hook region)))


#[ Killing Commands

Most commands which kill text append into the kill ring, meaning that
consecutive uses of killing commands will insert all text killed into the
top entry in the kill ring.  This allows large pieces of text to be killed
by repeatedly using a killing command.

{command:Kill Line}
{command:Backward Kill Line}
{command:Kill Next Word}
{command:Kill Previous Word}
]#


;;;; Commands.

(defcommand "Kill Region" ()
  "Kill the region (the text between the point and the mark), pushing it
   onto the kill ring.  If the region is passive and `Active Regions
   Enabled' is set then signal an error."
  (kill-region (current-region)
	       (if (mark< (current-mark) (current-point))
		   :kill-backward
		   :kill-forward)))

(defun update-kill-browse ()
  (let ((buffer (getstring "Kill Ring" *buffer-names*)))
    (when (and buffer
	       (string= (buffer-major-mode buffer) "Kill-Browse"))
      (refresh-kill-browse buffer))))

(defun push-kill (region)
  (ring-push region *kill-ring*)
  (update-kill-browse)
  (invoke-hook kill-ring-hook region))

(defcommand "Save Region" ()
  "Insert the region onto the kill ring.  If the region is passive and
   `Active Regions Enabled' is set then signal an error."
  (or (value active-regions-enabled) (activate-region))
  ;; FIX this an make an option (make this an option?)
  (if (mark< (current-mark) (window-display-start (current-window)))
      (message "~A" (line-string (mark-line (current-mark))))
      (show-mark (current-mark) (current-window) 0.16)) ; FIX (value ..time)
  (push-kill (copy-region (current-region))))

(defcommand "Kill Next Word" (p)
 "Kill the characters from the point to the end of the current or next
  word.  If a prefix argument is supplied, then kill that many words.

  Append the text to the text currently at the top of the kill ring if that
  text was next to the text being killed."
  (let ((point (current-point))
	(num (or p 1)))
    (with-mark ((mark point :temporary))
      (if (word-offset mark num)
	  (if (minusp num)
	      (kill-region (region mark point) :kill-backward)
	      (kill-region (region point mark) :kill-forward))
	  (editor-error "Too few words to kill.")))))

(defcommand "Kill Previous Word" (p)
  "Kill the characters from the point to the beginning of the current or
   next word.  If a prefix argument is supplied, then kill that many words.
   Append the text to the text currently at the top of the kill ring if
   that text was next to the text being killed."
  (kill-next-word-command (- (or p 1))))

(defcommand "Kill Line" (p)
  "Kill the characters from the point to the end of the current line.  If
   the point is at the end of the line then kill the end of line marker.
   With a prefix argument, kill that many lines past the point (or before
   if the prefix is negative)."
  (let* ((point (current-point))
	 (line (mark-line point)))
    (with-mark ((mark point))
      (cond
       (p
	(and (/= (mark-charpos point) 0)
	     (minusp p)
	     (incf p))
	(or (line-offset mark p 0)
	    (editor-error "Too few lines."))
	(if (plusp p)
	    (kill-region (region point mark) :kill-forward)
	    (kill-region (region mark point) :kill-backward)))
       (t
	(cond ((not (blank-after-p mark))
	       (line-end mark))
	      ((line-next line)
	       (line-start mark (line-next line)))
	      ((fi (end-line-p mark))
	       (line-end mark))
	      (t
	       (editor-error "Empty buffer.")))
	(kill-region (region point mark) :kill-forward))))))

(defcommand "Backward Kill Line" (p)
  "Kill from the point to the beginning of the line.  If point is at the
   beginning of the line, kill the newline and any trailing space on the
   previous line.  With a prefix argument, call `Kill Line' with the
   argument negated."
  (if p
      (kill-line-command (- p))
      (with-mark ((m (current-point)))
	(cond ((zerop (mark-charpos m))
	       (mark-before m)
	       (unless (reverse-find-attribute m :space #'zerop)
		 (buffer-start m)))
	       (t
		(line-start m)))
	(kill-region (region m (current-point)) :kill-backward))))

(defcommand "Delete Blank Lines" ()
  "Flush all blank lines surrounding the current line, leaving the point on
   a single blank line.  If the point is already on a single blank line,
   then flush that line.  If the point is on a non-blank line, then flush
   all blank lines immediately following that line.  This command is often
   used to clean up after `Open Line'."
  (let ((point (current-point)))
    (with-mark ((beg-mark point :left-inserting)
		(end-mark point :right-inserting))
      ;; handle case when the current line is blank
      (when (blank-line-p (mark-line point))
	;; back up to last non-whitespace character
	(reverse-find-attribute beg-mark :whitespace #'zerop)
	(when (previous-character beg-mark)
	  ;; that is, we didn't back up to the beginning of the buffer
	  (unless (same-line-p beg-mark end-mark)
	    (line-offset beg-mark 1 0)))
	;; if isolated, zap the line else zap the blank ones above
	(cond ((same-line-p beg-mark end-mark)
	       (line-offset end-mark 1 0))
	      (t
	       (line-start end-mark)))
	(delete-region (region beg-mark end-mark)))
      ;; always delete all blank lines after the current line
      (move-mark beg-mark point)
      (when (line-offset beg-mark 1 0)
	(move-mark end-mark beg-mark)
	(find-attribute end-mark :whitespace #'zerop)
	(when (next-character end-mark)
	  ;; that is, we didn't go all the way to the end of the buffer
	  (line-start end-mark))
	(delete-region (region beg-mark end-mark))))))


#[ Kill Ring Manipulation

{command:Yank}
{command:Rotate Kill Ring}

`Rotate Kill Ring' is used to step back through the text in the kill ring
to find a desired portion of text on the kill ring.

{command:Kill Region}
{command:Save Region}
{command:Browse Kill Ring}
]#

(defcommand "Yank" (p)
  "Yank back the most recently killed piece of text, i.e. insert the top
   item in the kill-ring at the point.  Leave the mark before the inserted
   text and the point after it.

   If a prefix argument is supplied, then yank back the text that distance
   into the kill ring."
  (let ((idx (1- (or p 1))))
    (cond ((> (ring-length *kill-ring*) idx -1)
	   (let* ((region (ring-ref *kill-ring* idx))
		  (point (current-point))
		  (mark (copy-mark point)))
	     (push-buffer-mark mark)
	     (insert-region point region)
	     (make-region-undo :delete "Yank"
			       (region (copy-mark mark) (copy-mark point))))
	   (update-kill-browse)
	   (setf (last-command-type) :yank))
	  (t (editor-error "Too few entries in kill ring.")))))
;;;
(push :yank *ephemerally-active-command-types*)

(defcommand "Rotate Kill Ring" (p)
  "If the previous command was a `Yank' or `Rotate Kill Ring' then rotate
   the kill ring forward, replacing the most recently yanked text with the
   next most recent text in the kill ring.  Otherwise call `Yank' with the
   prefix.

   If a prefix argument is supplied, then rotate the kill ring that many
   times."
  (if (zerop (ring-length *kill-ring*))
      (editor-error "Kill ring empty."))
  (if (eq (last-command-type) :yank)
      (let ((point (current-point))
	    (mark (current-mark)))
	(delete-region (region mark point))
	(rotate-ring *kill-ring* (or p 1))
	(insert-region point (ring-ref *kill-ring* 0))
	(make-region-undo :delete "Yank"
			  (region (copy-mark mark) (copy-mark point)))
	(update-kill-browse)
	(setf (last-command-type) :yank))
      (yank-command p)))


;;;; Kill ring browsing.

#|
(defun setup-kill-browse-mode (buffer)
  "Setup $buffer for Kill-Browse mode."
  (highlight-kill-browse buffer)
  (pushnew '("Kill-Browse" () highlight-visible-kill-browse)
	   *mode-highlighters*))
|#

(defmode "Kill Browse" :major-p t
  :short-name "Kill"
  ;:setup-function #'setup-kill-browse-mode
  :documentation "Kill ring browsing mode.")

(defun refresh-kill-browse (buffer)
  "Refresh the kill ring listing in $buffer."
  (let ((pos (count-lines (region (buffer-start-mark buffer)
				  (buffer-point buffer)))))
    (setf (buffer-writable buffer) t)
    (delete-region (buffer-region buffer))
    (let ((mark (copy-mark (buffer-point buffer))))
      (loop for index from 0 to (1- (ring-length *kill-ring*)) do
	(let ((entry (ring-ref *kill-ring* index)))
	  (insert-region mark entry)
	  (insert-character mark #\newline)
	  (setf (getf (line-plist (mark-line mark)) 'kill-entry)
		entry)
	  ;(insert-string mark "==================================")
	  (insert-string mark "------------")
	  (insert-character mark #\newline))))
    (setf (buffer-major-mode buffer) "Kill Browse")
    (highlight-kill-browse buffer)
    (setf (buffer-writable buffer) ())
    (setf (buffer-modified buffer) ())
    (buffer-start (buffer-point buffer))
    (line-offset (buffer-point buffer) (1- pos))))

(defcommand "Browse Kill Ring" ()
  "Browse the kill ring."
  (let ((buffer (make-buffer "Kill Ring")))
    (cond (buffer
	   (refresh-kill-browse buffer)
	   (change-to-buffer buffer))
	  (t
	   (change-to-buffer (getstring "Kill Ring" *buffer-names*))
	   (refresh-kill-browse (current-buffer))))))

(defun kill-on-line (line)
  "Return the kill ring entry that ends on $line, if any."
  (getf (line-plist line) 'kill-entry))

(defcommand "Kill Browse Insert Kill" ()
  "Insert the kill entry under point at the current point of the next
   buffer."
  (do-lines-from-mark (line (current-point))
    (let ((kill (kill-on-line line)))
      (when kill
	(rotate-buffers-forward-command)
	(insert-region (current-point) kill)
	(return)))))

(defcommand "Refresh Kill Browse" ()
  "Refresh the browse of the kill ring entries."
  (refresh-kill-browse (current-buffer)))

(defcommand "Next Kill" (p)
  "Move to the next kill entry.  With a prefix move to the previous entry."
  (setq p (if p -1 1))
  (do-lines-from-mark (line (current-point) :backwards (minusp p))
    (when (kill-on-line line)
      (move-mark (current-point) (mark line 0))
      (line-offset (current-point) p 0)
      (return))))

(defcommand "Previous Kill" (p)
  "Move to the previous kill entry.  With a prefix move to the next entry."
  (next-kill-command (fi p)))

(defmacro rehighlight-kill-line (line info)
  `(progn
     (dolist (fmark (ch-info-font-marks ,info))
       (delete-font-mark fmark))
     (push (color-mark ,line 0 :special-form)
	   (ch-info-font-marks ,info))))

#|
(defun highlight-visible-kill-browse (buffer)
  (dolist (window (buffer-windows buffer))
    (or (eq (edi::window-first-changed window) edi::the-sentinel)
	(loop
	  for num from (window-height window) downto 1
	  for line = (mark-line (window-display-start window))
	  then (line-next line)
	  while line
	  do
	  (if (plusp (line-length line))
	      (let ((info (check-highlight-line line 'kill-entry-chi)))
		(and info
		     (kill-on-line line)
		     (rehighlight-kill-line line info))))))))
|#

(defun highlight-kill-browse (buffer)
  (do-buffer-lines (line buffer)
    (when (> (line-length line) 0)
      (let ((info (check-highlight-line line 'kill-entry-chi)))
	(and info
	     (kill-on-line line)
	     (rehighlight-kill-line line info))))))
