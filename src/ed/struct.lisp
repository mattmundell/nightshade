;;; Structures and assorted macros.

(in-package "EDI")

(export '(mark mark-line mark-charpos markp region region-start region-end
	  regionp buffer bufferp buffer-modes buffer-point buffer-writable
	  buffer-delete-hook buffer-variables buffer-windows buffer-write-date
	  buffer-line-count
	  region regionp region-start region-end window windowp window-height
	  window-width window-display-start window-display-end window-point
	  window-display-recentering window-line-number
	  commandp command command-function command-documentation
	  modeline-field modeline-field-p))

#[ Representation of Text

[ Lines   ]
[ Marks   ]
[ Regions ]
]#


;;;; Marks.

#[ Marks

A mark indicates a specific position within the text represented by a line
and a character position within that line.  Although a mark is sometimes
loosely referred to as pointing to some character, it in fact points
between characters.  If the charpos (the column) is zero, the previous
character is the newline character separating the previous line from the
mark's line.  If the charpos is equal to the number of characters in the
line, the next character is the newline character separating the current
line from the next.  If the mark's line is the first line in a buffer, a
mark with charpos of zero has no previous character; if the mark's line is
the last line in the buffer, a mark with charpos equal to the length of the
line has no next character.

This section discusses the very basic operations involving marks.  A lot
of editor programming is built on altering some text at a mark.  Extended
uses of marks is described in chapter FIX [doing-stuff].

(FIX this is being reconsidered, as the implementation increases mark sizes)
Marks are also [Streams], so a buffer can be modified by using the stream
functions on a mark.

[ Kinds of Marks ]
[ Mark Functions ]
[ Making Marks   ]
[ Moving Marks   ]
]#

#[ Kinds of Marks

A mark may have one of two lifetimes: temporary or permanent.
Permanent marks remain valid after arbitrary operations on the text; temporary
marks do not.  Temporary marks are used because less bookkeeping overhead is
involved in their creation and use.  If a temporary mark is used after the text
it points to has been modified results will be unpredictable.  Permanent marks
continue to point between the same two characters regardless of insertions and
deletions made before or after them.

There are two different kinds of permanent marks which differ only in their
behavior when text is inserted at the position of the mark; text is
inserted to the left of a left-inserting mark and to the right of
right-inserting mark.
]#

(defstruct (mark (:print-function %print-hmark)
		 (:predicate markp)
		 (:copier nil)
		 ;; FIX this greatly increases the size of a mark
		 (:include lisp-stream
			   (in #'mark-stream-read-char)
			   (out #'insert-character)
			   (sout #'insert-string)
			   (misc #'mark-stream-handle-misc))
		 (:constructor internal-make-mark (line charpos %kind)))
  "An editor mark object."
  line     ; pointer to line
  charpos  ; character position
  %kind)   ; type of mark

(setf (documentation 'markp 'function)
  "Return true if mark is an editor $mark, false otherwise.")
(setf (documentation 'mark-line 'function)
  "Return the line that $mark points to.")
(setf (documentation 'mark-charpos 'function)
  "Return the character position of $mark.  A mark's character position is
   the index within the mark's line of the character following the mark (or
   the character that would follow mark if it the last character in the
   buffer).")
(setf (documentation 'mark-kind 'function)
  "Return one of :right-inserting, :left-inserting or :temporary depending
   on the mark's kind.")

(defstruct (font-mark (:print-function
		       (lambda (s stream d)
			 (declare (ignore d))
			 (format stream "#<Editor Font-Mark ~D \""
				 (font-mark-font s))
			 (%print-before-mark s stream)
			 (write-string "/\\" stream)
			 (%print-after-mark s stream)
			 (write-string "\">" stream)))
		      (:include mark)
		      (:copier nil)
		      (:constructor internal-make-font-mark
				    (line charpos %kind font)))
  font
  fore-color
  back-color)

(defmacro fast-font-mark-p (s)
  `(typep ,s 'font-mark))


;;;; Regions, buffers, modeline fields.

#[ Regions

A region is simply a pair of marks: a starting mark and an ending mark.
The text in a region consists of the characters following the starting
mark and preceding the ending mark (keep in mind that a mark points between
characters on a line, not at them).

By modifying the starting or ending mark in a region it is possible to
produce regions with a start and end which are out of order or even in
different buffers.  The use of such regions is undefined and may
result in arbitrarily bad behavior.

[ Region Functions ]
]#

;;; The region object:
;;;
(defstruct (region (:print-function %print-hregion)
		   (:predicate regionp)
		   (:copier nil)
		   (:constructor internal-make-region (start end)))
  "An editor region object."
  start					; starting mark
  end)					; ending mark

(setf (documentation 'regionp 'function)
  "Return true if $region is an region object, else ().")
(setf (documentation 'region-end 'function)
  "Return the mark that is the end of $region.")
(setf (documentation 'region-start 'function)
  "Return the mark that is the start of region.")

#[ Buffers (extension)

A buffer is an environment within the editor consisting of:

  * A name.

  * A piece of text.

  * A current focus of attention, the point.

  * An associated file (optional).

  * A write protect flag.

  * Some editor variables.

  * Some key bindings.

  * Some collection of modes.

  * Some windows in which it is displayed.

  * A list of modeline fields (optional).

[ The Current Buffer ]
[ Buffer Functions   ]
[ Modelines          ]
]#

;;; The buffer structure.
;;;
(defstruct (buffer (:constructor internal-make-buffer)
		   (:print-function %print-hbuffer)
		   (:copier internal-copy-buffer)
		   (:predicate bufferp))
  "An editor buffer object."
  %name			      ; name of the buffer (a string)
  %region		      ; the buffer's region
  %deep-region		      ; the underlying region, if any
  %pathname		      ; associated pathname
  modes			      ; list of buffer's mode names
  mode-objects		      ; list of buffer's mode objects
  bindings		      ; buffer's command table
  point			      ; current position in buffer
  (%writable t)		      ; t => can alter buffer's region
  (modified-tick -2)	      ; The last time the buffer was modified.
  (unmodified-tick -1)	      ; The last time the buffer was unmodified
  windows		      ; List of all windows into this buffer.
  var-values		      ; the buffer's local variables
  variables		      ; string-table of local variables
  write-date		      ; File-Write-Date for pathname.
  display-start		      ; Window display start when switching to buf.
  %modeline-fields	      ; List of modeline-field-info's.
  (line-count 0)              ; Periodically updated number of lines in
                              ;     buffer, for modeline field.
  (delete-hook nil))	      ; List of functions to call upon deletion.

(setf (documentation 'bufferp 'function)
  "Returns true if $buffer is a buffer object, else ().")
(setf (documentation 'buffer-modes 'function)
  "Return a list of the names of the [modes] active in $buffer, with the
   major mode first, followed by any minor modes. ")
(setf (documentation 'buffer-point 'function)
  "Return the mark that is the current location within $buffer.  To move
   the point, use `move-mark' or `move-to-position' rather than setting
   buffer-point with `setf'.")
(setf (documentation 'buffer-windows 'function)
  "Return a list of all the windows in which $buffer may be displayed.
   This list may include windows which are currently out of view.
   [Windows] discusses windows.")
(setf (documentation 'buffer-variables 'function)
  "Return the [string-table] containing the names of the [variables] local to
   $buffer.")
(setf (documentation 'buffer-write-date 'function)
  "Return in universal time format the write date of the file associated
   with the buffer.  Only set if there is a file or if the date is known.")
(setf (documentation 'buffer-delete-hook 'function)
  "Return the list of buffer specific functions that delete-buffer invokes
   when deleting $buffer.  This is `setf'able.")
(setf (documentation 'buffer-line-count 'function)
 "The number of lines in the buffer, for modeline fields.  Updated on
  certain hooks.")

#[ Modelines

A Buffer may specify a modeline, a line of text which is displayed across the
bottom of a window to indicate status information.  Modelines are described as
a list of modeline-field objects which have individual update functions and
are optionally fixed-width.  These have an eql name for convenience in
referencing and updating, but the name must be unique for all created
modeline-field objects.  When creating a modeline-field with a specified width,
the result of the update function is either truncated or padded on the right to
meet the constraint.  All modeline-field functions must return simple strings
with standard characters, and these take a buffer and a window as arguments.
Modeline-field objects are typically shared amongst, or aliased by, different
buffers' modeline fields lists.  These lists are unique allowing fields to
behave the same wherever they occur, but different buffers may display these
fields in different arrangements.

Whenever one of the following changes occurs, all of a buffer's modeline fields
are updated:

  - A buffer's major mode is set.

  - One of a buffer's minor modes is turned on or off.

  - A buffer is renamed.

  - A buffer's pathname changes.

  - A buffer's modified status changes.

  - A window's buffer is changed.

The policy is that whenever one of these changes occurs, it is guaranteed that
the modeline will be updated before the next trip through redisplay.
Furthermore, since the system cannot know what modeline-field objects the
user has added whose update functions rely on these values, or how he has
changed `Default Modeline Fields', we must update all the fields.  When any
but the last occurs, the modeline-field update function is invoked once for
each window into the buffer.  When a window's buffer changes, each
modeline-field update function is invoked once; other windows' modeline
fields should not be affected due to a given window's buffer changing.

The user should note that modelines can be updated at any time, so update
functions should be careful to avoid needless delays (for example, waiting for
a local area network to determine information).

{function:ed:make-modeline-field}
{function:ed:modeline-field-p}
{function:ed:modeline-field-name}
{function:ed:modeline-field}
{function:ed:modeline-field-function}
{function:ed:modeline-field-width}
{function:ed:modeline-field-primary-click}
{function:ed:modeline-field-at}
{function:ed:buffer-modeline-fields}
{function:ed:buffer-modeline-field-p}
{function:ed:update-modeline-fields}
{function:ed:update-modeline-field}
]#

;;; Modeline fields.
;;;
(defstruct (modeline-field (:print-function print-modeline-field)
			   (:constructor %make-modeline-field
					 (%name %function %width
					  primary-click)))
  "This is one item displayed in an editor window's modeline."
  %name		; EQL name of this field.
  %function	; Function that returns a string for this field.
  %width	; Width to display this field in.
  primary-click)  ; Function that handles a primary click.

(setf (documentation 'modeline-field-p 'function)
  "Return true if $x d is a modeline-field, else return ().")

(defstruct (modeline-field-info (:print-function print-modeline-field-info)
				(:conc-name ml-field-info-)
				(:constructor make-ml-field-info (field)))
  field
  (start nil)
  (end nil))


;;;; The mode object.

(defstruct (mode-object (:predicate modep)
			(:copier nil)
			(:print-function %print-editor-mode))
  name                   ; name of this mode
  short-name             ; short name, for the modeline
  setup-function         ; setup function for this mode
  cleanup-function       ; Cleanup function for this mode
  bindings               ; The mode's command table.
  transparent-p		 ; Are key-bindings transparent?
  hook-name              ; The name of the mode hook.
  major-p                ; Is this a major mode?
  precedence		 ; The precedence for a minor mode.
  character-attributes   ; Mode local character attributes
  variables              ; String-table of mode variables
  var-values             ; Alist for saving mode variables
  documentation)         ; Introductory comments for mode describing commands.

(defun %print-editor-mode (object stream depth)
  (declare (ignore depth))
  (write-string "#<Editor Mode \"" stream)
  (write-string (mode-object-name object) stream)
  (write-string "\">" stream))


;;;; Variables.

;;; This holds information about editor variables, and the system stores
;;; these structures on the property list of the variable's symbolic
;;; representation under the 'editor-variable-value property.
;;;
(defstruct (variable-object
	    (:print-function
	     (lambda (object stream depth)
	       (declare (ignore depth))
	       (format stream "#<Editor Variable-Object ~S>"
		       (variable-object-name object))))
	    (:copier nil)
	    (:constructor make-variable-object (documentation name)))
  value		; The value of this variable.
  hooks		; The hook list for this variable.
  down		; The variable-object for the previous value.
  documentation ; The documentation.
  source        ; The file or buffer from which this variable was defined.
  name)		; The string name.


;;;; Windows, dis-lines, and font-changes.

;;; The window object:
;;;
(defstruct (window (:constructor internal-make-window)
		   (:predicate windowp)
		   (:copier nil)
		   (:print-function %print-hwindow))
  "This structure implements an editor window."
  tick				; The last time this window was updated.
  %buffer			; Buffer displayed in this window.
  height			; Height of window in lines.
  width				; Width of the window in characters.
  old-start			; The charpos of the first char displayed.
  first-line			; The head of the list of dis-lines.
  last-line			; The last dis-line displayed.
  first-changed			; The first changed dis-line on last update.
  last-changed			; The last changed dis-line.
  spare-lines			; The head of the list of unused dis-lines
  (old-lines 0)			; Slot used by display to keep state info
  hunk				; The device hunk that displays this window.
  display-start			; First character position displayed.
  display-end			; Last character displayed.
  point				; Position of the cursor in the window.
  modeline-dis-line		; Dis-line for modeline display.
  modeline-buffer		; Complete string of all modeline data.
  modeline-buffer-len           ; Valid chars in modeline-buffer.
  modeline-fore-color           ; Foreground editor color of modeline.
  modeline-back-color           ; Background editor color of modeline.
  (line-number 0)               ; Periodically updated line number of buffer
                                ;    point, for modeline field.
  (display-recentering nil))	; Tells whether redisplay recenters window
				;    regardless of whether it is current.

(setf (documentation 'windowp 'function)
  "Return true if $window is an editor window, else ().")
(setf (documentation 'window-height 'function)
  "Return the height of the area of $window used for displaying the buffer,
   in character positions.

   May be changed with `setf'.")
(setf (documentation 'window-width 'function)
  "Return the width of the area of $window used for displaying the buffer,
   in character positions.

   May be changed with `setf'.")
(setf (documentation 'window-display-start 'function)
  "Return the mark that points before the first character displayed in
   $window.

   If window is the current window, then moving the start may leave the
   window in place, since recentering may move it back to approximately
   where it was originally.")
(setf (documentation 'window-display-end 'function)
  "Return the mark that points after the last character displayed in
   $window.

   Redisplay always moves the display end to after the last character
   displayed.")
(setf (documentation 'window-point 'function)
  "Return as a mark the position in the buffer in $window where the cursor
   is displayed.

   This may be set with `setf'.  If $window is the current window, then
   setting the point will have little effect as $window is forced to track
   the buffer point.  For other windows, the window point is the position
   that the buffer point will be moved to when the window becomes
   current.")
(setf (documentation 'window-display-recentering 'function)
 "Return whether redisplay ensures that the the point in the buffer in
  $window is visible after redisplay.

  This is `setf'able.  Changing window's buffer sets this to nil via
  *Window Buffer Hook*.")
(setf (documentation 'window-line-number 'function)
 "The line number of the buffer point, for modeline fields.  Updated on
  certain hooks.")

(defstruct (dis-line (:copier nil)
		     (:constructor nil))
  chars			      ; The line-image to be displayed.
  (length 0 :type fixnum)     ; Length of line-image.
  font-changes)		      ; Font-Change structures for changes in this line.

(defstruct (window-dis-line (:copier nil)
			    (:include dis-line)
			    (:constructor make-window-dis-line (chars))
			    (:conc-name dis-line-))
  old-chars		      ; Line-Chars of line displayed.
  line			      ; Line displayed.
  (flags 0 :type fixnum)      ; Bit flags indicate line status.
  (delta 0 :type fixnum)      ; # lines moved from previous position.
  (position 0 :type fixnum)   ; Line # to be displayed on.
                              ; (FIX think dis-line # inc by 1 from start of win)
  (end 0 :type fixnum))	      ; Index after last logical character displayed.

(defstruct (font-change (:copier nil)
			(:constructor make-font-change (next)))
  x			      ; X position that change takes effect.
  font			      ; Index into font-map of font to use.
  fore-color                  ; Color (structure) to draw text.
  back-color                  ; Color (structure) to paint background.
  next			      ; The next Font-Change on this dis-line.
  mark)			      ; Font-Mark responsible for this change.


;;;; Font family.

(defstruct font-family
  map			; Font-map for hunk.
  height		; Height of char box includung VSP. FIX vsp?
  width			; Width of font.
  baseline		; Pixels from top of char box added to Y.
  cursor-width		; Pixel width of cursor.
  cursor-height		; Pixel height of cursor.
  cursor-x-offset	; Added to pos of UL corner of char box to get
  cursor-y-offset)	; UL corner of cursor blotch.


;;;; Attribute descriptors.

(defstruct (attribute-descriptor
	    (:copier nil)
	    (:print-function %print-attribute-descriptor))
  "This structure is used internally in the editor to describe a character
   attribute."
  name
  keyword
  documentation
  vector
  hooks
  end-value)


;;;; Commands.

(defstruct (command (:constructor internal-make-command
				  (%name documentation function))
		    (:copier nil)
		    (:predicate commandp)
		    (:print-function %print-hcommand))
  %name		   ; The name of the command
  documentation	   ; Command documentation string or function
  function	   ; The function which implements the command
  %bindings)	   ; Places where command is bound

(setf (documentation 'commandp 'function)
  "Returns true if the argument is an editor command, () otherwise.")
(setf (documentation 'command-documentation 'function)
  "Return the documentation for an editor command.  Command documentation
   may be either a string or a function.  This may be set with Setf.")
(setf (documentation 'command-function 'function)
  "Return the function for an editor command.  This may be set with Setf.")
(setf (documentation 'command-name 'function)
  "Return the name for an editor command.  This may be set with Setf.")


;;;; Random typeout streams.

;;; These streams write to random typeout buffers for WITH-POP-UP-DISPLAY.
;;;
(defstruct (random-typeout-stream (:include sys:lisp-stream)
				  (:print-function print-random-typeout-stream)
				  (:constructor
				   make-random-typeout-stream (mark)))
  mark		       ; The buffer point of the associated buffer.
  window	       ; The editor window all this shit is in.
  more-mark	       ; The mark that is not displayed when we need to more.
  no-prompt	       ; T when we want to exit, still collecting output.
  (first-more-p t))    ; T until the first time we more. Nil after.

(defun print-random-typeout-stream (object stream ignore)
  (declare (ignore ignore))
  (format stream "#<Editor Random-Typeout-Stream ~S>"
	  (buffer-name
	   (line-buffer (mark-line (random-typeout-stream-mark object))))))


;;;; Redisplay devices.

;;; Devices contain monitor specific redisplay methods referenced by
;;; redisplay independent code.
;;;
(defstruct (device (:print-function print-device)
		   (:constructor %make-device))
  name			; simple-string such as "concept" or "lnz".
  init			; fun to call whenever going into the editor.
			; args: device
  exit			; fun to call whenever leaving the editor.
			; args: device
  smart-redisplay	; fun to redisplay a window on this device.
			; args: window &optional recenterp
  dumb-redisplay	; fun to redisplay a window on this device.
			; args: window &optional recenterp
  after-redisplay	; args: device
			; fun to call at the end of redisplay entry points.
  clear			; fun to clear the entire display.
			; args: device
  note-read-wait	; fun to somehow note on display that input is expected.
			; args: on-or-off
  put-cursor		; fun to put the cursor at (x,y) or (column,line).
			; args: hunk &optional x y
  show-mark		; fun to display the screens cursor at a certain mark.
			; args: window x y time
  next-window		; funs to return the next and previous window
  previous-window	;    of some window.
			; args: window
  make-window		; fun to make a window on the screen.
			; args: device start-mark
			;       &optional modeline-string modeline-function
  delete-window		; fun to remove a window from the screen.
			; args: window
  set-window-height     ; fun to set height
			; args: window height
  set-foreground-color  ; fun to set foreground color
			; args: window color
  set-background-color  ; fun to set background color
			; args: window color
  random-typeout-setup	; fun to prepare for random typeout.
  			; args: device n
  random-typeout-cleanup; fun to clean up after random typeout.
  			; args: device degree
  random-typeout-line-more ; fun to keep line-buffered streams up to date.
  random-typeout-full-more ; fun to do full-buffered  more-prompting.
			   ; args: # of newlines in the object just inserted
			   ;    in the buffer.
  force-output		; if true, fun to force any output possibly buffered.
  finish-output		; if true, fun to force output and hand until done.
  			; args: device window
  beep			; fun to beep or flash the screen.
  bottom-window-base    ; bottom text line of bottom window.
  make-color            ; fun to make a new color.
                        ; args: device color
  hunks)		; list of hunks on the screen.

(defun print-device (obj str n)
  (declare (ignore n))
  (format str "#<Editor Device ~S>" (device-name obj)))


(defstruct (bitmap-device #|(:print-function print-device)|#
			  (:constructor %make-bitmap-device)
			  (:include device))
  display)		      ; CLX display object.

(defstruct (tty-device #|(:print-function print-device)|#
		       (:constructor %make-tty-device)
		       (:include device))
  dumbp			; t if it does not have line insertion and deletion.
  lines			; number of lines on device.
  columns		; number of columns per line.
  display-string	; fun to display a string of characters at (x,y).
			; args: hunk x y string &optional start end
  standout-init         ; fun to put terminal in standout mode.
			; args: hunk
  standout-end          ; fun to take terminal out of standout mode.
			; args: hunk
  clear-lines		; fun to clear n lines starting at (x,y).
			; args: hunk x y n
  clear-to-eol		; fun to clear to the end of a line from (x,y).
			; args: hunk x y
  space-to-eol		; fun to clear to the end of a line with spaces.
			; args: hunk x y &optional back-color
  clear-to-eow		; fun to clear to the end of a window from (x,y).
			; args: hunk x y
  open-line		; fun to open a line moving lines below it down.
			; args: hunk x y &optional n
  delete-line		; fun to delete a line moving lines below it up.
			; args: hunk x y &optional n
  insert-string		; fun to insert a string in the middle of a line.
			; args: hunk x y string &optional start end
  delete-char		; fun to delete a character from the middle of a line.
			; args: hunk x y &optional n
  (cursor-x 0)		; column the cursor is in.
  (cursor-y 0)		; line the cursor is on.
  standout-init-string  ; string to put terminal in standout mode.
  standout-end-string   ; string to take terminal out of standout mode.
  clear-to-eol-string	; string to cause device to clear to eol at (x,y).
  clear-string		; string to cause device to clear entire screen.
  open-line-string	; string to cause device to open a blank line.
  delete-line-string	; string to cause device to delete a line, moving
			; lines below it up.
  insert-init-string	; string to put terminal in insert mode.
  insert-char-init-string ; string to prepare terminal for insert-mode character.
  insert-char-end-string ; string to affect terminal after insert-mode character.
  insert-end-string	; string to take terminal out of insert mode.
  delete-init-string	; string to put terminal in delete mode.
  delete-char-string	; string to delete a character.
  delete-end-string	; string to take terminal out of delete mode.
  init-string		; device init string.
  cm-end-string		; takes device out of cursor motion mode.
  (cm-x-add-char nil)	; char-code to unconditionally add to x coordinate.
  (cm-y-add-char nil)	; char-code to unconditionally add to y coordinate.
  (cm-x-condx-char nil)	; char-code threshold for adding to x coordinate.
  (cm-y-condx-char nil)	; char-code threshold for adding to y coordinate.
  (cm-x-condx-add-char nil) ; char-code to conditionally add to x coordinate.
  (cm-y-condx-add-char nil) ; char-code to conditionally add to y coordinate.
  cm-string1		; initial substring of cursor motion string.
  cm-string2		; substring of cursor motion string between coordinates.
  cm-string3		; substring of cursor motion string after coordinates.
  cm-one-origin		; true if need to add one to coordinates.
  cm-reversep		; true if need to reverse coordinates.
  (cm-x-pad nil)	; nil, 0, 2, or 3 for places to pad.
			; 0 sends digit-chars.
  (cm-y-pad nil)	; nil, 0, 2, or 3 for places to pad.
			; 0 sends digit-chars.
  screen-image		; vector device-lines long of strings
			; device-columns long.
  colors                ; number of colors device can display.
  original-pair-string  ; string to revert to original color pair.
  adjust-fg-string      ; string to adjust foreground color.
  adjust-bg-string      ; string to adjust background color.
  ;;
  ;; This terminal's baud rate, or NIL for infinite.
  (speed nil :type (or (unsigned-byte 24) null)))


;;;; Device screen hunks and window-group.

;;; Window groups are used to keep track of the old width and height of a group
;;; so that when a configure-notify event is sent, we can determine if the size
;;; of the window actually changed or not.
;;;
(defstruct (window-group (:print-function %print-window-group)
			 (:constructor
			  make-window-group (xparent width height)))
  xparent
  width
  height)

(defun %print-window-group (object stream depth)
  (declare (ignore object depth))
  (format stream "#<Editor Window Group>"))

;;; Device-hunks are used to claim a piece of the screen and for ordering
;;; pieces of the screen.  Window motion primitives and splitting/merging
;;; primitives use hunks.  Hunks are somewhat of an interface between the
;;; portable and non-portable parts of screen management, between what the
;;; user sees on the screen and how the editor internals deal with window
;;; sequencing and creation.  Note: the echo area hunk is not hooked into
;;; the ring of other hunks via the next and previous fields.
;;;
(defstruct (device-hunk (:print-function %print-device-hunk))
  "This structure is used internally by the editor's screen management system."
  window		; Window displayed in this hunk.
  position		; Bottom Y position of hunk.
  height		; Height of hunk in pixels or lines.
  next			; Next and previous hunks.
  previous
  device)		; Display device hunk is on.

(defun %print-device-hunk (object stream depth)
  (declare (ignore depth))
  (format stream "#<Editor Device-Hunk ~D+~D~@[, ~S~]>"
	  (device-hunk-position object)
	  (device-hunk-height object)
	  (let* ((window (device-hunk-window object))
		 (buffer (if window (window-buffer window))))
	    (if buffer (buffer-name buffer)))))

;;; Bitmap hunks.
;;;
;;; The lock field is no longer used.  If events could be handled while we
;;; were in the middle of something with the hunk, then this could be set
;;; for exclusion purposes.
;;;
(defstruct (bitmap-hunk #|(:print-function %print-device-hunk)|#
			(:include device-hunk))
  width			      ; Pixel width.
  char-height	      	      ; Height of text body in characters.
  char-width		      ; Width in characters.
  xwindow		      ; X window for this hunk.
  gcontext                    ; X gcontext for xwindow.
  start			      ; Head of dis-line list (no dummy).
  end			      ; Exclusive end, i.e. nil if nil-terminated.
  modeline-dis-line	      ; Dis-line for modeline, or NIL if none.
  modeline-pos		      ; Position of modeline in pixels.
  (lock t)		      ; Something going on, set trashed if we're changed.
  trashed 		      ; Something bad happened, recompute image.
  font-family		      ; Font-family used in this window.
  input-handler		      ; Gets hunk, char, x, y when char read.
  changed-handler	      ; Gets hunk when size changed.
  (thumb-bar-p nil)	      ; True if we draw a thumb bar in the top border.
  window-group)		      ; The window-group to which this hunk belongs.

;;; Terminal hunks.
;;;
(defstruct (tty-hunk #|(:print-function %print-device-hunk)|#
		     (:include device-hunk))
  text-position		; Bottom Y position of text in hunk.
  text-height		; Number of lines of text.
  foreground-color
  background-color)


;;;; Some defsetfs:

(defsetf buffer-writable %set-buffer-writable
  "Sets whether the buffer is writable and invokes the Buffer Writable Hook.")
(defsetf buffer-name %set-buffer-name
  "Sets the name of a specified buffer, invoking the Buffer Name Hook.")
(defsetf buffer-modified %set-buffer-modified
  "Make a buffer modified or unmodified.")
(defsetf buffer-pathname %set-buffer-pathname
  "Sets the pathname of a buffer, invoking the Buffer Pathname Hook.")

(defsetf window-buffer %set-window-buffer
  "Change the buffer a window is mapped to.")

(lisp::define-setf-method value (var)
  "Set the value of an editor variable, calling any hooks."
  (let ((svar (gensym)))
    (values
     ()
     ()
     (list svar)
     `(%set-value ',var ,svar)
     `(value ,var))))

(defsetf variable-value (name &optional (kind :current) where) (new-value)
  "Set the value of an editor variable, calling any hooks."
  `(%set-variable-value ,name ,kind ,where ,new-value))

(defsetf variable-hooks (name &optional (kind :current) where) (new-value)
  "Set the list of hook functions for an editor variable."
  `(%set-variable-hooks ,name ,kind ,where ,new-value))

(defsetf variable-documentation (name &optional (kind :current) where) (new-value)
  "Set a editor variable's documentation."
  `(%set-variable-documentation ,name ,kind ,where ,new-value))

(defsetf variable-source (name &optional (kind :current) where) (new-value)
  "Set a editor variable's source."
  `(%set-variable-source ,name ,kind ,where ,new-value))

(defsetf buffer-minor-mode %set-buffer-minor-mode
  "Turn a buffer minor mode on or off.")
(defsetf buffer-major-mode %set-buffer-major-mode
  "Set a buffer's major mode.")
(defsetf previous-character %set-previous-character
  "Sets the character previous to the given $mark.  Signal an error if
   $mark is before the first character.")
(defsetf next-character %set-next-character
  "Set the character following the given $mark.  Signal an error if mark is
   after the last character.")
(defsetf character-attribute %set-character-attribute
  "Set the value for a character attribute.")
(defsetf character-attribute-hooks %set-character-attribute-hooks
  "Set the hook list for an editor character attribute.")
(defsetf ring-ref %set-ring-ref "Set an element in a ring.")
(defsetf current-window %set-current-window "Set the current window.")
(defsetf current-buffer %set-current-buffer
  "Set the current buffer, FIX doing necessary stuff.")
(defsetf mark-kind %set-mark-kind "Set the kind of mark.")
(defsetf buffer-region %set-buffer-region "Set a buffer's region.")
(defsetf buffer-deep-region %set-buffer-deep-region
  "Set a buffer's deep region.")
(defsetf command-name %set-command-name
  "Change an editor command's name.")
(defsetf line-string %set-line-string
  "Replace the contents of a line.")
(defsetf last-command-type %set-last-command-type
  "Set the Last-Command-Type for use by the next command.")
(defsetf prefix-argument %set-prefix-argument
  "Set the prefix argument for the next command.")
(defsetf logical-key-event-p %set-logical-key-event-p
  "Change what Logical-Char= returns for the specified arguments.")
(defsetf window-font %set-window-font
  "Change the font-object associated with a font-number in a window.")
(defsetf default-font %set-default-font
  "Change the font-object associated with a font-number in new windows.")

(defsetf buffer-modeline-fields %set-buffer-modeline-fields
  "Sets the buffer's list of modeline fields causing all windows into buffer
   to be updated for the next redisplay.")
(defsetf modeline-field-name %set-modeline-field-name
  "Sets a modeline-field's name.  If one already exists with that name, an
   error is signaled.")
(defsetf modeline-field-width %set-modeline-field-width
  "Sets a modeline-field's width and updates all the fields for all windows
   in any buffer whose fields list contains the field.")
(defsetf modeline-field-function %set-modeline-field-function
  "Sets a modeline-field's function and updates this field for all windows in
   any buffer whose fields list contains the field.")
