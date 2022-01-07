;;; Commands and support specifically for X related features.

(in-package "ED")


#[ Cut Buffer Commands

These commands allow the X cut buffer to be used from the editor.

{command:Region to Cut Buffer}
{command:Insert Cut Buffer}
]#

(defcommand "Region to Cut Buffer" ()
  "Place the current region into the X cut buffer."
  (store-cut-string (edi::bitmap-device-display
		     (edi::device-hunk-device (edi::window-hunk (current-window))))
		    (region-to-string (current-region))))

(defcommand "Insert Cut Buffer" ()
  "Insert the X cut buffer at the current point."
  "Insert the X cut buffer at the current point.  Return () when the cut
   buffer is empty."
  (let ((str (fetch-cut-string (edi::bitmap-device-display
				(edi::device-hunk-device
				 (edi::window-hunk (current-window)))))))
    (if str
	(let ((point (current-point)))
	  (push-buffer-mark (copy-mark point))
	  (insert-string (current-point) str))
	(editor-error "X cut buffer empty.")))
  (setf (last-command-type) :ephemerally-active))


#[ Selection Commands

These commands provide access to the X primary selection and the clipboard.

{command:Insert Selection}

{command:Copy}
{command:Paste}
{command:Cut}
]#

(defcommand "Insert Selection" ()
  "Insert the X primary selection at the current point."
  (let ((str (fetch-selection)))
    (if str
	(let ((point (current-point)))
	  (push-buffer-mark (copy-mark point))
	  (insert-string (current-point) str))
	(editor-error "X selection is empty.")))
  (setf (last-command-type) :ephemerally-active))

(defcommand "Paste" ()
  "Insert the X clipboard at the current point."
  (let ((str (fetch-clipboard)))
    (if str
	(let ((point (current-point)))
	  (push-buffer-mark (copy-mark point))
	  (insert-string (current-point) str))
	(editor-error "X clipboard is empty.")))
  (setf (last-command-type) :ephemerally-active))

(defcommand "Copy" ()
  "Call `Save Region', which saves the current region to the Kill Ring and
   the X clipboard."
  (save-region-command))

(defcommand "Cut" ()
  "Call `Kill Region', which moves the current region to the Kill Ring and
   the X clipboard."
  (kill-region-command))


;;;; Tracing.

(defcommand "Toggle Event Printing" ()
  "Toggle printing of X events."
  (setq ext::*object-set-event-handler-print*
	(fi ext::*object-set-event-handler-print*)))


;;;; Windows.

(defvar *xbrowse-buffer* ())

(defmode "XBrowse" :major-p t)

(defun refresh-xbrowse-buffer (buf windows)
  "Refresh bufed buffer $buf, which lists the list of $buffers using array
   $bufed-buffer.  Assume that *Xbrowse Buffer End* is already set."
  (setf (buffer-writable buf) t)
  (delete-region (buffer-region buf))
  (with-mark ((mark (buffer-point buf)))
    (dolist (window windows)
      (let ((name (xlib:window-name window))
	    ;(wm-name (xlib:window-wm-name window))
	    )
	(ed::msg "name ~A" name)
	;(ed::msg "wm-name ~A" name)
	(ed::msg "window ~A" window)
	(when name
	  (insert-string mark name)
	  (setf (getf (line-plist (mark-line mark)) 'window) window)))
      (insert-character mark #\newline)))
  (setf (buffer-writable buf) ())
  (setf (buffer-modified buf) ())
  (buffer-start (buffer-point buf)))

(defun make-xbrowse-buffer (name windows &optional buffer)
  "Make an X windows browse buffer listing $windows."
  (let* ((buf (or buffer
		  (make-buffer name :modes '("XBrowse")))))
    (refresh-xbrowse-buffer buf windows)
    buf))

(defcommand "Browse X Windows" ()
  "Browse a list of X windows."
  (let* ((hunk (edi::window-hunk (current-window)))
	 (display (edi::bitmap-device-display
		   (edi::device-hunk-device hunk)))
	 (root (xlib:screen-root
		(xlib:display-default-screen display)))
	 (windows (xlib:query-tree root)))
    (change-to-buffer
     (or *xbrowse-buffer*
	 (setq *xbrowse-buffer*
	       (make-xbrowse-buffer "Browse X Windows" windows))))))


#|
(in-package "EDI")

;; FIX move into ed

;;; todd kaufmann     24 mar 88

;;; because we will have lots of windows, with users moving between them lots,
;;; we would like the window the mouse is in to be the one input goes to.
;;;
;;; also, this is more in line with the way most X windows work
;;;   (unless you like to use focus)

;;; Modified by Dave Touretzky 5/23/88 to fix incremental search problem,
;;; to change windows upon leaving the echo area if the mouse moves
;;; while we're prompting for input, and to select the correct window
;;; when we split a window, based on where the mouse ends up.

;;; FIX
;;;  - a few more prompt-for-* functions need to be modified.  they're
;;;    flagged below.

(defevar "Follow Mouse to Window"
  "When true, set the current window to the one the mouse enters; otherwise
   FIX behave the old way"
  :value t)

(defevar "Follow Mouse To Read-only Buffers"
  "When true follow the mouse to read-only buffers."
  :value t)

(defvar *last-window-mouse-entered* ())  ;set to the most recent LEGAL window entered

(defun change-to-current-entered-window (window)
  "Make the buffer the mouse has moved to be the current buffer."
  (let ((buffer (window-buffer window)))
    (and *in-the-editor*
	 (value ed::follow-mouse-to-window)
	 ;; don't move cursor to menus
	 (not (string= (buffer-major-mode buffer) "HMenu"))
	 ;; don't move TO echo area
	 (not (eq buffer *echo-area-buffer*))
	 ;; can move to read-only buffers if user says it's okay
	 (if (buffer-writable buffer) t
	     (value ed::follow-mouse-to-read-only-buffers))
	 ;; if we get here, this is a valid window to move to:  remember it
	 (setf *last-window-mouse-entered* window)
	 ;; but don't move OUT of the echo area if it's currently prompting.
	 (not (eq (current-window) *echo-area-window*))
	 ;; and don't move out of current window during an incremental search
	 (not *during-incremental-search*)
	 ;; Make sure it's not a random typeout buffer.
	 (member window *window-list*)
	 (setf (current-window) window
	       (current-buffer) buffer
	       *last-window-mouse-entered* ()))))

(add-hook ed::enter-window-hook 'change-to-current-entered-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; If the user is being prompted in the echo area and moves the
;;; mouse, we can't switch windows.  So remember where the mouse
;;; has gone to, and switch when he leaves the echo area buffer.

(defun change-windows-if-mouse-moved ()
  (and *last-window-mouse-entered*
       (or (eq (current-window) *last-window-mouse-entered*)
	   (change-to-current-entered-window *last-window-mouse-entered*))))

;from echo.lisp

(defun parse-for-something ()
  (display-prompt-nicely)
  (let ((start-window (current-window)))
    (move-mark *parse-starting-mark* (buffer-point *echo-area-buffer*))
    (setf (current-window) *echo-area-window*)
    (unwind-protect
	(use-buffer *echo-area-buffer*
		    (recursive-edit nil))
      (setf (current-window) start-window)
      (change-windows-if-mouse-moved))))  ; <--- here's the change

;---> Note:  must make the same change to the following:
;	prompt-for-y-or-n
;	prompt-for-character*
;	prompt-for-key

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fix for the problem of switching buffers in the middle of an
;;; incremental search.
;;;
;;; Modify %i-search from searchcoms.lisp to bind a global variable
;;; hi::*during-incremental-search* to T.  Check for this in body of
;;; change-to-current-entered-window above.

(defvar *during-incremental-search* nil)

(in-package "ED")

(defun %i-search (string point trailer direction failure)
  (unwind-protect			           ;<---- here's the change
   (do* ((hi::*during-incremental-search* t)       ;<---- here's the change
	 (curr-point (copy-mark point :temporary))
	 (curr-trailer (copy-mark trailer :temporary))
	 (next-char (get-key-event *editor-input* t)
		    (get-key-event *editor-input* t)))
	(nil)
     (case (%i-search-char-eval next-char string point trailer direction failure)
       (:cancel
	(%i-search-echo-refresh string direction failure)
	(or (zerop (length string))
	    (i-search-pattern string direction)))
       (:return-cancel
	(or (zerop (length string)) (return :cancel))
	(beep))
       (:control-g
	(if failure (return :control-g))
	(%i-search-echo-refresh string direction nil)
	(or (zerop (length string))
	    (i-search-pattern string direction))))
     (move-mark point curr-point)
     (move-mark trailer curr-trailer))
   (hi::change-windows-if-mouse-moved)))	   ;<---- here's the change

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Fix Split Window to not change the current window.
;;; That way, if the mouse is still in the current window,
;;; everything's fine.  If the mouse ended up in the new
;;; window, a mouse-entered-window event will handle the
;;; change to the new window.

; From filecoms.lisp:

(defcommand "Split Window" ()
  "Make a new window by splitting the current window.
   The new window is made the current window and displays starting at
   the same place as the current window."
  "Create a new window which displays starting at the same place
   as the current window."
  (let ((new (make-window (window-display-start (current-window)))))
    (or new (editor-error "Could not make a new window."))
;    (setf (current-window) new)     <--- commented out this line
 ))
|#
