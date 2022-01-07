(in-package "ED")

#[ Page Commands

Another unit of text recognized by the editor is the page.  A page is a piece
of text delimited by formfeeds (^L's.)  The first non-blank line after the
page marker is the page title.  The page commands are quite useful when
logically distinct parts of a file are put on separate pages.  See also
comrefCount Lines Page.  These commands only recognize ^L's at the
beginning of a lines, so those quoted in string literals do not get in the way.

{command:Previous Page}
{command:Next Page}
{command:Mark Page}
{command:Goto Next Page}
{command:View Page Directory}
{command:Insert Page Directory}
]#

(defun setup-page-mode (buffer)
  (defevar "Page Buffer"
    "The buffer associated with current page directory."
    :value buffer))

(defmode "Page" :major-p t
  :setup-function #'setup-page-mode
  :documentation
  "Mode for browsing the pages in a buffer.")

(defcommand "Select Page" ()
  "Select the page under point."
  (let ((mark (copy-mark (current-point))))
    (line-start mark)
    (with-input-from-region (stream (region mark (buffer-end-mark (current-buffer))))
      (change-to-buffer (value page-buffer))
      (goto-next-page-command (read stream)))))

(defcommand "Goto Next Page" (p)
  "Go to the next page.

   With a positive argument go to an absolute page number, moving that many
   pages from the beginning of the file.

   With a zero argument prompt for string and go to the page with that
   string in its title.  Repeated invocations in this manner continue
   searching from the point of the last find, and a first search with a
   particular pattern pushes a buffer mark.

   With a negative argument move backward by that many pages, if possible."
  (let ((point (current-point)))
    (cond ((fi p)
	   (page-offset point 1)
	   (setf (last-command-type) :next-page))
	  ((zerop p)
	   (let* ((againp (eq (last-command-type) :goto-page-zero))
		  (name (prompt-for-string :prompt "Substring of page title: "
					   :default (if againp
							*goto-page-last-string*
							*parse-default*)))
		  (dir (page-directory (current-buffer)))
		  (i 1))
	     (declare (simple-string name))
	     (cond ((not againp)
		    (push-buffer-mark (copy-mark point)))
		   ((string-equal name *goto-page-last-string*)
		    (setf dir (nthcdr *goto-page-last-num* dir))
		    (setf i (1+ *goto-page-last-num*))))
	     (loop
	       (or dir
		   (editor-error "Failed to find a page title containing ~S."
				 name))
	       (when (search name (the simple-string (car dir))
			     :test #'char-equal)
		 (goto-page point i)
		 (setf (last-command-type) :goto-page-zero)
		 (setf *goto-page-last-num* i)
		 (setf *goto-page-last-string* name)
		 (return t))
	       (incf i)
	       (setf dir (cdr dir))))
	   (setf (last-command-type) :next-page))
	  ((minusp p)
	   (page-offset point p)
	   (setf (last-command-type) :previous-page))
	  (t (goto-page point p)
	     (setf (last-command-type) :next-page)))
    (line-start (move-mark (window-display-start (current-window)) point))))

(defcommand "Goto Page" ()
  "Prompt for a page number and goto that page.

   Prompt for string and go to the page with that string in its title.
   Repeated invocations in this manner continue searching from the point of
   the last find, and a first search with a particular pattern pushes a
   buffer mark."
  (goto-next-page-command 0))

(defcommand "View Page Directory" ()
  "Pop up a listing of the number and title of each page in the current
   buffer."
  (let ((dir (page-directory (current-buffer))))
    (declare (list dir))
    (with-pop-up-display (s :height (1+ (the fixnum (length dir))))
      (display-page-directory s dir))))

(defcommand "Browse Page Directory" ()
  "Pop up a window listing the number and title of each page in the current
   buffer."
  (let ((dir (page-directory (current-buffer))))
    (declare (list dir))
    (with-pop-up-window (buffer window
				:buffer-name (format () "Page Directory: ~A"
						     (buffer-name (current-buffer)))
				:modes '("Page"))
      (with-output-to-mark (s (buffer-mark buffer))
	(display-page-directory s dir))
      (buffer-start (buffer-point buffer))
      (setf (buffer-modified buffer) ())
      (setf (buffer-writable buffer) ())
      (recursive-edit))))

(defcommand "Insert Page Directory" (p)
  "Insert at the beginning of the buffer a listing of the number and title
   of each page in the current buffer.  Push a mark before going to the
   beginning of the buffer.  If an argument is supplied, insert the page
   directory at point."
  (let ((point (current-point)))
    (unless p
      (push-buffer-mark (copy-mark point))
      (buffer-start point))
    (push-buffer-mark (copy-mark point))
    (display-page-directory (make-editor-output-stream point :full)
			    (page-directory (current-buffer))))
  (setf (last-command-type) :ephemerally-active))

(defcommand "Previous Page" (p)
  "Move point to the beginning of the current page.  With a prefix argument
   move backward that many pages."
  "Move backward $p pages."
  (let ((point (current-point)))
    (or (page-offset point (- (or p 1)))
	(editor-error "No such page."))
    (line-start (move-mark (window-display-start (current-window)) point))
    (setf (last-command-type) :previous-page)))

(defcommand "Next Page" (p)
  "Move to the beginning of the next page.  With prefix argument move that
   many pages forward."
  "Move forward $p pages."
  (let ((point (current-point)))
    (or (page-offset point (or p 1))
	(editor-error "No such page."))
    (line-start (move-mark (window-display-start (current-window)) point))
    (setf (last-command-type) :next-page)))

(defcommand "Self Insert or Previous Page" (p)
  "Invoke Previous Page if the last command was a page movement command,
   else invoke Self Insert."
  (if (or (eq (last-command-type) :next-page)
	  (eq (last-command-type) :previous-page))
      (previous-page-command p)
      (self-insert-command p)))

(defcommand "Self Insert or Next Page" (p)
  "Invoke Next Page if the last command was a page movement command, else
   invoke Self Insert."
  (if (or (eq (last-command-type) :next-page)
	  (eq (last-command-type) :previous-page))
      (next-page-command p)
      (self-insert-command p)))

(defcommand "Mark Page" (p)
  "Put point at beginning of the current page and the mark at end of
   current page.  With a prefix argument, mark the page that many pages
   after the current one."
  "Mark the p'th page after the current one, putting the point at the
   beginning of the current one."
  (let ((point (current-point)))
    (if p
	(or (page-offset point (1+ p)) (editor-error "No such page."))
	(page-offset point 1)) ;If this loses, we're at buffer-end.
    (with-mark ((m point))
      (or (page-offset point -1)
	  (editor-error "No such page."))
      (push-buffer-mark (copy-mark m) t)
      (line-start (move-mark (window-display-start (current-window)) point)))))

