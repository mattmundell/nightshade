;;; Minimal dylan mode.

(in-package "ED")


;;; hack ..

(setf (getstring "dylan" *mode-names*) nil)


(defmode "Dylan" :major-p t)
(defcommand "Dylan Mode" ()
  "Put the current buffer into \"Dylan\" mode."
  (setf (buffer-major-mode (current-buffer)) "Dylan"))

(define-file-type-hook ("dylan") (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Dylan"))

(defevar "Indent Function"
  "A function called by indent to do the indentation.  Passed a mark as its
   argument.  The function should indent the line that the mark points to.
   The function may move the mark around on the line.  The mark is
   :left-inserting."
  :value #'generic-indent
  :mode "Dylan")

(defevar "Auto Fill Space Indent"
  "When true, use `Indent New Comment Line' to break lines instead of `New
   Line'."
  :mode "Dylan" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Dylan" :value "//")

(defevar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Dylan")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Dylan" :value "// ")

(bind-key "Delete Previous Character Expanding Tabs" #k"backspace"
	  :mode "Dylan")
(bind-key "Delete Previous Character Expanding Tabs" #k"delete" :mode "Dylan")

;;; hacks...

(shadow-attribute :scribe-syntax #\< nil "Dylan")
(shadow-attribute :scribe-syntax #\> nil "Dylan")
(bind-key "Self Insert" #k"\>" :mode "Dylan")
(bind-key "Scribe Insert Bracket" #k")" :mode "Dylan")
(bind-key "Scribe Insert Bracket" #k"]" :mode "Dylan")
(bind-key "Scribe Insert Bracket" #k"}" :mode "Dylan")
