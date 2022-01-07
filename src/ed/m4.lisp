;;; M4 mode.

(in-package "ED")

(defmode "M4" :major-p t)

(defcommand "M4 Mode" ()
  "Put the current buffer into M4 mode."
  (setf (buffer-major-mode (current-buffer)) "M4"))

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :value #'generic-indent
  :mode "M4")

(defevar "Auto Fill Space Indent"
  "When true, uses `Indent New Comment Line' to break lines instead of `New
   Line'."
  :mode "M4" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "M4" :value "dnl")

(defevar "Comment End"
  "String that ends comments.  () indicates #\newline termination."
  :mode "M4")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "M4" :value "dnl ")

(defevar "Highlight Comments"
  "If true single-line comments will be highlighted."
  :mode "M4" :value t)

;(shadow-attribute :scribe-syntax #\< nil "M4")
