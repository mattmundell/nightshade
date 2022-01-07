;;; Roff mode.

(in-package "ED")

(defmode "Roff" :major-p t)

(defcommand "Roff Mode" ()
  "Put the current buffer into Roff mode."
  (setf (buffer-major-mode (current-buffer)) "Roff"))

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :value #'generic-indent
  :mode "Roff")

(defevar "Auto Fill Space Indent"
  "When true, uses `Indent New Comment Line' to break lines instead of `New
   Line'."
  :mode "Roff" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Roff" :value ".\\\"")

(defevar "Comment End"
  "String that ends comments.  () indicates #\newline termination."
  :mode "Roff")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Roff" :value ".\\\" ")

(defevar "Highlight Comments"
  "If true single-line comments will be highlighted."
  :mode "Roff" :value t)

;(shadow-attribute :scribe-syntax #\< nil "Roff")
