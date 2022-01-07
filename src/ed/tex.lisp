;;; Tex mode.

(in-package "ED")

(defmode "Tex" :major-p t)

(defcommand "Tex Mode" ()
  "Put the current buffer into Tex mode."
  (setf (buffer-major-mode (current-buffer)) "Tex"))

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :value #'generic-indent
  :mode "Tex")

(defevar "Auto Fill Space Indent"
  "When true, uses `Indent New Comment Line' to break lines instead of `New
   Line'."
  :mode "Tex" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Tex" :value "%")

(defevar "Comment End"
  "String that ends comments.  () indicates #\newline termination."
  :mode "Tex")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Tex" :value "% ")

(defevar "Highlight Comments"
  "If true single-line comments will be highlighted."
  :mode "Tex" :value t)

;(shadow-attribute :scribe-syntax #\< nil "Tex")
