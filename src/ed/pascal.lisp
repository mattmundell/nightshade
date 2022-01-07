;;; A minimal Pascal mode.

(in-package "ED")

(defmode "Pascal" :major-p t)

(defcommand "Pascal Mode" ()
  "Put the current buffer into \"Pascal\" mode."
  (setf (buffer-major-mode (current-buffer)) "Pascal"))

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :value #'generic-indent
  :mode "Pascal")

(defevar "Auto Fill Space Indent"
  "When true, use `Indent New Comment Line' to break lines instead of `New
   Line'."
  :mode "Pascal" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Pascal" :value "(*")

(defevar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Pascal" :value " *)")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Pascal" :value "(* ")

(shadow-attribute :scribe-syntax #\< nil "Pascal")
