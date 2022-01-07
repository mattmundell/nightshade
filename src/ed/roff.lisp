;;; Roff mode.

(in-package "ED")

(defmode "Roff" :major-p t)

(defcommand "Roff Mode" (p)
  "Put the current buffer into Roff mode."
  "Put the current buffer into Roff mode."
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Roff"))

(defhvar "Indent Function"
  "Indentation function which is invoked by \"Indent\" command.
   It must take one argument that is the prefix argument."
  :value #'generic-indent
  :mode "Roff")

(defhvar "Auto Fill Space Indent"
  "When non-nil, uses \"Indent New Comment Line\" to break lines instead of
   \"New Line\"."
  :mode "Roff" :value t)

(defhvar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Roff" :value ".\\\"")

(defhvar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Roff" :value nil)

(defhvar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Roff" :value ".\\\" ")

(defhvar "Highlight Comments"
  "If true single-line comments will be highlighted."
  :mode "Roff" :value t)

;(shadow-attribute :scribe-syntax #\< nil "Roff")
