;;; M4 mode.

(in-package "HEMLOCK")

(defmode "M4" :major-p t)

(defcommand "M4 Mode" (p)
  "Put the current buffer into M4 mode."
  "Put the current buffer into M4 mode."
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "M4"))

(defhvar "Indent Function"
  "Indentation function which is invoked by \"Indent\" command.
   It must take one argument that is the prefix argument."
  :value #'generic-indent
  :mode "M4")

(defhvar "Auto Fill Space Indent"
  "When non-nil, uses \"Indent New Comment Line\" to break lines instead of
   \"New Line\"."
  :mode "M4" :value t)

(defhvar "Comment Start"
  "String that indicates the start of a comment."
  :mode "M4" :value "dnl")

(defhvar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "M4" :value nil)

(defhvar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "M4" :value "dnl ")

(defhvar "Highlight Comments"
  "If true single-line comments will be highlighted."
  :mode "M4" :value t)

;(shadow-attribute :scribe-syntax #\< nil "M4")
