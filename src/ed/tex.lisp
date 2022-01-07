;;; Tex mode.

(in-package "HEMLOCK")

(defmode "Tex" :major-p t)

(defcommand "Tex Mode" (p)
  "Put the current buffer into Tex mode."
  "Put the current buffer into Tex mode."
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Tex"))

(defhvar "Indent Function"
  "Indentation function which is invoked by \"Indent\" command.
   It must take one argument that is the prefix argument."
  :value #'generic-indent
  :mode "Tex")

(defhvar "Auto Fill Space Indent"
  "When non-nil, uses \"Indent New Comment Line\" to break lines instead of
   \"New Line\"."
  :mode "Tex" :value t)

(defhvar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Tex" :value "%")

(defhvar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Tex" :value nil)

(defhvar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Tex" :value "% ")

(defhvar "Highlight Comments"
  "If true single-line comments will be highlighted."
  :mode "Tex" :value t)

;(shadow-attribute :scribe-syntax #\< nil "Tex")
