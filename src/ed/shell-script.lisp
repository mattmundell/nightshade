;;; Shell script mode.

(in-package "ED")

(defmode "Shell" :major-p t)

(defcommand "Shell Mode" (p)
  "Put the current buffer into Shell script mode."
  "Put the current buffer into Shell script mode."
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "Shell"))

(defhvar "Indent Function"
  "Indentation function which is invoked by \"Indent\" command.
   It must take one argument that is the prefix argument."
  :value #'generic-indent
  :mode "Shell")

(defhvar "Auto Fill Space Indent"
  "When non-nil, uses \"Indent New Comment Line\" to break lines instead of
   \"New Line\"."
  :mode "Shell" :value t)

(defhvar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Shell" :value "#")

(defhvar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Shell" :value nil)

(defhvar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Shell" :value "# ")

(defhvar "Highlight Comments"
  "If true single-line comments will be highlighted."
  :mode "Shell" :value t)

;(shadow-attribute :scribe-syntax #\< nil "Shell")
