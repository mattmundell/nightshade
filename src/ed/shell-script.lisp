;;; Shell script mode.

(in-package "ED")

(defvar shell-special-forms (make-hash-table :size 50 :test #'equal)
  "A hashtable of Shell special form names.")

(setf (gethash "function" shell-special-forms) t)

(setf (gethash "if" shell-special-forms) t)
(setf (gethash "then" shell-special-forms) t)
(setf (gethash "elif" shell-special-forms) t)
(setf (gethash "else" shell-special-forms) t)
(setf (gethash "fi" shell-special-forms) t)
(setf (gethash "case" shell-special-forms) t)
(setf (gethash "in" shell-special-forms) t)
(setf (gethash "esac" shell-special-forms) t)
;(setf (gethash "default" shell-special-forms) t)

(setf (gethash "while" shell-special-forms) t)
(setf (gethash "for" shell-special-forms) t)
(setf (gethash "done" shell-special-forms) t)
;(setf (gethash "do" shell-special-forms) t)

;(setf (gethash "return" shell-special-forms) t)
;(setf (gethash "goto" shell-special-forms) t)
;(setf (gethash "exit" shell-special-forms) t)

;(setf (gethash "cast" shell-special-forms) t)
;(setf (gethash "typedef" shell-special-forms) t)

(defun setup-shell-mode (buffer)
  (if (editor-bound-p 'shell-special-forms)
      (setv shell-special-forms shell-special-forms)
      (defevar "Shell Special Forms"
	"Hashtable of Shell special forms."
	:buffer buffer
	:value shell-special-forms))
  (highlight-visible-shell-buffer buffer)
  (pushnew '("Shell" t highlight-visible-shell-buffer) *mode-highlighters*))

(defmode "Shell" :major-p t
  :setup-function 'setup-shell-mode)

(defevar "Indent Function"
  "Indentation function invoked by the `Indent' command.  The function
   takes a :left-inserting mark that may be moved, and indents the line
   that the mark is on."
  :value #'generic-indent
  :mode "Shell")

(defevar "Auto Fill Space Indent"
  "When true, use `Indent New Comment Line' to break lines instead of `New
   Line'."
  :mode "Shell" :value t)

(defevar "Comment Start"
  "String that indicates the start of a comment."
  :mode "Shell" :value "#")

(defevar "Comment End"
  "String that ends comments.  Nil indicates #\newline termination."
  :mode "Shell")

(defevar "Comment Begin"
  "String that is inserted to begin a comment."
  :mode "Shell" :value "# ")

(defevar "Highlight Comments"
  "If true single-line comments will be highlighted."
  :mode "Shell" :value t)

;(shadow-attribute :scribe-syntax #\< nil "Shell")

(defun highlight-shell-buffer (buffer)
  (elet ((c-special-forms shell-special-forms))
    (highlight-chi-buffer buffer highlight-c-line)))

(defun highlight-visible-shell-buffer (buffer)
  (elet ((c-special-forms shell-special-forms))
    (highlight-visible-chi-buffer buffer highlight-c-line)))
