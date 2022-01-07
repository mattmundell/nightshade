;;; One line description of german-tutor.

(defpackage "GERMAN-TUTOR"
  (:version 0)
  (:use "LISP" "EXT")
  (:documentation "FIX."))

(in-package "ED")

;/home/mattm/.kde/share/apps/klettres/de/
(defevar "Tutor Sound Dir" #p"/usr/share/apps/klettres/data/")

(defun phrase-sound (language category phrase)
  (merge-pathnames
   (format () "~A/~A/~A/"
			   (namify (or (value tutor-sound-dir)
				       "library:packages/german-tutor/data/"))
			   language
			   category)
   (merge-pathnames phrase "*.ogg")))

(defun tutor-letters (language letters)
  (loop
    (let ((char (nth (random (length letters)) letters)))
      (until* ((sound (phrase-sound language "alpha" (string char)))
	       (key (progn
		      (view sound)
		      (prompt-for-key :prompt `("~C: " ,char)))
		    (prompt-for-key :prompt `("~C: " ,char)))
	       (key-char (if key (key-event-char (aref key 0)))
			 (if key (key-event-char (aref key 0)))))
	      ((and key-char
		    (string= (string-upcase (string key-char))
			     (string-upcase (string char)))))
	(view sound)
	;(message "~C" (key-event-char (aref key 0)))
	;(sleep 0.5)
	))))

(defvar *english-alphabet*
  '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M))

(defcommand "English Alphabet 1" ()
  "Tutor English Alphabet, showing characters."
  (tutor-letters "en" *english-alphabet*))

(defcommand "German Alphabet 1" ()
  "Tutor German Alphabet, showing characters."
  (tutor-letters "en" *english-alphabet*))



/home/mattm/down/lingoteach-en-de

(while ((count 1 (1+ count)))
       ((< count 10000))
  (let ((pn (format ()
		    "/usr/share/lingoteach/data/german/robos/basic/~A.ogg"
		    count)))
    (format t "~A~%" pn)
    (when (probe-file pn)
      (view pn)
      (sleep 2))))
