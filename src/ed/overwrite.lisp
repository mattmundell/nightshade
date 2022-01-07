;;; Overwrite mode.

(in-package "ED")

#[ Overwrite Mode

`Overwrite' mode is a minor mode which is useful for creating figures and
tables out of text.  In this mode, typing a key-event with a corresponding
graphic character replaces the character at the point instead of inserting the
character.  `Quoted Insert' can be used to insert characters normally.

{mode:Overwrite}
{command:Self Overwrite}
{command:Overwrite Delete Previous Character}
]#

(defmode "Overwrite"
  :documentation
  "Mode in which printing characters overwrite characters instead of
   pushing them to the right.  `Quoted Insert' inserts characters
   normally.")

(defcommand "Self Overwrite" (p)
  "Replace the next character with the last character typed, then move past
   the inserted character.  With prefix argument, do it that many times.
   If the next character is a tab, first expands the tab into the
   appropriate number of spaces, replacing just the next space character.
   At the end of the line, insert the character and keep the newline."
  (let ((char (ext:key-event-char *last-key-event-typed*))
	(point (current-point)))
    (unless char (editor-error "Can't insert that character."))
    (do ((n (or p 1) (1- n)))
	((zerop n))
      (case (next-character point)
	(#\tab
	 (let ((col1 (mark-column point))
	       (col2 (mark-column (mark-after point))))
	   (if (= (- col2 col1) 1)
	       (setf (previous-character point) char)
	       (insert-character (mark-before point) char))))
	((#\newline nil) (insert-character point char))
	(t (setf (next-character point) char)
	   (mark-after point))))))

(defcommand "Overwrite Delete Previous Character" (p)
  "Replace the previous character with a space and move backwards.  Delete
   tabs and newlines.  With a prefix argument, do it that many times."
  (do ((point (current-point))
       (n (or p 1) (1- n)))
      ((zerop n))
    (case (previous-character point)
      ((#\newline #\tab) (delete-characters point -1))
      ((nil) (editor-error "Beginning of buffer."))
      (t (setf (previous-character point) #\space)
	 (mark-before point)))))
