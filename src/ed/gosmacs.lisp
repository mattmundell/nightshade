;;; Some degree of upward compatibility for Gosling Emacs users.

(in-package "ED")


(defcommand "Gosmacs Permute Characters" (p)
  "Transpose the two characters before the point."
  "Transpose the two characters before the point."
  (declare (ignore p))
  (with-mark ((m (current-point) :left-inserting))
    (unless (and (mark-before m) (previous-character m))
      (editor-error "NIB     You have addressed a character not in the buffer?"))
    (rotatef (previous-character m) (next-character m))))

(bind-key "Gosmacs Permute Characters" #k"control-t")
(bind-key "Kill Previous Word" #k"meta-h")
(bind-key "Replace String" #k"meta-r")
(bind-key "Query Replace" #k"meta-q")
(bind-key "Fill Paragraph" #k"meta-j")
(bind-key "Visit File" #k"control-x control-r")
(bind-key "Find File" #k"control-x control-v")
(bind-key "Insert File" #k"control-x control-i")
