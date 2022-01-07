;;; -*- Log: hemlock.log; Package: Hemlock; Mode: Editor -*-
;;;
;;; HTML parser.

(in-package "HEMLOCK")

(eval-when (compile eval load)

  (defparser
      `((:html       "<html>" :white
		     (or :html-head :epsilon) :white
		     (or :html-body :epsilon) :white
		     "</html>" :white)

	(:html-head  "<head>"  :white
		     (or :title :epsilon)
		     "</head>")

	(:title      "<title>" :white
		     "</title>")

	(:html-body  "<body>" :white "</body>")

	(:white      (or (group (many (or #\  #\tab #\newline))) :epsilon))

	(:epsilon    "")
	))
  
) ; eval-when
