;;; Tests of mh:draft-resend.     -*- Flush-Trailing-Whitespace: nil -*-

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar *draft-resend-mail-1* "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar *draft-resend-mail-2* "To: test2@two.org.za
From: short@name.org
Subject: subject
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")

(defvar *draft-resend-mail-3* "To: Three@three.org
Cc: people@CC.org
From: short@Name.org
Subject: tEst 3
Date: 16 Apr 2008 16:02:10 +0100
Arb: xXx

Body body body.

More body.

Ending.
")


;;;; Tests.

(deftest draft-resend ('(23
		      "Resent-To: 
Resent-Cc: 
")
		    draft-resend-1)
  "Test `draft-resend' with existing drafts."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: folder~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-resend-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *draft-resend-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-resend-mail-1* stream))
      (mh:with-fresh-state
       (list (mh:draft-resend)
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '(23))))))))

(deftest draft-resend ('(1
		      "Resent-To: 
Resent-Cc: 
")
		    draft-resend-2)
  "Test `draft-resend' with an empty draft directory."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: folder~%"))
      (mh:with-fresh-state
       (list (mh:draft-resend)
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '(1))))))))


;;;; Errors.

(deftest draft-resend (t draft-resend-100)
  "Test `draft-resend' with a missing draft-folder component."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:draft-resend)
	   (error () (setq ret t))))))))
