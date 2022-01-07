;;; Tests of mh:draft-forward.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar *draft-forward-mail-1* "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar *draft-forward-mail-2* "To: test2@two.org.za
From: short@name.org
Subject: subject
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")

(defvar *draft-forward-mail-3* "To: Three@three.org
Cc: people@CC.org
From: short@Name.org
Subject: tEst 3
Date: 16 Apr 2008 16:02:10 +0100
Arb: xXx

Body body body.

More body.

Ending.
")

(defvar *draft-forward-mail-4* "To: Three@three.org
Cc: people@CC.org
From: short@Name.org
Date: 16 Apr 2008 16:02:10 +0100
Arb: xXx

Body body body.

More body.

Ending.
")


;; Directory with exising drafts.

(deftest draft-forward (t draft-forward-1)
  "Test `draft-forward' with the automatic components."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-3* stream))
      (to-file (stream "home:Mail/drafts/12"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-2* stream))
      (mh:with-fresh-state
       (and (eq (mh:draft-forward "folder" 22) 13)
	    (string= (format () "To:
Cc:
Subject:
--------
#:message/rfc822 [Forward: tEst 3] ~A/Mail/folder/22
"
			     (namify folder))
		     (with-output-to-string (stream)
		       (mh:write-messages stream "drafts"
					  '(13))))
	    (equal (mh:sequence-list "drafts" "highest" t)
		   '((13 . 13)))
	    (equal (mh:sequence-list "folder" "highest" t)
		   '((22 . 22))))))))

(deftest draft-forward (t draft-forward-2)
  "Test `draft-forward' with components from a file."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/" "Mail/forwcomps")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/forwcomps")
	(write-string "To:
From: forwarder
Subject: [Forward: ]
--------
"
		      stream))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-3* stream))
      (to-file (stream "home:Mail/drafts/12"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-2* stream))
      (mh:with-fresh-state
       (and (eq (mh:draft-forward "+folder" 22) 13)
	    (string= (format () "To:
From: forwarder
Subject: [Forward: ]
--------
#:message/rfc822 [Forward: tEst 3] ~A/Mail/folder/22
"
			     (namify folder))
		     (with-output-to-string (stream)
		       (mh:write-messages stream "drafts" '(13))))
	    (equal (mh:sequence-list "drafts" "highest" t)
		   '((13 . 13)))
	    (equal (mh:sequence-list "folder" "highest" t)
		   '((22 . 22))))))))

(deftest draft-forward (t draft-forward-3)
  "Test `draft-forward', where the forwarded message is missing a subject
   header."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/" "Mail/forwcomps")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/forwcomps")
	(write-string "To:
From: forwarder
Subject: [Forward: ]
--------
"
		      stream))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-4* stream))
      (to-file (stream "home:Mail/drafts/12"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-2* stream))
      (mh:with-fresh-state
       (and (eq (mh:draft-forward "+folder" 22) 13)
	    (string= (format () "To:
From: forwarder
Subject: [Forward: ]
--------
#:message/rfc822 [Forward: ] ~A/Mail/folder/22
"
			     (namify folder))
		     (with-output-to-string (stream)
		       (mh:write-messages stream "drafts"
					  '(13))))
	    (equal (mh:sequence-list "drafts" "highest" t)
		   '((13 . 13)))
	    (equal (mh:sequence-list "folder" "highest" t)
		   '((22 . 22))))))))

(deftest draft-forward (t draft-forward-4)
  "Test `draft-forward', checking the sequences on disk."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-3* stream))
      (to-file (stream "home:Mail/drafts/12"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-2* stream))
      (mh:with-fresh-state
       (and (eq (mh:draft-forward "+folder" 22) 13)
	    (string= (format () "To:
Cc:
Subject:
--------
#:message/rfc822 [Forward: tEst 3] ~A/Mail/folder/22
"
			     (namify folder))
		     (with-output-to-string (stream)
		       (mh:write-messages stream "drafts" '(13))))
	    (equal (mh:sequence-list "drafts" "highest" t)
		   '((13 . 13)))
	    (equal (mh:sequence-list "folder" "highest" t)
		   '((22 . 22))))))))


;; Empty directory.

(deftest draft-forward (t draft-forward-10)
  "Test `draft-forward' with the automatic components and an empty drafts
   dir."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-3* stream))
      (mh:with-fresh-state
       (and (eq (mh:draft-forward "+folder/" 1) 1)
	    (string= (format () "To:
Cc:
Subject:
--------
#:message/rfc822 [Forward: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss] ~A/Mail/folder/1
"
			     (namify folder))
		     (with-output-to-string (stream)
		       (mh:write-messages stream "drafts"
					  '(1)))))))))

(deftest draft-forward (t draft-forward-11)
  "Test `draft-forward' with components from a file and an empty drafts
   dir."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/" "Mail/forwcomps")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/forwcomps")
	(write-string "To:
From: forwarder
Subject: [Forward: ]
--------
"
		      stream))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-3* stream))
      (mh:with-fresh-state
       (and (eq (mh:draft-forward "folder" 22) 1)
	    (string= (format () "To:
From: forwarder
Subject: [Forward: ]
--------
#:message/rfc822 [Forward: tEst 3] ~A/Mail/folder/22
"
			     (namify folder))
		     (with-output-to-string (stream)
		       (mh:write-messages stream "drafts"
					  '(1)))))))))


;;;; Errors.

(deftest draft-forward ('(t (12 22)) draft-forward-100)
  "Test `draft-forward' with a component from file that contains parse errors,
   checking the draft folder afterwards."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/" "Mail/forwcomps")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-1* stream))
      (to-file (stream "home:Mail/drafts/12"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-2* stream))
      (to-file (stream "home:Mail/drafts/22"
		       :if-does-not-exist :create)
	(write-string *draft-forward-mail-3* stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:draft-forward "folder" 3)
	   (error () (setq ret t)))
	 (list ret
	       (mh:pick-messages "drafts" '(:all))))))))
