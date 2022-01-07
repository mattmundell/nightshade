;;; Tests of mh:draft-new.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar *draft-new-mail-1* "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar *draft-new-mail-2* "To: test2@two.org.za
From: short@name.org
Subject: subject
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")

(defvar *draft-new-mail-3* "To: Three@three.org
Cc: people@CC.org
From: short@Name.org
Subject: tEst 3
Date: 16 Apr 2008 16:02:10 +0100
Arb: xXx

Body body body.

More body.

Ending.
")


;; Directory with exising drafts.

(deftest draft-new ('(23
		      "To:
Cc:
Subject:
--------
")
		    draft-new-1)
  "Test `draft-new' with the automatic components."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: folder~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-1* stream))
      (mh:with-fresh-state
       (list (mh:draft-new)
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '(23))))))))

(deftest draft-new ('(23
		      "To:
From: a@b.c
Subject: [TEST]
-------
")
		    draft-new-2)
  "Test `draft-new' giving components."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: folder~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-2* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-3* stream))
      (mh:with-fresh-state
       (list (mh:draft-new () "To:
From: a@b.c
Subject: [TEST]
-------
")
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '(23))))))))

(deftest draft-new ('(23
		      "To:
Cc:
Subject: [TEST] From file.
-------
")
		    draft-new-3)
  "Test `draft-new' giving components in a file."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/" "Mail/comps")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: folder~%"))
      (to-file (stream "home:Mail/comps")
	(write-string "To:
Cc:
Subject: [TEST] From file.
-------
"
		      stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-2* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-3* stream))
      (mh:with-fresh-state
       (list (mh:draft-new "comps")
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '(23))))))))

(deftest draft-new ('(23
		      "To:
Cc:
Subject:
--------
")
		    draft-new-4)
  "Test `draft-new' giving components in a missing file."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: folder~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-2* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-3* stream))
      (mh:with-fresh-state
       (list (mh:draft-new "comps")
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '(23))))))))


;; Empty directory.

(deftest draft-new ('(1
		      "To:
Cc:
Subject:
--------
")
		    draft-new-51)
  "Test `draft-new' with the automatic components."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: folder~%"))
      (mh:with-fresh-state
       (list (mh:draft-new)
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '(1))))))))

(deftest draft-new ('(1
		      "To:
From: a@b.c
Subject: [TEST]
-------
")
		    draft-new-52)
  "Test `draft-new' giving components."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: folder~%"))
      (mh:with-fresh-state
       (list (mh:draft-new () "To:
From: a@b.c
Subject: [TEST]
-------
")
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '(1))))))))

(deftest draft-new ('(1
		      "To:
Cc:
Subject: [TEST] From file.
-------
")
		    draft-new-53)
  "Test `draft-new' giving components in a file."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/" "Mail/comps")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: folder~%"))
      (to-file (stream "home:Mail/comps")
	(write-string "To:
Cc:
Subject: [TEST] From file.
-------
"
		      stream))
      (mh:with-fresh-state
       (list (mh:draft-new "comps")
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '(1))))))))

(deftest draft-new ('(1
		      "To:
Cc:
Subject:
--------
")
		    draft-new-54)
  "Test `draft-new' giving components in a missing file."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: folder~%"))
      (mh:with-fresh-state
       (list (mh:draft-new "comps")
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '(1))))))))


;;;; Errors.

(deftest draft-new (t draft-new-100)
  "Test `draft-new' with a component string that contains parse errors."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: folder~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:draft-new () "")
	   (error () (setq ret t))))))))

(deftest draft-new ('(t (3 12 22)) draft-new-101)
  "Test `draft-new' with a component from file that contains parse errors,
   checking the draft folder afterwards."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/" "Mail/comps")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: folder~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-2* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-new-mail-3* stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:draft-new "comps")
	   (error () (setq ret t)))
	 (list ret
	       (mh:pick-messages "folder" '(:all))))))))
