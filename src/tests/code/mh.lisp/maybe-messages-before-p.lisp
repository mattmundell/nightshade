;;; Tests of mh:maybe-messages-before-p.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar *maybe-messages-before-p-mail-1* "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar *maybe-messages-before-p-mail-2* "To: test2@two.org.za
From: short@name.org
Subject: subject
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")

(defvar *maybe-messages-before-p-mail-3* "To: Three@three.org
Cc: people@CC.org
From: short@Name.org
Subject: tEst 3
Date: 16 Apr 2008 16:02:10 +0100
Arb: xXx

Body body body.

More body.

Ending.
")


;; A folder of many.

(deftest maybe-messages-before-p (t maybe-messages-before-p-1)
  "Test `maybe-messages-before-p' true in a folder of many, using a string
   spec."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:maybe-messages-before-p "folder/" "12")))))

(deftest maybe-messages-before-p (t maybe-messages-before-p-2)
  "Test `maybe-messages-before-p' true in a folder of many, using a string
   spec, giving the lowest message."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:maybe-messages-before-p "+folder" "2")))))

(deftest maybe-messages-before-p (() maybe-messages-before-p-3)
  "Test `maybe-messages-before-p' false by virtue of 1 in a folder of many,
   using a string spec."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:maybe-messages-before-p "+folder/" "1")))))

(deftest maybe-messages-before-p (() maybe-messages-before-p-4)
  "Test `maybe-messages-before-p' false in a folder of many, using a string
   spec, giving the lowest message."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "+folder" '(:all))
       (mh:maybe-messages-before-p "+folder" "2")))))

(deftest maybe-messages-before-p (t maybe-messages-before-p-5)
  "Test `maybe-messages-before-p' true in a folder of many, using a string
   spec, ensuring all are cached beforehand."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '(:all))
       (mh:maybe-messages-before-p "folder/" "12")))))

(deftest maybe-messages-before-p (t maybe-messages-before-p-6)
  "Test `maybe-messages-before-p' false via a number above the highest in a
   folder of many, using an integer spec."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/21"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-2* stream))
      (mh:with-fresh-state
       (mh:maybe-messages-before-p "folder/" 23)))))

(deftest maybe-messages-before-p (t maybe-messages-before-p-7)
  "Test `maybe-messages-before-p' false via a number above the highest in a
   folder of many, using an integer spec, with all cached."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/20"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-3* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '(:all))
       (mh:maybe-messages-before-p "folder/" 23)))))


;; A folder of one.

(deftest maybe-messages-before-p (t maybe-messages-before-p-10)
  "Test `maybe-messages-before-p' true in a folder of one, using an integer
   spec, giving the lowest."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:maybe-messages-before-p "folder/" 12)))))

(deftest maybe-messages-before-p (() maybe-messages-before-p-11)
  "Test `maybe-messages-before-p' false in a folder of one, using an integer
   spec, giving the lowest."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '(:all))
       (mh:maybe-messages-before-p "folder/" 12)))))

(deftest maybe-messages-before-p (() maybe-messages-before-p-12)
  "Test `maybe-messages-before-p' false by virtue of being 1 in a folder of
   one, using an integer spec."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:maybe-messages-before-p "+folder" "1")))))

(deftest maybe-messages-before-p (() maybe-messages-before-p-13)
  "Test `maybe-messages-before-p' false via a number below the lowest in a
   folder of one, using an integer spec."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '(:all))
       (mh:maybe-messages-before-p "folder/" 2)))))

(deftest maybe-messages-before-p (t maybe-messages-before-p-14)
  "Test `maybe-messages-before-p' false via a number above the highest in a
   folder of one, using an integer spec."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:maybe-messages-before-p "folder/" 23)))))

(deftest maybe-messages-before-p (t maybe-messages-before-p-15)
  "Test `maybe-messages-before-p' false via a number above the highest in a
   folder of one, using an integer spec, with all cached."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '(:all))
       (mh:maybe-messages-before-p "folder/" 23)))))


;;;; Sequences, All.

(deftest maybe-messages-before-p (t maybe-messages-before-p-20)
  "Test `maybe-messages-before-p' true in folder of one giving highest
   string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:maybe-messages-before-p "folder/" "highest")))))

(deftest maybe-messages-before-p (() maybe-messages-before-p-21)
  "Test `maybe-messages-before-p' false in folder of one message giving
   :highest."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder/" '(:all))
       (mh:maybe-messages-before-p "folder/" "highest")))))


;;;; Errors.

(deftest maybe-messages-before-p (t maybe-messages-before-p-60)
  "Test `maybe-messages-before-p' with an erroneous symbol message."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:maybe-messages-before-p "folder" :cur)
	   (error () (setq ret t))))))))

(deftest maybe-messages-before-p (t maybe-messages-before-p-61)
  "Test `maybe-messages-before-p' specifying a range with integers."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-2* stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-2* stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:maybe-messages-before-p "folder" '((2 . 2)))
	   (error () (setq ret t)))
	 ret)))))

(deftest maybe-messages-before-p (t maybe-messages-before-p-62)
  "Test `maybe-messages-before-p' true in a folder of many, giving the
   current string."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (let ((ret ()))
	 (handler-case
	     (mh:maybe-messages-before-p "folder" "cur")
	   (error () (setq ret t)))
	 ret)))))

(deftest maybe-messages-before-p (t maybe-messages-before-p-63)
  "Test `maybe-messages-before-p' true in a folder of many, giving a
   missing sequence."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (let ((ret ()))
	 (handler-case
	     (mh:maybe-messages-before-p "folder" "xxx")
	   (error () (setq ret t)))
	 ret)))))

(deftest maybe-messages-before-p (t maybe-messages-before-p-64)
  "Test `maybe-messages-before-p' true in a folder of many, giving the all
   string."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (let ((ret ()))
	 (handler-case
	     (mh:maybe-messages-before-p "folder" "xxx")
	   (error () (setq ret t)))
	 ret)))))

(deftest maybe-messages-before-p (t maybe-messages-before-p-65)
  "Test `maybe-messages-before-p' true in a folder of many, giving the all
   symbol."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *maybe-messages-before-p-mail-1* stream))
      (mh:with-fresh-state
       (let ((ret ()))
	 (handler-case
	     (mh:maybe-messages-before-p "folder" :all)
	   (error () (setq ret t)))
	 ret)))))
