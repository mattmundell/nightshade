;;; Tests of mh:pick-messages.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar *pick-messages-mail-1* "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar *pick-messages-mail-2* "To: test2@two.org.za
From: short@name.org
Subject: subject
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")

(defvar *pick-messages-mail-3* "To: Three@three.org
Cc: people@CC.org
From: short@Name.org
Subject: tEst 3
Date: 16 Apr 2008 16:02:10 +0100
Arb: xXx

Body body body.

More body.

Ending.
")


;; Pick all from one.

(deftest pick-messages ('(12) pick-messages-1)
  "Test `pick-messages' picking all from one message in a folder of many,
   using a string spec."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/dest/22"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/dest/23"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder/" '("12"))))))

(deftest pick-messages ('(22) pick-messages-2)
  "Test `pick-messages' picking all from one message in a folder of one,
   using an integer spec."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder/" '(22))))))

(deftest pick-messages ('(22) pick-messages-3)
  "Test `pick-messages' picking all from one message in a folder of one,
   with an integer spec."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:pick-messages "folder/" '(:highest))
	   (error () (setq ret '(22))))
	 ret)))))

(deftest pick-messages ('(22) pick-messages-4)
  "Test `pick-messages' picking all from one message in a folder of one,
   with a sequence name spec."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder/" '("highest"))))))


;;;; Pick all from many.

(deftest pick-messages ('(12 22) pick-messages-20)
  "Test `pick-messages' picking all from many messages in a folder of
   many."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/dest/22"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/dest/23"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder/" '("12" 22))))))

(deftest pick-messages ('(12) pick-messages-21)
  "Test `pick-messages' picking all from many messages in a folder of many,
   specifying a range of one."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/dest/22"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/dest/23"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder/" '(("12" . "12")))))))

(deftest pick-messages ('(11 12 13) pick-messages-22)
  "Test `pick-messages' picking all from many messages in a folder of many,
   specifying a range of many."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder/" '(("11" . "13")))))))

(deftest pick-messages ('(11 12 13) pick-messages-23)
  "Test `pick-messages' picking all from many messages in a folder of many,
   specifying a range of many backwards."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder/" '(("13" . "11")))))))

(deftest pick-messages ('(13) pick-messages-24)
  "Test `pick-messages' picking all from many messages in a folder of many,
   specifying a string range of one."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder/" '("13-13"))))))

(deftest pick-messages ('(12 13) pick-messages-25)
  "Test `pick-messages' picking all from many messages in a folder of many,
   specifying a string range of many."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '("12-13"))))))

(deftest pick-messages ('(12 13) pick-messages-26)
  "Test `pick-messages' picking all from many messages in a folder of many,
   specifying a string range of many backwards."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '("13-12"))))))

(deftest pick-messages ('(13) pick-messages-27)
  "Test `pick-messages' picking all from many messages in a folder of many,
   specifying a sequence string."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '("cur"))))))

(deftest pick-messages (() pick-messages-28)
  "Test `pick-messages' picking a missing sequence."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '("xxx"))))))


;;;; Pick all from all.

(deftest pick-messages ('(11 12 13) pick-messages-40)
  "Test `pick-messages' picking all from all messages in a folder of many,
   specifying the all string."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '("all"))))))

(deftest pick-messages ('(11 12 13) pick-messages-41)
  "Test `pick-messages' picking all from all messages in a folder of many,
   specifying the all symbol."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '(:all))))))


;;;; Pick expression from one.

(deftest pick-messages ('(13) pick-messages-60)
  "Test `pick-messages' picking one from one message in a folder of many,
   specifying the all symbol and a large expression."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-3* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder"
			 '("13")
			 '(and (mh:to "Three@")
			       (mh:f-to "three@")
			       (mh:from "Name.org")
			       (mh:f-from "name.org")
 			       (mh:cc "CC.org")
			       (mh:f-cc "cc.org")
			       (mh:subject "tEst 3")
			       (mh:f-subject "test 3")
			       (mh:content "More body.")
 			       (mh:f-content "more body.")
 			       (mh:date "16 Apr")
 			       (mh:f-date "16 apr")
 			       (mh:before "16 apr 2009")
 			       (mh:after "16 mar 2008")
 			       (mh:-- "Cc" "CC.org")
 			       (mh:-- "Arb" "xXx")
 			       (mh:f--- "Arb" "xxx")))))))

(deftest pick-messages ('() pick-messages-61)
  "Test `pick-messages' picking one from one message in a folder of many,
   specifying the all symbol and a large expression."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-3* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder"
			 '("13")
			 '((lambda ())))))))


;;;; Pick expression from many.

(deftest pick-messages ('(13) pick-messages-80)
  "Test `pick-messages' picking one from many messages in a folder of many,
   specifying the all symbol and a large expression."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-3* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder"
			 '(:all)
			 '(and (mh:to "Three@")
			       (mh:f-to "three@")
			       (mh:from "Name.org")
			       (mh:f-from "name.org")
 			       (mh:cc "CC.org")
			       (mh:f-cc "cc.org")
			       (mh:subject "tEst 3")
			       (mh:f-subject "test 3")
			       (mh:content "More body.")
 			       (mh:f-content "more body.")
 			       (mh:date "16 Apr")
 			       (mh:f-date "16 apr")
 			       (mh:before "16 apr 2009")
 			       (mh:after "16 mar 2008")
 			       (mh:-- "Cc" "CC.org")
 			       (mh:-- "Arb" "xXx")
 			       (mh:f--- "Arb" "xxx")))))))

(deftest pick-messages ('() pick-messages-81)
  "Test `pick-messages' picking empty from many messages in a folder of
   many, specifying the all symbol and a large expression."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-3* stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder"
			 '(:all)
			 '(and (mh:to "three@")  ; Three@
			       (mh:f-to "three@")
			       (mh:from "Name.org")
			       (mh:f-from "name.org")
 			       (mh:cc "CC.org")
			       (mh:f-cc "cc.org")
			       (mh:subject "tEst 3")
			       (mh:f-subject "test 3")
			       (mh:content "More body.")
 			       (mh:f-content "more body.")
 			       (mh:date "16 Apr")
 			       (mh:f-date "16 apr")
 			       (mh:before "16 apr 2009")
 			       (mh:after "16 mar 2008")
 			       (mh:-- "Cc" "CC.org")
 			       (mh:-- "Arb" "xXx")
 			       (mh:f--- "Arb" "xxx")))))))


;;;; Errors.

(deftest pick-messages (t pick-messages-100)
  "Test `pick-messages' picking a missing message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:pick-messages "folder" '(1))
	   (error () (setq ret t)))
	 ret)))))

(deftest pick-messages (t pick-messages-111)
  "Test `pick-messages' with an erroneous symbol message."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:pick-messages "folder" '(:cur))
	   (error () (setq ret t))))))))

(deftest pick-messages (t pick-messages-113)
  "Test `pick-messages' specifying a range with integers."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:pick-messages "folder" '((2 . 2)))
	   (error () (setq ret t)))
	 ret)))))

(deftest pick-messages (t pick-messages-114)
  "Test `pick-messages' specifying a range with missing messages."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (to-file (stream "home:Mail/folder/9"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:pick-messages "folder" '(("7" . "9")))
	   (error () (setq ret t)))
	 ret)))))

(deftest pick-messages (t pick-messages-115)
  "Test `pick-messages' specifying a range with junk in the first
   string."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:pick-messages "folder" '(("7." "8")))
	   (error () (setq ret t))))))))

(deftest pick-messages (t pick-messages-116)
  "Test `pick-messages' specifying a range with junk in the second
   string."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:pick-messages "folder" '(("7" "8-")))
	   (error () (setq ret t))))))))

(deftest pick-messages (t pick-messages-117)
  "Test `pick-messages', with :highest."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string *pick-messages-mail-2* stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:pick-messages "folder" '(:highest))
	   (error () (setq ret t)))
	 ret)))))
