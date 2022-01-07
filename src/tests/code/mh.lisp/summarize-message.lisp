;;; Tests of mh:summarize-message.     -*- Flush-Trailing-Whitespace: nil -*-

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar summarize-message-mail-1 "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar summarize-message-mail-2 "To: to@toto.org
From: Tester ftest@ttt.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar summarize-message-mail-3 "To: totest@toto.org
From: Tester ftest@ttt.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100

Body.
")


;;;; Automatic args.

(deftest summarize-message ("   1   15-Apr    184 nn1n2n3n4n5n6n7n8 ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgsh  Bod
" summarize-message-1)
  "Test `summarize-message' where name, subject and body must be truncated."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-message-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-message "home:Mail/folder/1" stream))))))

(deftest summarize-message ("   1   15-Apr    100 To: to@toto.org   test 1b                                Bod
" summarize-message-2)
  "Test `summarize-message' where the from field matches mh:*from-name*."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-message-mail-2 stream))
      (mh:with-fresh-state
       (let ((mh:*from-name* "Tester"))
	 (with-output-to-string (stream)
	   (mh:summarize-message "home:Mail/folder/1" stream)))))))

(deftest summarize-message ("   1   15-Apr    104 To: totest@toto.o test 1b                                Bod
" summarize-message-3)
  "Test `summarize-message' where the from field matches
   mh:*alternate-addresses*, using a relative pathname."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-message-mail-3 stream))
      (mh:with-fresh-state
       (let ((mh:*alternate-addresses* '("ftest@ttt.org")))
	 (with-output-to-string (stream)
	   (in-directory "home:Mail/folder/"
	     (mh:summarize-message "1" stream))))))))

(deftest summarize-message ("   1   15-Apr     80                   test 1b                                B o
" summarize-message-4)
  "Test `summarize-message' where the from field is missing and the quoted
   body contains newlines."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100

B
o
d
y.
"
		      stream))
      (mh:with-fresh-state
       (let ((mh:*alternate-addresses* '("ftest@ttt.org")))
	 (with-output-to-string (stream)
	   (mh:summarize-message "home:Mail/folder/1" stream)))))))

(deftest summarize-message (t summarize-message-5)
  "Test `summarize-message' where the date field is missing."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tester ftest@ttt.org
Subject: test 1b

Body.
"
		      stream))
      (mh:with-fresh-state
       (let ((mh:*alternate-addresses* '("ftest@ttt.org")))
	 (string=
	  (with-output-to-string (stream)
	    (mh:summarize-message "home:Mail/folder/1" stream))
 	  (format () "   1   ~6<~A~>*    71 To: totest@toto.o test 1b                                Bod
"
 		  (mh::file-date "home:Mail/folder/1"))))))))

(deftest summarize-message ("   1   15-Apr     87 To: totest@toto.o                                        Bod
"
			    summarize-message-6)
  "Test `summarize-message' where the date is more than a year ago and the
   subject is missing."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tester ftest@ttt.org
Date: 15 Apr 2000 16:02:10 +0100

Body.
"
		      stream))
      (mh:with-fresh-state
       (let ((mh:*alternate-addresses* '("ftest@ttt.org")))
	 (with-output-to-string (stream)
	   (mh:summarize-message "home:Mail/folder/1" stream)))))))

(deftest summarize-message ("   1   15-Apr    104 To: totest@toto.o test 1b                                Bod
"
			    summarize-message-7)
  "Test `summarize-message' where the date is more than a year ago, using a
   relative pathname."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tester ftest@ttt.org
Subject: test 1b
Date: 15 Apr 2000 16:02:10 +0100

Body.
"
		      stream))
      (mh:with-fresh-state
       (let ((mh:*alternate-addresses* '("ftest@ttt.org")))
	 (with-output-to-string (stream)
	   (in-directory "home:Mail/folder/"
	     (mh:summarize-message "1" stream))))))))

(deftest summarize-message ("   1 - 15-Apr    138 To: totest@toto.o test 1b                                Bod
"
			    summarize-message-8)
  "Test `summarize-message' where the message has a replied field."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tester ftest@ttt.org
Subject: test 1b
Date: 15 Apr 2000 16:02:10 +0100
replied: 04 Mar 2008 13:43:40 GMT

Body.
"
		      stream))
      (mh:with-fresh-state
       (let ((mh:*alternate-addresses* '("ftest@ttt.org")))
	 (with-output-to-string (stream)
	   (mh:summarize-message "home:Mail/folder/1" stream)))))))

(deftest summarize-message ("   1 - 15-Apr    135 To: totest@toto.o test 1b                                Bod
"
			    summarize-message-9)
  "Test `summarize-message' where the message has a broken replied field."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tester ftest@ttt.org
Subject: test 1b
Date: 15 Apr 2000 16:02:10 +0100
replied: this should be a date

Body.
"
		      stream))
      (mh:with-fresh-state
       (let ((mh:*alternate-addresses* '("ftest@ttt.org")))
	 (with-output-to-string (stream)
	   (mh:summarize-message "home:Mail/folder/1" stream)))))))

; FIX this shows trailing space error
;                                                                                  trailing space to here
(deftest summarize-message ("   1   15-Apr     97 Tess              test 1b                              
"
			    summarize-message-10)
  "Test `summarize-message' with an empty body and where the From name must
   be stipped."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tess <ftest@ttt.org>
Subject: test 1b
Date: 15 Apr 2000 16:02:10 +0100
"
		      stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-message "home:Mail/folder/1" stream))))))

(deftest summarize-message ("   1   15-Apr   107K ftest@ttt.org     test 1b                                123
"
			    summarize-message-11)
  "Test `summarize-message' with message size between 1K and 1M."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: ftest@ttt.org
Subject: test 1b
Date: 15 Apr 2000 16:02:10 +0100

"
		      stream)
	(dotimes (i 2001)
	  (write-string "1234567890 1234567890 1234567890 1234567890 1234567890"
			stream)
	  (terpri stream)))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-message "home:Mail/folder/1" stream))))))

#| FIX how to do this fast?
(deftest summarize-message ("   1   15-Apr   107K To: totest@toto.o test 1b                                123
"
			    summarize-message-12)
  "Test `summarize-message' with message size bigger than 1M."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tester ftest@ttt.org
Subject: test 1b
Date: 15 Apr 2000 16:02:10 +0100

"
		      stream)
	(dotimes (i 20000001)
	  (write-string "1234567890 1234567890 1234567890 1234567890 1234567890"
			stream)
	  (terpri stream)))
      (mh:with-fresh-state
       (let ((mh:*alternate-addresses* '("ftest@ttt.org")))
	 (with-output-to-string (stream)
	   (mh:summarize-message "home:Mail/folder/1" stream)))))))
|#

(deftest summarize-message ("   1   15-Apr     96 To: totest@toto.o                                        Bod
"
			    summarize-message-13)
  "Test `summarize-message' where the subject is empty."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tester ftest@ttt.org
Date: 15 Apr 2000 16:02:10 +0100
Subject:

Body.
"
		      stream))
      (mh:with-fresh-state
       (let ((mh:*alternate-addresses* '("ftest@ttt.org")))
	 (with-output-to-string (stream)
	   (mh:summarize-message "home:Mail/folder/1" stream)))))))


;;;; Width argument.

;; FIX whitespace error
;                                                                                      whitespace to here
(deftest summarize-message ("   1   15-Apr    100 Tester ftest@ttt. foo                                  
"
			    summarize-message-40)
  "Test `summarize-message' with a small width argument."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tester ftest@ttt.org
Date: 15 Apr 2000 16:02:10 +0100
Subject: foo

Body.
"
		      stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-message "home:Mail/folder/1" stream 20))))))

(deftest summarize-message ("   1   15-Apr    100 Tester ftest@ttt. foo                                  
"
			    summarize-message-41)
  "Test `summarize-message' with a very small width argument."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tester ftest@ttt.org
Date: 15 Apr 2000 16:02:10 +0100
Subject: foo

Body.
"
		      stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-message "home:Mail/folder/1" stream 1))))))

(deftest summarize-message ("   1   15-Apr    100 Tester ftest@ttt. foo                                  
"
			    summarize-message-42)
  "Test `summarize-message' with the smallest width argument."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tester ftest@ttt.org
Date: 15 Apr 2000 16:02:10 +0100
Subject: foo

Body.
"
		      stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-message "home:Mail/folder/1" stream 0))))))

(deftest summarize-message ("   1   15-Apr    113 Tester ftest@ttt. foo                                    Body.   More body.
"
			    summarize-message-43)
  "Test `summarize-message' with a large width argument."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tester ftest@ttt.org
Date: 15 Apr 2000 16:02:10 +0100
Subject: foo

Body.


More body.
"
		      stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-message "home:Mail/folder/1" stream 2000))))))


;;;; Current arg.

(deftest summarize-message ("   1+  15-Apr    113 Tester ftest@ttt. foo                                    Bod
"
			    summarize-message-60)
  "Test `summarize-message' with a large width argument."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
From: Tester ftest@ttt.org
Date: 15 Apr 2000 16:02:10 +0100
Subject: foo

Body.


More body.
"
		      stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-message "home:Mail/folder/1" stream 80 t))))))


;;;; Errors.

(deftest summarize-message (t summarize-message-90)
  "Test `summarize-message' with a missing file."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (with-output-to-string (stream)
	   (handler-case
	       (mh:summarize-message "home:Mail/folder/1" stream)
	     (error () (setq ret t))))
	 ret)))))

(deftest summarize-message (() summarize-message-91)
  "Test `summarize-message' with an empty file."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (touch-file "home:Mail/folder/1")
      (mh:with-fresh-state
       (let (ret)
	 (with-output-to-string (stream)
	   (setq ret (mh:summarize-message "home:Mail/folder/1"
					   stream)))
	 ret)))))
