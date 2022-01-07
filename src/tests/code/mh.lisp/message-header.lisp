;;; Tests of mh:message-header.     -*- Flush-Trailing-Whitespace: nil -*-

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar message-header-mail-1 "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar message-header-mail-2 "To: to@toto.org
From: Tester ftest@ttt.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar message-header-mail-3 "To: totest@toto.org
From: Tester ftest@ttt.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100

Body.
")


;;;; Automatic args.

(deftest message-header ("totest@toto.org" message-header-1)
  "Test `message-header' with To."
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
       (mh:message-header "folder" 1 "To")))))

(deftest message-header (() message-header-2)
  "Test `message-header' with a missing header."
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
       (mh:message-header "+folder" "1" "Xxx")))))

(deftest message-header ("xxx" message-header-3)
  "Test `message-header' with a single character header."
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
X: xxx

B
o
d
y.
"
		      stream))
      (mh:with-fresh-state
       (mh:message-header "folder/" :highest "X")))))

(deftest message-header ("xxx" message-header-4)
  "Test `message-header' with a single character header."
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
X: xxx

B
o
d
y.
"
		      stream))
      (mh:with-fresh-state
       (mh:message-header "+folder/" "highest" "X")))))

(deftest message-header ("15 Apr 2008 16:02:10 +0100"
			 message-header-5)
  "Test `message-header' with Date."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/111"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100
X: xxx

B
o
d
y.
"
		      stream))
      (mh:with-fresh-state
       (mh:message-header "folder" "111" "Date")))))

(deftest message-header ("" message-header-6)
  "Test `message-header' with an empty header."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100
Xxx:

B
o
d
y.
"
		      stream))
      (mh:with-fresh-state
       (mh:message-header "folder" 11 "Xxx")))))

(deftest message-header ("" message-header-7)
  "Test `message-header' with a whitespace header (the whitespace is
   trimmed)."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100
Xxx:    

B
o
d
y.
"
		      stream))
      (mh:with-fresh-state
       (mh:message-header "folder" 11 "Xxx")))))

(deftest message-header ("abc   " message-header-8)
  "Test `message-header' with a header bounded by whitespace (the leading
   whitespace is trimmed)."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string "To: totest@toto.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100
Test:    abc   

B
o
d
y.
"
		      stream))
      (mh:with-fresh-state
       (mh:message-header "folder" 11 "Test")))))


;;;; Errors.

(deftest message-header (() message-header-90)
  "Test `message-header' with a missing message."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (mh:message-header "folder" "1" "To")))))

;; FIX prints error message
;;
(deftest message-header (() message-header-91)
  "Test `message-header' with an empty message."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string "" stream))
      (mh:with-fresh-state
       (mh:message-header "folder" 11 "To")))))

(deftest message-header (() message-header-92)
  "Test `message-header' with a colon ending the header name."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
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
       (mh:message-header "folder" 11 "To:")))))

(deftest message-header (() message-header-93)
  "Test `message-header' with a lowercase header name."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
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
       (mh:message-header "folder" 11 "to")))))
