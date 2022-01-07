;;; Tests of mh:current-message.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar current-message-mail-1 "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar current-message-mail-2 "To: to@toto.org
From: Tester ftest@ttt.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar mail-3 "To: totest@toto.org
From: Tester ftest@ttt.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(deftest current-message (1 current-message-1)
  "Test `current-message' with a single message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (mh:with-fresh-state
       ;; Force creation of current sequence.
       (mh:current-message "folder")
       (mh:current-message "folder")))))

(deftest current-message (15 current-message-2)
  "Test `current-message' many spread messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/10"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/15"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (mh:with-fresh-state
       ;; Force creation of current sequence.
       (mh:current-message "folder")
       (mh:current-message "folder")))))

(deftest current-message (1 current-message-3)
  "Test `current-message' with a single message, where the current sequence
   must be added."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (mh:with-fresh-state
       ;; Force creation of highest sequence.
       (mh:pick-messages "folder" '(:all))
       (mh:current-message "folder")))))

(deftest current-message (15 current-message-4)
  "Test `current-message' many spread messages, where the current message
   must be added."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/10"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/15"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (mh:with-fresh-state
       ;; Force creation of highest sequence.
       (mh:pick-messages "folder" '(:all))
       (mh:current-message "folder")))))

(deftest current-message (15 current-message-5)
  "Test `current-message' many spread messages, where the current and
   highest sequences must be added."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/10"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/15"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (mh:with-fresh-state
       (mh:current-message "folder")))))

(deftest current-message (15 current-message-6)
  "Test `current-message' many spread messages, where the current and
   highest sequences must be added."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/10"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/15"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (mh:with-fresh-state
       (mh:current-message "folder")))))



;;;; Empty folder.

(deftest current-message (0 current-message-10)
  "Test `current-message' with an empty folder."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       ;; Force creation of current sequence.
       (mh:current-message "folder")
       (mh:current-message "folder")))))

(deftest current-message (0 current-message-11)
  "Test `current-message' with an empty folder, where the current sequence
   must be added."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       ;; Force creation of highest sequence.
       (mh:pick-messages "folder" '(:all))
       (mh:current-message "folder")))))

(deftest current-message (0 current-message-12)
  "Test `current-message' on an empty folder where the current and highest
   sequences must be added."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (mh:current-message "folder")))))


;;;; `setf'.

(deftest current-message (t current-message-20)
  "Test setting `current-message' in an empty folder."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (setf (mh:current-message "folder") 1)
	   (error () (setq ret t)))
	 ret)))))

(deftest current-message (0 current-message-21)
  "Test setting `current-message' to zero in an empty folder."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (setf (mh:current-message "folder") 0)
       (mh:current-message "folder")))))

(deftest current-message (1 current-message-22)
  "Test setting `current-message' by changing the current message, where
   the current sequence is missing."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/15"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (mh:with-fresh-state
       (setf (mh:current-message "folder") 1)
       (mh:current-message "folder")))))

(deftest current-message (2 current-message-23)
  "Test setting `current-message' by changing the current message, where
   the current sequence exists."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/15"
		       :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string current-message-mail-2 stream))
      (mh:with-fresh-state
       ;; Create the current sequence by side effect.
       (mh:current-message "folder")
       (setf (mh:current-message "folder") 2)
       (mh:current-message "folder")))))

(deftest current-message (2 current-message-24)
  "Test `current-message', trying to get the disk and cache sequences out
   of sync by caching after sequence creation."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/2" :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/4" :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (mh:with-fresh-state
       ;; Force sequence creation, should be 4.
       (mh:current-message "folder")
       ;; Force caching.
       (mh:pick-messages "folder" '("cur"))
       (setf (mh:current-message "folder") 2)
       (mh:current-message "folder")))))

(deftest current-message (2 current-message-25)
  "Test `current-message', trying to get the disk and cache sequences out
   of sync by caching before sequence creation."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/2" :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/4" :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (mh:with-fresh-state
       ;; Force caching.
       (mh:pick-messages "folder" '(:all))
       ;; Here (mh:current-message "folder") => 4, as 4 is highest.
       (setf (mh:current-message "folder") 2)
       (mh:current-message "folder")))))

(deftest current-message (2 current-message-26)
  "Test `current-message', trying to get the disk and cache sequences out
   of sync by caching between sequence creation and setting."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/2" :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/4" :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (mh:with-fresh-state
       ;; Force creation.
       (mh:current-message "folder")
       ;; Force caching, cur seq in cache should be 4 afterwards.
       (mh:pick-messages "folder" '("cur"))
       (setf (mh:current-message "folder") 2)
       ;; Here cur seq on disk and in cache should be 2.
       (mh:current-message "folder")))))

(deftest current-message ('((4 . 4))
			current-message-27)
  "Test setting `current-message', ensuring that the actual sequence is
   correct afterwards."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "a: 1 2 3 4~%")
	(format stream "cur: 1~%"))
      (to-file (stream "home:Mail/folder/1" :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/2" :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/3" :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/4" :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/7" :if-does-not-exist :create)
	(write-string current-message-mail-1 stream))
      (mh:with-fresh-state
       ;; Force caching.
       (mh:pick-messages "folder" '("cur"))
       (setf (mh:current-message "folder") 4)
       (mh:sequence-list "folder" "cur" :direct)))))


;;;; Errors.

(deftest current-message (t current-message-60)
  "Test `current-message' with wrong type of argument."
  (let (ret)
    (handler-case
	(mh:current-message t)
      (error () (setq ret t)))
    ret))
