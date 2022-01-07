;;; Tests of mh:mark-messages.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar mark-messages-mail-1 "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar mark-messages-mail-2 "To: test2@two.org.za
From: short@name.org
Subject: subject
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")


;;;; Single message.

(deftest mark-messages ('(1) mark-messages-1)
  "Test `mark-messages' adding a message to create a new sequence."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1) "trash" :add)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(2) mark-messages-2)
  "Test `mark-messages' taking a message from a new sequence, with a + on
   the folder name."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1) "trash" :add)
       (mh:mark-messages "+folder" '(2) "trash" :add)
       (mh:mark-messages "+folder" '(1) "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(1 11 22) mark-messages-3)
  "Test `mark-messages' adding a message to an existing sequence."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 11 22" stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1) "trash" :add)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(11 22) mark-messages-4)
  "Test `mark-messages' taking a message from an existing sequence, with a
   trailing slash on the folder name, clearing the message cache before
   picking."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder/" '(3) "trash" :delete)
       (mh:update-folder-table)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages (() mark-messages-5)
  "Test `mark-messages' taking all messages from an existing sequence."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder/" '(:all) "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages (1 mark-messages-6)
  "Test `mark-messages', ensuring the current message stays in tact."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (mh:with-fresh-state
       (setf (mh:current-message "folder") 1)
       (mh:mark-messages "+folder" '(1) "trash" :add)
       (mh:current-message "folder")))))

(deftest mark-messages (22 mark-messages-7)
  "Test `mark-messages', ensuring the current message is accomodated."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (mh:with-fresh-state
       (setf (mh:current-message "folder") 12)
       (mh:mark-messages "+folder" '(:all) "cur" :delete)
       (mh:current-message "folder")))))

(deftest mark-messages ('((12) (1 22) (12 22) (12 22 12) (1 22))
			 mark-messages-8)
  "Test `mark-messages' with multiple sequences."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       	:if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "aaa: 12~%")
	(format stream "bbb: 1 22~%")
	(format stream "ccc: 12 22~%")
	(format stream "ddd: 12 22 12~%") ; An error.
	(format stream "abc: 1 12 22~%"))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(12) "abc" :delete)
       (list (mh:pick-messages "folder" '("aaa"))
	     (mh:pick-messages "folder" '("bbb"))
	     (mh:pick-messages "folder" '("ccc"))
	     (mh:pick-messages "folder" '("ddd"))
	     (mh:pick-messages "folder" '("abc")))))))


;;;; Multiple messages.

(deftest mark-messages ('(1 2) mark-messages-20)
  "Test `mark-messages', adding all messages explicitly."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1 "2") "trash" :add)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(1 2) mark-messages-21)
  "Test `mark-messages', adding some of the messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1 2) "trash" :add)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(2) mark-messages-22)
  "Test `mark-messages', adding a range of one."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(("2" . "2")) "trash" :add)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(7 8) mark-messages-23)
  "Test `mark-messages', adding a range of many."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(("7" . "8")) "trash" :add)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(7 8) mark-messages-24)
  "Test `mark-messages', adding a range of many, backwards."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(("7" . "8")) "trash" :add)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(8) mark-messages-25)
  "Test `mark-messages', adding a string range of one."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '("8-8") "trash" :add)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(7 8) mark-messages-26)
  "Test `mark-messages', adding a string range of many."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '("7-8") "trash" :add)
       (mh:pick-messages "folder" '("trash"))))))


(deftest mark-messages (() mark-messages-30)
  "Test `mark-messages', taking all messages explicitly."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1 "2") "trash" :add)
       (mh:mark-messages "folder" '(1 "2") "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(1 3) mark-messages-31)
  "Test `mark-messages', taking some of the messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1 2 3) "trash" :add)
       (mh:mark-messages "folder" '(2) "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(1) mark-messages-32)
  "Test `mark-messages', taking a range of one."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(("1" . "2")) "trash" :add)
       (mh:mark-messages "folder" '(("2" . "2")) "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(1) mark-messages-33)
  "Test `mark-messages', taking a range of many."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1 ("7" . "8")) "trash" :add)
       (mh:mark-messages "folder" '(("7" . "8")) "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(1) mark-messages-34)
  "Test `mark-messages', taking a range of many, where some of the range
   are missing."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/9"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1 7 9) "trash" :add)
       (mh:mark-messages "folder" '(("7" . "9")) "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(1) mark-messages-35)
  "Test `mark-messages', taking a range of many, backwards."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1 7 8) "trash" :add)
       (mh:mark-messages "folder" '(("8" . "7")) "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(1) mark-messages-36)
  "Test `mark-messages', taking a string range of one."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1 8) "trash" :add)
       (mh:mark-messages "folder" '("8-8") "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages ('(1) mark-messages-37)
  "Test `mark-messages', taking a string range of many."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1 7 8) "trash" :add)
       (mh:mark-messages "folder" '("7-8") "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))


;;;; All messages.

(deftest mark-messages (() mark-messages-50)
  "Test `mark-messages', specifying the all string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(7 8) "trash" :add)
       (mh:mark-messages "folder" '("all") "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages (() mark-messages-51)
  "Test `mark-messages', specifying the all symbol."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1 8) "trash" :add)
       (mh:mark-messages "folder" '(:all) "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))


;;;; Errors.

(deftest mark-messages ('(11 3 22) mark-messages-90)
  "Test `mark-messages' taking a message from an existing sequence, where
   the sequences are out of order."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 11 3 22" stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(3) "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-messages (t mark-messages-91)
  "Test `mark-messages' adding all messages to an existing sequence."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:mark-messages "folder/" '(:all) "trash" :add)
	   (error () (setq ret t))))))))

(deftest mark-messages ('((1 . 1)) mark-messages-92)
  "Test `mark-messages' adding missing message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1) "trash" :add)
       (mh:sequence-list "folder" "trash")))))

(deftest mark-messages (() mark-messages-93)
  "Test `mark-messages' subtracting a missing message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(1) "trash" :delete)
       (mh:sequence-list "folder" "trash")))))

(deftest mark-messages (t mark-messages-94)
  "Test `mark-messages' with an erroneous symbol message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:mark-messages "folder" '(:cur) "trash" :add)
	   (error () (setq ret t))))))))

(deftest mark-messages (t mark-messages-95)
  "Test `mark-messages' with all string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:mark-messages "folder" '("all") "trash" :add)
	   (error () (setq ret t))))))))

(deftest mark-messages (t mark-messages-96)
  "Test `mark-messages', with a sequence string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:mark-messages "folder" '("cur") "trash" :add)
	   (error () (setq ret t)))
	 ret)))))

(deftest mark-messages (t mark-messages-113)
  "Test `mark-messages' specifying a range with integers."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:mark-messages "folder" '((2 . 2)) "trash" :delete)
	   (error () (setq ret t)))
	 ret)))))

(deftest mark-messages (t mark-messages-114)
  "Test `mark-messages' specifying a range with missing messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/9"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:mark-messages "folder" '(("7" . "9")) "trash" :add)
       (let (ret)
	 (handler-case
	     ;; FIX maybe should handle better
	     (mh:pick-messages "folder" '("trash"))
	   (error () (setq ret t)))
	 ret)))))

(deftest mark-messages (t mark-messages-115)
  "Test `mark-messages' specifying a range with junk in the first
   string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:mark-messages "folder" '(("7." "8")) "trash" :delete)
	   (error () (setq ret t))))))))

(deftest mark-messages (t mark-messages-116)
  "Test `mark-messages' specifying a range with junk in the second
   string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:mark-messages "folder" '(("7" "8-")) "trash" :add)
	   (error () (setq ret t))))))))

(deftest mark-messages (t mark-messages-117)
  "Test `mark-messages', adding :highest."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:mark-messages "folder" '(:highest) "trash" :add)
	   (error () (setq ret t)))
	 ret)))))

(deftest mark-messages (t mark-messages-118)
  "Test `mark-messages', subtracting :highest."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string mark-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:mark-messages "folder" '(:highest) "trash" :delete)
	   (error () (setq ret t)))
	 ret)))))
