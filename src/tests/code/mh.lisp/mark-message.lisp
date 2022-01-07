;;; Tests of mh:mark-message.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar mark-message-mail-1 "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(deftest mark-message ('(1) mark-message-1)
  "Test `mark-message' adding a message to create a new sequence."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (mh:with-fresh-state
       (mh:mark-message "folder" 1 "trash" :add)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-message ('(2) mark-message-2)
  "Test `mark-message' taking a message from a new sequence, with a + on
   the folder name."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (mh:with-fresh-state
       (mh:mark-message "folder" 1 "trash" :add)
       (mh:mark-message "+folder" 2 "trash" :add)
       (mh:mark-message "+folder" 1 "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-message ('(1 11 22) mark-message-3)
  "Test `mark-message' adding a message to an existing sequence."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 11 22" stream))
      (mh:with-fresh-state
       (mh:mark-message "folder" 1 "trash" :add)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-message ('(11 22) mark-message-4)
  "Test `mark-message' taking a message from an existing sequence, with a
   trailing slash on the folder name, clearing the message cache before
   picking."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (mh:mark-message "folder/" 3 "trash" :delete)
       (mh:update-folder-table)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-message (() mark-message-5)
  "Test `mark-message' taking all messages from an existing sequence."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (mh:mark-message "folder/" :all "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-message (1 mark-message-6)
  "Test `mark-message', ensuring the current message stays in tact."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (mh:with-fresh-state
       (setf (mh:current-message "folder") 1)
       (mh:mark-message "+folder" 1 "trash" :add)
       (mh:current-message "folder")))))

(deftest mark-message (22 mark-message-7)
  "Test `mark-message', ensuring the current message is accomodated."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (mh:with-fresh-state
       (setf (mh:current-message "folder") 12)
       (mh:mark-message "+folder" :all "cur" :delete)
       (mh:current-message "folder")))))

(deftest mark-message ('((12) (1 22) (12 22) (12 22 12) (1 22))
			 mark-message-8)
  "Test `mark-message' with multiple sequences."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       	:if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "aaa: 12~%")
	(format stream "bbb: 1 22~%")
	(format stream "ccc: 12 22~%")
	(format stream "ddd: 12 22 12~%") ; An error.
	(format stream "abc: 1 12 22~%"))
      (mh:with-fresh-state
       (mh:mark-message "folder" 12 "abc" :delete)
       (list (mh:pick-messages "folder" '("aaa"))
	     (mh:pick-messages "folder" '("bbb"))
	     (mh:pick-messages "folder" '("ccc"))
	     (mh:pick-messages "folder" '("ddd"))
	     (mh:pick-messages "folder" '("abc")))))))


;;;; Errors.

(deftest mark-message ('(11 3 22) mark-message-50)
  "Test `mark-message' taking a message from an existing sequence, where
   the sequences are out of order."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 11 3 22" stream))
      (mh:with-fresh-state
       (mh:mark-message "folder" 3 "trash" :delete)
       (mh:pick-messages "folder" '("trash"))))))

(deftest mark-message (t mark-message-51)
  "Test `mark-message' adding all messages to an existing sequence."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mark-message-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:mark-message "folder/" :all "trash" :add)
	   (error () (setq ret t))))))))

(deftest mark-message ('((1 . 1)) mark-message-52)
  "Test `mark-message' adding missing message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (mh:mark-message "folder" 1 "trash" :add)
       (mh:sequence-list "folder" "trash")))))

(deftest mark-message (() mark-message-53)
  "Test `mark-message' subtracting a missing message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (mh:mark-message "folder" 1 "trash" :delete)
       (mh:sequence-list "folder" "trash")))))

(deftest mark-message (t mark-message-54)
  "Test `mark-message' with an erroneous symbol message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:mark-message "folder" :cur "trash" :add)
	   (error () (setq ret t))))))))

(deftest mark-message (t mark-message-55)
  "Test `mark-message' with all string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:mark-message "folder" "all" "trash" :add)
	   (error () (setq ret t))))))))
