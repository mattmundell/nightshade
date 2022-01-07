;;; Tests of mh:delete-messages.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar delete-messages-mail-1 "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar delete-messages-mail-2 "To: test2@two.org.za
From: short@name.org
Subject: subject
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")


;;;; Single message.

(deftest delete-messages (() delete-messages-1)
  "Test `delete-messages' with a lone message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder" '(1))
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-messages ('((22) (1 22)) delete-messages-2)
  "Test `delete-messages' with multiple messages and a + on the folder
   name."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (mh:with-fresh-state
       (mh:delete-messages "+folder" '(12))
       (list (mh:pick-messages "folder" '("highest"))
	     (mh:pick-messages "folder" '(:all)))))))

(deftest delete-messages ('(1 22) delete-messages-3)
  "Test `delete-messages' with multiple messages, a string message arg and a
   trailing slash on the folder name, clearing the folder cache before
   picking messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder/" '("12"))
       (mh:update-folder-table)
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-messages ('(1 12) delete-messages-4)
  "Test `delete-messages' with multiple messages and a symbol message arg."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-messages "folder/" '(:highest))
	   (error () (setq ret '(1 12))))
	 ;(mh:pick-messages "folder" '(:all))
	 ret)))))

(deftest delete-messages ('(1 12) delete-messages-5)
  "Test `delete-messages' with multiple messages and a sequence name message
   arg, clearing the cache before picking the messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder/" '("highest"))
       (mh:update-folder-table)
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-messages (1 delete-messages-6)
  "Test `delete-messages' with multiple messages, ensuring the current
   message stays in tact."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (mh:with-fresh-state
       (setf (mh:current-message "folder") 1)
       (mh:delete-messages "+folder" '(12))
       (mh:current-message "folder")))))

(deftest delete-messages (22 delete-messages-7)
  "Test `delete-messages' with multiple messages, ensuring the current
   message is accomodated."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (mh:with-fresh-state
       (setf (mh:current-message "folder") 12)
       (mh:delete-messages "+folder" '(12))
       (mh:current-message "folder")))))

(deftest delete-messages ('(() (1 22) (22) (22) (1 22))
			 delete-messages-8)
  "Test `delete-messages' with multiple messages, checking that sequences
   are adjusted."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "aaa: 12~%")
	(format stream "bbb: 1 22~%")
	(format stream "ccc: 12 22~%")
	(format stream "ddd: 12 22 12~%")
	(format stream "abc: 1 12 22~%"))
      (mh:with-fresh-state
       (mh:delete-messages "folder" '(12))
       (list (mh:pick-messages "folder" '("aaa"))
	     (mh:pick-messages "folder" '("bbb"))
	     (mh:pick-messages "folder" '("ccc"))
	     (mh:pick-messages "folder" '("ddd"))
	     (mh:pick-messages "folder" '("abc")))))))


;;;; Multiple messages.

(deftest delete-messages (() delete-messages-20)
  "Test `delete-messages', specifying all messages explicitly."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder" '(1 "2"))
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-messages ('(3) delete-messages-21)
  "Test `delete-messages', specifying some of the messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder" '(1 2))
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-messages ('(1 3) delete-messages-22)
  "Test `delete-messages', specifying a range of one."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder" '(("2" . "2")))
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-messages ('(1) delete-messages-23)
  "Test `delete-messages', specifying a range of many."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder" '(("7" . "8")))
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-messages ('(1) delete-messages-24)
  "Test `delete-messages', specifying a range of many, backwards."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder" '(("8" . "7")))
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-messages ('(1 7) delete-messages-25)
  "Test `delete-messages', specifying a string range of one."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder" '("8-8"))
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-messages ('(1) delete-messages-26)
  "Test `delete-messages', specifying a string range of many."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder" '("7-8"))
       (mh:pick-messages "folder" '(:all))))))


;;;; All messages.

(deftest delete-messages (() delete-messages-50)
  "Test `delete-messages', specifying the all string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder" '("all"))
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-messages (() delete-messages-51)
  "Test `delete-messages', specifying the all symbol."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder" '(:all))
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-messages (() delete-messages-52)
  "Test `delete-messages', implying all."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder")
       (mh:pick-messages "folder" '(:all))))))


;;;; Sequences.

(deftest delete-messages ('(1 7) delete-messages-70)
  "Test `delete-messages', with a sequence string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:delete-messages "folder" '("cur"))
       (mh:pick-messages "folder" '(:all))))))


;;;; Errors.

(deftest delete-messages (t delete-messages-110)
  "Test `delete-messages' with a missing message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-messages "folder" '(1))
	   (error () (setq ret t)))
	 ret)))))

(deftest delete-messages (t delete-messages-111)
  "Test `delete-messages' with an erroneous symbol message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-messages "folder" '(:cur))
	   (error () (setq ret t))))))))

(deftest delete-messages (t delete-messages-112)
  "Test `delete-messages' with an erroneous sequence name message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-messages "folder" '("cur"))
	   (error () (setq ret t))))))))

(deftest delete-messages (t delete-messages-113)
  "Test `delete-messages' specifying a range with integers."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-messages "folder" '((2 . 2)))
	   (error () (setq ret t)))
	 ret)))))

(deftest delete-messages (t delete-messages-114)
  "Test `delete-messages' specifying a range with missing messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/9"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-messages "folder" '(("7" . "9")))
	   (error () (setq ret t)))
	 ret)))))

(deftest delete-messages (t delete-messages-115)
  "Test `delete-messages' specifying a range with junk in the first
   string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-messages "folder" '(("7." "8")))
	   (error () (setq ret t))))))))

(deftest delete-messages (t delete-messages-116)
  "Test `delete-messages' specifying a range with junk in the second
   string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-messages "folder" '(("7" "8-")))
	   (error () (setq ret t))))))))

(deftest delete-messages (t delete-messages-117)
  "Test `delete-messages', with :highest."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string delete-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-messages "folder" '(:highest))
	   (error () (setq ret t)))
	 ret)))))
