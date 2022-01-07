;;; Tests of mh:delete-message.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar mail-1 "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(deftest delete-message (() delete-message-1)
  "Test `delete-message' with a lone message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (mh:with-fresh-state
       (mh:delete-message "folder" 1)
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-message ('((22) (1 22)) delete-message-2)
  "Test `delete-message' with multiple messages and a + on the folder
   name."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (mh:with-fresh-state
       (mh:delete-message "+folder" 12)
       (list (mh:pick-messages "folder" '("highest"))
	     (mh:pick-messages "folder" '(:all)))))))

(deftest delete-message ('(1 22) delete-message-3)
  "Test `delete-message' with multiple messages, a string message arg and a
   trailing slash on the folder name, clearing the folder cache before
   picking messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (mh:with-fresh-state
       (mh:delete-message "folder/" "12")
       (mh:update-folder-table)
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-message ('(1 12) delete-message-4)
  "Test `delete-message' with multiple messages and a symbol message arg."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (mh:with-fresh-state
       (mh:delete-message "folder/" :highest)
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-message ('(1 12) delete-message-5)
  "Test `delete-message' with multiple messages and a sequence name message
   arg, clearing the cache before picking the messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (mh:with-fresh-state
       (mh:delete-message "folder/" "highest")
       (mh:update-folder-table)
       (mh:pick-messages "folder" '(:all))))))

(deftest delete-message (1 delete-message-6)
  "Test `delete-message' with multiple messages, ensuring the current
   message stays in tact."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (mh:with-fresh-state
       (setf (mh:current-message "folder") 1)
       (mh:delete-message "+folder" 12)
       (mh:current-message "folder")))))

(deftest delete-message (22 delete-message-7)
  "Test `delete-message' with multiple messages, ensuring the current
   message is accomodated."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (mh:with-fresh-state
       (setf (mh:current-message "folder") 12)
       (mh:delete-message "+folder" 12)
       (mh:current-message "folder")))))

(deftest delete-message ('(() (1 22) (22) (22) (1 22))
			 delete-message-8)
  "Test `delete-message' with multiple messages, checking that sequences
   are adjusted."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "aaa: 12~%")
	(format stream "bbb: 1 22~%")
	(format stream "ccc: 12 22~%")
	(format stream "ddd: 12 22 12~%")
	(format stream "abc: 1 12 22~%"))
      (mh:with-fresh-state
       (mh:delete-message "folder" 12)
       (list (mh:pick-messages "folder" '("aaa"))
	     (mh:pick-messages "folder" '("bbb"))
	     (mh:pick-messages "folder" '("ccc"))
	     (mh:pick-messages "folder" '("ddd"))
	     (mh:pick-messages "folder" '("abc")))))))


;;;; Errors.

(deftest delete-message (t delete-message-50)
  "Test `delete-message' with a missing message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-message "folder" 1)
	   (error () (setq ret t))))))))

(deftest delete-message (t delete-message-51)
  "Test `delete-message' with an erroneous symbol message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-message "folder" :cur)
	   (error () (setq ret t))))))))

(deftest delete-message (t delete-message-52)
  "Test `delete-message' with an erroneous sequence name message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-message "folder" "cur")
	   (error () (setq ret t))))))))

(deftest delete-message (t delete-message-53)
  "Test `delete-message' with all string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-message "folder" "all")
	   (error () (setq ret t))))))))

(deftest delete-message (t delete-message-54)
  "Test `delete-message' with all symbol."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:delete-message "folder" :all)
	   (error () (setq ret t))))))))
