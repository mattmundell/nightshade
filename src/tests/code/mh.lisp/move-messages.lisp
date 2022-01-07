;;; Tests of mh:move-messages.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar move-messages-mail-1 "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar move-messages-mail-2 "To: test2@two.org.za
From: short@name.org
Subject: subject
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")

(defvar move-messages-mail-3 "To: three@three.org
From: short@name.org
Subject: test 3
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")


;;;; Single message.

(deftest move-messages ('(() (1)) move-messages-1)
  "Test `move-messages' moving a lone message to an empty folder."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest" '(1))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "dest" '(:all)))))))

(deftest move-messages ('((22) (1 22) (1)) move-messages-2)
  "Test `move-messages' moving one message from many to an empty folder,
   with +s on the folder names."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (mh:with-fresh-state
       (mh:move-messages "+folder" "+dest" '(12))
       (list (mh:pick-messages "folder" '("highest"))
	     (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "dest" '(:all)))))))

(deftest move-messages ('((1 22) (22) 22  (22 23 24) (24) 24)
			move-messages-3)
  "Test `move-messages' moving one message from many to a folder of many,
   with a string message arg and a trailing slash on the folder names,
   clearing the folder cache before picking messages."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/dest/22"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/dest/23"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder/" "dest/" '("12"))
       (mh:update-folder-table)
       (list (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("highest"))
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest"))
	     (mh:current-message "dest"))))))

(deftest move-messages ('(1 12) move-messages-4)
  "Test `move-messages' with a symbol message arg."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:move-messages "folder" "dest" '(:highest))
	   (error () (setq ret '(1 12))))
	 ;(mh:pick-messages "folder" '(:all))
	 ret)))))

(deftest move-messages ('((1 12) (12) (1) (1)) move-messages-5)
  "Test `move-messages', moving a single messages to an empty folder with a
   sequence name message arg, clearing the cache before picking the
   messages."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder" "+dest" '("highest"))
       (mh:update-folder-table)
       (list (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("highest"))
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest")))))))


;;;; Multiple messages.

(deftest move-messages ('((1 2) (2) 2 (1 2) (2)) move-messages-20)
  "Test `move-messages', moving multiple messages to an empty folder,
   ensuring the current message stays in tact."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (mh:with-fresh-state
       (setf (mh:current-message "folder") 2)
       (mh:move-messages "folder" "+dest" '(12 22))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("highest"))
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest")))))))

(deftest move-messages ('((3) (3) 3  (12 22 23 24) (24))
			move-messages-21)
  "Test `move-messages' moving multiple messages to a folder of many."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/dest/12"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/dest/22"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (mh:with-fresh-state
       (setf (mh:current-message "folder") 12)
       (mh:move-messages "folder/" "dest" '(12 22))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("highest"))
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest")))))))

(deftest move-messages ('(() (1 22) (22) (22) (1 22))
			move-messages-22)
  "Test `move-messages' moving a single message, checking that sequences
   are adjusted."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "aaa: 12~%")
	(format stream "bbb: 1 22~%")
	(format stream "ccc: 12 22~%")
	(format stream "ddd: 12 22 12~%")
	(format stream "abc: 1 12 22~%"))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest" '(12))
       (list (mh:pick-messages "folder" '("aaa"))
	     (mh:pick-messages "folder" '("bbb"))
	     (mh:pick-messages "folder" '("ccc"))
	     (mh:pick-messages "folder" '("ddd"))
	     (mh:pick-messages "folder" '("abc")))))))

(deftest move-messages ('(() () 0 (1 2) (2) 2)
			move-messages-23)
  "Test `move-messages', specifying all messages explicitly."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest" '(1 "2"))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:sequence-list "folder" "highest")
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest"))
	     (mh:current-message "dest"))))))

(deftest move-messages ('((3) (3) 3  (1 2) (2) 2)
			move-messages-24)
  "Test `move-messages', specifying some of the messages."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest/" '(1 2))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("highest"))
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest"))
	     (mh:current-message "dest"))))))

(deftest move-messages ('((1 3) (3) 3  (1) (1) 1)
			move-messages-25)
  "Test `move-messages', specifying a range of one."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest" '(("2" . "2")))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("highest"))
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest"))
	     (mh:current-message "dest"))))))

(deftest move-messages ('((1) (1) 1  (18 19 20) (20) 20)
			move-messages-26)
  "Test `move-messages', specifying a range of many."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/dest/18"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest" '(("7" . "8")))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("highest"))
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest"))
	     (mh:current-message "dest"))))))

(deftest move-messages ('((1) (1) 1  (1 2 3) (3) 3)
			move-messages-27)
  "Test `move-messages', specifying a range of many, backwards."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/dest/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest" '(("8" . "7")))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("highest"))
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest"))
	     (mh:current-message "dest"))))))

(deftest move-messages ('((1 7) (7) 7  (1) (1) 1)
			move-messages-28)
  "Test `move-messages', specifying a string range of one."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest" '("8-8"))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("highest"))
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest"))
	     (mh:current-message "dest"))))))

(deftest move-messages ('((1) (1) 1  (1 2) (2) 2)
			move-messages-29)
  "Test `move-messages', specifying a string range of many."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest" '("7-8"))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("highest"))
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest"))
	     (mh:current-message "dest"))))))

(deftest move-messages ('(() (1) () () (1))
			move-messages-30)
  "Test `move-messages' moving multiple messages, checking that sequences
   are adjusted."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "aaa: 12~%")
	(format stream "bbb: 1 22~%")
	(format stream "ccc: 12 22~%")
	(format stream "ddd: 12 22 12~%")
	(format stream "abc: 1 12 22~%"))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest" '(12 22))
       (list (mh:pick-messages "folder" '("aaa"))
	     (mh:pick-messages "folder" '("bbb"))
	     (mh:pick-messages "folder" '("ccc"))
	     (mh:pick-messages "folder" '("ddd"))
	     (mh:pick-messages "folder" '("abc")))))))


;;;; All messages.

(deftest move-messages ('(() () 0  (1 2 3) (3) 3  t)
			move-messages-50)
  "Test `move-messages', specifying the all string."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-3 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest" '("all"))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:sequence-list "folder" "highest")
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest"))
	     (mh:current-message "dest")
	     (string= (mh:message-header "dest" 3 "Subject")
		      "test 3"))))))

(deftest move-messages ('(() () 0  (1 2 3) (3) 3  t)
			move-messages-51)
  "Test `move-messages', specifying the all symbol."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-3 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest" '(:all))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:sequence-list "folder" "highest")
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest"))
	     (mh:current-message "dest")
	     (string= (mh:message-header "dest" 2 "Subject")
		      "subject"))))))


;;;; Sequences.

(deftest move-messages ('((1 7) (7) 7  (1) (1) 1)
			move-messages-70)
  "Test `move-messages', with a sequence string."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:move-messages "folder" "dest" '("cur"))
       (list (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("highest"))
	     (mh:current-message "folder")
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "dest" '("highest"))
	     (mh:current-message "dest"))))))


;;;; Errors.

(deftest move-messages (t move-messages-110)
  "Test `move-messages' with a missing message."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:move-messages "folder" "dest" '(1))
	   (error () (setq ret t)))
	 ret)))))

(deftest move-messages (t move-messages-111)
  "Test `move-messages' with an erroneous symbol message."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:move-messages "folder" "dest" '(:cur))
	   (error () (setq ret t))))))))

(deftest move-messages (t move-messages-113)
  "Test `move-messages' specifying a range with integers."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:move-messages "folder" "dest" '((2 . 2)))
	   (error () (setq ret t)))
	 ret)))))

(deftest move-messages (t move-messages-114)
  "Test `move-messages' specifying a range with missing messages."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/9"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:move-messages "folder" "dest" '(("7" . "9")))
	   (error () (setq ret t)))
	 ret)))))

(deftest move-messages (t move-messages-115)
  "Test `move-messages' specifying a range with junk in the first
   string."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:move-messages "folder" "dest" '(("7." "8")))
	   (error () (setq ret t))))))))

(deftest move-messages (t move-messages-116)
  "Test `move-messages' specifying a range with junk in the second
   string."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:move-messages "folder" "dest" '(("7" "8-")))
	   (error () (setq ret t))))))))

(deftest move-messages (t move-messages-117)
  "Test `move-messages', with :highest."
  (with-test-dir (folder "Mail/folder/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/8"
		       :if-does-not-exist :create)
	(write-string move-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:move-messages "folder" "dest" '(:highest))
	   (error () (setq ret t)))
	 ret)))))
