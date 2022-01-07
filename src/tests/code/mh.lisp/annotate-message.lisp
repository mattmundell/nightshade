;;; Tests of mh:annotate-message.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar annotate-message-mail-1 "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(deftest annotate-message ("" annotate-message-1)
  "Test `annotate-message' with an empty component."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string annotate-message-mail-1 stream))
      (mh:with-fresh-state
       (mh:annotate-message "folder" 1 "test" :datep ())
       (mh:message-header "folder" 1 "Test")))))

(deftest annotate-message (t annotate-message-2)
  "Test `annotate-message' with a date component and a leading + on the
   folder name."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string annotate-message-mail-1 stream))
      (mh:with-fresh-state
       (mh:annotate-message "+folder" 1 "test")
       (if (parse-time (mh:message-header "folder" 1 "Test")) t)))))

(deftest annotate-message ("arbitrary text" annotate-message-3)
  "Test `annotate-message' with a text component and a trailing slash on
   the folder name."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string annotate-message-mail-1 stream))
      (mh:with-fresh-state
       (mh:annotate-message "folder/" 1 "test"
			    :datep ()
			    :text "arbitrary text")
       (mh:message-header "folder" 1 "Test")))))

(deftest annotate-message ('("arbitrary text") annotate-message-4)
  "Test `annotate-message' with date and text components."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string annotate-message-mail-1 stream))
      (mh:with-fresh-state
       (mh:annotate-message "folder" 1 "test"
			    :text "arbitrary text")
       (multiple-value-bind (first rest)
			    (mh:message-header "folder" 1 "Test")
	 (and (parse-time first) rest))))))

(deftest annotate-message ('("arbitrary text") annotate-message-5)
  "Test `annotate-message' with an existing components."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string annotate-message-mail-1 stream))
      (mh:with-fresh-state
       (mh:annotate-message "folder" 1 "test"
			    :datep ()
			    :text "arbitrary text")
       (mh:annotate-message "folder" 1 "test")
       (multiple-value-bind (first rest)
			    (mh:message-header "folder" 1 "Test")
	 (and (parse-time first) rest))))))


;;;; Weird component names.

(deftest annotate-message ("arbitrary text" annotate-message-10)
  "Test `annotate-message' with a weird component name."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string annotate-message-mail-1 stream))
      (mh:with-fresh-state
       (mh:annotate-message "folder" 1 "=="
			    :datep ()
			    :text "arbitrary text")
       (mh:message-header "folder" 1 "==")))))

(deftest annotate-message (() annotate-message-11)
  "Test `annotate-message' with the message separator as component name."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string annotate-message-mail-1 stream))
      (mh:with-fresh-state
       (mh:annotate-message "folder" 1 "--"
			    :datep ()
			    :text "arbitrary text")
       (mh:message-header "folder" 1 "--")))))

(deftest annotate-message (t annotate-message-12)
  "Test `annotate-message' with a colon as component name."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string annotate-message-mail-1 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:annotate-message "folder" 1 ":"
				  :datep ()
				  :text "arbitrary text")
	   (error () (setq ret t))))))))


;;;; Errors.

(deftest annotate-message (t annotate-message-20)
  "Test `annotate-message' with a missing message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:annotate-message "folder" "1" "test")
	   (error () (setq ret t))))))))

(deftest annotate-message (t annotate-message-21)
  "Test `annotate-message' with too few arguments."
  (let (ret)
    (handler-case
	(mh:annotate-message "folder" 1)
      (error () (setq ret t)))
    ret))
