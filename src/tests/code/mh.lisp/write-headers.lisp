;;; Tests of mh:write-headers.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar write-headers-mail-1 "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100

Mail 1.
")

(defvar write-headers-mail-2 "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.

More body.
")


;;;; All headers.

(deftest write-headers ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
"
			write-headers-1)
  "Test `write-headers', implying all."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:write-headers "folder" 1 stream))))))

(deftest write-headers ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
"
			write-headers-2)
  "Test `write-headers', requesting all, with a trailing slash on the
   folder name and a string message spec."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:write-headers "folder/" "1" stream t))))))


;;;; Some headers.

(deftest write-headers ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
"
			write-headers-3)
  "Test `write-headers', requesting every header explicitly, with a + on
   the folder name."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:write-headers "+folder" "1" stream
			   '("To" "From" "Subject" "Date")))))))

(deftest write-headers ("To: test@ttt.org
Subject: ss1
"
			write-headers-4)
  "Test `write-headers', requesting some headers, specifying the highest
   message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:write-headers "folder" :highest stream
			   '("To" "Subject")))))))

(deftest write-headers ("To: test@ttt.org
"
			write-headers-5)
  "Test `write-headers', requesting one header."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:write-headers "folder" :highest stream
			   '("To")))))))

#|
(deftest write-headers ("Body.

More body.
"
			write-headers-6)
  "Test `write-headers', requesting the body."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/222"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-2 stream))
      (to-file (stream "home:Mail/folder/225"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:write-headers "folder" 222 stream '(:body)))))))
|#

(deftest write-headers (""
			write-headers-7)
  "Test `write-headers', requesting a missing header."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/222"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-2 stream))
      (to-file (stream "home:Mail/folder/225"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:write-headers "folder" :highest stream '("Xxx")))))))

(deftest write-headers ("To: test@ttt.org
From: nn <n@n.org>
"
			write-headers-8)
  "Test `write-headers', requesting a missing header amongst others."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/222"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-2 stream))
      (to-file (stream "home:Mail/folder/225"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:write-headers "folder" :highest stream '("To" "Xxx" "From")))))))


;;;; Errors.

(deftest write-headers (t write-headers-50)
  "Test `write-headers' requesting all messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-headers "folder/" :all stream))
	   (error () (setq ret t))))))))

(deftest write-headers (t write-headers-51)
  "Test `write-headers' requesting multiple messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-headers "folder/" '(3 11) stream))
	   (error () (setq ret t))))))))

(deftest write-headers (t write-headers-52)
  "Test `write-headers' requesting a sequence as multiple messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-headers "folder/" '("trash") stream))
	   (error () (setq ret t))))))))

(deftest write-headers (t write-headers-53)
  "Test `write-headers' requesting a sequence."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string write-headers-mail-1 stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-headers "folder/" "trash" stream))
	   (error () (setq ret t))))))))

(deftest write-headers (t write-headers-54)
  "Test `write-headers' with an erroneous symbol message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-headers "folder" :cur stream))
	   (error () (setq ret t))))))))

(deftest write-headers (t write-headers-55)
  "Test `write-headers' with all string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-headers "folder" "all" stream))
	   (error () (setq ret t))))))))
