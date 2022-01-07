;;; Tests of mh:sequence-list.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))


;;;; Mail texts.

(defvar sequence-list-mail-1 "To: test@ttt.org
From: tester@ttt.org
Subject: test 1
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar sequence-list-mail-2 "To: test@ttt.org
From: tester@ttt.org
Subject: test 2
Date: 15 May 2008 16:02:10 +0100

Body.
")

(defvar sequence-list-mail-3 "To: test@ttt.org
From: tester@ttt.org
Subject: test 3
Date: 16 May 2008 16:02:10 +0100

Body.
")


;;;; Including the cache.

(deftest sequence-list ('((1 . 1) (2 . 2) (3 . 3) (4 . 4))
			sequence-list-1)
  "Test `sequence-list'."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "a: 1 2 3 4~%")
	(format stream "b: 1-7~%"))
      (mh:with-fresh-state
       (mh:sequence-list "folder" "a")))))

(deftest sequence-list ('((1 . 1) (2 . 4) (7 . 7))
			sequence-list-2)
  "Test `sequence-list', including a range, with a + on the folder name."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "a: 1 2 3 4~%")
	(format stream "b: 1 2-4 7~%"))
      (mh:with-fresh-state
       (mh:sequence-list "+folder" "b")))))

(deftest sequence-list ('((1 . 1) (2 . 4) (7 . 7))
			sequence-list-3)
  "Test `sequence-list', including a range, where the folder is cached
   already."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "a: 1 2 3 4~%")
	(format stream "b: 1 2-4 7~%"))
      (to-file (stream "home:Mail/folder/1" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (to-file (stream "home:Mail/folder/2" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (to-file (stream "home:Mail/folder/3" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (to-file (stream "home:Mail/folder/4" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (to-file (stream "home:Mail/folder/7" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '("b"))
       (mh:sequence-list "folder" "b")))))


;;; Argument "directly".

(deftest sequence-list ('((1 . 1) (2 . 2) (3 . 3) (4 . 4))
			sequence-list-20)
  "Test `sequence-list', directly, with a trailing slash on the folder
   name."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "a: 1 2 3 4~%")
	(format stream "b: 1-7~%"))
      (mh:with-fresh-state
       (mh:sequence-list "folder/" "a" :directly)))))

(deftest sequence-list ('((1 . 1) (2 . 4) (7 . 7))
			sequence-list-21)
  "Test `sequence-list', including a range, directly."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "a: 1 2 3 4~%")
	(format stream "b: 1 2-4 7~%"))
      (mh:with-fresh-state
       (mh:sequence-list "folder" "b" t)))))

(deftest sequence-list ('((1 . 1) (2 . 4) (7 . 7))
			sequence-list-22)
  "Test `sequence-list' directly, including a range, where the folder is
   cached already."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "a: 1 2 3 4~%")
	(format stream "b: 1 2-4 7~%"))
      (to-file (stream "home:Mail/folder/1" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (to-file (stream "home:Mail/folder/2" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (to-file (stream "home:Mail/folder/3" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (to-file (stream "home:Mail/folder/4" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (to-file (stream "home:Mail/folder/7" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '("b"))
       (mh:sequence-list "folder" "b" :direct)))))

(deftest sequence-list ('((4 . 4)) sequence-list-23)
  "Test `sequence-list' directly, trying to get the disk and cache
   sequences out of sync by setting `current-message'."
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
	(write-string sequence-list-mail-1 stream))
      (to-file (stream "home:Mail/folder/2" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (to-file (stream "home:Mail/folder/3" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (to-file (stream "home:Mail/folder/4" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (to-file (stream "home:Mail/folder/7" :if-does-not-exist :create)
	(write-string sequence-list-mail-1 stream))
      (mh:with-fresh-state
       ;; Force caching.
       (mh:pick-messages "folder" '("cur"))
       (setf (mh:current-message "folder") 4)
       (mh:sequence-list "folder" "cur" :direct)))))

(deftest sequence-list ('((1 . 1) (2 . 4) (7 . 7))
			sequence-list-24)
  "Test `sequence-list', including a range, with a weird sequence name."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "a: 1 2 3 4~%")
	(format stream "=%=: 1 2-4 7~%"))
      (mh:with-fresh-state
       (mh:sequence-list "folder" "=%=" t)))))


;;;; Second return.

(deftest sequence-list ('(() ()) sequence-list-30)
  "Test `sequence-list' with a missing sequence."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "a: 1 2 3 4~%")
	(format stream "cur: 1~%"))
      (mh:with-fresh-state
       (multiple-value-list
	(mh:sequence-list "folder" "zzz"))))))

(deftest sequence-list ('(() t) sequence-list-31)
  "Test `sequence-list' with an empty sequence."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "a: 1 2 3 4~%")
	(format stream "zzz:~%")
	(format stream "cur: 1~%"))
      (mh:with-fresh-state
       (multiple-value-list
	(mh:sequence-list "folder" "zzz"))))))


;;;; Errors.

(deftest sequence-list (t sequence-list-40)
  "Test `sequence-list' with a missing folder."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "a: 1 2 3 4~%")
	(format stream "cur: 1~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:sequence-list "folder2" "a")
	   (error () (setq ret t)))
	 ret)))))

(deftest sequence-list (t sequence-list-41)
  "Test `sequence-list', including a range, with a body boundary in the
   sequence name."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "a: 1 2 3 4~%")
	(format stream "--%--: 1 2-4 7~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:sequence-list "folder" "--%--" t)
	   (error () (setq ret t))))))))
