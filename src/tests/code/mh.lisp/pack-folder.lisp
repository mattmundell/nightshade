;;; Tests of mh:pack-folder.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest pack-folder (t pack-folder-1)
  "Test `pack-folder' on an empty folder."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (mh:pack-folder "folder")
       (and (in-directory dir
	      (equal (list-files "Mail/folder/")
		     (list "Mail/folder/.mh_sequences")))
	    (fi (mh:pick-messages "folder" '(:all) ())))))))

(defvar pack-folder-mail-1 "To: test@ttt.org
From: tester@ttt.org
Subject: test 1
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar pack-folder-mail-1b "To: test@ttt.org
From: tester@ttt.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar pack-folder-mail-2 "To: test@ttt.org
From: tester@ttt.org
Subject: test 2
Date: 15 May 2008 16:02:10 +0100

Body.
")

(defvar pack-folder-mail-3 "To: test@ttt.org
From: tester@ttt.org
Subject: test 3
Date: 16 May 2008 16:02:10 +0100

Body.
")

(deftest pack-folder (t pack-folder-2)
  "Test `pack-folder' on a single message number 1."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder")
       (and (in-directory dir
	      (equal (list-files "Mail/folder/")
		     (list "Mail/folder/1"
			   "Mail/folder/.mh_sequences")))
	    (equal (mh:pick-messages "folder" '(:all) ())
		   '(1)))))))

(deftest pack-folder (t pack-folder-3)
  "Test `pack-folder' on a single message above number 1."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/17"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder")
       (and (in-directory dir
	      (equal (list-files "Mail/folder/")
		     (list "Mail/folder/1"
			   "Mail/folder/.mh_sequences")))
	    (equal (mh:pick-messages "folder" '(:all) ())
		   '(1)))))))

(deftest pack-folder (t pack-folder-4)
  "Test `pack-folder' on two packed messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder")
       (and (in-directory dir
	      (equal (list-files "Mail/folder/")
		     (list "Mail/folder/1"
			   "Mail/folder/2"
			   "Mail/folder/.mh_sequences")))
	    (equal (mh:pick-messages "folder" '(:all) ())
		   '(1 2)))))))

(deftest pack-folder (t pack-folder-5)
  "Test `pack-folder' on two spread messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/17"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/") #'string<)
		     (list "Mail/folder/.mh_sequences"
			   "Mail/folder/1"
			   "Mail/folder/2")))
	    (equal (mh:pick-messages "folder" '(:all) ())
		   '(1 2))
	    (string= (mh:message-header "folder" 1 "Subject")
		     "test 2")
	    (string= (mh:message-header "folder" 2 "Subject")
		     "test 1"))))))

(deftest pack-folder (t pack-folder-6)
  "Test `pack-folder' on three out-of-order spread messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/20"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/21"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/") #'string<)
		     (list "Mail/folder/.mh_sequences"
			   "Mail/folder/1"
			   "Mail/folder/2"
			   "Mail/folder/3")))
	    (equal (mh:pick-messages "folder" '(:all) ())
		   '(1 2 3))
	    (string= (mh:message-header "folder" 1 "Subject")
		     "test 2")
	    (string= (mh:message-header "folder" 2 "Subject")
		     "test 3")
	    (string= (mh:message-header "folder" 3 "Subject")
		     "test 1"))))))

(deftest pack-folder (t pack-folder-7)
  "Test `pack-folder' on three ordered spread messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/") #'string<)
		     (list "Mail/folder/.mh_sequences"
			   "Mail/folder/1"
			   "Mail/folder/2"
			   "Mail/folder/3")))
	    (equal (mh:pick-messages "folder" '(:all) ())
		   '(1 2 3))
	    (string= (mh:message-header "folder" 1 "Subject")
		     "test 1")
	    (string= (mh:message-header "folder" 2 "Subject")
		     "test 2")
	    (string= (mh:message-header "folder" 3 "Subject")
		     "test 3"))))))

(deftest pack-folder (t pack-folder-8)
  "Test `pack-folder' on three ordered packed messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/") #'string<)
		     (list "Mail/folder/.mh_sequences"
			   "Mail/folder/1"
			   "Mail/folder/2"
			   "Mail/folder/3")))
	    (equal (mh:pick-messages "folder" '(:all) ())
		   '(1 2 3))
	    (string= (mh:message-header "folder" 1 "Subject")
		     "test 1")
	    (string= (mh:message-header "folder" 2 "Subject")
		     "test 2")
	    (string= (mh:message-header "folder" 3 "Subject")
		     "test 3"))))))

(deftest pack-folder (t pack-folder-9)
  "Test `pack-folder' with a leading + on the folder name."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/111"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/222"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/333"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (mh:with-fresh-state
       (mh:pack-folder "+folder")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/") #'string<)
		     (list "Mail/folder/.mh_sequences"
			   "Mail/folder/1"
			   "Mail/folder/2"
			   "Mail/folder/3")))
	    (equal (mh:pick-messages "folder" '(:all) ())
		   '(1 2 3))
	    (string= (mh:message-header "folder" 1 "Subject")
		     "test 2")
	    (string= (mh:message-header "folder" 2 "Subject")
		     "test 3")
	    (string= (mh:message-header "folder" 3 "Subject")
		     "test 1"))))))

(deftest pack-folder (t pack-folder-10)
  "Test `pack-folder', packing one of many folders."
  (with-test-dir (dir "Mail/folder/" "Mail/folder2/" "Mail/folder3/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder2/3"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder3/2"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder3/7"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/") #'string<)
		     (list "Mail/folder/.mh_sequences"
			   "Mail/folder/1"
			   "Mail/folder/2"
			   "Mail/folder/3")))
	    (equal (mh:pick-messages "folder" '(:all) ())
		   '(1 2 3))
	    (string= (mh:message-header "folder" 1 "Subject")
		     "test 2")
	    (string= (mh:message-header "folder" 2 "Subject")
		     "test 1")
	    (string= (mh:message-header "folder" 3 "Subject")
		     "test 3"))))))

(deftest pack-folder (t pack-folder-11)
  "Test `pack-folder' on three packed messages in a lone subfolder."
  (with-test-dir (dir "Mail/folder/subfolder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/subfolder/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder/2"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/3"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder/subfolder")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/subfolder/") #'string<)
		     (list "Mail/folder/subfolder/.mh_sequences"
			   "Mail/folder/subfolder/1"
			   "Mail/folder/subfolder/2"
			   "Mail/folder/subfolder/3")))
	    (equal (mh:pick-messages "folder/subfolder" '(:all) ())
		   '(1 2 3))
	    (string= (mh:message-header "folder/subfolder" 1 "Subject")
		     "test 1")
	    (string= (mh:message-header "folder/subfolder" 2 "Subject")
		     "test 2")
	    (string= (mh:message-header "folder/subfolder" 3 "Subject")
		     "test 3"))))))

(deftest pack-folder (t pack-folder-12)
  "Test `pack-folder' on three packed messages in a lone subfolder, with a
   leading + on the folder name."
  (with-test-dir (dir "Mail/folder/subfolder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/subfolder/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder/2"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/3"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (mh:with-fresh-state
       (mh:pack-folder "+folder/subfolder")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/subfolder/") #'string<)
		     (list "Mail/folder/subfolder/.mh_sequences"
			   "Mail/folder/subfolder/1"
			   "Mail/folder/subfolder/2"
			   "Mail/folder/subfolder/3")))
	    (equal (mh:pick-messages "folder/subfolder" '(:all) ())
		   '(1 2 3))
	    (string= (mh:message-header "folder/subfolder" 1 "Subject")
		     "test 1")
	    (string= (mh:message-header "folder/subfolder" 2 "Subject")
		     "test 2")
	    (string= (mh:message-header "folder/subfolder" 3 "Subject")
		     "test 3"))))))

(deftest pack-folder (t pack-folder-13)
  "Test `pack-folder' on three spread messages in one of many
   subfolders."
  (with-test-dir (dir "Mail/folder/subfolder/"
		      "Mail/folder/subfolder1/"
		      "Mail/folder/subfolder2/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/subfolder/8"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/11"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder/13"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder1/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder2/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder2/2"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder/subfolder")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/subfolder/") #'string<)
		     (list "Mail/folder/subfolder/.mh_sequences"
			   "Mail/folder/subfolder/1"
			   "Mail/folder/subfolder/2"
			   "Mail/folder/subfolder/3")))
	    (equal (mh:pick-messages "folder/subfolder" '(:all) ())
		   '(1 2 3))
	    (string= (mh:message-header "folder/subfolder" 1 "Subject")
		     "test 2")
	    (string= (mh:message-header "folder/subfolder" 2 "Subject")
		     "test 3")
	    (string= (mh:message-header "folder/subfolder" 3 "Subject")
		     "test 1"))))))

(deftest pack-folder (t pack-folder-14)
  "Test `pack-folder' on three spread messages in one of many subfolders,
   with a trailing slash on the folder name."
  (with-test-dir (dir "Mail/folder/subfolder/"
		      "Mail/folder/subfolder1/"
		      "Mail/folder/subfolder2/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/subfolder/8"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/11"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder/13"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder1/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder2/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder2/2"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder/subfolder/")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/subfolder/") #'string<)
		     (list "Mail/folder/subfolder/.mh_sequences"
			   "Mail/folder/subfolder/1"
			   "Mail/folder/subfolder/2"
			   "Mail/folder/subfolder/3")))
	    (equal (mh:pick-messages "folder/subfolder" '(:all) ())
		   '(1 2 3))
	    (string= (mh:message-header "folder/subfolder" 1 "Subject")
		     "test 2")
	    (string= (mh:message-header "folder/subfolder" 2 "Subject")
		     "test 3")
	    (string= (mh:message-header "folder/subfolder" 3 "Subject")
		     "test 1"))))))

(deftest pack-folder (t pack-folder-15)
  "Test `pack-folder' on two spread messages, with extra files in the
   folder."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/2000"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/") #'string<)
		     (list "Mail/folder/,1"
			   "Mail/folder/.mh_sequences"
			   "Mail/folder/1"
			   "Mail/folder/2"
			   "Mail/folder/file")))
	    (equal (mh:pick-messages "folder" '(:all) ())
		   '(1 2))
	    (string= (mh:message-header "folder" 1 "Subject")
		     "test 2")
	    (string= (mh:message-header "folder" 2 "Subject")
		     "test 1"))))))

(deftest pack-folder (t pack-folder-16)
  "Test `pack-folder' on an empty folder with extra files."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/,1"
		      "Mail/folder/xxx" "Mail/folder/1.BAK")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (mh:pack-folder "folder")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/") #'string<)
		     (list "Mail/folder/,1"
			   "Mail/folder/.mh_sequences"
			   "Mail/folder/1.BAK"
			   "Mail/folder/xxx")))
	    (fi (mh:pick-messages "folder" '(:all) ())))))))

(deftest pack-folder (t pack-folder-17)
  "Test `pack-folder' on two messages with the exact same date."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/24"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1b stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/") #'string<)
		     (list "Mail/folder/.mh_sequences"
			   "Mail/folder/1"
			   "Mail/folder/2")))
	    (equal (mh:pick-messages "folder" '(:all) ())
		   '(1 2))
	    (string= (mh:message-header "folder" 1 "Subject")
		     "test 1")
	    (string= (mh:message-header "folder" 2 "Subject")
		     "test 1b"))))))

(deftest pack-folder (t pack-folder-18)
  "Test `pack-folder' on three out of order messages in one of many
   subfolders, where two of the messages have the exact same date."
  (with-test-dir (dir "Mail/folder/subfolder/"
		      "Mail/folder/subfolder1/"
		      "Mail/folder/subfolder2/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/subfolder/8"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/11"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1b stream))
      (to-file (stream "home:Mail/folder/subfolder/13"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder1/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder2/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder2/2"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (mh:with-fresh-state
       (mh:pack-folder "folder/subfolder/")
       (and (in-directory dir
	      (equal (sort (list-files "Mail/folder/subfolder/") #'string<)
		     (list "Mail/folder/subfolder/.mh_sequences"
			   "Mail/folder/subfolder/1"
			   "Mail/folder/subfolder/2"
			   "Mail/folder/subfolder/3")))
	    (equal (mh:pick-messages "folder/subfolder" '(:all) ())
		   '(1 2 3))
	    (string= (mh:message-header "folder/subfolder" 1 "Subject")
		     "test 2")
	    (string= (mh:message-header "folder/subfolder" 2 "Subject")
		     "test 1b")
	    (string= (mh:message-header "folder/subfolder" 3 "Subject")
		     "test 1"))))))


;;;; Errors.

;; TODO Test recovery when packing fails half way.  At least the cache of
;; the folder should be cleared.

(deftest pack-folder (t pack-folder-40)
  "Test `pack-folder' with an empty argument."
  (let (ret)
    (handler-case
	(mh:pack-folder ())
      (error () (setq ret t)))
    ret))

(deftest pack-folder (t pack-folder-41)
  "Test `pack-folder' on a missing folder."
  (with-test-dir (dir "Mail/" )
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:pack-folder "folder")
	   (error () (setq ret t)))
	 ret)))))

(deftest pack-folder (t pack-folder-42)
  "Test `pack-folder' on a missing subfolder."
  (with-test-dir (dir "Mail/folder/" )
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:pack-folder "folder/subfolder")
	   (error () (setq ret t)))
	 ret)))))

(deftest pack-folder (t pack-folder-43)
  "Test `pack-folder' where the folder directory has been write-blocked."
  (with-test-dir (dir "Mail/folder/three/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/three/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/three/3"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (mh:with-fresh-state
       (setf (file-mode (mh:folder-pathname "folder/three")) "a-rwx")
       (let (ret)
	 (handler-case
	     (mh:pack-folder "folder/three")
	   (error () (setq ret t)))
	 (setf (file-mode (mh:folder-pathname "folder/three")) "a+rwx")
	 ret)))))

(deftest pack-folder (t pack-folder-44)
  "Test `pack-folder', attempting to pack the root of the mail directory."
  (with-test-dir (dir "Mail/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/1"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-2 stream))
      (to-file (stream "home:Mail/2"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-3 stream))
      (to-file (stream "home:Mail/3"
		       :if-does-not-exist :create)
	(write-string pack-folder-mail-1 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:pack-folder "")
	   (error () (setq ret t)))
	 ret)))))
