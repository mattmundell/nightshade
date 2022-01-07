;;; Tests of mh:summarize-folder.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

; FIX weird folder names 3 ,foo

(deftest summarize-folder ("" summarize-folder-1)
  "Test `summarize-folder' on an empty folder."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder" stream))))))

(defvar summarize-folder-mail-1 "To: test@ttt.org
From: tester@ttt.org
Subject: test 1
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar summarize-folder-mail-1b "To: test@ttt.org
From: tester@ttt.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar summarize-folder-mail-2 "To: test@ttt.org
From: tester@ttt.org
Subject: test 2
Date: 15 May 2008 16:02:10 +0100

Body.
")

(defvar summarize-folder-mail-3 "To: test@ttt.org
From: tester@ttt.org
Subject: test 3
Date: 16 May 2008 16:02:10 +0100

Body.
")

(deftest summarize-folder ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			   summarize-folder-2)
  "Test `summarize-folder' on a single message numbered 1."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder" stream))))))

(deftest summarize-folder ("  17   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			   summarize-folder-3)
  "Test `summarize-folder' on a single message above number 1."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/17"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder" stream))))))

(deftest summarize-folder ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
"
			   summarize-folder-4)
  "Test `summarize-folder' on two packed messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder" stream))))))

(deftest summarize-folder ("  17   15-May     94 tester@ttt.org    test 2                                 Bod
  22   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			   summarize-folder-5)
  "Test `summarize-folder' on two spread messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/17"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder" stream))))))

(deftest summarize-folder ("   1   15-May     94 tester@ttt.org    test 2                                 Bod
  20   16-May     94 tester@ttt.org    test 3                                 Bod
  21   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			   summarize-folder-6)
  "Test `summarize-folder' on three out-of-order spread messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/20"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/21"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder" stream))))))

(deftest summarize-folder ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
   3   15-May     94 tester@ttt.org    test 2                                 Bod
  13   16-May     94 tester@ttt.org    test 3                                 Bod
"
			   summarize-folder-7)
  "Test `summarize-folder' on three ordered spread messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder" stream))))))

(deftest summarize-folder ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   16-May     94 tester@ttt.org    test 3                                 Bod
"
			   summarize-folder-8)
  "Test `summarize-folder' on three ordered packed messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder" stream))))))

(deftest summarize-folder (" 111   15-May     94 tester@ttt.org    test 2                                 Bod
 222   16-May     94 tester@ttt.org    test 3                                 Bod
 333   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			   summarize-folder-9)
  "Test `summarize-folder' with a leading + on the folder name."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/111"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/222"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/333"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "+folder" stream))))))

(deftest summarize-folder ("   1   15-May     94 tester@ttt.org    test 2                                 Bod
   3   15-Apr     94 tester@ttt.org    test 1                                 Bod
  12   16-May     94 tester@ttt.org    test 3                                 Bod
"
			   summarize-folder-10)
  "Test `summarize-folder', summarizing one of many folders."
  (with-test-dir (dir "Mail/folder/" "Mail/folder2/" "Mail/folder3/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder2/3"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder3/2"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder3/7"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder" stream))))))

(deftest summarize-folder ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   16-May     94 tester@ttt.org    test 3                                 Bod
"
			   summarize-folder-11)
  "Test `summarize-folder' on three packed messages in a lone subfolder."
  (with-test-dir (dir "Mail/folder/subfolder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/subfolder/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder/2"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/3"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder/subfolder" stream))))))

(deftest summarize-folder ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   16-May     94 tester@ttt.org    test 3                                 Bod
"
			   summarize-folder-12)
  "Test `summarize-folder' on three packed messages in a lone subfolder, with a
   leading + on the folder name."
  (with-test-dir (dir "Mail/folder/subfolder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/subfolder/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder/2"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/3"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "+folder/subfolder" stream))))))

(deftest summarize-folder ("   8   15-May     94 tester@ttt.org    test 2                                 Body.
  11   16-May     94 tester@ttt.org    test 3                                 Body.
  13   15-Apr     94 tester@ttt.org    test 1                                 Body.
"
			   summarize-folder-13)
  "Test `summarize-folder' on three spread messages in one of many
   subfolders, passing the width argument."
  (with-test-dir (dir "Mail/folder/subfolder/"
		      "Mail/folder/subfolder1/"
		      "Mail/folder/subfolder2/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/subfolder/8"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/11"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder/13"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder1/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder2/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder2/2"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder/subfolder" stream 83))))))

(deftest summarize-folder ("   8   15-May     94 tester@ttt.org    test 2                                 Bod
  11   16-May     94 tester@ttt.org    test 3                                 Bod
  13   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			   summarize-folder-14)
  "Test `summarize-folder' on three spread messages in one of many
   subfolders, with a trailing slash on the folder name."
  (with-test-dir (dir "Mail/folder/subfolder/"
		      "Mail/folder/subfolder1/"
		      "Mail/folder/subfolder2/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/subfolder/8"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/11"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder/13"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder1/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder2/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder2/2"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder/subfolder/" stream))))))

(deftest summarize-folder ("  11   15-May     94 tester@ttt.org    test 2                                 Bod
2000   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			   summarize-folder-15)
  "Test `summarize-folder' on two spread messages, with extra files in the
   folder."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/2000"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder" stream))))))

(deftest summarize-folder ("" summarize-folder-16)
  "Test `summarize-folder' on an empty folder with extra files."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/,1"
		      "Mail/folder/xxx" "Mail/folder/1.BAK")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder" stream))))))

(deftest summarize-folder ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
  24   15-Apr     95 tester@ttt.org    test 1b                                Bod
"
			   summarize-folder-17)
  "Test `summarize-folder' on two messages with the exact same date."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/24"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1b stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder" stream))))))

(deftest summarize-folder ("   8   15-May     94 tester@ttt.org    test 2                                 Bo
  11   15-Apr     95 tester@ttt.org    test 1b                                Bo
  13   15-Apr     94 tester@ttt.org    test 1                                 Bo
"
			   summarize-folder-18)
  "Test `summarize-folder' on three out of order messages in one of many
   subfolders, where two of the messages have the exact same date, giving a
   width argument."
  (with-test-dir (dir "Mail/folder/subfolder/"
		      "Mail/folder/subfolder1/"
		      "Mail/folder/subfolder2/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/subfolder/8"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/11"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1b stream))
      (to-file (stream "home:Mail/folder/subfolder/13"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder1/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder2/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder2/2"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-folder "folder/subfolder" stream 79))))))


;;;; Errors.

(deftest summarize-folder (t summarize-folder-40)
  "Test `summarize-folder' with an empty folder name argument."
  (let (ret)
    (handler-case
	(with-output-to-string (stream)
	  (mh:summarize-folder () stream))
      (error () (setq ret t)))
    ret))

(deftest summarize-folder (t summarize-folder-41)
  "Test `summarize-folder' on a missing folder."
  (with-test-dir (dir "Mail/" )
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:summarize-folder "folder" stream))
	   (error () (setq ret t)))
	 ret)))))

(deftest summarize-folder (t summarize-folder-42)
  "Test `summarize-folder' on a missing subfolder."
  (with-test-dir (dir "Mail/folder/" )
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:summarize-folder "folder/subfolder" stream))
	   (error () (setq ret t)))
	 ret)))))

(deftest summarize-folder (t summarize-folder-43)
  "Test `summarize-folder' where the folder directory has been write-blocked."
  (with-test-dir (dir "Mail/folder/three/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/three/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/folder/three/3"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (mh:with-fresh-state
       (setf (file-mode (mh:folder-pathname "folder/three")) "a-rwx")
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:summarize-folder "folder/three" stream))
	   (error () (setq ret t)))
	 (setf (file-mode (mh:folder-pathname "folder/three")) "a+rwx")
	 ret)))))

(deftest summarize-folder (t summarize-folder-44)
  "Test `summarize-folder', attempting to summarize the root of the mail
   directory."
  (with-test-dir (dir "Mail/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/1"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-2 stream))
      (to-file (stream "home:Mail/2"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-3 stream))
      (to-file (stream "home:Mail/3"
		       :if-does-not-exist :create)
	(write-string summarize-folder-mail-1 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:summarize-folder "" stream))
	   (error () (setq ret t)))
	 ret)))))
