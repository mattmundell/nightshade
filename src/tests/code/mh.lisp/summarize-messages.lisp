;;; Tests of mh:summarize-messages.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

; FIX weird folder names 3 ,foo

(defvar summarize-messages-mail-1 "To: test@ttt.org
From: tester@ttt.org
Subject: test 1
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar summarize-messages-mail-1b "To: test@ttt.org
From: tester@ttt.org
Subject: test 1b
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar summarize-messages-mail-2 "To: test@ttt.org
From: tester@ttt.org
Subject: test 2
Date: 15 May 2008 16:02:10 +0100

Body.
")

(defvar summarize-messages-mail-3 "To: test@ttt.org
From: tester@ttt.org
Subject: test 3
Date: 16 May 2008 16:02:10 +0100

Body.
")


;;;; All messages, empty range.  (Same tests as `summarize-folder').

(deftest summarize-messages ("" summarize-messages-1)
  "Test `summarize-messages' on an empty folder."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-2)
  "Test `summarize-messages' on a single message numbered 1."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages ("  17   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-3)
  "Test `summarize-messages' on a single message above number 1."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/17"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
"
			     summarize-messages-4)
  "Test `summarize-messages' on two packed messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages ("  17   15-May     94 tester@ttt.org    test 2                                 Bod
  22   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-5)
  "Test `summarize-messages' on two spread messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/17"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages ("   1   15-May     94 tester@ttt.org    test 2                                 Bod
  20   16-May     94 tester@ttt.org    test 3                                 Bod
  21   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-6)
  "Test `summarize-messages' on three out-of-order spread messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/20"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/21"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
   3   15-May     94 tester@ttt.org    test 2                                 Bod
  13   16-May     94 tester@ttt.org    test 3                                 Bod
"
			     summarize-messages-7)
  "Test `summarize-messages' on three ordered spread messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/13"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   16-May     94 tester@ttt.org    test 3                                 Bod
"
			     summarize-messages-8)
  "Test `summarize-messages' on three ordered packed messages."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages (" 111   15-May     94 tester@ttt.org    test 2                                 Bod
 222   16-May     94 tester@ttt.org    test 3                                 Bod
 333   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-9)
  "Test `summarize-messages' with a leading + on the folder name."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/111"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/222"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/333"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "+folder" '(:all) stream))))))

(deftest summarize-messages ("   1   15-May     94 tester@ttt.org    test 2                                 Bod
   3   15-Apr     94 tester@ttt.org    test 1                                 Bod
  12   16-May     94 tester@ttt.org    test 3                                 Bod
"
			     summarize-messages-10)
  "Test `summarize-messages', summarizing one of many folders."
  (with-test-dir (dir "Mail/folder/" "Mail/folder2/" "Mail/folder3/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/12"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder2/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder3/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder3/7"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   16-May     94 tester@ttt.org    test 3                                 Bod
"
			     summarize-messages-11)
  "Test `summarize-messages' on three packed messages in a lone subfolder."
  (with-test-dir (dir "Mail/folder/subfolder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/subfolder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder/subfolder" '(:all) stream))))))

(deftest summarize-messages ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   16-May     94 tester@ttt.org    test 3                                 Bod
"
			     summarize-messages-12)
  "Test `summarize-messages' on three packed messages in a lone subfolder, with a
   leading + on the folder name."
  (with-test-dir (dir "Mail/folder/subfolder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/subfolder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "+folder/subfolder" '(:all) stream))))))

(deftest summarize-messages ("   8   15-May     94 tester@ttt.org    test 2                                 Body.
  11   16-May     94 tester@ttt.org    test 3                                 Body.
  13   15-Apr     94 tester@ttt.org    test 1                                 Body.
"
			     summarize-messages-13)
  "Test `summarize-messages' on three spread messages in one of many
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
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/11"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder/13"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder1/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder2/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder2/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder/subfolder" '(:all) stream
				:width 83))))))

(deftest summarize-messages ("   8   15-May     94 tester@ttt.org    test 2                                 Bod
  11   16-May     94 tester@ttt.org    test 3                                 Bod
  13   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-14)
  "Test `summarize-messages' on three spread messages in one of many
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
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/11"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder/13"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder1/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder2/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder2/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder/subfolder/" '(:all) stream))))))

(deftest summarize-messages ("  11   15-May     94 tester@ttt.org    test 2                                 Bod
2000   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-15)
  "Test `summarize-messages' on two spread messages, with extra files in the
   folder."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/2000"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages ("" summarize-messages-16)
  "Test `summarize-messages' on an empty folder with extra files."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/,1"
		      "Mail/folder/xxx" "Mail/folder/1.BAK")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages ("   1   15-Apr     94 tester@ttt.org    test 1                                 Bod
  24   15-Apr     95 tester@ttt.org    test 1b                                Bod
"
			     summarize-messages-17)
  "Test `summarize-messages' on two messages with the exact same date."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/24"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages ("   8   15-May     94 tester@ttt.org    test 2                                 Bo
  11   15-Apr     95 tester@ttt.org    test 1b                                Bo
  13   15-Apr     94 tester@ttt.org    test 1                                 Bo
"
			     summarize-messages-18)
  "Test `summarize-messages' on three out of order messages in one of many
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
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/subfolder/11"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/subfolder/13"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder1/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/subfolder2/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/subfolder2/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder/subfolder" '(:all) stream
				:width 79))))))


;;;; Message specs.

(deftest summarize-messages ("" summarize-messages-30)
  "Test `summarize-messages' with a message spec and an empty folder."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(:all) stream))))))

(deftest summarize-messages ("  11   15-May     94 tester@ttt.org    test 2                                 Bod
2000   15-Apr     94 tester@ttt.org    test 1                                 Bod
2001   16-May     94 tester@ttt.org    test 3                                 Bod
2002   16-May     94 tester@ttt.org    test 3                                 Bod
2006   16-May     94 tester@ttt.org    test 3                                 Bod
2007   16-May     94 tester@ttt.org    test 3                                 Bod
2008   16-May     94 tester@ttt.org    test 3                                 Bod
"
			     summarize-messages-31)
  "Test `summarize-messages' with a varied message spec that defines all
   the messages in the folder."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/2000"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2001"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/2002"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/2006"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/2007"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/2008"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder"
				'(11 "2000" ("2001" . "2002")
				     "2006-2008")
				stream))))))

(deftest summarize-messages ("  11   15-May     94 tester@ttt.org    test 2                                 Bod
2000   15-Apr     94 tester@ttt.org    test 1                                 Bod
2001   16-May     94 tester@ttt.org    test 3                                 Bod
2002   16-May     94 tester@ttt.org    test 3                                 Bod
2006   16-May     94 tester@ttt.org    test 3                                 Bod
2007   16-May     94 tester@ttt.org    test 3                                 Bod
2008   16-May     94 tester@ttt.org    test 3                                 Bod
"
			     summarize-messages-32)
  "Test `summarize-messages' with a varied message spec that defines some
   the messages in the folder."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/2000"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/2001"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/2002"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/2003"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/2006"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/2007"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/2008"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/folder/2009"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder"
				'(11 "2000" ("2001" . "2002")
				     "2006-2008")
				stream))))))

(deftest summarize-messages ("  11   15-May     94 tester@ttt.org    test 2                                 Bod
"
			     summarize-messages-33)
  "Test `summarize-messages' with an range of one message that defines some
   the messages in the folder."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/20"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder"
				'(("11" . "11"))
				stream))))))

(deftest summarize-messages ("   1   15-Apr     95 tester@ttt.org    test 1b                                Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-34)
  "Test `summarize-messages' with a backward list range that defines all
   the messages in the folder."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder"
				'(("3" . "1"))
				stream))))))

(deftest summarize-messages ("   1   15-Apr     95 tester@ttt.org    test 1b                                Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-35)
  "Test `summarize-messages' with a backward string range that defines some
   of the messages in the folder."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '("3-1") stream))))))

(deftest summarize-messages ("   4   15-Apr     94 tester@ttt.org    test 1                                 Bod
   4   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-36)
  "Test `summarize-messages', requesting the same message twice."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(4 4) stream))))))

;;; The resulting order is because the ranges are sorted according to the
;;; highest message in the range.  Perhaps if the messages are given the
;;; order should remain as given.
;;;
(deftest summarize-messages ("   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   15-Apr     94 tester@ttt.org    test 1                                 Bod
   1   15-Apr     95 tester@ttt.org    test 1b                                Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   15-Apr     94 tester@ttt.org    test 1                                 Bod
   4   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-37)
  "Test `summarize-messages', with a range engulfed by an earlier range."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(("1" . "4") ("2" . "3")) stream))))))

;;; Same ordering issue as above.
;;;
(deftest summarize-messages ("   1   15-Apr     95 tester@ttt.org    test 1b                                Bod
   3   15-Apr     94 tester@ttt.org    test 1                                 Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   15-Apr     94 tester@ttt.org    test 1                                 Bod
   4   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-38)
  "Test `summarize-messages', with a message engulfed by a later range."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '(1 3 ("2" . "4")) stream))))))


;;;; Sequences.

(deftest summarize-messages ("   4   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-60)
  "Test `summarize-messages', requesting the highest sequence."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "tester: 2 3~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '("highest") stream))))))

(deftest summarize-messages ("   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-61)
  "Test `summarize-messages', requesting a sequence with a few messages."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "tester: 2 3~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '("tester") stream))))))

(deftest summarize-messages (""
			     summarize-messages-62)
  "Test `summarize-messages', requesting an empty sequence."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "tester:~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '("tester") stream))))))

(deftest summarize-messages ("   1   15-Apr     95 tester@ttt.org    test 1b                                Bod
   2   15-May     94 tester@ttt.org    test 2                                 Bod
   3   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-63)
  "Test `summarize-messages', with a range."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "in: 2 3~%")
	(format stream "tester: 2 3~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '("in") stream :range -10))))))

(deftest summarize-messages ("   3   15-Apr     94 tester@ttt.org    test 1                                 Bod
   4   15-Apr     94 tester@ttt.org    test 1                                 Bod
"
			     summarize-messages-64)
  "Test `summarize-messages', with a range where there are more messages in
   the cache than required by the range."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(format stream "in: 4~%")
	(format stream "tester: 2 3~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (mh:pick-messages "folder" '(:all))
       (with-output-to-string (stream)
	 (mh:summarize-messages "folder" '("in") stream :range -2))))))


;;;; Errors.

(deftest summarize-messages (t summarize-messages-90)
  "Test `summarize-messages' with an empty folder name argument."
  (let (ret)
    (handler-case
	(with-output-to-string (stream)
	  (mh:summarize-messages () '(:all) stream))
      (error () (setq ret t)))
    ret))

(deftest summarize-messages (t summarize-messages-91)
  "Test `summarize-messages' on a missing folder."
  (with-test-dir (dir "Mail/" )
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:summarize-messages "folder" '(:all) stream))
	   (error () (setq ret t)))
	 ret)))))

(deftest summarize-messages (t summarize-messages-92)
  "Test `summarize-messages' on a missing subfolder."
  (with-test-dir (dir "Mail/folder/" )
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:summarize-messages "folder/subfolder" '(:all) stream))
	   (error () (setq ret t)))
	 ret)))))

(deftest summarize-messages (t summarize-messages-93)
  "Test `summarize-messages' where the folder directory has been write-blocked."
  (with-test-dir (dir "Mail/folder/three/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/three/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/three/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (setf (file-mode (mh:folder-pathname "folder/three")) "a-rwx")
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:summarize-messages "folder/three" '(:all) stream))
	   (error () (setq ret t)))
	 (setf (file-mode (mh:folder-pathname "folder/three")) "a+rwx")
	 ret)))))

(deftest summarize-messages (t summarize-messages-94)
  "Test `summarize-messages', attempting to summarize the root of the mail
   directory."
  (with-test-dir (dir "Mail/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-3 stream))
      (to-file (stream "home:Mail/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:summarize-messages "" '(:all) stream))
	   (error () (setq ret t)))
	 ret)))))

(deftest summarize-messages (t summarize-messages-95)
  "Test `summarize-messages' with a string range that defines all of the
   messages in the folder, where there are gaps in the messages in the
   range."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/7"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/10"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:summarize-messages "folder" '("1-10") stream))
	   (error () (setq ret t))))))))

(deftest summarize-messages (t summarize-messages-96)
  "Test `summarize-messages', requesting the :highest messages in the
   folder."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:summarize-messages "folder" '(:highest) stream))
	   (error () (setq ret t))))))))

(deftest summarize-messages (t summarize-messages-97)
  "Test `summarize-messages', requesting the a range pair with integers."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:summarize-messages "folder" '((1 . 3)) stream))
	   (error () (setq ret t)))
	 ret)))))

(deftest summarize-messages ("" summarize-messages-98)
  "Test `summarize-messages', requesting a missing sequence."
  (with-test-dir (dir "Mail/folder/" "Mail/folder/file" "Mail/folder/,1")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1b stream))
      (to-file (stream "home:Mail/folder/2"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-2 stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (to-file (stream "home:Mail/folder/4"
		       :if-does-not-exist :create)
	(write-string summarize-messages-mail-1 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:summarize-messages "folder" '("tester") stream))
	   (error () (setq ret t)))
	 #|ret|#)))))
