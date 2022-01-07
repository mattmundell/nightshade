;;; Tests of mh:print-folders.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest print-folders ("" print-folders-1)
  "Test `print-folders' with an empty mail directory."
  (with-test-dir (dir "Mail/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:print-folders stream))))))

(deftest print-folders ("" print-folders-2)
  "Test `print-folders' with an empty mail directory and a prefix."
  (with-test-dir (dir "Mail/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:print-folders stream "prefix"))))))

(deftest print-folders ("folder
" print-folders-3)
  "Test `print-folders' with a single folder."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:print-folders stream))))))

(deftest print-folders ("prefixfolder
" print-folders-4)
  "Test `print-folders' with a single folder and a prefix."
  (with-test-dir (dir "Mail/folder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:print-folders stream "prefix"))))))

(deftest print-folders ("folder
folder/subfolder
" print-folders-5)
  "Test `print-folders' with a single subfolder."
  (with-test-dir (dir "Mail/folder/subfolder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:print-folders stream))))))

(deftest print-folders ("---folder
---folder/subfolder
" print-folders-6)
  "Test `print-folders' with a single subfolder and a prefix."
  (with-test-dir (dir "Mail/folder/subfolder/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:print-folders stream "---"))))))

(deftest print-folders ("folder
xxx
yyy
zzz
" print-folders-7)
  "Test `print-folders' with many folders."
  (with-test-dir (dir "Mail/folder/"
		      "Mail/xxx/"
		      "Mail/yyy/"
		      "Mail/zzz/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:print-folders stream))))))

(deftest print-folders ("100folder
100xxx
100yyy
100zzz
" print-folders-8)
  "Test `print-folders' with many folders, with a number as prefix."
  (with-test-dir (dir "Mail/folder/"
		      "Mail/xxx/"
		      "Mail/yyy/"
		      "Mail/zzz/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:print-folders stream 100))))))

(deftest print-folders ("folder
folder/subfolder
folder/subfolder1
folder/subfolder2
folder/subfolder3
folder/subfolder3/one
folder/subfolder3/one/two
xxx
yyy
zzz
zzz/x
zzz/x/zz
zzz/x/zz/zz
zzz/x/zz/zz/z
zzz/x/zz/zz/z/z
zzz/y
zzz/y/zz
zzz/y/zz/zz
zzz/y/zz/zz/z
zzz/y/zz/zz/z/z
zzz/z
zzz/z/zz
zzz/z/zz/zz
zzz/z/zz/zz/z
zzz/z/zz/zz/z/z
" print-folders-9)
  "Test `print-folders' with many folders and subfolders."
  (with-test-dir (dir "Mail/folder/subfolder/"
		      "Mail/folder/subfolder1/"
		      "Mail/folder/subfolder2/"
		      "Mail/folder/subfolder3/one/two/"
		      "Mail/xxx/"
		      "Mail/yyy/"
		      "Mail/zzz/z/zz/zz/z/z/"
		      "Mail/zzz/y/zz/zz/z/z/"
		      "Mail/zzz/x/zz/zz/z/z/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:print-folders stream))))))

(deftest print-folders ("   folder
   folder/subfolder
   folder/subfolder1
   folder/subfolder2
   folder/subfolder3
   folder/subfolder3/one
   folder/subfolder3/one/two
   xxx
   yyy
   zzz
   zzz/x
   zzz/x/zz
   zzz/x/zz/zz
   zzz/x/zz/zz/z
   zzz/x/zz/zz/z/z
   zzz/y
   zzz/y/zz
   zzz/y/zz/zz
   zzz/y/zz/zz/z
   zzz/y/zz/zz/z/z
   zzz/z
   zzz/z/zz
   zzz/z/zz/zz
   zzz/z/zz/zz/z
   zzz/z/zz/zz/z/z
" print-folders-10)
  "Test `print-folders' with many folders and subfolders, with the stream t
   and a prefix."
  (with-test-dir (dir "Mail/folder/subfolder/"
		      "Mail/folder/subfolder1/"
		      "Mail/folder/subfolder2/"
		      "Mail/folder/subfolder3/one/two/"
		      "Mail/xxx/"
		      "Mail/yyy/"
		      "Mail/zzz/z/zz/zz/z/z/"
		      "Mail/zzz/y/zz/zz/z/z/"
		      "Mail/zzz/x/zz/zz/z/z/")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (with-output-to-string (*standard-output*)
	 (mh:print-folders t "   "))))))


;;;; Errors.

(deftest print-folders (t print-folders-40)
  "Test `print-folders' with an empty argument."
  (let (ret)
    (handler-case
	(mh:print-folders ())
      (error () (setq ret t)))
    ret))

(deftest print-folders (t print-folders-41)
  "Test `print-folders' with too few arguments."
  (let (ret)
    (handler-case
	(mh:print-folders)
      (error () (setq ret t)))
    ret))
