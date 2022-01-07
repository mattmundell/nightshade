;;; Tests of mh:folder-pathname.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest folder-pathname (t folder-pathname-1)
  "Test `folder-pathname'."
  (with-test-dir (dir "test/subtest/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: test/subtest/~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (equal (namestring (mh:folder-pathname "folder"))
	      (namestring (merge-pathnames "test/subtest/folder/"
					   dir)))))))

(deftest folder-pathname (t folder-pathname-2)
  "Test `folder-pathname' with a + prefix on the folder."
  (with-test-dir (dir "test/subtest/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: test/subtest/~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (equal (namestring (mh:folder-pathname "+folder"))
	      (namestring (merge-pathnames "test/subtest/folder/"
					   dir)))))))

(deftest folder-pathname (t folder-pathname-3)
  "Test `folder-pathname' with a subfolder."
  (with-test-dir (dir "test/subtest/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: test/subtest/~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (equal (namestring (mh:folder-pathname "folder/sub"))
	      (namestring (merge-pathnames "test/subtest/folder/sub/"
					   dir)))))))

(deftest folder-pathname (t folder-pathname-4)
  "Test `folder-pathname' with a subfolder and a + prefix on the folder."
  (with-test-dir (dir "test/subtest/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: test/subtest/~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (equal (namestring (mh:folder-pathname "+folder/sub"))
	      (namestring (merge-pathnames "test/subtest/folder/sub/"
					   dir)))))))

(deftest folder-pathname (t folder-pathname-5)
  "Test `folder-pathname' with a trailing slash on the folder name."
  (with-test-dir (dir "test/subtest/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: test/subtest/~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (equal (namestring (mh:folder-pathname "+folder/sub/"))
	      (namestring (merge-pathnames "test/subtest/folder/sub/"
					   dir)))))))


;;;; Errors.

(deftest folder-pathname (t folder-pathname-10)
  "Test `folder-pathname' with an empty argument."
  (let (ret)
    (handler-case
	(mh:folder-pathname ())
      (error () (setq ret t)))
    ret))

(deftest folder-pathname (t folder-pathname-11)
  "Test `folder-pathname' with an empty string."
  (let (ret)
    (handler-case
	(mh:folder-pathname "")
      (error () (setq ret t)))
    ret))
