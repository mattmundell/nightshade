;;; Tests of mh:profile-component.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest profile-component ("xyz" profile-component-1)
  "Test `profile-component' with an absolute profile pathname."
  (with-test-dir (dir "test/" "test/profile")
    (in-directory dir
      (with-open-file (stream "test/profile"
			      :direction :output
			      :if-exists ()
			      :if-does-not-exist :error)
	(write-line "abc: xyz" stream)
	(write-line "abcd: xxx" stream)))
    (namestring (mh:profile-component "abc"
				      (merge-pathnames "test/profile"
						       dir)))))

(deftest profile-component ("xyz" profile-component-2)
  "Test `profile-component' with a relative profile pathname."
  (with-test-dir (dir "test/" "test/profile")
    (in-directory dir
      (with-open-file (stream "test/profile"
			      :direction :output
			      :if-exists ()
			      :if-does-not-exist :error)
	(write-line "abc: xyz" stream)
	(write-line "abcd: xxx" stream))
      (in-directory "test/"
	(namestring (mh:profile-component "abc" "profile"))))))

(deftest profile-component ("xxx" profile-component-3)
  "Test `profile-component' with an absolute profile pathname, reading the
   last component in the file."
  (with-test-dir (dir "test/" "test/profile")
    (in-directory dir
      (with-open-file (stream "test/profile"
			      :direction :output
			      :if-exists ()
			      :if-does-not-exist :error)
	(write-line "abc: xyz" stream)
	(write-line "ccc: ffffff" stream)
	(write-line "ddd: 1.2" stream)
	(write-line "abcd: xxx" stream))
      (namestring (mh:profile-component "abcd"
					(merge-pathnames "test/profile"
							 dir))))))

(deftest profile-component ("xxx" profile-component-4)
  "Test `profile-component' with a relative profile pathname, reading the
   last component in the file."
  (with-test-dir (dir "test/" "test/profile")
    (in-directory dir
      (with-open-file (stream "test/profile"
			      :direction :output
			      :if-exists ()
			      :if-does-not-exist :error)
	(write-line "abc: xyz" stream)
	(write-line "ccc: ffffff" stream)
	(write-line "ddd: 1.2" stream)
	(write-line "abcd: xxx" stream))
      (in-directory "test/"
	(namestring (mh:profile-component "abcd" "profile"))))))

(deftest profile-component (() profile-component-5)
  "Test `profile-component' with a relative profile pathname, where the
   component is missing."
  (with-test-dir (dir "test/" "test/profile")
    (in-directory dir
      (with-open-file (stream "test/profile"
			      :direction :output
			      :if-exists ()
			      :if-does-not-exist :error)
	(write-line "abc: xyz" stream)
	(write-line "ccc: ffffff" stream)
	(write-line "ddd: 1.2" stream)
	(write-line "abcd: xxx" stream))
      (in-directory "test/"
	(mh:profile-component "missing" "profile")))))

(deftest profile-component ("value" profile-component-6)
  "Test `profile-component' with an implied profile pathname."
  (with-test-dir (config-dir)
    (with-test-dir (dir)
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "component: value~%")
	  (format stream "Path: ~A~%" dir)))
      (let ((config::*config-directory* config-dir)
	    mh::*folder-table*
	    mh::*profile-pathname*
	    mh::*root-pathname*
	    (ext:*environment-list*
	     (cons (cons :mh
			 (merge-pathnames ".mh_profile" dir))
		   ext:*environment-list*)))
	(mh:profile-component "component")))))

(deftest profile-component (() profile-component-7)
  "Test `profile-component' with an implied profile pathname where the
   component is missing."
  (with-test-dir (config-dir)
    (with-test-dir (dir)
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: ~A~%" dir)))
      (let ((config::*config-directory* config-dir)
	    mh::*folder-table*
	    mh::*profile-pathname*
	    mh::*root-pathname*
	    (ext:*environment-list*
	     (cons (cons :mh
			 (merge-pathnames ".mh_profile" dir))
		   ext:*environment-list*)))
	(mh:profile-component "component")))))


;;;; Errors.

(deftest profile-component (t profile-component-10)
  "Test `profile-component' with an empty argument."
  (let (ret)
    (handler-case
	(mh:profile-component ())
      (error () (setq ret t)))
    ret))

(deftest profile-component (t profile-component-11)
  "Test `profile-component' with an empty string."
  (let (ret)
    (handler-case
	(mh:profile-component "")
      (error () (setq ret t)))
    ret))
