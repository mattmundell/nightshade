;;; Tests of mh:root-pathname.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest root-pathname (t root-pathname-1)
  "Test `root-pathname'."
  (with-test-dir (dir)
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: ~A~%" dir)))
    (let (mh::*root-pathname*
	  mh::*profile-pathname*
	  (ext:*environment-list*
	   (cons (cons :mh
		       (merge-pathnames ".mh_profile" dir))
		 ext:*environment-list*)))
      (string= (namestring (mh:root-pathname))
	       (namestring dir)))))

(deftest root-pathname (t root-pathname-2)
  "Test calling `root-pathname' twice."
  (with-test-dir (dir)
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: ~A~%" dir)))
    (let (mh::*root-pathname*
	  mh::*profile-pathname*
	  (ext:*environment-list*
	   (cons (cons :mh
		       (merge-pathnames ".mh_profile" dir))
		 ext:*environment-list*)))
      (let ((name (mh:root-pathname)))
	(and (string= (namestring name)
		      (namestring (mh:root-pathname)))
	     (string= (namestring name)
		      (namestring dir)))))))


;;;; Errors.

(deftest root-pathname (t root-pathname-10)
  "Test `root-pathname' with an empty argument."
  (let (ret)
    (handler-case
	(mh:root-pathname ())
      (error () (setq ret t)))
    ret))

(deftest root-pathname (t root-pathname-11)
  "Test `root-pathname' with a missing profile directory."
  (let ((dir (pick-new-dir)))
    (delete-dir dir)
    (let (mh::*root-pathname*
	  mh::*profile-pathname*
	  (ext:*environment-list*
	   (cons (cons :mh
		       (merge-pathnames ".mh_profile" dir))
		 ext:*environment-list*))
	  ret)
      (handler-case
	  (mh:root-pathname)
	(error () (setq ret t))))))

(deftest root-pathname (t root-pathname-12)
  "Test `root-pathname' with a missing path component."
  (with-test-dir (dir ".mh_profile")
    (let (mh::*root-pathname*
	  mh::*profile-pathname*
	  (ext:*environment-list*
	   (cons (cons :mh
		       (merge-pathnames ".mh_profile" dir))
		 ext:*environment-list*))
	  ret)
      (handler-case
	  (mh:root-pathname)
	(error () (setq ret t))))))
