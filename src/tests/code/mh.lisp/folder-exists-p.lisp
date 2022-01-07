;;; Tests of mh:folder-exists-p.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest folder-exists-p (() folder-exists-p-1)
  "Test `folder-exists-p' with an empty set of folders."
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
	(mh:folder-exists-p "folder")))))

(deftest folder-exists-p (() folder-exists-p-2)
  "Test `folder-exists-p' on a subfolder with an empty set of folders."
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
	(mh:folder-exists-p "folder/subfolder")))))

(deftest folder-exists-p (t folder-exists-p-3)
  "Test `folder-exists-p' with an existing folder."
  (with-test-dir (config-dir)
    (with-test-dir (dir "a/sub1/one/" "a/two/" "three/")
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
	(mh:folder-exists-p "three")))))

(deftest folder-exists-p (t folder-exists-p-4)
  "Test `folder-exists-p' with an existing subfolder."
  (with-test-dir (config-dir)
    (with-test-dir (dir "a/sub1/one/" "a/two/" "three/")
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
	(mh:folder-exists-p "a/sub1/one")))))


;;;; Errors.

(deftest folder-exists-p (t folder-exists-p-5)
  "Test `folder-exists-p' with an empty argument."
  (let (ret)
    (handler-case
	(mh:folder-exists-p ())
      (error () (setq ret t)))
    ret))
