;;; Tests of mh:draft-folder.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest draft-folder ("/abc/def" draft-folder-1)
  "Test `draft-folder' with an absolute drafts pathname."
  (with-test-dir (config-dir)
    (with-test-dir (dir)
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "draft-folder: /abc/def/~%")
	  (format stream "Path: ~A~%" dir)))
      (mh:with-fresh-state
       (let ((ext:*environment-list*
	      (cons (cons :mh
			  (merge-pathnames ".mh_profile" dir))
		    ext:*environment-list*)))
	 (mh:draft-folder))))))

(deftest draft-folder ("def" draft-folder-2)
  "Test `draft-folder' with a relative draft pathname."
  (with-test-dir (config-dir)
    (with-test-dir (dir)
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "draft-folder: def~%")
	  (format stream "Path: ~A~%" dir)))
      (let ((ext:*environment-list*
	     (cons (cons :mh
			 (merge-pathnames ".mh_profile" dir))
		   ext:*environment-list*)))
	(mh:with-fresh-state (mh:draft-folder))))))

(deftest draft-folder ("/abc/def" draft-folder-3)
  "Test `draft-folder' with the component alone in the profile."
  (with-test-dir (config-dir)
    (with-test-dir (dir)
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "draft-folder: /abc/def/~%")))
      (let ((ext:*environment-list*
	     (cons (cons :mh
			 (merge-pathnames ".mh_profile" dir))
		   ext:*environment-list*)))
	(mh:with-fresh-state (mh:draft-folder))))))


;;;; Errors.

(deftest draft-folder (t draft-folder-10)
  "Test `draft-folder' with too many arguments."
  (let (ret)
    (handler-case
	(mh:draft-folder t)
      (error () (setq ret t)))
    ret))

(deftest draft-folder (t draft-folder-11)
  "Test `draft-folder' with the component missing."
  (with-test-dir (config-dir)
    (with-test-dir (dir)
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "raft-folder: /abc/def/~%")))
      (let (ret
	    (ext:*environment-list*
	     (cons (cons :mh
			 (merge-pathnames ".mh_profile" dir))
		   ext:*environment-list*)))
	(mh:with-fresh-state
	 (handler-case
	     (mh:draft-folder)
	   (error () (setq ret t))))
	ret))))
