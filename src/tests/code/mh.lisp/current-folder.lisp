;;; Tests of mh:current-folder.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest current-folder ("/abc/def/" current-folder-1)
  "Test `current-folder' where the context file is given as a component."
  (with-test-dir (dir)
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "context: ~A/.mh_profile~%" (namify dir))
	(format stream "current-folder: /abc/def/~%")
	(format stream "Path: ~A~%" dir)))
    (let (mh::*root-pathname*
	  mh::*context-pathname*
	  mh::*profile-pathname*
	  (ext:*environment-list*
	   (cons (cons :mh
		       (merge-pathnames ".mh_profile" dir))
		 ext:*environment-list*)))
      (mh:current-folder))))

(deftest current-folder ("box" current-folder-2)
  "Test `current-folder' where the context file is \"context\"."
  (with-test-dir (dir)
    (in-directory dir
      (with-open-file (stream "context"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "current-folder: box~%"))
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "current-folder: /abc/def/~%")
	(format stream "Path: ~A~%" dir)))
    (let (mh::*root-pathname*
	  mh::*context-pathname*
	  mh::*profile-pathname*
	  (ext:*environment-list*
	   (cons (cons :mh
		       (merge-pathnames ".mh_profile" dir))
		 ext:*environment-list*)))
      (mh:current-folder))))

(deftest current-folder (t current-folder-3)
  "Test calling `current-folder' twice."
  (with-test-dir (dir)
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "context: ~A/.mh_profile~%" (namify dir))
	(format stream "current-folder: folder~%")
	(format stream "Path: ~A~%" dir)))
    (let (mh::*root-pathname*
	  mh::*context-pathname*
	  mh::*profile-pathname*
	  (ext:*environment-list*
	   (cons (cons :mh
		       (merge-pathnames ".mh_profile" dir))
		 ext:*environment-list*)))
      (string= (mh:current-folder) (mh:current-folder)))))

(deftest current-folder (() current-folder-4)
  "Test calling `current-folder' when the component is missing."
  (with-test-dir (dir)
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "context: ~A/.mh_profile~%" (namify dir))
	(format stream "Path: ~A~%" dir)))
    (let (mh::*root-pathname*
	  mh::*context-pathname*
	  mh::*profile-pathname*
	  (ext:*environment-list*
	   (cons (cons :mh
		       (merge-pathnames ".mh_profile" dir))
		 ext:*environment-list*)))
      (mh:current-folder))))


;;;; Errors.

(deftest current-folder (t current-folder-10)
  "Test `current-folder' with too many arguments."
  (let (ret)
    (handler-case
	(mh:current-folder t)
      (error () (setq ret t)))
    ret))
