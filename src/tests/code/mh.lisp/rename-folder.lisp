;;; Tests of mh:rename-folder.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest rename-folder (t rename-folder-1)
  "Test `rename-folder' on a single folder."
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
	(mh:create-folder "folder")
	(mh:rename-folder "folder" "new")
	(fi (probe-file (merge-pathnames "folder/" dir))
	    (and (probe-file (merge-pathnames "new/" dir))
		 (string= "new
"
			  (with-output-to-string (out)
			    (with-open-file (in (merge-pathnames
						 "folders"
						 dir))
			      (transfer in out))))))))))

(deftest rename-folder (t rename-folder-2)
  "Test `rename-folder' on a single folder with leading +s on the folder
   names."
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
	(mh:create-folder "folder")
	(mh:rename-folder "+folder" "+new")
	(fi (probe-file (merge-pathnames "folder/" dir))
	    (and (probe-file (merge-pathnames "new/" dir))
		 (string= "new
"
			  (with-output-to-string (out)
			    (with-open-file (in (merge-pathnames
						 "folders"
						 dir))
			      (transfer in out))))))))))

(deftest rename-folder (t rename-folder-3)
  "Test `rename-folder', on one of many folders."
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
	(mh:create-folder "one")
	(mh:create-folder "two")
	(mh:create-folder "three")
	(mh:rename-folder "three" "four")
	(and (probe-file (merge-pathnames "one/" dir))
	     (probe-file (merge-pathnames "two/" dir))
	     (fi (probe-file (merge-pathnames "three/" dir))
		 (and (probe-file (merge-pathnames "four/" dir))
		      (string= "four
one
two
"
			  (with-output-to-string (out)
			    (with-open-file (in (merge-pathnames
						 "folders"
						 dir))
			      (transfer in out)))))))))))

(deftest rename-folder (t rename-folder-4)
  "Test `rename-folder' on a single subfolder."
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
	(mh:create-folder "folder/subfolder")
	(mh:rename-folder "folder/subfolder" "new")
	(fi (probe-file (merge-pathnames "folder/subfolder" dir))
	    (and (probe-file (merge-pathnames "new/" dir))
		 (string= "new
"
			  (with-output-to-string (out)
			    (with-open-file (in (merge-pathnames
						 "folders"
						 dir))
			      (transfer in out))))))))))

(deftest rename-folder (t rename-folder-5)
  "Test `rename-folder' on a single subfolderfolder with leading +s on the
   folder names."
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
	(mh:create-folder "folder")
	(mh:create-folder "folder/subfolder")
	(mh:rename-folder "+folder/subfolder" "+folder/one")
	(fi (probe-file (merge-pathnames "folder/subfolder" dir))
	    (and (probe-file (merge-pathnames "folder/one" dir))
		 (string= "folder
folder/one
"
			  (with-output-to-string (out)
			    (with-open-file (in (merge-pathnames
						 "folders"
						 dir))
			      (transfer in out))))))))))

(deftest rename-folder (t rename-folder-6)
  "Test `rename-folder', on one of many subfolders."
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
	(mh:create-folder "folder/one")
	(mh:create-folder "folder/two")
	(mh:create-folder "folder/three")
	(mh:rename-folder "folder/three" "folder/four")
	(and (probe-file (merge-pathnames "folder/one/" dir))
	     (probe-file (merge-pathnames "folder/two/" dir))
	     (fi (probe-file (merge-pathnames "folder/three/" dir))
		 (and (probe-file (merge-pathnames "folder/four/" dir))
		      (string= "folder/four
folder/one
folder/two
"
			  (with-output-to-string (out)
			    (with-open-file (in (merge-pathnames
						 "folders"
						 dir))
			      (transfer in out)))))))))))

(deftest rename-folder (t rename-folder-7)
  "Test `rename-folder' on a single folder with a trailing slash on the
   names."
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
	(mh:create-folder "folder")
	(mh:rename-folder "folder/" "new/")
	(fi (probe-file (merge-pathnames "folder/" dir))
	    (and (probe-file (merge-pathnames "new/" dir))
		 (string= "new
"
			  (with-output-to-string (out)
			    (with-open-file (in (merge-pathnames
						 "folders"
						 dir))
			      (transfer in out))))))))))

(deftest rename-folder (t rename-folder-8)
  "Test `rename-folder', where the directory has files."
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
	(mh:create-folder "folder")
	(in-directory (mh:folder-pathname "folder")
	  (touch-file "1")
	  (touch-file "10")
	  (touch-file "139")
	  (touch-file ".file")
	  (touch-file ",file")
	  (touch-file "+file")
	  (touch-file "_file")
	  (touch-file "#file")
	  (touch-file "@")
	  (touch-file ",")
	  (touch-file "cur"))
	(mh:rename-folder "folder" "folder2")
	(fi (probe-file (merge-pathnames "folder/" dir))
	    (and (probe-file (merge-pathnames "folder2/" dir))
		 (string= "folder2
"
			  (with-output-to-string (out)
			    (with-open-file (in (merge-pathnames
						 "folders"
						 dir))
			      (transfer in out))))))))))


;;;; Errors.

(deftest rename-folder (t rename-folder-40)
  "Test `rename-folder' with an empty argument."
  (let (ret)
    (handler-case
	(mh:rename-folder "folder" ())
      (error () (setq ret t)))
    ret))

(deftest rename-folder (t rename-folder-41)
  "Test `rename-folder' with a missing folder."
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
		   ext:*environment-list*))
	    ret)
	(handler-case
	    (mh:rename-folder "folder" "folder2")
	  (error () (setq ret t)))))))

(deftest rename-folder (() rename-folder-42)
  "Test `rename-folder' on a missing subfolder."
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
		   ext:*environment-list*))
	    ret)
	(mh:create-folder "folder")
	(handler-case
	    (mh:rename-folder "folder/sub" "two")
	  (error () (setq ret ())))))))

(deftest rename-folder (t rename-folder-43)
  "Test `rename-folder' where the folder directory has been write-blocked."
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
		   ext:*environment-list*))
	    ret)
	(mh:create-folder "folder/one")
	(mh:create-folder "folder/two")
	(mh:create-folder "folder/three")
	(setf (file-mode (mh:folder-pathname "folder/three")) "a-rwx")
	(handler-case
	    (mh:rename-folder "folder/three/" "new")
	  (error () (setq ret t)))
	(setf (file-mode (mh:folder-pathname "folder/three")) "a+rwx")
	ret))))

(deftest rename-folder (t rename-folder-44)
  "Test `rename-folder' where the destination exists already."
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
		   ext:*environment-list*))
	    ret)
	(mh:create-folder "folder/one")
	(mh:create-folder "folder/two")
	(mh:create-folder "folder/three")
	(handler-case
	    (mh:rename-folder "folder/three" "folder/two")
	  (error () (setq ret t)))
	ret))))
