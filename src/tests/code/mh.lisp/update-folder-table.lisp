;;; Tests of mh:update-folder-table.

(in-package "MH")

(import '(deftest:deftest deftest:with-test-dir))

(deftest update-folder-table (0 update-folder-table-0)
  "Test `update-folder-table', reading empty cached folders."
  (with-test-dir (dir "Mail/" "Mail/folders")
    (letf (((search-list "home:") dir))
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
      (with-fresh-state
       (ext:string-table-length (update-folder-table))))))

(deftest update-folder-table (5 update-folder-table-1)
  "Test `update-folder-table', reading cached folders."
  (with-test-dir (dir "Mail/" "Mail/folders")
    (letf (((search-list "home:") dir))
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%"))
	(with-open-file (stream "Mail/folders" :direction :output)
	  (write-line "a" stream)
	  (write-line "a/suba" stream)
	  (write-line "a/suba/one" stream)
	  (write-line "a/two" stream)
	  (write-line "three" stream)))
      (with-fresh-state
       (ext:string-table-length (update-folder-table))))))

(deftest update-folder-table (0 update-folder-table-2)
  "Test `update-folder-table', reading empty folders."
  (with-test-dir (dir "Mail/")
    (letf (((search-list "home:") dir))
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
      (with-fresh-state
       (ext:string-table-length (update-folder-table))))))

(deftest update-folder-table (5 update-folder-table-3)
  "Test `update-folder-table', reading folders."
  (with-test-dir (dir "Mail/a/sub1/one/" "Mail/a/two/" "Mail/three/")
    (letf (((search-list "home:") dir))
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: Mail~%")))
      (with-fresh-state
       (ext:string-table-length (update-folder-table))))))

(deftest update-folder-table (t update-folder-table-4)
  "Test calling `update-folder-table' twice."
  (with-test-dir (dir "mail/a/sub1/folder one/" "mail/a/two/"
		      "mail/three/")
    (letf (((search-list "home:") dir))
      (in-directory dir
	(with-open-file (stream ".mh_profile"
				:direction :output
				:if-exists :error
				:if-does-not-exist :create)
	  (format stream "Path: mail~%")))
      (with-fresh-state
       (let ((table (update-folder-table))
	     (table-again (update-folder-table)))
	 (and (equalp table table-again)
	      (eq (ext:string-table-length table-again) 5)
	      (eq (ext:string-table-length table) 5)))))))
