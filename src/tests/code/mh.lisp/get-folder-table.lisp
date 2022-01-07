;;; Tests of mh:get-folder-table.

(in-package "MH")

(import '(deftest:deftest deftest:with-test-dir))

(deftest get-folder-table (0 get-folder-table-0)
  "Test `get-folder-table', reading empty cached folders."
  (with-test-dir (dir "Mail/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (ext:string-table-length (get-folder-table))))))

(deftest get-folder-table (5 get-folder-table-1)
  "Test `get-folder-table', reading cached folders."
  (with-test-dir (dir "Mail/" "Mail/folders")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (in-directory "Mail/"
	(with-open-file (stream "folders" :direction :output)
	  (write-line "a" stream)
	  (write-line "a/suba" stream)
	  (write-line "a/suba/one" stream)
	  (write-line "a/two" stream)
	  (write-line "three" stream))))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (ext:string-table-length (get-folder-table))))))

(deftest get-folder-table (0 get-folder-table-2)
  "Test `get-folder-table', reading empty folders."
  (with-test-dir (dir "Mail/" "Mail/folders")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (ext:string-table-length (get-folder-table))))))

(deftest get-folder-table (5 get-folder-table-3)
  "Test `get-folder-table', reading folders."
  (with-test-dir (dir "Mail/" "Mail/a/sub1/one/" "Mail/a/two/"
		      "Mail/three/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (ext:string-table-length (get-folder-table))))))

(deftest get-folder-table (t get-folder-table-4)
  "Test calling `get-folder-table' twice."
  (with-test-dir (dir "Mail/" "Mail/a/sub1/one/" "Mail/a/two/"
		      "Mail/three/")
    (in-directory dir
      (with-open-file (stream ".mh_profile"
			      :direction :output
			      :if-exists :error
			      :if-does-not-exist :create)
	(format stream "Path: Mail~%")))
    (letf (((search-list "home:") dir))
      (mh:with-fresh-state
       (let ((table (get-folder-table)))
	 (and (eq (ext:string-table-length table) 5)
	      (eq table (get-folder-table))))))))
