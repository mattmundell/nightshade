;;; Tests of internet:fill-from-netrc.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest fill-from-netrc ((internet:make-inet-account "machine.org"
						      "name"
						      "pwd"
						      "ssh")
			  fill-from-netrc-0
			  :test #'equalp)
  "Test `fill-from-netrc'."
  (with-test-dir (dir ".netrc")
    (in-directory dir
      (to-file (stream ".netrc")
	(write-line "machine machine.org" stream)
	(write-line "login name" stream)
	(write-line "password pwd" stream)
	(write-line "protocol ssh" stream)))
    (letf (((search-list "home:") (list dir)))
      (let ((account (internet:make-inet-account "machine.org")))
	(internet:fill-from-netrc account)))))

(deftest fill-from-netrc ((internet:make-inet-account "machine.org" "name2" "pwd2" "ftp")
			  fill-from-netrc-1
			  :test #'equalp)
  "Test `fill-from-netrc' where a lower entry matches the given account."
  (with-test-dir (dir ".netrc")
    (in-directory dir
      (to-file (stream ".netrc")
	(write-line "machine machine.org" stream)
	(write-line "login name" stream)
	(write-line "password pwd" stream)
	(write-line "protocol ssh" stream)
	(terpri stream)
	(write-line "machine machine.org" stream)
	(write-line "login name2" stream)
	(write-line "password pwd2" stream)
	(write-line "protocol ftp" stream)))
    (letf (((search-list "home:") (list dir)))
      (let ((account (internet:make-inet-account "machine.org"
						 "name2")))
	(internet:fill-from-netrc account)))))

(deftest fill-from-netrc ((internet:make-inet-account "new" "name3" "pwd3" "telnet")
			  fill-from-netrc-2
			  :test #'equalp)
  "Test `fill-from-netrc' where a lower entry matches the given account."
  (with-test-dir (dir ".netrc")
    (in-directory dir
      (to-file (stream ".netrc")
	(write-line "machine machine.org" stream)
	(write-line "login name" stream)
	(write-line "password pwd" stream)
	(write-line "protocol ssh" stream)
	(terpri stream)
	(write-line "machine machine.org" stream)
	(write-line "login name2" stream)
	(write-line "password pwd2" stream)
	(write-line "protocol ftp" stream)
	(terpri stream)
	(write-line "default" stream)
	(write-line "login name3" stream)
	(write-line "password pwd3" stream)
	(write-line "protocol telnet" stream)))
    (letf (((search-list "home:") (list dir)))
      (let ((account (internet:make-inet-account "new")))
	(internet:fill-from-netrc account)))))
