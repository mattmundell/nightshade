;;; Tests of mh:make-drop.

(in-package "MH")

(import '(deftest:deftest))

(deftest make-drop (t make-drop-0)
  "Test `make-drop' :local."
  (let ((drop (make-drop :local)))
    (and (eq (drop-new-fun drop) #'new-local-mail-p)
	 (eq (drop-inc-fun drop) #'incorporate-local))))

(deftest make-drop (t make-drop-1)
  "Test `make-drop' :pop."
  (let ((drop (make-drop :pop :kpop
			 :server "pop3.example.org"
			 :user "user@example.org")))
    (and (eq (drop-new-fun drop) #'mh::new-pop-mail-p)
	 (eq (drop-inc-fun drop) #'mh::incorporate-pop)
	 (eq (pop-drop-type drop) :kpop)
	 (string= (internet::inet-account-server (pop-drop-account drop))
		  "pop3.example.org")
	 (string= (internet::account-user (pop-drop-account drop))
		  "user@example.org"))))


;;;; Errors.

(deftest make-drop (t make-drop-20)
  "Test `make-drop' with the empty string."
  (let (ret)
    (handler-case
	(make-drop "")
      (error () (setq ret t)))
    ret))

(deftest make-drop (t make-drop-21)
  "Test `make-drop' with an empty folder name."
  (let (ret)
    (handler-case
	(make-drop ())
      (error () (setq ret t)))
    ret))
