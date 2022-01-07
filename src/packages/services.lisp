;;; Internet services.

(defpackage "SERVICES"
  (:version 1)
  (:documentation "Internet services."))


;;;; Editor interface.

(in-package "ED")

(defcommand "Start Service" ()
  "Start a prompted service in a slave."
  (let ((exp (format ()
		     "(internet:start-service \"~A\")"
		     (prompt-for-keyword `(,internet:*services*)
					 :prompt "Service: "))))
    (message "=> ~{~#[~;~A~:;~A, ~]~}"
	     (eval-form-in-server (get-current-eval-server) exp))))

(defcommand "Stop Service" ()
  "Start a prompted service in a slave."
  (let ((exp (format ()
		     "(internet:stop-service \"~A\")"
		     (prompt-for-keyword `(,internet:*services*)
					 :prompt "Service: "))))
    (message "=> ~{~#[~;~A~:;~A, ~]~}"
	     (eval-form-in-server (get-current-eval-server) exp))))

(defcommand "Editor Start Service" ()
  "Start a prompted service in the Editor lisp."
  (internet:start-service
   (prompt-for-keyword `(,internet:*services*)
		       :prompt "Service: ")))

(defcommand "Editor Stop Service" ()
  "Stop a prompted service in the Editor lisp."
  (internet:stop-service
   (prompt-for-keyword `(,internet:*services*)
		       :prompt "Service: ")))
