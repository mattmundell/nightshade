;;; Commands to run system tests.

;; FIX a new pkg, that uses ed

(in-package "ED")

(defun test-symbol (symbol)
  "Run the test for symbol, outputting a pass/fail message."
  (multiple-value-bind (pass test value expected)
		       (deftest:test symbol)
    (if pass
	(format t "~A passed." symbol)
	(format t "~A failed: ~A returned ~A, expected ~A."
		symbol test value expected))))

(defcommand "Test Symbol" ()
  "Run the tests associated with a prompted symbol in a slave lisp."
  (let* ((name (prompt-for-string
		:prompt "Symbol: "
		:default (word-at-point)
		:help "Name of symbol to test."))
	 (symbol (read-from-string name)))
    (eval-form-in-server (get-current-eval-server)
			 (format () "(ed::test-symbol '~A)" symbol))))

(defcommand "Editor Test Symbol" ()
  "Run the tests associated with a prompted symbol in the editor lisp."
  (let* ((name (prompt-for-string
		:prompt "Symbol: "
		:default (word-at-point)
		:help "Name of symbol to test."))
	 (symbol (read-from-string name)))
    (multiple-value-bind (pass test value expected)
			 (deftest:test symbol)
      (if pass
	  (message "~A passed." symbol)
	  (message "~A failed: ~A returned ~A, expected ~A."
		   symbol test value expected)))))
