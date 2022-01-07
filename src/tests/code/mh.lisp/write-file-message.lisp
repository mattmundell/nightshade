;;; Tests of mh:write-file-message and mh:get-headers-part.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar *write-file-message-headers*
  '("Resent-To" "To" "Resent-Cc" "Cc"
    "Resent-From" "From" "Reply-To" "Date" "Subject"
    "Resent-Date")
  "The headers used by the editor mail interface.")

(defun write-file-message-html-handler (out in-pathname)
  "Write the file at $in-pathname to stream $out."
  (from-file (in in-pathname) (transfer in out)))

; FIX Nested forwarded messages (---..--- Start of forwarded message ---...---)


;;;; Plain message, all headers.

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
"
			write-file-message-1)
  "Test `write-file-message', implying all headers."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
" stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:write-file-message stream "home:Mail/folder/1"))))))


;;;; Plain message, some headers.

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Body.
"
			     write-file-message-50)
  "Test `write-file-message' with some headers and a plain message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100

Body.
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1
"
			write-file-message-51)
  "Test `write-file-message' with some headers and an empty body."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))


;;;; MIME text, all headers.

;; FIX 100


;;;; MIME text, some headers.

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1
"
			write-file-message-150)
  "Test `write-file-message' with some headers, text/plain and an empty body."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Body.
"
			write-file-message-151)
  "Test `write-file-message' with some headers and text/plain."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/2222"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain

Body.
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/2222"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

An initial sentence in the body.

A second sentence of the body which starts the second paragraph and is
longer, so it includes a line break.  The third and final sentence, which
ends the paragraph and body.
"
			write-file-message-152)
  "Test `write-file-message' with some headers, text/plain and base64 encoding."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Transfer-Encoding: base64

QW4gaW5pdGlhbCBzZW50ZW5jZSBpbiB0aGUgYm9keS4KCkEgc2Vjb25kIHNl
bnRlbmNlIG9mIHRoZSBib2R5IHdoaWNoIHN0YXJ0cyB0aGUgc2Vjb25kIHBh
cmFncmFwaCBhbmQgaXMKbG9uZ2VyLCBzbyBpdCBpbmNsdWRlcyBhIGxpbmUg
YnJlYWsuICBUaGUgdGhpcmQgYW5kIGZpbmFsIHNlbnRlbmNlLCB3aGljaApl
bmRzIHRoZSBwYXJhZ3JhcGggYW5kIGJvZHkuCg==
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1
"
			write-file-message-153)
  "Test `write-file-message' with some headers, text/plain and an empty body
   explicitly inlined."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Disposition: inline
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Body.
"
			write-file-message-154)
  "Test `write-file-message' with some headers and text/plain explicitly
   inlined."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Disposition: inline

Body.
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

An initial sentence in the body.

A second sentence of the body which starts the second paragraph and is
longer, so it includes a line break.  The third and final sentence, which
ends the paragraph and body.
"
			write-file-message-155)
  "Test `write-file-message' with some headers, base64 text/plain explicitly
   inlined."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Transfer-Encoding: base64

QW4gaW5pdGlhbCBzZW50ZW5jZSBpbiB0aGUgYm9keS4KCkEgc2Vjb25kIHNl
bnRlbmNlIG9mIHRoZSBib2R5IHdoaWNoIHN0YXJ0cyB0aGUgc2Vjb25kIHBh
cmFncmFwaCBhbmQgaXMKbG9uZ2VyLCBzbyBpdCBpbmNsdWRlcyBhIGxpbmUg
YnJlYWsuICBUaGUgdGhpcmQgYW5kIGZpbmFsIHNlbnRlbmNlLCB3aGljaApl
bmRzIHRoZSBwYXJhZ3JhcGggYW5kIGJvZHkuCg==
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1
"
			write-file-message-156)
  "Test `write-file-message' with some headers, text/plain and an empty body
   attached."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: Example body.
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ('("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

#text/plain [Example body.]
"
			  ("Body.
"
			   "text/plain"))
			write-file-message-157)
  "Test `write-file-message' and `get-headers-part' with some headers and
   text/plain attached."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: Example body.

Body.
" stream))
      (mh:with-fresh-state
       (block ()
	 (let ((fields))
	   (list (with-output-to-string (stream)
		   (multiple-value-bind
		       (success msg)
		       (mh:write-file-message stream "home:Mail/folder/1"
					      *write-file-message-headers*)
		     (if success
			 (setq fields msg)
			 (progn
			   (format t "Error message: ~A" msg)
			   (return)))))
		 (multiple-value-list
		  (mh:get-headers-part fields 83)))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

#text/plain [Example base64 body.]
"
			write-file-message-158)
  "Test `write-file-message' with some headers, base64 text/plain attached."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Transfer-Encoding: base64
Content-Disposition: attachment
Content-Description: Example base64 body.

QW4gaW5pdGlhbCBzZW50ZW5jZSBpbiB0aGUgYm9keS4KCkEgc2Vjb25kIHNl
bnRlbmNlIG9mIHRoZSBib2R5IHdoaWNoIHN0YXJ0cyB0aGUgc2Vjb25kIHBh
cmFncmFwaCBhbmQgaXMKbG9uZ2VyLCBzbyBpdCBpbmNsdWRlcyBhIGxpbmUg
YnJlYWsuICBUaGUgdGhpcmQgYW5kIGZpbmFsIHNlbnRlbmNlLCB3aGljaApl
bmRzIHRoZSBwYXJhZ3JhcGggYW5kIGJvZHkuCg==
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("
#text/plain [Example base64 body.]
"
			write-file-message-159)
  "Test `write-file-message' with some headers, base64 text/plain attached,
   requesting a single missing header."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Transfer-Encoding: base64
Content-Disposition: attachment
Content-Description: Example base64 body.

QW4gaW5pdGlhbCBzZW50ZW5jZSBpbiB0aGUgYm9keS4KCkEgc2Vjb25kIHNl
bnRlbmNlIG9mIHRoZSBib2R5IHdoaWNoIHN0YXJ0cyB0aGUgc2Vjb25kIHBh
cmFncmFwaCBhbmQgaXMKbG9uZ2VyLCBzbyBpdCBpbmNsdWRlcyBhIGxpbmUg
YnJlYWsuICBUaGUgdGhpcmQgYW5kIGZpbmFsIHNlbnRlbmNlLCB3aGljaApl
bmRzIHRoZSBwYXJhZ3JhcGggYW5kIGJvZHkuCg==
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      '("Xxx"))
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

#text/plain [Example base64 body.]
"
			write-file-message-160)
  "Test `write-file-message' with some headers, base64 text/plain attached,
   requesting a missing header amongst the headers."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Transfer-Encoding: base64
Content-Disposition: attachment
Content-Description: Example base64 body.

QW4gaW5pdGlhbCBzZW50ZW5jZSBpbiB0aGUgYm9keS4KCkEgc2Vjb25kIHNl
bnRlbmNlIG9mIHRoZSBib2R5IHdoaWNoIHN0YXJ0cyB0aGUgc2Vjb25kIHBh
cmFncmFwaCBhbmQgaXMKbG9uZ2VyLCBzbyBpdCBpbmNsdWRlcyBhIGxpbmUg
YnJlYWsuICBUaGUgdGhpcmQgYW5kIGZpbmFsIHNlbnRlbmNlLCB3aGljaApl
bmRzIHRoZSBwYXJhZ3JhcGggYW5kIGJvZHkuCg==
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      (cons "Xxx" *write-file-message-headers*))
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

#application/example [Example inline application.]
"
			write-file-message-161)
  "Test `write-file-message' with some headers, an inline application
   attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: application/example
Content-Disposition: inline
Content-Description: Example inline application.

This should be an attachment, even though it is specified as inline, so
that the client can decide what to do with it.
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      (cons "Xxx" *write-file-message-headers*))
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

; FIX inline, attachment, assumed versions of all tests below


;;;; MIME message/*, all headers.

; FIX 200


;;;; MIME message/*, some headers.

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: [Forward: subject]

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
"
			write-file-message-250)
  "Test `write-file-message' with some headers and inline message/rfc822."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: [Forward: subject]
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: message/rfc822
Content-Disposition: inline

From: oo <o@o.org>
To: test@ttt.org
Subject: subject
Date: 14 Apr 2008 16:02:10 +0100
References: xxx

Plain text body of forwarded message.
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: [Forward: subject]

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
"
			write-file-message-251)
  "Test `write-file-message' with some headers and message/rfc822."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: [Forward: subject]
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: message/rfc822

From: oo <o@o.org>
To: test@ttt.org
Subject: subject
Date: 14 Apr 2008 16:02:10 +0100
References: xxx

Plain text body of forwarded message.
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: [Forward: subject]

#message/rfc822 [Forwarded message: subject]
"
			write-file-message-252)
  "Test `write-file-message' with some headers and attachment message/rfc822."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: [Forward: subject]
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: message/rfc822
Content-Disposition: attachment
Content-Description: Forwarded message: subject

From: oo <o@o.org>
To: test@ttt.org
Subject: subject
Date: 14 Apr 2008 16:02:10 +0100
References: xxx

Plain text body of forwarded message.
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: [Forward: subject]

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
"
			write-file-message-253)
  "Test `write-file-message' with some headers and base64 message/rfc822."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: [Forward: subject]
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: message/rfc822
Content-Transfer-Encoding: base64

RnJvbTogb28gPG9Aby5vcmc+ClRvOiB0ZXN0QHR0dC5vcmcKU3ViamVjdDogc3ViamVjdApEYXRl
OiAxNCBBcHIgMjAwOCAxNjowMjoxMCArMDEwMApSZWZlcmVuY2VzOiB4eHgKClBsYWluIHRleHQg
Ym9keSBvZiBmb3J3YXJkZWQgbWVzc2FnZS4K
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: [Forward: subject]

#message/rfc822 [Description of attachment]
"
			write-file-message-254)
  "Test `write-file-message' with some headers and base64 message/rfc822
   attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: [Forward: subject]
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: message/rfc822
Content-Transfer-Encoding: base64
Content-Disposition: attachment
Content-Description: Description of attachment

RnJvbTogb28gPG9Aby5vcmc+ClRvOiB0ZXN0QHR0dC5vcmcKU3ViamVjdDogc3ViamVjdApEYXRl
OiAxNCBBcHIgMjAwOCAxNjowMjoxMCArMDEwMApSZWZlcmVuY2VzOiB4eHgKClBsYWluIHRleHQg
Ym9keSBvZiBmb3J3YXJkZWQgbWVzc2FnZS4K
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: [Forward: subject]

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
"
			write-file-message-255)
  "Test `write-file-message' with some headers and base64 message/rfc822
   inline."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: [Forward: subject]
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: message/rfc822
Content-Transfer-Encoding: base64
Content-Disposition: inline
Content-Description: Description of attachment

RnJvbTogb28gPG9Aby5vcmc+ClRvOiB0ZXN0QHR0dC5vcmcKU3ViamVjdDogc3ViamVjdApEYXRl
OiAxNCBBcHIgMjAwOCAxNjowMjoxMCArMDEwMApSZWZlcmVuY2VzOiB4eHgKClBsYWluIHRleHQg
Ym9keSBvZiBmb3J3YXJkZWQgbWVzc2FnZS4K
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: Mail failure

Reporting-MTA: dns; foo.org
Arrival-Date: 15 Apr 2008 16:02:10 +0100

Action: fail
Remote-MTA: dns; two.org
"
			write-file-message-256)
  "Test `write-file-message' with some headers and message/delivery-status."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: Mail failure
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: message/delivery-status

Reporting-MTA: dns; foo.org
Arrival-Date: 15 Apr 2008 16:02:10 +0100

Action: fail
Remote-MTA: dns; two.org
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: Mail failure

Reporting-MTA: dns; foo.org
Arrival-Date: 15 Apr 2008 16:02:10 +0100

Action: fail
Remote-MTA: dns; two.org
"
			write-file-message-257)
  "Test `write-file-message' with some headers and base64
   message/delivery-status."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: Mail failure
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: message/delivery-status
Content-Transfer-Encoding: base64

UmVwb3J0aW5nLU1UQTogZG5zOyBmb28ub3JnCkFycml2YWwtRGF0ZTogMTU
gQXByIDIwMDggMTY6MDI6MTAgKzAxMDAKCkFjdGlvbjogZmFpbApSZW1vdGU
tTVRBOiBkbnM7IHR3by5vcmcK
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

; FIX deliver-status [base64] inline,attachment


;;;; MIME multipart/alternative, all headers.

; FIX 300


;;;; MIME multipart/alternative, some headers.

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Body.
"
			write-file-message-350)
  "Test `write-file-message' with some headers and multipart/alternative with a
   single text/plain part and body text before the first boundary."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

    This is a MIME encoded message.
    This text should be skipped.

--unique-string
Content-Type: text/plain

Body.
--unique-string--
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Body.
"
			write-file-message-351)
  "Test `write-file-message' with some headers and multipart/alternative with a
   single text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: text/plain

Body.
--unique-string--
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

First alternative.
"
			write-file-message-352)
  "Test `write-file-message' with some headers and multipart/alternative with
   multiple text/plain parts."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: text/plain

First alternative.
--unique-string
Content-Type: text/plain

Second alternative.
--unique-string--
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

First alternative.
"
			write-file-message-353)
  "Test `write-file-message' with some headers and multipart/alternative with a
   text/plain and a text/html part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: text/plain

First alternative.
--unique-string
Content-Type: text/html

<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
"
			write-file-message-354)
  "Test `write-file-message' with some headers and multipart/alternative with a
   text/html and a text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: text/html

<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
--unique-string
Content-Type: text/plain

Second alternative.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

An initial sentence in the body.

A second sentence of the body which starts the second paragraph and is
longer, so it includes a line break.  The third and final sentence, which
ends the paragraph and body.
"
			write-file-message-355)
  "Test `write-file-message' with some headers and multipart/alternative with a
   base64 encoded text/plain part and a text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"

--unique-string
Content-Type: text/plain
Content-Transfer-Encoding: base64

QW4gaW5pdGlhbCBzZW50ZW5jZSBpbiB0aGUgYm9keS4KCkEgc2Vjb25kIHNl
bnRlbmNlIG9mIHRoZSBib2R5IHdoaWNoIHN0YXJ0cyB0aGUgc2Vjb25kIHBh
cmFncmFwaCBhbmQgaXMKbG9uZ2VyLCBzbyBpdCBpbmNsdWRlcyBhIGxpbmUg
YnJlYWsuICBUaGUgdGhpcmQgYW5kIGZpbmFsIHNlbnRlbmNlLCB3aGljaApl
bmRzIHRoZSBwYXJhZ3JhcGggYW5kIGJvZHkuCg==

--unique-string
Content-Type: text/plain

Second alternative.
--unique-string--
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
"
			write-file-message-356)
  "Test `write-file-message' with some headers and multipart/alternative with a
   base64 text/html and a text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: text/html
Content-Transfer-Encoding: base64

PFRBQkxFPgogIDxUUj4KICAgIDxURD5IVE1MPC9URD4KICAgIDxURD5hbHRlcm5hdGl2ZTwvVEQ+
CiAgPC9UUj4KPC9UQUJMRT4KCjxBIEhSRUY9Imh0dHA6Ly93d3cubmlnaHRzaGFkZS5vcmcuemEv
Ij5Ib21lPC9BPgo=

--unique-string
Content-Type: text/plain

Second alternative.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
"
			write-file-message-357)
  "Test `write-file-message' with some headers and inline multipart/alternative
   with a message/rfc822 part and a text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"

--unique-string
Content-Type: message/rfc822
Content-Disposition: inline

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
--unique-string
Content-Type: text/plain

Second alternative.
--unique-string--
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
"
			write-file-message-358)
  "Test `write-file-message' with some headers and multipart/alternative with a
   base64 message/rfc822 and a text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: message/rfc822
Content-Transfer-Encoding: base64

RnJvbTogb28gPG9Aby5vcmc+ClRvOiB0ZXN0QHR0dC5vcmcKU3ViamVjdDogc3ViamVjdApEYXRl
OiAxNCBBcHIgMjAwOCAxNjowMjoxMCArMDEwMApSZWZlcmVuY2VzOiB4eHgKClBsYWluIHRleHQg
Ym9keSBvZiBmb3J3YXJkZWQgbWVzc2FnZS4K

--unique-string
Content-Type: text/plain

Second alternative.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
"
			write-file-message-359)
  "Test `write-file-message' with some headers and multipart/alternative with a
   base64 text/html inline and a text/plain part inline."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: text/html
Content-Transfer-Encoding: base64
Content-Disposition: inline

PFRBQkxFPgogIDxUUj4KICAgIDxURD5IVE1MPC9URD4KICAgIDxURD5hbHRlcm5hdGl2ZTwvVEQ+
CiAgPC9UUj4KPC9UQUJMRT4KCjxBIEhSRUY9Imh0dHA6Ly93d3cubmlnaHRzaGFkZS5vcmcuemEv
Ij5Ib21lPC9BPgo=

--unique-string
Content-Type: text/plain
Content-Disposition: inline

Second alternative.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ('("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

#text/html [the first part]
"
			  ("<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
"
			   "text/html"))
			write-file-message-360)
  "Test `write-file-message' and `get-headers-part' with some headers and
   multipart/alternative with a base64 text/html attachment and a
   text/plain part attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: text/html
Content-Transfer-Encoding: base64
Content-Disposition: attachment
Content-Description: the first part

PFRBQkxFPgogIDxUUj4KICAgIDxURD5IVE1MPC9URD4KICAgIDxURD5hbHRlcm5hdGl2ZTwvVEQ+
CiAgPC9UUj4KPC9UQUJMRT4KCjxBIEhSRUY9Imh0dHA6Ly93d3cubmlnaHRzaGFkZS5vcmcuemEv
Ij5Ib21lPC9BPgo=

--unique-string
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: the second part

Second alternative.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler)
	     (fields))
	 (block ()
	   (list (with-output-to-string (stream)
		   (multiple-value-bind
		       (success msg)
		       (mh:write-file-message stream "home:Mail/folder/1"
					      *write-file-message-headers*)
		     (if success
			 (setq fields msg)
			 (progn
			   (format t "Error message: ~A" msg)
			   (return)))))
		 (multiple-value-list
		  (mh:get-headers-part fields 83)))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Nested first alternative.

"
			write-file-message-361)
  "Test `write-file-message' with some headers and multipart/alternative with a
   nested multipart/alternative attachment and a text/plain part
   attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: multipart/alternative; boundary=\"unique-string2\"
Content-Description: the first part

--unique-string2
Content-Type: text/plain
Content-Description: the nested first part

Nested first alternative.

--unique-string2
Content-Type: text/plain
Content-Description: the nested second part

Nested second alternative.
--unique-string2--
--unique-string
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: the second part

Second alternative.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Nested first part.

Nested second part.
"
			write-file-message-362)
  "Test `write-file-message' with some headers and multipart/alternative with a
   nested multipart/mixed attachment and a text/plain part
   attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/alternative; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: multipart/mixed; boundary=\"unique-string2\"
Content-Description: the first part

--unique-string2
Content-Type: text/plain
Content-Description: the nested first part

Nested first part.

--unique-string2
Content-Type: text/plain
Content-Description: the nested second part

Nested second part.
--unique-string2--
--unique-string
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: the second part

Second alternative.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

; FIX nested nested multipart


;;;; MIME multipart/mixed, all headers.

; FIX 400


;;;; MIME multipart/mixed, some headers.

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Body.
"
			write-file-message-450)
  "Test `write-file-message' with some headers and multipart/mixed with a single
   text/plain part and body text before the first boundary."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

    This is a MIME encoded message.
    This text should be skipped.

--unique-string
Content-Type: text/plain

Body.
--unique-string--
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Body.
"
			write-file-message-451)
  "Test `write-file-message' with some headers and multipart/mixed with a
   single text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: text/plain

Body.
--unique-string--
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

First part.
Second part.

Multiple paragraphs.

"
			write-file-message-452)
  "Test `write-file-message' with some headers and multipart/mixed with
   multiple text/plain parts."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: text/plain

First part.
--unique-string
Content-Type: text/plain

Second part.

Multiple paragraphs.

--unique-string--
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

First part.
<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>part</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
"
			write-file-message-453)
  "Test `write-file-message' with some headers and multipart/mixed with a
   text/plain and a text/html part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: text/plain

First part.
--unique-string
Content-Type: text/html

<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>part</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>part</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
Second part.
"
			write-file-message-454)
  "Test `write-file-message' with some headers and multipart/mixed with a
   text/html and a text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: text/html

<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>part</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
--unique-string
Content-Type: text/plain

Second part.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

An initial sentence in the body.

A second sentence of the body which starts the second paragraph and is
longer, so it includes a line break.  The third and final sentence, which
ends the paragraph and body.
Second part.
"
			write-file-message-455)
  "Test `write-file-message' with some headers and multipart/mixed with a
   base64 encoded text/plain part and a text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"

--unique-string
Content-Type: text/plain
Content-Transfer-Encoding: base64

QW4gaW5pdGlhbCBzZW50ZW5jZSBpbiB0aGUgYm9keS4KCkEgc2Vjb25kIHNl
bnRlbmNlIG9mIHRoZSBib2R5IHdoaWNoIHN0YXJ0cyB0aGUgc2Vjb25kIHBh
cmFncmFwaCBhbmQgaXMKbG9uZ2VyLCBzbyBpdCBpbmNsdWRlcyBhIGxpbmUg
YnJlYWsuICBUaGUgdGhpcmQgYW5kIGZpbmFsIHNlbnRlbmNlLCB3aGljaApl
bmRzIHRoZSBwYXJhZ3JhcGggYW5kIGJvZHkuCg==

--unique-string
Content-Type: text/plain

Second part.
--unique-string--
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
Second part.
"
			write-file-message-456)
  "Test `write-file-message' with some headers and multipart/mixed with a
   base64 text/html and a text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: text/html
Content-Transfer-Encoding: base64

PFRBQkxFPgogIDxUUj4KICAgIDxURD5IVE1MPC9URD4KICAgIDxURD5hbHRlcm5hdGl2ZTwvVEQ+
CiAgPC9UUj4KPC9UQUJMRT4KCjxBIEhSRUY9Imh0dHA6Ly93d3cubmlnaHRzaGFkZS5vcmcuemEv
Ij5Ib21lPC9BPgo=

--unique-string
Content-Type: text/plain

Second part.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
Second part.
"
			write-file-message-457)
  "Test `write-file-message' with some headers and inline multipart/mixed
   with a message/rfc822 part and a text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"

--unique-string
Content-Type: message/rfc822
Content-Disposition: inline

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
--unique-string
Content-Type: text/plain

Second part.
--unique-string--
" stream))
      (mh:with-fresh-state
       (block ()
	 (with-output-to-string (stream)
	   (multiple-value-bind
	       (success msg)
	       (mh:write-file-message stream "home:Mail/folder/1"
				      *write-file-message-headers*)
	     (or success
		 (progn
		   (format t "Error message: ~A" msg)
		   (return))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
Second part.
"
			write-file-message-458)
  "Test `write-file-message' with some headers and multipart/mixed with a
   base64 message/rfc822 and a text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: message/rfc822
Content-Transfer-Encoding: base64

RnJvbTogb28gPG9Aby5vcmc+ClRvOiB0ZXN0QHR0dC5vcmcKU3ViamVjdDogc3ViamVjdApEYXRl
OiAxNCBBcHIgMjAwOCAxNjowMjoxMCArMDEwMApSZWZlcmVuY2VzOiB4eHgKClBsYWluIHRleHQg
Ym9keSBvZiBmb3J3YXJkZWQgbWVzc2FnZS4K

--unique-string
Content-Type: text/plain

Second part.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
Second part.

<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
Fourth part.

End of fourth part.
"
			write-file-message-459)
  "Test `write-file-message' with some headers and multipart/mixed with a base64
   message/rfc822, a text/plain, a base64 text/html and a text/plain."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: message/rfc822
Content-Transfer-Encoding: base64

RnJvbTogb28gPG9Aby5vcmc+ClRvOiB0ZXN0QHR0dC5vcmcKU3ViamVjdDogc3ViamVjdApEYXRl
OiAxNCBBcHIgMjAwOCAxNjowMjoxMCArMDEwMApSZWZlcmVuY2VzOiB4eHgKClBsYWluIHRleHQg
Ym9keSBvZiBmb3J3YXJkZWQgbWVzc2FnZS4K

--unique-string
Content-Type: text/plain

Second part.

--unique-string
Content-Type: text/html
Content-Transfer-Encoding: base64

PFRBQkxFPgogIDxUUj4KICAgIDxURD5IVE1MPC9URD4KICAgIDxURD5hbHRlcm5hdGl2ZTwvVEQ+
CiAgPC9UUj4KPC9UQUJMRT4KCjxBIEhSRUY9Imh0dHA6Ly93d3cubmlnaHRzaGFkZS5vcmcuemEv
Ij5Ib21lPC9BPgo=

--unique-string
Content-Type: text/plain

Fourth part.

End of fourth part.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ('("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

#message/rfc822 [the first attachment]
Second part.

<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
#text/plain [the second attachment]
"
			("From: oo <o@o.org>
To: test@ttt.org
Subject: subject
Date: 14 Apr 2008 16:02:10 +0100
References: xxx

Plain text body of forwarded message.
"
			 "message/rfc822"))
			write-file-message-460)
  "Test `write-file-message' and `get-headers-part' with some headers and
   multipart/mixed with a base64 message/rfc822 attachment, a text/plain
   part, a base64 text/html part and a text/plain attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: message/rfc822
Content-Transfer-Encoding: base64
Content-Disposition: attachment
Content-Description: the first attachment

RnJvbTogb28gPG9Aby5vcmc+ClRvOiB0ZXN0QHR0dC5vcmcKU3ViamVjdDogc3ViamVjdApEYXRl
OiAxNCBBcHIgMjAwOCAxNjowMjoxMCArMDEwMApSZWZlcmVuY2VzOiB4eHgKClBsYWluIHRleHQg
Ym9keSBvZiBmb3J3YXJkZWQgbWVzc2FnZS4K

--unique-string
Content-Type: text/plain

Second part.

--unique-string
Content-Type: text/html
Content-Transfer-Encoding: base64

PFRBQkxFPgogIDxUUj4KICAgIDxURD5IVE1MPC9URD4KICAgIDxURD5hbHRlcm5hdGl2ZTwvVEQ+
CiAgPC9UUj4KPC9UQUJMRT4KCjxBIEhSRUY9Imh0dHA6Ly93d3cubmlnaHRzaGFkZS5vcmcuemEv
Ij5Ib21lPC9BPgo=

--unique-string
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: the second attachment

Fourth part.

End of fourth part.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler)
	     (fields))
	 (block ()
	   (list (with-output-to-string (stream)
		   (multiple-value-bind
		       (success msg)
		       (mh:write-file-message stream "home:Mail/folder/1"
					      *write-file-message-headers*)
		     (if success
			 (setq fields msg)
			 (progn
			   (format t "Error message: ~A" msg)
			   (return)))))
		 (multiple-value-list
		  (mh:get-headers-part fields 83)))))))))

(deftest write-file-message ('("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

#message/rfc822 [the first attachment]
Nested first alternative.

<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
#text/plain [the second attachment]
"
			  ("From: oo <o@o.org>
To: test@ttt.org
Subject: subject
Date: 14 Apr 2008 16:02:10 +0100
References: xxx

Plain text body of forwarded message.
"
			 "message/rfc822"))
			write-file-message-461)
  "Test `write-file-message' and `get-headers-part' with some headers and
   multipart/mixed with a base64 message/rfc822 attachment, a nested
   multipart/alternative part, a base64 text/html part and a text/plain
   attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: message/rfc822
Content-Transfer-Encoding: base64
Content-Disposition: attachment
Content-Description: the first attachment

RnJvbTogb28gPG9Aby5vcmc+ClRvOiB0ZXN0QHR0dC5vcmcKU3ViamVjdDogc3ViamVjdApEYXRl
OiAxNCBBcHIgMjAwOCAxNjowMjoxMCArMDEwMApSZWZlcmVuY2VzOiB4eHgKClBsYWluIHRleHQg
Ym9keSBvZiBmb3J3YXJkZWQgbWVzc2FnZS4K

--unique-string
Content-Type: multipart/alternative; boundary=\"unique-string2\"
Content-Description: the nested multipart

--unique-string2
Content-Type: text/plain
Content-Description: the nested first part

Nested first alternative.

--unique-string2
Content-Type: text/plain
Content-Description: the nested second part

Nested second alternative.
--unique-string2--
--unique-string
Content-Type: text/html
Content-Transfer-Encoding: base64

PFRBQkxFPgogIDxUUj4KICAgIDxURD5IVE1MPC9URD4KICAgIDxURD5hbHRlcm5hdGl2ZTwvVEQ+
CiAgPC9UUj4KPC9UQUJMRT4KCjxBIEhSRUY9Imh0dHA6Ly93d3cubmlnaHRzaGFkZS5vcmcuemEv
Ij5Ib21lPC9BPgo=

--unique-string
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: the second attachment

Fourth part.

End of fourth part.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler)
	     (fields))
	 (block ()
	   (list (with-output-to-string (stream)
		   (multiple-value-bind
		       (success msg)
		       (mh:write-file-message stream "home:Mail/folder/1"
					      *write-file-message-headers*)
		     (if success
			 (setq fields msg)
			 (progn
			   (format t "Error message: ~A" msg)
			   (return)))))
		 (multiple-value-list
		  (mh:get-headers-part fields 83)))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

#message/rfc822 [the first attachment]
Nested first part.

Nested second part.
<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
#text/plain [the second attachment]
"
			write-file-message-462)
  "Test `write-file-message' with some headers and multipart/mixed with a base64
   message/rfc822 attachment, a nested multipart/mixed part, a base64
   text/html part and a text/plain attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: message/rfc822
Content-Transfer-Encoding: base64
Content-Disposition: attachment
Content-Description: the first attachment

RnJvbTogb28gPG9Aby5vcmc+ClRvOiB0ZXN0QHR0dC5vcmcKU3ViamVjdDogc3ViamVjdApEYXRl
OiAxNCBBcHIgMjAwOCAxNjowMjoxMCArMDEwMApSZWZlcmVuY2VzOiB4eHgKClBsYWluIHRleHQg
Ym9keSBvZiBmb3J3YXJkZWQgbWVzc2FnZS4K

--unique-string
Content-Type: multipart/mixed; boundary=\"unique-string2\"
Content-Description: the nested multipart

--unique-string2
Content-Type: text/plain
Content-Description: the nested first part

Nested first part.

--unique-string2
Content-Type: text/plain
Content-Description: the nested second part

Nested second part.
--unique-string2--
--unique-string
Content-Type: text/html
Content-Transfer-Encoding: base64

PFRBQkxFPgogIDxUUj4KICAgIDxURD5IVE1MPC9URD4KICAgIDxURD5hbHRlcm5hdGl2ZTwvVEQ+
CiAgPC9UUj4KPC9UQUJMRT4KCjxBIEhSRUY9Imh0dHA6Ly93d3cubmlnaHRzaGFkZS5vcmcuemEv
Ij5Ib21lPC9BPgo=

--unique-string
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: the second attachment

Fourth part.

End of fourth part.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ('("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Nested first part.

Nested second part.
#image/png [the nested image] ((name . nested image.png))
<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
#text/plain [the text attachment]
"
			  ("âPNG

   IHDR   
   
   PXÍ   	pHYs     öú   tIMEÿ*ΩhJ@   oIDAT”EèA!gúﬁÿ◊˜çï˙zH(≤1„ƒÔ˚€óYï'µ^µ*O’SY	l ATÖ`Ê∫ú'ä¥Ô∂Áo¥MßõO`‡éõ¥z≤/‚Q:i≤g‡7wÚHß7¸h˛K_≤l«˚FÒsÒ 7    IENDÆB`Ç"
			    "image/png; name=\"nested image.png\""))
			write-file-message-463)
  "Test `write-file-message' with some headers and multipart/mixed with a nested
   multipart/related part (which has a multipart/mixed), a base64
   image/png, a base64 HTML part and a plain/text attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: multipart/related; boundary=\"unique-string2\"
Content-Description: the nested multipart related

--unique-string2
Content-Type: multipart/mixed; boundary=\"unique-string3\"
Content-Description: the nested nested multipart mixed

--unique-string3
Content-Type: text/plain
Content-Description: the nested nested first part

Nested first part.

--unique-string3
Content-Type: text/plain
Content-Description: the nested nested second part

Nested second part.
--unique-string3--
--unique-string2
Content-Type: image/png; name=\"nested image.png\"
Content-Description: the nested image
Content-Transfer-Encoding: base64

iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAIAAAACUFjqAAAACXBIWXMAAAsTAAALEwEAmpwYAAAA
B3RJTUUH2AUPESoVvWhKQAAAAG9JREFUGNNFj0EOAyEMA2ec3tjX942V+gx6SCgRsgQx48Tv+wPb
l1mVJ7VetSpP1VNZCWwAQVSFYOYaupwnAoq077bnbwO0Tacdm39PYOCOm7R6si/iUTppsmfgAQ4G
N3fySKc3/Gj+S1+ybMf7A0bxB3PxAAg3AAAAAElFTkSuQmCC

--unique-string2--
--unique-string
Content-Type: text/html
Content-Transfer-Encoding: base64

PFRBQkxFPgogIDxUUj4KICAgIDxURD5IVE1MPC9URD4KICAgIDxURD5hbHRlcm5hdGl2ZTwvVEQ+
CiAgPC9UUj4KPC9UQUJMRT4KCjxBIEhSRUY9Imh0dHA6Ly93d3cubmlnaHRzaGFkZS5vcmcuemEv
Ij5Ib21lPC9BPgo=

--unique-string
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: the text attachment

Fourth part.

End of fourth part.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler)
	     (fields))
	 (block ()
	   (list (with-output-to-string (stream)
		   (multiple-value-bind
		       (success msg)
		       (mh:write-file-message stream "home:Mail/folder/1"
					      *write-file-message-headers*)
		     (if success
			 (setq fields msg)
			 (progn
			   (format t "Error message: ~A" msg)
			   (return)))))
		 (multiple-value-list
		  (mh:get-headers-part fields 123)))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Nested first alternative.

#image/png [the nested image]
<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
#text/plain [the text attachment]
"
			write-file-message-464)
  "Test `write-file-message' with some headers and multipart/mixed with a nested
   multipart/related part (which has a multipart/alternative), a base64
   image/png, a base64 HTML part and a plain/text attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: multipart/related; boundary=\"unique-string2\"
Content-Description: the nested multipart related

--unique-string2
Content-Type: multipart/alternative; boundary=\"unique-string3\"
Content-Description: the nested nested multipart mixed

--unique-string3
Content-Type: text/plain
Content-Disposition: inline
Content-Description: the nested nested first alternative

Nested first alternative.

--unique-string3
Content-Type: text/plain
Content-Disposition: inline
Content-Description: the nested nested second alternative

Nested second alternative.
--unique-string3--
--unique-string2
Content-Type: image/png
Content-Description: the nested image
Content-Transfer-Encoding: base64

iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAIAAAACUFjqAAAACXBIWXMAAAsTAAALEwEAmpwYAAAA
B3RJTUUH2AUPESoVvWhKQAAAAG9JREFUGNNFj0EOAyEMA2ec3tjX942V+gx6SCgRsgQx48Tv+wPb
l1mVJ7VetSpP1VNZCWwAQVSFYOYaupwnAoq077bnbwO0Tacdm39PYOCOm7R6si/iUTppsmfgAQ4G
N3fySKc3/Gj+S1+ybMf7A0bxB3PxAAg3AAAAAElFTkSuQmCC

--unique-string2--
--unique-string
Content-Type: text/html
Content-Transfer-Encoding: base64

PFRBQkxFPgogIDxUUj4KICAgIDxURD5IVE1MPC9URD4KICAgIDxURD5hbHRlcm5hdGl2ZTwvVEQ+
CiAgPC9UUj4KPC9UQUJMRT4KCjxBIEhSRUY9Imh0dHA6Ly93d3cubmlnaHRzaGFkZS5vcmcuemEv
Ij5Ib21lPC9BPgo=

--unique-string
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: the text attachment

Fourth part.

End of fourth part.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Nested first alternative.

#image/png [the nested image]
<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
"
			write-file-message-465)
  "Test `write-file-message' with some headers and multipart/mixed with a nested
   multipart/related part (which has a multipart/alternative) and a base64
   image/png where the end boundary is missing."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: multipart/related; boundary=\"unique-string2\"
Content-Description: the nested multipart related

--unique-string2
Content-Type: multipart/alternative; boundary=\"unique-string3\"
Content-Description: the nested nested multipart mixed

--unique-string3
Content-Type: text/plain
Content-Disposition: inline
Content-Description: the nested nested first alternative

Nested first alternative.

--unique-string3
Content-Type: text/plain
Content-Disposition: inline
Content-Description: the nested nested second alternative

Nested second alternative.
--unique-string3--
--unique-string2
Content-Type: image/png
Content-Description: the nested image
Content-Transfer-Encoding: base64

iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAIAAAACUFjqAAAACXBIWXMAAAsTAAALEwEAmpwYAAAA
B3RJTUUH2AUPESoVvWhKQAAAAG9JREFUGNNFj0EOAyEMA2ec3tjX942V+gx6SCgRsgQx48Tv+wPb
l1mVJ7VetSpP1VNZCWwAQVSFYOYaupwnAoq077bnbwO0Tacdm39PYOCOm7R6si/iUTppsmfgAQ4G
N3fySKc3/Gj+S1+ybMf7A0bxB3PxAAg3AAAAAElFTkSuQmCC

--unique-string2--
--unique-string
Content-Type: text/html
Content-Transfer-Encoding: base64

PFRBQkxFPgogIDxUUj4KICAgIDxURD5IVE1MPC9URD4KICAgIDxURD5hbHRlcm5hdGl2ZTwvVEQ+
CiAgPC9UUj4KPC9UQUJMRT4KCjxBIEhSRUY9Imh0dHA6Ly93d3cubmlnaHRzaGFkZS5vcmcuemEv
Ij5Ib21lPC9BPgo=
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Text about the message which follows.

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
An ending comment.

"
			write-file-message-466)
  "Test `write-file-message' with some headers and multipart/mixed with an
   message/rfc822 straddled by text/plain parts."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"=-=\"
Content-Transfer-Encoding: 8bit

--=-=
Content-Type: text/plain

Text about the message which follows.

--=-=
Content-Type: message/rfc822
Content-Description: a forwarded message

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
--=-=
Content-Type: text/plain

An ending comment.

--=-=--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

Text about the message which follows.

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
An ending comment.

"
			write-file-message-467)
  "Test `write-file-message' with some headers and multipart/mixed with an
   message/rfc822 straddled by implied text/plain parts."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"=-=\"
Content-Transfer-Encoding: 8bit

--=-=

Text about the message which follows.

--=-=
Content-Type: message/rfc822
Content-Description: a forwarded message

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
--=-=

An ending comment.

--=-=--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
Plain text body of forwarded message, which runs on for a long stretch, to the end of the paragraph, in fact.  Why?  Because it is in quoted printable encoding.
"
			write-file-message-468)
  "Test `write-file-message' with some headers and multipart/mixed with an
   message/rfc822 and a quoted-printable text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"=-=\"
Content-Transfer-Encoding: 8bit

--=-=
Content-Type: message/rfc822
Content-Description: a forwarded message

To: test@ttt.org
From: oo <o@o.org>
Date: 14 Apr 2008 16:02:10 +0100
Subject: subject

Plain text body of forwarded message.
--=-=
Content-Type: text/plain
Content-Transfer-Encoding: quoted-printable

Plain=20text=20body=20of=20forwarded=20message,=20which=20=
runs=20on=20for=20a=20long=20stretch,=20to=20the=20end=20of=20the=20=
paragraph,=20in=20fact=2E=20=20Why=3F=20=20Because=20it=20is=20in=20quoted=20=
printable=20encoding=2E
--=-=--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler))
	 (block ()
	   (with-output-to-string (stream)
	     (multiple-value-bind
		 (success msg)
		 (mh:write-file-message stream "home:Mail/folder/1"
					*write-file-message-headers*)
	       (or success
		   (progn
		     (format t "Error message: ~A" msg)
		     (return)))))))))))

(deftest write-file-message ('("To: test@ttt.org
From: nn <n@n.org>
Date: 15 Apr 2008 16:02:10 +0100
Subject: ss1

#message/rfc822 [the first attachment]
Nested first part.

Nested second part.
<TABLE>
  <TR>
    <TD>HTML</TD>
    <TD>alternative</TD>
  </TR>
</TABLE>

<A HREF=\"http://www.nightshade.org.za/\">Home</A>
#text/plain [the second attachment]
"
			  ("Fourth part.

End of fourth part.
"
			   "text/plain"))
			write-file-message-469)
  "Test `write-file-message' and `get-headers-part' with some headers and
   multipart/mixed with a base64 message/rfc822 attachment, a nested inline
   multipart/mixed part, a base64 text/html part and a text/plain
   attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: multipart/mixed; boundary=\"unique-string\"
Content-Transfer-Encoding: 8bit

--unique-string
Content-Type: message/rfc822
Content-Transfer-Encoding: base64
Content-Disposition: attachment
Content-Description: the first attachment

RnJvbTogb28gPG9Aby5vcmc+ClRvOiB0ZXN0QHR0dC5vcmcKU3ViamVjdDogc3ViamVjdApEYXRl
OiAxNCBBcHIgMjAwOCAxNjowMjoxMCArMDEwMApSZWZlcmVuY2VzOiB4eHgKClBsYWluIHRleHQg
Ym9keSBvZiBmb3J3YXJkZWQgbWVzc2FnZS4K

--unique-string
Content-Type: multipart/mixed; boundary=\"unique-string2\"
Content-Description: the nested multipart
Content-Disposition: inline

--unique-string2
Content-Type: text/plain
Content-Description: the nested first part

Nested first part.

--unique-string2
Content-Type: text/plain
Content-Description: the nested second part

Nested second part.
--unique-string2--
--unique-string
Content-Type: text/html
Content-Transfer-Encoding: base64

PFRBQkxFPgogIDxUUj4KICAgIDxURD5IVE1MPC9URD4KICAgIDxURD5hbHRlcm5hdGl2ZTwvVEQ+
CiAgPC9UUj4KPC9UQUJMRT4KCjxBIEhSRUY9Imh0dHA6Ly93d3cubmlnaHRzaGFkZS5vcmcuemEv
Ij5Ib21lPC9BPgo=

--unique-string
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: the second attachment

Fourth part.

End of fourth part.
--unique-string--
" stream))
      (mh:with-fresh-state
       (let ((mh:*html-handler* #'write-file-message-html-handler)
	     (fields))
	 (block ()
	   (list (with-output-to-string (stream)
		   (multiple-value-bind
		       (success msg)
		       (mh:write-file-message stream "home:Mail/folder/1"
					      *write-file-message-headers*)
		     (if success
			 (setq fields msg)
			 (progn
			   (format t "Error message: ~A" msg)
			   (return)))))
		 (multiple-value-list
		  ;; FIX should be 289, test `file-position'?
		  (mh:get-headers-part fields 287)))))))))


;;;; `write-file-message' errors.

(defvar *write-file-message-mail-1* "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(deftest write-file-message ('("From: nn <n@n.org>
To: test@ttt.org

Body.
"
			  "Expected MIME version 1.0")
			write-file-message-506)
  "Test `write-file-message' with the wrong MIME version."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/2222"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 0.0
Content-Type: text/plain

Body.
" stream))
      (mh:with-fresh-state
       (let (err)
	 (list
	  (with-output-to-string (stream)
	    (setq err
		  (nth-value
		   1
		   (mh:write-file-message stream "home:Mail/folder/2222"
					  '("From" "To")))))
	  err))))))


;;;; `get-headers-part' errors.

(deftest write-file-message (t write-file-message-550)
  "Test `get-headers-part' requesting multiple messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *write-file-message-mail-1* stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *write-file-message-mail-1* stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:get-headers-part "folder" '(3 11) 1)
	   (error () (setq ret t))))))))

(deftest write-file-message (() write-file-message-551)
  "Test `get-headers-part' with headers ()."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *write-file-message-mail-1* stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (mh:get-headers-part () 1)))))
