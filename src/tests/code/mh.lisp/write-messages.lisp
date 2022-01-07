;;; Tests of mh:write-messages.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

; FIX Nested forwarded messages (---..--- Start of forwarded message ---...---)

(defvar *write-messages-mail-1* "To: test@ttt.org
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: ss1s2s3s4s5s6s7s8s9s0sasbscsdsesfsgshsisjskslsmsnss
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar *write-messages-mail-2* "To: test2@two.org.za
From: short@name.org
Subject: subject
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")

(defvar *write-messages-mail-3* "To: three@three.org
From: short@name.org
Subject: test 3
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")


;;;; Plain message, all headers.

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
"
			write-messages-1)
  "Test `write-messages', implying all headers."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
" stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))


;;;; Plain message, some headers.

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100

Body.
"
			write-messages-50)
  "Test `write-messages' with some headers and a plain message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100

Body.
" stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder/" '("1")))))))

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
"
			write-messages-51)
  "Test `write-messages' with some headers and an empty body."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string "To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
" stream))
      (mh:with-fresh-state
       (with-output-to-string (stream)
	 (mh:write-messages stream "+folder" '(1)))))))


;;;; MIME text, all headers.

;; FIX 100


;;;; MIME text, some headers.

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
"
			write-messages-150)
  "Test `write-messages' with some headers, text/plain and an empty body."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "+folder/" '("1")))))))

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain

Body.
"
			write-messages-151)
  "Test `write-messages' with some headers and text/plain."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(2222)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-152)
  "Test `write-messages' with some headers, text/plain and base64 encoding."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Disposition: inline
"
			write-messages-153)
  "Test `write-messages' with some headers, text/plain and an empty body
   explicitly inlined."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Disposition: inline

Body.
"
			write-messages-154)
  "Test `write-messages' with some headers and text/plain explicitly
   inlined."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-155)
  "Test `write-messages' with some headers, base64 text/plain explicitly
   inlined."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: Example body.
"
			write-messages-156)
  "Test `write-messages' with some headers, text/plain and an empty body
   attached."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: text/plain
Content-Disposition: attachment
Content-Description: Example body.

Body.
"
			write-messages-157)
  "Test `write-messages' with some headers and text/plain attached."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-158)
  "Test `write-messages' with some headers, base64 text/plain attached."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-159)
  "Test `write-messages' with some headers, base64 text/plain attached,
   requesting a single missing header."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-160)
  "Test `write-messages' with some headers, base64 text/plain attached,
   requesting a missing header amongst the headers."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: application/example
Content-Disposition: inline
Content-Description: Example inline application.

This should be an attachment, even though it is specified as inline, so
that the client can decide what to do with it.
"
			write-messages-161)
  "Test `write-messages' with some headers, an inline application
   attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

; FIX inline, attachment, assumed versions of all tests below


;;;; MIME message/*, all headers.

; FIX 200


;;;; MIME message/*, some headers.

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-250)
  "Test `write-messages' with some headers and inline message/rfc822."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-251)
  "Test `write-messages' with some headers and message/rfc822."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-252)
  "Test `write-messages' with some headers and attachment message/rfc822."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: [Forward: subject]
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: message/rfc822
Content-Transfer-Encoding: base64

RnJvbTogb28gPG9Aby5vcmc+ClRvOiB0ZXN0QHR0dC5vcmcKU3ViamVjdDogc3ViamVjdApEYXRl
OiAxNCBBcHIgMjAwOCAxNjowMjoxMCArMDEwMApSZWZlcmVuY2VzOiB4eHgKClBsYWluIHRleHQg
Ym9keSBvZiBmb3J3YXJkZWQgbWVzc2FnZS4K
"
			write-messages-253)
  "Test `write-messages' with some headers and base64 message/rfc822."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-254)
  "Test `write-messages' with some headers and base64 message/rfc822
   attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-255)
  "Test `write-messages' with some headers and base64 message/rfc822
   inline."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: Mail failure
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: message/delivery-status

Reporting-MTA: dns; foo.org
Arrival-Date: 15 Apr 2008 16:02:10 +0100

Action: fail
Remote-MTA: dns; two.org
"
			write-messages-256)
  "Test `write-messages' with some headers and message/delivery-status."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: Mail failure
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 1.0
Content-Type: message/delivery-status
Content-Transfer-Encoding: base64

UmVwb3J0aW5nLU1UQTogZG5zOyBmb28ub3JnCkFycml2YWwtRGF0ZTogMTU
gQXByIDIwMDggMTY6MDI6MTAgKzAxMDAKCkFjdGlvbjogZmFpbApSZW1vdGU
tTVRBOiBkbnM7IHR3by5vcmcK
"
			write-messages-257)
  "Test `write-messages' with some headers and base64
   message/delivery-status."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

; FIX deliver-status [base64] inline,attachment


;;;; MIME multipart/alternative, all headers.

; FIX 300


;;;; MIME multipart/alternative, some headers.

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-350)
  "Test `write-messages' with some headers and multipart/alternative with a
   single text/plain part and body text before the first boundary."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-351)
  "Test `write-messages' with some headers and multipart/alternative with a
   single text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-352)
  "Test `write-messages' with some headers and multipart/alternative with
   multiple text/plain parts."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	(mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-353)
  "Test `write-messages' with some headers and multipart/alternative with a
   text/plain and a text/html part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-354)
  "Test `write-messages' with some headers and multipart/alternative with a
   text/html and a text/plain part."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-362)
  "Test `write-messages' with some headers and multipart/alternative with a
   nested multipart/mixed attachment and a text/plain part
   attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

; FIX nested nested multipart


;;;; MIME multipart/mixed, all headers.

; FIX 400


;;;; MIME multipart/mixed, some headers.

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-450)
  "Test `write-messages' with some headers and multipart/mixed with a single
   text/plain part and body text before the first boundary."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))

(deftest write-messages ("To: test@ttt.org
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
"
			write-messages-469)
  "Test `write-messages' with some headers and
   multipart/mixed with a base64 message/rfc822 attachment, a nested inline
   multipart/mixed part, a base64 text/html part and a text/plain
   attachment."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(1)))))))


;;;; Multiple messages.

(deftest write-messages ((concat *write-messages-mail-1*
				 *write-messages-mail-1*)
			 write-messages-500)
  "Test `write-messages' requesting multiple messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '(3 11)))
	   (error () (setq ret t))))))))

(deftest write-messages ((concat *write-messages-mail-1*
				 *write-messages-mail-3*)
			 write-messages-501)
  "Test `write-messages' requesting many with a sequence."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-2* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-3* stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 22" stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '("trash")))
	   (error () (setq ret t))))))))

(deftest write-messages ((concat *write-messages-mail-1*
				 *write-messages-mail-2*
				 *write-messages-mail-3*)
			 write-messages-502)
  "Test `write-messages' requesting all."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-2* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-3* stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 22" stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" '(:all)))
	   (error () (setq ret t))))))))


;;;; `write-messages' errors.

(deftest write-messages (t write-messages-550)
  "Test `write-messages' requesting all messages."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" :all))
	   (error () (setq ret t))))))))

(deftest write-messages (t write-messages-551)
  "Test `write-messages' requesting a sequence."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/folder/3"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/11"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *write-messages-mail-1* stream))
      (to-file (stream "home:Mail/folder/.mh_sequences"
		       :if-does-not-exist :create)
	(write-line "trash: 3 11 22" stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" "trash"))
	   (error () (setq ret t))))))))

(deftest write-messages (t write-messages-552)
  "Test `write-messages' with an erroneous symbol message."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" :cur stream))
	   (error () (setq ret t))))))))

(deftest write-messages (t write-messages-553)
  "Test `write-messages' with the all string."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (with-output-to-string (stream)
	       (mh:write-messages stream "folder" "all" stream))
	   (error () (setq ret t))))))))

(deftest write-messages ("To: test@ttt.org
From: nn <n@n.org>
Subject: ss1
Date: 15 Apr 2008 16:02:10 +0100
Mime-Version: 0.0
Content-Type: text/plain

Body.
"
			 write-messages-554)
  "Test `write-messages' with the wrong MIME version."
  (with-test-dir (folder "Mail/folder/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (with-output-to-string (stream)
	 (mh:write-messages stream "folder" '(2222)))))))
