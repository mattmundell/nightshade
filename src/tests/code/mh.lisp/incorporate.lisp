;;; Tests of mh:incorporate.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))


;;;; Local only.

(deftest incorporate ('(t () ()) incorporate-1)
  "Test `incorporate', on an empty local drop."
  (with-test-dir (dir "Mail/inbox/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Unseen-Sequence: in~%"))
      (mh:with-fresh-state
       (list (mh:incorporate (list (mh:make-drop
				    :local
				    "home:local")))
	     (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "inbox" '("in")))))))

(deftest incorporate ('(t (1) (1)
"To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-Id: xxxxx@ff.ff

Body.
")
		      incorporate-2)
  "Test `incorporate', on a local drop with one message, where the inbox
   needs to be created."
  (with-test-dir (dir "Mail/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Unseen-Sequence: in~%"))
      (to-file (stream "home:local")
	(format stream "From a@a
To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: xxxxx@ff.ff

Body.
"))
      (mh:with-fresh-state
       (list (mh:incorporate (list (mh:make-drop :local "home:local")))
	     (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "inbox" '("in"))
	     (with-output-to-string (stream)
	       (mh:write-message "inbox" "1" stream)))))))

(deftest incorporate ('(t (11 12 13) (12 13)
"To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-Id: xxxxx@ff.ff

Body.

"
"To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
")
		      incorporate-3)
  "Test `incorporate', on a local drop with multiple messages, where the
   inbox contains a message beforehand."
  (with-test-dir (dir "Mail/inbox/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Unseen-Sequence: in~%"))
      (to-file (stream "home:Mail/inbox/11" :if-does-not-exist :create)
	(format stream "To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 13 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: 1111@ff.ff

Example.
"))
      (to-file (stream "home:local")
	(format stream "From a@a
To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: xxxxx@ff.ff

Body.

From a@a
To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
"))
      (mh:with-fresh-state
       (list (mh:incorporate (list (mh:make-drop :local "home:local")))
	     (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "inbox" '("in"))
	     (with-output-to-string (stream)
	       (mh:write-message "inbox" "12" stream))
	     (with-output-to-string (stream)
	       (mh:write-message "inbox" "13" stream)))))))

(deftest incorporate ('(t (11 12 13) (12 13)
"To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-Id: xxxxx@ff.ff

Body.

"
"To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
")
		      incorporate-4)
  "Test `incorporate', on a local drop with multiple messages, where the
   inbox contains a message beforehand, giving the name of the inbox."
  (with-test-dir (dir "Mail/folder/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Unseen-Sequence: in~%"))
      (to-file (stream "home:Mail/folder/11" :if-does-not-exist :create)
	(format stream "To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 13 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: 1111@ff.ff

Example.
"))
      (to-file (stream "home:local")
	(format stream "From a@a
To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: xxxxx@ff.ff

Body.

From a@a
To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
"))
      (mh:with-fresh-state
       (list (mh:incorporate (list (mh:make-drop :local "home:local"))
			     "folder")
	     (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("in"))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "12" stream))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "13" stream)))))))

(deftest incorporate ('(t (11 12 13) (12 13)
"To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-Id: xxxxx@ff.ff

Body.

"
"To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
")
		      incorporate-5)
  "Test `incorporate', on a local drop with multiple messages, where the
   inbox contains a message beforehand, giving the name of the inbox with a
   trailing slash and leading +."
  (with-test-dir (dir "Mail/folder/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Unseen-Sequence: in~%"))
      (to-file (stream "home:Mail/folder/11" :if-does-not-exist :create)
	(format stream "To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 13 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: 1111@ff.ff

Example.
"))
      (to-file (stream "home:local")
	(format stream "From a@a
To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: xxxxx@ff.ff

Body.

From a@a
To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
"))
      (mh:with-fresh-state
       (list (mh:incorporate (list (mh:make-drop :local "home:local"))
			     "+folder/")
	     (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("in"))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "12" stream))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "13" stream)))))))

(deftest incorporate ('(t (11 12 13) (12 13)
"To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-Id: xxxxx@ff.ff

Body.

"
"To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
")
		      incorporate-6)
  "Test `incorporate', on a local drop with multiple messages, where the
   inbox contains a message beforehand, specifying the name of the inbox in a component."
  (with-test-dir (dir "Mail/folder/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Inbox: +folder/~%")
	(format stream "Unseen-Sequence: in~%"))
      (to-file (stream "home:Mail/folder/11" :if-does-not-exist :create)
	(format stream "To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 13 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: 1111@ff.ff

Example.
"))
      (to-file (stream "home:local")
	(format stream "From a@a
To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: xxxxx@ff.ff

Body.

From a@a
To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
"))
      (mh:with-fresh-state
       (list (mh:incorporate (list (mh:make-drop :local "home:local"))
			     "+folder/")
	     (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("in"))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "12" stream))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "13" stream)))))))

(deftest incorporate ('(t (11 12 13) (12 13)
"To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-Id: xxxxx@ff.ff

Body.


"
"To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
")
		      incorporate-7)
  "Test `incorporate', on a local drop with multiple messages, where the
   first message ends in an even number of blank lines."
  (with-test-dir (dir "Mail/folder/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Inbox: +folder/~%")
	(format stream "Unseen-Sequence: in~%"))
      (to-file (stream "home:Mail/folder/11" :if-does-not-exist :create)
	(format stream "To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 13 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: 1111@ff.ff

Example.
"))
      (to-file (stream "home:local")
	(format stream "From a@a
To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: xxxxx@ff.ff

Body.


From a@a
To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
"))
      (mh:with-fresh-state
       (list (mh:incorporate (list (mh:make-drop :local "home:local"))
			     "+folder/")
	     (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("in"))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "12" stream))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "13" stream)))))))


;;;; Empty drop.

(deftest incorporate ('(t ()) incorporate-30)
  "Test `incorporate', on an empty drop list."
  (with-test-dir (dir "Mail/inbox/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Unseen-Sequence: in~%"))
      (to-file (stream "home:local")
	(format stream "From a@a
To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: xxxxx@ff.ff

Body.

From a@a
To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
"))
      (mh:with-fresh-state
       (list (mh:incorporate '())
	     (mh:pick-messages "inbox" '(:all)))))))


;;;; Errors.

(deftest incorporate ('(t (11 12 13) (12 13)
"
Body.

"
"To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
")
		      incorporate-60)
  "Test `incorporate', on a local drop with multiple messages, where the
   first message is missing all headers."
  (with-test-dir (dir "Mail/folder/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Inbox: +folder/~%")
	(format stream "Unseen-Sequence: in~%"))
      (to-file (stream "home:Mail/folder/11" :if-does-not-exist :create)
	(format stream "To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 13 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: 1111@ff.ff

Example.
"))
      (to-file (stream "home:local")
	(format stream "From a@a

Body.

From a@a
To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

Two lines.
"))
      (mh:with-fresh-state
       (list (mh:incorporate (list (mh:make-drop :local "home:local"))
			     "+folder/")
	     (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("in"))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "12" stream))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "13" stream)))))))

(deftest incorporate ('(t (11 12) (12)
"To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-Id: xxxxx@ff.ff

Body.

From a@a

Body two.

Two lines.
")
		      incorporate-61)
  "Test `incorporate', on a local drop with multiple messages, where the
   second message is missing all headers."
  (with-test-dir (dir "Mail/folder/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Inbox: +folder/~%")
	(format stream "Unseen-Sequence: in~%"))
      (to-file (stream "home:Mail/folder/11" :if-does-not-exist :create)
	(format stream "To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 13 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: 1111@ff.ff

Example.
"))
      (to-file (stream "home:local")
	(format stream "From a@a
To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 15 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: xxxxx@ff.ff

Body.

From a@a

Body two.

Two lines.
"))
      (mh:with-fresh-state
       (list (mh:incorporate (list (mh:make-drop :local "home:local"))
			     "+folder/")
	     (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("in"))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "12" stream)))))))

(deftest incorporate ('(t (11 12 13) (12 13)
			  "To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body.

"
			  "To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

From a@a

Two lines.
")
		      incorporate-62)
  "Test `incorporate', on a local drop with multiple messages, where the
   second message includes \"From \" in the body."
  (with-test-dir (dir "Mail/folder/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Inbox: +folder/~%")
	(format stream "Unseen-Sequence: in~%"))
      (to-file (stream "home:Mail/folder/11" :if-does-not-exist :create)
	(format stream "To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 13 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: 1111@ff.ff

Example.
"))
      (to-file (stream "home:local")
	(format stream "From a@a
To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body.

From a@a
To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

From a@a

Two lines.
"))
      (mh:with-fresh-state
       (list (mh:incorporate (list (mh:make-drop :local "home:local"))
			     "+folder/")
	     (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("in"))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "12" stream))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "13" stream)))))))

;; FIX maybe ">From" should become "From" in the resulting messages
;; FIX     test for when msg really contains ">From"
;;
(deftest incorporate ('(t (11 12 13) (12 13)
"To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body.

"
   "To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

>From a@a
Note: this looks like a header

Two lines.
")
		      incorporate-63)
  "Test `incorporate', on a local drop with multiple messages, where the
   second message includes \"From \" followed by a header-like line in the
   body."
  (with-test-dir (dir "Mail/folder/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Inbox: +folder/~%")
	(format stream "Unseen-Sequence: in~%"))
      (to-file (stream "home:Mail/folder/11" :if-does-not-exist :create)
	(format stream "To: test@ttt.org
From: Name Surname <n.surname@name.org>
Subject: Test
Date: 13 Apr 2008 16:02:10 +0100
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
Message-ID: 1111@ff.ff

Example.
"))
      (to-file (stream "home:local")
	(format stream "From a@a
To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body.

From a@a
To: test@ttt.org
From: two <two@2.org>
Subject: 222
Date: 16 Apr 2008 16:02:10 +0100

Body two.

>From a@a
Note: this looks like a header

Two lines.
"))
      (mh:with-fresh-state
       (list (mh:incorporate (list (mh:make-drop :local "home:local"))
			     "+folder/")
	     (mh:pick-messages "folder" '(:all))
	     (mh:pick-messages "folder" '("in"))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "12" stream))
	     (with-output-to-string (stream)
	       (mh:write-message "folder" "13" stream)))))))
