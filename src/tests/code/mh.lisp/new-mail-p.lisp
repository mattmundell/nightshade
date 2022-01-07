;;; Tests of mh:new-mail-p.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))


;;;; Local only.

(deftest new-mail-p (() new-mail-p-1)
  "Test `new-mail-p', on an empty local drop."
  (with-test-dir (dir "Mail/folder/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (mh:new-mail-p (list (mh:make-drop
			     :local
			     "home:local")))))))

(deftest new-mail-p (t new-mail-p-2)
  "Test `new-mail-p', on a local drop with one message."
  (with-test-dir (dir "Mail/folder/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (mh:new-mail-p (list (mh:make-drop
			     :local
			     "home:local")))))))

(deftest new-mail-p (t new-mail-p-3)
  "Test `new-mail-p', on a local drop with multiple messages."
  (with-test-dir (dir "Mail/folder/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (mh:new-mail-p (list (mh:make-drop
			     :local
			     "home:local")))))))


;;;; Empty drop.

(deftest new-mail-p (() new-mail-p-30)
  "Test `new-mail-p', on an empty drop list."
  (with-test-dir (dir "Mail/folder/" "local")
    (letf (((search-list "home:") (list dir)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
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
       (mh:new-mail-p '())))))
