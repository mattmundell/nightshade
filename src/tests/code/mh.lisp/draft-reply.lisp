;;; Tests of mh:draft-reply.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar *draft-reply-mail-1* "To: one@one.org, one-alt@one.org
Cc: people@CC.org, Three@three.org, short@Name.org, one-alt-2@one.org
From: short@Name.org
Subject: one
Date: 12 Apr 2008 16:02:10 +0100
Message-ID: xxxxx@ff.ff

Body body body.

More body.

Ending.
")

(defvar *draft-reply-mail-2* "To: test2@two.org.za
From: short@name.org
Subject: subject
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")

(defvar *draft-reply-mail-3* "To: Three@three.org
Cc: people@CC.org, Three@three.org, short@Name.org
From: short@Name.org
Subject: tEst 3
Date: 16 Apr 2008 16:02:10 +0100
Arb: xXx
Message-ID: a12345b@ff.ff

Body body body.

More body.

Ending.
")


;; Directory with exising drafts.

(deftest draft-reply ('(13
			"To: short@Name.org
Fcc: folder
Subject: Re: tEst 3
In-reply-to: Message of 16 Apr 2008 16:02:10 +0100.
          a12345b@ff.ff
From: Name Surname <login@server>
Reply-To: login@server
--------
")
			draft-reply-1)
  "Test `draft-reply' with missing Cc and missing Message-ID, with cc arg
   ()."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-3* stream))
      (to-file (stream "home:Mail/drafts/12"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-2* stream))
      (mh:with-fresh-state
       (let ((mh:*address* "login@server")
	     (mh:*from-name* "Name Surname"))
	 (list (mh:draft-reply "folder" 22)
	       (with-output-to-string (stream)
		 (mh:write-messages stream "drafts" '(13)))))))))

(deftest draft-reply ('(13
			"To: short@Name.org
Fcc: folder
Subject: Re: tEst 3
In-reply-to: Message of 16 Apr 2008 16:02:10 +0100.
          a12345b@ff.ff
From: Name Surname <login@server>
Reply-To: login@server
--------
")
			draft-reply-2)
  "Test `draft-reply' with Cc and Message-ID, with cc arg implied ()."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-3* stream))
      (to-file (stream "home:Mail/drafts/12"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-2* stream))
      (mh:with-fresh-state
       (let ((mh:*address* "login@server")
	     (mh:*from-name* "Name Surname"))
	 (list (mh:draft-reply "folder" 22)
	       (with-output-to-string (stream)
		 (mh:write-messages stream "drafts" '(13)))))))))

(deftest draft-reply ('(13
			"To: short@Name.org
Fcc: folder
Subject: Re: tEst 3
In-reply-to: Message of 16 Apr 2008 16:02:10 +0100.
          a12345b@ff.ff
From: Name Surname <login@server>
Reply-To: login@server
--------
")
			draft-reply-3)
  "Test `draft-reply' with Cc and Message-ID, with cc arg set ()."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-3* stream))
      (to-file (stream "home:Mail/drafts/12"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-2* stream))
      (mh:with-fresh-state
       (let ((mh:*address* "login@server")
	     (mh:*from-name* "Name Surname"))
	 (list (mh:draft-reply "folder" 22 ())
	       (with-output-to-string (stream)
		 (mh:write-messages stream "drafts" '(13)))))))))

(deftest draft-reply ('(13
			"To: short@Name.org
Cc: people@CC.org,
    short@Name.org,
    Three@three.org
Fcc: folder
Subject: Re: tEst 3
In-reply-to: Message of 16 Apr 2008 16:02:10 +0100.
          a12345b@ff.ff
From: Short Name <short@Name.org>
Reply-To: short@Name.org
--------
")
			draft-reply-4)
  "Test `draft-reply' with Cc and Message-ID, with cc arg :all."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-3* stream))
      (to-file (stream "home:Mail/drafts/12"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-2* stream))
      (mh:with-fresh-state
       (list (let ((mh:*address* "short@Name.org")
		   (mh:*from-name* "Short Name"))
	       (mh:draft-reply "folder" 22 :all))
	     (with-output-to-string (stream)
	       (mh:write-messages stream "drafts" '(13))))))))

(deftest draft-reply ('(13
			"To: short@Name.org
Cc: one-alt-2@one.org,
    one-alt@one.org,
    one@one.org,
    people@CC.org,
    short@Name.org,
    Three@three.org
Fcc: folder
Subject: Re: one
In-reply-to: Message of 12 Apr 2008 16:02:10 +0100.
          xxxxx@ff.ff
From: One <one@one.org>
Reply-To: one@one.org
--------
")
			draft-reply-5)
  "Test `draft-reply' with Cc and Message-ID, with cc arg :all where there
   is a sender address in the Cc and an alternate address in the To."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-2* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (to-file (stream "home:Mail/drafts/12"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-3* stream))
      (mh:with-fresh-state
       (list (let ((mh:*address* "one@one.org")
		   (mh:*from-name* "One"))
	       (mh:draft-reply "folder" 22 :all))
	     (with-output-to-string (stream)
	       (mh:write-messages stream "drafts"
				  '(13))))))))

(deftest draft-reply ('(13
			"To: short@Name.org
Cc: people@CC.org,
    Three@three.org
Fcc: folder
Subject: Re: tEst 3
In-reply-to: Message of 16 Apr 2008 16:02:10 +0100.
          a12345b@ff.ff
From: Short Name <short@Name.org>
Reply-To: short@Name.org
--------
")
			draft-reply-6)
  "Test `draft-reply' with Cc and Message-ID, with cc arg :others."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-3* stream))
      (to-file (stream "home:Mail/drafts/12"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-2* stream))
      (mh:with-fresh-state
       (list (let ((mh:*address* "short@Name.org")
		   (mh:*from-name* "Short Name"))
	       (mh:draft-reply "folder" 22 :others))
	     (with-output-to-string (stream)
	       (mh:write-messages stream "drafts"
				  '(13))))))))

(deftest draft-reply ('(13
			"To: short@Name.org
Cc: people@CC.org,
    short@Name.org,
    Three@three.org
Fcc: folder
Subject: Re: one
In-reply-to: Message of 12 Apr 2008 16:02:10 +0100.
          xxxxx@ff.ff
From: One <one@one.org>
Reply-To: one@one.org
--------
")
			draft-reply-7)
  "Test `draft-reply' with Cc and Message-ID, with :cc :others where there
   is a sender address in the Cc and an alternate address in the To."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-2* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (to-file (stream "home:Mail/drafts/12"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-3* stream))
      (mh:with-fresh-state
       (list (let ((mh:*address* "one@one.org")
		   (mh:*from-name* "One")
		   (mh:*alternate-addresses*
		    '("one-alt@one.org" "one-alt-2@one.org")))
	       (mh:draft-reply "folder" 22 :others))
	     (with-output-to-string (stream)
	       (mh:write-messages stream "drafts"
				  '(13))))))))


;; Empty directory.

(deftest draft-reply ('(1
			"To: short@Name.org
Fcc: folder
Subject: Re: tEst 3
In-reply-to: Message of 16 Apr 2008 16:02:10 +0100.
          a12345b@ff.ff
From: Name Surname <login@server>
Reply-To: login@server
--------
")
			draft-reply-21)
  "Test `draft-reply' with missing Cc and missing Message-ID, with cc arg
   ()."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-3* stream))
      (mh:with-fresh-state
       (let ((mh:*address* "login@server")
	     (mh:*from-name* "Name Surname"))
	 (list (mh:draft-reply "folder" 22)
	       (with-output-to-string (stream)
		 (mh:write-messages stream "drafts" '(1)))))))))

(deftest draft-reply ('(1
			"To: short@Name.org
Fcc: folder
Subject: Re: tEst 3
In-reply-to: Message of 16 Apr 2008 16:02:10 +0100.
          a12345b@ff.ff
From: Name Surname <login@server>
Reply-To: login@server
--------
")
			draft-reply-22)
  "Test `draft-reply' with Cc and Message-ID, with cc arg implied ()."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-3* stream))
      (mh:with-fresh-state
       (let ((mh:*address* "login@server")
	     (mh:*from-name* "Name Surname"))
	 (list (mh:draft-reply "folder" 22)
	       (with-output-to-string (stream)
		 (mh:write-messages stream "drafts" '(1)))))))))

(deftest draft-reply ('(1
			"To: short@Name.org
Fcc: folder
Subject: Re: tEst 3
In-reply-to: Message of 16 Apr 2008 16:02:10 +0100.
          a12345b@ff.ff
From: Name Surname <login@server>
Reply-To: login@server
--------
")
			draft-reply-23)
  "Test `draft-reply' with Cc and Message-ID, with cc arg set ()."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-3* stream))
      (mh:with-fresh-state
       (let ((mh:*address* "login@server")
	     (mh:*from-name* "Name Surname"))
	 (list (mh:draft-reply "folder" 22 ())
	       (with-output-to-string (stream)
		 (mh:write-messages stream "drafts" '(1)))))))))

(deftest draft-reply ('(1
			"To: short@Name.org
Cc: people@CC.org,
    short@Name.org,
    Three@three.org
Fcc: folder
Subject: Re: tEst 3
In-reply-to: Message of 16 Apr 2008 16:02:10 +0100.
          a12345b@ff.ff
From: Short Name <short@Name.org>
Reply-To: short@Name.org
--------
")
			draft-reply-24)
  "Test `draft-reply' with Cc and Message-ID, with cc arg :all."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-3* stream))
      (mh:with-fresh-state
       (list (let ((mh:*address* "short@Name.org")
		   (mh:*from-name* "Short Name"))
	       (mh:draft-reply "folder" 22 :all))
	     (with-output-to-string (stream)
	       (mh:write-messages stream "drafts"
				  '(1))))))))

(deftest draft-reply ('(1
			"To: short@Name.org
Cc: one-alt-2@one.org,
    one-alt@one.org,
    one@one.org,
    people@CC.org,
    short@Name.org,
    Three@three.org
Fcc: folder
Subject: Re: one
In-reply-to: Message of 12 Apr 2008 16:02:10 +0100.
          xxxxx@ff.ff
From: One <one@one.org>
Reply-To: one@one.org
--------
")
			draft-reply-25)
  "Test `draft-reply' with Cc and Message-ID, with cc arg :all where there
   is a sender address in the Cc and an alternate address in the To."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-2* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (mh:with-fresh-state
       (list (let ((mh:*address* "one@one.org")
		   (mh:*from-name* "One"))
	       (mh:draft-reply "folder" 22 :all))
	     (with-output-to-string (stream)
	       (mh:write-messages stream "drafts"
				  '(1))))))))

(deftest draft-reply ('(1
			"To: short@Name.org
Cc: people@CC.org,
    Three@three.org
Fcc: folder
Subject: Re: tEst 3
In-reply-to: Message of 16 Apr 2008 16:02:10 +0100.
          a12345b@ff.ff
From: Short Name <short@Name.org>
Reply-To: short@Name.org
--------
")
			draft-reply-26)
  "Test `draft-reply' with Cc and Message-ID, with cc arg :others."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-3* stream))
      (mh:with-fresh-state
       (list (let ((mh:*address* "short@Name.org")
		   (mh:*from-name* "Short Name"))
	       (mh:draft-reply "folder" 22 :others))
	     (with-output-to-string (stream)
	       (mh:write-messages stream "drafts"
				  '(1))))))))

(deftest draft-reply ('(1
			"To: short@Name.org
Cc: people@CC.org,
    short@Name.org,
    Three@three.org
Fcc: folder
Subject: Re: one
In-reply-to: Message of 12 Apr 2008 16:02:10 +0100.
          xxxxx@ff.ff
From: One <one@one.org>
Reply-To: one@one.org
--------
")
			draft-reply-27)
  "Test `draft-reply' with Cc and Message-ID, with :cc :others where there
   is a sender address in the Cc and an alternate address in the To."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-2* stream))
      (to-file (stream "home:Mail/folder/22"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-1* stream))
      (mh:with-fresh-state
       (list (let ((mh:*address* "one@one.org")
		   (mh:*from-name* "One")
		   (mh:*alternate-addresses*
		    '("one-alt@one.org" "one-alt-2@one.org")))
	       (mh:draft-reply "folder" 22 :others))
	     (with-output-to-string (stream)
	       (mh:write-messages stream "drafts"
				  '(1))))))))

; FIX addresses containing , or ;


;;;; Errors.

(deftest draft-reply (t draft-reply-40)
  "Test `draft-reply' with a missing message."
  (with-test-dir (folder "Mail/folder/" "Mail/drafts/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%")
	(format stream "Draft-Folder: drafts~%"))
      (to-file (stream "home:Mail/folder/1"
		       :if-does-not-exist :create)
	(write-string *draft-reply-mail-2* stream))
      (mh:with-fresh-state
       (let ((ret)
	     (mh:*address* "one@one.org")
	     (mh:*from-name* "One")
	     (mh:*alternate-addresses*
	      '("one-alt@one.org" "one-alt-2@one.org")))
	 (handler-case
	     (mh:draft-reply "folder" 22 :others)
	   (error () (setq ret t)))
	 ret)))))

; FIX broken addresses
