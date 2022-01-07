;;; Tests of mh:split-messages.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(defvar split-messages-mail-1 "To: test@ttt.org
Cc: one cc
From: nn1n2n3n4n5n6n7n8n9n0nanbncndnenfnn surname <long@name.org>
Subject: s1
Date: 15 Apr 2008 16:02:10 +0100

Body.
")

(defvar split-messages-mail-2 "To: test2@two.org.za
From: short@name.org
Subject: subject
Date: 16 Apr 2008 16:02:10 +0100

Body body body.

More body.

Ending.
")

(defvar split-messages-mail-3 "To: Three@three.org
Cc: people@CC.org
From: short@Name.org
Subject: tEst 3
Date: 16 Apr 2008 16:02:10 +0100
Arb: xXx

Body body body.

More body.

Ending.
")

(deftest split-messages ('((1 12 22) (22 23)) split-messages-1)
  "Test `split-messages' with an empty rule list."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/12"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/dest/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/dest/23"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:split-messages "inbox/" '(1 "12" 22) ())
       (list (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "dest" '(:all)))))))

(deftest split-messages ('((1 12 22) (22 23)) split-messages-2)
  "Test `split-messages' with a rule list that fails to match."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/12"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/dest/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/dest/23"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:split-messages "inbox/" '(:all)
			  '(("dest" ())))
       (list (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "dest" '(:all)))))))

(deftest split-messages ('((1 12) (1)) split-messages-3)
  "Test `split-messages' with a rule list that moves one to an empty folder."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/12"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-3 stream))
      (mh:with-fresh-state
       (mh:split-messages "inbox/" '("all")
			  '(("dest" (mh:f-to "three"))))
       (list (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "dest" '(:all)))))))

(deftest split-messages ('((1 12) (22 23) "tEst 3") split-messages-4)
  "Test `split-messages' with a rule list that moves one to a folder."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/12"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-3 stream))
      (to-file (stream "home:Mail/dest/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (mh:with-fresh-state
       (mh:split-messages "inbox/" '(1 12 "22-22")
			  '(("dest" (mh:f-to "three"))))
       (list (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "dest" '(:all))
	     (mh:message-header "dest" 23 "Subject"))))))

(deftest split-messages ('((21) (1 2)) split-messages-5)
  "Test `split-messages' where one rule moves many to an empty folder."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/21"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-3 stream))
      (mh:with-fresh-state
       (mh:split-messages "inbox/" '(1 "21-22")
			  '(("dest" (or (mh:f-to "three")
					(mh:f-cc "one")))))
       (list (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "dest" '(:all)))))))

(deftest split-messages ('((21) (1 2)) split-messages-6)
  "Test `split-messages' where many rules move many to an empty folder."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/21"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-3 stream))
      (mh:with-fresh-state
       (mh:split-messages "inbox/" '(1 "22-21")
			  '(("dest" (mh:f-to "three"))
			    ("dest" (mh:f-cc "one"))))
       (list (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "dest" '(:all)))))))

(deftest split-messages ('((21) (11 12 13)) split-messages-7)
  "Test `split-messages' where one rule moves many to a folder."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/21"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-3 stream))
      (to-file (stream "home:Mail/dest/11"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:split-messages "inbox/" '(1 ("21" . "22"))
			  '(("dest" (or (mh:f-to "three")
					(mh:f-cc "one")))))
       (list (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "dest" '(:all)))))))

(deftest split-messages ('((21) (4 5 6)) split-messages-8)
  "Test `split-messages' where many rules move many to a folder."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/21"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-3 stream))
      (to-file (stream "home:Mail/dest/4"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:split-messages "inbox/" '(1 ("22" . "21"))
			  '(("dest" (mh:f-to "three"))
			    ("dest" (mh:f-cc "one"))))
       (list (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "dest" '(:all)))))))

(deftest split-messages ('((12) (4 5) (1) "s1") split-messages-9)
  "Test `split-messages' where many rules move many to many folder."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/" "Mail/other/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/12"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-3 stream))
      (to-file (stream "home:Mail/dest/4"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:split-messages "inbox/" '(:all)
			  '(("dest" (mh:f-to "three"))
			    ("other" (mh:f-cc "one"))))
       (list (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "other" '(:all))
	     (mh:message-header "other" 1 "Subject"))))))

(deftest split-messages ('((1 12) (4 5) () "tEst 3") split-messages-10)
  "Test `split-messages' where an earlier rule takes precedence."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/" "Mail/other/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/12"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-3 stream))
      (to-file (stream "home:Mail/dest/4"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:split-messages "inbox/" '(:all)
			  '(("dest" (mh:f-to "three"))
			    ("inbox" (mh:f-to "three"))))
       (list (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "other" '(:all))
	     (mh:message-header "dest" 5 "Subject"))))))

(deftest split-messages ('((11 12) (1)) split-messages-11)
  "Test `split-messages' splitting one from one message in a folder of
   many, specifying the all symbol and a large expression."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/11"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/12"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/13"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-3 stream))
      (mh:with-fresh-state
       (mh:split-messages "inbox"
			 '("13")
			 '(("dest" (and (mh:to "Three@")
					(mh:f-to "three@")
					(mh:from "Name.org")
					(mh:f-from "name.org")
					(mh:cc "CC.org")
					(mh:f-cc "cc.org")
					(mh:subject "tEst 3")
					(mh:f-subject "test 3")
					(mh:content "More body.")
					(mh:f-content "more body.")
					(mh:date "16 Apr")
					(mh:f-date "16 apr")
					(mh:before "16 apr 2009")
					(mh:after "16 mar 2008")
					(mh:-- "Cc" "CC.org")
					(mh:-- "Arb" "xXx")
					(mh:f--- "Arb" "xxx")))))
       (list (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "dest" '(:all)))))))

(deftest split-messages ('(() (4 5) (1 2) "s1") split-messages-12)
  "Test `split-messages' where many rules move many to many folder, with a
   catch all rule."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/" "Mail/other/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/12"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/22"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-3 stream))
      (to-file (stream "home:Mail/dest/4"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (mh:with-fresh-state
       (mh:split-messages "inbox/" '(:all)
			  '(("dest" (mh:f-to "three"))
			    ("other" t)))
       (list (mh:pick-messages "inbox" '(:all))
	     (mh:pick-messages "dest" '(:all))
	     (mh:pick-messages "other" '(:all))
	     (mh:message-header "other" 1 "Subject"))))))


;;;; Errors.

(deftest split-messages (t split-messages-100)
  "Test `split-messages' splitting a missing message."
  (with-test-dir (folder "Mail/inbox/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/11"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/12"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/13"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:split-messages "inbox" '(1) '(("dest" t)))
	   (error () (setq ret t)))
	 ret)))))

(deftest split-messages (t split-messages-111)
  "Test `split-messages' with an erroneous symbol message."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:split-messages "inbox" '(:cur) '(("dest" t)))
	   (error () (setq ret t))))))))

(deftest split-messages (t split-messages-113)
  "Test `split-messages' specifying a range with integers."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/2"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/3"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:split-messages "inbox" '((2 . 2)) '(("dest" t)))
	   (error () (setq ret t)))
	 ret)))))

(deftest split-messages (t split-messages-114)
  "Test `split-messages' specifying a range with missing messages."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/7"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/9"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:split-messages "inbox" '(("7" . "9")) '(("dest" t)))
	   (error () (setq ret t)))
	 ret)))))

(deftest split-messages (t split-messages-115)
  "Test `split-messages' specifying a range with junk in the first
   string."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/7"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/8"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:split-messages "inbox" '(("7." "8")) '(("dest" t)))
	   (error () (setq ret t))))))))

(deftest split-messages (t split-messages-116)
  "Test `split-messages' specifying a range with junk in the second
   string."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/7"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/8"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:split-messages "inbox" '(("7" "8-")) '(("dest" t)))
	   (error () (setq ret t))))))))

(deftest split-messages (t split-messages-117)
  "Test `split-messages', with :highest."
  (with-test-dir (folder "Mail/inbox/" "Mail/dest/")
    (letf (((search-list "home:") (list folder)))
      (to-file (stream "home:.mh_profile"
		       :if-does-not-exist :create)
	(format stream "Path: Mail~%"))
      (to-file (stream "home:Mail/inbox/1"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-1 stream))
      (to-file (stream "home:Mail/inbox/7"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (to-file (stream "home:Mail/inbox/8"
		       :if-does-not-exist :create)
	(write-string split-messages-mail-2 stream))
      (mh:with-fresh-state
       (let (ret)
	 (handler-case
	     (mh:split-messages "inbox" '(:highest) '(("dest" t)))
	   (error () (setq ret t)))
	 ret)))))
