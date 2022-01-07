;;; Tests of lisp:complete-file.

(in-package "LISP")

(import '(deftest:deftest deftest:with-test-dir
			  deftest:with-test-search-list))


;;;; Lone directory.

(deftest complete-file ('(() ()) complete-file-1)
  "Test `complete-file' with the absolute complete name of a lone dir, in
   directory name form (with the trailing slash)."
  (with-test-dir (dir "a/")
    (multiple-value-list (complete-file (merge-pathnames "a/" dir)))))

(deftest complete-file ('(() ()) complete-file-2)
  "Test `complete-file' with a relative complete name of a lone dir, in
   directory name form (with the trailing slash)."
  (with-test-dir (dir "a/")
    (multiple-value-list (in-directory dir (complete-file "a/")))))

(deftest complete-file (t complete-file-3)
  "Test `complete-file' with the absolute complete name of a lone dir, in
   file name form."
  (with-test-dir (dir "bbbbbbb/")
    (let ((new (merge-pathnames "bbbbbbb/" dir)))
      (multiple-value-bind (prefix unique)
			   (complete-file (namify new))
	(and prefix
	     (string= (namestring prefix) (namify new))
	     unique)))))

(deftest complete-file ('(#p"bbbbbbb" t) complete-file-4)
  "Test `complete-file' with the relative complete name of a lone dir, in
   file name form."
  (with-test-dir (dir "bbbbbbb/")
    (multiple-value-list (in-directory dir (complete-file "bbbbbbb")))))

(deftest complete-file (t complete-file-5)
  "Test `complete-file' with the absolute partial name of a lone dir, in
   file name form."
  (with-test-dir (dir "abcdef/")
    (let ((new (merge-pathnames "abcdef/" dir)))
      (multiple-value-bind (prefix unique)
			   (complete-file (merge-pathnames "abc" dir))
	(and prefix
	     (string= (namestring prefix) (namify new))
	     unique)))))

(deftest complete-file ('(#p"abcdef" t) complete-file-6)
  "Test `complete-file' with the relative partial name of a lone dir, in
   file name form."
  (with-test-dir (dir "abcdef/")
    (multiple-value-list (in-directory dir (complete-file "abc")))))


;;;; Lone file.

(deftest complete-file (t complete-file-11)
  "Test `complete-file' with the absolute complete name of a lone file."
  (with-test-dir (dir "a")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "a" dir))
      (and prefix
	   unique
	   (string= (namestring prefix)
		    (namestring (merge-pathnames "a" dir)))))))

(deftest complete-file ('(#p"a" t) complete-file-12)
  "Test `complete-file' with a relative complete name of a lone file."
  (with-test-dir (dir "a")
    (multiple-value-list (in-directory dir (complete-file "a")))))

(deftest complete-file (t complete-file-15)
  "Test `complete-file' with the absolute partial name of a lone file."
  (with-test-dir (dir "abcdef")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "abc" dir))
      (and prefix
	   (string= (namestring prefix)
		    (namestring (merge-pathnames "abcdef" dir)))
	   unique))))

(deftest complete-file ('(#p"abcdef" t) complete-file-16)
  "Test `complete-file' with the relative partial name of a lone file."
  (with-test-dir (dir "abcdef")
    (multiple-value-list (in-directory dir (complete-file "abc")))))


;;;; Directory with siblings.

(deftest complete-file ('(() ()) complete-file-21)
  "Test `complete-file' with the absolute complete unique name of a
   directory with siblings, in directory name form (with the trailing
   slash)."
  (with-test-dir (dir "a/" "b/" "c" ".a")
    (multiple-value-list (complete-file (merge-pathnames "a/" dir)))))

(deftest complete-file ('(() ()) complete-file-22)
  "Test `complete-file' with a relative complete unique name of a directory
   with siblings, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/" "ba/" "b" ".b")
    (multiple-value-list (in-directory dir (complete-file "a/")))))

(deftest complete-file (t complete-file-23)
  "Test `complete-file' with the absolute complete unique name of a
   directory with siblings, in file name form."
  (with-test-dir (dir "bbbbbbb/" "bb" "ccccccc/")
    (let ((new (merge-pathnames "bbbbbbb" dir)))
      (multiple-value-bind (prefix unique)
			   (complete-file new)
	(and prefix
	     (string= (namestring prefix) (namestring new))
	     unique)))))

(deftest complete-file ('(#p"bbbbbbb" t) complete-file-24)
  "Test `complete-file' with the relative complete unique name of a
   directory with siblings, in file name form."
  (with-test-dir (dir "bbbbbbb/" "c" "d.c")
    (multiple-value-list (in-directory dir (complete-file "bbbbbbb")))))

(deftest complete-file (t complete-file-25)
  "Test `complete-file' with the absolute partial unique name of a dir with
   siblings, in file name form."
  (with-test-dir (dir "abcdef/" ".a" "bcdef/" "A")
    (let ((new (merge-pathnames "abcdef/" dir)))
      (multiple-value-bind (prefix unique)
			   (complete-file (merge-pathnames "abc" dir))
	(and prefix
	     (string= (namestring prefix) (namify new))
	     unique)))))

(deftest complete-file ('(#p"abcdef" t) complete-file-26)
  "Test `complete-file' with the relative partial unique name of a dir with
   siblings, in file name form."
  (with-test-dir (dir "abcdef/" "abCDEF/" "aabcdef")
    (multiple-value-list (in-directory dir (complete-file "abc")))))

(deftest complete-file (t complete-file-27)
  "Test `complete-file' with an absolute partial ambiguous name of a dir
   with siblings, in file name form."
  (with-test-dir (dir "abcdef/" ".a" "bcdef/" "A" "abcd" "abacus")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "abc" dir))
      (fi unique
	  (and prefix
	       (string= (namestring prefix)
			(namestring (merge-pathnames "abcd" dir))))))))

(deftest complete-file ('(#p"ab" ()) complete-file-28)
  "Test `complete-file' with a relative partial ambiguous name of a
   directory with siblings, in file name form."
  (with-test-dir (dir "abcdef/" "abCDEF/" "aabcdef")
    (multiple-value-list (in-directory dir (complete-file "ab")))))


;;;; File with siblings.

(deftest complete-file (t complete-file-31)
  "Test `complete-file' with the absolute unique complete name of a file
   with siblings."
  (with-test-dir (dir "a" "b" "dir/")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "a" dir))
      (and prefix
	   unique
	   (string= (namestring prefix)
		    (namestring (merge-pathnames "a" dir)))))))

(deftest complete-file ('(#p"a" t) complete-file-32)
  "Test `complete-file' with a relative unique complete name of a file with
   siblings."
  (with-test-dir (dir "a" "bbb")
    (multiple-value-list (in-directory dir (complete-file "a")))))

(deftest complete-file (t complete-file-35)
  "Test `complete-file' with the absolute partial name of a file with
   siblings."
  (with-test-dir (dir "abcdef" "cde")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "abc" dir))
      (and prefix
	   (string= (namestring prefix)
		    (namestring (merge-pathnames "abcdef" dir)))
	   unique))))

(deftest complete-file ('(#p"abcdef" t) complete-file-36)
  "Test `complete-file' with the relative partial name of a file with
   siblings."
  (with-test-dir (dir "abcdef" "ab" "A" "a")
    (multiple-value-list (in-directory dir (complete-file "abc")))))

(deftest complete-file (t complete-file-37)
  "Test `complete-file' with an absolute partial ambiguous name of a file
   with siblings."
  (with-test-dir (dir "abcdef.c" ".a" "bcdef/" "A.bcd" "abc" "abacus")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "abc" dir))
      (fi unique
	  (and prefix
	       (string= (namestring prefix)
			(namestring (merge-pathnames "abc" dir))))))))

(deftest complete-file ('(#p"abc" ()) complete-file-38)
  "Test `complete-file' with a relative partial ambiguous name of a file
   with siblings."
  (with-test-dir (dir "abcdef.c" "abcDEF/" "aabcdef")
    (multiple-value-list (in-directory dir (complete-file "ab")))))


;;;; Failure.

(deftest complete-file ('(() ()) complete-file-40)
  "Test `complete-file' with an absolute failing file name in a directory
   of many files."
  (with-test-dir (dir "abcdef.c" ".a" "bcdef/" "A.bcd" "abc" "abacus")
    (multiple-value-list (complete-file (merge-pathnames "abcx" dir)))))

(deftest complete-file ('(() ()) complete-file-41)
  "Test `complete-file' with a relative failing file name in a directory of
   many files."
  (with-test-dir (dir "abcdef.c" ".a" "bcdef/" "A.bcd" "abc" "abacus")
    (multiple-value-list (in-directory dir
			   (complete-file "abcx")))))


;;;; Symlink to file.

(deftest complete-file (t complete-file-50)
  "Test `complete-file' with the absolute complete unique name of a file
   symlink with siblings."
  (with-test-dir (dir "a" ("l" "a") "b" "dir/")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "l" dir))
      (and prefix
	   unique
	   (string= (namestring prefix)
		    (namestring (merge-pathnames "l" dir)))))))

(deftest complete-file ('(#p"l" t) complete-file-51)
  "Test `complete-file' with a relative complete unique name of a file
   symlink with siblings."
  (with-test-dir (dir "a" ("l" "a") "bbb")
    (multiple-value-list (in-directory dir (complete-file "l")))))

(deftest complete-file (t complete-file-52)
  "Test `complete-file' with the absolute partial name of a file symlink
   with siblings."
  (with-test-dir (dir "abcdef" (".link" "abcdef") "cde")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames ".li" dir))
      (and prefix
	   (string= (namestring prefix)
		    (namestring (merge-pathnames ".link" dir)))
	   unique))))

(deftest complete-file (t complete-file-53)
  "Test `complete-file' with the relative partial name of a file symlink
   which has siblings."
  (with-test-dir (dir "abcdef" "ab" "A" ("l.A" "A") "a")
    (multiple-value-bind (prefix unique)
			 (in-directory dir (complete-file "l"))
      (and prefix
	   (string= (namestring prefix) "l.A")
	   unique))))

(deftest complete-file (t complete-file-54)
  "Test `complete-file' with an absolute partial ambiguous unique name of a
   file symlink with siblings."
  (with-test-dir (dir "abc.c" ("l" "abc.c") "abc.b" ("link" "abc.b"))
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "l" dir))
      (fi unique
	  (and prefix
	       (string= (namestring prefix)
			(namestring (merge-pathnames "l" dir))))))))

(deftest complete-file (t complete-file-55)
  "Test `complete-file' with a relative partial ambiguous name of a file
   symlink with siblings."
  (with-test-dir (dir "abc.c" (".li" "abc.c") "abc.b" (".link" "abc.b"))
    (multiple-value-bind (prefix unique)
			 (in-directory dir
			   (complete-file ".li"))
      (fi unique
	  (and prefix
	       (string= (namestring prefix) ".li"))))))

(deftest complete-file (t complete-file-56)
  "Test `complete-file' with an absolute partial ambiguous name of a broken
   file symlink with siblings."
  (with-test-dir (dir "abc.c" ("li" "abc") "abc.b" ("link" "abc.b"))
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "li" dir))
      (fi unique
	  (and prefix
	       (string= (namestring prefix)
			(namestring (merge-pathnames "li" dir))))))))

(deftest complete-file (t complete-file-57)
  "Test `complete-file' with a relative partial ambiguous name of a broken
   file symlink with siblings."
  (with-test-dir (dir "abc.c" (".linky" "abc") "abc.b" (".link" "abc.b"))
    (multiple-value-bind (prefix unique)
			 (in-directory dir
			   (complete-file ".li"))
      (fi unique
	  (and prefix
	       (string= (namestring prefix) ".link"))))))


;;;; Symlink to directory.

(deftest complete-file (t complete-file-60)
  "Test `complete-file' with the absolute complete unique name of a
   directory symlink with siblings."
  (with-test-dir (dir "a/" ("l" "a/") "b" "dir/")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "l" dir))
      (and prefix
	   unique
	   (string= (namestring prefix)
		    (namestring (merge-pathnames "l" dir)))))))

(deftest complete-file ('(#p"l" t) complete-file-61)
  "Test `complete-file' with a relative complete unique name of a directory
   symlink with siblings."
  (with-test-dir (dir "a/" ("l" "a/") "bbb")
    (multiple-value-list (in-directory dir (complete-file "l")))))

(deftest complete-file (t complete-file-62)
  "Test `complete-file' with the absolute partial name of a directory
   symlink with siblings."
  (with-test-dir (dir "abcdef/" (".link" "abcdef/") "cde")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames ".li" dir))
      (and prefix
	   (string= (namestring prefix)
		    (namestring (merge-pathnames ".link" dir)))
	   unique))))

(deftest complete-file (t complete-file-63)
  "Test `complete-file' with the relative partial name of a directory
   symlink which has siblings."
  (with-test-dir (dir "abcdef" "ab" "A/" ("l.A" "A/") "a")
    (multiple-value-bind (prefix unique)
			 (in-directory dir (complete-file "l"))
      (and prefix
	   (string= (namestring prefix) "l.A")
	   unique))))

(deftest complete-file (t complete-file-64)
  "Test `complete-file' with an absolute partial ambiguous name of a
   directory symlink with siblings."
  (with-test-dir (dir "abc.c/" ("lin" "abc.c/") "abc.b/" ("link" "abc.b/"))
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "li" dir))
      (fi unique
	  (and prefix
	       (string= (namestring prefix)
			(namestring (merge-pathnames "lin" dir))))))))

(deftest complete-file (t complete-file-65)
  "Test `complete-file' with a relative partial ambiguous name of a
   directory symlink with siblings."
  (with-test-dir (dir "abc.c/" (".li" "abc.c/") "abc.b/" (".link" "abc.b/"))
    (multiple-value-bind (prefix unique)
			 (in-directory dir
			   (complete-file ".li"))
      (fi unique
	  (and prefix
	       (string= (namestring prefix) ".li"))))))

(deftest complete-file (t complete-file-66)
  "Test `complete-file' with an absolute partial ambiguous name of a broken
   directory symlink with siblings."
  (with-test-dir (dir "abc.c" ("li" "abc/") "abc.b" ("link" "abc.b"))
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "li" dir))
      (fi unique
	  (and prefix
	       (string= (namestring prefix)
			(namestring (merge-pathnames "li" dir))))))))

(deftest complete-file (t complete-file-67)
  "Test `complete-file' with a relative partial ambiguous name of a broken
   directory symlink with siblings."
  (with-test-dir (dir "abc.c/" (".li" "abc/") "abcd" (".link" "abc.b/"))
    (multiple-value-bind (prefix unique)
			 (in-directory dir
			   (complete-file ".l"))
      (fi unique
	  (and prefix
	       (string= (namestring prefix) ".li"))))))


;;;; Hidden file.

(deftest complete-file (t complete-file-70)
  "Test `complete-file' with the absolute complete unique name of a hidden
   file with siblings."
  (with-test-dir (dir ".a" ("l" "a") "b" "dir/")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames ".a" dir))
      (and prefix
	   unique
	   (string= (namestring prefix)
		    (namestring (merge-pathnames ".a" dir)))))))

(deftest complete-file ('(#p".a" t) complete-file-71)
  "Test `complete-file' with a relative complete unique name of a hidden
   file with siblings."
  (with-test-dir (dir ".a" ("l" ".a") "bbb")
    (multiple-value-list (in-directory dir (complete-file ".a")))))

(deftest complete-file (t complete-file-72)
  "Test `complete-file' with the absolute partial name of a hidden file
   with siblings."
  (with-test-dir (dir ".abcdef" (".link" "abcdef") "cde")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames ".ab" dir))
      (and prefix
	   (string= (namestring prefix)
		    (namestring (merge-pathnames ".abcdef" dir)))
	   unique))))

(deftest complete-file (t complete-file-73)
  "Test `complete-file' with the relative partial unique name of a hidden
   file which has siblings."
  (with-test-dir (dir ".abcdef" "ab" "A" ("l.A" "A") "a")
    (multiple-value-bind (prefix unique)
			 (in-directory dir (complete-file ".abc"))
      (and prefix
	   (string= (namestring prefix) ".abcdef")
	   unique))))

(deftest complete-file (t complete-file-74)
  "Test `complete-file' with an absolute partial ambiguous name of a hidden
   file with siblings."
  (with-test-dir (dir ".abc.c" ("li" ".abc.c") ".abc.b" ("link" ".abc.b"))
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames ".abc" dir))
      (fi unique
	  (and prefix
	       (string= (namestring prefix)
			(namestring (merge-pathnames ".abc." dir))))))))

(deftest complete-file ('(#p".abc." ()) complete-file-75)
  "Test `complete-file' with a relative partial ambiguous name of a hidden
   file symlink with siblings."
  (with-test-dir (dir ".abc.c" (".li" ".abc.c") ".abc.b" (".link" ".abc.b"))
    (multiple-value-list (in-directory dir (complete-file ".ab")))))


;;;; Backup file.

(deftest complete-file (t complete-file-80)
  "Test `complete-file' with the absolute complete unique name of a backup
   file with siblings."
  (with-test-dir (dir "a~" ("l" "a~") "b" "dir/")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames "a~" dir))
      (and prefix
	   unique
	   (string= (namestring prefix)
		    (namestring (merge-pathnames "a~" dir)))))))

(deftest complete-file ('(#p"a.BAK" t) complete-file-81)
  "Test `complete-file' with a relative complete unique name of a backup
   file with siblings."
  (with-test-dir (dir "a.BAK" ("l" ".a") "bbb")
    (multiple-value-list (in-directory dir (complete-file "a.BAK")))))

(deftest complete-file (t complete-file-82)
  "Test `complete-file' with the absolute partial name of a backup file
   with siblings."
  (with-test-dir (dir ".abcdef.BAK" (".link" "abcdef.BAK") "cde")
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames ".ab" dir))
      (and prefix
	   (string= (namestring prefix)
		    (namestring (merge-pathnames ".abcdef.BAK" dir)))
	   unique))))

(deftest complete-file (t complete-file-83)
  "Test `complete-file' with the relative partial unique name of a backup
   file which has siblings."
  (with-test-dir (dir ".abcdef" "ab.CKP" "A" ("l.A" "A") "a")
    (multiple-value-bind (prefix unique)
			 (in-directory dir (complete-file "ab"))
      (and prefix
	   (string= (namestring prefix) "ab.CKP")
	   unique))))

(deftest complete-file (t complete-file-84)
  "Test `complete-file' with an absolute partial ambiguous name of a backup
   file with siblings."
  (with-test-dir (dir ".abc.c~" ("li" ".abc.c~") ".abc.b"
		      ("link" ".abc.b"))
    (multiple-value-bind (prefix unique)
			 (complete-file (merge-pathnames ".abc" dir))
      (fi unique
	  (and prefix
	       (string= (namestring prefix)
			(namestring (merge-pathnames ".abc." dir))))))))

(deftest complete-file ('(#p".abc." ()) complete-file-85)
  "Test `complete-file' with a relative partial ambiguous name of a backup
   file symlink with siblings."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      ".abc.b" (".link" ".abc.b"))
    (multiple-value-list (in-directory dir (complete-file ".ab")))))


;;;; Search list.

(deftest complete-file ('(#p"a:.abc." ()) complete-file-90)
  "Test `complete-file' with a partial ambiguous name including a search
   list."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      ".abc.b" (".link" ".abc.b"))
    (with-test-search-list ("a" dir)
      (multiple-value-list (complete-file "a:.ab")))))

(deftest complete-file ('("a:" ()) complete-file-91)
  "Test `complete-file' with a search list."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      ;; FIX For some reason (equal #p"a:" prefix) fails, possibly because
      ;; the search list has been cleared by the time the comparison is
      ;; made.  Work around by converting prefix to a string.
      (multiple-value-bind (prefix unique)
			   (complete-file "a:")
	(list (namestring prefix) unique)))))

(deftest complete-file ('(#p"a:." ()) complete-file-92)
  "Test `complete-file' with a search list."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      ".abc.b" (".link" ".abc.b"))
    (with-test-search-list ("a" dir)
      (multiple-value-list (complete-file "a:")))))


;;;; Directory levels.

(deftest complete-file ('(#p"a/b" t) complete-file-100)
  "Test `complete-file' with a subdirectory."
  (with-test-dir (dir "a/" "a/b/" "a/b/c/" "a/b/c/f")
    (multiple-value-list (in-directory dir (complete-file "a/b")))))

(deftest complete-file ('(#p"../../b" t) complete-file-101)
  "Test `complete-file' with a parent directory."
  (with-test-dir (dir "a/b/c/" "a/b/c/f")
    (multiple-value-list (in-directory dir
			   (in-directory "a/b/c/"
			     ;; Inside a is b.
			     (complete-file "../../b"))))))


;;;; Key argument "ignore-types".

(deftest complete-file ('(#p"abc.c.BAK" t) complete-file-110)
  "Test `complete-file' :ignore-types."
  (with-test-dir (dir "abc.c.BAK" (".link" ".abc.c")
		      "abc.b" ("abc.link" "abc.b"))
    (multiple-value-list (in-directory dir
			   (complete-file "ab"
					  :ignore-types '("b" "link"))))))

(deftest complete-file ('(() ()) complete-file-111)
  "Test `complete-file' :ignore-types, where the contstraint filters out
   all files."
  (with-test-dir (dir "abc.c.BAK" (".link" ".abc.c")
		      "abc.b" ("abc.link" "abc.b"))
    (multiple-value-list (in-directory dir
			   (complete-file "ab"
					  :ignore-types '("BAK" "b" "link"))))))


;;;; Key argument "directory".

(deftest complete-file ('(#p"abc.c.BAK" t) complete-file-120)
  "Test `complete-file' with a search list."
  (with-test-dir (dir "abc.c.BAK" (".link" ".abc.c")
		      "abc.b" ("abc.link" "abc.b"))
    (with-test-dir (dir2)
      (multiple-value-list (in-directory dir2
			     (complete-file "ab"
					    :ignore-types '("b" "link")
					    :directory dir))))))


;;;; Errors.

(deftest complete-file (t complete-file-130)
  "Test `complete-file' with an absolute failing file name in a directory
   of many files."
  (let (ret)
    (with-test-dir (dir "abcdef.c" ".a" "bcdef/" "A.bcd" "abc" "abacus")
      (handler-case
	  (complete-file (merge-pathnames "xXx/" dir))
	(error () (setq ret t))))
    ret))

(deftest complete-file (t complete-file-131)
  "Test `complete-file' with a relative failing file name in a directory of
   many files."
  (let (ret)
    (with-test-dir (dir "abcdef.c" ".a" "bcdef/" "A.bcd" "abc" "abacus")
      (handler-case
	  (in-directory dir
	    (complete-file "xXx/"))
	(error () (setq ret t))))
    ret))

(deftest complete-file (t complete-file-132)
  "Test `complete-file' with wildcards."
  (let (ret)
    (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
			"abc.b" (".link" "abc.b"))
      (handler-case
	  (multiple-value-list (in-directory dir (complete-file "*.*")))
	(error () (setq ret t))))
    ret))
