;;; Tests of lisp:ambiguous-files.

(in-package "LISP")

(import '(deftest:deftest deftest:with-test-dir
			  deftest:with-test-search-list))

(defmacro files= (list1 list2 list2-dir)
  "Return true if $list1 lists the same set of files as those in $list2
   merged with $dir."
  `(equal (sort (mapcar #'namestring ,list1) #'string<)
	  (sort (mapcar (lambda (ele)
			  (namestring (merge-pathnames ele ,list2-dir)))
			,list2)
		#'string<)))


;;;; Lone directory.

(deftest ambiguous-files (() ambiguous-files-1)
  "Test `ambiguous-files' with the absolute complete name of a lone empty
   dir, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/")
    (ambiguous-files (merge-pathnames "a/" dir))))

(deftest ambiguous-files (() ambiguous-files-2)
  "Test `ambiguous-files' with a relative complete name of a lone empty
   dir, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/")
    (in-directory dir (ambiguous-files "a/"))))

(deftest ambiguous-files (t ambiguous-files-3)
  "Test `ambiguous-files' with the absolute complete name of a lone dir, in
   file name form."
  (with-test-dir (dir "bbbbbbb/")
    (files= (ambiguous-files (merge-pathnames "bbbbbbb" dir))
	    '("bbbbbbb")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-4)
  "Test `ambiguous-files' with the relative complete name of a lone dir, in
   file name form."
  (with-test-dir (dir "bbbbbbb/")
    (files= (in-directory dir (ambiguous-files "bbbbbbb"))
	    '("bbbbbbb")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-5)
  "Test `ambiguous-files' with the absolute partial name of a lone dir, in
   file name form."
  (with-test-dir (dir "abcdef/")
    (files= (ambiguous-files (merge-pathnames "abc" dir))
	    '("abcdef")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-6)
  "Test `ambiguous-files' with the relative partial name of a lone dir, in
   file name form."
  (with-test-dir (dir "abcdef/")
    (files= (in-directory dir (ambiguous-files "abc"))
	    '("abcdef")
	    dir)))


;;;; Lone file.

(deftest ambiguous-files (t ambiguous-files-11)
  "Test `ambiguous-files' with the absolute complete name of a lone file."
  (with-test-dir (dir "a")
    (files= (ambiguous-files (merge-pathnames "a" dir))
	    '("a")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-12)
  "Test `ambiguous-files' with a relative complete name of a lone file."
  (with-test-dir (dir "a")
    (files= (in-directory dir (ambiguous-files "a"))
	    '("a")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-13)
  "Test `ambiguous-files' with the absolute partial name of a lone file."
  (with-test-dir (dir "abcdef")
    (files= (ambiguous-files (merge-pathnames "abc" dir))
	    '("abcdef")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-14)
  "Test `ambiguous-files' with the relative partial name of a lone file."
  (with-test-dir (dir "abcdef")
    (files= (in-directory dir (ambiguous-files "abc"))
	    '("abcdef")
	    dir)))


;;;; Directory with siblings.

(deftest ambiguous-files (() ambiguous-files-21)
  "Test `ambiguous-files' with the absolute complete unique name of a
   directory with siblings, in directory name form (with the trailing
   slash)."
  (with-test-dir (dir "a/" "b/" "c" ".a")
    (ambiguous-files (merge-pathnames "a/" dir))))

(deftest ambiguous-files (() ambiguous-files-22)
  "Test `ambiguous-files' with a relative complete unique name of a directory
   with siblings, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/" "ba/" "b" ".b")
    (in-directory dir (ambiguous-files "a/"))))

(deftest ambiguous-files (t ambiguous-files-23)
  "Test `ambiguous-files' with the absolute complete unique name of a
   directory with siblings, in file name form."
  (with-test-dir (dir "bbbbbbb/" "bb" "ccccccc/")
    (files= (ambiguous-files (merge-pathnames "bbbbbbb" dir))
	    '("bbbbbbb")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-24)
  "Test `ambiguous-files' with the relative complete unique name of a
   directory with siblings, in file name form."
  (with-test-dir (dir "bbbbbbb/" "c" "d.c")
    (files= (in-directory dir (ambiguous-files "bbbbbbb"))
	    '("bbbbbbb")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-25)
  "Test `ambiguous-files' with the absolute partial unique name of a dir with
   siblings, in file name form."
  (with-test-dir (dir "abcdef/" ".a" "bcdef/" "A")
    (files= (ambiguous-files (merge-pathnames "abc" dir))
	    '("abcdef")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-26)
  "Test `ambiguous-files' with the relative partial unique name of a dir with
   siblings, in file name form."
  (with-test-dir (dir "abcdef/" "abCDEF/" "aabcdef")
    (files= (in-directory dir (ambiguous-files "abc"))
	    '("abcdef")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-27)
  "Test `ambiguous-files' with an absolute partial ambiguous name of a dir
   with siblings, in file name form."
  (with-test-dir (dir "abcdef/" ".a" "bcdef/" "A" "abcd" "abacus")
    (files= (ambiguous-files (merge-pathnames "abc" dir))
	    '("abcdef" "abcd")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-28)
  "Test `ambiguous-files' with a relative partial ambiguous name of a
   directory with siblings, in file name form."
  (with-test-dir (dir "abcdef/" "abCDEF/" "aabcdef")
    (files= (in-directory dir (ambiguous-files "ab"))
	    '("abcdef" "abCDEF")
	    dir)))


;;;; File with siblings.

(deftest ambiguous-files (t ambiguous-files-31)
  "Test `ambiguous-files' with the absolute unique complete name of a file
   with siblings."
  (with-test-dir (dir "a" "b" "dir/")
    (files= (ambiguous-files (merge-pathnames "a" dir))
	    '("a")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-32)
  "Test `ambiguous-files' with a relative unique complete name of a file with
   siblings."
  (with-test-dir (dir "a" "bbb")
    (files= (in-directory dir (ambiguous-files "a"))
	    '("a")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-33)
  "Test `ambiguous-files' with the absolute partial name of a file with
   siblings."
  (with-test-dir (dir "abcdef" "cde")
    (files= (ambiguous-files (merge-pathnames "abc" dir))
	    '("abcdef")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-36)
  "Test `ambiguous-files' with the relative partial name of a file with
   siblings."
  (with-test-dir (dir "abcdef" "ab" "A" "a")
    (files= (in-directory dir (ambiguous-files "abc"))
	    '("abcdef")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-37)
  "Test `ambiguous-files' with an absolute partial ambiguous name of a file
   with siblings."
  (with-test-dir (dir "abcdef.c" ".a" "bcdef/" "A.bcd" "abc" "abacus")
    (files= (ambiguous-files (merge-pathnames "abc" dir))
	    '("abcdef.c" "abc")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-38)
  "Test `ambiguous-files' with a relative partial ambiguous name of a file
   with siblings."
  (with-test-dir (dir "abcdef.c" "abcDEF/" "aabcdef")
    (files= (in-directory dir (ambiguous-files "ab"))
	    '("abcdef.c" "abcDEF")
	    dir)))


;;;; Failure.

(deftest ambiguous-files (() ambiguous-files-40)
  "Test `ambiguous-files' with an absolute failing file name in a directory
   of many files."
  (with-test-dir (dir "abcdef.c" ".a" "bcdef/" "A.bcd" "abc" "abacus")
    (ambiguous-files (merge-pathnames "abcx" dir))))

(deftest ambiguous-files (() ambiguous-files-41)
  "Test `ambiguous-files' with a relative failing file name in a directory of
   many files."
  (with-test-dir (dir "abcdef.c" ".a" "bcdef/" "A.bcd" "abc" "abacus")
    (in-directory dir (ambiguous-files "abcx"))))

(deftest ambiguous-files (() ambiguous-files-42)
  "Test `ambiguous-files' with an absolute failing file name in a directory
   of many files."
  (with-test-dir (dir "abcdef.c" ".a" "bcdef/" "A.bcd" "abc" "abacus")
    (ambiguous-files (merge-pathnames "xXx/" dir))))

(deftest ambiguous-files (() ambiguous-files-43)
  "Test `ambiguous-files' with a relative failing file name in a directory
   of many files."
  (with-test-dir (dir "abcdef.c" ".a" "bcdef/" "A.bcd" "abc" "abacus")
    (in-directory dir (ambiguous-files "xXx/"))))


;;;; Symlink to file.

(deftest ambiguous-files (t ambiguous-files-50)
  "Test `ambiguous-files' with the absolute complete unique name of a file
   symlink with siblings."
  (with-test-dir (dir "a" ("l" "a") "b" "dir/")
    (files= (ambiguous-files (merge-pathnames "l" dir))
	    '("l")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-51)
  "Test `ambiguous-files' with a relative complete unique name of a file
   symlink with siblings."
  (with-test-dir (dir "a" ("l" "a") "bbb")
    (files= (in-directory dir (ambiguous-files "l"))
	    '("l")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-52)
  "Test `ambiguous-files' with the absolute partial name of a file symlink
   with siblings."
  (with-test-dir (dir "abcdef" (".link" "abcdef") "cde")
    (files= (ambiguous-files (merge-pathnames ".li" dir))
	    '(".link")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-53)
  "Test `ambiguous-files' with the relative partial name of a file symlink
   which has siblings."
  (with-test-dir (dir "abcdef" "ab" "A" ("l.A" "A") "a")
    (files= (in-directory dir (ambiguous-files "l"))
	    '("l.A")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-54)
  "Test `ambiguous-files' with an absolute partial ambiguous unique name of a
   file symlink with siblings."
  (with-test-dir (dir "abc.c" ("l" "abc.c") "abc.b" ("link" "abc.b"))
    (files= (ambiguous-files (merge-pathnames "l" dir))
	    '("l" "link")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-55)
  "Test `ambiguous-files' with a relative partial ambiguous name of a file
   symlink with siblings."
  (with-test-dir (dir "abc.c" (".li" "abc.c") "abc.b" (".link" "abc.b"))
    (files= (in-directory dir
	      (ambiguous-files ".li"))
	    '(".li" ".link")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-56)
  "Test `ambiguous-files' with an absolute partial ambiguous name of a broken
   file symlink with siblings."
  (with-test-dir (dir "abc.c" ("li" "abc") "abc.b" ("link" "abc.b"))
    (files= (ambiguous-files (merge-pathnames "li" dir))
	    '("li" "link")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-57)
  "Test `ambiguous-files' with a relative partial ambiguous name of a broken
   file symlink with siblings."
  (with-test-dir (dir "abc.c" (".linky" "abc") "abc.b" (".link" "abc.b"))
    (files= (in-directory dir
	      (ambiguous-files ".li"))
	    '(".linky" ".link")
	    dir)))


;;;; Symlink to directory.

(deftest ambiguous-files (t ambiguous-files-60)
  "Test `ambiguous-files' with the absolute complete unique name of a
   directory symlink with siblings."
  (with-test-dir (dir "a/" ("l" "a/") "b" "dir/")
    (files= (ambiguous-files (merge-pathnames "l" dir))
	    '("l")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-61)
  "Test `ambiguous-files' with a relative complete unique name of a directory
   symlink with siblings."
  (with-test-dir (dir "a/" ("l" "a/") "bbb")
    (files= (in-directory dir (ambiguous-files "l"))
	    '("l")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-62)
  "Test `ambiguous-files' with the absolute partial name of a directory
   symlink with siblings."
  (with-test-dir (dir "abcdef/" (".link" "abcdef/") "cde")
    (files= (ambiguous-files (merge-pathnames ".li" dir))
	    '(".link")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-63)
  "Test `ambiguous-files' with the relative partial name of a directory
   symlink which has siblings."
  (with-test-dir (dir "abcdef" "ab" "A/" ("l.A" "A/") "a")
    (files= (in-directory dir (ambiguous-files "l"))
	    '("l.A")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-64)
  "Test `ambiguous-files' with an absolute partial ambiguous name of a
   directory symlink with siblings."
  (with-test-dir (dir "abc.c/" ("lin" "abc.c/") "abc.b/" ("link" "abc.b/"))
    (files= (ambiguous-files (merge-pathnames "li" dir))
	    '("lin" "link")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-65)
  "Test `ambiguous-files' with a relative partial ambiguous name of a
   directory symlink with siblings."
  (with-test-dir (dir "abc.c/" (".li" "abc.c/") "abc.b/" (".link" "abc.b/"))
    (files= (in-directory dir
	      (ambiguous-files ".li"))
	    '(".li" ".link")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-66)
  "Test `ambiguous-files' with an absolute partial ambiguous name of a broken
   directory symlink with siblings."
  (with-test-dir (dir "abc.c" ("li" "abc/") "abc.b" ("link" "abc.b"))
    (files= (ambiguous-files (merge-pathnames "li" dir))
	    '("li" "link")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-67)
  "Test `ambiguous-files' with a relative partial ambiguous name of a broken
   directory symlink with siblings."
  (with-test-dir (dir "abc.c/" (".li" "abc/") "abcd" (".link" "abc.b/"))
    (files= (in-directory dir
	      (ambiguous-files ".l"))
	    '(".li" ".link")
	    dir)))


;;;; Hidden file.

(deftest ambiguous-files (t ambiguous-files-70)
  "Test `ambiguous-files' with the absolute complete unique name of a hidden
   file with siblings."
  (with-test-dir (dir ".a" ("l" "a") "b" "dir/")
    (files= (ambiguous-files (merge-pathnames ".a" dir))
	    '(".a")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-71)
  "Test `ambiguous-files' with a relative complete unique name of a hidden
   file with siblings."
  (with-test-dir (dir ".a" ("l" ".a") "bbb")
    (files= (in-directory dir (ambiguous-files ".a"))
	    '(".a")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-72)
  "Test `ambiguous-files' with the absolute partial name of a hidden file
   with siblings."
  (with-test-dir (dir ".abcdef" (".link" "abcdef") "cde")
    (files= (ambiguous-files (merge-pathnames ".ab" dir))
	    '(".abcdef")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-73)
  "Test `ambiguous-files' with the relative partial unique name of a hidden
   file which has siblings."
  (with-test-dir (dir ".abcdef" "ab" "A" ("l.A" "A") "a")
    (files= (in-directory dir (ambiguous-files ".abc"))
	    '(".abcdef")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-74)
  "Test `ambiguous-files' with an absolute partial ambiguous name of a hidden
   file with siblings."
  (with-test-dir (dir ".abc.c" ("li" ".abc.c") ".abc.b" ("link" ".abc.b"))
    (files= (ambiguous-files (merge-pathnames ".abc" dir))
	    '(".abc.c" ".abc.b")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-75)
  "Test `ambiguous-files' with a relative partial ambiguous name of a hidden
   file symlink with siblings."
  (with-test-dir (dir ".abc.c" (".li" ".abc.c") ".abc.b" (".link" ".abc.b"))
    (files= (in-directory dir (ambiguous-files ".ab"))
	    '(".abc.c" ".abc.b")
	    dir)))


;;;; Backup file.

(deftest ambiguous-files (t ambiguous-files-80)
  "Test `ambiguous-files' with the absolute complete unique name of a backup
   file with siblings."
  (with-test-dir (dir "a~" ("l" "a~") "b" "dir/")
    (files= (ambiguous-files (merge-pathnames "a~" dir))
	    '("a~")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-81)
  "Test `ambiguous-files' with a relative complete unique name of a backup
   file with siblings."
  (with-test-dir (dir "a.BAK" ("l" ".a") "bbb")
    (files= (in-directory dir (ambiguous-files "a.BAK"))
	    '("a.BAK")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-82)
  "Test `ambiguous-files' with the absolute partial name of a backup file
   with siblings."
  (with-test-dir (dir ".abcdef.BAK" (".link" "abcdef.BAK") "cde")
    (files= (ambiguous-files (merge-pathnames ".ab" dir))
	    '(".abcdef.BAK")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-83)
  "Test `ambiguous-files' with the relative partial unique name of a backup
   file which has siblings."
  (with-test-dir (dir ".abcdef" "ab.CKP" "A" ("l.A" "A") "a")
    (files= (in-directory dir (ambiguous-files "ab"))
	    '("ab.CKP")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-84)
  "Test `ambiguous-files' with an absolute partial ambiguous name of a backup
   file with siblings."
  (with-test-dir (dir ".abc.c~" ("li" ".abc.c~") ".abc.b"
		      ("link" ".abc.b"))
    (files= (ambiguous-files (merge-pathnames ".abc" dir))
	    '(".abc.c~" ".abc.b")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-85)
  "Test `ambiguous-files' with a relative partial ambiguous name of a backup
   file symlink with siblings."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      ".abc.b" (".link" ".abc.b"))
    (files= (in-directory dir (ambiguous-files ".ab"))
	    '(".abc.c.BAK" ".abc.b")
	    dir)))


;;;; Mixed.

(deftest ambiguous-files (t ambiguous-files-90)
  "Test `ambiguous-files' with an absolute ambiguous name in a directory of
   many entities."
  (with-test-dir (dir "abc/" "bc" "ab.c" ("l" "ab.c") "ccc/z/"
		      ".abc" "bc.BAK")
    (files= (ambiguous-files (merge-pathnames "a" dir))
	    '("ab.c" "abc")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-91)
  "Test `ambiguous-files' with a relative ambiguous name in a directory of
   many entities."
  (with-test-dir (dir "abc/" "bc" "ab.c" ("l" "ab.c") "ccc/z/"
		      ".abc" "bc.BAK")
    (files= (in-directory dir (ambiguous-files "a"))
	    '("ab.c" "abc")
	    dir)))


;;;; Search list.

(deftest ambiguous-files (t ambiguous-files-100)
  "Test `ambiguous-files' with a partial ambiguous name including a search
   list."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      ".abc.b" (".link" ".abc.b"))
    (files= (with-test-search-list ("a" dir)
	      (ambiguous-files "a:.ab"))
	    '(".abc.c.BAK" ".abc.b")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-101)
  "Test `ambiguous-files' with a search list bound to a directory of
   files."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (files= (with-test-search-list ("a" dir)
              (ambiguous-files "a:"))
	    '(".abc.c.BAK" ".li" "abc.b" "link")
	    dir)))

(deftest ambiguous-files (t ambiguous-files-102)
  "Test `ambiguous-files' with a search list full of hidden files."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      ".abc.b" (".link" ".abc.b"))
    (files= (with-test-search-list ("a" dir)
              (ambiguous-files "a:"))
	    '(".abc.c.BAK" ".li" ".abc.b" ".link")
	    dir)))

(deftest ambiguous-files (() ambiguous-files-103)
  "Test `ambiguous-files' with a search list bound to an empty directory."
  (with-test-dir (dir)
    (with-test-search-list ("a" dir)
      (ambiguous-files "a:"))))


;;;; Ancestor directory.

(deftest ambiguous-files (t ambiguous-files-110)
  "Test `ambiguous-files' on a subdirectory."
  (with-test-dir (dir "a/" "a/b/" "a/b/c/" "a/b/c/f")
    (files= (in-directory dir (ambiguous-files "a/b"))
	    '("b")
	    (merge-pathnames "a/" dir))))

(deftest ambiguous-files (t ambiguous-files-111)
  "Test `ambiguous-files' on a parent directory."
  (with-test-dir (dir "a/b/c/" "a/b/c/f")
    (files= (in-directory dir
	      (in-directory "a/b/c/"
		;; Inside a is b.
		(ambiguous-files "../../b")))
	    '("b/c/../../b")
	    (merge-pathnames "a/" dir))))


;;;; Errors.

(deftest ambiguous-files (t ambiguous-files-120)
  "Test `ambiguous-files' with wildcards."
  (let (ret)
    (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
			"abc.b" (".link" "abc.b"))
      (handler-case
	  (multiple-value-list (in-directory dir (ambiguous-files "*.*")))
	(error () (setq ret t))))
    ret))
