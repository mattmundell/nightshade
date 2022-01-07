;;; Tests of lisp:file-writable.

(in-package "LISP")

(import '(deftest:deftest deftest:with-test-dir
			  deftest:with-test-search-list))


;;;; Directory.

(deftest file-writable (t file-writable-21)
  "Test `file-writable' with the absolute name of a writable directory with
   siblings, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/" "b/" "c" ".a")
    (file-writable (merge-pathnames "a/" dir))))

(deftest file-writable (t file-writable-22)
  "Test `file-writable' with a relative name of a writable directory with
   siblings, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/" "ba/" "b" ".b")
    (in-directory dir (file-writable "a/"))))

(deftest file-writable (t file-writable-23)
  "Test `file-writable' with the absolute name of a writable directory with
   siblings, in file name form."
  (with-test-dir (dir "bbbbbbb/" "bb" "ccccccc/")
    (file-writable (merge-pathnames "bbbbbbb" dir))))

(deftest file-writable (t file-writable-24)
  "Test `file-writable' with the relative name of a writable directory with
   siblings, in file name form."
  (with-test-dir (dir "bbbbbbb/" "c" "d.c")
    (in-directory dir (file-writable "bbbbbbb"))))

(deftest file-writable (() file-writable-25)
  "Test `file-writable' with the absolute name of a read-only directory
   with siblings, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/" "b/" "c" ".a")
    (setf (file-mode (merge-pathnames "a/" dir)) "u-w")
    (file-writable (merge-pathnames "a/" dir))))

(deftest file-writable (() file-writable-26)
  "Test `file-writable' with a relative name of a read-only directory with
   siblings, in directory name form (with the trailing slash)."
  (with-test-dir (dir "a/" "ba/" "b" ".b")
    (setf (file-mode (merge-pathnames "a/" dir)) "a-w")
    (in-directory dir (file-writable "a/"))))

(deftest file-writable (() file-writable-27)
  "Test `file-writable' with the absolute name of a read-only directory
   with siblings, in file name form."
  (with-test-dir (dir "bbbbbbb/" "bb" "ccccccc/")
    (setf (file-mode (merge-pathnames "bbbbbbb" dir)) "a-w")
    (file-writable (merge-pathnames "bbbbbbb" dir))))

(deftest file-writable (() file-writable-28)
  "Test `file-writable' with the relative name of a read-only directory with
   siblings, in file name form."
  (with-test-dir (dir "bbbbbbb/" "c" "d.c")
    (setf (file-mode (merge-pathnames "bbbbbbb" dir)) "u-w")
    (in-directory dir (file-writable "bbbbbbb"))))


;;;; File.

(deftest file-writable (t file-writable-31)
  "Test `file-writable' with the absolute name of a writable file with
   siblings."
  (with-test-dir (dir "a" "b" "dir/")
    (file-writable (merge-pathnames "a" dir))))

(deftest file-writable (t file-writable-32)
  "Test `file-writable' with a relative name of a writable file with
   siblings."
  (with-test-dir (dir "a" "bbb")
    (in-directory dir (file-writable "a"))))

(deftest file-writable (() file-writable-33)
  "Test `file-writable' with the absolute name of a read-only file with
   siblings."
  (with-test-dir (dir "a" "b" "dir/")
    (setf (file-mode (merge-pathnames "a" dir)) "a-w")
    (file-writable (merge-pathnames "a" dir))))

(deftest file-writable (() file-writable-34)
  "Test `file-writable' with a relative name of a read-only file with
   siblings."
  (with-test-dir (dir "a" "bbb")
    (setf (file-mode (merge-pathnames "a" dir)) "u-w")
    (in-directory dir (file-writable "a"))))


;;;; Symlink to file.

(deftest file-writable (t file-writable-40)
  "Test `file-writable' with the absolute name of a writable symlink with a
   writable file destination."
  (with-test-dir (dir "a" ("l" "a") "b" "dir/")
    (file-writable (merge-pathnames "l" dir))))

(deftest file-writable (t file-writable-41)
  "Test `file-writable' with a relative name of a writable symlink with a
   writable file destination."
  (with-test-dir (dir "a" ("l" "a") "bbb")
    (in-directory dir (file-writable "l"))))

(deftest file-writable (() file-writable-42)
  "Test `file-writable' with the absolute name of a writable symlink with a
   read-only file destination."
  (with-test-dir (dir "a" ("l" "a") "b" "dir/")
    (setf (file-mode (merge-pathnames "a" dir)) "a-w")
    (file-writable (merge-pathnames "l" dir))))

(deftest file-writable (() file-writable-43)
  "Test `file-writable' with a relative name of a writable symlink with a
   read-only file destination."
  (with-test-dir (dir "a" ("l" "a") "bbb")
    (setf (file-mode (merge-pathnames "a" dir)) "a-w")
    (in-directory dir (file-writable "l"))))

;; FIX Expected next two to be writable.

(deftest file-writable (() file-writable-44)
  "Test `file-writable' with the absolute name of a read-only symlink with
   a writable file destination."
  (with-test-dir (dir "a" ("l" "a") "b" "dir/")
    (setf (file-mode (merge-pathnames "l" dir)) "u-w")
    (file-writable (merge-pathnames "l" dir))))

(deftest file-writable (() file-writable-45)
  "Test `file-writable' with a relative name of a read-only symlink with a
   writable file destination."
  (with-test-dir (dir "a" ("l" "a") "bbb")
    (setf (file-mode (merge-pathnames "l" dir)) "a-w")
    (in-directory dir (file-writable "l"))))

(deftest file-writable (() file-writable-46)
  "Test `file-writable' with the absolute name of a read-only symlink with
   a read-only file destination."
  (with-test-dir (dir "a" ("l" "a") "b" "dir/")
    (setf (file-mode (merge-pathnames "l" dir)) "u-w")
    (setf (file-mode (merge-pathnames "a" dir)) "a-w")
    (file-writable (merge-pathnames "l" dir))))

(deftest file-writable (() file-writable-47)
  "Test `file-writable' with a relative name of a read-only symlink with a
   read-only file destination."
  (with-test-dir (dir "a" ("l" "a") "bbb")
    (setf (file-mode (merge-pathnames "l" dir)) "u-w")
    (setf (file-mode (merge-pathnames "a" dir)) "a-w")
    (in-directory dir (file-writable "l"))))


;;;; Symlink to directory.

(deftest file-writable (t file-writable-50)
  "Test `file-writable' with the absolute name of a writable symlink with a
   writable directory destination."
  (with-test-dir (dir "a/" ("l" "a") "b" "dir/")
    (file-writable (merge-pathnames "l" dir))))

(deftest file-writable (t file-writable-51)
  "Test `file-writable' with a relative name of a writable symlink with a
   writable directory destination."
  (with-test-dir (dir "a/" ("l" "a") "bbb")
    (in-directory dir (file-writable "l"))))

(deftest file-writable (() file-writable-52)
  "Test `file-writable' with the absolute name of a writable symlink with a
   read-only directory destination."
  (with-test-dir (dir "a/" ("l" "a") "b" "dir/")
    (setf (file-mode (merge-pathnames "a/" dir)) "a-w")
    (file-writable (merge-pathnames "l" dir))))

(deftest file-writable (() file-writable-53)
  "Test `file-writable' with a relative name of a writable symlink with a
   read-only directory destination."
  (with-test-dir (dir "a/" ("l" "a") "bbb")
    (setf (file-mode (merge-pathnames "a" dir)) "a-w")
    (in-directory dir (file-writable "l"))))

;; FIX Expected next two to be writable.

(deftest file-writable (() file-writable-54)
  "Test `file-writable' with the absolute name of a read-only symlink with
   a writable directory destination."
  (with-test-dir (dir "a/" ("l" "a") "b" "dir/")
    (setf (file-mode (merge-pathnames "l" dir)) "u-w")
    (file-writable (merge-pathnames "l" dir))))

(deftest file-writable (() file-writable-55)
  "Test `file-writable' with a relative name of a read-only symlink with a
   writable directory destination."
  (with-test-dir (dir "a/" ("l" "a") "bbb")
    (setf (file-mode (merge-pathnames "l" dir)) "a-w")
    (in-directory dir (file-writable "l"))))

(deftest file-writable (() file-writable-56)
  "Test `file-writable' with the absolute name of a read-only symlink with
   a read-only directory destination."
  (with-test-dir (dir "a/" ("l" "a") "b" "dir/")
    (setf (file-mode (merge-pathnames "l" dir)) "u-w")
    (setf (file-mode (merge-pathnames "a" dir)) "a-w")
    (file-writable (merge-pathnames "l" dir))))

(deftest file-writable (() file-writable-57)
  "Test `file-writable' with a relative name of a read-only symlink with a
   read-only directory destination."
  (with-test-dir (dir "a/" ("l" "a") "bbb")
    (setf (file-mode (merge-pathnames "l" dir)) "u-w")
    (setf (file-mode (merge-pathnames "a" dir)) "a-w")
    (in-directory dir (file-writable "l"))))


;;;; Symlink into subdirectory.

(deftest file-writable (t file-writable-60)
  "Test `file-writable' with the absolute name of a writable symlink with a
   writable directory destination."
  (with-test-dir (dir "a/s/" ("l" "a/s/") "b" "dir/")
    (file-writable (merge-pathnames "l" dir))))

(deftest file-writable (t file-writable-61)
  "Test `file-writable' with a relative name of a writable symlink with a
   writable directory destination."
  (with-test-dir (dir "a/s/" ("l" "a/s/") "bbb")
    (in-directory dir (file-writable "l"))))

(deftest file-writable (() file-writable-62)
  "Test `file-writable' with the absolute name of a writable symlink with a
   read-only directory destination."
  (with-test-dir (dir "a/s/" ("l" "a/s/") "b" "dir/")
    (setf (file-mode (merge-pathnames "a/s/" dir)) "a-w")
    (file-writable (merge-pathnames "l" dir))))

(deftest file-writable (() file-writable-63)
  "Test `file-writable' with a relative name of a writable symlink with a
   read-only directory destination."
  (with-test-dir (dir "a/s/" ("l" "a/s/") "bbb")
    (setf (file-mode (merge-pathnames "a/s/" dir)) "a-w")
    (in-directory dir (file-writable "l"))))

;; FIX Expected next two to be writable.

(deftest file-writable (() file-writable-64)
  "Test `file-writable' with the absolute name of a read-only symlink with
   a writable directory destination."
  (with-test-dir (dir "a/s/" ("l" "a/s/") "b" "dir/")
    (setf (file-mode (merge-pathnames "l" dir)) "u-w")
    (file-writable (merge-pathnames "l" dir))))

(deftest file-writable (() file-writable-65)
  "Test `file-writable' with a relative name of a read-only symlink with a
   writable directory destination."
  (with-test-dir (dir "a/s/" ("l" "a/s/") "bbb")
    (setf (file-mode (merge-pathnames "l" dir)) "a-w")
    (in-directory dir (file-writable "l"))))

(deftest file-writable (() file-writable-66)
  "Test `file-writable' with the absolute name of a read-only symlink with
   a read-only directory destination."
  (with-test-dir (dir "a/s/" ("l" "a/s/") "b" "dir/")
    (setf (file-mode (merge-pathnames "l" dir)) "u-w")
    (setf (file-mode (merge-pathnames "a/s/" dir)) "a-w")
    (file-writable (merge-pathnames "l" dir))))

(deftest file-writable (() file-writable-67)
  "Test `file-writable' with a relative name of a read-only symlink with a
   read-only directory destination."
  (with-test-dir (dir "a/s/" ("l" "a/s/") "bbb")
    (setf (file-mode (merge-pathnames "l" dir)) "u-w")
    (setf (file-mode (merge-pathnames "a/s/" dir)) "a-w")
    (in-directory dir (file-writable "l"))))


;;;; Symlink into parent directory.

(deftest file-writable (t file-writable-70)
  "Test `file-writable' with the absolute name of a writable symlink with a
   writable parent directory destination."
  (with-test-dir (dir "a/s/" ("a/l" "a/") "b" "dir/")
    (file-writable (merge-pathnames "a/l" dir))))

(deftest file-writable (() file-writable-73)
  "Test `file-writable' with a relative name of a writable symlink with a
   read-only parent directory destination."
  (with-test-dir (dir "a/s/" ("a/l" "a/") "bbb")
    (setf (file-mode (merge-pathnames "a/" dir)) "a-w")
    (prog1 (in-directory dir (file-writable "a/l"))
      (setf (file-mode (merge-pathnames "a/" dir)) "a+w"))))

;; FIX Expected this to be writable.

(deftest file-writable (() file-writable-74)
  "Test `file-writable' with the absolute name of a read-only symlink with
   a writable parent directory destination."
  (with-test-dir (dir "a/s/" ("a/s/l" "a/") "b" "dir/")
    (setf (file-mode (merge-pathnames "a/s/l" dir)) "u-w")
    (prog1 (file-writable (merge-pathnames "a/s/l" dir))
      (setf (file-mode (merge-pathnames "a/s/l" dir)) "u+w"))))

(deftest file-writable (() file-writable-77)
  "Test `file-writable' with a relative name of a read-only symlink with a
   read-only parent file destination."
  (with-test-dir (dir "a/s/" "a/b" ("a/s/l" "a/b") "bbb")
    (setf (file-mode (merge-pathnames "a/s/l" dir)) "u-w")
    (setf (file-mode (merge-pathnames "a/b" dir)) "a-w")
    (in-directory dir (file-writable "l"))))


;;;; Hidden file.

(deftest file-writable (t file-writable-80)
  "Test `file-writable' with the absolute name of a hidden file with
   siblings."
  (with-test-dir (dir ".a" ("l" "a") "b" "dir/")
    (file-writable (merge-pathnames ".a" dir))))

(deftest file-writable (t file-writable-81)
  "Test `file-writable' with a relative name of a hidden file with
   siblings."
  (with-test-dir (dir ".a.c" ("l" ".a") "bbb")
    (in-directory dir (file-writable ".a.c"))))

(deftest file-writable (() file-writable-82)
  "Test `file-writable' with the absolute name of a hidden file with
   siblings."
  (with-test-dir (dir ".ABC.ZZZ" ("l" "a") "b" "dir/")
    (setf (file-mode (merge-pathnames ".ABC.ZZZ" dir)) "u-w")
    (file-writable (merge-pathnames ".ABC.ZZZ" dir))))

(deftest file-writable (() file-writable-83)
  "Test `file-writable' with a relative name of a hidden file with
   siblings."
  (with-test-dir (dir ".abac" ("l" ".a") "bbb")
    (setf (file-mode (merge-pathnames ".abac" dir)) "a-w")
    (in-directory dir (file-writable ".abac"))))


;;;; Backup file.

(deftest file-writable (t file-writable-90)
  "Test `file-writable' with the absolute name of a writable backup file
   with siblings."
  (with-test-dir (dir "a~" ("l" "a~") "b" "dir/")
    (file-writable (merge-pathnames "a~" dir))))

(deftest file-writable (t file-writable-91)
  "Test `file-writable' with a relative name of a writable backup file with
   siblings."
  (with-test-dir (dir "a.BAK" ("l" ".a") "bbb")
    (in-directory dir (file-writable "a.BAK"))))

(deftest file-writable (() file-writable-92)
  "Test `file-writable' with the absolute name of a read-only backup file
   with siblings."
  (with-test-dir (dir "a~" ("l" "a~") "b" "dir/")
    (setf (file-mode (merge-pathnames "a~" dir)) "a-w")
    (file-writable (merge-pathnames "a~" dir))))

(deftest file-writable (() file-writable-93)
  "Test `file-writable' with a relative name of a read-only backup file
   with siblings."
  (with-test-dir (dir "a.BAK" ("l" ".a") "bbb")
    (setf (file-mode (merge-pathnames "a.BAK" dir)) "u-w")
    (in-directory dir (file-writable "a.BAK"))))


;;;; Search list.

(deftest file-writable (t file-writable-100)
  "Test `file-writable' with a search list bound to a writable directory of
   files."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (with-test-search-list ("a" dir)
      (file-writable "a:"))))

(deftest file-writable (() file-writable-101)
  "Test `file-writable' with a search list bound to a read-only directory
   of files."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      "abc.b" ("link" "abc.b"))
    (setf (file-mode dir) "a-w")
    (with-test-search-list ("a" dir)
      (prog1 (file-writable "a:")
	(setf (file-mode dir) "a+w")))))

(deftest file-writable (t file-writable-102)
  "Test `file-writable' with a search list bound to a writable directory
   full of hidden files."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      ".abc.b" (".link" ".abc.b"))
    (with-test-search-list ("a" dir)
      (file-writable "a:"))))

(deftest file-writable (() file-writable-103)
  "Test `file-writable' with a search list bound to a read-only directory
   full of hidden files."
  (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
		      ".abc.b" (".link" ".abc.b"))
    (setf (file-mode dir) "a-w")
    (with-test-search-list ("a" dir)
      (prog1 (file-writable "a:")
	(setf (file-mode dir) "a+w")))))

(deftest file-writable (t file-writable-104)
  "Test `file-writable' with a search list bound to an empty directory."
  (with-test-dir (dir)
    (with-test-search-list ("a" dir)
      (file-writable "a:"))))

(deftest file-writable (() file-writable-105)
  "Test `file-writable' with a search list bound to an empty directory."
  (with-test-dir (dir)
    (with-test-search-list ("a" dir)
      (setf (file-mode dir) "a-w")
      (prog1 (file-writable "a:")
	(setf (file-mode dir) "a+w")))))


;;;; Ancestor directory.

(deftest file-writable (t file-writable-110)
  "Test `file-writable' on a writable subdirectory."
  (with-test-dir (dir "a/" "a/b/" "a/b/c/" "a/b/c/f")
    (in-directory dir (file-writable "a/b/c"))))

(deftest file-writable (() file-writable-111)
  "Test `file-writable' on a read-only subdirectory."
  (with-test-dir (dir "a/" "a/b/" "a/b/c/" "a/b/c/f")
    (in-directory dir
      (setf (file-mode "a/b/c") "a-w")
      (prog1 (file-writable "a/b/c")
	(setf (file-mode "a/b/c") "a+w")))))

(deftest file-writable (t file-writable-112)
  "Test `file-writable' on a writable parent directory."
  (with-test-dir (dir "a/b/c/" "a/b/c/f")
    (in-directory dir
      (in-directory "a/b/c/"
	;; Inside a is b.
	(file-writable "../../b")))))

(deftest file-writable (() file-writable-113)
  "Test `file-writable' on a read-only parent directory."
  (with-test-dir (dir "a/b/c/" "a/b/c/f")
    (in-directory dir
      (setf (file-mode "a/b/") "a-w")
      (in-directory "a/b/c/"
	;; Inside a is b.
	(prog1 (file-writable "../../b")
	  (setf (file-mode "../..//b/") "a+w"))))))


;;;; Current directory.

(deftest file-writable (t file-writable-120)
  "Test `file-writable' on a writable current directory."
  (with-test-dir (dir "a/" "a/b/" "a/b/c/" "a/b/c/f")
    (in-directory dir
      (file-writable (merge-pathnames "." dir)))))

(deftest file-writable (() file-writable-121)
  "Test `file-writable' on a read-only current directory."
  (with-test-dir (dir "a/" "a/b/" "a/b/c/" "a/b/c/f")
    (setf (file-mode dir) "a-w")
    (prog1 (in-directory dir (file-writable "."))
      (setf (file-mode dir) "a+w"))))


;;;; Errors.

(deftest file-writable (t file-writable-130)
  "Test `file-writable' with wildcards."
  (let (ret)
    (with-test-dir (dir ".abc.c.BAK" (".li" ".abc.c.BAK")
			"abc.b" (".link" "abc.b"))
      (handler-case
	  (in-directory dir (file-writable "*.*"))
	(error () (setq ret t))))
    ret))

(deftest file-writable (() file-writable-131)
  "Test `file-writable' with a missing directory."
  (with-test-dir (dir)
    (in-directory dir (file-writable "aa/"))))

(deftest file-writable (() file-writable-132)
  "Test `file-writable' with a missing file."
  (with-test-dir (dir "b" "c/")
    (in-directory dir (file-writable "a"))))
