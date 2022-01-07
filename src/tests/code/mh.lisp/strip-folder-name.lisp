;;; Tests of mh:strip-folder-name.

(in-package "MH")

(import '(deftest:deftest))

(deftest strip-folder-name ("folder" strip-folder-name-0)
  "Test `strip-folder-name'."
  (strip-folder-name "folder"))

(deftest strip-folder-name ("folder" strip-folder-name-1)
  "Test `strip-folder-name' with a leading +."
  (strip-folder-name "+folder"))

(deftest strip-folder-name ("FOLDER" strip-folder-name-2)
  "Test `strip-folder-name' with capitals."
  (strip-folder-name "FOLDER"))

(deftest strip-folder-name ("FOLDER" strip-folder-name-3)
  "Test `strip-folder-name' with capitals and a leading +."
  (strip-folder-name "+FOLDER"))

(deftest strip-folder-name ("f" strip-folder-name-4)
  "Test `strip-folder-name' with a short name."
  (strip-folder-name "f"))

(deftest strip-folder-name ("f" strip-folder-name-5)
  "Test `strip-folder-name' with a short name with a leading +."
  (strip-folder-name "+f"))

(deftest strip-folder-name ("looabcdefghijklmnopqrstuvwxyzoongfolder" strip-folder-name-6)
  "Test `strip-folder-name' with a long name."
  (strip-folder-name "looabcdefghijklmnopqrstuvwxyzoongfolder"))

(deftest strip-folder-name ("looabcdefghijklmnopqrstuvwxyzoongfolder" strip-folder-name-7)
  "Test `strip-folder-name' with a long name with a leading +."
  (strip-folder-name "+looabcdefghijklmnopqrstuvwxyzoongfolder"))

(deftest strip-folder-name ("folder/subfolder" strip-folder-name-8)
  "Test `strip-folder-name' on a folder with a subfolder."
  (strip-folder-name "folder/subfolder"))

(deftest strip-folder-name ("folder/subfolder" strip-folder-name-9)
  "Test `strip-folder-name' on a folder with a subfolder."
  (strip-folder-name "+folder/subfolder"))


;;;; Errors.

(deftest strip-folder-name (t strip-folder-name-20)
  "Test `strip-folder-name' with the empty string."
  (let (ret)
    (handler-case
	(strip-folder-name "")
      (error () (setq ret t)))
    ret))

(deftest strip-folder-name (t strip-folder-name-21)
  "Test `strip-folder-name' with an empty folder name."
  (let (ret)
    (handler-case
	(strip-folder-name ())
      (error () (setq ret t)))
    ret))
