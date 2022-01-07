;;; Tests of mh:coerce-folder-name.

(in-package "MH")

(import '(deftest:deftest))

(deftest coerce-folder-name ("+folder" coerce-folder-name-0)
  "Test `coerce-folder-name'."
  (coerce-folder-name "folder"))

(deftest coerce-folder-name ("+folder" coerce-folder-name-1)
  "Test `coerce-folder-name' with a leading +."
  (coerce-folder-name "+folder"))

(deftest coerce-folder-name ("+FOLDER" coerce-folder-name-2)
  "Test `coerce-folder-name' with capitals."
  (coerce-folder-name "FOLDER"))

(deftest coerce-folder-name ("+FOLDER" coerce-folder-name-3)
  "Test `coerce-folder-name' with capitals and a leading +."
  (coerce-folder-name "+FOLDER"))

(deftest coerce-folder-name ("+f" coerce-folder-name-4)
  "Test `coerce-folder-name' with a short name."
  (coerce-folder-name "f"))

(deftest coerce-folder-name ("+f" coerce-folder-name-5)
  "Test `coerce-folder-name' with a short name with a leading +."
  (coerce-folder-name "+f"))

(deftest coerce-folder-name ("+looabcdefghijklmnopqrstuvwxyzoongfolder"
			     coerce-folder-name-6)
  "Test `coerce-folder-name' with a long name."
  (coerce-folder-name "looabcdefghijklmnopqrstuvwxyzoongfolder"))

(deftest coerce-folder-name ("+looabcdefghijklmnopqrstuvwxyzoongfolder"
			     coerce-folder-name-7)
  "Test `coerce-folder-name' with a long name with a leading +."
  (coerce-folder-name "+looabcdefghijklmnopqrstuvwxyzoongfolder"))

(deftest coerce-folder-name ("+folder/subfolder" coerce-folder-name-8)
  "Test `coerce-folder-name' on a folder with a subfolder."
  (coerce-folder-name "folder/subfolder"))

(deftest coerce-folder-name ("+folder/subfolder" coerce-folder-name-9)
  "Test `coerce-folder-name' on a folder with a subfolder."
  (coerce-folder-name "+folder/subfolder"))


;;;; Errors.

(deftest coerce-folder-name (t coerce-folder-name-20)
  "Test `coerce-folder-name' with the empty string."
  (let (ret)
    (handler-case
	(coerce-folder-name "")
      (error () (setq ret t)))
    ret))

(deftest coerce-folder-name (t coerce-folder-name-21)
  "Test `coerce-folder-name' with an empty folder name."
  (let (ret)
    (handler-case
	(coerce-folder-name ())
      (error () (setq ret t)))
    ret))
