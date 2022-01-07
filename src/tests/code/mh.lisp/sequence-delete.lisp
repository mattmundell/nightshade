;;; Tests of mh:sequence-delete.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))


;;;; Number.

(deftest sequence-delete (() sequence-delete-1)
  "Test `sequence-delete', removing a number from an empty list."
  (mh:sequence-delete 1 ()))

(deftest sequence-delete ('((3 . 3)) sequence-delete-2)
  "Test `sequence-delete', removing a new number from a single entry
   list."
  (mh:sequence-delete 1 '((3 . 3))))

(deftest sequence-delete ('((2 . 4)) sequence-delete-3)
  "Test `sequence-delete', removing a number from the front of a range."
  (mh:sequence-delete 1 '((1 . 4))))

(deftest sequence-delete ('((1 . 3)) sequence-delete-4)
  "Test `sequence-delete', removing a number from the end of a range."
  (mh:sequence-delete 4 '((1 . 4))))

(deftest sequence-delete ('((1 . 2) (4 . 4)) sequence-delete-5)
  "Test `sequence-delete', removing a number from the middle of a range."
  (mh:sequence-delete 3 '((1 . 4))))

(deftest sequence-delete ('((2 . 2) (8 . 19)) sequence-delete-6)
  "Test `sequence-delete', removing a new number from a multi entry list."
  (mh:sequence-delete 7 '((2 . 2) (8 . 19))))

(deftest sequence-delete ('((2 . 2) (8 . 19)) sequence-delete-7)
  "Test `sequence-delete', removing an existing number from the front of a
   range in a multi entry list."
  (mh:sequence-delete 8 '((2 . 2) (8 . 19))))

(deftest sequence-delete ('((3 . 18) (22 . 22)) sequence-delete-8)
  "Test `sequence-delete', removing an existing number from the end of a
   range in a multi entry list."
  (mh:sequence-delete 19 '((3 . 19) (22 . 22))))


;;;; String.

(deftest sequence-delete (() sequence-delete-21)
  "Test `sequence-delete', removing a number from an empty list."
  (mh:sequence-delete "1" ()))

(deftest sequence-delete ('((3 . 3)) sequence-delete-22)
  "Test `sequence-delete', removing a new number from a single entry
   list."
  (mh:sequence-delete "1" '((3 . 3))))

(deftest sequence-delete ('((2 . 4)) sequence-delete-23)
  "Test `sequence-delete', removing a number from the front of a range."
  (mh:sequence-delete "1" '((1 . 4))))

(deftest sequence-delete ('((1 . 3)) sequence-delete-24)
  "Test `sequence-delete', removing a number from the end of a range."
  (mh:sequence-delete "4" '((1 . 4))))

(deftest sequence-delete ('((1 . 2) (4 . 4)) sequence-delete-25)
  "Test `sequence-delete', removing a number from the middle of a range."
  (mh:sequence-delete "3" '((1 . 4))))

(deftest sequence-delete ('((2 . 2) (8 . 19)) sequence-delete-26)
  "Test `sequence-delete', removing a new number from a multi entry list."
  (mh:sequence-delete "8" '((2 . 2) (8 . 19))))

(deftest sequence-delete ('((3 . 18) (22 . 22)) sequence-delete-27)
  "Test `sequence-delete', removing an existing number from a multi entry
   list."
  (mh:sequence-delete "19" '((3 . 19) (22 . 22))))


;;;; Range.

(deftest sequence-delete (() sequence-delete-41)
  "Test `sequence-delete', removing a range from an empty list."
  (mh:sequence-delete '(1 . 3) ()))

(deftest sequence-delete ('((3 . 3)) sequence-delete-42)
  "Test `sequence-delete', removing a new range from a single entry
   list."
  (mh:sequence-delete '(1 . 2) '((3 . 3))))

(deftest sequence-delete ('((3 . 4)) sequence-delete-43)
  "Test `sequence-delete', removing a range from the front of a range."
  (mh:sequence-delete '(1 . 2) '((1 . 4))))

(deftest sequence-delete ('((1 . 2)) sequence-delete-44)
  "Test `sequence-delete', removing a range from the end of a range."
  (mh:sequence-delete '(3 . 4) '((1 . 4))))

(deftest sequence-delete ('((12 . 13) (17 . 17)) sequence-delete-45)
  "Test `sequence-delete', removing a range from the middle of a range."
  (mh:sequence-delete '(14 . 16)'((12 . 17))))

(deftest sequence-delete ('((2 . 2) (8 . 19)) sequence-delete-46)
  "Test `sequence-delete', removing a new range from a multi entry list."
  (mh:sequence-delete '(23 . 25) '((2 . 2) (8 . 19))))

(deftest sequence-delete ('((3 . 19)) sequence-delete-47)
  "Test `sequence-delete', removing an existing range from a multi entry
   list."
  (mh:sequence-delete '(22 . 22) '((3 . 19) (22 . 22))))

(deftest sequence-delete ('((3 . 12) (23 . 23)) sequence-delete-48)
  "Test `sequence-delete', removing a range that spans multiple existing
   ranges."
  (mh:sequence-delete '(13 . 22) '((3 . 19) (22 . 23))))


;;;; Errors.

(deftest sequence-delete (t sequence-delete-60)
  "Test `sequence-delete', removing an empty list."
  (let (ret)
    (handler-case
	(mh:sequence-delete () '((3 . 19) (1 . 1)))
      (error () (setq ret t)))
    ret))

(deftest sequence-delete (t sequence-delete-61)
  "Test `sequence-delete', removing a range with the start and end
   strings."
  (let (ret)
    (handler-case
	(mh:sequence-delete '("2" . "3") '((3 . 19) (1 . 1)))
      (error () (setq ret t)))
    ret))

(deftest sequence-delete (t sequence-delete-62)
  "Test `sequence-delete', removing a range with the start a string."
  (let (ret)
    (handler-case
	(mh:sequence-delete '("2" . 3) '((3 . 19) (1 . 1)))
      (error () (setq ret t)))
    ret))

(deftest sequence-delete (t sequence-delete-63)
  "Test `sequence-delete', removing a range with the end a string."
  (let (ret)
    (handler-case
	(mh:sequence-delete '(2 . "3") '((3 . 19) (1 . 1)))
      (error () (setq ret t)))
    ret))
