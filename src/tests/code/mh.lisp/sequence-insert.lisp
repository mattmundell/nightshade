;;; Tests of mh:sequence-insert.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))


;;;; Number.

(deftest sequence-insert ('((1 . 1)) sequence-insert-1)
  "Test `sequence-insert', inserting a number into an empty list."
  (mh:sequence-insert 1 ()))

(deftest sequence-insert ('((1 . 1) (3 . 3)) sequence-insert-2)
  "Test `sequence-insert', inserting a new number into a single entry
   list."
  (mh:sequence-insert 1 '((3 . 3))))

(deftest sequence-insert ('((1 . 2)) sequence-insert-3)
  "Test `sequence-insert', inserting a new number into a single entry list
   where the ranges coalesce."
  (mh:sequence-insert 1 '((2 . 2))))

(deftest sequence-insert ('((3 . 3)) sequence-insert-4)
  "Test `sequence-insert', inserting an existing number into a single entry
   list."
  (mh:sequence-insert 3 '((3 . 3))))

(deftest sequence-insert ('((3 . 3) (5 . 10) (12 . 12)) sequence-insert-5)
  "Test `sequence-insert', inserting a new number into a multi entry list."
  (mh:sequence-insert 12 '((3 . 3) (5 . 10))))

(deftest sequence-insert ('((2 . 2) (7 . 19)) sequence-insert-6)
  "Test `sequence-insert', inserting a new number into a multi entry list
   where the ranges coalesce."
  (mh:sequence-insert 7 '((2 . 2) (8 . 19))))

(deftest sequence-insert ('((3 . 19) (22 . 22)) sequence-insert-7)
  "Test `sequence-insert', inserting an existing number into a multi entry
   list."
  (mh:sequence-insert 19 '((22 . 22) (3 . 19))))


;;;; String.

(deftest sequence-insert ('((1 . 1)) sequence-insert-21)
  "Test `sequence-insert', inserting a string into an empty list."
  (mh:sequence-insert "1" ()))

(deftest sequence-insert ('((1 . 1) (3 . 3)) sequence-insert-22)
  "Test `sequence-insert', inserting a new string into a single entry
   list."
  (mh:sequence-insert "1" '((3 . 3))))

(deftest sequence-insert ('((1 . 2)) sequence-insert-23)
  "Test `sequence-insert', inserting a new string into a single entry list
   where the ranges coalesce."
  (mh:sequence-insert "1" '((2 . 2))))

(deftest sequence-insert ('((3 . 3)) sequence-insert-24)
  "Test `sequence-insert', inserting an existing string into a single entry
   list."
  (mh:sequence-insert "3" '((3 . 3))))

(deftest sequence-insert ('((3 . 3) (5 . 10) (12 . 12)) sequence-insert-25)
  "Test `sequence-insert', inserting a new string into a multi entry list."
  (mh:sequence-insert "12" '((3 . 3) (5 . 10))))

(deftest sequence-insert ('((2 . 2) (7 . 19)) sequence-insert-26)
  "Test `sequence-insert', inserting a new string into a multi entry list
   where the ranges coalesce."
  (mh:sequence-insert "7" '((2 . 2) (8 . 19))))

(deftest sequence-insert ('((3 . 19) (23 . 25)) sequence-insert-27)
  "Test `sequence-insert', inserting an existing string into a multi entry
   list."
  (mh:sequence-insert "19" '((3 . 19) (23 . 25))))


;;;; Range.

(deftest sequence-insert ('((1 . 3)) sequence-insert-41)
  "Test `sequence-insert', inserting a range into an empty list."
  (mh:sequence-insert '(1 . 3) ()))

(deftest sequence-insert ('((1 . 1) (3 . 3)) sequence-insert-42)
  "Test `sequence-insert', inserting a new range into a single entry
   list."
  (mh:sequence-insert '(1 . 1) '((3 . 3))))

(deftest sequence-insert ('((1 . 2)) sequence-insert-43)
  "Test `sequence-insert', inserting a new range into a single entry list
   where the ranges coalesce."
  (mh:sequence-insert '(1 . 2) '((2 . 2))))

(deftest sequence-insert ('((3 . 4)) sequence-insert-44)
  "Test `sequence-insert', inserting an existing range into a single entry
   list."
  (mh:sequence-insert '(3 . 4) '((3 . 4))))

(deftest sequence-insert ('((3 . 3) (5 . 10) (12 . 17)) sequence-insert-45)
  "Test `sequence-insert', inserting a new range into a multi entry list."
  (mh:sequence-insert '(12 . 17) '((3 . 3) (5 . 10))))

(deftest sequence-insert ('((2 . 2) (7 . 19)) sequence-insert-46)
  "Test `sequence-insert', inserting a new range into a multi entry list
   where the ranges coalesce."
  (mh:sequence-insert '(7 . 7) '((2 . 2) (8 . 19))))

(deftest sequence-insert ('((1 . 1) (3 . 19)) sequence-insert-47)
  "Test `sequence-insert', inserting an existing range into a multi entry
   list."
  (mh:sequence-insert '(17 . 19) '((3 . 19) (1 . 1))))


;;;; Errors.

(deftest sequence-insert (t sequence-insert-60)
  "Test `sequence-insert', inserting an empty list."
  (let (ret)
    (handler-case
	(mh:sequence-insert () '((3 . 19) (1 . 1)))
      (error () (setq ret t)))
    ret))

(deftest sequence-insert (t sequence-insert-61)
  "Test `sequence-insert', inserting a range with the start and end
   strings."
  (let (ret)
    (handler-case
	(mh:sequence-insert '("2" . "3") '((3 . 19) (1 . 1)))
      (error () (setq ret t)))
    ret))

(deftest sequence-insert (t sequence-insert-62)
  "Test `sequence-insert', inserting a range with the start a string."
  (let (ret)
    (handler-case
	(mh:sequence-insert '("2" . 3) '((3 . 19) (1 . 1)))
      (error () (setq ret t)))
    ret))

(deftest sequence-insert (t sequence-insert-63)
  "Test `sequence-insert', inserting a range with the end a string."
  (let (ret)
    (handler-case
	(mh:sequence-insert '(2 . "3") '((3 . 19) (1 . 1)))
      (error () (setq ret t)))
    ret))
