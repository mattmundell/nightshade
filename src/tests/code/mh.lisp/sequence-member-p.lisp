;;; Tests of mh:sequence-member-p.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))


;;;; Number.

(deftest sequence-member-p (() sequence-member-p-1)
  "Test `sequence-member-p', with a number and an empty list."
  (mh:sequence-member-p 1 ()))

(deftest sequence-member-p (() sequence-member-p-2)
  "Test `sequence-member-p', with a new number and a single entry
   list."
  (mh:sequence-member-p 1 '((3 . 3))))

(deftest sequence-member-p (t sequence-member-p-3)
  "Test `sequence-member-p', with a number and a single entry list."
  (mh:sequence-member-p 3 '((3 . 3))))

(deftest sequence-member-p (t sequence-member-p-4)
  "Test `sequence-member-p', with a number on the front of a range."
  (mh:sequence-member-p 1 '((1 . 4))))

(deftest sequence-member-p (t sequence-member-p-5)
  "Test `sequence-member-p', with a number on the end of a range."
  (mh:sequence-member-p 4 '((1 . 4))))

(deftest sequence-member-p (t sequence-member-p-6)
  "Test `sequence-member-p', with a number in the middle of a range."
  (mh:sequence-member-p 3 '((1 . 4))))

(deftest sequence-member-p (() sequence-member-p-7)
  "Test `sequence-member-p', with a new number and a multi entry list."
  (mh:sequence-member-p 7 '((2 . 2) (8 . 19))))

(deftest sequence-member-p (t sequence-member-p-8)
  "Test `sequence-member-p', with a number from the end of a range in a
   multi entry list."
  (mh:sequence-member-p 19 '((3 . 19) (22 . 22))))

(deftest sequence-member-p (t sequence-member-p-9)
  "Test `sequence-member-p', with a number from the front of a range in a
   multi entry list."
  (mh:sequence-member-p 3 '((3 . 19) (22 . 22))))

(deftest sequence-member-p (t sequence-member-p-10)
  "Test `sequence-member-p', with a number from the middle of a range in a
   multi entry list."
  (mh:sequence-member-p 11 '((3 . 19) (22 . 22))))


;;;; String.

(deftest sequence-member-p (() sequence-member-p-21)
  "Test `sequence-member-p', with a string and an empty list."
  (mh:sequence-member-p "1" ()))

(deftest sequence-member-p (() sequence-member-p-22)
  "Test `sequence-member-p', with a new string and a single entry
   list."
  (mh:sequence-member-p "1" '((3 . 3))))

(deftest sequence-member-p (t sequence-member-p-23)
  "Test `sequence-member-p', with a string and a single entry list."
  (mh:sequence-member-p "3" '((3 . 3))))

(deftest sequence-member-p (t sequence-member-p-24)
  "Test `sequence-member-p', with a string on the front of a range."
  (mh:sequence-member-p "1" '((1 . 4))))

(deftest sequence-member-p (t sequence-member-p-25)
  "Test `sequence-member-p', with a string on the end of a range."
  (mh:sequence-member-p "4" '((1 . 4))))

(deftest sequence-member-p (t sequence-member-p-26)
  "Test `sequence-member-p', with a string in the middle of a range."
  (mh:sequence-member-p "3" '((1 . 4))))

(deftest sequence-member-p (() sequence-member-p-27)
  "Test `sequence-member-p', with a new string and a multi entry list."
  (mh:sequence-member-p "7" '((2 . 2) (8 . 19))))

(deftest sequence-member-p (t sequence-member-p-28)
  "Test `sequence-member-p', with a string from the end of a range in a
   multi entry list."
  (mh:sequence-member-p "19" '((3 . 19) (22 . 22))))

(deftest sequence-member-p (t sequence-member-p-29)
  "Test `sequence-member-p', with a string from the front of a range in a
   multi entry list."
  (mh:sequence-member-p "3" '((3 . 19) (22 . 22))))

(deftest sequence-member-p (t sequence-member-p-30)
  "Test `sequence-member-p', with a string from the middle of a range in a
   multi entry list."
  (mh:sequence-member-p "11" '((3 . 19) (22 . 22))))


;;;; Errors.

(deftest sequence-member-p (t sequence-member-p-60)
  "Test `sequence-member-p', with an empty list."
  (let (ret)
    (handler-case
	(mh:sequence-member-p () '((3 . 19) (1 . 1)))
      (error () (setq ret t)))
    ret))

(deftest sequence-member-p (t sequence-member-p-61)
  "Test `sequence-member-p', with a range with the start and end
   strings."
  (let (ret)
    (handler-case
	(mh:sequence-member-p '("2" . "3") '((3 . 19) (1 . 1)))
      (error () (setq ret t)))
    ret))

(deftest sequence-member-p (t sequence-member-p-62)
  "Test `sequence-member-p', with a range with the start a string."
  (let (ret)
    (handler-case
	(mh:sequence-member-p '("2" . 3) '((3 . 19) (1 . 1)))
      (error () (setq ret t)))
    ret))

(deftest sequence-member-p (t sequence-member-p-63)
  "Test `sequence-member-p', with a range with the end a string."
  (let (ret)
    (handler-case
	(mh:sequence-member-p '(2 . "3") '((3 . 19) (1 . 1)))
      (error () (setq ret t)))
    ret))
