;;; Tests of mh:sequence-strings.

(in-package "USER")

(import '(deftest:deftest deftest:with-test-dir))

(deftest sequence-strings (() sequence-strings-1)
  "Test `sequence-strings', with an empty list."
  (mh:sequence-strings ()))

(deftest sequence-strings ('("3") sequence-strings-2)
  "Test `sequence-strings', with a single entry list."
  (mh:sequence-strings '((3 . 3))))

(deftest sequence-strings ('("1-4") sequence-strings-3)
  "Test `sequence-strings', with a single range."
  (mh:sequence-strings '((1 . 4))))

(deftest sequence-strings ('("2" "8-19") sequence-strings-7)
  "Test `sequence-strings', with a list of two entries."
  (mh:sequence-strings '((2 . 2) (8 . 19))))

(deftest sequence-strings ('("1-4" "8-19") sequence-strings-8)
  "Test `sequence-strings', with a list of two ranges."
  (mh:sequence-strings '((1 . 4) (8 . 19))))

(deftest sequence-strings ('("1" "11") sequence-strings-9)
  "Test `sequence-strings', with a list of two singles."
  (mh:sequence-strings '((1 . 1) (11 . 11))))

(deftest sequence-strings ('("1" "11-110" "120-122") sequence-strings-10)
  "Test `sequence-strings', with a list of three entries."
  (mh:sequence-strings '((1 . 1) (11 . 110) (120 . 122))))

(deftest sequence-strings ('("1" "120-122" "11-110") sequence-strings-11)
  "Test `sequence-strings', with a list of out-of-order entries."
  (mh:sequence-strings '((1 . 1) (120 . 122) (11 . 110))))

(deftest sequence-strings ('("11-121" "120-122") sequence-strings-12)
  "Test `sequence-strings', with a list of entries that could be merged."
  (mh:sequence-strings '((11 . 121) (120 . 122))))


;;;; Errors.

(deftest sequence-strings (t sequence-strings-60)
  "Test `sequence-strings', with strings in the list."
  (let (ret)
    (handler-case
	(mh:sequence-strings '(("3" . "19") (1 . 1)))
      (error () (setq ret t)))
    ret))

(deftest sequence-strings (t sequence-strings-61)
  "Test `sequence-strings', with a range with a start string in the list."
  (let (ret)
    (handler-case
	(mh:sequence-strings '(("2" . 3) (1 . 1)))
      (error () (setq ret t)))
    ret))

(deftest sequence-strings (t sequence-strings-62)
  "Test `sequence-strings', with a range with an end string in the list."
  (let (ret)
    (handler-case
	(mh:sequence-strings '((2 . "3") (1 . 1)))
      (error () (setq ret t)))
    ret))
