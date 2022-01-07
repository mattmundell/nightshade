;;; Tests of mh:parse-content-type.

(in-package "MH")

(import '(deftest:deftest))

(deftest parse-content-type ('("text" () ()) parse-content-type-0)
  "Test `parse-content-type' with just a type."
  (with-input-from-string (stream "text")
    (multiple-value-list (parse-content-type stream))))

(deftest parse-content-type ('("text" "plain" ()) parse-content-type-1)
  "Test `parse-content-type' with just the type and subtype."
  (with-input-from-string (stream "text/plain")
    (multiple-value-list (parse-content-type stream))))

(deftest parse-content-type ('("text" "plain"
			       (("charset" . "iso-8859-1")))
			     parse-content-type-2)
  "Test `parse-content-type' with a single parameter."
  (with-input-from-string (stream "text/plain; charset=iso-8859-1
")
    (multiple-value-list (parse-content-type stream))))

(deftest parse-content-type ('("multipart" "mixed"
			       (("boundary" . "aaaa")))
			     parse-content-type-3)
  "Test `parse-content-type' with a boundary parameter."
  (with-input-from-string (stream "multipart/mixed; boundary=aaaa")
    (multiple-value-list (parse-content-type stream))))

(deftest parse-content-type ('("multipart" "mixed"
			       (("boundary" . "aaaa")))
			     parse-content-type-4)
  "Test `parse-content-type' with a quoted parameter."
  (with-input-from-string (stream "multipart/mixed; boundary=\"aaaa\"")
    (multiple-value-list (parse-content-type stream))))

(deftest parse-content-type ('("multipart" "mixed"
			       (("boundary" . "=-=-=")))
			     parse-content-type-5)
  "Test `parse-content-type' with a quoted boundary parameter."
  (with-input-from-string (stream "multipart/mixed; boundary=\"=-=-=\"
")
    (multiple-value-list (parse-content-type stream))))

(deftest parse-content-type ('("multipart" "mixed"
			       (("boundary" . "aaa")
				("type" . "bbb")))
			     parse-content-type-6)
  "Test `parse-content-type' with a quoted boundary parameter and a type
   parameter."
  (with-input-from-string (stream "multipart/mixed; boundary=aaa; type=bbb")
    (multiple-value-list (parse-content-type stream))))

(deftest parse-content-type ('("Multipart" "report"
			       (("reporttype" . "delivery-status")
				("boundary" . "------------Boundary-00=_UQN9G6G0000000000000"))) parse-content-type-7)
  "Test `parse-content-type' with two parameters across three lines."
  (with-input-from-string (stream "Multipart/report;
reporttype=\"delivery-status\";
boundary=\"------------Boundary-00=_UQN9G6G0000000000000\"")
    (multiple-value-list (parse-content-type stream))))

(deftest parse-content-type ('("multipart" "related"
			       (("type" . "multipart/alternative")
				("boundary" . "----=_NextPart_000_0007_01C763F1.7CF6B680")))
			     parse-content-type-8)
  "Test `parse-content-type' with two parameters across three lines with
   indenting."
  (with-input-from-string (stream "multipart/related;
	type=\"multipart/alternative\";
	boundary=\"----=_NextPart_000_0007_01C763F1.7CF6B680\"")
    (multiple-value-list (parse-content-type stream))))
