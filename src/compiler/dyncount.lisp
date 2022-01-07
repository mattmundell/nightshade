;;; Support for collecting dynamic VOP statistics.

(in-package "C")

(export '(*collect-dynamic-statistics*
	  dyncount-info-counts dyncount-info-costs dyncount-info
	  dyncount-info-p count-me))

(defvar *collect-dynamic-statistics* nil
  "When T, emit extra code to collect dynamic statistics about VOP usages.")

(defvar *dynamic-counts-tn* nil
  "Holds the TN for the counts vector.")

(defstruct (dyncount-info
	    (:print-function %print-dyncount-info)
	    (:make-load-form-fun :just-dump-it-normally))
  for
  (costs (required-argument) :type (simple-array (unsigned-byte 32) (*)))
  (counts (required-argument) :type (simple-array (unsigned-byte 32) (*))))

(defprinter dyncount-info
  for
  costs
  counts)
