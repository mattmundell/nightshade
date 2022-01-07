;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/mips/insts.lisp,v 1.52 1999/04/30 11:54:53 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Description of the MIPS architecture.
;;;
;;; Written by William Lott
;;;
(in-package "MIPS")

(use-package "NEW-ASSEM")
(use-package "EXT")
(use-package "C")

(def-assembler-params
    :scheduler-p t
  :max-locations 68)


;;;; Constants, types, conversion functions, some disassembler stuff.

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    (zero zero-offset)
    (null null-offset)
    (t
     (if (eq (sb-name (sc-sb (tn-sc tn))) 'registers)
	 (tn-offset tn)
	 (error "~S isn't a register." tn)))))

(defun fp-reg-tn-encoding (tn)
  (declare (type tn tn))
  (unless (eq (sb-name (sc-sb (tn-sc tn))) 'float-registers)
    (error "~S isn't a floating-point register." tn))
  (tn-offset tn))

(disassem:set-disassem-params :instruction-alignment 32)

(defvar *disassem-use-lisp-reg-names* t)

(def-vm-support-routine location-number (loc)
  (etypecase loc
    (null)
    (number)
    (label)
    (fixup)
    (tn
     (ecase (sb-name (sc-sb (tn-sc loc)))
       (immediate-constant
	;; Can happen if $ZERO or $NULL are passed in.
	nil)
       (registers
	(unless (zerop (tn-offset loc))
	  (tn-offset loc)))
       (float-registers
	(+ (tn-offset loc) 32))))
    (symbol
     (ecase loc
       (:memory 0)
       (:hi-reg 64)
       (:low-reg 65)
       (:float-status 66)
       (:ctrl-stat-reg 67)
       (:r31 31)))))

(defparameter reg-symbols
  (map 'vector
       #'(lambda (name)
	   (cond ((null name) nil)
		 (t (make-symbol (concatenate 'string "$" name)))))
       *register-names*))

(disassem:define-argument-type reg
  :printer #'(lambda (value stream dstate)
	       (declare (stream stream) (fixnum value))
	       (let ((regname (aref reg-symbols value)))
		 (princ regname stream)
		 (disassem:maybe-note-associated-storage-ref
		  value
		  'registers
		  regname
		  dstate))))

(defparameter float-reg-symbols
  (coerce 
   (loop for n from 0 to 31 collect (make-symbol (format nil "$F~d" n)))
   'vector))

(disassem:define-argument-type fp-reg
  :printer #'(lambda (value stream dstate)
	       (declare (stream stream) (fixnum value))
	       (let ((regname (aref float-reg-symbols value)))
		 (princ regname stream)
		 (disassem:maybe-note-associated-storage-ref
		  value
		  'float-registers
		  regname
		  dstate))))

(disassem:define-argument-type control-reg
  :printer "(CR:#x~X)")

(disassem:define-argument-type relative-label
  :sign-extend t
  :use-label #'(lambda (value dstate)
		 (declare (type (signed-byte 16) value)
			  (type disassem:disassem-state dstate))
		 (+ (ash (1+ value) 2) (disassem:dstate-cur-addr dstate))))

(deftype float-format ()
  '(member :s :single :d :double :w :word))

(defun float-format-value (format)
  (ecase format
    ((:s :single) 0)
    ((:d :double) 1)
    ((:w :word) 4)))

(disassem:define-argument-type float-format
  :printer #'(lambda (value stream dstate)
	       (declare (ignore dstate)
			(stream stream)
			(fixnum value))
	       (princ (case value
			(0 's)
			(1 'd)
			(4 'w)
			(t '?))
		      stream)))

(defconstant compare-kinds
  '(:f :un :eq :ueq :olt :ult :ole :ule :sf :ngle :seq :ngl :lt :nge :le :ngt))

(defconstant compare-kinds-vec
  (apply #'vector compare-kinds))

(deftype compare-kind ()
  `(member ,@compare-kinds))

(defun compare-kind (kind)
  (or (position kind compare-kinds)
      (error "Unknown floating point compare kind: ~S~%Must be one of: ~S"
	     kind
	     compare-kinds)))

(disassem:define-argument-type compare-kind
  :printer compare-kinds-vec)

(defconstant float-operations '(+ - * /))

(deftype float-operation ()
  `(member ,@float-operations))

(defconstant float-operation-names
  ;; this gets used for output only
  #(add sub mul div))

(defun float-operation (op)
  (or (position op float-operations)
      (error "Unknown floating point operation: ~S~%Must be one of: ~S"
	     op
	     float-operations)))

(disassem:define-argument-type float-operation
  :printer float-operation-names)



;;;; Constants used by instruction emitters.

(defconstant special-op #b000000)
(defconstant bcond-op #b000001)
(defconstant cop0-op #b010000)
(defconstant cop1-op #b010001)
(defconstant cop2-op #b010010)
(defconstant cop3-op #b010011)



;;;; dissassem:define-instruction-formats

(defconstant immed-printer
  '(:name :tab rt (:unless (:same-as rt) ", " rs) ", " immediate))

;;; for things that use rt=0 as a nop
(defconstant immed-zero-printer
  '(:name :tab rt (:unless (:constant 0) ", " rs) ", " immediate))

(disassem:define-instruction-format
    (immediate 32 :default-printer immed-printer)
  (op :field (byte 6 26))
  (rs :field (byte 5 21) :type 'reg)
  (rt :field (byte 5 16) :type 'reg)
  (immediate :field (byte 16 0) :sign-extend t))

(eval-when (compile eval load)
  (defparameter jump-printer
    #'(lambda (value stream dstate)
	(let ((addr (ash value 2)))
	  (disassem:maybe-note-assembler-routine addr t dstate)
	  (write addr :base 16 :radix t :stream stream)))))

(disassem:define-instruction-format
    (jump 32 :default-printer '(:name :tab target))
  (op :field (byte 6 26))
  (target :field (byte 26 0) :printer jump-printer))

(defconstant reg-printer
  '(:name :tab rd (:unless (:same-as rd) ", " rs) ", " rt))

(disassem:define-instruction-format
    (register 32 :default-printer reg-printer)
  (op :field (byte 6 26))
  (rs :field (byte 5 21) :type 'reg)
  (rt :field (byte 5 16) :type 'reg)
  (rd :field (byte 5 11) :type 'reg)
  (shamt :field (byte 5 6) :value 0)
  (funct :field (byte 6 0)))

(disassem:define-instruction-format
    (break 32 :default-printer
	   '(:name :tab code (:unless (:constant 0) subcode)))
  (op :field (byte 6 26) :value special-op)
  (code :field (byte 10 16))
  (subcode :field (byte 10 6) :value 0)
  (funct :field (byte 6 0) :value #b001101))

(disassem:define-instruction-format
    (coproc-branch 32 :default-printer '(:name :tab offset))
  (op :field (byte 6 26))
  (funct :field (byte 10 16))
  (offset :field (byte 16 0)))

(defconstant float-fmt-printer
  '((:unless :constant funct)
    (:choose (:unless :constant sub-funct) nil)
    "." format))

(defconstant float-printer
  `(:name ,@float-fmt-printer
	  :tab
	  fd
	  (:unless (:same-as fd) ", " fs)
	  ", " ft))

(disassem:define-instruction-format
    (float 32 :default-printer float-printer)
  (op :field (byte 6 26) :value cop1-op)
  (filler :field (byte 1 25) :value 1)
  (format :field (byte 4 21) :type 'float-format)
  (ft :field (byte 5 16) :value 0)
  (fs :field (byte 5 11) :type 'fp-reg)
  (fd :field (byte 5 6) :type 'fp-reg)
  (funct :field (byte 6 0)))

(disassem:define-instruction-format
    (float-aux 32 :default-printer float-printer)
  (op :field (byte 6 26) :value cop1-op)
  (filler-1 :field (byte 1 25) :value 1)
  (format :field (byte 4 21) :type 'float-format)
  (ft :field (byte 5 16) :type 'fp-reg)
  (fs :field (byte 5 11) :type 'fp-reg)
  (fd :field (byte 5 6) :type 'fp-reg)
  (funct :field (byte 2 4))
  (sub-funct :field (byte 4 0)))

(disassem:define-instruction-format
    (float-op 32
	      :include 'float
	      :default-printer
	        '('f funct "." format
		  :tab
		  fd
		  (:unless (:same-as fd) ", " fs)
		  ", " ft))
  (funct        :field (byte 2 0) :type 'float-operation)
  (funct-filler :field (byte 4 2) :value 0)
  (ft           :value nil :type 'fp-reg))


;;;; Primitive emitters.

(define-emitter emit-word 32
  (byte 32 0))

(define-emitter emit-short 16
  (byte 16 0))

(define-emitter emit-immediate-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 16 0))

(define-emitter emit-jump-inst 32
  (byte 6 26) (byte 26 0))

(define-emitter emit-register-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 5 11) (byte 5 6) (byte 6 0))

(define-emitter emit-break-inst 32
  (byte 6 26) (byte 10 16) (byte 10 6) (byte 6 0))

(define-emitter emit-float-inst 32
  (byte 6 26) (byte 1 25) (byte 4 21) (byte 5 16)
  (byte 5 11) (byte 5 6) (byte 6 0))



;;;; Math instructions.

(defun emit-math-inst (segment dst src1 src2 reg-opcode immed-opcode
			       &optional allow-fixups)
  (unless src2
    (setf src2 src1)
    (setf src1 dst))
  (etypecase src2
    (tn
     (emit-register-inst segment special-op (reg-tn-encoding src1)
			 (reg-tn-encoding src2) (reg-tn-encoding dst)
			 0 reg-opcode))
    (integer
     (emit-immediate-inst segment immed-opcode (reg-tn-encoding src1)
			  (reg-tn-encoding dst) src2))
    (fixup
     (unless allow-fixups
       (error "Fixups aren't allowed."))
     (note-fixup segment :addi src2)
     (emit-immediate-inst segment immed-opcode (reg-tn-encoding src1)
			  (reg-tn-encoding dst) 0))))

(define-instruction add (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (signed-byte 16) null) src1 src2))
  (:printer register ((op special-op) (funct #b100000)))
  (:printer immediate ((op #b001000)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100000 #b001000)))

(define-instruction addu (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (signed-byte 16) fixup null) src1 src2))
  (:printer register ((op special-op) (funct #b100001)))
  (:printer immediate ((op #b001001)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100001 #b001001 t)))

(define-instruction sub (segment dst src1 &optional src2)
  (:declare
   (type tn dst)
   (type (or tn (integer #.(- 1 (ash 1 15)) #.(ash 1 15)) null) src1 src2))
  (:printer register ((op special-op) (funct #b100010)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (unless src2
     (setf src2 src1)
     (setf src1 dst))
   (emit-math-inst segment dst src1
		   (if (integerp src2) (- src2) src2)
		   #b100010 #b001000)))

(define-instruction subu (segment dst src1 &optional src2)
  (:declare
   (type tn dst)
   (type
    (or tn (integer #.(- 1 (ash 1 15)) #.(ash 1 15)) fixup null) src1 src2))
  (:printer register ((op special-op) (funct #b100011)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (unless src2
     (setf src2 src1)
     (setf src1 dst))
   (emit-math-inst segment dst src1
		   (if (integerp src2) (- src2) src2)
		   #b100011 #b001001 t)))

(define-instruction and (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (unsigned-byte 16) null) src1 src2))
  (:printer register ((op special-op) (funct #b100100)))
  (:printer immediate ((op #b001100) (immediate nil :sign-extend nil)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100100 #b001100)))

(define-instruction or (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (unsigned-byte 16) null) src1 src2))
  (:printer register ((op special-op) (funct #b100101)))
  (:printer immediate ((op #b001101)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100101 #b001101)))

(define-instruction xor (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (unsigned-byte 16) null) src1 src2))
  (:printer register ((op special-op) (funct #b100110)))
  (:printer immediate ((op #b001110)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100110 #b001110)))

(define-instruction nor (segment dst src1 &optional src2)
  (:declare (type tn dst src1) (type (or tn null) src2))
  (:printer register ((op special-op) (funct #b100111)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100111 #b000000)))

(define-instruction slt (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (signed-byte 16) null) src1 src2))
  (:printer register ((op special-op) (funct #b101010)))
  (:printer immediate ((op #b001010)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b101010 #b001010)))

(define-instruction sltu (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (signed-byte 16) null) src1 src2))
  (:printer register ((op special-op) (funct #b101011)))
  (:printer immediate ((op #b001011)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b101011 #b001011)))

(defconstant divmul-printer '(:name :tab rs ", " rt))

(define-instruction div (segment src1 src2)
  (:declare (type tn src1 src2))
  (:printer register ((op special-op) (rd 0) (funct #b011010)) divmul-printer)
  (:dependencies (reads src1) (reads src2) (writes :hi-reg) (writes :low-reg))
  (:delay 1)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src1)
		       (reg-tn-encoding src2) 0 0 #b011010)))

(define-instruction divu (segment src1 src2)
  (:declare (type tn src1 src2))
  (:printer register ((op special-op) (rd 0) (funct #b011011))
	    divmul-printer)
  (:dependencies (reads src1) (reads src2) (writes :hi-reg) (writes :low-reg))
  (:delay 1)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src1)
		       (reg-tn-encoding src2) 0 0 #b011011)))

(define-instruction mult (segment src1 src2)
  (:declare (type tn src1 src2))
  (:printer register ((op special-op) (rd 0) (funct #b011000)) divmul-printer)
  (:dependencies (reads src1) (reads src2) (writes :hi-reg) (writes :low-reg))
  (:delay 1)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src1)
		       (reg-tn-encoding src2) 0 0 #b011000)))

(define-instruction multu (segment src1 src2)
  (:declare (type tn src1 src2))
  (:printer register ((op special-op) (rd 0) (funct #b011001)))
  (:dependencies (reads src1) (reads src2) (writes :hi-reg) (writes :low-reg))
  (:delay 1)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src1)
		       (reg-tn-encoding src2) 0 0 #b011001)))

(defun emit-shift-inst (segment opcode dst src1 src2)
  (unless src2
    (setf src2 src1)
    (setf src1 dst))
  (etypecase src2
    (tn
     (emit-register-inst segment special-op (reg-tn-encoding src2)
			 (reg-tn-encoding src1) (reg-tn-encoding dst)
			 0 (logior #b000100 opcode)))
    ((unsigned-byte 5)
     (emit-register-inst segment special-op 0 (reg-tn-encoding src1)
			 (reg-tn-encoding dst) src2 opcode))))

(defconstant shift-printer
  '(:name :tab
          rd
          (:unless (:same-as rd) ", " rt)
          ", " (:cond ((rs :constant 0) shamt)
                      (t rs))))

(define-instruction sll (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (unsigned-byte 5) null) src1 src2))
  (:printer register ((op special-op) (rs 0) (shamt nil) (funct #b000000))
	    shift-printer)
  (:printer register ((op special-op) (funct #b000100)) shift-printer)
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-shift-inst segment #b00 dst src1 src2)))

(define-instruction sra (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (unsigned-byte 5) null) src1 src2))
  (:printer register ((op special-op) (rs 0) (shamt nil) (funct #b000011))
	    shift-printer)
  (:printer register ((op special-op) (funct #b000111)) shift-printer)
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-shift-inst segment #b11 dst src1 src2)))

(define-instruction srl (segment dst src1 &optional src2)
  (:declare (type tn dst)
	    (type (or tn (unsigned-byte 5) null) src1 src2))
  (:printer register ((op special-op) (rs 0) (shamt nil) (funct #b000010))
	    shift-printer)
  (:printer register ((op special-op) (funct #b000110)) shift-printer)
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-shift-inst segment #b10 dst src1 src2)))


;;;; Floating point math.

(define-instruction float-op (segment operation format dst src1 src2)
  (:declare (type float-operation operation)
	    (type float-format format)
	    (type tn dst src1 src2))
  (:printer float-op ())
  (:dependencies (reads src1) (reads src2) (writes dst))
  (:delay 0)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format)
		    (fp-reg-tn-encoding src2) (fp-reg-tn-encoding src1)
		    (fp-reg-tn-encoding dst) (float-operation operation))))

(defconstant float-unop-printer
  `(:name ,@float-fmt-printer :tab fd (:unless (:same-as fd) ", " fs)))

(define-instruction fabs (segment format dst &optional (src dst))
  (:declare (type float-format format) (type tn dst src))
  (:printer float ((funct #b000101)) float-unop-printer)
  (:dependencies (reads src) (writes dst))
  (:delay 0)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format)
		    0 (fp-reg-tn-encoding src) (fp-reg-tn-encoding dst)
		    #b000101)))

(define-instruction fneg (segment format dst &optional (src dst))
  (:declare (type float-format format) (type tn dst src))
  (:printer float ((funct #b000111)) float-unop-printer)
  (:dependencies (reads src) (writes dst))
  (:delay 0)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format)
		    0 (fp-reg-tn-encoding src) (fp-reg-tn-encoding dst)
		    #b000111)))
  
(define-instruction fcvt (segment format1 format2 dst src)
  (:declare (type float-format format1 format2) (type tn dst src))
  (:printer float-aux ((funct #b10) (sub-funct nil :type 'float-format))
	   `(:name "." sub-funct "." format :tab fd ", " fs))
  (:dependencies (reads src) (writes dst))
  (:delay 0)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format2) 0
		    (fp-reg-tn-encoding src) (fp-reg-tn-encoding dst)
		    (logior #b100000 (float-format-value format1)))))

(define-instruction fcmp (segment operation format fs ft)
  (:declare (type compare-kind operation)
	    (type float-format format)
	    (type tn fs ft))
  (:printer float-aux ((fd 0) (funct #b11) (sub-funct nil :type 'compare-kind))
	    `(:name "-" sub-funct "." format :tab fs ", " ft))
  (:dependencies (reads fs) (reads ft) (writes :float-status))
  (:delay 1)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format) 
		    (fp-reg-tn-encoding ft) (fp-reg-tn-encoding fs) 0
		    (logior #b110000 (compare-kind operation)))))


;;;; Branch/Jump instructions.

(defun emit-relative-branch (segment opcode r1 r2 target)
  (emit-back-patch segment 4
		   #'(lambda (segment posn)
		       (emit-immediate-inst segment
					    opcode
					    (if (fixnump r1)
						r1
						(reg-tn-encoding r1))
					    (if (fixnump r2)
						r2
						(reg-tn-encoding r2))
					    (ash (- (label-position target)
						    (+ posn 4))
						 -2)))))

(define-instruction b (segment target)
  (:declare (type label target))
  (:printer immediate ((op #b000100) (rs 0) (rt 0)
		       (immediate nil :type 'relative-label))
	    '(:name :tab immediate))
  (:attributes branch)
  (:delay 1)
  (:emitter
   (emit-relative-branch segment #b000100 0 0 target)))

(define-instruction bal (segment target)
  (:declare (type label target))
  (:printer immediate ((op bcond-op) (rs 0) (rt #b01001)
		       (immediate nil :type 'relative-label))
	    '(:name :tab immediate))
  (:attributes branch)
  (:delay 1)
  (:emitter
   (emit-relative-branch segment bcond-op 0 #b10001 target)))


(define-instruction beq (segment r1 r2-or-target &optional target)
  (:declare (type tn r1)
	    (type (or tn fixnum label) r2-or-target)
	    (type (or label null) target))
  (:printer immediate ((op #b000100) (immediate nil :type 'relative-label)))
  (:attributes branch)
  (:dependencies (reads r1) (reads r2-or-target))
  (:delay 1)
  (:emitter
   (unless target
     (setf target r2-or-target)
     (setf r2-or-target 0))
   (emit-relative-branch segment #b000100 r1 r2-or-target target)))

(define-instruction bne (segment r1 r2-or-target &optional target)
  (:declare (type tn r1)
	    (type (or tn fixnum label) r2-or-target)
	    (type (or label null) target))
  (:printer immediate ((op #b000101) (immediate nil :type 'relative-label)))
  (:attributes branch)
  (:dependencies (reads r1) (reads r2-or-target))
  (:delay 1)
  (:emitter
   (unless target
     (setf target r2-or-target)
     (setf r2-or-target 0))
   (emit-relative-branch segment #b000101 r1 r2-or-target target)))

(defconstant cond-branch-printer
  '(:name :tab rs ", " immediate))

(define-instruction blez (segment reg target)
  (:declare (type label target) (type tn reg))
  (:printer
   immediate ((op #b000110) (rt 0) (immediate nil :type 'relative-label))
	    cond-branch-printer)
  (:attributes branch)
  (:dependencies (reads reg))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment #b000110 reg 0 target)))

(define-instruction bgtz (segment reg target)
  (:declare (type label target) (type tn reg))
  (:printer
   immediate ((op #b000111) (rt 0) (immediate nil :type 'relative-label))
	    cond-branch-printer)
  (:attributes branch)
  (:dependencies (reads reg))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment #b000111 reg 0 target)))

(define-instruction bltz (segment reg target)
  (:declare (type label target) (type tn reg))
  (:printer
   immediate ((op bcond-op) (rt 0) (immediate nil :type 'relative-label))
	    cond-branch-printer)
  (:attributes branch)
  (:dependencies (reads reg))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment bcond-op reg #b00000 target)))

(define-instruction bgez (segment reg target)
  (:declare (type label target) (type tn reg))
  (:printer
   immediate ((op bcond-op) (rt 1) (immediate nil :type 'relative-label))
	    cond-branch-printer)
  (:attributes branch)
  (:dependencies (reads reg))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment bcond-op reg #b00001 target)))

(define-instruction bltzal (segment reg target)
  (:declare (type label target) (type tn reg))
  (:printer
   immediate ((op bcond-op) (rt #b01000) (immediate nil :type 'relative-label))
	    cond-branch-printer)
  (:attributes branch)
  (:dependencies (reads reg) (writes :r31))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment bcond-op reg #b10000 target)))

(define-instruction bgezal (segment reg target)
  (:declare (type label target) (type tn reg))
  (:printer
   immediate ((op bcond-op) (rt #b01001) (immediate nil :type 'relative-label))
	    cond-branch-printer)
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads reg) (writes :r31))
  (:emitter
   (emit-relative-branch segment bcond-op reg #b10001 target)))

(defconstant j-printer
  '(:name :tab (:choose rs target)))

(define-instruction j (segment target)
  (:declare (type (or tn fixup) target))
  (:printer register ((op special-op) (rt 0) (rd 0) (funct #b001000))
	    j-printer)
  (:printer jump ((op #b000010)) j-printer)
  (:attributes branch)
  (:dependencies (reads target))
  (:delay 1)
  (:emitter
   (etypecase target
     (tn
      (emit-register-inst segment special-op (reg-tn-encoding target)
			  0 0 0 #b001000))
     (fixup
      (note-fixup segment :jump target)
      (emit-jump-inst segment #b000010 0)))))

(define-instruction jal (segment reg-or-target &optional target)
  (:declare (type (or null tn fixup) target)
	    (type (or tn fixup (integer -16 31)) reg-or-target))
  (:printer register ((op special-op) (rt 0) (funct #b001001)) j-printer)
  (:printer jump ((op #b000011)) j-printer)
  (:attributes branch)
  (:dependencies (if target (writes reg-or-target) (writes :r31)))
  (:delay 1)
  (:emitter
   (unless target
     (setf target reg-or-target)
     (setf reg-or-target 31))
   (etypecase target
     (tn
      (emit-register-inst segment special-op (reg-tn-encoding target) 0
			  reg-or-target 0 #b001001))
     (fixup
      (note-fixup segment :jump target)
      (emit-jump-inst segment #b000011 0)))))

(define-instruction bc1f (segment target)
  (:declare (type label target))
  (:printer coproc-branch ((op cop1-op) (funct #x100)
			   (offset nil :type 'relative-label)))
  (:attributes branch)
  (:dependencies (reads :float-status))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment cop1-op #b01000 #b00000 target)))

(define-instruction bc1t (segment target)
  (:declare (type label target))
  (:printer coproc-branch ((op cop1-op) (funct #x101)
			   (offset nil :type 'relative-label)))
  (:attributes branch)
  (:dependencies (reads :float-status))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment cop1-op #b01000 #b00001 target)))



;;;; Random movement instructions.

(define-instruction lui (segment reg value)
  (:declare (type tn reg)
	    (type (or fixup (signed-byte 16) (unsigned-byte 16)) value))
  (:printer immediate ((op #b001111)
		       (immediate nil :sign-extend nil :printer "#x~4,'0X")))
  (:dependencies (writes reg))
  (:delay 0)
  (:emitter
   (when (fixup-p value)
     (note-fixup segment :lui value)
     (setf value 0))
   (emit-immediate-inst segment #b001111 0 (reg-tn-encoding reg) value)))

(defconstant mvsreg-printer '(:name :tab rd))

(define-instruction mfhi (segment reg)
  (:declare (type tn reg))
  (:printer register ((op special-op) (rs 0) (rt 0) (funct #b010000))
	    mvsreg-printer)
  (:dependencies (reads :hi-reg) (writes reg))
  (:delay 2)
  (:emitter
   (emit-register-inst segment special-op 0 0 (reg-tn-encoding reg) 0
			#b010000)))

(define-instruction mthi (segment reg)
  (:declare (type tn reg))
  (:printer register ((op special-op) (rs 0) (rt 0) (funct #b010001))
	    mvsreg-printer)
  (:dependencies (reads reg) (writes :hi-reg))
  (:delay 0)
  (:emitter
   (emit-register-inst segment special-op 0 0 (reg-tn-encoding reg) 0
			#b010001)))

(define-instruction mflo (segment reg)
  (:declare (type tn reg))
  (:printer register ((op special-op) (rs 0) (rt 0) (funct #b010010))
	    mvsreg-printer)
  (:dependencies (reads :low-reg) (writes reg))
  (:delay 2)
  (:emitter
   (emit-register-inst segment special-op 0 0 (reg-tn-encoding reg) 0
			#b010010)))

(define-instruction mtlo (segment reg)
  (:declare (type tn reg))
  (:printer register ((op special-op) (rs 0) (rt 0) (funct #b010011))
	    mvsreg-printer)
  (:dependencies (reads reg) (writes :low-reg))
  (:delay 0)
  (:emitter
   (emit-register-inst segment special-op 0 0 (reg-tn-encoding reg) 0
			#b010011)))

(define-instruction move (segment dst src)
  (:declare (type tn dst src))
  (:printer register ((op special-op) (rt 0) (funct #b100001))
	    '(:name :tab rd ", " rs))
  (:attributes flushable)
  (:dependencies (reads src) (writes dst))
  (:delay 0)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src) 0
		       (reg-tn-encoding dst) 0 #b100001)))

(define-instruction fmove (segment format dst src)
  (:declare (type float-format format) (type tn dst src))
  (:printer float ((funct #b000110)) '(:name "." format :tab fd ", " fs))
  (:attributes flushable)
  (:dependencies (reads src) (writes dst))
  (:delay 0)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format) 0
		    (fp-reg-tn-encoding src) (fp-reg-tn-encoding dst)
		    #b000110)))

(defun %li (reg value)
  (etypecase value
    ((unsigned-byte 16)
     (inst or reg zero-tn value))
    ((signed-byte 16)
     (inst addu reg zero-tn value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (inst lui reg (ldb (byte 16 16) value))
     (inst or reg (ldb (byte 16 0) value)))
    (fixup
     (inst lui reg value)
     (inst addu reg value))))
  
(define-instruction-macro li (reg value)
  `(%li ,reg ,value))

(defconstant sub-op-printer '(:name :tab rd ", " rt))

(define-instruction mtc1 (segment to from)
  (:declare (type tn to from))
  (:printer register ((op cop1-op) (rs #b00100) (funct 0)) sub-op-printer)
  (:dependencies (reads from) (writes to))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00100 (reg-tn-encoding from)
		       (fp-reg-tn-encoding to) 0 0)))

(define-instruction mtc1-odd (segment to from)
  (:declare (type tn to from))
  (:dependencies (reads from) (writes to))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00100 (reg-tn-encoding from)
		       (1+ (fp-reg-tn-encoding to)) 0 0)))

(define-instruction mfc1 (segment to from)
  (:declare (type tn to from))
  (:printer register ((op cop1-op) (rs 0) (rd nil :type 'fp-reg) (funct 0))
	    sub-op-printer)
  (:dependencies (reads from) (writes to))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00000 (reg-tn-encoding to)
		       (fp-reg-tn-encoding from) 0 0)))

(define-instruction mfc1-odd (segment to from)
  (:declare (type tn to from))
  (:dependencies (reads from) (writes to))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00000 (reg-tn-encoding to)
		       (1+ (fp-reg-tn-encoding from)) 0 0)))

(define-instruction mfc1-odd2 (segment to from)
  (:declare (type tn to from))
  (:dependencies (reads from) (writes to))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00000 (1+ (reg-tn-encoding to))
		       (fp-reg-tn-encoding from) 0 0)))

(define-instruction mfc1-odd3 (segment to from)
  (:declare (type tn to from))
  (:dependencies (reads from) (writes to))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00000 (1+ (reg-tn-encoding to))
		       (1+ (fp-reg-tn-encoding from)) 0 0)))

(define-instruction cfc1 (segment reg cr)
  (:declare (type tn reg) (type (unsigned-byte 5) cr))
  (:printer register ((op cop1-op) (rs #b00010) (rd nil :type 'control-reg)
		      (funct 0)) sub-op-printer)
  (:dependencies (reads :ctrl-stat-reg) (writes reg))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00000 (reg-tn-encoding reg)
		       cr 0 0)))

(define-instruction ctc1 (segment reg cr)
  (:declare (type tn reg) (type (unsigned-byte 5) cr))
  (:printer register ((op cop1-op) (rs #b00110) (rd nil :type 'control-reg)
		      (funct 0)) sub-op-printer)
  (:dependencies (reads reg) (writes :ctrl-stat-reg))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00110 (reg-tn-encoding reg)
		       cr 0 0)))



;;;; Random system hackery and other noise

(define-instruction-macro entry-point ()
  nil)

(define-emitter emit-break-inst 32
  (byte 6 26) (byte 10 16) (byte 10 6) (byte 6 0))

(defun snarf-error-junk (sap offset &optional length-only)
  (let* ((length (system:sap-ref-8 sap offset))
         (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type system:system-area-pointer sap)
             (type (unsigned-byte 8) length)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond (length-only
           (values 0 (1+ length) nil nil))
          (t
           (kernel:copy-from-system-area sap (* mips:byte-bits (1+ offset))
                                         vector (* mips:word-bits
                                                   mips:vector-data-offset)
                                         (* length mips:byte-bits))
           (collect ((sc-offsets)
                     (lengths))
             (lengths 1)                ; the length byte
             (let* ((index 0)
                    (error-number (c::read-var-integer vector index)))
               (lengths index)
               (loop
                 (when (>= index length)
                   (return))
                 (let ((old-index index))
                   (sc-offsets (c::read-var-integer vector index))
                   (lengths (- index old-index))))
               (values error-number
                       (1+ length)
                       (sc-offsets)
                       (lengths))))))))

(defmacro break-cases (breaknum &body cases)
  (let ((bn-temp (gensym)))
    (collect ((clauses))
      (dolist (case cases)
        (clauses `((= ,bn-temp ,(car case)) ,@(cdr case))))
      `(let ((,bn-temp ,breaknum))
         (cond ,@(clauses))))))

(defun break-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (disassem:note x dstate))))
    (case (break-code chunk dstate)
      (#.vm:error-trap
       (nt "Error trap")
       (disassem:handle-break-args #'snarf-error-junk stream dstate))
      (#.vm:cerror-trap
       (nt "Cerror trap")
       (disassem:handle-break-args #'snarf-error-junk stream dstate))
      (#.vm:breakpoint-trap
       (nt "Breakpoint trap"))
      (#.vm:pending-interrupt-trap
       (nt "Pending interrupt trap"))
      (#.vm:halt-trap
       (nt "Halt trap"))
      (#.vm:function-end-breakpoint-trap
       (nt "Function end breakpoint trap"))
    )))

(define-instruction break (segment code &optional (subcode 0))
  (:declare (type (unsigned-byte 10) code subcode))
  (:printer break ((op special-op) (funct #b001101))
	    '(:name :tab code (:unless (:constant 0) subcode))
	    :control #'break-control )
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (emit-break-inst segment special-op code subcode #b001101)))

(define-instruction syscall (segment)
  (:printer register ((op special-op) (rd 0) (rt 0) (rs 0) (funct #b001100))
	    '(:name))
  :pinned
  (:delay 0)
  (:emitter
   (emit-register-inst segment special-op 0 0 0 0 #b001100)))

(define-instruction nop (segment)
  (:printer register ((op 0) (rd 0) (rd 0) (rs 0) (funct 0)) '(:name))
  (:attributes flushable)
  (:delay 0)
  (:emitter
   (emit-word segment 0)))

(def-vm-support-routine emit-nop (segment)
  (emit-word segment 0))

(define-instruction word (segment word)
  (:declare (type (or (unsigned-byte 32) (signed-byte 32)) word))
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (emit-word segment word)))

(define-instruction short (segment short)
  (:declare (type (or (unsigned-byte 16) (signed-byte 16)) short))
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (emit-short segment short)))

(define-instruction byte (segment byte)
  (:declare (type (or (unsigned-byte 8) (signed-byte 8)) byte))
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (emit-byte segment byte)))


(defun emit-header-data (segment type)
  (emit-back-patch
   segment 4
   #'(lambda (segment posn)
       (emit-word segment
		  (logior type
			  (ash (+ posn (component-header-length))
			       (- type-bits word-shift)))))))

(define-instruction function-header-word (segment)
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (emit-header-data segment function-header-type)))

#-gengc
(define-instruction lra-header-word (segment)
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (emit-header-data segment return-pc-header-type)))


(defun emit-compute-inst (segment vop dst src label temp calc)
  (emit-chooser
   ;; We emit either 12 or 4 bytes, so we maintain 8 byte alignments.
   segment 12 3
   #'(lambda (segment posn delta-if-after)
       (let ((delta (funcall calc label posn delta-if-after)))
	  (when (<= (- (ash 1 15)) delta (1- (ash 1 15)))
	    (emit-back-patch segment 4
			     #'(lambda (segment posn)
				 (assemble (segment vop)
					   (inst addu dst src
						 (funcall calc label posn 0)))))
	    t)))
   #'(lambda (segment posn)
       (let ((delta (funcall calc label posn 0)))
	 (assemble (segment vop)
		   (inst lui temp (ldb (byte 16 16) delta))
		   (inst or temp (ldb (byte 16 0) delta))
		   (inst addu dst src temp))))))

;; code = fn - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-fn (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (- other-pointer-type
			     (label-position label posn delta-if-after)
			     (component-header-length))))))

;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
;;      = lra - (header + label-offset)
#-gengc
(define-instruction compute-code-from-lra (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (- (+ (label-position label posn delta-if-after)
				(component-header-length)))))))

;; code = ra - header - label-offset + other-pointer-tag
;;      = ra + other-pointer-tag - (header + label-offset)
#+gengc
(define-instruction compute-code-from-ra (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (- other-pointer-type
			     (+ (label-position label posn delta-if-after)
				(component-header-length)))))))

;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
#-gengc
(define-instruction compute-lra-from-code (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (+ (label-position label posn delta-if-after)
			     (component-header-length))))))

;; ra = code - other-pointer-tag + header + label-offset
;;    = code + header + label-offset - other-pointer-tag
#+gengc
(define-instruction compute-ra-from-code (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
		      #'(lambda (label posn delta-if-after)
			  (- (+ (label-position label posn delta-if-after)
				(component-header-length))
			     other-pointer-type)))))


;;;; Loads and Stores

(defun emit-load/store-inst (segment opcode reg base index
                                     &optional (oddhack 0))
  (when (fixup-p index)
    (note-fixup segment :addi index)
    (setf index 0))
  (emit-immediate-inst segment opcode (reg-tn-encoding reg)
		       (+ (reg-tn-encoding base) oddhack) index))

(defconstant load-store-printer
  '(:name :tab
          rt ", "
          rs
          (:unless (:constant 0) "[" immediate "]")))

(define-instruction lb (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100000)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100000 base reg index)))

(define-instruction lh (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100001)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100001 base reg index)))

(define-instruction lwl (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100010)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100010 base reg index)))

(define-instruction lw (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100011)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100011 base reg index)))

;; next is just for ease of coding double-in-int c-call convention
(define-instruction lw-odd (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100011 base reg index 1)))

(define-instruction lbu (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100100)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100100 base reg index)))

(define-instruction lhu (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100101)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100101 base reg index)))

(define-instruction lwr (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100110)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100110 base reg index)))

(define-instruction sb (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b101000)) load-store-printer)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-load/store-inst segment #b101000 base reg index)))

(define-instruction sh (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b101001)) load-store-printer)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-load/store-inst segment #b101001 base reg index)))

(define-instruction swl (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b101010)) load-store-printer)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-load/store-inst segment #b101010 base reg index)))

(define-instruction sw (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b101011)) load-store-printer)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-load/store-inst segment #b101011 base reg index)))

(define-instruction swr (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b101110)) load-store-printer)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-load/store-inst segment #b101110 base reg index)))


(defun emit-fp-load/store-inst (segment opcode reg odd base index)
  (when (fixup-p index)
    (note-fixup segment :addi index)
    (setf index 0))
  (emit-immediate-inst segment opcode (reg-tn-encoding base)
		       (+ (fp-reg-tn-encoding reg) odd) index))

(define-instruction lwc1 (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b110001) (rt nil :type 'fp-reg)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-fp-load/store-inst segment #b110001 reg 0 base index)))

(define-instruction lwc1-odd (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-fp-load/store-inst segment #b110001 reg 1 base index)))

(define-instruction swc1 (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b111001) (rt nil :type 'fp-reg)) load-store-printer)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-fp-load/store-inst segment #b111001 reg 0 base index)))

(define-instruction swc1-odd (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-fp-load/store-inst segment #b111001 reg 1 base index)))

#+gengc 
(defun sw-and-maybe-remember (segment vop reg base index slot-p)
  (assemble (segment vop)
    (sc-case reg
      ((any-reg null zero)
       (inst sw reg base index))
      (descriptor-reg
       (cond (slot-p
	      (inst addu lip-tn base index)
	      (inst sw lip-tn ssb-tn 0)
	      (inst sw reg lip-tn 0))
	     (t
	      (inst sw base ssb-tn 0)
	      (inst sw reg base index)))
       (inst addu ssb-tn 4)))))

#+gengc
(define-instruction sw-and-remember-slot (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:dependencies (reads base) (reads reg) (writes :memory) (writes lip-tn))
  (:attributes variable-length)
  (:vop-var vop)
  (:emitter
   (sw-and-maybe-remember segment vop reg base index t)))

#+gengc
(define-instruction sw-and-remember-object
		    (segment reg base &optional (index 0))
  (:declare (type tn reg base)
	    (type (or (signed-byte 16) fixup) index))
  (:dependencies (reads base) (reads reg) (writes :memory) (writes lip-tn))
  (:attributes variable-length)
  (:vop-var vop)
  (:emitter
   (sw-and-maybe-remember segment vop reg base index t)))
