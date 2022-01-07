;;; Various array operations that are too expensive (in space) to do
;;; inline.

(in-package :x86)


;;;; Allocation

(define-assembly-routine (allocate-vector
			  (:policy :fast-safe)
			  (:translate allocate-vector)
			  (:arg-types positive-fixnum
				      positive-fixnum
				      positive-fixnum))
			 ((:arg type unsigned-reg eax-offset)
			  (:arg length any-reg ebx-offset)
			  (:arg words any-reg ecx-offset)
			  (:res result descriptor-reg edx-offset))
  (inst mov result (+ (1- (ash 1 lowtag-bits))
		      (* vector-data-offset word-bytes)))
  (inst add result words)
  (inst and result (lognot vm:lowtag-mask))
  (pseudo-atomic
   (allocation result result)
   (inst lea result (make-ea :byte :base result :disp other-pointer-type))
   (storew type result 0 other-pointer-type)
   (storew length result vector-length-slot other-pointer-type))
  (inst ret))


;;;; Hash primitives

(define-assembly-routine (sxhash-simple-string
			  (:translate %sxhash-simple-string)
			  (:policy :fast-safe)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg ebx-offset)
			  (:res result any-reg edx-offset)

			  (:temp length any-reg edi-offset)
			  (:temp esi unsigned-reg esi-offset)
			  (:temp ecx unsigned-reg ecx-offset)
			  (:temp eax unsigned-reg eax-offset))
  (declare (ignore result esi ecx eax))
  (loadw length string vector-length-slot other-pointer-type)
  (inst jmp (make-fixup 'sxhash-simple-substring :assembly-routine)))

(define-assembly-routine (sxhash-simple-substring
			  (:translate %sxhash-simple-substring)
			  (:policy :fast-safe)
			  (:arg-types * positive-fixnum)
			  (:result-types positive-fixnum))
			 ((:arg string descriptor-reg ebx-offset)
			  (:arg length any-reg edi-offset)
			  (:res result any-reg edx-offset)

			  (:temp esi unsigned-reg esi-offset)
			  (:temp ecx unsigned-reg ecx-offset)
			  (:temp eax unsigned-reg eax-offset))
  ;; Compute a pointer to where we are going to be extracting the bits.
  (inst lea esi	(make-ea :byte :base string
			 :disp (- (* vector-data-offset word-bytes)
				  other-pointer-type)))
  ;; Initialize the result.
  (inst xor result result)
  ;; Get the count.  If it's zero, blow out.
  (inst mov ecx length)
  (inst jecxz done)
  ;; Convert it into count of the number of full words.  If zero, then skip
  ;; to the part that handles the tail.
  (inst shr ecx 4)
  (inst jecxz do-extra)
  ;; Clear the direction flag, so we advance through memory.
  (inst cld)

  LOOP
  ;; Merge each successive word with the result.
  (inst lods eax)			; load 32-bits into eax and (+4 esi)

  (inst rol result 5)
  (inst xor result eax)
  (inst loop loop)

  DO-EXTRA
  ;; Now we have to take care of any bytes that don't make up a full word.
  ;; First, check to see how many of them there are.  If zero, blow out of
  ;; here.  Otherwise, multiply by 8.
  (inst mov ecx length)
  (inst and ecx (fixnum 3))
  (inst jecxz done)
  (inst shl ecx 1)

  ;; Grab the last word.
  (inst lods eax)

  ;; Convert the count into a mask.  The count is multiplied by 8, so we just
  ;; shift -1 left, which shifts count*8 zeros into the low order end.  We
  ;; then invert that, ending up with a mask of count*8 ones.
  (inst mov esi -1)
  (inst shl esi :cl)
  (inst not esi)
  ;; Use the mask to strip off the bits we aren't interested in, and merge
  ;; the remaining bits with the result.
  (inst and eax esi)
  (inst rol result 5)
  (inst xor result eax)

  DONE

  ;; Force result to be a positive fixnum.
  (inst and result #x7ffffffc)
  (inst ret))
