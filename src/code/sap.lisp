;;; Support for System Area Pointers (saps).

(in-package "SYSTEM")

(export '(system-area-pointer sap-ref-8 sap-ref-16 sap-ref-32 sap-ref-sap
	  signed-sap-ref-8 signed-sap-ref-16 signed-sap-ref-32
	  sap-ref-64 signed-sap-ref-64
	  sap+ sap- sap< sap<= sap= sap>= sap>
	  allocate-system-memory allocate-system-memory-at
	  reallocate-system-memory deallocate-system-memory))

(in-package "KERNEL")
(export '(%set-sap-ref-sap %set-sap-ref-single %set-sap-ref-double
	  %set-sap-ref-8 %set-signed-sap-ref-8
	  %set-sap-ref-16 %set-signed-sap-ref-16
	  %set-sap-ref-32 %set-signed-sap-ref-32
	  %set-sap-ref-64 %set-signed-sap-ref-64))
(in-package "SYSTEM")

(use-package "KERNEL")


#[ System Area Pointers

Note that in some cases an address is represented by a Lisp integer, and in
other cases it is represented by a real pointer.  Pointers are usually used
when an object in the current address space is being referred to.  The MACH
virtual memory manipulation calls must use integers, since in principle the
address could be in any process, and Lisp cannot abide random pointers.
Because these types are represented differently in Lisp, one must explicitly
coerce between these representations.

System Area Pointers (SAPs) provide a mechanism that bypasses the alien type
system and accesses virtual memory directly.  A SAP is a raw byte pointer into
the lisp process address space.  SAPs are represented with a pointer
descriptor, so SAP creation can cause consing.  However, the compiler uses
a non-descriptor representation for SAPs when possible, so the consing
overhead is generally minimal.

{function:system:sap-int}
{function:system:int-sap}
{function:system:sap+}
{function:system:sap-ref-8}
{function:system:sap-ref-16}
{function:system:sap-ref-32}
{function:system:signed-sap-ref-8}
{function:system:signed-sap-ref-16}
{function:system:signed-sap-ref-32}
]#


;;;; Primitive SAP operations.

(defun sap< (x y)
  "Return t if the SAP $x points to a smaller address then the SAP $y, else
   ()."
  (declare (type system-area-pointer x y))
  (sap< x y))

(defun sap<= (x y)
  "Return t if the SAP $x points to a smaller or the same address as the
   SAP Y, else ()."
  (declare (type system-area-pointer x y))
  (sap<= x y))

(defun sap= (x y)
  "Return t if the SAP $x points to the same address as the SAP $y."
  (declare (type system-area-pointer x y))
  (sap= x y))

(defun sap>= (x y)
  "Return t if the SAP $x points to a larger or the same address as the SAP
   $y."
  (declare (type system-area-pointer x y))
  (sap>= x y))

(defun sap> (x y)
  "Return t if the SAP $x points to a larger address then the SAP $y."
  (declare (type system-area-pointer x y))
  (sap> x y))

(defun sap+ (sap offset)
  "Return a new sap $offset bytes from $sap."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap+ sap offset))

(defun sap- (sap1 sap2)
  "Return the byte offset between $sap1 and $sap2."
  (declare (type system-area-pointer sap1 sap2))
  (sap- sap1 sap2))

(defun sap-int (sap)
  "Convert System Area Pointer $sap into an integer, suitable for passing
   to the kernel interfaces (which want all addresses specified as
   integers).

   The integer representation of a SAP is the byte offset of the SAP from
   the start of the address space."
  (declare (type system-area-pointer sap))
  (sap-int sap))

(defun int-sap (int)
  "Convert integer $int into a System Area Pointer.

   The integer representation of a SAP is the byte offset of the SAP from
   the start of the address space."
  (declare (type (unsigned-byte #-alpha #.vm:word-bits #+alpha 64) int))
  (int-sap int))

(defun sap-ref-8 (sap offset)
  "Return the 8-bit byte at $offset bytes from $sap.

   $offset is always a byte offset, however many bits are accessed.

  `setf' may be used to deposit values into virtual memory."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-8 sap offset))

(defun sap-ref-16 (sap offset)
  "Return the 16-bit word at $offset bytes from $sap.

   $offset is always a byte offset, whatever the number of bits accessed.

   `setf' may be used to deposit values into virtual memory."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-16 sap offset))

(defun sap-ref-32 (sap offset)
  "Return the 32-bit dualword at $offset bytes from $sap.

   $offset is always a byte offset, whatever the number of bits accessed.

   `setf' may be used to deposit values into virtual memory."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-32 sap offset))

(defun sap-ref-64 (sap offset)
  "Return the 64-bit quadword at $offset bytes from $sap.

   $offset is always a byte offset, whatever the number of bits accessed.

   `setf' may be used to deposit values into virtual memory."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-64 sap offset))

(defun sap-ref-sap (sap offset)
  "Return the 32-bit system-area-pointer at $offset bytes from $sap."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-sap sap offset))

(defun sap-ref-single (sap offset)
  "Return the 32-bit single-float at $offset bytes from $sap."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-single sap offset))

(defun sap-ref-double (sap offset)
  "Return the 64-bit double-float at $offset bytes from $sap."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-double sap offset))

#+(or x86 long-float)
(defun sap-ref-long (sap offset)
  "Return the long-float at $offset bytes from $sap."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (sap-ref-long sap offset))

(defun signed-sap-ref-8 (sap offset)
  "Return the signed 8-bit byte at $offset bytes from $sap, returning a
   negative number if the high bit is set.

   $offset is always a byte offset, whatever the number of bits accessed.

   `setf' may be used to deposit values into virtual memory."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (signed-sap-ref-8 sap offset))

(defun signed-sap-ref-16 (sap offset)
  "Return the signed 16-bit word at $offset bytes from $sap, returning a
   negative number if the high bit is set.

   $offset is always a byte offset, whatever the number of bits accessed.

   `setf' may be used to deposit values into virtual memory."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (signed-sap-ref-16 sap offset))

(defun signed-sap-ref-32 (sap offset)
  "Return the signed 32-bit dualword at $offset bytes from $sap, returning
   a negative number if the high bit is set.

   $offset is always a byte offset, whatever the number of bits accessed.

   `setf' may be used to deposit values into virtual memory."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (signed-sap-ref-32 sap offset))

(defun signed-sap-ref-64 (sap offset)
  "Return the signed 64-bit quadword at $offset bytes from $sap, returning a
   negative number if the high bit is set.

   $offset is always a byte offset, whatever the number of bits accessed.

   `setf' may be used to deposit values into virtual memory."
  (declare (type system-area-pointer sap)
	   (fixnum offset))
  (signed-sap-ref-64 sap offset))

(defun %set-sap-ref-8 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (unsigned-byte 8) new-value))
  (setf (sap-ref-8 sap offset) new-value))

(defun %set-sap-ref-16 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (unsigned-byte 16) new-value))
  (setf (sap-ref-16 sap offset) new-value))

(defun %set-sap-ref-32 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (unsigned-byte 32) new-value))
  (setf (sap-ref-32 sap offset) new-value))

(defun %set-sap-ref-64 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (unsigned-byte 64) new-value))
  (setf (sap-ref-64 sap offset) new-value))

(defun %set-signed-sap-ref-8 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (signed-byte 8) new-value))
  (setf (signed-sap-ref-8 sap offset) new-value))

(defun %set-signed-sap-ref-16 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (signed-byte 16) new-value))
  (setf (signed-sap-ref-16 sap offset) new-value))

(defun %set-signed-sap-ref-32 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (signed-byte 32) new-value))
  (setf (signed-sap-ref-32 sap offset) new-value))

(defun %set-signed-sap-ref-64 (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type (signed-byte 64) new-value))
  (setf (signed-sap-ref-64 sap offset) new-value))

(defun %set-sap-ref-sap (sap offset new-value)
  (declare (type system-area-pointer sap new-value)
	   (fixnum offset))
  (setf (sap-ref-sap sap offset) new-value))

(defun %set-sap-ref-single (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type single-float new-value))
  (setf (sap-ref-single sap offset) new-value))

(defun %set-sap-ref-double (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type double-float new-value))
  (setf (sap-ref-double sap offset) new-value))

#+long-float
(defun %set-sap-ref-long (sap offset new-value)
  (declare (type system-area-pointer sap)
	   (fixnum offset)
	   (type long-float new-value))
  (setf (sap-ref-long sap offset) new-value))


;;;; System memory allocation.

(alien:def-alien-routine ("os_allocate" allocate-system-memory)
			 system-area-pointer
  (bytes c-call:unsigned-long))

(alien:def-alien-routine ("os_allocate_at" allocate-system-memory-at)
			 system-area-pointer
  (address system-area-pointer)
  (bytes c-call:unsigned-long))

(alien:def-alien-routine ("os_reallocate" reallocate-system-memory)
			 system-area-pointer
  (old system-area-pointer)
  (old-size c-call:unsigned-long)
  (new-size c-call:unsigned-long))

(alien:def-alien-routine ("os_deallocate" deallocate-system-memory)
			 c-call:void
  (addr system-area-pointer)
  (bytes c-call:unsigned-long))
