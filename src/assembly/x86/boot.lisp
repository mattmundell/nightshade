;;; Boot.

#|
FIX
cl cleared by int #x10
|#

(in-package "X86")

(defvar *block-size-bits* 9)
(defvar *block-size* (ash 1 *block-size-bits*))
(defvar *cd-block-size* 2048)
(defvar *stack-size* *block-size*)
(defvar *module-size* *cd-block-size*)
(defvar *boot-high-loc*
  (ash (- #xA0000 (+ *stack-size* *module-size*)) -4))

(let ((*default-operand-size* :word))

  (define-assembly-routine
      (start
       (:return-style :none))
      ((:temp ax word-reg ax-offset)
       (:temp bx word-reg bx-offset)
       (:temp cx word-reg cx-offset)
       (:temp dx word-reg dx-offset)
       ;;
       (:temp esp unsigned-stack esp-offset)
       ;;
       (:temp sp word-reg sp-offset)
       (:temp si word-reg si-offset)
       (:temp di word-reg di-offset)
       ;;
       (:temp ah byte-reg ah-offset)
       (:temp bh byte-reg bh-offset)
       (:temp ch byte-reg ch-offset)
       (:temp dh byte-reg dh-offset)
       (:temp al byte-reg al-offset)
       (:temp bl byte-reg bl-offset)
       (:temp cl byte-reg cl-offset)
       (:temp dl byte-reg dl-offset))
    ZERO

    ;(inst toggle-data-size)
    (inst cld)        ; clear direction

    (inst nop)

    (inst jmp first)

    (inst nop)

    SECOND

    (inst nop)

    (inst jmp main)

    (inst nop)

    ;;; This is after SECOND, to enable the call to `label-position' for
    ;;; the byte instruction.

    FIRST

    (dotimes (i 30)
      (inst nop))

    (inst mov ax *boot-high-loc*)
    ;(inst mov es ax) ; FIX ES extended? segment
    (inst byte #x66)
    (inst byte #x8E)
    (inst byte #xC0)
    ;(inst mov ss ax) ; FIX SS stack segment?
    (inst byte #x66)
    (inst byte #x8E)
    (inst byte #xD0)

    (inst mov sp (+ *stack-size* *module-size*))

    (inst call get-instruction-pointer)

    GET-INSTRUCTION-POINTER
    (inst pop bx)
    ; FIX 6 too high for some reason, maybe due to asm with word operand size
    (inst sub bx (- (label-position get-instruction-pointer) 6))
    (inst shr bx 4)
    ;(inst mov ax cs)  ;; "copy high"   FIX CS is code segment
    (inst byte #x66)
    (inst byte #x8C)
    (inst byte #xC8)
    (inst add ax bx)
    ;(inst mov ds ax) ; FIX DS data segment
    (inst byte #x66)
    (inst byte #x8E)
    (inst byte #xD8)
    (inst mov cx *module-size*)
    (inst xor si si)
    (inst xor di di)
    (inst repe)
    (inst movs :byte)

    ;(inst mov ax ss)
    (inst byte #x66)
    (inst byte #x8C)
    (inst byte #xD0)
    ;(inst mov ds ax)
    (inst byte #x66)
    (inst byte #x8E)
    (inst byte #xD8)

    ;(inst jmp main)
    ;lt: DU1 0xEA;
    (inst byte #xEA)
    ;lt: DU2 BCD_MAIN,BOOT_HIGH_LOC;
    ; FIX 2 too high for some reason, maybe due to asm with word operand size
    (inst byte (- (label-position second) 2))
    (inst byte 0)
    (inst byte (logand #xff *boot-high-loc*))
    (inst byte (ash (logand #xff00 *boot-high-loc*) -8))

    ;; end of FIRST

    ;; FIX The call here is assembled one too many, so put an idle here.
    (inst nop)
    WRITE
    (inst nop)
    ;; get video page to bh
    (inst mov ah #xf)
    (inst int #x10)

    (inst mov al #x70) ; p
    (inst mov ah #xe)
    (inst int #x10)
    (inst ret)

    #|
    PAGE
    (inst byte 0)
    |#

    (inst nop)
    PUTS
    (inst nop)
    ;; get video page to bh
    (inst mov ah #xf)
    (inst int #x10)

    NEXT
    (inst lods al)
    (inst or al al)
    ;(inst cmp al 0)
    (inst jmp :z ret)

    (inst mov ah #xe)
    (inst int #x10)

    (inst jmp next)
    RET
    (inst ret)

    MESSAGE

#|
    (inst byte (char-code #\N))
    (inst byte (char-code #\i))
    (inst byte (char-code #\g))
    (inst byte (char-code #\h))
|#
    (every (lambda (char) (inst byte (char-code char)) t)
	   "Nightshade Boot CD...")
    (inst byte (char-code #\newline))
    (inst byte (char-code #\return))
    (inst byte 0)

    #|
    ;;(inst mov ss 0)
    ;(inst mov ax 0)  ; FIX weird, changes video mode
    ;(inst mov ax 2048)
    ;(inst mov ax 1024)
    ;(inst mov ax 512) ; seems to move cursor to start
    (dotimes (i 5) (inst nop))
    ; mov ss ax => 66 8E D0
    (inst byte #x66)
    (inst byte #x8E)
    (inst byte #xD0)
    (dotimes (i 5) (inst nop))

    (inst mov si (label-position message))
    |#

    (inst nop)

    MAIN

    ; FIX these are required
    (inst nop)
    (inst nop)

#|
    (inst mov ah #xf)
    (inst int #x10)
    (inst mov al (char-code #\A))
    (inst mov ah #xe)
    (inst int #x10)
|#

    ;(inst call write)

    ; FIX too high for some reason, maybe due to asm with word operand size
    (inst mov si (- (label-position message) 10))
    (inst call puts)

#|
    ;;; Print all the characters.

    (inst mov ah #xf)
    (inst int #x10)

    (inst mov cl 255)
    AGAIN
    (inst mov al cl)
    (inst or al al)
    (inst jmp :z stop)
    (inst sub cl 1)

    (inst mov ah #xe)
    ;(inst mov bh [page])   ; video page
    (inst int #x10)

    (inst mov al (char-code #\space))
    (inst mov ah #xe)
    (inst int #x10)

    (inst jmp again)
    STOP
|#

    ;; read
    (inst xor ax ax)
    (inst int #x16)

    ;; halt
    (inst hlt)

    (dotimes (i 100) (inst nop))))

#|
(when (eq c:*backend* c:*native-backend*)
  (load "target:assembly/assemfile"))

(user::comf "target:assembly/x86/boot" :assem t)
|#
