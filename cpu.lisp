(defclass cpu ()
  ((a :initform 0 :accessor a)
   (b :initform 0 :accessor b)
   (c :initform 0 :accessor c)
   (d :initform 0 :accessor d)
   (e :initform 0 :accessor e)
   (f :initform 0 :accessor f)
   (h :initform 0 :accessor h)
   (l :initform 0 :accessor l)
   (pc :initform 0 :accessor pc)
   (sp :initform #xFFFE :accessor sp)))
(defmacro with-cpu (cpu &body body)
  `(with-accessors ((a a) (b b)
		    (c c) (d d)
		    (e e) (h h)
		    (l l) (sp sp)
		    (pc pc) (f f))
		   ,cpu
     ,@body))
(defun set-flags (cpu z n h-flag c-flag)
  (with-cpu
   cpu
   (setf f (+
	    (ash (if z 1 0) 7)
	    (ash (if n 1 0) 6)
	    (ash (if h-flag 1 0) 5)
	    (ash (if c-flag 1 0) 4)))))
(defun nullp (value)
  (eq value '()))

(defparameter *opcode-bases*
  #(nop ld ld inc-16 inc dec ld rlc ld add-16 ld dec-16 inc dec ld rrc  ; 0
    stop ld ld inc-16 inc dec ld rl jr add-16 ld dec-16 inc dec ld rr   ; 1
    jr ld ldi inc-16 inc dec ld daa jr add-16 ldi dec-16 inc dec ld cpl ; 2
    jr ld ldd inc-16 inc dec ld scf jr add-16 ldd dec-16 inc dec ld ccf ; 3
    ld ld ld ld ld ld ld ld ld ld ld ld ld ld ld ld                     ; 4
    ld ld ld ld ld ld ld ld ld ld ld ld ld ld ld ld                     ; 5
    ld ld ld ld ld ld ld ld ld ld ld ld ld ld ld ld                     ; 6
    ld ld ld ld ld ld halt ld ld ld ld ld ld ld ld ld                   ; 7
    add add add add add add add add adc adc adc adc adc adc adc adc     ; 8
    sub sub sub sub sub sub sub sub sbc sbc sbc sbc sbc sbc sbc sbc     ; 9
    gb-and gb-and gb-and gb-and gb-and gb-and gb-and gb-and
                                    xor xor xor xor xor xor xor xor     ; A
    gb-or  gb-or  gb-or  gb-or  gb-or  gb-or  gb-or  gb-or
                                    cp  cp  cp  cp  cp  cp  cp  cp      ; B
    ret pop jp jp call push add rst ret ret jp extern call call adc rst ; C
    ret pop jp xx call push sub rst ret ret jp xx call xx sbc rst       ; D
    ldh pop ldh xx xx push and rst add-16 jp ld xx xx xx xor rst        ; E
    ldh pop xx di xx push or rst ldhl ld ld ei xx xx cp ret             ; F
    ))

(defparameter *opcode-mappings*
  '(; 0x0_
    () (bc nn) (bc a) (bc)
    (b) (b) (b n) (a)
    (nn sp) (hl bc) (a bc) (bc)
    (c) (c) (c n) (a)
    ; 0x1_
    () (de nn) (de a) (de)
    (d) (d) (d n) (a)
    (n) (hl de) (a de) (de)
    (e) (e) (e n) (a)
    ; 0x2_
    (nz n) (hl nn) (hl a) (hl)
    (h) (h) (h n) ()
    (z n) (hl hl) (a hl) (hl)
    (l) (l) (l n) ()
    ; 0x3_
    (nc n) (hl nn) (hl a) (hl)
    (h) (h) (h n) ()
    (z n) (hl hl) (a hl) (hl)
    (l) (l) (l n) ()
    ; 0x4_
    (b b) (b c) (b d) (b e)
    (b h) (b l) (b hl) (b a)
    (c b) (c c) (c d) (c e)
    (c h) (c l) (c hl) (c a)
    ; 0x5_
    (d b) (d c) (d d) (d e)
    (d h) (d l) (d hl) (d a)
    (e b) (e c) (e d) (e e)
    (e h) (e l) (e hl) (e a)
    ; 0x6_
    (h b) (h c) (h d) (h e)
    (h h) (h l) (h hl) (h a)
    (l b) (l c) (l d) (l e)
    (l h) (l l) (l hl) (l a)
    ; 0x7_
    (hl b) (hl c) (hl d) (hl e)
    (hl h) (hl l) () (hl a)
    (a b) (a c) (a d) (a e)
    (a h) (a l) (a hl) (a a)
    ; 0x8_
    (a b) (a c) (a d) (a e)
    (a h) (a l) (a hl) (a a)
    (a b) (a c) (a d) (a e)
    (a h) (a l) (a hl) (a a)
    ; 0x9_
    (a b) (a c) (a d) (a e)
    (a h) (a l) (a hl) (a a)
    (a b) (a c) (a d) (a e)
    (a h) (a l) (a hl) (a a)
    ; 0xA_
    (b) (c) (d) (e)
    (h) (l) (hl) (a)
    (b) (c) (d) (e)
    (h) (l) (hl) (a)
    ; 0xB_
    (b) (c) (d) (e)
    (h) (l) (hl) (a)
    (b) (c) (d) (e)
    (h) (l) (hl) (a)
    ; 0xC_
    (nz) (bc) (nz nn) (nn)
    (nz nn) (bc) (a n) (0)
    (z) () (z nn) ()
    (z nn) (nn) (a n) (8)
    ; 0xD_
    (nc) (de) (nc nn) ()
    (nc nn) (de) (a n) (10)
    (c) () (c nn) ()
    (c nn) () (a n) (18)
    ; 0xE_
    (n a) (hl) (c a) ()
    () (hl) (n) (20)
    (sp d) (hl) (nn a) ()
    () () (n) (28)
    ; 0xF_
    (a n) (af) (xx) ()
    () (af) (n) (30)
    (sp d) (sp hl) (a nn) ()
    () () (n) (38)))

(defun make-16-bit-value (upper lower)
  (+ (ash upper 8) lower))
(defun decompose-16-bit-value (value)
  (values
   (ash value -8)
   (logand #xFF value)))
(defun write-to-mapping (cpu mapping value)
  (with-cpu cpu
     (if (eq '() mapping)
	 0
       (case (car mapping)
	 (nil 0)
	 (a (setq a value))
	 (b (setq b value))
	 (c (setq c value))
	 (d (setq d value))
	 (e (setq e value))
	 (h (setq h value))
	 (l (setq l value))
	 (hf (multiple-value-setq (h f) (decompose-16-bit-value value)))
	 (bc (multiple-value-setq (b c) (decompose-16-bit-value value)))
	 (de (multiple-value-setq (d e) (decompose-16-bit-value value)))
	 (hl (multiple-value-setq (h l) (decompose-16-bit-value value)))
	 (sp (setq sp value))
	 (pc (setq pc value))))))
(defun read-from-mapping (cpu mapping)
  (with-cpu cpu
    (if (nullp mapping)
	0
      (let ((source (if (nullp (cadr mapping))
			(car mapping)
		      (cadr mapping))))
	(case source
	  (a a)
	  (b b)
	  (c c)
	  (d d)
	  (e e)
	  (h h)
	  (l l)
	  (hf (make-16-bit-value h f))
	  (bc (make-16-bit-value b c))
	  (de (make-16-bit-value d e))
	  (hl (make-16-bit-value h l))
	  (sp sp)
	  (pc pc))))))

(defun run-instruction (cpu opcode)
  (multiple-value-bind (opcode-symbol mapping) (decode-instruction opcode)
    (interpret-instruction cpu opcode-symbol mapping)))

(defun decode-instruction (opcode)
  (values
   (elt *opcode-bases* opcode)
   (elt *opcode-mappings* opcode)))

(defun interpret-instruction (cpu opcode mapping)
  (funcall
   (symbol-function opcode)
   cpu
   (read-from-mapping cpu mapping)
   (lambda (value) (write-to-mapping cpu mapping value))))

(defmacro defopcode (name &body body)
  `(defun ,name (cpu arg write-function)
     (declare (ignorable cpu))
     (declare (ignorable arg))
     (declare (ignorable write-function))
     (with-cpu cpu
	       ,@body)))

(defmacro check-carry (bit old-value new-value)
  `(if (and (/= 0 (logand (ash 1 ,bit) ,old-value)) (= 0 (logand (ash 1 ,bit) ,new-value)))
      t
    nil))

(defmacro check-borrow (bit old-value argument new-value)
  (let ((mask (ash 1 (- bit 1))))
    `(if (= 0 (logand (logand ,mask (logxor ,old-value ,argument))
		      (logand ,mask (logxor ,old-value ,new-value))))
	 nil
       t)))

(defopcode ld
  (funcall write-function arg))

(defopcode add
  (let ((newA (+ a arg)))
    (set-flags cpu
	       (= (logand #xFF newA) 0)
	       nil
	       (check-carry 3 a newA)
	       (check-carry 7 a newA))
    (setf a newA)))
(defopcode add-16
  (let ((newHL (+ hl arg)))
    (set-flags cpu
	       (/= 0 (logand #x80 f))
	       nil
	       (check-carry 11 hl newHL)
	       (check-carry 15 hl newHL))))
(defopcode adc
  (let ((newA (+ a arg (if c 1 0))))
    (set-flags cpu
	       (= (logand #xFF newA) 0)
	       nil
	       (check-carry 3 a newA)
	       (check-carry 7 a newA))
    (setf a newA)))
(defopcode sub
  (let ((newA (- a arg)))
    (set-flags cpu
	       (= (logand #xFF newA) 0)
	       t
	       (check-borrow 4 a arg newA)
	       (check-borrow 8 a arg newA))
    (setf a newA)))
(defopcode sbc
  (let ((newA (- a arg (if c 1 0))))
    (set-flags cpu
	       (= (logand #xFF newA) 0)
	       t
	       (check-borrow 4 a arg newA)
	       (check-borrow 8 a arg newA))
    (setf a newA)))
(defopcode gb-and
  (setf a (logand a arg))
  (set-flags cpu (= a 0) nil t nil))
(defopcode gb-or
  (setf a (logior a arg))
  (set-flags cpu (= a 0) nil nil nil))
(defopcode xor
  (setf a (logxor a arg))
  (set-flags cpu (= a 0) nil nil nil))
(defopcode cp
    (let ((newA (- a arg)))
    (set-flags cpu
	       (= (logand newA #xFF) 0)
	       t
	       (check-borrow 4 a arg newA)
	       (check-borrow 8 a arg newA))))
(defopcode inc
  (let ((new-val (+ arg 1)))
    (set-flags cpu
	       (= (logand #xFF new-val) 0)
	       nil
	       (check-carry 3 a new-val)
	       nil)
    (funcall write-function new-val)))
(defopcode inc-16
  (funcall write-function (+ arg 1)))
(defopcode dec
  (let ((new-val (- arg 1)))
    (set-flags cpu
	       (= (logand #xFF new-val) 0)
	       t
	       (not (check-borrow 4 arg 1 new-val))
	       (/= 0 (logand #x10 f)))
    (funcall write-function new-val)))
(defopcode dec-16
  (funcall write-function (- arg 1)))
