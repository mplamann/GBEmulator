(defvar *VRAM* (make-array #x2000 :initial-element 0))
(defvar *RAM* (make-array #x2000 :initial-element 0))

(defun mem-read (addr)
  addr)
(defun mem-write (addr value)
  (+ addr value)
  (format t "~a" value))
