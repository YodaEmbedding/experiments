(defun square (x)
  (* x x))

(defun factorial (n)
  (if (< n 2)
    1
    (* n (factorial (- n 1)))))

(defun fib (n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(print (square 4))
(print (cons 0 '(1 2 3)))
(print (loop for i from 0 to 10 collect (fib i)))
(print (mapcar #'fib (loop for i from 0 to 10 collect i)))
(print (mapcar #'fib (loop :for n :below 11 :collect n)))
(print (mapcar #'factorial (loop :for n :below 10 :collect n)))
(format t "~%")

(defun hi-you (name)
  (format t "Hello ~a! ~%" name))

; (setf (readtable-case *readtable*) :preserve)
; (setq *print-case* :capitalize)
(defvar *name* (read))
(hi-you *name*)

