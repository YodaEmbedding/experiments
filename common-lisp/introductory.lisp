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

(defun hi-you (name)
  (format t "Hello ~a! ~%" name))

(print (square 4))
(print (cons 1 '(2 3 4)))
(print (mapcar 'fib (loop :for n :below 10 :collect n)))
(print (mapcar 'factorial (loop :for n :below 10 :collect n)))

; (setf (readtable-case *readtable*) :preserve)
; (setq *print-case* :capitalize)
(defvar *name* (read))
(hi-you *name*)

