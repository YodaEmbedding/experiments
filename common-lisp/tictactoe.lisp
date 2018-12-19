(defun print-board (board)
  (progn
    (dolist (row board) (format t "~a~%" row))
    (format t "~%")))

(defun get-value (board x y)
  (nth x (nth y board)))

(defun make-move (board x y value)
  (setf (nth x (nth y board)) value))

(defun all-equal (items value)
  (every (lambda (x) (equal x value)) items))

(defun check-player-win (board player)
  (or
    (loop for row in board thereis (all-equal row player))
    (loop for i from 0 to 2 thereis
      (all-equal (loop for j from 0 to 2 collect (get-value board i j)) player))
    (all-equal (loop for i from 0 to 2 collect (get-value board i i)) player)
    (all-equal (loop for i from 0 to 2 collect (get-value board (- 2 i) i)) player)))

(defun check-game-result (board)
  (cond
    ((check-player-win board "x") "x")
    ((check-player-win board "o") "o")))

(defvar *board*
  (loop for i from 0 to 2 collect
    (make-list 3 :initial-element " ")))

; (defun evaluate)
; (defun minimax)
; (defun negamax)
; (min (loop for move in candidate-moves collect (negamax (- turn))))

(print-board *board*)
(make-move *board* 2 2 "o")
(make-move *board* 1 1 "x")
(make-move *board* 0 0 "o")
(print-board *board*)
(print (check-player-win *board* "x"))
(print (check-game-result *board*))

