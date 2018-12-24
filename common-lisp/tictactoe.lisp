(defstruct coord x y)

(defun print-info (board player)
  (mapcar (lambda (move)
            (format t "~a  ~a~%" move (evaluate-move board player move)))
          (get-move-list board)))

(defun print-board (board)
  (progn
    (dolist (row board) (format t "~a~%" row))
    (format t "~%")))

(defun copy-board (board)
  (loop for row in board collect (copy-list row)))

(defun get-value (board x y)
  (nth x (nth y board)))

(defun board-move (board player coord)
  (setf (nth (coord-x coord) (nth (coord-y coord) board)) player))

(defun copy-board-and-move (board player coord)
  (let ((board (copy-board board)))
    (board-move board player coord)
    board))

(defun all-equal (items value)
  (every (lambda (x) (equal x value)) items))

(defun check-player-win (board player)
  (or
    (loop for row in board thereis (all-equal row player))
    (loop for i from 0 to 2 thereis
          (all-equal (loop for j from 0 to 2 collect (get-value board i j)) player))
    (all-equal (loop for i from 0 to 2 collect (get-value board i i)) player)
    (all-equal (loop for i from 0 to 2 collect (get-value board (- 2 i) i)) player)))

(defun flatten-list (xs)
  (mapcan #'copy-list xs))

(defun get-move-list (board)
  (loop for j from 0 to 2 nconcing
        (loop for i from 0 to 2
              when (equal (get-value board i j) " ")
              collect (make-coord :x i :y j))))

(defun other-player (player)
  (cond
    ((equal player "x") "o")
    ((equal player "o") "x")
    ((equal player " ") " ")))

(defun evaluate (board player)
  (cond
    ((check-player-win board player) 1)
    ((check-player-win board (other-player player)) -1)
    (t 0)))

(defun negamax (board player)
  (let ((moves (get-move-list board)))
    (if (null moves)
      (evaluate board player)
      (loop for move in moves maximize (- (evaluate-move board player move))))))

(defun evaluate-move (board player move)
  (negamax (copy-board-and-move board player move) (other-player player)))

(defun get-best-move (board player)
  (let ((moves (get-move-list board))
        (other (other-player player)))
    (reduce (lambda (a b)
              (if (> (evaluate-move board player a)
                     (evaluate-move board player b))
                a b))
            moves)))

(defvar *board*
  (loop for i from 0 to 2 collect
        (make-list 3 :initial-element " ")))

(defvar *player* "x")
(defvar *best-move* nil)

(print-board *board*)
(print-info *board* *player*)
(format t "~%~a~%~%" "----")

(board-move *board* "x" (make-coord :x 2 :y 2))
(board-move *board* "o" (make-coord :x 1 :y 1))
(board-move *board* "x" (make-coord :x 0 :y 0))

(print-board *board*)
(print-info *board* *player*)
(format t "~%~a~%~%" "----")

(do ()
  ((null (get-move-list *board*)))
  (setf *best-move* (get-best-move *board* *player*))
  (board-move *board* *player* *best-move*)
  (print-board *board*)
  (setf *player* (other-player *player*))
  (print-info *board* *player*)
  (format t "~%~a~a~%~a~%~%" "Player to move: " *player* "----"))
