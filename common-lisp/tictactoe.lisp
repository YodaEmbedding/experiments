(defstruct coord x y)

(defun format-coord (coord)
  (if (null coord) ""
    (format nil "(~a ~a)" (coord-y coord) (coord-x coord))))

(defun print-info (board player)
  (format t "~a~a~%" "Player to move: " player)
  (format t "Move  | Evaluation~%")
  (mapcar (lambda (move)
            (format t "~a | ~a~%"
                    (format-coord move)
                    (evaluate-move board player move)))
          (get-move-list board))
  (format t "Best move:~%~a" (format-coord (get-best-move board player)))
  (format t "~%-------~%~%"))

(defun print-board (board)
  (dolist (row board) (format t "~a~%" row))
  (format t "~%"))

(defun copy-board (board)
  (loop for row in board collect (copy-list row)))

(defun board-at (board x y)
  (nth x (nth y board)))

(defun board-move (board player coord)
  (setf (nth (coord-x coord) (nth (coord-y coord) board)) player))

(defun copy-board-and-move (board player coord)
  (let ((board (copy-board board)))
    (board-move board player coord)
    board))

(defun other-player (player)
  (cond
    ((equal player "x") "o")
    ((equal player "o") "x")
    ((equal player " ") " ")))

(defun check-player-win (board player)
  (or
    (loop for row in board thereis (all-equal row player))
    (loop for i from 0 to 2 thereis
          (all-equal (loop for j from 0 to 2 collect (board-at board i j)) player))
    (all-equal (loop for i from 0 to 2 collect (board-at board i i)) player)
    (all-equal (loop for i from 0 to 2 collect (board-at board (- 2 i) i)) player)))

(defun all-equal (items value)
  (every (lambda (x) (equal x value)) items))

(defun evaluate (board player)
  (cond
    ((check-player-win board player) 1)
    ((check-player-win board (other-player player)) -1)
    (t 0)))

(defun evaluate-move (board player move)
  (- (negamax (copy-board-and-move board player move) (other-player player))))

(defun negamax (board player)
  (let ((moves (get-move-list board))
        (evaluation (evaluate board player)))
    (if (or (null moves) (not (eq 0 evaluation)))
      evaluation
      (loop for move in moves maximize (evaluate-move board player move)))))

(defun get-move-list (board)
  (loop for j from 0 to 2 nconcing
        (loop for i from 0 to 2
              when (equal (board-at board i j) " ")
              collect (make-coord :x i :y j))))

; TODO 2x redundant comparisons
(defun get-best-move (board player)
  (let ((moves (get-move-list board)))
    (if (null moves) ()
      (reduce (lambda (a b)
                (if (>= (evaluate-move board player a)
                        (evaluate-move board player b))
                  a b))
              moves))))

(defun check-game-end (board player)
  (not (and (get-move-list board)
            (eq 0 (evaluate board player)))))

(defun parse-input-move (input)
  (let ((xs (with-input-from-string (in input)
              (loop for x = (read in nil nil) while x collect x))))
    (make-coord :x (second xs) :y (first xs))))

(defun player-human-read ()
  (format t "Please input a move (for example: 0 1):~%")
  (parse-input-move (read-line)))

(defun make-player-ai-read (board player)
  (lambda () (get-best-move board player)))

(defun play-game (board player-x-read player-o-read)
  (do
    ((player "x" (other-player player)))
    ((check-game-end board player))
    (print-board board)
    (print-info board player)
    (board-move board player
                (cond ((equal player "x") (funcall player-x-read))
                      ((equal player "o") (funcall player-o-read)))))
  (print-board board)
  (cond
    ((check-player-win board "x") (format t "Congrats player x!~%"))
    ((check-player-win board "o") (format t "Congrats player o!~%"))
    (t (format t "Game ended in draw!~%"))))

(defvar *board*
  (loop for i from 0 to 2 collect
        (make-list 3 :initial-element " ")))

(play-game *board*
           'player-human-read
           (make-player-ai-read *board* "o"))
