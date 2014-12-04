; The tic-tac-toe board is represented as a list. If each square in the board is labeled like so:
; nw n ne
; w  c e
; sw s se
; Then the list format is: (nw n ne w c e sw s se)

(defvar *board-rows* 3)
(defvar *board-cols* 3)
(defvar *max-plys* 3)
(defvar *human-player* 'X)
(defvar *computer-player* 'O)
(defvar *debug* nil)

; create an empty board
(defun clear-board ()
  (list 'B 'B 'B 'B 'B 'B 'B 'B 'B))

; create a list with the location of all things
(defun make-state (nw n ne w c e sw s se) 
  (list nw n ne w c e sw s se))

; convert from row, column to index
(defun index (row col)
  (+ (* row *board-cols*) col))

; what is in square row, col?
(defun square-row-col (row col state)
  (nth (index row col) state))

; pretty-print a board
(defun pretty-print (board)
  (format t "~%")
  (format t "~S ~S ~S~%" (nth 0 board) (nth 1 board) (nth 2 board))
  (format t "~S ~S ~S~%" (nth 3 board) (nth 4 board) (nth 5 board))
  (format t "~S ~S ~S~%" (nth 6 board) (nth 7 board) (nth 8 board)))

; did someone win?
; this could be re-written to be a more general - right now it only works for a 3x3 board
(defun col-winner (col player board)
  (cond ((>= col *board-cols*) nil)
        ((and (equal (nth col board) (nth (+ col *board-cols*) board))
              (equal (nth (+ col *board-cols*) board) (nth (+ col (* 2 *board-cols*)) board))
              (equal (nth (+ col (* 2 *board-cols*)) board) player))
         player)
        (t (col-winner (+ col 1) player board))))


; this could be re-written to be a more general - right now it only works for a 3x3 board
(defun row-winner (row player board)
  (cond ((>= row *board-rows*) nil)
        ((and (equal (nth (* row *board-cols*) board) (nth (+ 1 (* row *board-cols*)) board))
              (equal (nth (+ 1 (* row *board-cols*)) board) (nth (+ 2 (* row *board-cols*)) board))
              (equal (nth (+ 2 (* row *board-cols*)) board) player))
         player)
        (t (row-winner (+ row 1) player board))))


; this could be re-written to be a more general - right now it only works for a 3x3 board
(defun diagonal-winner-direction (player board direction)
  (cond ((and (equal direction 'nw-to-se)
              (equal (nth 0 board) (nth 4 board))
              (equal (nth 4 board) (nth 8 board))
              (equal (nth 8 board) player))
         player)
        ((and (equal direction 'se-to-nw)
              (equal (nth 2 board) (nth 4 board))
              (equal (nth 4 board) (nth 6 board))
              (equal (nth 6 board) player))
         player)
        (t nil)))

(defun diagonal-winner (player board)
  (or (diagonal-winner-direction player board 'nw-to-se)
      (diagonal-winner-direction player board 'se-to-nw)))

;(defun diagonal-winner (player board)
;  (cond ((and (equal (nth 0 board) (nth 4 board))
;              (equal (nth 4 board) (nth 8 board))
;              (equal (nth 8 board) player))
;         player)
;        ((and (equal (nth 2 board) (nth 4 board))
;              (equal (nth 4 board) (nth 6 board))
;              (equal (nth 6 board) player))
;         player)
;        (t nil)))


(defun winner (board)
  (or (col-winner 0 *human-player* board)
      (row-winner 0 *human-player* board)
      (diagonal-winner *human-player* board)
      (col-winner 0 *computer-player* board)
      (row-winner 0 *computer-player* board)
      (diagonal-winner *computer-player* board)))


(defun winner-player (player board)
  (or (col-winner 0 player board)
      (row-winner 0 player board)
      (diagonal-winner player board)))

;;
;; functions for changing the state (pg 178 - 179)
;;

; switch between players
(defun change-player (player)
  (if (equal player *human-player*) *computer-player* *human-player*))

; replace the nth item in a list with elem
(defun replace-nth (lst n elem)
  (cond 
    ((null lst) nil)
    ((= n 0) (cons elem (cdr lst)))
    (t (cons (car lst) (replace-nth (cdr lst) (- n 1) elem)))))

; put item in a square
(defun fill-in (player posn board)
  (if (equal 'B (nth posn board))     ; check to see if the space is open
    (replace-nth board posn player)   ; if it's empty, put the player's marker there
    nil))                             ; otherwise, return nil

;;
;; min-max search starts here
;;

; declare global variables
(defvar *open*)
(defvar *closed*)
(defvar *squares* '(0 1 2 3 4 5 6 7 8))


; given a state and its parent, return a list with both in the correct order
(defun build-record (state parent depth weight player) (list state parent depth weight player))

; state accessor: the first element in the list is the state
(defun get-state (state-tuple) (nth 0 state-tuple))

; parent accessor: the second element in the list is the parent state
(defun get-parent (state-tuple) (nth 1 state-tuple))

; depth accessor
(defun get-depth (state-tuple) (nth 2 state-tuple))

; weight accessor: value for choosing best option to search next
(defun get-weight (state-tuple) (nth 3 state-tuple))

; player accessor: which player is associated with that state
(defun get-player (state-tuple) (nth 4 state-tuple))

; the algorithm requires that a simple heuristic be used to evaluate
; states.  For the 8-puzzle, a simple heuristic 
; counts the number of players not in their goal positions:

(defun heuristic-eval (state)
  (random 10))


(defun heuristic (state)
 (heuristic-eval state))



; this function takes in a state and generates all of its descendants using the function calls in the "moves" list
(defun generate-descendants (board squares depth player) 
  (cond ((null squares) nil)
        (t            (let ((child (fill-in player (car squares) board))
                            ; keep picking things off the list until we get to the last move
                            (restOfList  (generate-descendants board (cdr squares) depth player)))
                        (cond ((null child)                            restOfList)
                              ; is it already in the list of children we've generated?
                              ((member child restOfList :test #'equal) restOfList)
                              ; is it already in the list of open states we need to check?
                              ((member child *open* :test #'equal)     restOfList)
                              ; is it in the list of states we've already checked?
                              ((member child *closed* :test #'equal)   restOfList)
                              ; if it's a new state, add it to our list
                              (t (cons (build-record child board depth (heuristic child) player) restOfList)))))))


; go through the closed list to pick the best move - since the current board has depth 0, we want to find the node of 
; depth 1 with the highest heuristic score
(defun find-best-move (max-posn max-weight current-posn)
  (cond (*debug* (format t "max-posn ~S~%" max-posn)
                 (format t "max-weight ~S~%" max-weight)
                 (format t "current-posn ~S~%" current-posn)))

  (cond ((>= current-posn (length *closed*)) max-posn)
        (t 
         (let ((current-state (nth current-posn *closed*)))
           (if *debug* (format t "depth ~S weight ~S~%" (get-depth current-state) (get-weight current-state)))
           (if (and (= 1 (get-depth current-state)) 
                    (< max-weight (get-weight current-state)))
             (find-best-move current-posn (get-weight current-state) (+ current-posn 1))
             (find-best-move max-posn max-weight (+ current-posn 1)))))))


(defun minimax ()
  (cond (nil (print "open") (print *open*)
             (print "closed") (print *closed*)))

  (cond ; if we've run out of states to search, then go through the closed list to find the best move
        ((null *open*) (get-state (nth (find-best-move -1 -1 0) *closed*)))
        (t ; if there are still states to search, then get all the kids, add them to the open list and recurse
         (let ((state-record (car *open*)))
           (if nil (format t "state-record ~S~%" state-record))
           (cond ; If we haven't hit the max depth, then generate the offspring and continue search
                 ((> *max-plys* (get-depth state-record)) 
                  ; add the current state to the visited states
                  (setq *closed* (cons state-record *closed*))
                  ; add the descendants to the open list
                  (setq *open* (append (generate-descendants 
                                        (get-state state-record) 
                                        *squares* 
                                        (+ 1 (get-depth state-record))
                                        (change-player (get-player state-record))) 
                                       (cdr *open*))))
                 (t 
                  ; add the current state to the visited states
                  (setq *closed* (cons state-record *closed*))
                  ; take the current state off the open list
                  (setq *open* (cdr *open*)))))
         ; recurse
         (minimax))))



; computer plays - using minimax
(defun computer-plays (board)
  ; start of the search is the current board state: (state parent depth weight)
  (setq *open* (list (build-record board nil 0 (heuristic board) *human-player*)))
  ; closed list is empty
  (setq *closed* nil)
  ; do minimax search
  (minimax)
)


(defun computer-plays-fake (board)
  (print "choose a square")
  (fill-in *computer-player* (read) board)
)

; ask the human for a move
(defun human-plays (board)
  (print "choose a square")
  ; (read) gets the move, fill-in puts that move on the board
  (fill-in *human-player* (read) board)
)


; This function checks to see if someone has won. If nobody has won, it figures out who goes next and lets them make
; a move
(defun tic-tac-toe (player board)
  (pretty-print board)
  (print "player")
  (print player)
  ; figure out if we had a winner, if not then the next person plays
  (cond ((winner board)                (print "winner") (winner board))
        ((equal player *human-player*) (tic-tac-toe (change-player player) (human-plays board)))
        (t                             (tic-tac-toe (change-player player) (computer-plays board)))))


; would you like to play a game?
(tic-tac-toe *human-player* (clear-board))
