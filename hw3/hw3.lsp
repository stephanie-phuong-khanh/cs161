;
; CS161 Hw3: Sokoban
; 
; NOTE: I collaborated with Anita Ilango (904938024) for this assignment. We discussed the
; solution for the next-state and try-move functions and wrote the detailed skeleton together.
;
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 




; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)

; Helper function to goal-test: checks row invidually
; for boxes, recursively calling itself on the rest of
; the row, returning NIL (state not at goal) if box is
; found. Returns T when no box is found.
(defun row-goal-test(s)
	(if (null s)
		T
		(if (isBox (car s))
			NIL
			(row-goal-test (cdr s))
		)
	)
)

; Checks row by row of state, calling itself recusively
; on the rest of the list of rows, which are checked
; via the row-goal-test helper function. Immediately
; returns NIL (state not at goal) if a box is found.
(defun goal-test (s)
	(if (null s)
		T
		(and (row-goal-test (car s)) (goal-test (cdr s)))
	)
);end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

; Helper function to try-move that takes as arguments state s, row r, and column c
; and returns the integer content at (row, column) = (r, c)
(defun get-square (s r c)
	(let* ((tmpRow (nth r s)) (curTarget (nth c tmpRow)))
		curTarget
	)
)

; Helper function to set-row that takes as arguments a row, current column (analogous
; to a loop's iterator), target column (index of square we want to change), and the value
; we want to change that square to. Recursively calls itself on the remainder of row until
; the square to be replaced is found, then we return a row with that square replaced with
; value.
(defun set-col (row currentCol targetCol value)
	(if (equal currentCol targetCol)
		(cons value (cdr row))
		(cons (car row) (set-col (cdr row) (+ currentCol 1) targetCol value))
	)
)

; Helper function to set-square that takes as arguments state s, current row (analogous
; to a loop's iterator), targetRow (index of the row we want to change), and value we want
; to change the specific square to. Recursively calls itself on the remaining rows of the
; state until the row to be replaced is found, then set-col is called on that row.
(defun set-row (s currentRow targetRow targetCol value)
	(if (equal currentRow targetRow)
		(cons (set-col (car s) 0 targetCol value) (cdr s))
		(cons (car s) (set-row (cdr s) (+ currentRow 1) targetRow targetCol value))
	)
)

; Helper function to try-move that takes as arguments state s, row r, column c and 
; integer square content value v. Returns new state with square at (row, column) = (r, c)
; to value v.
(defun set-square (s r c v)
	(set-row s 0 r c v)
)

; Helper function to try-move that takes as arguments state s, coordinates of current
; keeper position, and the change in coordinates we want to see in the movement (-1 0) for left,
; (1 0) for right, (0 -1) for up, and (0 1) for down. Returns final state after change or
; NIL if move is invalid.
(defun move-direction (s curK_X curK_Y offset_X offset_Y)
	(let* ((newK_Y (+ curK_Y offset_Y)) (newK_X (+ curK_X offset_X)) (curTarget (get-square s newK_Y newK_X)))
		(cond 
			((isBlank curTarget)
				(let* ((tmpState (set-square s newK_Y newK_X 3)))
					(cond
						((isKeeper (get-square s curK_Y curK_X)) (set-square tmpState curK_Y curK_X 0))
						((isKeeperStar (get-square s curK_Y curK_X)) (set-square tmpState curK_Y curK_X 4))
						(t ())
					)
				)
			)
			((isWall curTarget) ())
			((isBox curTarget)
				(let* ((newBox_Y (+ newK_Y offset_Y)) (newBox_X (+ newK_X offset_X)))
					(cond
						((isWall (get-square s newBox_Y newBox_X)) ())
						((isBox (get-square s newBox_Y newBox_X)) ())
						((isBoxStar (get-square s newBox_Y newBox_X)) ())
						((isBlank (get-square s newBox_Y newBox_X))
							(let* ((tmpState1 (set-square s newBox_Y newBox_X 2)) (tmpState2 (set-square tmpState1 newK_Y newK_X 3)))
								(cond
									((isKeeper (get-square s curK_Y curK_X)) (set-square tmpState2 curK_Y curK_X 0))
									((isKeeperStar (get-square s curK_Y curK_X)) (set-square tmpState2 curK_Y curK_X 4))
									(t ())
								)
							)
						)
						((isStar (get-square s newBox_Y newBox_X))
							(let* ((tmpState1 (set-square s newBox_Y newBox_X 5)) (tmpState2 (set-square tmpState1 newK_Y newK_X 3)))
								(cond
									((isKeeper (get-square s curK_Y curK_X)) (set-square tmpState2 curK_Y curK_X 0))
									((isKeeperStar (get-square s curK_Y curK_X)) (set-square tmpState2 curK_Y curK_X 4))
									(t ())
								)
							)
						)
					)
				)
			)
			((isStar curTarget)
				(let* ((tmpState (set-square s newK_Y newK_X 6)))
					(cond
						((isKeeper (get-square s curK_Y curK_X)) (set-square tmpState curK_Y curK_X 0))
						((isKeeperStar (get-square s curK_Y curK_X)) (set-square tmpState curK_Y curK_X 4))
						(t ())
					)
				)
			)
			((isBoxStar curTarget) 
				(let* ((newBox_Y (+ newK_Y offset_Y)) (newBox_X (+ newK_X offset_X)))
					(cond
						((isWall (get-square s newBox_Y newBox_X)) ())
						((isBox (get-square s newBox_Y newBox_X)) ())
						((isBoxStar (get-square s newBox_Y newBox_X)) ())
						((isBlank (get-square s newBox_Y newBox_X))
							(let* ((tmpState1 (set-square s newBox_Y newBox_X 2)) (tmpState2 (set-square tmpState1 newK_Y newK_X 6)))
								(cond
									((isKeeper (get-square s curK_Y curK_X)) (set-square tmpState2 curK_Y curK_X 0))
									((isKeeperStar (get-square s curK_Y curK_X)) (set-square tmpState2 curK_Y curK_X 4))
									(t ())
								)
							)
						)
						((isStar (get-square s newBox_Y newBox_X))
							(let* ((tmpState1 (set-square s newBox_Y newBox_X 5)) (tmpState2 (set-square tmpState1 newK_Y newK_X 6)))
								(cond
									((isKeeper (get-square s curK_Y curK_X)) (set-square tmpState2 curK_Y curK_X 0))
									((isKeeperStar (get-square s curK_Y curK_X)) (set-square tmpState2 curK_Y curK_X 4))
									(t ())
								)
							)
						)
					)
				)
			)
		)
	)
)

; Try-move is the helper function to next-states function. Takes as arguments
; s (current game state) and DIR (one of 4 possible moves: UP, DOWN, LEFT, RIGHT). 
(defun try-move (s DIR)

	; store current position of keeper, X and Y coordiates separetely, as local variable
	(let* ((curK (getKeeperPosition s 0)) (curK_X (first curK)) (curK_Y (second curK)))
		(cond
			((equal DIR 'UP) ; (curK_X, curK_Y-1)
				(move-direction s curK_X curK_Y 0 -1)
			)

			((equal DIR 'DOWN)	; (curK_X, curK_Y+1)
				(move-direction s curK_X curK_Y 0 1)
			)
			
			((equal DIR 'LEFT)	; (curK_X-1, curK_Y)
				(move-direction s curK_X curK_Y -1 0)
			)

			((equal DIR 'RIGHT) ; (curK_X+1, curK_Y)
				(move-direction s curK_X curK_Y 1 0)
			)
			
			(t ())	; Invalid DIR argument returns NIL
		)
	)
)

; Function next-states takes in current state s as a nested list of integers and returns a list
; possible states that the player can take (minimum 0, maximum 4), found by calling try-move
; on each of the 4 directions and returning their output in a cumulative list.
(defun next-states (s)
	(let* ((result (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT))))
		(cleanUpList result)
   	)
)

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)


; Helper function to count-boxes-row that counts the total number of boxes 
; in a row by checking the first square in the row and recursively calling
; itself on the remainder of the row, while keeping a running total
; in the variable count.
(defun count-boxes-row (row count)
	(if (null row)
		count
		(if (isBox (car row))
			(count-boxes-row (cdr row) (+ count 1))
			(count-boxes-row (cdr row) count)
		)
	)
)

; Helper function to h1 that counts the total number of boxes in a state by
; calling count-boxes-row on the first row in a state and recursively calling
; itself on the remainder of the nested list, while keeping a running total
; in the variable count.
(defun count-boxes(s count)
	(if (null s)
		count
		(+ (count-boxes-row (car s) 0) (count-boxes (cdr s) count))
	)
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; h1 calls count-boxes on the state s and returns total number of boxes
; in state s
;
; Yes, this heuristic is admissable because it measures the closeness of the
; current state to the solution, because if there are N boxes in state s, we
; will need a minimum of N moves to move all the boxes into their goal state.
(defun h1 (s)
	(count-boxes s 0)
)

; Checks each spot for a box, calling itself recursively on the rest of the
; row and returning a list of coordinates where a box exists.
(defun row-box-list(row rowNum colNum)
	(if (null row)
		NIL
		(if (isBox (car row))
			(cons (list rowNum colNum) (row-box-list (cdr row) rowNum (+ colNum 1)))
			(row-box-list (cdr row) rowNum (+ colNum 1))
		)
	)
)

; Calls row-box-list on each row of the state to check for the existence
; of boxes. Does so recursively by calling itself on the rest of the state.
(defun box-list (s rowNum)
	(if (null s)
		()
		(let* ((returnedList (row-box-list (car s) rowNum 0)))
			(if (null returnedList) 
				(box-list (cdr s) (+ rowNum 1))
				(append returnedList (box-list (cdr s) (+ rowNum 1)))
			)
		)
	)
)

; Returns list of coordinates of targets found in a row, like row-box-list
(defun row-target-list(row rowNum colNum)
	(if (null row)
		NIL
		(if (or (isStar (car row)) (isKeeperStar (car row)))
			(cons (list rowNum colNum) (row-target-list (cdr row) rowNum (+ colNum 1)))
			(row-target-list (cdr row) rowNum (+ colNum 1))
		)
	)
)

; Returns list of targets in a state like box-list
(defun target-list (s rowNum)
	(if (null s)
		()
		(let* ((returnedList (row-target-list (car s) rowNum 0)))
			(if (null returnedList) 
				(target-list (cdr s) (+ rowNum 1))
				(append returnedList (target-list (cdr s) (+ rowNum 1)))
			)
		)
	)
)

; Finds Manhattan distance from box to target
(defun findManhattan (boxRow boxCol targetRow targetCol)
	(+ (abs (- boxRow targetRow)) (abs (- boxCol targetCol)))
)

; For each box, returns the minimum Manhattan distance to every target.
(defun findMinManhattan (boxRow boxCol targetList)
	(if (null targetList)
		100000
		(min (findManhattan boxRow boxCol (first (car targetList)) (second (car targetList))) (findMinManhattan boxRow boxCol (cdr targetList)))
	)
)

; Totals up the manhattan distance from each box to its closest target.
(defun findManhattanTotal (boxList targetList)
	(if (null boxList)
		0
		(+ (findMinManhattan (first (car boxList)) (second (car boxList)) targetList) (findManhattanTotal (cdr boxList) targetList))
	)
)

; This simple heuristic finds the Manhattan distance between each box and
; its closest target and returns the sum of these distances.
(defun hManhattan (s)
	(let* ((targetList (target-list s 0)) (boxList (box-list s 0)))
		(findManhattanTotal boxList targetList)
	)
)

; I had written above a heuristic that takes into account Manhattan distances
; but I was not able to get it to perform consistently better than h1, so here
; I will use h1 as the heuristic called. I tried :(
(defun h604981556 (s)
	(h1 s)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );


;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun