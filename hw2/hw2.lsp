;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

;; Function BFS takes in a search tree represented by nested lists of atoms performs a breadth-first
;; traversal through the tree, listing the nodes from left to right by layer. Works by listing leaf nodes
;; on the current layer until the layer is completed, then calling itself recursively for all the subtrees.
;; Argument: FRINGE, a (nested) list of atoms that represent a search tree with atoms being leaves
;; Returned: a list of atoms (leaf nodes) listed by the breadth-first traversal through the tree
(defun BFS (FRINGE)
    (if (null FRINGE)
        '()
        (if (atom FRINGE)
            '(FRINGE)
            (if (atom (car FRINGE))
                (cons (car FRINGE) (BFS (cdr FRINGE)))
                (if (null (cdr FRINGE))
                    (BFS (car FRINGE))
                    (if (atom (cadr FRINGE))
                        (cons (cadr FRINGE) (BFS (cons (car FRINGE) (cddr FRINGE))))
                        (BFS (cons (append (car FRINGE) (cadr FRINGE)) (cddr FRINGE)))
                    )
                )
            )
        )
    )
)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (if (null S)    ;; Checks for unlikely case of null argument S
        '()
        (if (null (cdr S))
            (if (not (car S))   ;; Last element is being checked, so returns T if last element is T and NIL otherwise
                NIL
                T
            )
            (if (not (car S))   ;; FINAL-STATE is called recursively on the rest of the list if the first element is not NIL
                NIL
                (FINAL-STATE (cdr S))
            )
        )
    )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
    (if (not (equal (list-length S) 4))         ;; Checks for unlikely case of S not being of length 4
        NIL
        (cond ((equal A 'h)                     ;; Homer moves
            (if (equal (first S) (second S))    ;; If homer is on the same side as the baby at first and will leave
                (if (equal (second S) (third S))    ;; Leaves baby alone with dog
                    NIL
                    (if (equal (second S) (fourth S))   ;; Leaves baby alone with poison
                        NIL
                        (list (cons (not (first S)) (cdr S)))
                    )
                )
                (list (cons (not (first S)) (cdr S)))
            ))

            ((equal A 'b)                          ;; Homer moves the baby
                (if (not (equal (first S) (second S)))  ;; Homer is not on same side as baby, cannot carry
                    NIL
                    (list (cons (not (first S)) (cons (not (second S)) (cddr S)))) ;; Both the baby and homer change values
                )
            )

            ((equal A 'd)                          ;; Homer moves the dog
                (if (not (equal (first S) (third S)))  ;; Homer is not on same side as dog
                    NIL
                    (if (equal (second S) (fourth S))   ;; Baby and poison are on same side
                        NIL
                        (list (cons (not (first S)) (cons (second S) (list (not (third S)) (fourth S)))))
                    )  
                )

            )

            ((equal A 'p)                           ;; Homer moves the poison
                (if (not (equal (first S) (fourth S)))  ;; Homer is not on same side as poison
                    NIL
                    (if (equal (second S) (third S))    ;; Baby andn dog are on same side
                        NIL
                        (list (cons (not (first S)) (cons (second S) (list (third S) (not (fourth S))))))
                    )

                )
            )

            (NIL)   ;; returns NIL if A does not equal one of valid options: h, b, d, p
        )
    )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S) ;; Creates list of list called on S with all possible options for the entity to move
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
    (if (null STATES)                   ;; Exhaused STATES list without finding S, so we return NIL
        NIL
        (if (equal S (first STATES))
            T                           ;; Found S in STATES, we stop looking
            (ON-PATH S (cdr STATES))    ;; Search rest of STATES for S by calling ON-PATH recursively on remaining part of STATES
        )
    )
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
    (if (null STATES)
        NIL                                         ;; Does not return complete path from initial state to goal state if there are no more paths in STATES to search
        (if (null (DFS (car STATES) PATH))          ;; If DFS is unable to find path from initial state to goal state using the first value in STATES...
            (MULT-DFS (cdr STATES) PATH)            ;; search the rest of STATES for a viable path
            (DFS (car STATES) PATH)                 ;; If a path is found by DFS, return it 
        )
    )
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (if (FINAL-STATE S)                                     ;; Final state already found
        (append PATH (list S))
        (if (ON-PATH S PATH)
            NIL                                             ;; S has already been visited by the given PATH
            (MULT-DFS (SUCC-FN S) (append PATH (list S)))   ;; If not, call MULT-DFS to test set of possible next steps to find complete path
        )
    )
)