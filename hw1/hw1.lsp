;; Q1 - Function PAD takes an argument n (a number) and returns the nth Padovian number, with the Padovian sequence 
;; defined as PAD(n+1) = PAD(n-1) + PAD(n-2) and the first three numbers in the sequence being 1. Since the first three
;; values in the Padovan sequence (0, 1, 2), the base cases, are 1, PAD(n) on n<3 would immediately return 1. For n>=3,
;; PAD(n) would return the sum of PAD(n-2) and PAD(n-3) since the recurrence relation previously stated is equivalent
;; to PAD(n) = PAD(n-2) + PAD(n-3). Each call of PAD(n) for n>=3 branches into two recursive calls.
;; Arguments:   n, an integer that is the index of the value in the Padovan sequence the function computes
;; Returned:    an integer that is the nth Padovan number: 1 if n<3, PAD(n-2) + PAD(n-3) if n>=3
(defun PAD(n)
  (if (< n 3)
      1                               
      (+ (PAD(- n 2)) (PAD(- n 3)))
  )
)

;; Q2 - Function SUMS takes an argument n and returns the number of additions used by the PAD(n) function. Since the
;; smallest three values of n (0, 1, 2), the base cases, would immediately return 1 in PAD and perform no additions,
;; SUMS(n) would return 0 in these cases. However, PAD(n) on n>=3 would perform one addition on the returned values
;; of PAD(n-2) and PAD(n-3), so SUMS(n) on n>=3 would add 1 to the sum of additions performed.
;; Arguments:   n, an integer that is the index of the value in the Padovan sequence the PAD function would compute
;; Returned:    an integer that is the number of additions performed by calling PAD(n) which computes the nth Padovan 
;;              number: 0 if n<3, SUMS(n-2)+SUMS(n-3)+1 if n>=3
(defun SUMS(n)
  (if (< n 3)
    0                                   
    (+ 1 (+ (SUMS (- n 2)) (SUMS (- n 3))))
  )
)

;; Q3 - Function ANON takes a tree as argument (either single-node atom or list) and returns the tree in the same 
;; structure, with every symbol and number replaced with '?'. The function first checks if the argument is null,
;; and if it is, an empty list is returned. If the argument is not null, the function checks if it is an atom.
;; If it is an atom, the function will return a '?' atom as it must replace every atom with '?'. if the argument
;; is a list, we check via car if the first item is an atom. If it is an atom, we recurisvely call ANON on the rest
;; of the lsit and append '?' to the returned list. This means one recursive call of ANON. If the first item is a 
;; list, we recursively call ANON on the first item (which is a list) and the rest of the list. This means two recursive
;; calls of ANON.
;; Arguments:   tree, which is either a single atom or a (nested) list of atoms
;; Returned:    either a single '?' atom or (nested) list with '?' as the only symbol/number
(defun ANON(tree)
  (if (not (null tree))
    (if (atom tree)
      '?                                                        ;; Single atom in list is replaced with '?' character
      (if (atom (car tree))
        (append (list '?) (ANON (cdr tree)))                    ;; If first item in list is atom, replace with '?' and call ATOM on rest of list
        (append (list (ANON (car tree))) (ANON (cdr tree)))     ;; If first item in list is list, call ATOM on that first item and rest of list
      )
    ) '()                                                       ;; ATOM function called on null value will return an empty list
  )
)


(print "Q1 TEST CASES: PAD(n)") (terpri)
* (loop for x in '(40)  ;; Test cases from 0 to 10 as argument to PAD function
  do (format t "PAD(~d) = ~d~%" x (PAD x)))
(terpri)

(print "Q2 TEST CASES: SUMS(n)") (terpri)
* (loop for x in '(40)  ;; Test cases from 0 to 10 as argument to SUMS function
  do (format t "SUMS(~d) = ~d~%" x (SUMS x)))
(terpri)