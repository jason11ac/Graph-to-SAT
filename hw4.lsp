;
; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that
; "node n gets color c" (when there are k possible colors).
;
(defun node2var (n c k)
  (+ (* (- n 1) k) c)
)

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
(defun at-least-one-color (n c k)
  (cond ((< k 1) NIL) ; k can't be less than 1
        ((< k c) NIL) ; k can't be less than c
        ((= k c) (list (node2var n c c)))
        (t (append (list (node2var n c k)) (at-least-one-color n (+ c 1) k)))
  )
)

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;
(defun at-most-one-color (n c k)
	(cond	((< k 1) NIL) ; k can't be less than 1
        ((< k c) NIL) ; k can't be less than c
        ((= k c) (list (list (* n c) (- (* n c))))) ; Only one choice for color. Can be either n*c or -n*c but not both
		    (t (at-most-helper (at-least-one-color n c k)))
	)
)

; helper function that appends permutations together
(defun at-most-helper (l)
	(cond ((< (length l) 2) NIL) ; if l is an single element atom
		    (t (append (permutations (- (car l)) (cdr l)) (at-most-helper (cdr l))))
  )
)


; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;
(defun generate-node-clauses (n k)
  (append (list (at-least-one-color n 1 k)) (at-most-one-color n 1 k)) ; The intersection of at least one color and at most one color
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
(defun generate-edge-clauses (e k)
  (let ((a (first e))
       (b (second e)))
    (generate-edge-helper a b 1 k))
)

(defun generate-edge-helper (a b c k)
  (cond ((or (< a 1) (< b 1) (< c 1) (< k 1)) nil)
        ((< k c) nil)
        (t (append (list (list (- (node2var a c k)) (- (node2var b c k)))) (generate-edge-helper a b (+ c 1) k)))
  )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
;
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn
  );end defun

;
; A utility function for parsing a pair of integers.
;
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun


; helper function to return all permutations of x with negation of
;	all elements in l
(defun permutations (x l)
	(cond
		((NULL l) NIL) ; empty list
		(t
			(append
				(list (list x (- (car l)))) ; list of pairs
				(permutations x (cdr l)) ;of negated literal in l w/
							; x
			)
		)
	)
)



; (print (permutations 1 '(2 3 4 5)))

;(print (node2var 1 1 5))
;(print (at-least-one-color 1 1 3))
;(print (at-most-one-color 1 1 3))

;(print (generate-node-clauses 1 3))
;(print (generate-edge-clauses '(1 3) 4))
