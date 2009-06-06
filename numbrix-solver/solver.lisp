#|

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

solver.lisp - Defines functionality used in solving Numbrix puzzles.

|#

(defun list-permutator (lis fun &optional permutation early-termination)
  "Iterates through all permutations of LIS, executing FUN on each permutation in turn.
   Useful in brute-force solving strategies."
  (cond 
    ((null lis) nil)
    ((null (rest lis)) 
     (funcall fun (append permutation lis)))
    (t (dolist (x lis)
	 (let* ((sublis (remove x lis))
		(base (list x))
		(result (list-permutator sublis fun (append permutation base) early-termination)))
	   (when (and early-termination result)
	     (return result)))))))

(defun board-apply-values (board values)
  "Applies values in VALUES to empty cells in BOARD."
  (let ((i 0)
	(test-board (make-board *size*)))
    (dotimes (y *size*)
      (dotimes (x *size*)
	(let ((val (get-pos board x y)))
	  (when (= val 0)
	    (progn
	      (setf val (elt values i))
	      (incf i)))
	  (set-pos test-board x y val))))
    test-board))

(defun solver-brute-force (board)
  "Utilizes a permutation method to solve BOARD."
  (let ((values (missing-values board))
	(result nil))
    (list-permutator 
     values 
     #'(lambda (x) 
	 (let* ((board-solution (board-apply-values board x))
		(valid (board-valid? board-solution)))
	   (when valid
	     (setf result board-solution))))
     nil
     t)
    result))

(defun sort-ascending-value-coord-tuple (tuple1 tuple2)
  "Predicate function used in sorting value-coord tuples."
  (let ((val1 (first tuple1))
	(val2 (first tuple2)))
    (< val1 val2)))

(defun endpoint-unknown-cost-estimate (val-diff)
  "Cost estimator for sequences that contain an endpoint with unknown coordinates."
  (let ((base (1+ val-diff)))
    (* (expt base base) 4)))

(defun endpoint-cost-comparison (e1 e2)
  "Predicate function used in sorting lists of unsolved endpoint pairs."
  (let* ((value-multiplier 2)
	 (e1-start (first e1))
	 (e1-end (second e1))
	 (e2-start (first e2))
	 (e2-end (second e2))
	 (e1-known? (endpoints-known? e1-start e1-end))
	 (e2-known? (endpoints-known? e2-start e2-end))
	 (e1-value-difference (- (endpoints-value-difference e1-start e1-end) 1))
	 (e2-value-difference (- (endpoints-value-difference e2-start e2-end) 1))
	 (e1-coord-difference (if e1-known? 
				  (endpoints-distance e1-start e1-end) 
				  (endpoint-unknown-cost-estimate e1-value-difference)))
	 (e2-coord-difference (if e2-known? 
				  (endpoints-distance e2-start e2-end) 
				  (endpoint-unknown-cost-estimate e2-value-difference)))
	 (e1-cost-estimate (if e1-known? (- (* e1-value-difference value-multiplier) e1-coord-difference) e1-coord-difference))
	 (e2-cost-estimate (if e2-known? (- (* e2-value-difference value-multiplier) e2-coord-difference) e2-coord-difference))
	 (e1-favored (< e1-cost-estimate e2-cost-estimate)))
    (debug-message "e1-start: ~a e1-end: ~a" e1-start e1-end)
    (debug-message "e2-start: ~a e2-end: ~a" e2-start e2-end)
    (debug-message "e1-val-diff ~a e2-val-diff: ~a" e1-value-difference e2-value-difference)
    (debug-message "e1-coord-diff ~a e2-coord-diff: ~a" e1-coord-difference e2-coord-difference)
    (debug-message "e1-cost-estimate ~a e2-cost-estimate: ~a" e1-cost-estimate e2-cost-estimate)
    (debug-message "e1-favored: ~a~%" e1-favored)    
    e1-favored))	 

(defun sort-endpoints (endpoints)
  "Sorter for unsolved endpoint pairs."
  (stable-sort endpoints #'endpoint-cost-comparison))

(defun incomplete-sequence-endpoints (board &optional (sorted t))
  "Scans BOARD for coordinates of values that form the endpoints of unsolved number sequences
   Returns a list of such endpoints that define a sequence with two endpoints.
   A special set of endpoints is returned for those broken sequences that define a sequence
   that contains either the missing min or max values."
  (let* ((values (stable-sort (remove-empty (flatten-board board t) t) #'sort-ascending-value-coord-tuple))
	 (values-length (length values))
	 (endpoints ())
	 (min 1)
	 (max *num-squares*)
	 (endpoints-min nil)
	 (endpoints-max nil))
    (let ((first-endpoint (first values))
	  (last-endpoint (first (reverse values))))
      (when (/= (first first-endpoint) min)
	(setf endpoints-min (list (list (list min nil nil) (car values)))))
      (when (/= (first last-endpoint) max)
	(setf endpoints-max (list (list (car (reverse values)) (list max nil nil))))))
    (dotimes (i values-length)
      (when (< i (1- values-length))
	(let ((start (elt values i))
	      (end (elt values (1+ i))))
	  (when (/= (- (first end) (first start)) 1)
	    (setf endpoints (append endpoints (list (list start end))))))))
    (setf endpoints (append endpoints-min endpoints endpoints-max))
    (if sorted
      (sort-endpoints endpoints)
      endpoints)))

(defun endpoints-distance (start end)
  "Returns the distance separating START and END coordinates."
  (let* ((x-start (second start))
	 (y-start (third start))
	 (x-end (second end))
	 (y-end (third end)))
    (- (+ (abs (- x-start x-end)) (abs (- y-start y-end))) 1)))

(defun endpoints-common-axis (start end)
  "Returns T if START and END either share the X or Y axis."
  (let* ((x-start (second start))
	 (y-start (third start))
	 (x-end (second end))
	 (y-end (third end)))
    (or (when (= x-start x-end) (list :x x-start))
	(when (= y-start y-end) (list :y y-start)))))

(defun endpoints-value-difference (start end)
  "Returns the difference in values between START and END endpoints."
  (let* ((value-start (first start))
	 (value-end (first end)))
    (- value-end value-start)))

(defun build-dynamic-value-list (distance start direction &optional coords)
  "Recursively build a list of integers of length DISTANCE starting at START
   and growing by DIRECTION with every iteration."
  (cond
    ((= distance 0) coords)
    ((null coords) 
     (build-dynamic-value-list
      (1- distance) start direction (list (+ start direction))))
    (t (build-dynamic-value-list
	(1- distance) start direction (append coords 
					      (list (+ (car (reverse coords)) direction)))))))

(defun build-static-value-list (distance coord &optional coords)
  "Recursively build a list of length DISTANCE with every element set to value
   COORD."
  (cond
    ((= distance 0) coords)
    (t (build-static-value-list (1- distance) coord (append coords (list coord))))))

(defun trivial-solution-multi-cell-sequence (start end common-axis distance)
  "Returns a list of size DISTANCE of value-coord tuples that bridge 
   START and END endpoints along COMMON-AXIS, a list of form (AXIS-KEYWORD AXIS-VALUE)."
  (let* ((x-coord-start (second start))
	 (y-coord-start (third start))
	 (x-coord-end (second end))
	 (y-coord-end (third end))
	 (value-start (first start))
	 (direction-positive 
	  (ecase (first common-axis)
	    (:x (plusp (- y-coord-end y-coord-start)))
	    (:y (plusp (- x-coord-end x-coord-start)))))
	 (coord-start 
	  (ecase (first common-axis)
	    (:x (if direction-positive 
		    (min y-coord-start y-coord-end) 
		    (max y-coord-start y-coord-end)))
	    (:y (if direction-positive 
		    (min x-coord-start x-coord-end) 
		    (max x-coord-start x-coord-end)))))
	 (solution-values 
	  (build-dynamic-value-list distance value-start 1))
	 (solution-dynamic-coords 
	  (build-dynamic-value-list distance coord-start (if direction-positive 1 -1)))
	 (solution-static-coords 
	  (build-static-value-list distance (second common-axis)))
	 (x nil)
	 (y nil))
    (ecase (first common-axis)
      (:x (progn
	    (setf x solution-static-coords)
	    (setf y solution-dynamic-coords)))
      (:y (progn
	    (setf x solution-dynamic-coords)
	    (setf y solution-static-coords))))
    (mapcar #'(lambda (value x y) (list value x y)) 
	    solution-values
	    x
	    y)))

(defun trivial-solution-multi-cell (*board*)
  "Identifies and solves those solutions in *BOARD* that are both trivial
   and occupy more than one cell."
  (let ((endpoints (incomplete-sequence-endpoints *board*))
	(solutions ()))
    (mapcar #'(lambda (x)
		(let ((start (first x))
		      (end (second x)))
		  (when (endpoints-known? start end)
		    (let ((distance (endpoints-distance start end))
			  (value-difference (endpoints-value-difference start end))
			  (common-axis (endpoints-common-axis start end)))
		      (when (and common-axis
				 (= distance (1- value-difference)))
			(setf solutions 
			      (append solutions 
				      (trivial-solution-multi-cell-sequence start end common-axis distance))))))))
	    endpoints)
    solutions))

(defun get-max-neighbors (position-type)
  "Returns the maximum number of neighbors a cell with type POSITION-TYPE can have."
  (getf *position-types-neighbors* position-type))

(defun neighbors-sequential? (neighbor1 neighbor2)
  "Returns T if the absolute value difference between NEIGHBOR1 and NEIGHBOR2 is equal to 2."
  (= 2 (abs (- neighbor1 neighbor2))))

(defun complete-sequence (neighbor1 neighbor2)
  "Given NEIGHBOR1 and NEIGHBOR2, values with absolute value difference of 2, return
   the value that will bridge these two values and form an unbroken integer sequence."
  (if (< neighbor1 neighbor2)
      (1+ neighbor1)
      (1+ neighbor2)))

(defun trivial-solution-linear (n1 n2)
  "Given the board positions N1 and N2, return the value that bridges the two if it exists."
  (when (and (value-set? n1) (value-set? n2) (neighbors-sequential? n1 n2))
    (complete-sequence n1 n2)))

(defun trivial-solution-angular (n1 n2 diag)
  "Given N1 and N2, two solved squares with corners that touch, and DIAG, a solved
   cell that borders N1 and N2, return the value of the unsolved cell that will bridge
   N1 and N2, if that value exists."
  (unless (value-empty? diag)
    (let ((solution (trivial-solution-linear n1 n2)))
      (unless (eq solution diag)
	solution))))

(defun trivial-solution-vert (board x y)
  "Return a solution for position X Y if position X Y could potentially form part of a 
   three-cell vertical sequence segment with its neighbors to the north and south."
  (let* ((n (get-pos board x (1- y)))
	 (s (get-pos board x (1+ y))))
    (trivial-solution-linear n s)))

(defun trivial-solution-horz (board x y)
  "Return a solution for position X Y if position X Y could potentially form part of a 
   three-cell horizontal sequence segment with its neighbors to the east and west."
  (let* ((e (get-pos board (1+ x) y))
	 (w (get-pos board (1- x) y)))
    (trivial-solution-linear e w)))

(defun trivial-solution-ne (board x y)
  "Return a solution for position X Y if position X Y could potentially form part of a 
   three-cell angular sequence segment with its neighbors to the north and east."
  (let* ((n (get-pos board x (1- y)))
	 (e (get-pos board (1+ x) y))
	 (ne (get-pos board (1+ x) (1- y))))
    (trivial-solution-angular n e ne)))

(defun trivial-solution-se (board x y)
  "Return a solution for position X Y if position X Y could potentially form part of a 
   three-cell angular sequence segment with its neighbors to the south and east."
  (let* ((s (get-pos board x (1+ y)))
	 (e (get-pos board (1+ x) y))
	 (se (get-pos board (1+ x) (1+ y))))
    (trivial-solution-angular s e se)))

(defun trivial-solution-sw (board x y)
  "Return a solution for position X Y if position X Y could potentially form part of a 
   three-cell angular sequence segment with its neighbors to the south and west."
  (let* ((s (get-pos board x (1+ y)))
	 (w (get-pos board (1- x) y))
	 (sw (get-pos board (1- x) (1+ y))))
    (trivial-solution-angular s w sw)))

(defun trivial-solution-nw (board x y)
  "Return a solution for position X Y if position X Y could potentially form part of a 
   three-cell angular sequence segment with its neighbors to the north and west."
  (let* ((n (get-pos board x (1- y)))
	 (w (get-pos board (1- x) y))
	 (nw (get-pos board (1- x) (1- y))))
    (trivial-solution-angular n w nw)))

(defun find-adjacency (board x y adjacency)
  "Returns the list (X Y) if the value of BOARD 
   at X Y is equal to the value of ADJACENCY."
  (let ((value (get-pos board x y)))
    (when (= value adjacency)
      (list x y))))

(defun find-adjacency-n (board x y adjacency)
  "Returns the coordinate pair (X Y) corresponding to the cell north of
   X Y if that cell has value equal to ADJACENCY."
  (let ((x-coord x)
	(y-coord (1- y)))
    (find-adjacency board x-coord y-coord adjacency)))

(defun find-adjacency-e (board x y adjacency)
  "Returns the coordinate pair (X Y) corresponding to the cell east of
   X Y if that cell has value equal to ADJACENCY."
  (let ((x-coord (1+ x))
	(y-coord y))
    (find-adjacency board x-coord y-coord adjacency)))

(defun find-adjacency-s (board x y adjacency)
  "Returns the coordinate pair (X Y) corresponding to the cell south of
   X Y if that cell has value equal to ADJACENCY."
  (let ((x-coord x)
	(y-coord (1+ y)))
    (find-adjacency board x-coord y-coord adjacency)))

(defun find-adjacency-w (board x y adjacency)
  "Returns the coordinate pair (X Y) corresponding to the cell west of
   X Y if that cell has value equal to ADJACENCY."
  (let ((x-coord (1- x))
	(y-coord y))
    (find-adjacency board x-coord y-coord adjacency)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-find-adjacency-exp (get-find-adjacency-fun)
    "Helper function used in the macro MAKE-FIND-ADJACENCY-EXPS."
    (let ((find-adjacency-fun (getf *find-adjacency-function-types* get-find-adjacency-fun)))
      (list find-adjacency-fun 'board 'x 'y 'adjacency))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-find-adjacency-exps (get-find-adjacency-funs)
    "Helper function used in the macro MAKE-FIND-ADJACENCY-EXPS."
    (mapcar #'(lambda (x) (get-find-adjacency-exp x)) get-find-adjacency-funs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro make-find-adjacency-exps (&rest funs)
    "Builds expressions for locating endpoint adjacencies."
    `(or ,@(get-find-adjacency-exps funs))))

(defun trivial-solution-endpoint-find-adjacency (board x y adjacency)
  "Finds coordinates of cell with value ADJACENCY if it 
   exists in the neighbors of the cell at X Y."
  (let ((position-type (square-position-type x y)))
    (ecase position-type
      (inner (make-find-adjacency-exps :n :e :s :w))
      (n (make-find-adjacency-exps :e :s :w))
      (ne (make-find-adjacency-exps :s :w))
      (e (make-find-adjacency-exps :n :s :w))
      (se (make-find-adjacency-exps :n :w))
      (s (make-find-adjacency-exps :n :e :w))
      (sw (make-find-adjacency-exps :n :e))
      (w (make-find-adjacency-exps :n :e :s))
      (nw (make-find-adjacency-exps :e :s)))))

(defun trivial-solution-endpoint (board x y adjacency target direction)
  "Searches neighbors for an endpoint adjacency. If adjacency found, its neighbors are tested
   to determine if the vicinity of position X Y forces an endpoint solution at that location."
  (unless (find target (flatten-board *board*))
    (let ((adjacency-coords (trivial-solution-endpoint-find-adjacency board x y adjacency))
	  (neighbors (get-neighbors *board* x y)))
      (when (and adjacency-coords
		 (find adjacency neighbors)) 
	(let* ((next-adjacency (+ adjacency direction))
	       (x-coord (first adjacency-coords))
	       (y-coord (second adjacency-coords))
	       (adjacency-neighbors (remove-if #'value-empty? (get-neighbors board x-coord y-coord)))
	       (adjacency-max-neighbors (get-max-neighbors (square-position-type x-coord y-coord t))))
	  (when (and (= (length adjacency-neighbors) (1- adjacency-max-neighbors))
		     (find next-adjacency adjacency-neighbors))
	    target))))))

(defun trivial-solution-min (board x y)
  "Trivial solution solver for the minimum value in BOARD."
  (let* ((min-value 1)
	 (min-value-adjacent (1+ min-value))
	 (sequence-direction 1))
    (trivial-solution-endpoint board x y min-value-adjacent min-value sequence-direction)))

(defun trivial-solution-max (board x y)
  "Trivial solution solver for the maximum value in BOARD."
  (let* ((max-value *num-squares*)       
	 (max-value-adjacent (1- max-value))
	 (sequence-direction -1))
    (trivial-solution-endpoint board x y max-value-adjacent max-value sequence-direction)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-trivial-solution-exp (get-trivial-solution-fun)
    "Helper function used in the macro MAKE-TRIVIAL-SOLUTION-EXPS."
    (let ((trivial-solution-fun (getf *trivial-solution-function-types* get-trivial-solution-fun)))
      (list trivial-solution-fun 'board 'x 'y))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-trivial-solution-exps (get-trivial-solution-funs)
    "Helper function used in the macro MAKE-TRIVIAL-SOLUTION-EXPS."
    (mapcar #'(lambda (x) (get-trivial-solution-exp x)) get-trivial-solution-funs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro make-trivial-solution-exps (&rest funs)
    "Builds expressions for solving trivial solutions."
    `(or ,@(get-trivial-solution-exps funs))))

(defun trivial-solution (board x y)
  "For one or two neighbors of empty square X Y in BOARD, see if such a value of square X Y exists
   that when set, will constitue a solution with no possibility of error or need for backtracking.
   For the case of one neighbor, this indicates a sequence maximum or minimum (1 or *size* x *size*).
   For the case of two neighbors, this indicates a non-endpoint portion of the sequence.

   Process: 
   1) Determine board position type. Following steps will depend on board position type.
   2) For cases of two or more neighbors, locate constraint that will
      force the completion of a non-endpoint sequence portion.
   3) If such a constraint is not found, try to locate a constraint that forces completion of an endpoint.
   4) If such a solution is found, return it. Otherwise, return NIL."
  (let ((value (get-pos board x y)))
    (when (value-empty? value)
      (let ((position-type (square-position-type x y)))
	(ecase position-type
	  (inner (make-trivial-solution-exps :vert :horz :ne :se :sw :nw :min :max))
	  (n (make-trivial-solution-exps :horz :se :sw :min :max))
	  (ne (make-trivial-solution-exps :sw :min :max))
	  (e (make-trivial-solution-exps :vert :sw :nw :min :max))
	  (se (make-trivial-solution-exps :nw :min :max))
	  (s (make-trivial-solution-exps :horz :ne :nw :min :max))
	  (sw (make-trivial-solution-exps :ne :min :max))
	  (w (make-trivial-solution-exps :vert :ne :se :min :max))
	  (nw (make-trivial-solution-exps :se :min :max)))))))

(defun apply-solutions (board solutions)
  "Apply the value-coord tuples in list SOLUTIONS to BOARD.
   Destructively edits BOARD."
  (mapcar #'(lambda (solution) 
	      (set-pos board 
		       (second solution) 
		       (third solution) 
		       (first solution)))
	  solutions))

(defun solver-move-legal? (board x y &optional (get-pos-fun #'get-pos))
  "Returns the list (X Y) if applying a move at X Y is legal."
  (let ((min 0)
	(max (1- *size*)))
    (unless (or (move-coord-invalid? x y min max)
		(value-set? (funcall get-pos-fun board x y)))
      (list x y))))

(defstruct failed-attempts
  "Tracks attempts made during nontrivial solution exploration.
   An attempt is marked T if has been shown to have failed."
  (n)
  (e)
  (s)
  (w))

(defun failed-attempt-made? (failed-attempts direction)
  "Returns the value of FAILED-ATTEMPTS at DIRECTION."
  (ecase direction
    (:n (failed-attempts-n failed-attempts))
    (:e (failed-attempts-e failed-attempts))
    (:s (failed-attempts-s failed-attempts))
    (:w (failed-attempts-w failed-attempts))))

(defun failed-attempt-mark (failed-attempts direction)
 "Sets the value of FAILED-ATTEMPTS at DIRECTION to indicate that that attempt has failed."
  (ecase direction
    (:n (setf (failed-attempts-n failed-attempts) t))
    (:e (setf (failed-attempts-e failed-attempts) t))
    (:s (setf (failed-attempts-s failed-attempts) t))
    (:w (setf (failed-attempts-w failed-attempts) t))))

(defun failed-attempt-mark-next (failed-attempts)
  "Given a FAILED-ATTEMPTS structure, destructively modify said structure by 
   marking the next available unmarked failed attempt and returning the direction
   corresponding to the attempt. Return NIL if structure has been completely marked."
  (let ((directions '(:n :e :s :w)))
    (mapcar #'(lambda (direction)
		(when (not (failed-attempt-made? failed-attempts direction))
		  (failed-attempt-mark failed-attempts direction)
		  (return-from failed-attempt-mark-next direction)))
	    directions)
    nil))

(defun failed-attempt-move-exists (failed-attempts)
  "Examines FAILED-ATTEMPTS and returns T if FAILED-ATTEMPTS has not been
   completely marked as failed."
  (let ((directions '(:n :e :s :w)))
    (some #'null
	  (mapcar  #'(lambda (direction)
		       (failed-attempt-made? failed-attempts direction))
		   directions))))

(defun attempt-tracker-move-exists (source attempt-tracker)
  "Examines the FAILED-ATTEMPTS at coordinates given by SOURCE in ATTEMPT-TRACKER.
   Returns T if that FAILED-ATTEMPTS has not been completely set to T."
  (let ((failed-attempts (get-pos attempt-tracker (second source) (third source))))
    (failed-attempt-move-exists failed-attempts)))

(defun nontrivial-solution-move-misguided? (value-move value-goal distance-to-goal)
  "Return T if a move's value and distance from goal place it far enough from goal that
   pursuing that move would make a solution impossible. Helps reduce the nontrivial solution search
   space."
  (>= distance-to-goal (- value-goal value-move)))

(defun nontrivial-solution-move-legal? (board failed-attempts value direction end value-goal x y)
  "Given board state, move attempts, move direction and current coordinates, 
   return the next move in (X Y) form.
   If no such move exists at X Y, then return NIL and mark said move invalid in FAILED-ATTEMPTS.
   NOTE: FAILED-ATTEMPTS is destructively edited as a side-effect."
  (debug-message "MOVE-LEGAL?~%failed-attempts: ~a move: (~a ~a ~a) direction: ~a~%end: ~a~%board-state: ~a"
		 failed-attempts value x y direction end board)
  (if (and (move-known? end)
	   (not (nontrivial-solution-move-misguided? value value-goal (endpoints-distance (list value x y) end)))
	   (solver-move-legal? board x y)
	   (value-empty? (get-pos board x y))
	   (not (failed-attempt-made? failed-attempts direction)))
      (list value x y)
      (progn
	(failed-attempt-mark failed-attempts direction)
	nil)))

(defun direction-neighbor (source neighbor)
  "Given SOURCE and NEIGHBOR moves of form (VALUE X Y), determine the direction of NEIGHBOR
   relative to SOURCE."
  (let* ((x-source (second source))
	 (y-source (third source))
	 (x-neighbor (second neighbor))
	 (y-neighbor (third neighbor))
	 (offset (list (- x-neighbor x-source) (- y-neighbor y-source)))
	 (n (list 0 -1))
	 (e (list 1 0))
	 (s (list 0 1))
	 (w (list -1 0)))
    (cond ((equal offset n) :n)
	  ((equal offset e) :e)
	  ((equal offset s) :s)
	  ((equal offset w) :w))))

(defun nontrivial-solution-next-move (board attempt-tracker value end value-goal x y)
  "Given board state, failed move attempts, and a coordinate, calculate next move.
   Returns next move in (X Y) form. Returns NIL if no such move exists.
   Destructively edits ATTEMPT-TRACKER as a side-effect."
  (let* ((north (list x (1- y))) (x-north (first north)) (y-north (second north))
	 (east (list (1+ x) y)) (x-east (first east)) (y-east (second east))
	 (south (list x (1+ y))) (x-south (first south)) (y-south (second south))
	 (west (list (1- x) y)) (x-west (first west)) (y-west (second west))
	 (failed-attempts (get-pos attempt-tracker x y)))
    (debug-message "NEXT-MOVE: ~a from (~a ~a) in:~%~a" value x y attempt-tracker)
    (or (nontrivial-solution-move-legal? board failed-attempts value :n end value-goal x-north y-north)
	(nontrivial-solution-move-legal? board failed-attempts value :e end value-goal x-east y-east)
	(nontrivial-solution-move-legal? board failed-attempts value :s end value-goal x-south y-south)
	(nontrivial-solution-move-legal? board failed-attempts value :w end value-goal x-west y-west))))

(defun nontrivial-solution-prepare-attempt-tracker (board)
  "Build a failed attempt tracker to match configuration of BOARD."
  (let ((attempt-tracker 
	 (copy-board 
	  *size* board nil
	  (type-of (make-failed-attempts)) nil)))
    (dotimes (i *size*)
      (dotimes (j *size*)
	(set-pos attempt-tracker i j (make-failed-attempts))))
    attempt-tracker)) 

(defun attempt-tracker-mark-failed (source neighbor attempt-tracker)
  "Marks FAILED-ATTEMPTS direction failed at position of neighbor relative to source."
  (let ((failed-attempts (get-pos attempt-tracker (second source) (third source))))
    (failed-attempt-mark failed-attempts (direction-neighbor source neighbor))))  

(defun attempt-tracker-mark-all-failed (square attempt-tracker)
  "Marks the FAILED-ATTEMPTS at SQUARE in ATTEMPT-TRACKER as failed for all DIRECTIONS."
  (let ((failed-attempts (get-pos attempt-tracker (second square) (third square)))
	(directions '(:n :e :s :w)))
    (mapcar #'(lambda (direction)
		(failed-attempt-mark failed-attempts direction))
	    directions)))

(defun move-unknown? (move)
  "Returns T if a move is of form (X Y Z) where Y and Z are both set to nil."
  (and (null (second move)) (null (third move))))

(defun move-known? (move)
  "Returns T if a move is of form (X Y Z) where Y and Z are both not nil."
  (not (move-unknown? move)))

(defun endpoints-known? (start end)
  "Returns T if both START and END are of form (X Y Z) where Y and Z are both not nil."
  (every #'move-known? (list start end)))

(defstruct endpoint-attempt
  "Defines the properties of a cell that indicate if that cell could POTENTIAL
   be considered for the location of an unsolved endpoint. If that location is shown
   to be impossible to satisfy, then the position is marked INVALID."
  (potential)
  (invalid))

(defun endpoint-tracker-next-available (endpoint-tracker)
  "Returns next available position in ENDPOINT-TRACKER in form (X Y).
   Returns NIL if no position exists."
  (dotimes (i *size*)
    (dotimes (j *size*)
      (let* ((endpoint-attempt (get-pos endpoint-tracker i j)))
	(when (and (endpoint-attempt-potential endpoint-attempt)
		   (not (endpoint-attempt-invalid endpoint-attempt)))
	  (return-from endpoint-tracker-next-available (list i j)))))))

(defun endpoint-tracker-invalidate-position (endpoint-tracker)
  "Invalidates next available un-invalidated position in ENDPOINT-TRACKER and returns
   that position in form (X Y). Returns NIL if no such position exists."
  (dotimes (i *size*)
    (dotimes (j *size*)
      (let* ((endpoint-attempt (get-pos endpoint-tracker i j)))
	(when (and (endpoint-attempt-potential endpoint-attempt)
		   (not (endpoint-attempt-invalid endpoint-attempt)))
	  (setf (endpoint-attempt-invalid endpoint-attempt) t)
	  (return-from endpoint-tracker-invalidate-position (list i j)))))))

(defun endpoint-tracker-positions-unsolved? (endpoint-tracker)
  "Returns T if any unsolved potential positions are found in ENDPOINT-TRACKER.
   Returns NIL otherwise."
  (dotimes (i *size*)
    (dotimes (j *size*)
      (let* ((endpoint-attempt (get-pos endpoint-tracker i j)))
	(when (and (endpoint-attempt-potential endpoint-attempt)
		   (not (endpoint-attempt-invalid endpoint-attempt)))
	  (when (null (endpoint-attempt-invalid endpoint-attempt))
	    (return-from endpoint-tracker-positions-unsolved? t)))))))

(defun nontrivial-solution-search-endpoints-endpoint-tracker (board unknown known)
  "Accepts BOARD, a value UNKNOWN with no known coordinates, and KNOWN, a value with known coordinates.
   Returns ENDPOINT-TRACKER, a grid that contains possible coordinates for the unknown value
   as well as handy flags that indicate when and if such coordinates are marked as invalid."
  (let ((max-distance (1- (abs (endpoints-value-difference unknown known))))
	(endpoint-tracker
	 (copy-board 
	  *size* board nil
	  (type-of (make-endpoint-attempt)) nil)))
    (dotimes (i *size*)
      (dotimes (j *size*)
	(let ((endpoint-attempt (make-endpoint-attempt))
	      (distance (endpoints-distance known (list nil i j)))
	      (value (get-pos board i j)))
	  (when (and (<= distance max-distance)
		     (value-empty? value))
	    (setf (endpoint-attempt-potential endpoint-attempt) t))
	  (set-pos endpoint-tracker i j endpoint-attempt))))
    endpoint-tracker)) 

(defun endpoint-tracker-update-unknown (endpoint-tracker unknown)
  "Updates the value-coord UKNOWN tuple with the X Y location of the next available
   potential endpoint position in ENDPOINT-TRACKER."
  (let* ((position-next-available (endpoint-tracker-next-available endpoint-tracker))
	 (position-next-available-x (first position-next-available))
	 (position-next-available-y (second position-next-available)))
    (setf (elt unknown 1) position-next-available-x)
    (setf (elt unknown 2) position-next-available-y)
    unknown))

(defun endpoints-clear-invalid (board-state unknown endpoint-tracker)
  "Given BOARD-STATE and ENDPOINT-TRACKER clear all positions in BOARD-STATE
   with value UKNOWN that have been shown to be invalid in ENDPOINT-TRACKER."
  (let ((value (first unknown)))
    (dotimes (i *size*)
      (dotimes (j *size*)
	(let ((endpoint-attempt (get-pos endpoint-tracker i j)))
	  (when (and (or (not (endpoint-attempt-potential endpoint-attempt))
			 (endpoint-attempt-invalid endpoint-attempt))
		     (= value (get-pos board-state i j)))
	    (set-pos board-state i j 0)))))))

(defun nontrivial-solution-search-endpoints-prepare (board start end &optional endpoint-tracker)
  "Given BOARD, and START and END endpoints, prepare a collection of values that will
   be used during a nontrivial solution search for values and coordinates that will bridge
   START and END."
  (let* ((unknown (copy-list (if (move-unknown? start) start end)))
	 (known (copy-list (if (move-unknown? end) start end)))
	 (unknown-identity (if (equal unknown start) :start :end))
	 (order-ascending (plusp (endpoints-value-difference unknown known)))
	 (endpoint-tracker (if (null endpoint-tracker)
			       (nontrivial-solution-search-endpoints-endpoint-tracker board unknown known)
			       endpoint-tracker))
	 (resolved (endpoint-tracker-update-unknown endpoint-tracker unknown))
	 (origin (if order-ascending resolved known))
	 (goal (if order-ascending known resolved))
	 (board-state (copy-board *size* board))
	 (solution-impossible nil))
    (debug-message "ENDPOINTS-PREPARE:~%board: ~a~%start: ~a~%end: ~a~%endpoint-tracker: ~a~%resolved: ~a~%"
		   board start end endpoint-tracker resolved)
    (if (move-known? resolved)
	(apply-solutions board-state (list resolved))
	(setf solution-impossible t))
    (endpoints-clear-invalid board-state resolved endpoint-tracker)
    (values
     origin
     goal
     endpoint-tracker
     board-state
     unknown-identity
     solution-impossible)))

(defun nontrivial-solution-search-endpoints (board start end &optional move-tracker board-state attempt-tracker endpoint-tracker endpoint-recalculated)
  "Assists in solving cases where only one endpoint in sequence is known. 
   Returns MOVE-TRACKER, BOARD-STATE, ATTEMPT-TRACKER and ENDPOINT-TRACKER.
   If no solution is found, this is indicated by returning MOVE-TRACKER with the value NIL."
  (debug-message "SEARCH-ENDPOINTS: start: ~a end: ~a~%move-tracker: ~a~%attempt-tracker: ~a~%endpoint-recalculated: ~a ~%"
		 start end move-tracker attempt-tracker endpoint-recalculated)
  (let ((unknown-identity nil)
	(origin nil)
	(goal nil)
	(value-difference nil)
	(short-solution-found nil)
	(solution-impossible nil))
    (loop do
	 (multiple-value-bind (origin-result 
			       goal-result 
			       endpoint-tracker-result 
			       board-state-result 
			       unknown-identity-result
			       solution-impossible-result)
	     (nontrivial-solution-search-endpoints-prepare 
	      (if endpoint-recalculated
		  board-state
		  board) 
	      start end endpoint-tracker)
	   (when (and origin 
		      goal 
		      (or (not (equal origin origin-result))
			  (not (equal goal goal-result))))
	     (setf endpoint-recalculated t))
	   (setf origin origin-result)
	   (setf goal goal-result)
	   (setf endpoint-tracker endpoint-tracker-result)
	   (setf board-state board-state-result)
	   (setf unknown-identity unknown-identity-result)
	   (setf value-difference (1- (endpoints-value-difference origin goal)))
	   (setf solution-impossible solution-impossible-result)
	   (debug-message "~%SEARCH-ENDPOINTS: ~a ~a ~a ~a ~a ~a~%~a~%~a~%~%" 
			  start end origin goal value-difference 
			  short-solution-found endpoint-tracker-result board-state)
	   (debug-message "solution-impossible: ~a" solution-impossible)
	   (when (not solution-impossible)
	     (multiple-value-bind (move-tracker-result board-state-result attempt-tracker-result short-solution-found-result)
		 (nontrivial-solution-search board-state origin goal 
					     (unless endpoint-recalculated move-tracker)
					     nil
					     (unless endpoint-recalculated attempt-tracker))
	       (setf move-tracker move-tracker-result)
	       (setf board-state board-state-result)
	       (setf attempt-tracker attempt-tracker-result)
	       (setf short-solution-found short-solution-found-result)
	       (unless (or move-tracker short-solution-found)
		 (progn
		   (setf endpoint-recalculated (not (null (endpoint-tracker-invalidate-position endpoint-tracker))))
		   (debug-message "board-state:~%~a~%" board-state)
		   (debug-message "ENDPOINT-RECALCULATED: ~a~%~a~%" endpoint-tracker endpoint-recalculated))))))
	 (error-if-duplicate-values board-state "NONTRIVIAL-SOLUTION-SEARCH-ENDPOINTS")
       while
	 (and (null move-tracker)
	      (endpoint-tracker-positions-unsolved? endpoint-tracker)
	      (not short-solution-found)
	      (not solution-impossible)))
    (if (and (or move-tracker short-solution-found) (not solution-impossible))
	(if (eq unknown-identity :start)
	    (setf move-tracker (append move-tracker (list origin)))
	    (setf move-tracker (append (list goal) move-tracker)))
	(progn
	  (setf board-state board)))
    (when solution-impossible
      (let ((fully-invalidated (if (move-known? origin) origin end))
	    (attempt-tracker 
	     (if attempt-tracker 
		 attempt-tracker 
		 (nontrivial-solution-prepare-attempt-tracker board-state))))
	(debug-message "origin: ~a~%end: ~a~%fully-invalidated: ~a~%attempt-tracker: ~a" 
		       origin end fully-invalidated attempt-tracker)
	(attempt-tracker-mark-all-failed fully-invalidated attempt-tracker)      
	(debug-message "SOLUTION-IMPOSSIBLE: move-tracker ~a~%board-state ~a~% attempt-tracker ~a~% endpoint-tracker"
		       move-tracker board-state attempt-tracker endpoint-tracker)))
	;(error "SOLUTION-IMPOSSIBLE")))
    (values
     move-tracker
     board-state
     attempt-tracker
     endpoint-tracker)))

(defun error-if-duplicate-values (board-state location)
  "Debugging function that halts execution if BOARD-STATE is shown to 
   contain two or more cells with value equal to value at LOCATION."
  (when *debug-mode*
    (when (duplicate-values? board-state)
      (print-board-pretty board-state)
      (error (format nil "ERROR: ~a Duplicate values exist!" location)))))

(defun nontrivial-solution-search (board start end &optional move-tracker board-state attempt-tracker)
  "Given BOARD state, and START and END endpoints, search BOARD for a solution.
   Returns 1) a list of moves that comprise a solution (NIL if no solution found),
   2) the board state resulting from application of these moves,
   and 3) a representation of those moves that were attempted but failed to be valid.
   Will optionally accept data representing a solution-in-progress when it is necessary to
   re-examine said solution to come up with an alternative."
  (debug-message "SOLUTION-SEARCH: start: ~a end: ~a" start end)
  (let* ((move-tracker (if (null move-tracker) () move-tracker))
	 (board-state (if (null board-state) (copy-board *size* board) board-state))
	 (attempt-tracker (if (null attempt-tracker) (nontrivial-solution-prepare-attempt-tracker board) attempt-tracker))
	 (current-move (if (null move-tracker) start (first move-tracker)))
	 (next-move ())
	 (value-difference (1- (endpoints-value-difference start end)))
	 (value-start (first start))
	 (value-goal (first end))
	 (expected-values (build-dynamic-value-list value-difference value-start 1))
	 (expected-value ())
	 (short-solution-found nil))
    (loop do
	 (debug-message "expected-values: ~a move-tracker: ~a value-difference: ~a" 
			expected-values move-tracker value-difference)
	 (if (> value-difference 0)
	     (progn
	       (setf expected-value (elt expected-values (length move-tracker)))
	       (setf next-move
		     (nontrivial-solution-next-move board-state attempt-tracker
						    expected-value
						    end
						    value-goal
						    (second current-move) 
						    (third current-move))))
	     (progn
	       (debug-message "SOLUTION-SEARCH: SHORT-SOLUTION")
	       (setf short-solution-found t)))
	 (debug-message "~a: current: ~a next: ~a move-tracker: ~a" 
			expected-value current-move next-move move-tracker)
	 (cond (next-move
		;; A valid next move has been selected:
		;; 1. Push next move onto MOVE-TRACKER stack
		;; 2. Update CURRENT-MOVE
		;; 3. Update BOARD-STATE
		(progn
		  (setf move-tracker (push next-move move-tracker))
		  (setf current-move next-move)
		  (set-pos board-state (second current-move) (third current-move) (first current-move) t)))
	       (move-tracker
		;; No valid move was found: Backtrack!
		;; 1. Remove most recent move from MOVE-TRACKER
		;; 2. Reset ATTEMPT-TRACKER and BOARD-STATE positions corresponding to invalid move
		;; 3. Update CURRENT-MOVE
		;; 4. Update ATTEMPT-TRACKER
		;; NOTE: Note special cases for a search that has backtracked to START
		(let* ((invalid-move (pop move-tracker)))
		  (set-pos attempt-tracker (second invalid-move) (third invalid-move) (make-failed-attempts))		    
		  (set-pos board-state (second invalid-move) (third invalid-move) 0)
		  (if move-tracker
		      ;; Search has not yet backtracked back to START position
		      (progn
			(setf current-move (first move-tracker))
			(attempt-tracker-mark-failed current-move invalid-move attempt-tracker))
		      ;; Handles case when search has backtracked back to START
		      (progn
			(setf current-move start)
			(attempt-tracker-mark-failed start invalid-move attempt-tracker))))))
	 (debug-message "~a: current: ~a next: ~a~%move-tracker: ~a" 
			expected-value current-move next-move move-tracker)
	 (debug-message "move exists: ~a~%" (attempt-tracker-move-exists start attempt-tracker))
	 (debug-message "board-state:~%~a~%" board-state)
	 (error-if-duplicate-values board-state "NONTRIVIAL-SOLUTION-SEARCH")
       while
       ;; Search until:
       ;; All solutions are found or no solutions can be found
	 (and (< (length move-tracker) (length expected-values)) 
	      (attempt-tracker-move-exists start attempt-tracker)
	      (not short-solution-found)))
    (debug-message "board-state:~%~a~%" board-state)
    (values
     move-tracker
     board-state
     attempt-tracker
     short-solution-found)))

(defun nontrivial-solution-calculate-alternate-solution (move-tracker board-state attempt-tracker endpoint-tracker start end)
  "Given the result of an attempted nontrivial solution search, modify state resulting from such a search
   to force a subsequent search using such state to attempt to find an alternate solution.
   The modifications are threefold:
   1. The most recent move in MOVE-TRACKER is removed
   2. The most recent move in BOARD-STATE is cleared
   3. The ATTEMPT-TRACKER position adjacent to the cleared state in steps 1. and 2. is 
      modified to reflect that the cleared state was an invalid move.
   Return values are similar to those of NONTRIVIAL-SOLUTION-SEARCH, including notification
   that no alternate solution was available."
  (let ((alternate-solution-found nil)
	(altered-move nil)
	(failed-attempts nil)
	(invalid-move nil)
	(valid-moves nil)
	(endpoint-recalculated nil))
    (debug-message "CALCULATE-ALTERNATE:~%move-tracker: ~a~%board-state: ~a~%start: ~a~%end: ~a~%endpoint-tracker: ~a" 
		   move-tracker board-state start end endpoint-tracker)
    (cond 
      ((move-unknown? end)
	 (cond 
	   ((= 2 (length move-tracker))
	    (progn
	      (setf move-tracker (rest move-tracker))
	      (setf invalid-move (first move-tracker))
	      (debug-message "CALCULATE-ALTERNATE: Recalculating End Point")
	      (set-pos board-state (second invalid-move) (third invalid-move) 0)
	      (setf alternate-solution-found (endpoint-tracker-invalidate-position endpoint-tracker))
					;(setf endpoint-recalculated (when alternate-solution-found endpoint-recalculated))
	      (setf valid-moves (endpoint-tracker-next-available endpoint-tracker))))
	   ((= 1 (length move-tracker))
	    (progn
	      (setf invalid-move (first move-tracker))
	      (debug-message "CALCULATE-ALTERNATE: Recalculating End Point")
	      (set-pos board-state (second invalid-move) (third invalid-move) 0)
	      (setf alternate-solution-found (endpoint-tracker-invalidate-position endpoint-tracker))
					;(setf endpoint-recalculated (when alternate-solution-found endpoint-recalculated))
	      (setf valid-moves (endpoint-tracker-next-available endpoint-tracker))))
	   (t
	    (progn
	      (setf move-tracker (rest move-tracker))
	      (setf invalid-move (first move-tracker))
	      (setf valid-moves (rest move-tracker))
	      (setf altered-move (first valid-moves))
	      (set-pos board-state (second invalid-move) (third invalid-move) 0)
	      (debug-message "CALCULATE-ALTERNATE: End Point~%tracker: ~a~%board-state:~a~%valid-moves: ~a~%"
			     move-tracker board-state valid-moves)
	      (setf failed-attempts (get-pos attempt-tracker (second altered-move) (third altered-move)))
	      (setf alternate-solution-found (failed-attempt-mark-next failed-attempts))
	      (debug-message "CALCULATE-ALTERNATE: End Point~%tracker: ~a~%board-state:~a~%found: ~a~%valid-moves: ~a~%failed-attempts: ~a"
			     move-tracker board-state alternate-solution-found valid-moves failed-attempts)))))
      ((move-unknown? start)
       (cond
	 ((= 2 (length move-tracker))
	  (progn
	    (setf invalid-move (first move-tracker))
	    (setf move-tracker (rest move-tracker))
	    (setf valid-moves move-tracker)
	    (setf altered-move (first valid-moves))
	    (setf failed-attempts (get-pos attempt-tracker (second altered-move) (third altered-move)))
	    (setf alternate-solution-found (failed-attempt-mark-next failed-attempts))
	    (set-pos board-state (second invalid-move) (third invalid-move) 0)
	    (debug-message 
	     "CALCULATE-ALTERNATE:~%tracker: ~a~%board-state:~a~%found: ~a~%valid-moves: ~a~%failed-attempts: ~a"
	     move-tracker board-state alternate-solution-found valid-moves failed-attempts)))
	 ((= 1 (length move-tracker))
	  (let ((invalid-move (first move-tracker)))
	    (debug-message "CALCULATE-ALTERNATE: Recalculating Start Point")
	    (set-pos board-state (second invalid-move) (third invalid-move) 0)
	    (setf alternate-solution-found (endpoint-tracker-invalidate-position endpoint-tracker))
	    (setf valid-moves (endpoint-tracker-next-available endpoint-tracker))))
	 (t
	  (progn
	    (setf move-tracker (butlast move-tracker))
	    (setf invalid-move (first move-tracker))
	    (setf valid-moves (rest move-tracker))
	    (setf altered-move (first valid-moves))
	    (setf failed-attempts (get-pos attempt-tracker (second altered-move) (third altered-move)))
	    (set-pos board-state (second invalid-move) (third invalid-move) 0)
	    (setf alternate-solution-found (failed-attempt-mark-next failed-attempts))
	    (debug-message 
	     "CALCULATE-ALTERNATE:~%tracker: ~a~%board-state:~a~%found: ~a~%valid-moves: ~a~%failed-attempts: ~a"
	     move-tracker board-state alternate-solution-found valid-moves failed-attempts)))))
      (t
       (let* ((move-tracker-copy 
	       (copy-list (if (= 1 (length move-tracker)) (append move-tracker (list start)) move-tracker))))
	 (setf invalid-move (first move-tracker-copy))
	 (setf valid-moves (rest move-tracker-copy))
	 (setf altered-move (first valid-moves))
	 (setf failed-attempts (get-pos attempt-tracker (second altered-move) (third altered-move)))
	 (set-pos board-state (second invalid-move) (third invalid-move) 0)
	 (setf alternate-solution-found (failed-attempt-mark-next failed-attempts))
	 (debug-message 
	  "CALCULATE-ALTERNATE:~%tracker: ~a~%tracker-copy: ~a~%board-state:~a~%found: ~a~%valid-moves: ~a~%failed-attempts: ~a"
	  move-tracker move-tracker-copy board-state alternate-solution-found valid-moves failed-attempts))))
    (debug-message "CALCULATE-ALTERNATE:~%attempt-tracker: ~a~%" attempt-tracker)
    (values
     (when (/= 1 (length move-tracker))
       valid-moves)
     board-state
     attempt-tracker
     endpoint-tracker
     alternate-solution-found
     endpoint-recalculated)))

(defun nontrivial-solution-alternate-solution (board start end move-tracker board-state attempt-tracker endpoint-tracker)
  "Computes alternate solution given input parameters."
  (multiple-value-bind (move-tracker board-state attempt-tracker 
				     endpoint-tracker alternate-solution-found endpoint-recalculated)
      (nontrivial-solution-calculate-alternate-solution move-tracker board-state attempt-tracker endpoint-tracker start end)
    (debug-message "ALTERNATE-SOLUTION: found: ~a~%move-tracker: ~a~%attempt-tracker: ~a~%" 
		   alternate-solution-found move-tracker attempt-tracker)
    (when alternate-solution-found
      (multiple-value-bind (move-tracker board-state attempt-tracker endpoint-tracker)
	  (if (endpoints-known? start end)
	      (nontrivial-solution-search board start end move-tracker board-state attempt-tracker)
	      (nontrivial-solution-search-endpoints board start end 
						    move-tracker board-state 
						    attempt-tracker endpoint-tracker endpoint-recalculated))
	(values
	 move-tracker
	 board-state
	 attempt-tracker
	 endpoint-tracker)))))

(defun solve-trivial (board)
  "Given a BOARD, returns a list of moves that, when applied,
   solves any trivial solutions in BOARD."
  (let ((board-state (copy-board *size* board))
	(solution-found nil)
	(solutions ())
	(solved-positions (remove-empty (flatten-board board))))
    (loop do
	 (setf solution-found nil)
	 (dotimes (x *size*)
	   (dotimes (y *size*)
	     (let ((solution (trivial-solution board-state x y)))
	       (when (and solution
			  (not (find solution solved-positions)))
		 (debug-message "SOLVE-TRIVIAL: solution ~a at (~a ~a)" solution x y)
		 (set-pos board-state x y solution t)
		 (setf solutions (push (list solution x y) solutions))
		 (setf solution-found t)))))
	 (let ((solutions-multi-cell (trivial-solution-multi-cell board-state)))
	   (when solutions-multi-cell
	     (progn
	       (apply-solutions board-state solutions-multi-cell)
	       (setf solutions (append solutions-multi-cell solutions))
	       (setf solution-found t))))
	 (error-if-duplicate-values board-state "SOLVE-TRIVIAL")
       while 
       solution-found)
    (values
     solutions
     board-state)))

(defstruct nontrivial-solution
  "Holds state useful in tracking progres of a nontrivial solution search.
   Moves are collected in MOVE-TRACKER and MOVE-TRIVIAL-TRACKER."
  (move-tracker)
  (move-trivial-tracker)
  (board-state)
  (attempt-tracker)
  (endpoint-start)
  (endpoint-end)
  (endpoint-tracker))

(defun nontrivial-solution-search-delegator (board start end &optional move-tracker board-state attempt-tracker endpoint-tracker)
  "Accepts nontrivial solution search state information and triggers the appropriate search mechanism:
   NONTRIVIAL-SOLUTION-SEARCH when both endpoints START and END have fixed coordinates.
   NONTRIVIAL-SOLUTION-SEARCH-ENDPOINTS when one of START and END has unknown coordinates."
  (debug-message "SEARCH-DELEGATOR START: ~a END: ~a" start end)
  (if (endpoints-known? start end)
      (nontrivial-solution-search board start end move-tracker board-state attempt-tracker)
      (nontrivial-solution-search-endpoints board start end move-tracker board-state attempt-tracker endpoint-tracker)))

(defun solver-nontrivial (board)
  "Numbrix game board solver. 
   Can solve any kind of valid game board."
  (let ((solution-tracker nil)
	(invalid-solution nil)
	(endpoints nil)
	(endpoint-start nil)
	(endpoint-end nil)
	(move-tracker nil)
	(board-state (copy-board *size* board))
	(attempt-tracker nil)
	(endpoint-tracker nil)
	(solutions-trivial nil))
    (loop do
	 (debug-message "SOLVER-NONTRIVIAL: ENDPOINTS: ~a ~a" endpoint-start endpoint-end)
	 (when *debug-mode* (print-board-pretty board-state))
	 (if invalid-solution
	     (progn
	       ;; The most recent attempt to solve a nontrivial solution has failed.
	       ;; This indicates that a previous attempt arrived at its solution in a way that
	       ;; caused a conflict with the most recent attempt.
	       ;; To resolve the issue, backtrack to previous attempt and try an alternate solution.
	       (debug-message "SOLVER-NONTRIVIAL: ALTERNATE:~%~a~%~a~%~a~%" move-tracker board-state attempt-tracker)
	       (multiple-value-bind (move-tracker-result board-state-result attempt-tracker-result endpoint-tracker-result)
		   (nontrivial-solution-alternate-solution 
		    board-state endpoint-start endpoint-end move-tracker board-state attempt-tracker endpoint-tracker)
		 (setf move-tracker move-tracker-result)
		 (setf board-state board-state-result)
		 (debug-message "BOARD-STATE: ~a" board-state)
		 (setf attempt-tracker attempt-tracker-result)
		 (setf endpoint-tracker endpoint-tracker-result)))
	     (multiple-value-bind (solutions-trivial-result board-state-trivial)
		 ;; Attempt to find a nontrivial solution by a) solving all trivial solutions
                 ;; and examining result to see if any unsolved sequence endpoints remain.
		 (solve-trivial board-state)
	       (setf move-tracker nil)
	       (setf solutions-trivial solutions-trivial-result)
	       (setf board-state board-state-trivial)
	       (debug-message "BOARD-STATE: ~a" board-state)
	       (setf endpoints (first (incomplete-sequence-endpoints board-state)))
	       (setf endpoint-start (first endpoints))
	       (setf endpoint-end (second endpoints))
	       (when endpoints
		 ;; Non-trivial solutions remain.
                 ;; Attempt to find such a solution.
		 (multiple-value-bind (move-tracker-result board-state-result 
							   attempt-tracker-result endpoint-tracker-result)
		     (nontrivial-solution-search-delegator board-state endpoint-start endpoint-end)
		   (setf move-tracker move-tracker-result)
		   (setf board-state board-state-result)
		   (debug-message "BOARD-STATE: ~a" board-state)
		   (setf attempt-tracker attempt-tracker-result)
		   (setf endpoint-tracker endpoint-tracker-result)))))
	 (when endpoints
	   ;; Only consider solutions if non-trivial positions remain.
	   (if move-tracker
	       (progn
		 ;; A solution has been found.
		 ;; 1. Signal that a valid solution has been found by setting INVALID-SOLUTION to NIL
		 ;; 2. Push this solution onto the SOLUTION-TRACKER stack
		 (setf invalid-solution nil)
		 (push (make-nontrivial-solution :move-tracker move-tracker 
						 :move-trivial-tracker solutions-trivial
						 :board-state board-state
						 :attempt-tracker attempt-tracker
						 :endpoint-start endpoint-start
						 :endpoint-end endpoint-end
						 :endpoint-tracker endpoint-tracker)
		       solution-tracker)
		 (debug-message "SOLVER-NONTRIVIAL: MOVE FOUND:~%move-tracker:~a~%board-state:~a~%invalid-solution:~a~%solution-tracker:~a~%" move-tracker board-state invalid-solution solution-tracker))	       
	       (progn
		 ;; No solution was found. Backtrack as follows:
		 ;; 1. Pop the most recent solution off of the SOLUTION-TRACKER stack
		 ;;    this is INVALID-SOLUTION. INVALID-SOLUTION triggers search for an alternate solution.
		 ;; 2. Restore system state to conditions corresponding to those experienced by the system
		 ;;    during the time INVALID-SOLUTION was computed.
		 (debug-message "SOLVER-NONTRIVIAL: MOVE NOT FOUND")
		 (setf invalid-solution (pop solution-tracker))
		 (when invalid-solution
		   (setf move-tracker (nontrivial-solution-move-tracker invalid-solution))
		   (setf solutions-trivial (nontrivial-solution-move-trivial-tracker invalid-solution))
		   (setf board-state (nontrivial-solution-board-state invalid-solution))
		   (setf attempt-tracker (nontrivial-solution-attempt-tracker invalid-solution))
		   (setf endpoint-start (nontrivial-solution-endpoint-start invalid-solution))
		   (setf endpoint-end (nontrivial-solution-endpoint-end invalid-solution))
		   (setf endpoint-tracker (nontrivial-solution-endpoint-tracker invalid-solution)))
		 (debug-message "invalid-solution: ~a" invalid-solution))))
	 (when (and (null endpoints) solutions-trivial)
	   (debug-message "Capturing lingering trivial solutions: ~a" solutions-trivial)
	   ;; Capture any lingering trivial solutions
	   (push (make-nontrivial-solution :move-tracker move-tracker 
					   :move-trivial-tracker solutions-trivial
					   :board-state board-state
					   :attempt-tracker attempt-tracker
					   :endpoint-start endpoint-start
					   :endpoint-end endpoint-end
					   :endpoint-tracker endpoint-tracker)
		 solution-tracker))
       while
	 (board-invalid? board-state))
    (values
     board-state
     solution-tracker)))

(defun extract-moves-from-solution-tracker (solution-tracker)
  "Extract all moves, trivial and nontrivial from SOLUTION-TRACKER.
   Return a list of moves in the order in which they were taken."
  (let ((moves (mapcan #'(lambda (solution)
			   (debug-message "extracting: ~a" solution)
			   (append (nontrivial-solution-move-tracker solution)
				   (nontrivial-solution-move-trivial-tracker solution)))
		       solution-tracker)))
    (reverse moves)))

(defun solver-trivial (board)
  "Numbrix game board solver. 
   Can only solve those boards that consist entirely of trivial solutions."
  (loop do
       (let ((solution-found nil))
	 (dotimes (x *size*)
	   (dotimes (y *size*)
	     (let ((solution (trivial-solution board x y)))
	       (when solution
		 (debug-message "solved: ~a at (~a ~a)" solution x y)
		 (setf solution-found t)
		 (set-pos board x y solution t)))))
	 (let ((solutions-multi-cell (trivial-solution-multi-cell board)))
	   (when solutions-multi-cell
	     (progn
	       (apply-solutions board solutions-multi-cell)
	       (setf solution-found t))))
	 (when (and (not solution-found)
		    (board-invalid? board))
	   (print-board-pretty board)
	   (error (format nil "Board not trivial!"))))
       (error-if-duplicate-values board "SOLVER-TRIVIAL")
     while 
       (board-invalid? board))
  board)

(defun solve (solver-type board)
  "Debugging function. Can trigger solution of BOARD using brute force, trivial, or nontrivial
   solution methods."
  (print-board-pretty
   (ecase solver-type
     (brute-force (solver-brute-force board))
     (trivial (solver-trivial board))
     (nontrivial (solver-nontrivial board)))))

(defun solver-nontrivial-benchmark ()
  "Solves *BOARD* using nontrivial solution method.
   Returns the board solution, the moves that were taken during the solution,
   and the wall-clock and CPU time to obtain a solution."
  (let ((real-start (get-internal-real-time))
	(cpu-start (get-internal-run-time)))
    (multiple-value-bind (board-result solutions-result)
	(solver-nontrivial *board*)
    (let* ((real-end (get-internal-real-time))
	   (cpu-end (get-internal-run-time))
	   (real-runtime (float (/ (- real-end real-start) internal-time-units-per-second)))
	   (cpu-runtime (float (/ (- cpu-end cpu-start) internal-time-units-per-second)))
	   (moves (extract-moves-from-solution-tracker solutions-result)))
      (values
       board-result
       moves
       real-runtime
       cpu-runtime)))))

(defun solver ()
  "Initiates an interactive nontrivial solution solver session."
  (format t "~%Choose a board:~%")
  (board-menu)
  (format t "~%BOARD UNSOLVED:~%~%")
  (print-board-pretty *board*)
  (print-user-error-msg "The solver will now be run.")
  (multiple-value-bind (board-result moves real-runtime cpu-runtime)
      (solver-nontrivial-benchmark)
    (let ((move-format "~3d. Value: ~3d  X:~3d  Y:~3d~%"))
      (format t "~%BOARD SOLVED:~%~%")
      (print-board-pretty board-result)
      (format t "Execution Time (seconds):~%")
      (format t "    Wall Clock: ~a~%" real-runtime)
      (format t "    CPU Time:   ~a~%~%~%" cpu-runtime)
      (when (prompt-yes-no "Would you like to view a list of moves?")
	(let ((counter 0))
	  (format t "~%")
	  (mapcar 
	   #'(lambda (m)
	       (let ((value (first m))
		     (x (1+ (second m)))
		     (y (- *size* (third m))))
		 (format t move-format (incf counter) value x y)))
	   moves)))
      (when (prompt-yes-no "Would you like to see the board updated after each move?")
	(let ((board-state (copy-board *size* *board*))
	      (counter 0)
	      (move-format (concatenate 'string "~%" move-format "~%")))
	  (mapcar #'(lambda (move)
		      (let ((value (first move))
			    (x (1+ (second move)))
			    (y (- *size* (third move))))
			(apply-solutions board-state (list move))		       
			(print-board-pretty board-state)
			(format t move-format (incf counter) value x y)
			(prompt-read-enter)))
		  moves)))
      (when (prompt-yes-no "Do you want to solve another board?")
	(solver)))))
