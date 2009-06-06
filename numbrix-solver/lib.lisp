#|

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

lib.lisp - Core numbrix functionality used by both game.lisp and solver.lisp.

|#

(defmacro debug-message (control-string &rest format-arguments)
  (when *debug-mode*
    `(format t (concatenate 'string ,control-string "~%") ,@format-arguments)))

(defun value-empty? (val)
  (= val 0))

(defun value-set? (val)
  (not (value-empty? val)))

(defun get-pos (board x y)
  "Retrieve the BOARD value at X Y"
  (aref (aref board y) x))
    
(defun set-pos (board x y val &optional verify)
  "Set the BOARD value at X Y. BOARD is destructively edited as a side-effect."
  (when (and verify
	     (value-set? (get-pos board x y)))
    (error (format nil "Position x: ~a y: ~a is being written to with value ~a but it already has value ~a"
		   x y val (get-pos board x y))))
  (setf (aref (aref board y) x) val))

(defun convert-internal-coord-x (x)
  "Converts an internal coordinate specification into a user-friendly form."
  (1+ x))

(defun convert-internal-coord-y (y)
  "Converts an internal coordinate specification into a user-friendly form."
  (- *size* y))

(defun get-neighbor-n (board x y)
  "Get the value of the neighbor to the North of position X Y"
  (get-pos board x (1- y)))

(defun get-neighbor-ne (board x y)
  "Get the value of the neighbor to the Northeast of position X Y"
  (get-pos board (1+ x) (1- y)))

(defun get-neighbor-e (board x y)
  "Get the value of the neighbor to the East of position X Y"
  (get-pos board (1+ x) y))

(defun get-neighbor-se (board x y)
  "Get the value of the neighbor to the Southeast of position X Y"
  (get-pos board (1+ x) (1+ y)))

(defun get-neighbor-s (board x y)
  "Get the value of the neighbor to the South of position X Y"
  (get-pos board x (1+ y)))

(defun get-neighbor-sw (board x y)
  "Get the value of the neighbor to the Southwest of position X Y"
  (get-pos board (1- x) (1+ y)))

(defun get-neighbor-w (board x y)
  "Get the value of the neighbor to the West of position X Y"
  (get-pos board (1- x) y))

(defun get-neighbor-nw (board x y)
  "Get the value of the neighbor to the Northwest of position X Y"
  (get-pos board (1- x) (1- y)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun interleave-lists (list1 list2)
    "Accepts two lists and interleaves their elements, with element 1 of list1 leading the list.
     Similar to MERGE except that it does not perform any sorting."
    (cond
      ((null list1) list2)
      ((null list2) list1)
      (t (append (list (car list1)) 
		 (list (car list2)) 
		 (interleave-lists (rest list1) (rest list2)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-neighbor-exp (get-neighbor-fun)
    "Helper function used in the macro MAKE-GET-NEIGHBOR-EXPS."
    (let ((neighbor-fun (getf *neighbor-function-types* get-neighbor-fun)))
      (list 'push (list neighbor-fun 'board 'x 'y) 'neighbors))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-neighbor-exps (get-neighbor-funs)
    "Helper function used in the macro MAKE-GET-NEIGHBOR-EXPS."
    (mapcar #'(lambda (x) (get-neighbor-exp x)) get-neighbor-funs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro make-get-neighbor-exps (&rest funs)
    "Builds expressions for retrieval of neighboring values."
    `(progn ,@(get-neighbor-exps funs))))

(defun get-neighbors (board x y &optional complete)
  "Get the values of all cells that neighbor the cell at position X Y."
  (let ((position-type (get-pos *board-position-types* x y))
	(neighbors ()))
    (when (> *size* 1)
      (if complete
	(ecase position-type
	  (inner (make-get-neighbor-exps :nw :w :sw :s :se :e :ne :n))
	  (n (make-get-neighbor-exps :w :sw :s :se :e))
	  (ne (make-get-neighbor-exps :w :sw :s))
	  (e (make-get-neighbor-exps :nw :w :sw :s :n))
	  (se (make-get-neighbor-exps :nw :w :n))
	  (s (make-get-neighbor-exps :nw :w :e :ne :n))
	  (sw (make-get-neighbor-exps :e :ne :n))
	  (w (make-get-neighbor-exps :s :se :e :ne :n))
	  (nw (make-get-neighbor-exps :s :se :e)))
	(ecase position-type
	  (inner (make-get-neighbor-exps :w :s :e :n))
	  (n (make-get-neighbor-exps :w :s :e))
	  (ne (make-get-neighbor-exps :w :s))
	  (e (make-get-neighbor-exps :w :s :n))
	  (se (make-get-neighbor-exps :w :n))
	  (s (make-get-neighbor-exps :w :e :n))
	  (sw (make-get-neighbor-exps :e :n))
	  (w (make-get-neighbor-exps :s :e :n))
	  (nw (make-get-neighbor-exps :s :e)))))
    neighbors))

(defun make-board (size &optional (init-val 0) (data-type 'integer))
  "Return a two-dimensional vector for the representation of game boards."
  (let ((board (make-array size :initial-element (make-array size) :element-type 'vector)))
    (map 'vector 
	 #'(lambda (x) (setf x (make-array size :initial-element init-val :element-type data-type)))
	 board)))

(defun make-board-default-positions (board)
  "Accepts BOARD. Returns a board of the same size with all non-zero values in BOARD set to T,
   and all zero values set to NIL."
  (let ((board-default-positions (make-board *size* nil 'boolean)))
    (dotimes (y *size*)
      (dotimes (x *size*)
	(let ((value (get-pos board x y)))
	  (when (> value 0)
	    (set-pos board-default-positions x y t)))))
    board-default-positions))

(defun make-board-position-types ()
  "Returns a board of the same size with all non-zero values 
   in BOARD set to one of *POSITION-TYPES*"
  (let ((board-position-types (make-board *size* nil 'symbol)))
    (dotimes (y *size*)
      (dotimes (x *size*)
	(let ((value (square-position-type x y)))
	  (set-pos board-position-types x y value))))
    board-position-types))

(defun load-board (boardname)
  "Reads a board definition file and indirectly triggers initialization of game state."
  (numbrix-board 
   (with-open-file (stream boardname)
     (read stream))))

(defun load-boardname-list ()
  "Reads an externally defined list of game boards into a list."
  (setf *boardname-list*
	(with-open-file (stream *boardname-definitions*)
	  (read stream))))

(defun read-board-definition (board-definition)
  "Reads a board definition expressed as a two-dimensional list and
   returns an identical two-dimensional vector."
  (let* ((size (length board-definition))
	 (board (make-board size)))
    (when board-definition
      (dotimes (y size)
	(dotimes (x size)
	  (set-pos board x y (elt (elt board-definition y) x)))))
    board))

(defun init (&key size board)
  "Initializes several global variables used to represent game state."
  (cond (size
	 (progn
	   (setf *size* size)
	   (setf *board* (make-board *size*))))
	(board
	 (progn
	   (setf *size* (length board))
	   (setf *board* board)))
	(t
	 (error "INIT - unexpected input")))
  (setf *board-default-positions* (make-board-default-positions *board*))
  (setf *board-position-types* (make-board-position-types))
  (setf *num-squares* (* *size* *size*))
  (setf *user-moves* ())
  board)

(defun numbrix-board (board-definition)
  "Triggers initialization of game state using an s-expression."
  (init :board (read-board-definition board-definition)))

(defun flatten-board (board &optional with-coords)
  "Accepts a two-dimensional board definition and returns a one-dimensional representation."
  (let ((flattened-board ()))
    (dotimes (y *size*)
      (dotimes (x *size*)
	(let* ((value (get-pos board x y))
	       (element (if with-coords (list value x y) value)))
	  (setf flattened-board (append flattened-board (list element))))))
    flattened-board))

(defun make-target-values ()
  "Returns a list of all values that should be found in a board with *NUM-SQUARES* cells."
  (let ((target-values ()))
    (dotimes (i *num-squares*)
      (setf target-values (append target-values (list (1+ i)))))
      target-values))

(defun missing-values (board)
  "Returns those values that are missing from BOARD."
  (let ((values (flatten-board board))
	(target-values (make-target-values)))
    (set-difference target-values values)))

(defun missing-values? (board)
  "Returns T of there are any values missing from BOARD."
  (consp (missing-values board)))

(defun remove-empty (values &optional with-coords)
  "Returns only non-empty values in the list VALUES."
  (remove-if-not #'(lambda (x) (> (if with-coords (first x) x) 0)) values))

(defun duplicate-values (board)
  "Returns those values that appear more than once in BOARD."
  (let* ((values (stable-sort (remove-empty (flatten-board board)) #'>))
	 (values-no-duplicates (remove-duplicates values))
	 (duplicates ()))
    (dolist (value values)
      (let ((test-value (car values-no-duplicates)))
	(if (not (eq value test-value))
	    (push value duplicates)
	    (pop values-no-duplicates))))
    duplicates))

(defun duplicate-values? (board)
  "Returns T if there are any duplicate values in BOARD."
  (consp (duplicate-values board)))

(defun empty-squares? (board)
  "Returns T if there are any empty squares in BOARD."
  (let ((values (flatten-board board)))
    (not (eq (length (remove-empty values)) (length values)))))

(defun square-position-type (x y &optional return-type-key)
  "Returns an atom indicating what position type a cell belongs to."
  (let ((bound-lower 0)
	(bound-upper (1- *size*))
	(position-types (if return-type-key *position-types-key* *position-types*)))
    (cond ((eq y bound-lower)
	   (cond ((eq x bound-lower)
		  (getf position-types :nw))
		 ((eq x bound-upper)
		  (getf position-types :ne))
		 (t
		  (getf position-types :n))))
	  ((eq y bound-upper)
	   (cond ((eq x bound-lower)
		  (getf position-types :sw))
		 ((eq x bound-upper)
		  (getf position-types :se))
		 (t
		  (getf position-types :s))))
	  ((eq x bound-lower)
	   (getf position-types :w))
	  ((eq x bound-upper)
	   (getf position-types :e))
	  (t
	   (getf position-types :inner)))))

(defun check-neighbor (neighbor)
  "Returns T if neighbor is valid."
  (and (integerp neighbor) (> neighbor 0)))

(defun has-neighbor-prev (value neighbors)
  "Returns T if VALUE N has a neighbor M of value N - 1."
  (check-neighbor (find (1- value) neighbors)))

(defun has-neighbor-next (value neighbors)
  "Returns T if VALUE N has a neighbor M of value N + 1."
  (check-neighbor (find (1+ value) neighbors)))

(defun check-neighbors (board x y)
  "Returns NIL if the position X Y and its neighbors do not form an integer sequence.
   Otherwise returns the value at position X Y."
  (let ((endpoint-upper *num-squares*)
	(endpoint-lower 1)
	(value (get-pos board x y))
	(neighbors (get-neighbors board x y))
	(neighbors-ok t))
    (cond 
      ((eq endpoint-lower endpoint-upper)
       (setf neighbors-ok (eq value endpoint-lower)))
      ((eq value endpoint-lower)
       (setf neighbors-ok (has-neighbor-next value neighbors)))
      ((eq value endpoint-upper)
       (setf neighbors-ok (has-neighbor-prev value neighbors)))
      (t
       (setf neighbors-ok (and (has-neighbor-next value neighbors) 
			       (has-neighbor-prev value neighbors)))))
    (unless neighbors-ok value)))

(defmacro make-board-traversal-expression (fun)
  "Helper macro used to extract duplicative code from related functions."
  `(let ((values ()))
     (dotimes (y *size*)
       (dotimes (x *size*)
	 (let ((value (,fun board x y)))
	   (when (integerp value)
	     (setf values (append values (list value)))))))
     values))

(defun copy-board (size board &optional (init-val 0) (data-type 'integer) (deep-copy t))
  (let ((board-copy (make-board size init-val data-type)))
    (when deep-copy
     (dotimes (y *size*)
       (dotimes (x *size*)
	 (let ((value (get-pos board x y)))
	   (set-pos board-copy x y value)))))
     board-copy))

(defun unordered-values (board)
  "Returns those values in board that are not part of the expected end-value sequence."
  (make-board-traversal-expression check-neighbors))

(defun unordered-values? (board)
  "Returns T if there are any invalid inter-cell relationships."
  (consp (unordered-values board)))

(defun invalid-value (board x y)
  "Returns value at X Y in BOARD if that value is too large or too small for
   the given board definition."
  (let ((value (get-pos board x y))
	(min-value 1)
	(max-value *num-squares*))
    (when (or (> value max-value)
	      (< value min-value))
      value)))

(defun invalid-values (board)
  "Returns those valuse in BOARD that are too large or too small for 
   the given board definition."
  (make-board-traversal-expression invalid-value))

(defun invalid-values? (board)
  "Returns T if BOARD contains any invalid values."
  (consp (invalid-values board)))

(defun board-to-string (board)
  "Simple function that prints BOARD in programmer-friendly fashion.
   Useful for debugging."
  (let ((board-string ""))
    (dotimes (y *size*)
      (dotimes (x *size*)
	(setf board-string (format nil "~a~4d" board-string (get-pos board x y))))
      (setf board-string (format nil "~a~%" board-string)))
    board-string))

(defun board-to-string-pretty (board)
  "Prints BOARD in a readable fashion."
  (let ((board-string ""))
    (dotimes (y *size*)
      (dotimes (x *size*)
	(setf board-string (format nil "~a~4d" board-string (get-pos board x y))))
      (setf board-string (format nil "~a~%" board-string)))
    board-string))

(defun board-to-string-pretty-component-build (size iteration component segment endpoint &key number aux-segment)
  "Returns a string representing a component used in building a user-friendly board representation."
  (cond
    ((null number) nil)
    ((= number 0) (setf segment aux-segment))
    (number (setf segment (format nil segment number))))
  (setf component (format nil "~a~a" component segment))
  (when (= (1- size) iteration)
    (setf component (format nil "~a~a" component endpoint)))
  component)

(defun board-to-string-pretty-component (size type &optional row)
  "Given a board size and TYPE atom, returns a string used in building
   a user-friendly board representation."
  (let ((divider-segment "+------")
	(divider-endpoint "+")
	(spacer-segment "|      ")
	(spacer-endpoint "|")
	(number-segment "|~4d  ")
	(component ""))
    (dotimes (i size)
      (ecase type
	(divider
	 (setf component (board-to-string-pretty-component-build 
			  size i component divider-segment divider-endpoint)))
	(spacer
	 (setf component (board-to-string-pretty-component-build 
			  size i component spacer-segment spacer-endpoint)))
	(number
	 (let ((number (elt row i)))
	   (setf component (board-to-string-pretty-component-build 
			    size i component number-segment spacer-endpoint :number number :aux-segment spacer-segment))))))
    component))

(defun print-board (board)
  "Prints a simple board represenation."
  (format t "~a" (board-to-string board)))

(defun format-board-row (row &optional (format-string "~a~a~%") (margin "   "))
  "Helper function that prints a formatted row of characters representing part of a board."
  (format t format-string margin row))

(defun format-board-x-coords ()
  "Returns a string representing a X-axis labels for a board of size *SIZE*."
  (let ((str "   "))
    (setf str (format nil "~%~a" str))
    (dotimes (i *size*)
      (setf str (format nil "~a~5d  " str (convert-internal-coord-x i))))
    str))

(defun print-board-pretty (board)
  "Prints a user-friendly board representation."
    (dotimes (row *size*)
      (format-board-row 
       (board-to-string-pretty-component *size* 'divider))
      (format-board-row 
       (board-to-string-pretty-component *size* 'spacer))
      (format-board-row 
       (board-to-string-pretty-component *size* 'number (elt board row)) "~2d ~a~%" (convert-internal-coord-y row))
      (format-board-row 
       (board-to-string-pretty-component *size* 'spacer)))
    (format-board-row (board-to-string-pretty-component *size* 'divider))
    (format t "~a~%~%" (format-board-x-coords)))

(defun board-invalid? (board)
  "Returns T if BOARD positions have not been completely or accurately filled."
  (or (empty-squares? board)
      (invalid-values? board)
      (missing-values? board)
      (unordered-values? board)))

(defun board-valid? (board)
  "Returns T if BOARD positions are all filled correctly."
  (not (board-invalid? board)))

(defun range-invalid? (min max value)
  "Returns T if VALUE lies outside of range [MIN, MAX] (inclusive)."
  (or (< value min) (> value max)))

(defun move-coord-invalid? (x y min max)
  (some #'(lambda (coord) (range-invalid? min max coord)) (list x y)))
