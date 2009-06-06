#|

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

test.lisp - Test suite used to verify correct function of Numbrix puzzle solver.

|#

(defun test-nontrivial-solution-search (tests)
  (mapcar #'(lambda (test)
	      (let* ((board (first test))
		     (expected-result (second test)))
		(load-board board)
		(let* ((endpoints (first (incomplete-sequence-endpoints *board*)))
		       (start (first endpoints))
		       (end (second endpoints))
		       (board-copy (copy-board *size* *board*)))
		  (multiple-value-bind (result board-state)
		      (nontrivial-solution-search *board* start end)
		    (unless (equal expected-result result)
		      (error (format nil "ERROR:~%NONTRIVIAL-SOLUTION-SEARCH~%EXPECTED: ~a~%ACTUAL: ~a" 
				     expected-result result)))
		    (when (and (null result)
			       (not (equalp board-copy board-state)))
		      (error (format nil "ERROR:~%NONTRIVIAL-SOLUTION-SEARCH~%Board destructively edited!~%~a~%~a~%"
				     board-state board-copy)))))))
	  tests))

(defun test-exec-nontrivial-solution-search ()
  (let ((tests '(("boards/3x3-search-01.lisp" 
		  ((6 1 2) (5 1 1) (4 2 1) (3 2 0) (2 1 0)))
		 ("boards/3x3-search-03.lisp" 
		  nil)
		 ("boards/3x3-search-04.lisp"
		  nil)
		 ("boards/3x3-search-05.lisp"
		  ((4 2 1) (3 1 1) (2 0 1)))
		 ("boards/3x3-search-06.lisp"
		  ((7 1 1) (6 1 2)))
		 ("boards/3x3-search-07.lisp"
		  ((8 0 1) (7 0 2) (6 1 2) (5 2 2) (4 2 1) (3 2 0) (2 1 0)))
		 ("boards/3x3-search-08.lisp"
		  nil)
		 ("boards/3x3-search-09.lisp"
		  ((7 2 0) (6 2 1) (5 2 2) (4 1 2) (3 0 2) (2 0 1)))
		 ("boards/3x3-search-10.lisp" 
		  ((4 2 0) (3 1 0) (2 0 0)))
		 ("boards/3x3-search-11.lisp" 
		  nil)
		 ("boards/4x4-search-01.lisp"
		  ((9 2 0) (8 2 1) (7 2 2) (6 2 3) (5 1 3) (4 0 3) (3 0 2) (2 0 1)))
		 ("boards/4x4-search-02.lisp"
		  ((15 2 0) (14 3 0) (13 3 1) (12 3 2) (11 3 3) (10 2 3) (9 1 3) (8 0 3) (7 0 2) (6 1 2) (5 2 2) (4 2 1) (3 1 1) (2 0 1)))
		 ("boards/4x4-search-04.lisp"
		  nil)
		 ("boards/4x4-search-05.lisp"
		  ((15 0 0) (14 1 0) (13 1 1) (12 1 2) (11 0 2) (10 0 3) (9 1 3) (8 2 3) (7 2 2) (6 2 1) (5 2 0) (4 3 0) (3 3 1) (2 3 2)))
		 )))
    (test-nontrivial-solution-search tests)))

(defun test-nontrivial-solution-alternate-solution (boardname)
  (load-board boardname)
  (let* ((endpoints (first (incomplete-sequence-endpoints *board*)))
	 (start (first endpoints))
	 (end (second endpoints))
	 (move-tracker nil)
	 (board-state nil)
	 (attempt-tracker nil)
	 (solution-counter 0))
  (loop do
       (if move-tracker
	   (multiple-value-bind (move-tracker-result board-state-result attempt-tracker-result)
	       (nontrivial-solution-alternate-solution *board* start end move-tracker board-state attempt-tracker nil)
	     (setf move-tracker move-tracker-result)
	     (setf board-state board-state-result)
	     (setf attempt-tracker attempt-tracker-result))
	   (multiple-value-bind (move-tracker-result board-state-result attempt-tracker-result)
	       (nontrivial-solution-search *board* start end)
	     (setf move-tracker move-tracker-result)
	     (setf board-state board-state-result)
	     (setf attempt-tracker attempt-tracker-result)))
       (when move-tracker
	 (incf solution-counter))
       while
       move-tracker)
  solution-counter))

(defun test-exec-solve-trivial ()
  (let* ((tests '(
		  ("boards/3x3-trivial-01-unsolved.lisp"
		   ((8 1 2) (5 1 1) (2 1 0)))
		  ("boards/4x4-trivial-01-unsolved.lisp"
		   ((12 3 2) (4 3 0) (14 2 3) (6 2 1) (2 1 0) (9 0 2)))
		  ("boards/4x4-trivial-02-unsolved.lisp"
		   ((4 3 0) (15 2 2) (6 2 1) (11 1 3) (2 1 0) (9 0 2)))
		  ("boards/4x4-trivial-03-unsolved.lisp"
		   ((2 1 0) (3 2 0) (6 2 1) (7 1 1) (10 1 2) (11 2 2) (14 2 3) (15 1 3)))
		  ("boards/4x4-trivial-04-unsolved.lisp"
		   ((2 0 1) (3 0 2) (5 1 3) (6 2 3) (8 3 2) (9 3 1) (15 2 2) (11 2 0) (13 1 1)))
		  )))
    (mapcar #'(lambda (test)
		(let* ((boardname (first test))
		       (result-expected (second test)))
		  (load-board boardname)
		  (let ((result-actual (solve-trivial *board*)))
		    (if (equal result-expected result-actual)
			(progn
			  (apply-solutions *board* result-actual)
			  (debug-message "solutions: ~a" result-actual)
			  (print-board-pretty *board*))
			(error (format nil "ERROR:~%SOLVER-TRIVIAL~%EXPECTED: ~a~%ACTUAL: ~a" 
				       result-expected result-actual))))))
	    tests)))

(defun test-exec-nontrivial-solution-alternate-solution ()
  (let* ((tests '(
		  ("boards/3x3-search-10.lisp" 6)
		  ("boards/4x4-search-06.lisp" 14)
		  )))
    (mapcar #'(lambda (test)
		(let* ((board (first test))
		       (solutions-expected (second test))
		       (solution-counter (test-nontrivial-solution-alternate-solution board)))
		  (when (/= solution-counter solutions-expected)
		    (error (format nil 
				   "ERROR: NONTRIVIAL-SOLUTION-ALTERNATE-SOLUTION~%Expected: ~a~%Actual: ~a"
				   solutions-expected
				   solution-counter)))))
	    tests)))

(defun test-solver-nontrivial (board)
  (load-board board)
  (format t "~%## SOLVER-NONTRIVIAL ~a ##~%" board)
  (print-board-pretty *board*)
  (multiple-value-bind (board-state solution-tracker)
      (solver-nontrivial *board*)
    (let ((result-actual (extract-moves-from-solution-tracker solution-tracker))
	  (board-result (copy-board *size* *board*)))
      (apply-solutions board-result result-actual)
      (format t "EXPECTED~%")
      (print-board-pretty board-result)
      (format t "ACTUAL~%")
      (when (not (equalp board-state board-result))
	(error (format nil "ERROR: Result report incorrect!")))
      (print-board-pretty board-state))))

(defun test-exec-solver-nontrivial-personal ()
  (let ((tests '(
		 "boards/2x2-nontrivial-01.lisp"
		 "boards/3x3-nontrivial-01.lisp"
		 "boards/3x3-nontrivial-02.lisp"
		 "boards/3x3-nontrivial-03.lisp"
		 "boards/3x3-nontrivial-04.lisp"
		 "boards/3x3-nontrivial-05.lisp"
		 "boards/3x3-nontrivial-06.lisp"
		 "boards/3x3-nontrivial-07.lisp"
		 "boards/3x3-nontrivial-08.lisp"
		 "boards/3x3-nontrivial-09.lisp"
		 "boards/3x3-nontrivial-10.lisp"
		 "boards/3x3-nontrivial-11.lisp"
		 "boards/3x3-nontrivial-12.lisp"
		 "boards/3x3-nontrivial-13.lisp"
		 "boards/3x3-nontrivial-14.lisp"
		 "boards/4x4-nontrivial-02.lisp"
		 "boards/4x4-nontrivial-03.lisp"
		 "boards/4x4-nontrivial-04.lisp"
		 "boards/4x4-nontrivial-05.lisp"
		 "boards/4x4-nontrivial-06.lisp"
		 "boards/4x4-nontrivial-07.lisp"
		 "boards/4x4-nontrivial-09.lisp"
		 "boards/4x4-nontrivial-10.lisp"
		 "boards/4x4-nontrivial-11.lisp"
		 "boards/4x4-nontrivial-12.lisp"
		 "boards/4x4-nontrivial-13.lisp"
		 "boards/4x4-nontrivial-14.lisp"
		 "boards/5x5-nontrivial-01.lisp"
		 "boards/8x8-01-unsolved.lisp"
		 "boards/8x8-02-unsolved.lisp"
		 "boards/9x9-nontrivial-01.lisp"
		 "boards/9x9-nontrivial-02.lisp"
		 "boards/9x9-nontrivial-03.lisp"
		 "boards/9x9-nontrivial-04.lisp"
		 "boards/9x9-nontrivial-05.lisp"
		 "boards/9x9-nontrivial-06.lisp"
		 "boards/9x9-nontrivial-07.lisp"
		 "boards/9x9-nontrivial-08.lisp"
		 "boards/9x9-nontrivial-09.lisp"
		 "boards/9x9-nontrivial-10.lisp"
		 "boards/10x10-nontrivial-01.lisp"
		 "boards/10x10-nontrivial-02.lisp"
		 )))
    (test-exec-solver-nontrivial tests)))

(defun test-exec-solver-nontrivial-tournament ()
  (let ((tests '(
		 "boards/tournament-01.lisp"
		 "boards/tournament-02.lisp"
		 "boards/tournament-03.lisp"
		 "boards/tournament-04.lisp"
		 "boards/tournament-05.lisp"
		 "boards/tournament-06.lisp"
		 "boards/tournament-07.lisp"
		 "boards/tournament-08.lisp"
		 "boards/tournament-09.lisp"
		 "boards/tournament-10.lisp"
		 ;"boards/tournament-11.lisp"
		 "boards/tournament-12.lisp"
		 "boards/tournament-13.lisp"
		 ;"boards/tournament-14.lisp"
		 ;"boards/tournament-15.lisp"
		 )))
    (test-exec-solver-nontrivial tests)))

(defun test-exec-solver-nontrivial (tests)
  (mapcar #'(lambda (board) (test-solver-nontrivial board)) tests))

(defun test-exec-all ()
  (test-exec-nontrivial-solution-search)
  (test-exec-nontrivial-solution-alternate-solution)
  (test-exec-solve-trivial)
  (test-exec-solver-nontrivial-personal))
