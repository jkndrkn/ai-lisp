#|

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

Traveling salesman problem with minor optimizations:

    * Redundant path eliminatin
    * Pre-emptive path pruning

If you are assigned this problem in a course and come across this code,
plagiarize at your own risk ;]

Usage:

> (traveling-salesman)

|#

(defparameter *graph* nil)

(defparameter *path-permutator-counter* nil)

(defparameter *distance-shortest* nil)

(defparameter *path-shortest* nil)

(defparameter *debug* nil)

(defun generate-unsafe (x lis)
  "Generates a list of 'unsafe' (visited) nodes given a list of nodes LIS
   and a node X in that lis. Used to abort expansion of symmetrical redundant
   paths."
  (let ((size (position x lis))
	(result nil))
    (dotimes (i size)
      (setf result (append result (list (elt lis i)))))
    result))

(defun path-distance (path)
  "Accepts PATH and returns the distance through that path according to the 
   graph data in *GRAPH*."
  (let ((distance 0))
    (dotimes (i (1- (length path)))
      (let* ((source (elt path i))
	     (dest (elt path (1+ i)))
	     (distances (expand-node dest))
	     (distance-neighbor (second (get-node source distances))))
	(when (null distance-neighbor)
	  (error 
	   (format nil "source: ~a dest: ~a distances: ~a ~%" source dest distances)))
	(setf distance (+ distance distance-neighbor))))
    distance))

(defun path-permutator-exec (lis start goal size &optional nodes permutation unsafe (depth 0))
  "Generates a list of all paths in LIS. Guarantees that symmetric paths aren't visited.
   Halts path exploration prematurely if the distance of the path exceeds the value of 
   *DISTANCE-SHORTEST*."
  (let* ((nodes (if (null nodes) lis nodes))
	 (top-level (eq (length lis) size))
	 (distance (path-distance (cons start permutation))))
    (cond 
      ((> distance *distance-shortest*)
       (progn
	 (when *debug* (format t "TOO LONG: ~a ~a ~a~%" permutation distance *distance-shortest*))
	 nil))
      ((null lis) nil)
      ((null (rest lis))
       (let* ((path-result (append (cons start permutation) lis (list goal)))
	      (distance-result (path-distance path-result)))
	 (incf *path-permutator-counter*)
	 (when (< distance-result *distance-shortest*)
	   (setf *distance-shortest* distance-result)
	   (setf *path-shortest* path-result))
	 (when *debug* (format t "count: ~a unsafe: ~a distance: ~a result: ~a~%" 
			       *path-permutator-counter* unsafe distance-result path-result))))
      (t (dolist (x lis)
	   (let ((index (position x nodes)))
	     (unless (and (< (1- size) index) top-level)
	       (let* ((base (list x))
		      (sublis (remove x lis))
		      (next (car sublis))
		      (permutation-new (append permutation base)))
		 (cond 
		   ((and (eq 0 index) top-level)
		    (path-permutator-exec sublis start goal size nodes permutation-new unsafe depth))
		   (t
		    (let* ((unsafe (if top-level (generate-unsafe x nodes) unsafe))
			   (next-unsafe (member next unsafe))
			   (depth (if next-unsafe (length (rest sublis)) (1- depth))))
		      (unless (and next-unsafe (eq depth 0))
			(path-permutator-exec sublis start goal size nodes permutation-new unsafe depth)))))))))))))

(defun path-permutator (lis start goal)
  "Initializer for PATH-PERMUTATOR-EXEC."
  (let ((size (length lis)))
    (setf *path-permutator-counter* 0)
    (path-permutator-exec lis start goal size)))

(defun load-file (filename)
  "Loads data corresponding to a s-expression in file with name FILENAME."
  (with-open-file (stream filename)
    (read stream)))

(defun init-graph (graph start goal)
  "Sets the global *GRAPH* parameter to contain a hash table that represents the graph.
   Creates a duplicate of START named GOAL and alters initial graph structure to reflect this."
  (if *graph*
      (clrhash *graph*)
      (setf *graph* (make-hash-table)))
  (mapcar #'(lambda (x) 
	      (let* ((dest (first x))
		     (neighbors (second x))
		     (goal-node (get-node start neighbors)))
		(setf (gethash dest *graph*) 
		      (if goal-node
			  (append (second x) (list (list goal (second goal-node))))
			  (second x)))))
	  graph)
  (setf (gethash goal *graph*) (expand-node start)))

(defun init (graphname)
  "Initializer function for a constrained brute force traveling salesman solver.
   A best-first solution is used as a baseline against which to reject or explore
   possible path permutations."
  (let* ((graph (load-file graphname))
	 (goal 'goal)
	 (nodes (append (mapcar #'car graph) (list goal)))
	 (start (first nodes))
	 (search-space (rest (butlast nodes))))
    (init-graph graph start goal)
    (multiple-value-bind (route estimate) (best-first-search nodes start goal)
      (format t "best-first: ~a ~a~%" route estimate)
      (setf *path-shortest* route)
      (setf *distance-shortest* estimate)
      (path-permutator search-space start goal)))
  (values *path-shortest* *distance-shortest*)) 

(defun expand-node (node)
  "Expands all neighbors of NODE in *GRAPH*."
  (gethash node *graph*))

(defun get-node (node nodes)
  "Will return the node NODE in NODES."
  (first (remove-if-not #'(lambda (x) (eq (car x) node)) nodes)))

(defun sort-node (node1 node2)
  "Returns T if the cost to travel to node1 is less than the cost of node2."
  (< (second node1) (second node2)))

(defun sort-nodes (nodes)
  "Nondestructive sorter for graph nodes."
  (sort (copy-list nodes) #'sort-node))

(defun select-route (search-queue nodes path goal)
  "Selects a route given information on problem state."
  (let ((new-state nil))
    (mapcar #'(lambda (x)
		(let ((dest (car x)))
		  (cond
		    ((and (not (find dest path))
			  (null new-state)
			  (not (eq dest goal)))
		     (setf new-state x))
		    ((and (null (set-difference nodes (push goal path)))
			  (eq dest goal))
		     (setf new-state x)))))
	    search-queue)
    new-state))

(defun best-first-search (nodes start goal &optional path state (distance 0))
  "Best-first implementation of travelling salesman algorithm."
  (let* ((state (if (null state) start state))
	 (path (push state path)))
    (cond ((null (set-difference nodes path)) 
	   (values (reverse path)
		   distance))
	  (t (let* ((search-queue (sort-nodes (expand-node state)))
		    (route (select-route search-queue nodes path goal))
		    (new-state (first route))
		    (new-distance (second route)))
	       (when *debug*
		 (format t "NODES: ~a~%START: ~a~%GOAL: ~a~%PATH: ~a~%STATE: ~a~%NEW-STATE: ~a~%SEARCH-QUEUE: ~a~%"
			 nodes start goal path state new-state search-queue))
	       (best-first-search nodes start goal path new-state (+ distance new-distance)))))))

(defun traveling-salesman ()
  "Initializer for best-first-search implementation of traveling salesman."
  (let ((graphs (load-file "graphs.lisp")))
    (mapcar #'(lambda (x) 
		(time
		 (multiple-value-bind (route distance) (init x)
		   (format t "~a:~%~a~%~a~%~%" x route distance)))
		(format t "~%"))
	    graphs)))
