#|

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

Lisp Project - Genetic Programming

Implementation of genetic programming algorithm described in Chapter 4 of 
Artificial Intelligence: A New Synthesis by Nils J. Nilsson

This code is presently unable to guarantee an individual of 100% fitness.
If you are assigned this problem in a course and come across this code,
plagiarize at your own risk ;]

Usage:
> (gp)

|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *debug-mode* nil
    "Toggles debug output."))

(defparameter *population-size* 5000
  "The number of programs in a given population.")

(defparameter *tournament-proportion* 0.10
  "Defines the proportion of programs in *POPULATION-SIZE* that are selected as
   moving to a subsequent generation without undergoing genetic crossover.")

(defparameter *tournament-size* 
  (round (* *population-size* *tournament-proportion*))
  "The proportion of programs that engage in tournament selection.")

(defparameter *crossover-size* 
  (- *population-size* *tournament-size*)
  "The proportion of programs that are subjected to genetic crossover and mutation.")

(defparameter *mutation-rate* 1
  "Expresses the percent chance that mutation occurs during genetic crossover.")

(defparameter *program-size-factor* 20
  "Rough determinant of maximum size of first generation programs.")

(defparameter *tournament-competitors* 7
  "The number of programs that compete head-to-head in a given tournament round.")

(defparameter *tournament-trials* 10
  "The number of times a program is tested during a tournament.")

(defparameter *gene-types*
  (list :sensors 'sensors :functions 'functions :actions 'actions :constants 'constants))

(defparameter *sensors* 
  (list :n 'n :ne 'ne :e 'e :se 'se :s 's :sw 'sw :w 'w :nw 'nw))

(defparameter *functions*
  (list :and 'and :or 'or :not 'not :if 'if))

(defparameter *actions*
  (list :north 'north :east 'east :south 'south :west 'west))

(defparameter *constants*
  (list :1 '1 :0 '0))

(defparameter *random-gene-generators*
  (list 'random-sensor 'random-function 'random-action 'random-constant))

(defparameter *arena* nil
  "Represents the space within which programs will be evaluated.")

(defparameter *arena-size* 10
  "Represents the dimensions of *ARENA*.")

(defparameter *arena-wall-adjacencies* 32
  "Represents the number of open squares immediately adjacent to obstacle squares.")

(defparameter *arena-obstacles* '((4 7) (5 7) (4 8) (5 8))
  "Represents coordinates of obstacles within *ARENA*.")

(defparameter *arena-open-positions* ()
  "Represents non-obstacle positions within *ARENA*.")

(defparameter *program-successful*
  '(if (and (or n ne) (not e))
    east
    (if (and (or e se) (not s))
        south
	(if (and (or s sw) (not w))
            west
	    north)))
  "A program with maximum fitness. Useful for debugging and verification purposes.")

(defvar *target-fitness* 320
  "Fitness goal for programs generated using genetic programming algorithm.")

(defvar *program-actions* nil
  "Global variable used to capture the actions that result from execution of a program.")

(defvar *program-size* 0
  "Global variable used to track the size of a program as it is built.")

(defvar *gene-count* 0
  "Global variable used for counting genes.")

(defvar *performance-profiles* nil
  "Stores list of performance profile data.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro debug-message (control-string &rest format-arguments)
    (when *debug-mode*
      `(progn
	 (format t (concatenate 'string ,control-string "~%") ,@format-arguments)
	 (force-output)))))

(defun arena-set (arena x y val)
  "Set the ARENA value at X Y to value VAL."
  (setf (aref (aref arena y) x) val))

(defun arena-get (arena x y)
  "Retrieve the ARENA value at X Y."
  (aref (aref arena y) x))

(defun arena-initialize ()
  "Create arena and mark arena edges as obstacles."
  (let* ((arena-size *arena-size*)
	 (arena (make-array arena-size))
	 (min 0)
	 (max (1- arena-size)))
    (setf arena
	  (map 'vector
	       #'(lambda (x) (setf x (make-array arena-size :initial-element nil)))
	       arena))
    (dotimes (i arena-size)
      (dotimes (j arena-size)
	(when (or (= i min) (= i max) (= j min) (= j max))
	  (arena-set arena i j t))))
    arena))

(defun arena-apply-obstacles (arena obstacles)
  "Apply OBSTACLES to ARENA. Return modified ARENA."
  (mapcar #'(lambda (obstacle)
	      (let ((x (first obstacle))
		    (y (second obstacle)))
		(arena-set arena x y t)))
	  obstacles))

(defun arena-open-positions (arena)
  "Returns OPEN-POSITIONS, a list of x y coordinate pairs representing the starting locations
   that will be used during tournament selection."
  (let ((open-positions ()))
    (dotimes (y *arena-size*)
      (dotimes (x *arena-size*)
	(unless (arena-get arena x y)
	  (push (list x y) open-positions))))
    open-positions))

(defun arena-to-string (arena)
  "Converts ARENA into string suitable for human readability."
  (let ((arena-string ""))
    (dotimes (y *arena-size*)
      (dotimes (x *arena-size*)
	(setf arena-string (format nil "~a~4d" arena-string (if (arena-get arena x y) "#" "-"))))
      (setf arena-string (format nil "~a~%" arena-string)))
    arena-string))

(defun arena-print (arena)
  "Simple function that prints ARENA in programmer-friendly fashion.
   Useful for debugging."
  (format t "~%~a~%" (arena-to-string arena)))

(defun init ()
  "Initialize global parameters, particularly arena and arena-related parameters and variables."
  (setf *arena* (arena-initialize))
  (arena-apply-obstacles *arena* *arena-obstacles*)
  (setf *arena-open-positions* (arena-open-positions *arena*)))

(defun program-initial-positions (tournament-trials)
  "Returns a list of initial positions of size TOURNAMENT-TRIALS selected randomly
   with replacement from *ARENA-OPEN-POSITIONS*."
  (let ((open-positions (copy-list *arena-open-positions*))
	(initial-positions ()))
    (dotimes (i tournament-trials)
      (let* ((index (random (length open-positions)))
	     (position (elt open-positions index)))
	(push position initial-positions)
	(setf open-positions (remove position open-positions))))
    initial-positions))

(defun program-apply-sensor-data (program sensor-data)
  "Accepts a PROGRAM and SENSOR-DATA. Converts all program parameters of type *SENSORS* 
   to a 1 or 0. Such parameters are converted to 1 if they exist in SENSOR-DATA.
   They are set to 0 otherwise."
  (cond
    ((null program) nil)
    ((sensor? program)
     (if (member program sensor-data) 1 0))
    ((listp program) (mapcar #'(lambda (x) (program-apply-sensor-data x sensor-data)) program))
    (t program)))

(defun program-prepare (program)
  "Accepts a PROGRAM. Converts all 1 and 0 constants to T and NIL, respectively.
   Converts all atoms of type *ACTIONS* into code suitable for execution.
   PROGRAM is now ready for execution."
  (cond
    ((null program) nil)
    ((constant? program)
     (decode-constant program))
    ((action? program)
     `(push ',program *program-actions*))
    ((listp program) (mapcar #'program-prepare program))
    (t program)))

(defun program-gather-sensor-data (arena x y)
  "Returns a list of *SENSORS* atoms. Returns only those atoms for which
   a corresponding obstacle has been detected in ARENA relative to positions X Y."
  (let ((sensor-data nil))
    (when (arena-get arena x (1- y)) (push 'n sensor-data))
    (when (arena-get arena (1+ x) (1- y)) (push 'ne sensor-data))
    (when (arena-get arena (1+ x) y) (push 'e sensor-data))
    (when (arena-get arena (1+ x) (1+ y)) (push 'se sensor-data))
    (when (arena-get arena x (1+ y)) (push 's sensor-data))
    (when (arena-get arena (1- x) (1+ y)) (push 'sw sensor-data))
    (when (arena-get arena (1- x) y) (push 'w sensor-data))
    (when (arena-get arena (1- x) (1- y)) (push 'nw sensor-data))
    sensor-data))

(defun program-execute (program arena x y)
  "Accepts PROGRAM, ARENA, and X Y coordinates.
   Program reads sensor data at X Y and is executed to completion.
   A list of atoms of type *ACTIONS* is returned. This list corresponds
   to all actions taken by PROGRAM during execution. Only the first
   of these actions is important, but all actions are collected here to make
   execution simpler and to verify that program code is always valid."
  (let* ((sensor-data (program-gather-sensor-data arena x y))
	 (program-data (program-apply-sensor-data program sensor-data))
	 (program-prepared (program-prepare program-data)))
    (setf *program-actions* nil)
    ;(debug-message "Executing program: ~a~%at (~a ~a)~%" program-prepared x y)
    ;; EVAL of PROGRAM pushes atoms of type *ACTIONS* into *program-actions* list
    (eval program-prepared)
    (reverse *program-actions*)))

(defun gene-children (function)
  "Defines the number of children a FUNCTION can have."
  (case function
    (and 2)
    (or 2)
    (not 1)
    (if 3)
    (otherwise 0)))

(defmacro get-gene (genes gene)
  "Fetches a GENE in GENES and ensures that GENE is not invalid."
  `(let ((result (find ,gene ,genes)))
     (if (null result)
	 (error (format nil "Invalid gene: ~a in: ~a" ,gene ,genes))
	 result)))

(defun decode-constant (constant)
  "Converts a numeric boolean expression into Lisp equivalent."
  (ecase constant
    (1 t)
    (0 nil)))

(defun get-sensor (sensor)
  (get-gene *sensors* sensor))

(defun get-function (function)
  (get-gene *functions* function))

(defun get-actions (action)
  (get-gene *actions* action))

(defun get-constant (constant)
  (decode-constant
   (get-gene *constants* constant)))

(defun sensor? (gene)
  (member gene *sensors*))

(defun function? (gene)
  (member gene *functions*))

(defun action? (gene)
  (member gene *actions*))

(defun constant? (gene)
  (member gene *constants*))

(defmacro random-gene (genes)
  "Obtains random gene atom in a given list of genes."
  `(elt ,genes (+ (* (random (/ (length ,genes) 2)) 2) 1)))

(defun random-sensor ()
  (random-gene *sensors*))

(defun random-function ()
  (random-gene *functions*))

(defun random-action ()
  (random-gene *actions*))

(defun random-constant ()
  (random-gene *constants*))

(defun random-child (&optional (options *random-gene-generators*))
  (funcall (elt options (random (length options)))))

(defun random-child-non-function ()
  (random-child (remove 'random-function *random-gene-generators*)))

(defun build-program-children (parent &optional disable-size-limit)
  "Given PARENT gene, generate the appropriate CHILDREN gene list and return (PARENT CHILDREN)."
  (let* ((num-children (gene-children parent))
	 (children ()))
      (dotimes (i num-children)
	(setf children (append children (list (build-program-exec parent disable-size-limit)))))
      (append (list parent) children)))

(defun build-program-exec (&optional parent disable-size-limit)
  "Generates a genetic program randomly and recursively."
  (incf *program-size*)
  (when (> *program-size* *program-size-factor*)
    (setf disable-size-limit t))
  (cond
    ((null parent) 
     (let* ((parent (random-function)))
       (build-program-children parent)))
    ((and (not disable-size-limit)
	  (function? parent))
     (let ((child (random-child)))
       (if (or disable-size-limit
	       (function? child))
	   (build-program-children child disable-size-limit)
	   child)))
    (t (random-child-non-function))))

(defun build-program ()
  "Initializes genetic program generation."
  (setf *program-size* 0)
  (build-program-exec))

(defun build-first-generation (population-size)
  "Return PROGRAMS, a list of POPULATION-SIZE randomly generated genetic programs."
  (let ((programs ()))
    (dotimes (i population-size)
      (setf programs (append (list (build-program)) programs)))
    programs))

(defun count-function-genes (program)
  "Returns the number of genes of type *FUNCTIONS* in PROGRAM."
  (cond
    ((function? program) 1)
    ((atom program) 0)
    (t (apply #'+ (mapcar #'count-function-genes program)))))

(defun count-primitives (program)
  "Returns the number of genes in PROGRAM."
  (cond
    ((atom program) 1)
    ((null program) 0)
    (t (apply #'+ (mapcar #'count-primitives program)))))
 
(defun extract-random-genes-exec (program index)
  "Extracts random gene subtree from PROGRAM."
  (when (and (listp program) (function? (car program)))
    (progn
      (incf *gene-count*)
      (if (eq index *gene-count*)
	  program
	  (mapcan #'(lambda (x) (extract-random-genes-exec x index)) program)))))

(defun extract-random-genes (program)
  "Initiates extraction of random gene subtree from PROGRAM."
  (let* ((function-count (count-function-genes program))
	 (index (1+ (random function-count))))
    (setf *gene-count* 0)
    (extract-random-genes-exec program index)))

(defun replace-random-genes-preprocess (program index)
  "Marks the gene subtree at INDEX for replacement."
  (let ((replacement-ok (atom program)))
    (when replacement-ok
      (incf *gene-count*))
    (cond
      ((null program) nil)
      ((and replacement-ok (= *gene-count* index)) 'gene-replacement)
      ((listp program) (mapcar #'(lambda(x) (replace-random-genes-preprocess x index)) program))
      (t program))))

(defun replace-random-genes-finalize (program replacement)
  "Replaces the marked gene subtree in PROGRAM with REPLACEMENT."
  (cond
    ((null program) nil)
    ((and (atom program) (eq program 'gene-replacement)) replacement)
    ((and (listp program) (eq (first program) 'gene-replacement)) replacement)
    ((listp program) (mapcar #'(lambda(x) (replace-random-genes-finalize x replacement)) program))
    (t program)))

(defun replace-random-genes (program replacement)
  "Main gene replacement function used during crossover operation."
  (let* ((primitives-count (count-primitives program))
	 (index (1+ (random primitives-count))))
    (setf *gene-count* 0)
    ;(debug-message "~%count: ~a index: ~a~%" primitives-count index)
    (let* ((result-preprocessed (replace-random-genes-preprocess program index))
	   (result-finalized (replace-random-genes-finalize result-preprocessed replacement)))
      (debug-message "program:      ~a" program)
      (debug-message "genes:        ~a" replacement)
      (debug-message "preprocessed: ~a" result-preprocessed)
      (debug-message "finalized:    ~a" result-finalized)
      result-finalized)))

(defun crossover (father mother)
  "Random genes are extracted from FATHER and replace a random gene subtree in MOTHER."
  (let ((father-genes (extract-random-genes father)))
    (replace-random-genes mother father-genes)))

(defun mutator (mother)
  "A random subtree in MOTHER is replaced by program MUTATION."
  (let ((mutation (build-program)))
    (replace-random-genes mother mutation)))

(defun calculate-displacement (action position-current)
  "Accepts an *ACTIONS* atom ACTION and POSITION-CURRENT in form (X Y).
   calculates displacement from POSITION-CURRENT based on type of ACTION."
  (let ((x (first position-current))
	(y (second position-current)))
  (ecase action
    (north (list x (1- y)))
    (east (list (1+ x) y))
    (south (list x (1+ y)))
    (west (list (1- x) y)))))

(defun collision-detected? (position)
  "Returns T if a program at POSITION has collided with an obstacle."
  (arena-get *arena* (first position) (second position)))

(defun following-wall? (position)
  "Returns T if program at POSITION is adjacent to an obstacle."
  (let* ((x (first position))
	 (y (second position))
	 (neighbor-locations 
	  (list (list x (1- y)) (list (1+ x) (1- y)) 
		(list (1+ x) y) (list (1+ x) (1+ y)) 
		(list x (1+ y)) (list (1- x) (1+ y)) 
		(list (1- x) y) (list (1- x) (1- y)))))
    (some #'(lambda (r) (eq t r)) 
	  (mapcar #'(lambda (p)
		      (let ((x (first p))
			    (y (second p)))
			(arena-get *arena* x y)))
		  neighbor-locations))))

(defun tournament (program tournament-trials)
  "Runs PROGRAM through TOURNAMENT-TRIALS 
   tournament trials and returns the total fitness
   of the program."
  (let ((positions (program-initial-positions tournament-trials)))
    (apply #'+ (mapcar #'(lambda (p) (tournament-trial program p)) positions))))

(defun tournament-trial (program position)
  "Accepts PROGRAM genes and an initial POSITION in (X Y) format.
   Places PROGRAM through a trial on *ARENA* and 
   returns the resulting FITNESS score."
  (let ((fitness-max *arena-wall-adjacencies*)
	(fitness-current 0)
	(iterations 60)
	(collision nil)
	(action nil)
	(visited nil)
	(oscillation nil)
	(stuck nil))
    (debug-message "TOURNAMENT TRIAL: ~a @ ~a" program position)
    (dotimes (i iterations)
      (unless (or collision stuck oscillation)
	(setf action (first (program-execute program *arena* (first position) (second position))))
	(if action
	    (progn
	      (debug-message "action: ~a~%start: (~a ~a)" action (first position) (second position))
	      (setf position (calculate-displacement action position))
	      (debug-message "end: (~a ~a)~%" (first position) (second position))
	      (setf collision (collision-detected? position))
	      (if (member position visited :test 'equal)
		  (setf oscillation t)
		  (push position visited))
	      (when (and (not collision)
			 (not oscillation)
			 (following-wall? position)
			 (< fitness-current fitness-max))
		(incf fitness-current)))
	    (setf stuck t))))
    fitness-current))

(defun tournament-selection-competitors (population number-competitors)
  "Generates NUMBER-COMPETITORS programs from POPULATION, a list of programs."
  (let ((competitors nil))
    (dotimes (i number-competitors)
      (let ((index (random (length population))))
	(push (elt population index) competitors)))
    competitors))

(defun program-fitness-sorter (program1 program2)
  "Predicate used for sorting the programs according to fitness."
  (> (first program1) (first program2)))
		   
(defun tournament-selection (population number-competitors tournament-trials)
  "Selects NUMBER-COMPETITORS individuals from POPULATION and submits them to a tournament.
   The fittest individual is returned."
  (let ((programs (tournament-selection-competitors population number-competitors)))
    (cadr
     (sort 
      (mapcar #'(lambda (p) 
		  (let ((fitness (tournament p tournament-trials)))
		    (list fitness p)))
	      programs)
      #'program-fitness-sorter))))

(defun build-tournament-generation (population number-competitors tournament-trials tournament-size)
  "Builds a list GENERATION of size TOURNAMENT-SIZE 
   consisting of the winners of TOURNAMENT-SIZE tournaments."
  (let ((generation nil))
    (dotimes (i tournament-size)
      (debug-message "BUILD-TOURNAMENT-GENERATION: ~a~%" i)
      (push (second (tournament-selection population number-competitors tournament-trials)) generation))
    generation))

(defun mutation? (mutation-rate)
  "Returns T with a probability of mutation-rate / 100."
  (<= (random 101) mutation-rate))	

(defun build-crossover-generation (population number-competitors tournament-trials crossover-size mutation-rate)
  "Builds a list GENERATION of CROSSOVER-SIZE consisting 
   of individuals that have been subjected to crossover and, possibly, mutation."
  (let ((generation nil))
    (dotimes (i crossover-size)
      (debug-message "BUILD-CROSSOVER-GENERATION: ~a~%" i)
      (let* ((mother (second (tournament-selection population number-competitors tournament-trials)))
	     (father (second (tournament-selection population number-competitors tournament-trials)))
	     (offspring (crossover father mother)))
	(push (if (mutation? mutation-rate) (mutator offspring) offspring) generation)))
    generation))

(defun population-contains-fit-individual (population tournament-trials fitness)
  "Evaluates a population POPULATION to see if it contains an individual of FITNESS fitness.
   Returns the fit individual, if it exists, along with the fittest individual and its corresponding
   fitness as well as the average fitness of POPULATION."
  (let ((fit-individual nil)
	(total-fitness 0)
	(current-fitness 0)
	(fittest-individual nil)
	(fittest-individual-fitness 0))
    (dolist (program population)
      (setf current-fitness (tournament program tournament-trials))
      (incf total-fitness current-fitness)
      (when (and (= fitness current-fitness) (null fit-individual))
	(setf fit-individual program))
      (when (> current-fitness fittest-individual-fitness)
	(progn
	  (setf fittest-individual program)
	  (setf fittest-individual-fitness current-fitness))))
    (values
     fit-individual
     fittest-individual
     fittest-individual-fitness
     (float (/ total-fitness (length population))))))

(defun build-new-generation (population tournament-size crossover-size number-competitors tournament-trials mutation-rate)
  "Builds a new generation consisting of TOURNAMENT-SIZE and CROSSOVER-SIZE individuals."
  (let* ((generation-tournament 
	  (build-tournament-generation population number-competitors tournament-trials tournament-size))
	 (generation-crossover 
	  (build-crossover-generation population number-competitors tournament-trials crossover-size mutation-rate))
	 (generation-new (append generation-tournament generation-crossover)))
    generation-new))
	    
(defun gp (&optional population-size tournament-size crossover-size number-competitors)
  "Main driver function for genetic programming algorithm."
  (let* ((population-size (if population-size population-size *population-size*))
	 (tournament-size (if tournament-size tournament-size *tournament-size*))
	 (crossover-size (if crossover-size crossover-size *crossover-size*))
	 (number-competitors (if number-competitors number-competitors *tournament-competitors*))
	 (tournament-trials *tournament-trials*)
	 (mutation-rate *mutation-rate*)
	 (target-fitness *target-fitness*)
	 ;(population (append (build-first-generation population-size) (list *program-successful*)))
	 (population (build-first-generation population-size))
	 (program-fit nil)
	 (population-counter 0))
    (init)
    (loop do
	 (format t "Generation ~a:~%Calculating Fitness...~%" (incf population-counter)) 
	 (force-output)
	 (multiple-value-bind (fit-individual fittest-individual fittest-individual-fitness fitness-average)
	     (population-contains-fit-individual population tournament-trials target-fitness)
	   (setf program-fit fit-individual)
	   (format t "Fitness Average: ~a~%Fittest Individual: ~a~%Fittest Individual Fitness: ~a~%" 
		   fitness-average fittest-individual fittest-individual-fitness) 
	   (force-output))
	 (when (null program-fit)
	   (format t "Building new generation...~%~%") 
	   (force-output)
	   (setf population 
		 (build-new-generation 
		  population 
		  tournament-size 
		  crossover-size 
		  number-competitors
		  tournament-trials
		  mutation-rate)))
       while
	 (null program-fit))
    program-fit))
