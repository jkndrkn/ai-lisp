#|

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

game.lisp - Defines functionality related to the Numbrix user-interactive game.

|#

(defun print-user-help-message (msg)
  "Displays help message to user."
  (format t "~%~a~%~%" msg) 
  (prompt-read-enter))

(defun print-user-help-messages ()
  "Displays help message to user."
  (mapcar #'(lambda (msg) (print-user-help-message msg)) *user-help-message*))

(defun print-user-moves (moves &optional (format "~3d. X:~3d   Y:~3d   Value:~3d~%"))
  "Displays a record of all moves a user has made during the course of the game."
  (let ((movenum 0))
    (mapcar 
     #'(lambda (m) 
	(format t format (incf movenum) (first m) (second m) (third m)))
     (reverse moves))))

(defun prompt-read-enter ()
  "Simple prompt used for regulating and adding clarity to gameplay."
  (prompt-read "Press ENTER to continue"))

(defun print-user-error-msg (msg)
  "Prints MSG and waits for user confirmation before returning to game play."
  (format t "~%~a~%~%" msg)
  (prompt-read-enter))

(defun prompt-yes-no (msg)
   (let ((response (string-downcase (prompt-read (concatenate 'string msg " (yes/no) ")))))
      (find response '("y" "ye" "yes") :test 'string=)))

(defun print-board-menu ()
  "Generates a user-friendly game board definition selection list."
  (dotimes (i (length *boardname-list*))
    (format t "~2d. ~a~%" (1+ i) (elt *boardname-list* i))))
  
(defun board-menu-choice-invalid? (choice)
  "Returns T if user supplies an invalid board menu choice."
  (let ((min-val 1)
	(max-val (length *boardname-list*)))
    (or (not (integerp choice)) (range-invalid? min-val max-val choice))))

(defun board-menu ()
  "Main driver function for board menu construction, display, and user input retrieval."
  (let ((choice-index nil)
	(choice-raw "")
	(choice-value "")
	(board-not-loaded t)
	(user-command nil))
    (load-boardname-list)
    (loop do
	 (progn
	   (format t "~%")
	   (print-board-menu)
	   (format t "~%")
	   (setf choice-raw (prompt-read "BOARD"))
	   (setf choice-index (parse-integer choice-raw :junk-allowed t))
	   (setf user-command (user-command-parse choice-raw))
	   (if user-command
	       (progn
		 (setf user-command (user-command-handle user-command))
		 (when (eq user-command (getf *program-flow* :exit))
		   (setf board-not-loaded nil)))
	       (progn
		 (if (board-menu-choice-invalid? choice-index)
		     (print-user-error-msg "-_- Invalid option! -_-")
		     (progn
		       (setf choice-value (elt *boardname-list* (1- choice-index)))
		       (load-board (make-pathname :directory `(:relative ,*board-path*) :name choice-value))
		       (setf board-not-loaded nil))))))
       while 
       board-not-loaded)
    user-command))

(defun convert-user-coord-x (x)
  "Converts a user-friendly coordinate specification to a value 
   that can be used to access the positions of a two-dimensional list or vector."
  (1- x))

(defun convert-user-coord-y (y)
  "Converts a user-friendly coordinate specification to a value 
   that can be used to access the positions of a two-dimensional list or vector."
  (- *size* y))

(defun user-move-invalid? (move)
  "Returns error message if a user supplies input that does not specify a valid move.
   Returns NIL otherwise."
  (let ((x-user (first move))
	(y-user (second move))
	(value (third move))
	(min-value 1)
	(max-value *num-squares*)
	(min-coord 1)
	(max-coord *size*))
    (cond 
      ((some #'null (list x-user y-user value))
       "Input must consist of three integers separated by spaces: \"X Coordinate\" \"Y Coordinate\" \"Value\".")
      ((notevery #'integerp (list x-user y-user value))
       "Grid coordinates and cell value must be integers.")
      (t 
	 (let ((x (convert-user-coord-x x-user))
	       (y (convert-user-coord-y y-user)))
	   (cond
	     ((move-coord-invalid? x-user y-user min-coord max-coord)
	      (format nil "Grid coordinates must be no smaller than ~a and no greater than ~a." min-coord max-coord))
	     (t
	      (let ((position-default (get-pos *board-default-positions* x y)))
		(cond
		  (position-default
		   (let ((value-default (get-pos *board* x y)))
		     (format nil "The value ~a at ~a ~a cannot be changed" value-default x-user y-user)))
		  ((range-invalid? min-value max-value value)
		   (format nil "Please enter an integer value between ~a and ~a." min-value max-value)))))))))))

(defun prompt-read (prompt)
  "Accepts and returns command-line input from a user."
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun string-split (string &optional (splitter " "))
  "Splits STRING on SPLITTER and returns values in list form."
  (let ((string (string-trim splitter string))
	(values ())
	(buffer ""))
    (dotimes (i (length string))
      (let ((c (elt string i)))
	(if (string= c splitter)
	    (progn
	      (when (> (length buffer) 0)
		(push buffer values))
	      (setf buffer ""))
	    (setf buffer (format nil "~a~a" buffer c)))
	(when (= i (1- (length string)))
	  (push buffer values))))
    (reverse values)))

(defun move-string-to-integer (move)
  "Converts a list of integer strings into a list of integers."
  (mapcar #'(lambda (x) (parse-integer x :junk-allowed t)) move))

(defun user-command-parse (move)
  "Accepts user input and returns an atom if user supplies a command 
   recognized by the system. Returns NIL otherwise."
  (let ((move-clean (string-downcase (string-trim '(#\Space #\Tab #\Newline) move))))
    (cond
      ((string= move-clean "quit")
       (getf *user-commands* :quit))
      ((string= move-clean "exit")
       (getf *user-commands* :quit))
      ((string= move-clean "help")
       (getf *user-commands* :help)))))

(defun user-command-handle (command)
  "Handles user commands that interrupt gameplay flow."
  (ecase command
    (quit 
     (getf *program-flow* :exit))
    (help
     (progn
       (print-user-help-messages)
       (getf *program-flow* :continue)))))

(defun user-move-prompt (prompt)
  "Main driver function for accepting and reacting to a user-specified move."
  (let* ((move (prompt-read prompt))
	 (user-command (user-command-parse move))
	 (move-list ())
	 (move-converted ())
	 (move-invalid nil)
	 (program-flow (getf *program-flow* :handle-move)))
    (when user-command
      (setf program-flow (user-command-handle user-command)))
    (cond ((eq program-flow (getf *program-flow* :handle-move))
	   (progn
	     (setf move-list (string-split move))
	     (setf move-converted (move-string-to-integer move-list))
	     (setf move-invalid (user-move-invalid? move-converted))
	     (cond
	       (move-invalid
		(progn
		  (print-user-error-msg move-invalid)
		  (getf *program-flow* :invalid-move)))
	       (t
		move-converted))))
	  (t
	   program-flow))))

(defun execute-user-move (move)
  "Applies a validated and sanitized use move to the global game state."
  (let ((x (convert-user-coord-x (first move)))
	(y (convert-user-coord-y (second move)))
	(value (third move)))
    (set-pos *board* x y value)))

(defun program-flow-decode (move test)
  "Reduces verbosity of calls to global program flow information."
  (eq move (getf *program-flow* test)))

(defun numbrix-game-win ()
  "Handles functionality corresponding to a successfully completed user game session."
  (print-board-pretty *board*)
  (format t "~%^_^ You win! ^_^~%~%")
  (prompt-read "Press ENTER to view a list of your moves")
  (format t "~%")
  (print-user-moves *user-moves*)
  (format t "~%")
  (when (prompt-yes-no "Do you wish to play again?")
    (progn
      (format t "~%")
      (numbrix))
    nil))

(defun numbrix-continue? (move)
  "Main driver function for handling game termination conditions including
   completion of board and early termination of game. Also prompts to restart game."
  (let ((exit (program-flow-decode move :exit))
	(win (not (board-invalid? *board*))))
    (cond
      (exit nil)
      (win (numbrix-game-win))
      (t t))))

(defun numbrix ()
  "Main driver function for the interactive Numbrix game session."
  (let ((move nil))
    (print-user-help-messages)
    (setf move (board-menu))
    (unless (eq move (getf *program-flow* :exit))
      (loop do
	   (progn
	     (print-board-pretty *board*)
	     (setf move (user-move-prompt "NUMBRIX"))
	     (when (listp move)
	       (progn
		 (push move *user-moves*)
		 (execute-user-move move))))
	 while 
	   (numbrix-continue? move)))))
