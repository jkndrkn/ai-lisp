#|

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

numbrix.lisp - Compiles and loads project source.

Compilation:

> (load (compile-file "numbrix.lisp"))

Numbrix User-interactive Game:

> (numbrix)

Numbrix Puzzle Solver:

> (solver)

Test Suite:

> (test-exec-call)

|#

(defparameter *source-files*
  (list "vars.lisp" "lib.lisp" "game.lisp" "solver.lisp" "test.lisp"))

(mapcar #'(lambda (file)
	    (load (compile-file file :verbose nil :print nil))) 
	*source-files*)
