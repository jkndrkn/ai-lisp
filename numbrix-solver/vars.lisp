#|

John David Eriksen
http://www.jkndrkn.com
john.eriksen@yahoo.com

vars.lisp - Global variables and parameters used throughout the system.

|#

(defvar *debug-mode* nil
  "Set to T to enable debugging messages.")

(defparameter *board* nil
  "Two-dimensional vector used to represent the game board.")

(defparameter *board-default-positions* nil
  "Two-dimensional vector used to mark cells occupied by starting values.")

(defparameter *board-position-types* nil
  "Two-dimensional vector used to mark the position type of each square.")

(defparameter *board-path* "boards"
  "Path to a directory containing game board definitions.")

(defparameter *boardname-list* nil
  "List of board game board definition file names.")

(defparameter *boardname-definitions* "boards.lisp"
  "Path to a file containing a list of game board definition file names.")

(defparameter *user-moves* nil
  "List used to store a record of a user's moves.")

(defparameter *size* nil
  "Integer representing the length of the side of *board*.")

(defparameter *num-squares* nil
  "The number of squares in *board*.")

(defparameter *position-types*
  '(:inner inner :n n :ne ne :e e :se se :s s :sw sw :w w :nw nw)
  "Position categories that describe a given cell.
   Positions are used to identify potential neighbors.")

(defparameter *position-types-key*
  '(:inner :inner :n :n :ne :ne :e :e :se :se :s :s :sw :sw :w :w :nw :nw)
  "Alternative to *POSITION-TYPES*.
   Position categories that describe a given cell.
   Positions are used to identify potential neighbors.")

(defparameter *position-types-neighbors*
  '(:inner 4 :n 3 :ne 2 :e 3 :se 2 :s 3 :sw 2 :w 3 :nw 2)
  "Defines the number of neighbors a cell of given position type has.")

(defparameter *user-commands*
  '(:quit quit :help help)
  "Commands a user may supply that interrupt normal gameplay.")

(defparameter *program-flow*
  '(:exit exit :continue continue :handle-move handle-move :invalid-move invalid-move)
  "Actions that may result as a consequence to user inputs.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *neighbor-function-types*
    '(:n get-neighbor-n :e get-neighbor-e :s get-neighbor-s :w get-neighbor-w
      :ne get-neighbor-ne :se get-neighbor-se :sw get-neighbor-sw :nw get-neighbor-nw)
    "A list of neighbor-retrieval functions used in a defmacro expression."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *trivial-solution-function-types*
    '(:vert trivial-solution-vert :horz trivial-solution-horz
      :ne trivial-solution-ne :se trivial-solution-se
      :sw trivial-solution-sw :nw trivial-solution-nw
      :max trivial-solution-max :min trivial-solution-min)
    "A list of trivial-solution-solving functions used in a defmacro expression."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *find-adjacency-function-types*
    '(:n find-adjacency-n :e find-adjacency-e
      :s find-adjacency-s :w find-adjacency-w)
    "A list of endpoint-adjacency-finding functions used in a defmacro expression."))

(defparameter *user-help-message*
  '("^_^ WOW! NUMBRIX! ^_^

This game presents you with a grid. This grid contains squares that are either
blank or contain a number. It is your joy and privilege to fill in the blank squares!"

"The rules:

* You are trying to \"connect\" the numbers in the grid by filling in empty squares such that each square
  is neighbored by a number of value one greater and a number of value one less. This is true for all numbers
  except for the very first value (1) and the very last value (varies depending on the size of the board).

* Soon you will find that by connecting the numbers in order, you are building a sequence of consecutive
  numbers inside the grid!

* The numbers can only connect to neighbors that are directly above, below, to the left,
  and to the right of it. Diagonals don't count!

* You win when all empty square are filled and all the numbers are properly connected!"

"More rules: 

* After you've read this help message, you will be asked to select a board by giving the number of
  of the board in the list. Don't mess this up! The board prompt will look like this:

        BOARD:

* Once you've selected your board, you have entered the exciting world of Numbrix!

* When the input prompt reads NUMBRIX:, this is your cue to start filling in squares."

"* Fill in squares by giving NUMBRIX: a list of numbers like this:

        NUMBRIX: 2 1 4

* The first number is the X coordinate (1 is the leftmost column of squares).

* The second number is the Y coordinate (1 is the bottommost row of squares).

* The third number is the square value."

"Even more rules:

* To see this help message again, enter \"help\" (no quotes) at a BOARD or NUMBRIX prompt.

* To quit the game at any time, enter \"quit\" (no quotes) at the BOARD or NUMBRIX prompts.

^_^ ENJOY NUMBRIX! ^_^"))
