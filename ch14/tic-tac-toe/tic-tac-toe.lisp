;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               tic-tac-toe.lisp
;;;;
;;;;   Started:            Mon Mar  5 23:18:28 2012
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   This file contains both the standalone game that can be played against the computer with
;;;;   the hard-wired (perfect) strategies: (play-game)
;;;;
;;;;   It is also the infrastructure for the "learner" classes that gradually learn how not
;;;;   to lose:
;;;;   (defvar *l* (make-instance 'learner))
;;;;   (play *l*)
;;;;   Turn on diagnostics: (setf *diagnostics* t)
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes: CS Logo Style vol. 1 ch. 6
;;;;          Touretzky section 10.6
;;;;          Java TDD ch. 3, 6
;;;;
;;;;   Board is a 3X3 array, but the following cell indexes can be used
;;;;   with ROW-MAJOR-AREF to locate the correct array element:
;;;;  0 | 1 | 2  
;;;; -----------
;;;;  3 | 4 | 5  
;;;; -----------
;;;;  6 | 7 | 8
;;;;
;;;;    GAME keeps track of number of rounds played with rationals! 0, 1/2, 1, 3/2, ...
;;;;    Integers are human's turns.
;;;;    

(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :tic-tac-toe (:use :common-lisp :lang :test))

(load "/home/slytobias/lisp/books/Slade/ch14/2012/learner.lisp")
(load "/home/slytobias/lisp/books/Slade/ch14/2012/symmetric-learner.lisp")
(load "/home/slytobias/lisp/books/Slade/ch14/2012/hash-learner.lisp")
(load "/home/slytobias/lisp/books/Slade/ch14/2012/symmetric-hash-learner.lisp")

(in-package :tic-tac-toe)

(defvar *diagnostics* nil)

;;;
;;;    Only for learners, not standalone game.
;;;    
(defun report (&rest msgs)
  (when *diagnostics*
    (if (stringp (first msgs))
        (apply #'format t (first msgs) (rest msgs))
        (format t "Diagnostic:~{ ~A~}~%" msgs))))

(defconstant human-char #\X)
(defconstant computer-char #\O)

;;;
;;;    Game contains a board with dimensions ROWS X COLUMNS
;;;    
(defclass game ()
  ((board)
   (round :reader game-round :initform 0)
   (rows :reader rows :initform 3)
   (columns :reader columns :initform 3)))

;;;
;;;    No way to make other than a 3X3 game???
;;;    
(defun make-game ()
  (make-instance 'game))

(defmethod initialize-instance :after ((g game) &key)
  (with-slots (board) g
    (setf board (make-array (list (rows g) (columns g)) 
                            :initial-element #\space 
                            :element-type 'character))))

;; (defmethod print-object ((g game) stream)
;;   (print-board (slot-value g 'board) stream))

;; (defun print-board (board &optional (stream t))
;;   (dotimes (i 3)
;;     (dotimes (j 3)
;;       (unless (zerop j)
;;         (format stream "|"))
;;       (format stream " ~A " (aref board i j)))
;;     (format stream "~%")
;;     (unless (= i 2)
;;       (format stream "-----------~%"))))

(defmethod print-object ((g game) stream)
  (dotimes (i (rows g))
    (unless (zerop i)
      (format stream "-----------~%"))
    (dotimes (j (columns g))
      (unless (zerop j)
        (format stream "|"))
      (format stream " ~C " (board-contents g i j)))
    (format stream "~%")))

;;;
;;;    Standalone game. Human vs. pre-defined strategies.
;;;    
(defun play-game ()
  (labels ((play-move (game player move)
             (make-move game player move)
             (let ((game-over (game-over-p game)))
               (when (or game-over (eq player :computer))
                 (print game))
               (when game-over
                 (return-from play-game game-over)))) )
    (let ((game (make-game)))
      (loop
         (play-move game :human (get-human-move game))
         (play-move game :computer (get-computer-move game)))) ))

(defun get-human-move (game)
  "Prompt user for index of remaining empty cell."
  (get-num "Enter move: " :test #'(lambda (n) (and (integerp n) (<= 0 n 8) (emptyp game n)))) )

(defun get-computer-move (game)
 (or (fill-2-in-a-row game :computer)
     (defend-2-in-a-row game :human)
     (detect-sneak-attack game)
     (take-center game)
     (take-corner game)
     (make-random-move game)))

(defun make-move (game player position)
  (with-slots (board round) game
    (assert (emptyp game position) () "~A tried to play ~D." player position)
    (setf (row-major-aref board position) (player-char player))
    (incf round 1/2) ; Ha!
    game))

(defun emptyp (game position)
  (not (or (human-occupies-p game position)
           (computer-occupies-p game position))))

(defun game-over-p (game)
  (cond ((wins game :human) :human)
        ((wins game :computer) :computer)
        ((> (game-round game) 4) 'tie)
        (t nil)))

(defun wins (game player)
  (or (horizontal-win-p game player)
      (vertical-win-p game player)
      (diagonal-win-p game player)))

(defun horizontal-win-p (game player)
  (dotimes (i (rows game) nil)
    (when (3-out-of-3-filled-p player (collect-row i game))
      (return player))))

(defun vertical-win-p (game player)
  (dotimes (j (columns game) nil)
    (when (3-out-of-3-filled-p player (collect-column j game))
      (return player))))

(defun diagonal-win-p (game player)
  (cond ((or (3-out-of-3-filled-p player (collect-diagonal-1 game))
             (3-out-of-3-filled-p player (collect-diagonal-2 game)))
         player)
        (t nil)))

(defun take-center (game)
  (if (emptyp game 4)
      4
      nil))

(defun find-open-positions (game positions)
  (loop for elt in positions
        when (emptyp game elt)
        collect elt))

(defun take-corner (game)
  (take-random-position game (find-open-positions game '(0 2 6 8))))

(defun take-edge (game)
  (take-random-position game (find-open-positions game '(1 3 5 7))))

(defun make-random-move (game)
  (take-random-position game (find-open-positions game #[0 8])))

(defun take-random-position (game open-positions)
  (if (null open-positions)
      nil
      (elt open-positions (random (length open-positions)))) )

(defun detect-sneak-attack (game)
  (if (= (game-round game) 3/2)
      (or (sneak-attack-diagonal game)
          (sneak-attack-double-edge game)
          (sneak-attack-l game)
          (sneak-attack-opposite-corners game))
      nil))

;;  O |   |   
;; -----------
;;    | X |   
;; -----------
;;    |   | X
;;
;;  Human has center and corner opposite computer, otherwise would have triggered
;;  DEFEND-2-IN-A-ROW.
;;  
(defun sneak-attack-diagonal (game)
  (if (human-occupies-p game 4)
      (cond ((human-occupies-p game 0) 2)
            ((human-occupies-p game 2) 8)
            ((human-occupies-p game 6) 0)
            ((human-occupies-p game 8) 6)
            (t nil))
      nil))

;;    | X |   
;; -----------
;;  X | O |   
;; -----------
;;    |   |
;;
;; Computer opens with center when possible.
;; 
(defun sneak-attack-double-edge (game)
  (cond ((human-occupies-p game 1 5) 2)
        ((human-occupies-p game 1 3) 0)
        ((human-occupies-p game 7 5) 8)
        ((human-occupies-p game 7 3) 6)
        (t nil)))

;;  X |   |   
;; -----------
;;    | O |   
;; -----------
;;    | X |   
;;
;; Computer opens with center when possible.
;; 
(defun sneak-attack-l (game)
  (cond ((human-occupies-p game 0 7) 6)
        ((human-occupies-p game 2 7) 8)
        ((human-occupies-p game 0 5) 2)
        ((human-occupies-p game 6 5) 8)
        ((human-occupies-p game 6 1) 0)
        ((human-occupies-p game 8 1) 2)
        ((human-occupies-p game 2 3) 0)
        ((human-occupies-p game 8 3) 6)
        (t nil)))

;;  X |   |   
;; -----------
;;    | O |   
;; -----------
;;    |   | X
;;
;; Computer opens with center when possible.
;; 
(defun sneak-attack-opposite-corners (game)
  (if (or (human-occupies-p game 0 8)
          (human-occupies-p game 6 2))
      (take-edge game)
      nil))

(defun player-char (player)
  (ccase player
    (:human human-char)
    (:computer computer-char)))

(defun fill-2-in-a-row (game self)
  (or (find-2-in-row game self)
      (find-2-in-column game self)
      (find-2-in-diagonal game self)))

(defun defend-2-in-a-row (game opponent)
  (or (find-2-in-row game opponent)
      (find-2-in-column game opponent)
      (find-2-in-diagonal game opponent)))

(defun find-2-in-row (game player)
  (dotimes (i (rows game) nil)
    (let ((row-hole (2-out-of-3-filled-p player (collect-row i game))))
      (when row-hole
        (return (row-major-index i row-hole)))) ))

(defun collect-row (i game)
  (loop for j from 0 below (columns game)
        collect (board-contents game i j)))

(defun find-2-in-column (game player)
  (dotimes (j (columns game) nil)
    (let ((column-hole (2-out-of-3-filled-p player (collect-column j game))))
      (when column-hole
        (return (row-major-index column-hole j)))) ))

(defun collect-column (j game)
  (loop for i from 0 below (rows game)
        collect (board-contents game i j)))

(defun find-2-in-diagonal (game player)
  (let ((diagonal-1-hole (2-out-of-3-filled-p player (collect-diagonal-1 game)))
        (diagonal-2-hole (2-out-of-3-filled-p player (collect-diagonal-2 game))))
    (if diagonal-1-hole
        (row-major-index diagonal-1-hole diagonal-1-hole)
        (if diagonal-2-hole
            (row-major-index (- (rows game) diagonal-2-hole 1) diagonal-2-hole)
            nil))))

(defun collect-diagonal-1 (game)
  (loop for i from 0 below (rows game)
        collect (board-contents game i i)))

(defun collect-diagonal-2 (game)
  (loop for i from 0 below (rows game)
        collect (board-contents game (- (rows game) i 1) i)))

(defun 2-out-of-3-filled-p (player contents)
  "Are 2 of the 3 cells in the given row/column/diagonal filled exclusively by PLAYER? Return index of 3rd empty cell."
  (and (= (count (player-char player) contents) 2)
       (position #\space contents)))

(defun 3-out-of-3-filled-p (player contents)
  "Are all 3 of the cells in the given row/column/diagnoal filled by PLAYER? We have a winner."
  (every #'(lambda (ch) (char= (player-char player) ch)) contents))
  
(defun row-major-index (i j)
  (+ (* i 3) j))

(defun board-row-index (i)
  (floor i 3))

(defun board-column-index (i)
  (mod i 3))

(defun opposite-corner (corner)
  (case corner
    (0 8)
    (8 0)
    (2 6)
    (6 2)))

;;;
;;;    These must be returned in clockwise order. See corner-trainer.
;;;    
(defun edges-adjacent-to-corner (corner)
  (case corner
    (0 '(3 1))
    (2 '(1 5))
    (6 '(7 3))
    (8 '(5 7))))

(defun human-occupies-p (game &rest positions)
  (apply #'player-occupies-p game :human positions))

(defun computer-occupies-p (game &rest positions)
  (apply #'player-occupies-p game :computer positions))

(defun player-occupies-p (game player &rest positions)  
  (if (null positions)
      nil
      (every #'(lambda (position) (compare-position game player position)) positions)))

(defun compare-position (game player position)
  (char= (board-contents game position) (player-char player)))

(defun board-contents (game i &optional (j nil j-supplied-p))
  "Return contents of a cell on the board specified either as an index I or as array indices I, J."
  (if (not j-supplied-p)
      (board-contents game (board-row-index i) (board-column-index i))
      (aref (slot-value game 'board) i j)))

;;;
;;;    Clockwise
;;;    
;; (defun next-corner (corner)
;;   (case corner
;;     (0 2)
;;     (2 8)
;;     (8 6)
;;     (6 0)))

;; (defun previous-corner (corner)
;;   (case corner
;;     (0 6)
;;     (2 0)
;;     (8 2)
;;     (6 8)))

(defun next-corner (corner)
  (defchain corner (0 2 8 6)))

(defun previous-corner (corner)
  (defchain corner (0 6 8 2)))
