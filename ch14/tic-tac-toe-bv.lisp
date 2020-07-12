;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               tic-tac-toe-bv.lisp
;;;;
;;;;   Started:            Fri Jul 10 01:47:55 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
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
;;;;   Notes: Bit vector implementation of the JavaScript Tic-Tac-Toe game
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :tic-tac-toe-bv (:use :common-lisp :test))

(in-package :tic-tac-toe-bv)

;;;;   Example:
;;;;
;;;;   Notes:  The board is described by an array which represents a clockwise
;;;;  spiral starting in the upper-left corner. This corner is element 0; the
;;;;  center is element 8.
;;;;
;;;;   Turns alternate between human (even) and computer (odd). The game ends when one player
;;;;   (not the human!) wins or the board is filled.
;;;;
;;;;   LOGTEST vs. LOGAND???
;;;;

(defconstant human-symbol #\X)
(defconstant computer-symbol #\O)
(defconstant empty-symbol #\Space)

;;;
;;;    This 3X3 array encodes the bit corresponding to each cell
;;;    E.g., upper left corner (0, 0) -> #*100000000
;;;    Center (1, 1) -> #*000000001
;;;    
(defconstant cells-array (make-array '(3 3) :initial-contents '((#*100000000 #*010000000 #*001000000)
                                                                (#*000000010 #*000000001 #*000100000)
                                                                (#*000000100 #*000001000 #*000010000))))

;;;
;;;    Encodings for two symbols in a row (or column/diagnoal).
;;;    If computer has two-in-a-row it should take the third to win.
;;;    Otherwise if human has two-in-a-row, computer should block.
;;;
;;;    Each element of the top-level vector contains patterns corresponding to the same
;;;    indexed cell. I.e., vector element 0 contains patterns that should occupy cell 0.
;;;    
(defconstant two-in-row #(#(#*011000000 #*000000110 #*000010001)
                          #(#*101000000 #*000001001)
                          #(#*110000000 #*000110000 #*000000101)
                          #(#*000000011 #*001010000)
                          #(#*000001100 #*001100000 #*100000001)
                          #(#*000010100 #*010000001)
                          #(#*000011000 #*100000010 #*001000001)
                          #(#*000100001 #*100000100)))


;;;
;;;    After the first two human moves, there are various strategies that are not necessarily obvious.
;;;    Missing one of these risks facing a crossfire in the next move that can't be blocked. If there
;;;    is no immediate two-in-a-row to block check for these more subtle cases.
;;;    
(defconstant sneak-attacks #(#(#*001000010 #*010000010 #*010000100)
                             #()
                             #(#*000010001 #*100100000 #*010100000 #*010010000)
                             #(#*100010000 #*001000100)
                             #(#*001001000 #*000101000 #*000100100)
                             #()
                             #(#*100001000 #*000001010 #*000010010)))

;;;
;;;    If computer (or human Ha!) has any of these three-in-a-row patterns filled game is won.
;;;    
(defconstant win-patterns #(#*111000000 #*000100011 #*000011100 #*100000110 #*010001001 #*001110000 #*001000101 #*100010001))

(defclass game ()
  ((board :initform (make-instance 'board) :reader board)
   (turn :initform 0 :reader turn)
   (game-over-p :initform nil :reader game-over-p)))

(defclass board ()
  ((filled :initform (make-sequence 'bit-vector 9 :initial-element 0) :documentation "Bit vector indicating which cells are filled.")
   (human :initform (make-sequence 'bit-vector 9 :initial-element 0) :documentation "Bit vector indicating which cells are filled by human.")))

(defun make-game ()
  (make-instance 'game))

(defmethod print-object ((g game) stream)
  (with-slots (board game-over-p) g
    (print-board board game-over-p stream)))

(defun print-board (b game-over-p stream)
  (when game-over-p
    (format stream "*************~%"))
  (let ((border (if game-over-p
                    "*"
                    "")))
    (print-row b 0 border stream)
    (print-divider border stream)
    (print-row b 1 border stream)
    (print-divider border stream)
    (print-row b 2 border stream))
  (when game-over-p
    (format stream "*************~%")))

(defun print-row (b i border stream)
  (with-slots (filled human) b
    (format stream "~A ~C | ~C | ~C ~A~%" border (board-symbol filled human i 0) (board-symbol filled human i 1) (board-symbol filled human i 2) border)))

(defun computer-cells (g)
  (with-slots (filled human) g
    (bit-xor human filled)))

(defun board-symbol (filled human i j)
  (let ((index (aref cells-array i j)))
    (if (filledp filled index)
        (if (equal (bit-and human index) index)
            human-symbol
            computer-symbol)
        empty-symbol)))

(defun print-divider (border stream)
  (format stream "~A-----------~A~%" border border))

(defun user-turn-p (g)
  (with-slots (turn) g
    (evenp turn)))

(defun computer-turn-p (g)
  (not (user-turn-p g)))

(defun filledp (filled target)
  (equal (bit-and filled target) target))

(defun human-move (g i j)
  (with-slots (game-over-p board turn) g
    (with-slots (filled human) board
      (when (and (user-turn-p g) (not game-over-p))
        (let ((index (aref cells-array i j)))
          (cond ((filledp filled index) (warn "The cell has already been filled!"))
                (t (bit-ior human index t)
                   (bit-ior filled index t)
                   (incf turn))))
        (computer-move g))))
  g)

(defun computer-move (g)
  (with-slots (game-over-p board turn) g
    (with-slots (filled human) board
      (when (and (computer-turn-p g) (not game-over-p))
        (case turn
          (1 (if (not (filledp filled (aref cells-array 1 1))) ; Take center if available
                 (bit-ior filled (aref cells-array 1 1) t)
                 (bit-ior filled (aref cells-array 0 0) t))
             (incf turn))
          (3 (let ((move (or (two-in-row g :human) (sneak-attack g) (any-old-cell filled))))
               (bit-ior filled move t)
               (incf turn)))
          (9 (format t "Tie game.~%") (setf game-over-p t)) ; Not possible for human to win on final move?
          (otherwise (cond ((winp g :human) 
                            (format t "You won!~%") ; Yeah, right...
                            (setf game-over-p t))
                           (t (let ((move (or (two-in-row g :computer) 
                                              (two-in-row g :human)
                                              (any-old-cell filled)))) 
                                (bit-ior filled move t)
                                (cond ((winp g :computer)
                                       (format t "I won!~%")
                                       (setf game-over-p t))
                                      (t (incf turn)))) )))) ))))

(defun any-old-cell (filled)
  (let ((preferences '((0 0) (0 2) (2 2) (2 0) (0 1) (1 2) (2 1) (1 0))))
    (dolist (preference preferences)
      (destructuring-bind (i j) preference
        (unless (filledp filled (aref cells-array i j))
          (return (aref cells-array i j)))) )))

(defun index->cell (i)
  (values-list (aref #((0 0) (0 1) (0 2) (1 2) (2 2) (2 1) (2 0) (1 0) (1 1)) i)))

(defun two-in-row (g opponent)
  (labels ((find-two-in-row (player filled)
             (dotimes (cell-to-take 8 nil)
               (let ((patterns (aref two-in-row cell-to-take)))
                 (multiple-value-bind (i j) (index->cell cell-to-take)
                   (map nil #'(lambda (pattern)
                                (unless (filledp filled (aref cells-array i j))
                                  (when (equal pattern (bit-and player pattern))
                                    (return (aref cells-array i j)))) )
                        patterns)))) ))
    (with-slots (board) g
      (with-slots (filled human) board
        (ecase opponent
          (:human (find-two-in-row human filled))
          (:computer (find-two-in-row (bit-xor human filled) filled)))) )))

(defun sneak-attack (g)
  (with-slots (board) g
    (with-slots (filled human) board
      (dotimes (cell-to-block 7 nil)
        (let ((patterns (aref sneak-attacks cell-to-block)))
          (multiple-value-bind (i j) (index->cell cell-to-block)
            (map nil #'(lambda (pattern)
                         (unless (filledp filled (aref cells-array i j))
                           (when (equal pattern (bit-and human pattern))
                             (return (aref cells-array i j)))) )
                 patterns)))) )))

(defun winp (g opponent)
  "Did somebody win?"
  (labels ((find-winner (player)
             (map nil #'(lambda (pattern)
                          (when (equal pattern (bit-and player pattern))
                            (return-from winp t)))
                        win-patterns)
             nil))
    (with-slots (board) g
      (with-slots (filled human) board
        (ecase opponent
          (:human (find-winner human))
          (:computer (find-winner (bit-xor human filled)))) ))))

