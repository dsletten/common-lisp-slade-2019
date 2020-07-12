;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               tic-tac-toe.lisp
;;;;
;;;;   Started:            Sun Jul  5 18:29:52 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   Fairly literal translation of JavaScript implementation (1999!).
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
;;;;   Notes:  The board is described by an array which represents a clockwise
;;;;  spiral starting in the upper-left corner. This corner is element 0; the
;;;;  center is element 8.
;;;;
;;;;   Turns alternate between human (even) and computer (odd). The game ends when one player
;;;;   (not the human!) wins or the board is filled.
;;;;
;;;;   LOGTEST vs. LOGAND???
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :tic-tac-toe (:use :common-lisp :test))

(in-package :tic-tac-toe)

(defconstant human-symbol #\X)
(defconstant computer-symbol #\O)
(defconstant empty-symbol #\Space)

;;;
;;;    This 3X3 array encodes the bit corresponding to each cell
;;;    E.g., upper left corner (0, 0) -> bit 0 (2 ^ 0)
;;;    Center (1, 1) -> bit 8 (2 ^ 8)
;;;    
(defconstant cells-array (make-array '(3 3) :initial-contents '((#b000000001 #b000000010 #b000000100)
                                                                (#b010000000 #b100000000 #b000001000)
                                                                (#b001000000 #b000100000 #b000010000))))

;;;
;;;    Encodings for two symbols in a row (or column/diagnoal).
;;;    If computer has two-in-a-row it should take the third to win.
;;;    Otherwise if human has two-in-a-row, computer should block.
;;;
;;;    Each element of the top-level vector contains patterns corresponding to the same
;;;    indexed cell. I.e., vector element 0 contains patterns that should occupy cell 0.
;;;    
(defconstant two-in-row #(#(#b000000110 #b011000000 #b100010000)
                          #(#b000000101 #b100100000)
                          #(#b000000011 #b000011000 #b101000000)
                          #(#b110000000 #b000010100)
                          #(#b001100000 #b000001100 #b100000001)
                          #(#b001010000 #b100000010)
                          #(#b000110000 #b010000001 #b100000100)
                          #(#b100001000 #b001000001)))

;;;
;;;    After the first two human moves, there are various strategies that are not necessarily obvious.
;;;    Missing one of these risks facing a crossfire in the next move that can't be blocked. If there
;;;    is no immediate two-in-a-row to block check for these more subtle cases.
;;;    
(defconstant sneak-attacks #(#(#b010000100 #b010000010 #b001000010)
                             #()
                             #(#b100010000 #b000001001 #b000001010 #b000010010)
                             #(#b000010001 #b001000100)
                             #(#b000100100 #b000101000 #b001001000)
                             #()
                             #(#b000100001 #b010100000 #b010010000)))

;;;
;;;    If computer (or human Ha!) has any of these three-in-a-row patterns filled game is won.
;;;    
(defconstant win-patterns #(#b000000111 #b110001000 #b001110000 #b011000001 #b100100010 #b000011100 #b101000100 #b100010001))

(defclass game ()
  ((board :initform (make-instance 'board))
   (turn :initform 0)
   (game-over-p :initform nil)))

(defclass board ()
  ((filled :initform 0 :documentation "Byte indicating which cells are filled.")
   (human :initform 0 :documentation "Byte indicating which cells are filled by human.")))

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
    (logxor human filled)))

(defun board-symbol (filled human i j)
  (let ((index (aref cells-array i j)))
    (if (logtest filled index)
        (if (logtest human index)
            human-symbol
            computer-symbol)
        empty-symbol)))

(defun print-divider (border stream)
  (format stream "~A-----------~A~%" border border))

;; (defun get-cells (g f)
;;   (with-slots (board) g
;;     (with-slots (filled) board
;;       (let ((cells '()))
;;         (dotimes (i 3 cells)
;;           (dotimes (j 3)
;;             (when (funcall f (aref contents i j))
;;               (push (list i j) cells)))) ))))

;; (defun filled-cells (g)
;;   (get-cells g #'filledp))

;; (defun human-cells (g)
;;   (get-cells g #'humanp))

;; (defun filledp (cell)
;;   (or (humanp cell) (computerp cell)))

;; (defun humanp (cell)
;;   (char= cell human-symbol))

;; (defun computerp (cell)
;;   (char= cell computer-symbol))

(defun user-turn-p (g)
  (with-slots (turn) g
    (evenp turn)))

(defun computer-turn-p (g)
  (not (user-turn-p g)))

(defun human-move (g i j)
  (with-slots (game-over-p board turn) g
    (with-slots (filled human) board
      (when (and (user-turn-p g) (not game-over-p))
        (let ((index (aref cells-array i j)))
          (cond ((logtest filled index) (warn "The cell has already been filled!"))
                (t (setf human (logior human index)
                         filled (logior filled index))
                   (incf turn))))
        (computer-move g))))
  g)

(defun computer-move (g)
  (with-slots (game-over-p board turn) g
    (with-slots (filled human) board
      (when (and (computer-turn-p g) (not game-over-p))
        (case turn
          (1 (if (not (logtest filled (aref cells-array 1 1))) ; Take center if available
                 (setf filled (logior filled (aref cells-array 1 1)))
                 (setf filled (logior filled (aref cells-array 0 0))))
             (incf turn))
          (3 (let ((move (or (two-in-row g :human) (sneak-attack g) (any-old-cell filled))))
               (setf filled (logior filled move))
               (incf turn)))
          (9 (format t "Tie game.~%") (setf game-over-p t)) ; Not possible for human to win on final move?
          (otherwise (cond ((winp g :human) 
                            (format t "You won!~%") ; Yeah, right...
                            (setf game-over-p t))
                           (t (let ((move (or (two-in-row g :computer) 
                                              (two-in-row g :human)
                                              (any-old-cell filled)))) 
                                (setf filled (logior filled move))
                                (cond ((winp g :computer)
                                       (format t "I won!~%")
                                       (setf game-over-p t))
                                      (t (incf turn)))) )))) ))))

(defun any-old-cell (filled)
  (let ((preferences '((0 0) (0 2) (2 2) (2 0) (0 1) (1 2) (2 1) (1 0))))
    (dolist (preference preferences)
      (destructuring-bind (i j) preference
        (unless (logtest filled (aref cells-array i j))
          (return (aref cells-array i j)))) )))

(defun index->cell (i)
  (values-list (aref #((0 0) (0 1) (0 2) (1 2) (2 2) (2 1) (2 0) (1 0) (1 1)) i)))

(defun two-in-row (g opponent)
  (labels ((find-two-in-row (player filled)
             (dotimes (cell-to-take 8 nil)
               (let ((patterns (aref two-in-row cell-to-take)))
                 (multiple-value-bind (i j) (index->cell cell-to-take)
                   (map nil #'(lambda (pattern)
                                (unless (logtest filled (aref cells-array i j)) ; Ignore if already filled
                                  (when (= pattern (logand player pattern))
                                    (return (aref cells-array i j)))) )
                        patterns)))) ))
    (with-slots (board) g
      (with-slots (filled human) board
        (ecase opponent
          (:human (find-two-in-row human filled))
          (:computer (find-two-in-row (logxor human filled) filled)))) )))

(defun sneak-attack (g)
  (with-slots (board) g
    (with-slots (filled human) board
      (dotimes (cell-to-block 7 nil)
        (let ((patterns (aref sneak-attacks cell-to-block)))
          (multiple-value-bind (i j) (index->cell cell-to-block)
            (map nil #'(lambda (pattern)
                         (unless (logtest filled (aref cells-array i j)) ; Ignore if already filled
                           (when (= pattern (logand human pattern))
                             (return (aref cells-array i j)))) )
                 patterns)))) )))

(defun winp (g opponent)
  "Did somebody win?"
  (labels ((find-winner (player)
             (map nil #'(lambda (pattern)
                          (when (= pattern (logand player pattern))
                            (return-from winp t)))
                        win-patterns)
             nil))
    (with-slots (board) g
      (with-slots (filled human) board
        (ecase opponent
          (:human (find-winner human))
          (:computer (find-winner (logxor human filled)))) ))))
