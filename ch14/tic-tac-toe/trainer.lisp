;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               trainer.lisp
;;;;
;;;;   Started:            Fri Mar 16 18:16:51 2012
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   This file defines classes of TRAINERs used to automate training of the LEARNERs.
;;;;   Aside from the top-level TRAINER class there are 5 concrete subclasses:
;;;;   HUMAN-TRAINER  - The default for a LEARNER's PLAY method. Relies on manually playing
;;;;                    a game against the LEARNER as in the standalone game.
;;;;   RANDOM-TRAINER - The least specialized automated trainer. Simply makes random moves against the LEARNER.
;;;;   CENTER-TRAINER -
;;;;   EDGE-TRAINER   -
;;;;   CORNER-TRAINER - 
;;;;   
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
;;;;   Notes:
;;;;
;;;;
(in-package :tic-tac-toe)

(defclass trainer () ())

(defgeneric get-move (trainer game))

;;;
;;;    Human trainer simply relies on human playing against the learner. (Default trainer)
;;;    
(defclass human-trainer (trainer) ())

(defmethod get-move ((trainer human-trainer) (game game))
  (get-human-move game))  

;;;
;;;    Tries to win if immediately possible. Blocks if immediately necessary. Otherwise just picks a random move.
;;;    
(defclass random-trainer (trainer) ())

(defmethod get-move ((trainer random-trainer) (game game))
  (or (fill-2-in-a-row game :human)
      (defend-2-in-a-row game :computer)
      (make-random-move game)))

(defclass center-trainer (trainer)
  ((moves :reader moves :initform (make-array 5))))

(defmethod get-move ((trainer center-trainer) (game game))
  (with-slots (moves) trainer
    (let ((round (game-round game))
          (move (or (fill-2-in-a-row game :human)
                    (defend-2-in-a-row game :computer)
                    (take-center game)
                    (make-random-move game))))
      (setf (aref moves round) move)
      move)))

(defclass edge-trainer (trainer)
  ((moves :reader moves :initform (make-array 5))))

(defmethod get-move ((trainer edge-trainer) (game game))
  (with-slots (moves) trainer
    (let* ((round (game-round game))
           (move (case round
                   (0 (aref [1 3 5 7] (random 4)))
                   (otherwise (or (fill-2-in-a-row game :human)
                                  (defend-2-in-a-row game :computer)
                                  (make-random-move game)))) ))
      (setf (aref moves round) move)
      move)))

;;; Makes illegal moves??
(defclass corner-trainer (trainer)
  ((moves :reader moves :initform (make-array 5))))

(defmethod get-move ((trainer corner-trainer) (game game))
  (with-slots (moves) trainer
    (let* ((round (game-round game))
           (move (case round
                   (0 (aref [0 2 6 8] (random 4)))
                   (1 (let ((previous-move (aref moves 0)))
                        (if (learner-in-opposite-corner-p game previous-move)
                            (prepare-corner-sneak-attack game previous-move)
                            (if (and (zerop (random 2)) (emptyp game 4)) 4 (opposite-corner previous-move)))) )
                   (2 (or (fill-2-in-a-row game :human)
                          (defend-2-in-a-row game :computer)
                          (corner-sneak-attack game moves)
                          (make-random-move game)))
                   (otherwise (or (fill-2-in-a-row game :human)
                                  (defend-2-in-a-row game :computer)
                                  (follow-up-corner-sneak-attack game (aref moves 0))
                                  (make-random-move game)))) ))
      (setf (aref moves round) move)
      move)))

(defun learner-in-opposite-corner-p (game corner)
  (compare-position game :computer (opposite-corner corner)))
  
(defun prepare-corner-sneak-attack (game previous-move)
  (declare (ignore game)) ; ??
  (case (random 2)
    (0 4)
    (1 (elt (edges-adjacent-to-corner (opposite-corner previous-move)) (random 2)))) )

(defun corner-sneak-attack (game moves)
  (let ((first-move (aref moves 0))
        (second-move (aref moves 1)))
    (cond ((= second-move (opposite-corner first-move)) (take-corner game)) ; We have opposite corners. Learner in center.
          ((= second-move 4)
           (let ((edges (edges-adjacent-to-corner first-move))) ;Anything else would force blocking 2 in a row
             (if (computer-occupies-p game (first edges))
                 (next-corner first-move)
                 (previous-corner first-move))))
          ((case first-move
             (0 (cond ((and (= second-move 7)
                            (or (computer-occupies-p game 3)
                                (computer-occupies-p game 6)))
                       1)
                      ((and (= second-move 5)
                            (or (computer-occupies-p game 1)
                                (computer-occupies-p game 2)))
                       3)
                      (t nil)))
             (2 (cond ((and (= second-move 7)
                            (or (computer-occupies-p game 5)
                                (computer-occupies-p game 8)))
                       1)
                      ((and (= second-move 3)
                            (or (computer-occupies-p game 0)
                                (computer-occupies-p game 1)))
                       5)
                      (t nil)))
             (6 (cond ((and (= second-move 1)
                            (or (computer-occupies-p game 0)
                                (computer-occupies-p game 3)))
                       7)
                      ((and (= second-move 5)
                            (or (computer-occupies-p game 7)
                                (computer-occupies-p game 8)))
                       3)
                      (t nil)))
             (8 (cond ((and (= second-move 1)
                            (or (computer-occupies-p game 2)
                                (computer-occupies-p game 5)))
                       7)
                      ((and (= second-move 3)
                            (or (computer-occupies-p game 6)
                                (computer-occupies-p game 7)))
                       5)
                      (t nil)))) )
          (t nil))))

(defun follow-up-corner-sneak-attack (game first-move)
  (cond ((human-occupies-p game (next-corner first-move)) (print 'a) (previous-corner first-move)) ; <-- bug
        ((human-occupies-p game (previous-corner first-move)) (print 'b) (next-corner first-move)) ; <-- bug
        (t (let ((edges (edges-adjacent-to-corner first-move)))
             (cond ((computer-occupies-p game (first edges)) (print 'c) (second edges)) ; <-- bug
                   ((computer-occupies-p game (second edges)) (print 'd) (first edges)) ; <-- bug
                   (t (take-center game)))) )))
