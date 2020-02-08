;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               baseball.lisp
;;;;
;;;;   Started:            Sun Oct 27 20:54:50 2019
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
;;;;   Notes:
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :baseball (:use :common-lisp :test) (:shadow :merge))

(in-package :baseball)

(defstruct game
  date
  (home 'bulldogs)
  visitor
  score
  next)

(defstruct score
  (home 0)
  (visitor 0))

(defstruct date
  year
  month 
  day
  day-of-week)

(defstruct (team (:constructor nil))
  name
  won
  lost
  average)

(defun make-team (&key name (won 0) (lost 0) average)
  (let ((team (make-instance 'team :name name :won won :lost lost)))
    (if (and (zerop won) (zerop lost))
        (setf (team-average team) 0)
        (setf (team-average team) (game-average team)))
    team))

;; (defmethod initialize-instance :after ((team team) &rest initargs)
;;   (print 'yo)
;;   (print (team-name team)))

(defun games-behind (a b)
  (/ (- (- (team-won b)
           (team-won a))
        (- (team-lost b)
           (team-lost a)))
     2))

(defun game-average (team)
  (coerce (/ (team-won team)
             (+ (team-won team) (team-lost team)))
          'double-float))

(defun inorderp (x y)
  (typecase x
    (number (< x y))
    (character (char< x y))
    (team (minusp (games-behind x y)))) )

(defun merge (l1 l2)
  "Merge two pre-sorted lists. Defined for lists of numbers and characters."
  (cond ((endp l1) l2)
        ((endp l2) l1)
        ((inorderp (first l1) (first l2)) (cons (first l1) (merge (rest l1) l2)))
        (t (cons (first l2) (merge l1 (rest l2)))) ))

(defun merge-sort (l)
  (labels ((merge-sort-aux (l l1 l2)
             (cond ((endp l) (merge (merge-sort l1) (merge-sort l2)))
                   (t (merge-sort-aux (rest l) (cons (first l) l2) l1)))) )
    (cond ((endp l) l)
          ((endp (rest l)) l)
          (t (merge-sort-aux l '() '())))) )

(defun print-standings (league)
  (let* ((ranking (merge-sort league))
         (top-team (first ranking)))
    (labels ((print-header ()
               (format t "Team ~12TWon Lost Average Games Behind~%"))
             (print-row (team)
               (format t "~A ~12T~A ~16T~A ~21T~4,3F ~29T~A~%" (team-name team) (team-won team) (team-lost team) (team-average team) (games-behind team top-team))))
      (print-header)
      (dolist (team ranking)
        (print-row team)))) )

(defvar *bulldogs* (make-team :name 'bulldogs :won 12 :lost 4))
(defvar *crimson* (make-team :name 'crimson :won 8 :lost 8))
(defvar *tigers* (make-team :name 'tigers :won 9 :lost 6))
(defvar *bears* (make-team :name 'bears :won 7 :lost 12))
(defvar *lions* (make-team :name 'lions :won 4 :lost 14))
(defvar *league* (list *bears* *lions* *crimson* *tigers* *bulldogs*))
