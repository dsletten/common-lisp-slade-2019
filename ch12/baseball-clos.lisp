;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               baseball-clos.lisp
;;;;
;;;;   Started:            Wed Oct 30 01:12:29 2019
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
(load "/Users/dsletten/lisp/packages/time.lisp")

(defpackage :baseball-clos (:use :common-lisp :test) (:shadow :merge))

(in-package :baseball-clos)

(defclass team ()
  ((name :initarg :name :reader name)
   (won :initarg :won :accessor won)
   (lost :initarg :lost :accessor lost)
   (average :reader average)))

(defmethod print-object ((team team) stream)
  (print-unreadable-object (team stream :type t)
    (format stream "~A ~D/~D [~4,3F]" (name team) (won team) (lost team) (average team))))

(defmethod initialize-instance :after ((team team) &rest initargs)
  (declare (ignore initargs))
  (set-average team))

(defun set-average (team)
  (unless (and (zerop (won team)) (zerop (lost team)))
    (with-slots (won lost average) team
      (setf average (compute-average won lost)))) )

(defmethod (setf won) :after (games (team team))
  (declare (ignore games))
  (set-average team))

(defmethod (setf lost) :after (games (team team))
  (declare (ignore games))
  (set-average team))

(defun compute-average (won lost)
  (coerce (/ won (+ won lost)) 'double-float))

(defclass game ()
  ((date :initarg :date :initform (make-instance 'time:date) :reader date)
   (home-team :initarg :home-team :reader home-team)
   (visitor :initarg :visitor :reader visitor)
   (score :initarg :score :reader score)
   (next :initarg :next :accessor next)))

(defmethod print-object ((g game) stream)
  (print-unreadable-object (g stream :type t)
    (format stream "~A vs. ~A (~A) ~A" (name (home-team g)) (name (visitor g)) (time:legal-date (date g)) (score g))))

(defclass score ()
  ((home-team :initarg :home-team :reader home-team)
   (visitor :initarg :visitor :reader visitor)))

(defmethod print-object ((s score) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "Home: ~D Visitor: ~D" (home-team s) (visitor s))))

;; (defclass date ()
;;   ((year :initarg :year :initform (time:get-year) :reader year)
;;    (month :initarg :month :initform (time:get-month) :reader month)
;;    (day :initarg :day :initform (time:get-day) :reader day)
;;    (day-of-week :reader day-of-week)))

;; (defmethod initializie-instance :after ((d date) &rest initargs)
;;   (declare (ignore initargs))
;;   (

(defgeneric games-behind (team1 team2))
(defmethod games-behind ((team1 team) (team2 team))
  (/ (- (- (won team2) (won team1))
        (- (lost team2) (lost team1)))
     2))

(defgeneric inorderp (obj1 obj2))
(defmethod inorderp ((x number) (y number)) (< x y))
(defmethod inorderp ((x character) (y character)) (char< x y))
(defmethod inorderp ((x team) (y team)) (minusp (games-behind x y)))
(defmethod inorderp ((x string) (y string)) (string< x y))
(defmethod inorderp ((x symbol) (y symbol)) (inorderp (symbol-name x) (symbol-name y)))
(defmethod inorderp ((x list) (y list))
  (cond ((endp y) nil) ; Strict inequality
        ((endp x) t)
  ;; (cond ((endp x) (not (endp y)))
  ;;       ((endp y) nil)
        ((inorderp (first x) (first y)) t)
        ((inorderp (first y) (first x)) nil)
        (t (inorderp (rest x) (rest y)))) )

; Mistakenly defined INORDERP for CONS, CONS (Doesn't handle NIL!)
; Redefined for LIST, LIST
; But CONS, CONS is more specific. Old version called until arg becomes NIL!!!!

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
               (format t "~A ~12T~A ~16T~A ~21T~4,3F ~29T~A~%" (name team) (won team) (lost team) (average team) (games-behind team top-team))))
      (print-header)
      (dolist (team ranking)
        (print-row team)))) )

(defvar *bulldogs* (make-instance 'team :name "Bulldogs" :won 12 :lost 4))
(defvar *crimson* (make-instance 'team :name "Crimson" :won 8 :lost 8))
(defvar *tigers* (make-instance 'team :name "Tigers" :won 9 :lost 6))
(defvar *bears* (make-instance 'team :name "Bears" :won 7 :lost 12))
(defvar *lions* (make-instance 'team :name "Lions" :won 4 :lost 14))
(defvar *league* (list *bears* *lions* *crimson* *tigers* *bulldogs*))

(deftest test-merge-sort ()
  (check
   (equal (merge-sort #1='(3 2 5 1)) (sort (copy-seq #1#) #'inorderp))
   (equal (merge-sort #2=(coerce "xad" 'list)) (sort (copy-seq #2#) #'inorderp))
   (equal (merge-sort #3='(*bears* *bulldogs* *crimson* *lions* *tigers*)) (sort (copy-seq #3#) #'inorderp))
   (equal (merge-sort #4='("one" "two" "three" "four" "five")) (sort (copy-seq #4#) #'inorderp))
   (equal (merge-sort #5='(one two three four five)) (sort (copy-seq #5#) #'inorderp))
   (equal (merge-sort #6='((3 4 5) (1 2) (1 2 3))) (sort (copy-seq #6#) #'inorderp))
   (equal (merge-sort #7='((x t c) (a d a m) (a c r i d))) (sort (copy-seq #7#) #'inorderp))))
