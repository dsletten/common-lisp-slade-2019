;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch03.lisp
;;;;
;;;;   Started:            Tue Jul 23 23:41:30 2019
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

(defpackage :ch03 (:use :common-lisp :test) (:shadow :pairlis))

(in-package :ch03)

;;;
;;;    3.14.4
;;;
(defun zero (elt)
  (if (and (numberp elt) (zerop elt))
      t
      elt))

;; (defun no-zeros (l)
;;   (remove t (mapcar #'zero l)))

(defun no-zeros (l)
  (remove-if #'(lambda (elt) (and (numberp elt) (zerop elt))) l))

(deftest test-no-zeros ()
  (check
   (equal (no-zeros '(1 0 2 0 3)) '(1 2 3))
   (equal (no-zeros '(a b c d e)) '(a b c d e))))

(defun collect-numbers (obj l)
  (if (numberp obj)
      (cons obj l)
      l))

(deftest test-collect-numbers ()
  (check
   (equal (collect-numbers 1 '(2 3 4 5)) '(1 2 3 4 5))
   (equal (collect-numbers 'a '(2 3 4 5)) '(2 3 4 5))))

(defvar *verb-list* '(is am are have has go went gone))
(defun lookup (word)
  (first (member word *verb-list*)))
;; (defun verb-find (sentence)
;;   (remove nil (mapcar #'lookup sentence)))

(defun verb-find (sentence)
  (remove-if-not #'(lambda (elt) (member elt *verb-list*)) sentence))

(deftest test-verb-find ()
  (check
   (equal (verb-find '(tom went to the store)) '(went))
   (equal (verb-find '(tom went to the store and mary went to town)) '(went went))
   (equal (verb-find '(have you gone to the store)) '(have gone))))

;;;
;;;    3.14.5
;;;
(defun proper-list-p (l)
  (and (listp l) (null (last l 0))))

(deftest test-proper-list-p ()
  (check
   (not (proper-list-p 'x))
   (not (proper-list-p 9))
   (proper-list-p '(a b c))
   (not (proper-list-p '(a b . c)))))

;;;
;;;    3.14.6
;;;
;; (defun last-atom (l)
;;   (if (listp l)
;;       (if (proper-list-p l)
;;           (first (last l))
;;           (rest (last l)))
;;       (error "Not a list: ~A" l)))
(defun evaluate (last)
  (if (null (rest last))
      (first last)
      (rest last)))
(defun last-atom (l)
  (evaluate (last l)))
(deftest test-last-atom ()
  (check
   (eq (last-atom '(a b c)) 'c)
   (eq (last-atom '(d e . f)) 'f)))

;;;
;;;    3.14.7
;;;
(defun pairlis (keys vals)
  (mapcar #'cons keys vals))

(deftest test-pairlis ()
  (check
   (equal (pairlis '(a b c) '(1 2 3)) '((a . 1) (b . 2) (c . 3)))
   (equal (pairlis '(a b c d) '(1 2 3)) '((a . 1) (b . 2) (c . 3)))
   (equal (pairlis '(a b c) '(1 2 3 4)) '((a . 1) (b . 2) (c . 3)))) )

;;;
;;;    3.14.8
;;;
(defun make-person (name age weight sex astrological-sign children)
  (pairlis '(name age weight sex astrological-sign children) (list name age weight sex astrological-sign children)))
;; (make-person 'joe 35 150 'male 'taurus '(irving mabel))

;; ((NAME . JOE) (AGE . 35) (WEIGHT . 150) (SEX . MALE)
;;  (ASTROLOGICAL-SIGN . TAURUS) (CHILDREN IRVING MABEL))
   
(setf (symbol-function 'value) #'cdr)
(defun get-name (person) (value (assoc 'name person)))
(defun get-age (person) (value (assoc 'age person)))
(defun get-weight (person) (value (assoc 'weight person)))
(defun get-sex (person) (value (assoc 'sex person)))
(defun get-astrological-sign (person) (value (assoc 'astrological-sign person)))
(defun get-children (person) (value (assoc 'children person)))

(defvar *joe* (make-person 'joe 35 150 'male 'taurus '(irving mabel)))

;; (dolist (key '(name age weight sex astrological-sign children))
;;   (format t "(defun get-~A (person) (value (assoc '~A person)))~%" key key))

;;;
;;;    3.14.9
;;;
;; (defun make-plist-person (name age weight sex astrological-sign children)
;;   (setf (symbol-plist name) (mapcan #'list '(name age weight sex astrological-sign children) (list name age weight sex astrological-sign children))))
(defun make-plist-person (name age weight sex astrological-sign children)
  (setf (symbol-plist name) (mapcan #'list '(age weight sex astrological-sign children) (list age weight sex astrological-sign children))))

;(defun get-plist-name (person) (get person 'name))
(defun get-plist-age (person) (get person 'age))
(defun get-plist-weight (person) (get person 'weight))
(defun get-plist-sex (person) (get person 'sex))
(defun get-plist-astrological-sign (person) (get person 'astrological-sign))
(defun get-plist-children (person) (get person 'children))

;;;
;;;    3.14.10
;;;
(make-plist-person 'joe 35 150 'male 'taurus '(irving mabel))
(make-plist-person 'irving 12 90 'male 'hokie '())
(make-plist-person 'mabel 10 75 'female 'pisces '())

(defun get-name+age (person)
  (list person (get-plist-age person)))

(defun age-of-children (person)
  (mapcar #'get-name+age (get-plist-children person)))

;;;
;;;    3.14.11
;;;
(defvar *months* (pairlis '(march april may june july august september october november december january february) '(1 2 3 4 5 6 7 8 9 10 11 12)))
(defvar *week-days* '(sunday monday tuesday wednesday thursday friday saturday))
(defun zeller (n m c y l)
  (mod (+ n (cl:floor (1- (* 13 m)) 5) y (cl:floor y 4) (cl:floor c 4) (- (* 2 c)) (- (* (1+ l) (cl:floor m 11))))
       7))
(defun leap-year-p (y)
  (cond ((zerop (mod y 100)) (zerop (mod y 400)))
        (t (zerop (mod y 4)))) )
(defun son-of-zeller (day month year)
  (zeller day month (truncate year 100) (mod year 100) (if (leap-year-p year) 1 0)))

(defun daughter-of-zeller (month day year)
  (nth (son-of-zeller day (rest (assoc month *months*)) year) *week-days*))

(deftest test-daughter-of-zeller ()
  (check
   (eq (daughter-of-zeller 'september 1 1996) 'sunday)
   (eq (daughter-of-zeller 'july 16 1999) 'friday)
   (eq (daughter-of-zeller 'february 24 1996) 'saturday)
   (eq (daughter-of-zeller 'july 24 2019) 'wednesday)))
