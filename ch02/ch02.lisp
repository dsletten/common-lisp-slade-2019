;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Sat Jul 20 02:43:46 2019
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

(defpackage :ch02 (:use :common-lisp :test) (:shadow :signum :floor :ceiling))

(in-package :ch02)

;;;
;;;    2.11.2
;;;    
(defun add2 (x)
  (+ x 2))

(deftest test-add2 ()
  (check
   (= (add2 5) 7)
   (= (add2 -12) -10)
   (= (add2 0.4) 2.4)))

(defun add5 (x)
  (+ x 5))

(deftest test-add5 ()
  (check
   (= (add5 5) 10)
   (= (add5 -12) -7)
   (= (add5 0.4) 5.4)))

(defun double (x)
  (* x 2))

(deftest test-double ()
  (check
   (= (double 5) 10)
   (= (double -12) -24)
   (= (double 0.4) 0.8)))

(defun min-abs4 (a b c d)
  (min (abs a) (abs b) (abs c) (abs d)))

(deftest test-min-abs4 ()
  (check
   (= (min-abs4 3 5 -2 -8) 2)))

(defun min-abs (&rest args)
  (apply #'min (mapcar #'abs args)))

(deftest test-min-abs ()
  (check
   (= (min-abs 3) 3)
   (= (min-abs 3 5 -2) 2)
   (= (min-abs 3 5 -2 -8) 2)
   (= (min-abs 3 5 -2 -8 0.1) 0.1)))

(defun max-abs4 (a b c d)
  (max (abs a) (abs b) (abs c) (abs d)))

(deftest test-max-abs4 ()
  (check
   (= (max-abs4 3 5 -2 -8) 8)))

(defun max-abs (&rest args)
  (apply #'max (mapcar #'abs args)))

(deftest test-max-abs ()
  (check
   (= (max-abs 3) 3)
   (= (max-abs 3 5 -2) 5)
   (= (max-abs 3 5 -2 -8) 8)
   (= (max-abs 3 5 -2 -8 99) 99)))

;;;
;;;    2.11.3
;;;
;; (defun ajoutez (x y)
;;   (+ x y))

(defun ajoutez (&rest numbers)
  (apply #'+ numbers))

(deftest test-ajoutez ()
  (check
   (= (ajoutez 2 5) (+ 2 5))
   (= (ajoutez 1 2 3) (+ 1 2 3))))

(defun retranchez (&rest numbers)
  (apply #'- numbers))

(deftest test-retranchez ()
  (check
   (= (retranchez 7 2) (- 7 2))
   (= (retranchez 99 5 4) (- 99 5 4))))

(defun hochstmas (n &rest ms)
  (apply #'max n ms))

(deftest test-hochstmas ()
  (check
   (= (hochstmas 4 7) (max 4 7))
   (= (hochstmas 2) (max 2))
   (= (hochstmas 2 4) (max 2 4))
   (= (hochstmas 2 4 3) (max 2 4 3))))

(defun multiplizieren (&rest numbers)
  (apply #'* numbers))

(deftest test-multiplizieren ()
  (check
   (= (multiplizieren 5 3) (* 5 3))
   (= (multiplizieren 5) (* 5))
   (= (multiplizieren 5 3 8 9) (* 5 3 8 9))))

(defun njia-ya-kutokea ()
  (sb-ext:exit))

;;;
;;;    2.11.4
;;;
(defun zeller (n m c y l)
  (mod (+ n (cl:floor (1- (* 13 m)) 5) y (cl:floor y 4) (cl:floor c 4) (- (* 2 c)) (- (* (1+ l) (cl:floor m 11))))
       7))

(deftest test-zeller ()
  (check
   (= (zeller 24 12 19 96 1) 6)
   (= (zeller 16 5 19 99 0) 5)
   (= (zeller 1 7 19 96 1) 0)
   (= (zeller 2 7 19 96 1) 1)
   (= (zeller 3 7 19 96 1) 2)))

;;;
;;;    2.11.5
;;;
(defun signum (x)
  (or (and (plusp x) 1)
      (and (minusp x) -1)
      (and (zerop x) 0)))

(deftest test-signum ()
  (check
   (= (signum 5) 1)
   (= (signum -23.67891) -1)
   (= (signum (+ 1 -1)) 0)))

(defun interest-rate (money)
  (or (and (<= money 0) 0)
      (and (< money 1000) 2)
      (and (< money 10000) 5)
      (and (< money 100000) 7)
      10))

(deftest test-interest-rate ()
  (check
   (= (interest-rate 99) 2)
   (= (interest-rate 5000000) 10)
   (= (interest-rate 0) 0)
   (= (interest-rate -2) 0)))

;;;
;;;    2.11.7
;;;
(defun go-to-movie-p (age cash)
  (cond ((< age 12) (> cash 3.0))
        ((and (>= age 12) (< age 65)) (> cash 7.0))
        ((>= age 65) (> cash 4.5))))

(deftest test-go-to-movie-p ()
  (check
   (go-to-movie-p 8 4.0)
   (not (go-to-movie-p 16 4.0))))

;;;
;;;    2.11.10
;;;
(defun floor (x)
  (if (minusp x)
      (if (zerop (rem x 1))
          (- (truncate (- x)))
          (1- (- (truncate (- x)))) )
      (truncate x)))

(defun ceiling (x)
  (- (floor (- x))))

(deftest test-floor ()
  (check
   (= (floor 3.0) (cl:floor 3.0))
   (= (floor 2.9) (cl:floor 2.9))
   (= (floor 2.1) (cl:floor 2.1))
   (= (floor 0.0) (cl:floor 0.0))
   (= (floor -3.0) (cl:floor -3.0))
   (= (floor -2.9) (cl:floor -2.9))
   (= (floor -2.1) (cl:floor -2.1))))

(deftest test-ceiling ()
  (check
   (= (ceiling 3.0) (cl:ceiling 3.0))
   (= (ceiling 2.9) (cl:ceiling 2.9))
   (= (ceiling 2.1) (cl:ceiling 2.1))
   (= (ceiling 0.0) (cl:ceiling 0.0))
   (= (ceiling -3.0) (cl:ceiling -3.0))
   (= (ceiling -2.9) (cl:ceiling -2.9))
   (= (ceiling -2.1) (cl:ceiling -2.1))))

;;;
;;;    2.11.11
;;;
(defun leap-year-p (y)
  (cond ((zerop (mod y 100)) (zerop (mod y 400)))
        (t (zerop (mod y 4)))) )

(deftest test-leap-year-p ()
  (check
   (not (leap-year-p 1995))
   (leap-year-p 1996)
   (not (leap-year-p 1900))
   (leap-year-p 2000)))

;;;
;;;    2.11.12
;;;
(defun son-of-zeller (day month year)
  (zeller day month (truncate year 100) (mod year 100) (if (leap-year-p year) 1 0)))

(deftest test-son-of-zeller ()
  (check
   (= (son-of-zeller 24 12 1996) 6)
   (= (son-of-zeller 16 5 1999) 5)
   (= (son-of-zeller 1 7 1996) 0)
   (= (son-of-zeller 2 7 1996) 1)
   (= (son-of-zeller 3 7 1996) 2)))
