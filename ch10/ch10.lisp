;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch10.lisp
;;;;
;;;;   Started:            Tue Sep 24 23:18:19 2019
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

(defpackage :zeller (:use :common-lisp :test))

(in-package :zeller)

;;;
;;;    2.11.11
;;;
(defun leap-year-p (y)
  (assert (typep y '(integer 1582)) (y) "Gregorian calendar not defined for: ~S" y)
  (cond ((zerop (mod y 100)) (zerop (mod y 400)))
        (t (zerop (mod y 4)))) )

(deftest test-leap-year-p ()
  (check
   (not (leap-year-p 1995))
   (leap-year-p 1996)
   (not (leap-year-p 1900))
   (leap-year-p 2000)))

;;;
;;;    3.14.11
;;;
(defvar *months* '(january february march april may june july august september october november december))
(defvar *zeller-index* '(11 12 1 2 3 4 5 6 7 8 9 10))
(defvar *month-map* (pairlis *months* *zeller-index*) "Map month names to their numeric values in terms of Zeller's congruence.")
(defvar *zeller-index-map* (let ((h (make-hash-table))) (loop for i from 0
                                                              for j in *zeller-index*
                                                              do (setf (gethash j h) i))
                                h)
  "Establish the mapping from Zeller month indices to conventional month order (0-11).")
(defvar *week-days* '(sunday monday tuesday wednesday thursday friday saturday))

(defun zeller (n m c y l)
  (mod (+ n (floor (1- (* 13 m)) 5) y (floor y 4) (floor c 4) (- (* 2 c)) (- (* (1+ l) (floor m 11))))
       7))

(defun zeller-index->index (zeller-index)
  (gethash zeller-index *zeller-index-map*))

(defun son-of-zeller (day zeller-month year)
  (assert (typep zeller-month '(integer 1 12)) (zeller-month) "Invalid value for month: ~S" zeller-month)
  (assert (typep year '(integer 1582)) (year) "Invalid year ~S (Gregorian calendar begins in 1582.)" year)
  (assert (typep day `(integer 1 ,(max-day-of-month (zeller-index->index zeller-month) year))) (day) "Invalid value for day: ~S" day)
  (zeller day zeller-month (truncate year 100) (mod year 100) (if (leap-year-p year) 1 0)))

(defun max-day-of-month (month year)
  "Determine the length of the given month (for a given year). 0-based month (0-11)."
  (ecase (1+ month)
    (2 (if (leap-year-p year) 29 28))
    ((4 6 9 11) 30)
    ((1 3 5 7 8 10 12) 31)))

(defun month->month-index (month)
  (position month *months*))

(defun month->zeller-index (month)
  (cdr (assoc month *month-map*)))

(defun daughter-of-zeller (month day year)
  (assert (typep month `(member ,@*months*)) (month) "Invalid month: ~S Choose: ~S" month *months*)
  (assert (typep year '(integer 1582)) (year) "Invalid year ~S (Gregorian calendar begins in 1582.)" year)
  (assert (typep day `(integer 1 ,(max-day-of-month (month->month-index month) year))) (day) "Invalid value for day: ~S" day)
  (nth (son-of-zeller day (month->zeller-index month) year) *week-days*))

(deftest test-daughter-of-zeller ()
  (check
   (eq (daughter-of-zeller 'september 1 1996) 'sunday)
   (eq (daughter-of-zeller 'july 16 1999) 'friday)
   (eq (daughter-of-zeller 'february 24 1996) 'saturday)
   (eq (daughter-of-zeller 'july 24 2019) 'wednesday)))


;;;
;;;    4.7.4
;;;    
(defpackage :make-change (:use :common-lisp :test))

(in-package :make-change)

(defconstant us-currency '((100 dollar)
                           (50 half-dollar)
                           (25 quarter)
                           (10 dime)
                           (5 nickel)
                           (1 penny pennies)))
(defun concatenate-symbol (s1 s2)
  (assert (symbolp s1) (s1) "Invalid symbol: ~S" s1)
  (assert (symbolp s2) (s2) "Invalid symbol: ~S" s2)
  (intern (format nil "~A~A" s1 s2)))

(defun pluralize (n singular plural)
  (assert (numberp n) (n) "Invalid number: ~S" n)
  (assert (symbolp singular) (singular) "Invalid symbol: ~S" singular)
  (assert (symbolp plural) (plural) "Invalid symbol: ~S" plural)
  (cond ((= n 1) singular)
        ((null plural) (concatenate-symbol singular 's))
        (t plural)))

(defun make-change (money currency-list)
  (assert (typep money '(integer 0)) (money) "Invalid monetary value: ~S" money)
  (assert (listp currency-list) (currency-list) "Invalid currency list: ~S" currency-list)
  (cond ((zerop money) '())
        (t (destructuring-bind ((value currency &optional plural) . tail) currency-list
             (if (>= money value)
                 (multiple-value-bind (m n) (truncate money value)
                   (cons (list m (pluralize m currency plural))
                         (make-change n tail)))
                 (make-change money tail)))) ))

(deftest test-make-change ()
  (check
   (equal (make-change 94 us-currency) '((1 HALF-DOLLAR) (1 QUARTER) (1 DIME) (1 NICKEL) (4 PENNIES)))
   (equal (make-change 372 us-currency) '((3 DOLLARS) (1 HALF-DOLLAR) (2 DIMES) (2 PENNIES)))) )

;;;
;;;    6.8.6
;;;
(defpackage :spell-correct (:use :common-lisp :test))

(in-package :spell-correct)

(defun string-swap (s i j)
  "Swap the two characters in string S located at indexes I and J."
  (assert (stringp s) (s) "~S should be a string." s)
  (assert (integerp i) (i) "Invalid integer ~S." i)
  (assert (typep i `(integer 0 (,(length s)))) (i) "Invalid index ~S in ~S." i s)
  (assert (typep j `(integer 0 (,(length s)))) (j) "Invalid index ~S in ~S." j s)
;;   (assert (typep i `(integer 0 (,(length s)))) (i) "Invalid index ~S in ~S." i s)
;;   (assert (typep j `(integer 0 (,(length s)))) (j) "Invalid index ~S in ~S." j s)
  (let ((result (copy-seq s)))
    (rotatef (char result i) (char result j))
    result))

(deftest test-string-swap ()
  (check
   (string= (string-swap "hello there" 3 5) "hel olthere")
   (string= (string-swap "hello there" 5 9) "hellorthe e")))

(defun string-insert (s ch i)
  "Insert character CH into string S at index I."
  (assert (stringp s) (s) "~S should be a string." s)
  (assert (characterp ch) (ch) "~S should be a character." ch)
  (assert (typep i `(integer 0 ,(length s))))
  (concatenate 'string (subseq s 0 i) (string ch) (subseq s i)))

(deftest test-string-insert ()
  (check
   (string= (string-insert "hello" #\X 0) "Xhello")
   (string= (string-insert "hello" #\X 1) "hXello")
   (string= (string-insert "hello" #\X 4) "hellXo")
   (string= (string-insert "hello" #\X 5) "helloX")))
