;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch04.lisp
;;;;
;;;;   Started:            Thu Jul 25 23:41:25 2019
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

(defpackage :ch04 (:use :common-lisp :test) (:shadow :reverse :append :nth :last :remove-duplicates))

(in-package :ch04)

;;;
;;;    4.7.1
;;;
(defun replicate (obj n)
  (check-type n (integer 0))
  (if (zerop n)
      '()
      (cons obj (replicate obj (1- n)))) )

(deftest test-replicate ()
  (check
   (equal (replicate 'a 10) (make-list 10 :initial-element 'a))))

(defun factorial (n)
  (check-type n (integer 0))
  (if (zerop n)
      1
      (* n (factorial (1- n)))) )

(defun loop-factorial (n)
  (check-type n (integer 0))
  (reduce #'* (loop for i from 1 to n collect i)))

(deftest test-factorial ()
  (check
   (= (factorial 0) (loop-factorial 0))
   (= (factorial 5) (loop-factorial 5))
   (= (factorial 20) (loop-factorial 20))))


;;;
;;;    4.7.3
;;;

;;
;;    Original
;;    
;; (defun make-change (money)
;;   (cond ((= money 0) '())
;;         ((< money 5) (list (list money 'penny)))
;;         ((< money 10) (multiple-value-bind (nickels remainder) (truncate money 5)
;;                         (cons (list nickels 'nickel) (make-change remainder))))
;;         ((< money 25) (multiple-value-bind (dimes remainder) (truncate money 10)
;;                         (cons (list dimes 'dime) (make-change remainder))))
;;         ((< money 50) (multiple-value-bind (quarters remainder) (truncate money 25)
;;                         (cons (list quarters 'quarter) (make-change remainder))))
;;         ((< money 100) (multiple-value-bind (half-dollars remainder) (truncate money 50)
;;                         (cons (list half-dollars 'half-dollar) (make-change remainder))))
;;         (t (multiple-value-bind (dollars remainder) (truncate money 100)
;;              (cons (list dollars 'dollar) (make-change remainder)))) ))

(defun pluralize (n singular plural)
  (if (= n 1)
      singular
      plural))

(defun make-change (money)
  (cond ((= money 0) '())
        ((< money 5) (list (list money (pluralize money 'penny 'pennies))))
        ((< money 10) (multiple-value-bind (nickels remainder) (truncate money 5)
                        (cons (list nickels 'nickel) (make-change remainder))))
        ((< money 25) (multiple-value-bind (dimes remainder) (truncate money 10)
                        (cons (list dimes (pluralize dimes 'dime 'dimes)) (make-change remainder))))
        ((< money 50) (multiple-value-bind (quarters remainder) (truncate money 25)
                        (cons (list quarters 'quarter) (make-change remainder))))
        ((< money 100) (multiple-value-bind (half-dollars remainder) (truncate money 50)
                        (cons (list half-dollars 'half-dollar) (make-change remainder))))
        (t (multiple-value-bind (dollars remainder) (truncate money 100)
             (cons (list dollars (pluralize dollars 'dollar 'dollars)) (make-change remainder)))) ))

;;;
;;;    4.7.4
;;;
(defconstant us-currency '((100 dollar)
                           (50 half-dollar)
                           (25 quarter)
                           (10 dime)
                           (5 nickel)
                           (1 penny pennies)))
(defun concatenate-symbol (s1 s2)
  (intern (format nil "~A~A" s1 s2)))

(defun pluralize (n singular plural)
  (cond ((= n 1) singular)
        ((null plural) (concatenate-symbol singular 's))
        (t plural)))

(defun make-change (money currency-list)
  (cond ((zerop money) '())
        (t (destructuring-bind ((value currency &optional plural) . tail) currency-list
             (if (>= money value)
                 (multiple-value-bind (m n) (truncate money value)
                   (cons (list m (pluralize m currency plural))
                         (make-change n tail)))
                 (make-change money tail)))) ))

;;;
;;;    4.7.5
;;;

;;
;;    Can't TRACE this!
;; (defun reverse (l)
;;   (labels ((reverse-aux (l result)
;;              (if (endp l)
;;                  result
;;                  (reverse-aux (rest l) (cons (first l) result)))) )
;;     (reverse-aux l '())))

(defun reverse (l)
  (reverse-aux l '()))
(defun reverse-aux (l result)
  (if (endp l)
      result
      (reverse-aux (rest l) (cons (first l) result))))

(deftest test-reverse ()
  (check
   (equal (reverse '()) (cl:reverse '()))
   (equal (reverse #1='(a)) (cl:reverse #1#))
   (equal (reverse #2='(a b)) (cl:reverse #2#))
   (equal (reverse #3='(a b c d)) (cl:reverse #3#))))

;;;
;;;    4.7.6
;;;
(defun append (l1 l2)
  (if (endp l1)
      l2
      (cons (first l1) (append (rest l1) l2))))

(deftest test-append ()
  (check
   (equal (append '() '(d e f)) (cl:append '() '(d e f)))
   (equal (append #1='(a) #2='(d e f)) (cl:append #1# #2#))
   (equal (append #3='(a b) #4='(d e f)) (cl:append #3# #4#))
   (equal (append #5='(a b c) #6='(d e f)) (cl:append #5# #6#))))

;;;
;;;    4.7.7
;;;
(defun nth (n l)
  (if (zerop n) ; Could short-circuit if L is empty...
      (first l)
      (nth (1- n) (rest l))))

(deftest test-nth ()
  (check
   (eq (nth 0 '(a b c d)) 'a)
   (eq (nth 1 '(a b c d)) 'b)
   (eq (nth 3 '(a b c d)) 'd)
   (eq (nth 4 '(a b c d)) nil)))

;;;
;;;    4.7.8
;;;    See notes pg. 52 (Or 2007 ch04.lisp)
(defun last (l &optional (n 1))
  (last-a l l n))

(defun last-a (l1 l2 n)
  (if (atom l2)
      (last-b l1 n)
      (last-a l1 (rest l2) (1- n))))

(defun last-b (l n)
  (if (minusp n)
      (last-b (rest l) (1+ n))
      l))

(deftest test-last ()
  (check
   (equal (last '(a b c d)) '(d))
   (equal (last '(a b c d) 1) '(d))
   (equal (last '(a b c d) 0) '())
   (equal (last '(a b c d) 2) '(c d))
   (equal (last '(a b c d) 3) '(b c d))
   (equal (last '(a b c d) 4) '(a b c d))
   (equal (last '(a b c d) 5) '(a b c d))
   (equal (last '(a b c . d)) '(c . d))
   (equal (last '(a b c . d) 1) '(c . d))
   (equal (last '(a b c . d) 0) 'd)
   (equal (last '(a b c . d) 2) '(b c . d))
   (equal (last '(a b c . d) 3) '(a b c . d))
   (equal (last '(a b c . d) 4) '(a b c . d))
   (equal (last '(a b c . d) 5) '(a b c . d))))

;;;
;;;    4.7.9
;;;
(defun remove-duplicates (l)
  (if (endp l)
      '()
      (adjoin (first l) (remove-duplicates (rest l)))) )

(deftest test-remove-duplicates ()
  (check
   (equal (remove-duplicates '(a b c d a b f g)) '(c d a b f g))
   (equal (remove-duplicates #1='(a b c d (a b) f g)) #1#)))

;;;
;;;    4.7.10
;;;
(defun creditp (obj)
  (and (numberp obj) (plusp obj)))

(defun debitp (obj)
  (and (numberp obj) (minusp obj)))

(defun interest-rate-p (obj)
  (and (listp obj)
       (numberp (first obj))
       (endp (rest obj))))

(defun rate (interest-rate)
  (first interest-rate))

(defun check-book (balance transactions)
  (check-type balance number)
  (check-type transactions list)
  (if (endp transactions)
      balance
      (destructuring-bind (transaction . rest) transactions
        (cond ((or (creditp transaction) (debitp transaction))
               (check-book (+ balance transaction) rest))
              ((interest-rate-p transaction)
               (check-book (* balance (rate transaction)) rest))
              (t (error "Unrecognized transaction: ~A" transaction)))) ))

(deftest test-check-book ()
  (check
   (= (check-book 100 '(100 50 -75)) 175)
   (= (check-book 100 '(-17.5 -1.73 -7.5)) 73.27)
   (= (check-book 100 '(100 50 -50 (1.1))) 220)
   (= (check-book 100 '((1.1) 100 50 -50 (1.1)))) ))

;;;
;;;    4.7.11
;;;
(defconstant now-account-minimum 500)
(defconstant penalty 0.1)
(defun now-account (balance transactions)
  (check-type balance number)
  (check-type transactions list)
  (if (endp transactions)
      balance
      (destructuring-bind (transaction . rest) transactions
        (cond ((creditp transaction) (now-account (+ balance transaction) rest))
              ((debitp transaction) (if (< balance now-account-minimum)
                                        (now-account (+ balance transaction (- penalty)) rest)
                                        (now-account (+ balance transaction) rest)))
              ((interest-rate-p transaction) (if (< balance now-account-minimum)
                                                 (now-account balance rest)
                                                 (now-account (* balance (rate transaction)) rest)))
              (t (error "Unrecognized transaction: ~A" transaction)))) ))

(deftest test-now-account ()
  (check
   (= (now-account 100 '(100 50 -50 (1.1))) 199.9)
   (= (now-account 500 '(100 50 -50 (1.1))) 660)))

;;;
;;;    4.7.12
;;;
(defun wild-card-p (obj)
  (eq obj '*wild*))
(defun matchp (pattern target)
  (cond ((endp pattern) (endp target))
        ((endp target) (and (wild-card-p (first pattern))
                            (matchp (rest pattern) target)))
        ((eq (first pattern) (first target))
         (matchp (rest pattern) (rest target)))
        ((wild-card-p (first pattern))
         (or (matchp (rest pattern) target)
             (matchp pattern (rest target))))
        (t nil)))

(deftest test-matchp ()
  (check
   (matchp '(a b c) '(a b c))
   (not (matchp '(a b c) '(a b c d)))
   (not (matchp '(a b c d) '(a b c)))
   (matchp '(a *wild*) '(a b c))
   (matchp '(a *wild*) '(a))
   (matchp '(a *wild* b) '(a b c d b))
   (not (matchp '(a *wild* b) '(a b c d e)))
   (matchp '(*wild* b *wild*) '(a b c d e))
   (matchp '(*wild*) '(a b c))
   (matchp '(*wild* *wild*) '())))

;;;
;;;    4.7.13
;;;
(defun count-occurrences (obj tree)
  (cond ((null tree) 0)
        ((atom tree) (if (eq obj tree) 1 0))
        (t (+ (count-occurrences obj (car tree))
              (count-occurrences obj (cdr tree)))) ))

(deftest test-count-occurrences ()
  (check
   (= (count-occurrences 'a '(a ((a b)) d c (a))) 3)
   (= (count-occurrences 'z '(a ((a b)) d c (a))) 0)))

;;;
;;;    4.7.14
;;;
(defun tree-addition (n tree)
  (cond ((null tree) '())
        ((atom tree) (check-type tree number)
         (+ n tree))
        (t (cons (tree-addition n (car tree))
                 (tree-addition n (cdr tree)))) ))

(deftest test-tree-addition ()
  (check
   (tree-equal (tree-addition 2 '(5 4 3 2 1)) '(7 6 5 4 3))
   (tree-equal (tree-addition 3 '(1 2 (3 (4 (5) 6) (7)) 8 (9))) '(4 5 (6 (7 (8) 9) (10)) 11 (12)))
   (tree-equal (tree-addition 5 '(((( (1) )))) ) '(((( (6) )))) )))

;;;
;;;    4.7.15
;;;
(defun compute (l)
  (/ (first l) (second l)))

(defun tree-average (tree)
  (compute (tree-average-aux tree 0 0)))

(defun tree-average-aux (tree sum count)
  (cond ((null tree) (list sum count))
        ((atom tree) (check-type tree number)
         (list (+ tree sum) (1+ count)))
        (t (process (tree-average-aux (car tree) sum count) (cdr tree)))) )

(defun process (l tree)
  (tree-average-aux tree (first l) (second l)))

(deftest test-tree-average ()
  (check
   (= (tree-average '(1 2 (3 (4 (5) 6) (7)) 8 (9))) 5)
   (= (tree-average '(((( ((1)) )))) ) 1)))




   