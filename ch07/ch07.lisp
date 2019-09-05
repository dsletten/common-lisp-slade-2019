;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch07.lisp
;;;;
;;;;   Started:            Sat Aug 10 00:49:04 2019
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

(defpackage :ch07 (:use :common-lisp :test))

(in-package :ch07)

;;;
;;;    7.9.1
;;;
(defun column-print (strings column stream)
  (unless (endp strings)
    (format stream "~VT~A~%" column (first strings))
    (column-print (rest strings) column stream)))

;;;
;;;    7.9.2
;;;    (Same as MAKE-MATRIX-2A in ch07/old/matrix.lisp)
;;;
(defun split-list-old (l n)
;  "Split up list L into N (nearly) even rows."
  "Split up list L into (nearly) even rows of N!"
  (labels ((split (l i)
             (cond ((endp l) (list '()))
                   ((zerop i) (cons '() (split l n)))
                   (t (let ((result (split (rest l) (1- i))))
                        (cons (cons (first l) (first result))
                              (rest result)))) )))
    (split l n)))

;;;
;;;    Aux function rather than aux variable.
;;;    (Same as LIST-TO-COLUMNS in ch07/old/matrix.lisp)
;;;    
(defun split-list-2 (l n)
  "Split up list L into (nearly) even rows of N!"
  (labels ((split (l i)
             (cond ((endp l) (list '()))
                   ((zerop i) (cons '() (split l n)))
                   (t (split-aux (first l) (split (rest l) (1- i)))) ))
           (split-aux (elt result)
             (cons (cons elt (first result))
                   (rest result))))
    (split l n)))

;;;
;;;    SPLIT-LIST-2 broken in two
;;;    
(defun split-1 (l i n)
  (cond ((endp l) (list '()))
        ((zerop i) (cons '() (split-1 l n n)))
        (t (split-2 (first l) (split-1 (rest l) (1- i) n)))) )

(defun split-2 (elt result)
  (cons (cons elt (first result))
        (rest result)))

;;;
;;;    This transposes the original results of the list split up.
;;;    
(defun split-list (l n)
  "Split up list L into N (nearly) even columns."
  (labels ((split (l i)
             (cond ((endp l) (list '()))
                   ((zerop i) (cons '() (split l n)))
                   (t (split-aux (first l) (split (rest l) (1- i)))) ))
           (split-aux (elt result)
             (cons (cons elt (first result))
                   (rest result))))
    (transpose (split l n))))

(deftest test-split-list-old ()
  (check
   (equal (split-list-old '(1 2 3 4 5 6) 2) '((1 2) (3 4) (5 6)))
   (equal (split-list-old '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6)))
   (equal (split-list-old '(a b c d e) 3) '((a b c) (d e)))
   (equal (split-list-old '(1 2 3 4 5 6 7 8 9 10 11 12) 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))
   (equal (split-list-old '(1 2 3 4 5 6 7 8 9 10 11 12) 3) '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equal (split-list-old '(1 2 3 4 5 6 7 8 9 10 11 12) 4) '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
   (equal (split-list-old '(1 2 3 4 5 6 7 8 9 10 11 12) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12)))
   (equal (split-list-old '(1 2 3 4 5 6 7 8 9 10 11 12) 5) '((1 2 3 4 5) (6 7 8 9 10) (11 12)))) )

(deftest test-split-1 ()
  (check
   (equal (split-1 '(1 2 3 4 5 6) 2 2) '((1 2) (3 4) (5 6)))
   (equal (split-1 '(1 2 3 4 5 6) 3 3) '((1 2 3) (4 5 6)))
   (equal (split-1 '(a b c d e) 3 3) '((a b c) (d e)))
   (equal (split-1 '(1 2 3 4 5 6 7 8 9 10 11 12) 2 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))
   (equal (split-1 '(1 2 3 4 5 6 7 8 9 10 11 12) 3 3) '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equal (split-1 '(1 2 3 4 5 6 7 8 9 10 11 12) 4 4) '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
   (equal (split-1 '(1 2 3 4 5 6 7 8 9 10 11 12) 6 6) '((1 2 3 4 5 6) (7 8 9 10 11 12)))
   (equal (split-1 '(1 2 3 4 5 6 7 8 9 10 11 12) 5 5) '((1 2 3 4 5) (6 7 8 9 10) (11 12)))) )

(deftest test-split-list-2 ()
  (check
   (equal (split-list-2 '(1 2 3 4 5 6) 2) '((1 2) (3 4) (5 6)))
   (equal (split-list-2 '(1 2 3 4 5 6) 3) '((1 2 3) (4 5 6)))
   (equal (split-list-2 '(a b c d e) 3) '((a b c) (d e)))
   (equal (split-list-2 '(1 2 3 4 5 6 7 8 9 10 11 12) 2) '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))
   (equal (split-list-2 '(1 2 3 4 5 6 7 8 9 10 11 12) 3) '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
   (equal (split-list-2 '(1 2 3 4 5 6 7 8 9 10 11 12) 4) '((1 2 3 4) (5 6 7 8) (9 10 11 12)))
   (equal (split-list-2 '(1 2 3 4 5 6 7 8 9 10 11 12) 6) '((1 2 3 4 5 6) (7 8 9 10 11 12)))
   (equal (split-list-2 '(1 2 3 4 5 6 7 8 9 10 11 12) 5) '((1 2 3 4 5) (6 7 8 9 10) (11 12)))) )

;;;
;;;    Ha! Despite the absolute simplicity of this definition, it isn't adequate.
;;;    This fails for non-rectangular tables!
;;;    
;; (defun transpose (tree)
;;   (apply #'mapcar #'list tree))

(deftest test-transpose ()
  (check
   (equal (transpose '((1 2 3 4 5 6))) '((1) (2) (3) (4) (5) (6)))
   (equal (transpose '((1) (2) (3) (4) (5) (6))) '((1 2 3 4 5 6)))
   (equal (transpose '((1 2 3 4 5 6) (7 8 9 10 11 12))) '((1 7) (2 8) (3 9) (4 10) (5 11) (6 12)))
   (equal (transpose '((1 2 3 4 5 6) (7 8 9 10 11))) '((1 7) (2 8) (3 9) (4 10) (5 11) (6)))
   (equal (transpose '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) '((1 4 7 10) (2 5 8 11) (3 6 9 12)))
   (equal (transpose '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12))) '((1 3 5 7 9 11) (2 4 6 8 10 12)))
   (equal (transpose (transpose #1='((1 2) (3 4) (5 6) (7 8) (9 10) (11 12)))) #1#)
   (equal (transpose '((1 2) (3 4) (5 6) (7 8) (9 10) (11))) '((1 3 5 7 9 11) (2 4 6 8 10)))
   (equal (transpose '((1 2) (3 4) (5 6) (7 8) (9) (11))) '((1 3 5 7 9 11) (2 4 6 8)))
   (equal (transpose '((1 2) (3 4) (5 6) (7) (9) (11))) '((1 3 5 7 9 11) (2 4 6)))
   (equal (transpose '((1 2) (3 4) (5) (7) (9) (11))) '((1 3 5 7 9 11) (2 4)))
   (equal (transpose '((1 2) (3) (5) (7) (9) (11))) '((1 3 5 7 9 11) (2)))
   (equal (transpose (transpose #2='((1 2) (3) (5) (7) (9) (11)))) #2#)))
   

(defun transpose (tree)
  (labels ((transpose-aux (tree)
             (if (null tree)
                 (cons '() '())
                 (let ((first-list (first tree))
                       (result (transpose-aux (rest tree))))
                   (if (null first-list)
                       result
                       (destructuring-bind (elt . rest) first-list
                         (cons (cons elt (first result))
                               (cons rest (rest result)))) ))))
           (transpose-row (tree)
             (destructuring-bind (row . reduced-tree) (transpose-aux tree)
               (if (null (first reduced-tree)) ; First sublist is exhausted, so all others must be too (Unless original tree was malformed.)
                   (list row)
                   (cons row (transpose-row reduced-tree)))) ))
    (transpose-row tree)))

(defun multi-column-print (l &key (columns 1) (width (floor 80 columns)) (indent 0) (stream *standard-output*))
  (labels ((print-table (rows)
             (unless (endp rows)
               (format stream "~V,0T~@?~%" indent (format nil "~~{~~~D@<~~A~~>~~}" width) (first rows))
               (print-table (rest rows)))) )
    (print-table (split-list l (ceiling (length l) columns)))) ) ; !!

;;;
;;;    The exercise is ill-defined for non-rectangular tables.
;;;    Is this the correct output below?
;; ? (multi-column-print (loop for i from 1 to 11 collect i) :columns 6)
;; 1            3            5            7            9            11           
;; 2            4            6            8            10           
;; NIL
;; ? (multi-column-print (loop for i from 1 to 10 collect i) :columns 6)
;; 1            3            5            7            9            
;; 2            4            6            8            10
;;
;; Or should it be this?
;; 1            3            5            7            9            10
;; 2            4            6            8                       
;; NIL
;;
;;    These are obviously OK.
;; ? (multi-column-print (loop for i from 1 to 11 collect i) :columns 2)
;; 1                                       7                                       
;; 2                                       8                                       
;; 3                                       9                                       
;; 4                                       10                                      
;; 5                                       11                                      
;; 6                                       
;; NIL
;; ? (multi-column-print (loop for i from 1 to 10 collect i) :columns 2)
;; 1                                       6                                       
;; 2                                       7                                       
;; 3                                       8                                       
;; 4                                       9                                       
;; 5                                       10                                      
;; NIL
;;
;;    OK
;; ? (multi-column-print (loop for i from 1 to 11 collect i) :columns 4)
;; 1                   4                   7                   10                  
;; 2                   5                   8                   11                  
;; 3                   6                   9                   
;; NIL
;;
;;    Wrong
;; ? (multi-column-print (loop for i from 1 to 10 collect i) :columns 4)
;; 1                   4                   7                   10                  
;; 2                   5                   8                   
;; 3                   6                   9                   
;; NIL
;;
;;    Should be
;; 1                   4                   7                   9                  
;; 2                   5                   8                   10
;; 3                   6                                      

;;;
;;;    7.9.5
;;;
(defun romans (limit size)
  (check-type limit (integer 1))
  (labels ((collect-romans (i result)
             (if (= i limit)
                 result
                 (let ((roman (format nil "~@R" i)))
                   (if (= (length roman) size)
                       (collect-romans (1+ i) (cons roman result))
                       (collect-romans (1+ i) result)))) ))
    (collect-romans 1 '())))

(deftest test-romans ()
  (check
   (equal (romans 1000 2) '("CM" "DC" "DL" "DX" "DV" "DI" "CD" "CC" "CL" "CX" "CV" "CI" "XC" "LX" "LV" "LI" "XL" "XX" "XV" "XI" "IX" "VI" "IV" "II"))
   (equal (romans 40 4) '("XXXV" "XXXI" "XXIX" "XXVI" "XXIV" "XXII" "XVII" "XIII" "VIII"))
   (equal (romans 1 1) '())
   (equal (romans 2 1) '("I"))
   (equal (romans 2 2) '())
   (equal (romans 100 3) '("XCV" "XCI" "LXX" "LXV" "LXI" "LIX" "LVI" "LIV" "LII" "XLV" "XLI" "XXX" "XXV" "XXI" "XIX" "XVI" "XIV" "XII" "VII" "III"))
   (equal (romans 100 4) '("XCIX" "XCVI" "XCIV" "XCII" "LXXX" "LXXV" "LXXI" "LXIX" "LXVI" "LXIV" "LXII" "LVII" "LIII" "XLIX" "XLVI" "XLIV" "XLII" "XXXV" "XXXI" "XXIX" "XXVI" "XXIV" "XXII" "XVII" "XIII" "VIII"))
   (equal (romans 100 5) '("XCVII" "XCIII" "LXXXV" "LXXXI" "LXXIX" "LXXVI" "LXXIV" "LXXII" "LXVII" "LXIII" "LVIII" "XLVII" "XLIII" "XXXIX" "XXXVI" "XXXIV" "XXXII" "XXVII" "XXIII" "XVIII"))
   (equal (romans 100 6) '("XCVIII" "LXXXIX" "LXXXVI" "LXXXIV" "LXXXII" "LXXVII" "LXXIII" "LXVIII" "XLVIII" "XXXVII" "XXXIII" "XXVIII"))
   (equal (romans 100 7) '("LXXXVII" "LXXXIII" "LXXVIII" "XXXVIII"))
   (equal (romans 100 8) '("LXXXVIII"))
   (equal (romans 100 9) '())))
