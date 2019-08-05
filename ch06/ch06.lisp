;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch06.lisp
;;;;
;;;;   Started:            Fri Aug  2 21:11:06 2019
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

(defpackage :ch06 (:use :common-lisp :test) (:shadow :string-equal :string-lessp :merge :reverse))

(in-package :ch06)

;;;
;;;    6.8.2
;;;
(defun last-char (s)
  (check-type s string)
  (if (zerop (length s))
      (code-char 0)
      (char s (1- (length s)))) )

(deftest test-last-char ()
  (check
   (char= (last-char "pung") #\g)))

;; (defun capitalize (s)
;;   (check-type s string)
;;   (if (zerop (length s))
;;       s
;;       (let ((result (copy-seq s)))
;;         (setf (char result 0) (char-upcase (char result 0)))
;;         result)))

;; (defun capitalize (s)
;;   (check-type s string)
;;   (if (zerop (length s))
;;       ""
;;       (string-capitalize s :end 1)))

;;
;;    Blech!
;;    
(defun capitalize (s)
  (check-type s string)
  (if (zerop (length s))
      ""
      (format nil "~@(~C~)~A" (char s 0) (subseq s 1))))

(deftest test-capitalize ()
  (check
   (string= (capitalize #1="Is this not pung?") #1#)
   (string= (capitalize "is this not pung?") #1#)
   (string= (capitalize "123") "123")
   (string= (capitalize "") "")))

(defun string-equal (s1 s2)
  (check-type s1 string)
  (check-type s2 string)
  (and (= (length s1) (length s2))
       (every #'char-equal s1 s2)))

(deftest test-string-equal ()
  (check
   (string-equal #1="Is this not pung?" #1#)
   (string-equal #1# (string-upcase #1#))
   (string-equal "" "")
   (not (string-equal "abc" "ab"))
   (not (string-equal "ab" "abc"))
   (not (string-equal "asdf" "qwer"))))

(defun string-lessp (s1 s2)
  (check-type s1 string)
  (check-type s2 string)
  (labels ((compare-strings (i)
;;              (cond ((= i (length s1)) (< i (length s2)))
;;                    ((= i (length s2)) nil)
             (cond ((= i (length s2)) nil) ; Fixed based on 2011 version!
                   ((= i (length s1)) i)
                   ((char-lessp (char s1 i) (char s2 i)) t)
                   ((char-equal (char s1 i) (char s2 i)) (compare-strings (1+ i)))
                   (t nil))))
    (compare-strings 0)))

(deftest test-string-lessp ()
  (check
   (string-lessp "alpha" "beta")
   (not (string-lessp "beta" "alpha"))
   (string-lessp "alphabet" "alphabetize")
   (not (string-lessp "alphabetize" "alphabet"))
   (string-lessp "" "a")
   (not (string-lessp "a" ""))
   (not (string-lessp "" ""))))
   
;;;
;;;    6.8.3
;;;
;; (defun merge (l1 l2)
;;   "Merge two pre-sorted lists."
;;   (labels ((merge-aux (l1 l2 test)
;;              (cond ((endp l1) l2)
;;                    ((endp l2) l1)
;;                    ((funcall test (first l1) (first l2))
;;                     (cons (first l1) (merge-aux (rest l1) l2 test)))
;;                    (t (cons (first l2) (merge-aux l1 (rest l2) test)))) ))
;;     (cond ((endp l1) l2)
;;           ((endp l2) l1)
;;           ((numberp (first l1)) (merge-aux l1 l2 #'<))
;;           ((characterp (first l1)) (merge-aux l1 l2 #'char<))
;;           (t 'error))))

;;
;;    Adapted from 2002 -- Originally from Slade??
;;    
(defun inorderp (x y)
  (typecase x
    (number (< x y))
    (character (char< x y))) )

(defun merge (l1 l2)
  "Merge two pre-sorted lists. Defined for lists of numbers and characters."
  (cond ((endp l1) l2)
        ((endp l2) l1)
        ((inorderp (first l1) (first l2)) (cons (first l1) (merge (rest l1) l2)))
        (t (cons (first l2) (merge l1 (rest l2)))) ))

(deftest test-merge ()
  (check
   (equal (merge '(2 7 9) '(1 8)) '(1 2 7 8 9))
   (equal (merge '(2 7 9) '()) '(2 7 9))
   (equal (merge '(9) '(5)) '(5 9))
   (equal (merge (coerce "ac" 'list) (coerce "bd" 'list)) '(#\a #\b #\c #\d))))

;;              (print (list l l1 l2))
;;              (force-output *query-io*)
;;              (when (eq (read) 'die) (error "adf"))

(defun merge-sort (l)
  (labels ((merge-sort-aux (l l1 l2)
             (cond ((endp l) (merge (merge-sort l1) (merge-sort l2)))
                   (t (merge-sort-aux (rest l) (cons (first l) l2) l1)))) )
    (cond ((endp l) l)
          ((endp (rest l)) l)
          (t (merge-sort-aux l '() '())))) )

(deftest test-merge-sort ()
  (check
   (equal (merge-sort #1='(3 2 5 1)) (sort (copy-seq #1#) #'<))
   (equal (merge-sort #2=(coerce "xad" 'list)) (sort (copy-seq #2#) #'char<))))

;;;
;;;    6.8.5
;;;
(defun string-reverse (s)
  (labels ((reverse (in out)
             (if (endp in)
                 out
                 (reverse (rest in) (cons (first in) out)))) )
    (coerce (reverse (coerce s 'list) '()) 'string)))

(deftest test-string-reverse ()
  (check
   (string= (string-reverse "hello") "olleh")
   (string= (string-reverse "a man a plan a canal panama") "amanap lanac a nalp a nam a")
   (string= (string-reverse "Is this not pung?") "?gnup ton siht sI")))
