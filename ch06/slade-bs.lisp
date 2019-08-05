;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               slade-bs.lisp
;;;;
;;;;   Started:            Sat Aug  3 23:10:31 2019
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

;;;
;;;    See "old" version 011014
;;;    
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :slade-bs (:use :common-lisp :test))

(in-package :slade-bs)

(defun msort (l)
  (sort2 l nil))

(defun sort2 (l tmplist)
  (cond ((null l) (sort3 tmplist nil))
        (t (sort2 (cdr l)
                  (sort-add (list (car l)) tmplist)))) )

(defun sort3 (l tmplist)
  (cond ((null l) tmplist)
        (t (sort3 (cdr l)
                  (lmerge (car l) tmplist)))) )

(defun sort-add (x tmplist)
  (cond ((null tmplist) (list x))
        ((null (car tmplist)) (cons x (cdr tmplist)))
        (t (cons nil (sort-add (lmerge x (car tmplist))
                               (cdr tmplist)))) ))

(defun lmerge (a b)
  (cond ((null a) b)
        ((null b) a)
        ((inorderp a b) (cons (car a) (lmerge (cdr a) b)))
        (t (cons (car b) (lmerge (cdr b) a)))) ) ; Why are A and B swapped for recursive call?!

;;
;;    This doesn't determine whether A and B are "in order"!
;;    Just their first elements!
;;    
(defun inorderp (a b)
  (cond ((numberp (car a)) (<= (car a) (car b)))
        ((characterp (car a)) (char<= (car a) (car b)))) )