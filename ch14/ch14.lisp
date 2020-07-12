;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               ch14.lisp
;;;;
;;;;   Started:            Sat May  9 21:59:48 2020
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
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :ch14 (:use :common-lisp :test))

(in-package :ch14)

;;;
;;;    14.8.18
;;;    
(defmacro make-executable (sym)
  `(etypecase ,sym
     (hash-table (defun ,sym (key)
                   (gethash key ,sym))
                 (defun (setf ,sym) (val key)
                   (setf (gethash key ,sym) val)))
     (string (defun ,sym (i)
               (char ,sym i))
             (defun (setf ,sym) (ch i)
               (setf (char ,sym i) ch)))
     (array (defun ,sym (&rest indexes)
              (apply #'aref ,sym indexes))
            (defun (setf ,sym) (val &rest indexes)
              (setf (apply #'aref ,sym indexes) val)))
     (list (defun ,sym (i)
             (nth i ,sym))
           (defun (setf ,sym) (val i)
             (setf (nth i ,sym) val)))) )

;; (macroexpand-1 '(make-executable *s*))
;; (ETYPECASE *S*
;;   (HASH-TABLE
;;    (DEFUN *S* (KEY) (GETHASH KEY *S*))
;;    (DEFUN (SETF *S*) (VAL KEY) (SETF (GETHASH KEY *S*) VAL)))
;;   (STRING
;;    (DEFUN *S* (I) (CHAR *S* I))
;;    (DEFUN (SETF *S*) (CH I) (SETF (CHAR *S* I) CH)))
;;   (ARRAY
;;    (DEFUN *S* (&REST INDEXES) (APPLY #'AREF *S* INDEXES))
;;    (DEFUN (SETF *S*) (VAL &REST INDEXES)
;;      (SETF (APPLY #'AREF *S* INDEXES) VAL)))
;;   (LIST
;;    (DEFUN *S* (I) (NTH I *S*))
;;    (DEFUN (SETF *S*) (VAL I) (SETF (NTH I *S*) VAL))))

;; (defvar *h* (make-hash-table))
;; (setf (gethash 'a *h*) 'foo)
;; (setf (gethash 'b *h*) 'bar)
;; (make-executable *h*)
;; (*h* 'a)
;; (*h* 'b)
;; (setf (*h* 'c) 10)
;; (*h* 'c)
;; (defvar *a* (make-array '(3 4)))
;; (make-executable *a*)
;; (*a* 0 0)
;; (*a* 0 1)
;; (setf (*a* 0 1) 9)
;; (*a* 0 1)
;; (*a* 2 3)
;; (setf (*a* 2 3) :foo)
;; (*a* 2 3)
;; (defvar *l* (list 1 2 3))
;; (make-executable *l*)
;; (*l* 0)
;; (setf (*l* 2) 99)
;; *l*
;; (defvar *s* (copy-seq "Is this not pung?"))
;; (make-executable *s*)
;; (*s* 0)
;; (loop for i from 0 below (length *s*) do (print (*s* i)))
;; (setf (*s* 12) #\P)
