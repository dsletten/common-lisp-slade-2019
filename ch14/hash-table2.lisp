;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               hash-table2.lisp
;;;;
;;;;   Started:            Tue May 26 01:57:52 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;    2nd implementation of hash table (Ex. 14.8.9)
;;;;    - Entries for keys with same hash wind up in same bucket.
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

(defpackage :hash-table2 (:use :common-lisp :test) (:shadow :hash-table :hash-table-p :make-hash-table :get :find :hash-table-count :hash-table-size :sxhash))

(in-package :hash-table2)

(defclass hash-table ()
  ((count :initform 0 :reader hash-table-count :documentation "Number of entries in hash table.")
   (buckets :documentation "Storage for the table."))
  (:documentation "Simple hash table class"))

(defmethod initialize-instance :after ((table hash-table) &rest initargs &key (size 10))
  (declare (ignore initargs))
  (with-slots (buckets) table
    (setf buckets (make-array size :initial-element nil))))

(defgeneric hash-table-size (obj))
(defmethod hash-table-size ((table hash-table))
  (with-slots (buckets) table
    (length buckets)))

(defgeneric hash-table-p (obj)
  (:method ((obj hash-table)) t)
  (:method (obj) nil))

(defun make-hash-table (&optional (size 10)) ; ?
  (make-instance 'hash-table :size size))

(defun get-entry (table key)
  (assoc key (get-bucket table key) :test #'test))
;  (assoc key (get-bucket table key) :test #'equal))

(defgeneric get-bucket (table key))
(defmethod get-bucket ((table hash-table) key)
  (with-slots (buckets) table
    (aref buckets (compute-hash table key))))

;; (defgeneric (setf get-bucket) (entry table key))
;; (defmethod (setf get-bucket) (entry (table hash-table) key)
;;   (with-slots (buckets) table
;;     (setf (aref buckets (compute-hash table key))))

(defun key (entry)
  (car entry))

(defun value (entry)
  (cdr entry))

(defun (setf value) (value entry)
  (setf (cdr entry) value))

(defun make-entry (key value)
  (cons key value))

(defgeneric compute-hash (table key))
(defmethod compute-hash ((table hash-table) key)
  (mod (sxhash key) (hash-table-size table)))

(defun test (obj candidate)
  (etypecase obj
    (symbol (and (symbolp candidate) (eq obj candidate)))
    (number (and (numberp candidate) (= obj candidate)))
    (string (and (stringp candidate) (string= obj candidate)))
    (list (and (listp candidate) (equal obj candidate)))) )

(defmethod get ((table hash-table) key)
  (let ((entry (get-entry table key)))
    (if (null entry)
        (values nil nil)
        (values (value entry) t))))

;;;
;;;    Add a new entry to the table or update an existing entry.
;;;    Count is not incremented for update.
;;;    
(defmethod put ((table hash-table) key value)
  (let ((index (compute-hash table key))
        (entry (get-entry table key)))
    (if (null entry)
        (with-slots (buckets count) table
          (incf count)
          (push (make-entry key value) (aref buckets index)))
        (setf (value entry) value))))

;(dolist (entry '((john 39) (sue 19) (kim 20) (tom 40) (bob 37) (mary 23) (tina 24) (beth 18) (carrie 20) (nancy 21))) (put *h1* (first entry) (second entry)))

;;;
;;;    14.8.7
;;;    Find first key in a table that is associated with given key.
;;;
(defgeneric find (table value))
(defmethod find ((table hash-table) value)
  (with-slots (buckets) table
    (let ((entry (loop for bucket across buckets
                       when (rassoc value bucket :test #'test) return it)))
      (if (null entry)
          nil
          (key entry)))) )

;;;
;;;    14.8.10
;;;    
(defun sxhash (obj)
  (typecase obj
    (integer obj)
    (character (char-code obj))
    (string (cond ((string= obj "") 1)
                  (t (+ (sxhash (char obj 0))
                        (* 2 (sxhash (subseq obj 1)))) )))
    ;; (cons (+ (sxhash (first obj)) (* 2 (sxhash (rest obj)))) ) ; Slade's version LIST -> CONS/NULL
    ;; (null 31415926)
    (list (cond ((null obj) 31415926)
                (t (+ (sxhash (first obj))
                      (* 2 (sxhash (rest obj)))) )))
    (vector (cond ((zerop (length obj)) #xDEADBEEF) ; Slade does (sxhash (coerce obj 'list)) !!
                  (t (+ (sxhash (aref obj 0))
                        (* 2 (sxhash (subseq obj 1)))) )))
    (symbol (sxhash (symbol-name obj)))) ) ; Must come after LIST to not intercept NIL!!

