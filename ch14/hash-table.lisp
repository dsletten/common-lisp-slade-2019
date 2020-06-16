;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               hash-table.lisp
;;;;
;;;;   Started:            Sat May 23 05:31:23 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;       Fixed Slade's code...
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

(defpackage :hash-table (:use :common-lisp :test) (:shadow :hash-table :hash-table-p :make-hash-table :get :find :hash-table-count :hash-table-size))

(in-package :hash-table)

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

(defmethod next-index ((table hash-table) (i integer))
  (mod (1+ i) (hash-table-size table)))

(defmethod get-entry ((table hash-table) (i integer))
  (with-slots (buckets) table
    (aref buckets i)))

(defun key (entry)
  (car entry))

(defun value (entry)
  (cdr entry))

(defgeneric compute-hash (table key))
(defmethod compute-hash ((table hash-table) key)
  (mod (sxhash key) (hash-table-size table)))

;;;
;;;    Returns the appropriate bucket index for the given KEY.
;;;    If that bucket is already in use (by a different key) try the next bucket.
;;;    A secondary boolean value is returned to indicate whether the KEY is already present.
;;;    
;; (defmethod get-index ((table hash-table) key &optional (n (compute-hash table key)))
;;   (labels ((get-index-aux (n i)
;;              (if (zerop i) 
;;                  nil
;;                  (let ((entry (get-entry table n)))
;;                    (cond ((null entry) (values n nil))
;;                          ((eq key (key entry)) (values n t))
;;                          (t (get-index-aux (next-index table n) (1- i)))) ))))
;;     (get-index-aux n (hash-table-size table))))
;;;
;;;    Modified for ex. 14.8.6 to allow arbitrary keys (symbol/number/string/list)
;;;    
(defmethod get-index ((table hash-table) key &optional (n (compute-hash table key)))
  (labels ((get-index-aux (n i)
             (if (zerop i) 
                 nil
                 (let ((entry (get-entry table n)))
                   (cond ((null entry) (values n nil))
                         ((test key (key entry)) (values n t))
                         (t (get-index-aux (next-index table n) (1- i)))) ))))
    (get-index-aux n (hash-table-size table))))

(defun test (obj candidate)
  (etypecase obj
    (symbol (and (symbolp candidate) (eq obj candidate)))
    (number (and (numberp candidate) (= obj candidate)))
    (string (and (stringp candidate) (string= obj candidate)))
    (list (and (listp candidate) (equal obj candidate)))) )

(defmethod get ((table hash-table) key)
  (multiple-value-bind (index presentp) (get-index table key)
    (if (not presentp)
        (values nil nil)
        (values (value (get-entry table index)) t))))

;;;
;;;    Add a new entry to the table or update an existing entry.
;;;    Count is not incremented for update.
;;;    
(defmethod put ((table hash-table) key value)
  (multiple-value-bind (index presentp) (get-index table key)
    (if (null index)
        (error "Hash table is full.")
        (with-slots (buckets count) table
          (unless presentp
            (incf count))
          (setf (aref buckets index) (cons key value)))) ))

;;;
;;;    This incorrectly prevents updating an entry when the table is full.
;;;    
;; (defmethod put :around ((table hash-table) key value)
;;   (if (= (count table) (hash-table-size table))
;;       (error "Hash table is full.")
;;       (call-next-method)))

;(dolist (entry '((john 39) (sue 19) (kim 20) (tom 40) (bob 37) (mary 23) (tina 24) (beth 18) (carrie 20) (nancy 21))) (put *h1* (first entry) (second entry)))

;;;
;;;    14.8.7
;;;    Find first key in a table that is associated with given key.
;;;
(defgeneric find (table value))
(defmethod find ((table hash-table) value)
  (with-slots (buckets) table
    (loop for entry across buckets 
          when (test value (value entry)) return (key entry))))
