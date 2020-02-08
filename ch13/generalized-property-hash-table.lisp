;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               generalized-property-hash-table.lisp
;;;;
;;;;   Started:            Fri Nov 29 23:22:35 2019
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

(defpackage :generalized-property-hash-table (:use :common-lisp :test) (:shadow :get))

(in-package :generalized-property-hash-table)

(defclass property-table ()
  ((table :initform (empty-table) :accessor table)
   (name :initarg :name :reader name)))

(defun empty-table ()
  (make-hash-table :test #'equal))

(defun make-property-table (name)
  (make-instance 'property-table :name name))

;;;
;;;    Implicitly creates (empty) hash-table for a missing KEY.
;;;    
(defgeneric lookup (property-table key)
  (:documentation "Retrieve the properties associated with a given KEY."))
(defmethod lookup ((table property-table) key)
  (let ((props (gethash key (table table))))
    (if (null props)
        (let ((new-props (empty-table)))
          (setf (gethash key (table table)) new-props)
          new-props)
        props)))
;;?????
;; (defgeneric (setf lookup) (value property-table key)
;;   (:documentation "Update the property list associated with KEY."))
;; (defmethod (setf lookup) (value (table property-table) key)
;;   (setf (cdr (assoc key (table table) :test #'equal)) value))

;;;
;;;    Implicitly creates hash-table in table for missing KEY.
;;;    No side effects for missing PROPERTY.
;;;    
(defgeneric get (property-table key property)
  (:documentation "Retrieve the PROPERTY associated with KEY."))
(defmethod get ((table property-table) key property)
  (gethash property (lookup table key)))

(defgeneric put (property-table key property value)
  (:documentation "Set VALUE of PROPERTY associated with KEY."))
(defmethod put ((table property-table) key property value)
  (setf (gethash property (lookup table key)) value))

(defgeneric all-keys (property-table)
  (:documentation "Return a list of all of the keys present in the TABLE."))
(defmethod all-keys ((table property-table))
  (loop for key being the hash-keys in (table table) collect key))

(defgeneric clear (property-table)
  (:documentation "Clear out all entries of the TABLE."))
(defmethod clear ((table property-table))
  (setf (table table) (empty-table)))

(defmethod print-object ((table property-table) stream)
  (print-unreadable-object (table stream)
    (format stream "Private Property Table: ~A" (name table))))

(defgeneric pretty-print-key (property-table key)
  (:documentation "Print all of the properties and values associated with KEY."))
(defmethod pretty-print-key ((table property-table) key)
  (format t "~A~%" key)
  (let ((property-map (lookup table key)))
    (loop for key being the hash-keys in property-map
          do (format t "~5T~A ~20T~A~%" key (gethash key property-map)))) )

(defgeneric pretty-print (property-table)
  (:documentation "Pretty print the entire table."))
(defmethod pretty-print ((table property-table))
  (dolist (key (all-keys table))
    (pretty-print-key table key)))

