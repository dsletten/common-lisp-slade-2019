;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               generalized-property-table.lisp
;;;;
;;;;   Started:            Tue Nov 26 00:55:34 2019
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
;;;;   Notes: Ex. 13.10.5 The original implementation (pg. 498) is introduced as an alternative
;;;;   to using "global" symbol property lists. Rather than using the symbol IAGO to store a property
;;;;   AGE with a value ANCIENT, the property-table holds its own entry corresponding to the symbol
;;;;   IAGO. This in turn has its own property list distinct from (symbol-plist 'iago) => NIL
;;;;
;;;;   The purpose of this exercise is to allow general "properties" to be stored with arbitrary key objects
;;;;   entered into the table. Neither the keys nor the properties associated with them need be symbols anymore.
;;;;   This is accomplished by using association lists rather than property lists as the underlying storage.
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :generalized-property-table (:use :common-lisp :test) (:shadow :get))

(in-package :generalized-property-table)

(defclass property-table ()
  ((table :initform (empty-table) :accessor table)
   (name :initarg :name :reader name)))

(defun empty-table ()
  (list))

(defun make-property-table (name)
  (make-instance 'property-table :name name))

;;;
;;;    Implicitly creates (empty) a-list for a missing KEY.
;;;    
(defgeneric lookup (property-table key)
  (:documentation "Retrieve the properties associated with a given KEY."))
(defmethod lookup ((table property-table) key)
  (let ((props (assoc key (table table) :test #'equal)))
    (if (null props)
        (let ((new-props (cons key '())))
          (push new-props (table table))
          (cdr new-props))
        (cdr props))))
;;?????
(defgeneric (setf lookup) (value property-table key)
  (:documentation "Update the property list associated with KEY."))
(defmethod (setf lookup) (value (table property-table) key)
  (setf (cdr (assoc key (table table) :test #'equal)) value))

;;;
;;;    Implicitly creates a-list in table for missing KEY.
;;;    No side effects for missing PROPERTY.
;;;    
(defgeneric get (property-table key property)
  (:documentation "Retrieve the PROPERTY associated with KEY."))
(defmethod get ((table property-table) key property)
  (second (assoc property (lookup table key) :test #'equal)))

(defgeneric put (property-table key property value)
  (:documentation "Set VALUE of PROPERTY associated with KEY."))
(defmethod put ((table property-table) key property value)
  (let ((pair (assoc property (lookup table key) :test #'equal)))
    (if (null pair)
        (push (list property value) (lookup table key))
        (setf (second pair) value))))

(defgeneric all-keys (property-table)
  (:documentation "Return a list of all of the keys present in the TABLE."))
(defmethod all-keys ((table property-table))
  (mapcar #'car (table table)))

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
  (loop for (property value) in (lookup table key)
        do (format t "~5T~A ~20T~A~%" property value)))

(defgeneric pretty-print (property-table)
  (:documentation "Pretty print the entire table."))
(defmethod pretty-print ((table property-table))
  (dolist (key (all-keys table))
    (pretty-print-key table key)))

