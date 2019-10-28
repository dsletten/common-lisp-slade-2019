;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               property-table.lisp
;;;;
;;;;   Started:            Wed Sep 11 22:18:43 2019
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
;;;;   This fixes much of the nonsense in Slade's version. In particular, I use actual _property lists_
;;;;   to store the properties for a given symbol.
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :property-table (:use :common-lisp :test) (:shadow :get))

(in-package :property-table)

(defclass property-table ()
  ((table :initform (empty-table) :accessor table)
   (name :initarg :name :reader name)))

(defun empty-table ()
  (list))

(defun make-property-table (name)
  (make-instance 'property-table :name name))

;;;
;;;    Implicitly creates (empty) property-list for a missing SYMBOL.
;;;    
(defgeneric lookup (property-table symbol)
  (:documentation "Retrieve the properties associated with a given SYMBOL."))
(defmethod lookup ((table property-table) (symbol symbol))
  (let ((props (assoc symbol (table table))))
    (if (null props)
        (let ((new-props (cons symbol '())))
          (push new-props (table table))
          (cdr new-props))
        (cdr props))))
(defgeneric (setf lookup) (value property-table symbol)
  (:documentation "Update the property list associated with SYMBOL."))
(defmethod (setf lookup) (value (table property-table) (symbol symbol))
  (setf (cdr (assoc symbol (table table))) value))

;;;
;;;    Implicitly creates property-list in table for missing SYMBOL.
;;;    No side effects for missing PROPERTY.
;;;    
(defgeneric get (property-table symbol property)
  (:documentation "Retrieve the PROPERTY associated with SYMBOL."))
(defmethod get ((table property-table) (symbol symbol) (property symbol))
  (getf (lookup table symbol) property))

(defgeneric put (property-table symbol property value)
  (:documentation "Set VALUE of PROPERTY associated with SYMBOL."))
(defmethod put ((table property-table) (symbol symbol) (property symbol) value)
  (setf (getf (lookup table symbol) property) value))

(defgeneric all-symbols (property-table)
  (:documentation "Return a list of all of the symbols present in the TABLE."))
(defmethod all-symbols ((table property-table))
  (mapcar #'car (table table)))

(defgeneric clear (property-table)
  (:documentation "Clear out all entries of the TABLE."))
(defmethod clear ((table property-table))
  (setf (table table) (empty-table)))

(defmethod print-object ((table property-table) stream)
  (print-unreadable-object (table stream)
    (format stream "Private Property Table: ~A" (name table))))

(defgeneric pretty-print-symbol (property-table symbol)
  (:documentation "Print all of the properties and values associated with SYMBOL."))
(defmethod pretty-print-symbol ((table property-table) (symbol symbol))
  (format t "~A~%" symbol)
  (loop for (property value) on (lookup table symbol) by #'cddr
        do (format t "~5T~A ~20T~A~%" property value)))

(defgeneric pretty-print (property-table)
  (:documentation "Pretty print the entire table."))
(defmethod pretty-print ((table property-table))
  (dolist (symbol (all-symbols table))
    (pretty-print-symbol table symbol)))

;; (defgeneric lookup (property-table symbol)
;;   (:documentation "Retrieve the properties associated with a given SYMBOL."))
;; (defmethod lookup ((table property-table) (symbol symbol))
;;   (let ((props (assoc symbol (table table))))
;;     (if (null props)
;;         (let ((new-props (cons symbol nil)))
;;           (push new-props (table table))
;;           (cdr new-props))
;;         (cdr props))))
;; (defgeneric (setf lookup) (value property-table symbol)
;;   (:documentation ""))
;; (defmethod (setf lookup) (value (table property-table) (symbol symbol))
;;   (setf (cdr (assoc symbol (table table))) value))

;; (defgeneric create (property-table symbol property value)
;;   (:documentation "Establish a PROPERTY for the SYMBOL in the TABLE."))
;; (defmethod create ((table property-table) (symbol symbol) (property symbol) value)
;;   (push (cons property value) (lookup table symbol)))

;; (defgeneric update (property-table symbol property value)
;;   (:documentation "Change the value of PROPERTY associated with SYMBOL in the TABLE."))
;; (defmethod update ((table property-table) (symbol symbol) (property symbol) value)
;;   (setf (cdr (assoc property (lookup table symbol))) value))

;; ;;;
;; ;;;    Implicitly creates property-list (a-list) in table for missing SYMBOL.
;; ;;;    No side effects for missing PROPERTY.
;; ;;;    
;; (defgeneric get (property-table symbol property)
;;   (:documentation "Retrieve the PROPERTY associated with SYMBOL."))
;; (defmethod get ((table property-table) (symbol symbol) (property symbol))
;;   (let ((record (assoc property (lookup table symbol))))
;;     (if (null record)
;;         nil
;;         (cdr record))))

;; (defgeneric put (property-table symbol property value)
;;   (:documentation "Set VALUE of PROPERTY associated with SYMBOL."))
;; (defmethod put ((table property-table) (symbol symbol) (property symbol) value)
;;   (let ((props (lookup table symbol)))
;;     (let ((record (assoc property props)))
;;       (if (null record)
;; ;          (setf (cdr (assoc symbol (table table))) (cons (cons property value) props))
;;           (create table symbol property value)
;; ;          (update (assoc symbol (table table)) (cons (cons property value) props))
;; ;          (push (cons property value) (lookup table symbol))
;; ;          (setf (cdr record) value)))) )
;;           (update table symbol property value)))) )

