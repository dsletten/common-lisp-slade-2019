;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               dpsq-clos.lisp
;;;;
;;;;   Started:            Sun Oct  6 16:40:56 2019
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

(defpackage :dpsq-clos (:use :common-lisp :test))

(in-package :dpsq-clos)

(defclass person ()
  ((name :initarg :name :accessor name)
   (age :initform 0 :accessor age)
   (sex :initform nil :accessor sex)
   (father :initform nil :accessor father)
   (mother :initform nil :accessor mother)
   (spouse :initform nil :accessor spouse)
   (children :initform '() :accessor children)
   (siblings :initform '() :accessor siblings)
   (jobs :initform '() :accessor jobs)
   (saved-slots :initform '() :accessor saved-slots)))

(defmethod print-object ((p person) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A age: ~D sex: ~A" (name p) (age p) (sex p))))

;http://www.lispworks.com/documentation/HyperSpec/Body/11_abab.htm
(let ((people (make-hash-table)))
;;   (defmethod make-instance :around (class &rest initargs)   <-- Package is locked!!!!
;;     (let ((name (getf initargs :name)))
;;       (cond ((gethash name people))
;;             (t (let ((person (call-next-method)))
;;                  (setf (gethash person people))
;;                  person)))) ))
  (defun make-person (name)
    (cond ((gethash name people))
          (t (let ((person (make-instance 'person :name name)))
               (setf (gethash name people) person)
               person))))
  (defun find-person (name)
    (cond ((gethash name people))
          (t (make-person name)))) )

;; (defmethod (setf spouse) :around (new-spouse (p person))
;;   (let ((spouse (spouse p)))
;;     (when spouse
;;       (setf (getf (saved-slots p) 'spouse) spouse)))
;;   (call-next-method))
(defclass property () ())

(defclass relationship (property)
  ((name :initarg :name :reader name)
   (instances :allocation :class :initform '() :accessor instances)
   (characteristics :initarg :characteristics :initform '() :accessor characteristics)))

(defmethod initialize-instance :after ((r relationship) &rest initargs)
  (pushnew (name r) (instances r))
  (setf (getf (characteristics r) '+save-property) t))

(defmethod print-object ((r relationship) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~A ~A" (name r) (characteristics r))))

(defclass *relationship (property)
  ((name :initarg :name :reader name)
   (instances :allocation :class :initform '() :accessor instances)
   (characteristics :initarg :characteristics :initform '() :accessor characteristics)))

(defmethod print-object ((r *relationship) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~A ~A" (name r) (characteristics r))))

(defmethod initialize-instance :after ((r *relationship) &rest initargs)
  (pushnew (name r) (instances r))
  (setf (getf (characteristics r) '+multiple-values) t))

(defclass job (property)
  ((name :initarg :name :reader name)
   (instances :allocation :class :initform '() :accessor instances)
   (characteristics :initarg :characteristics :initform '() :accessor characteristics)))

(defmethod initialize-instance :after ((j job) &rest initargs)
  (pushnew (name j) (instances j))
  (setf (getf (characteristics j) '+multiple-values) t))

(defmethod print-object ((j job) stream)
  (print-unreadable-object (j stream :type t)
    (format stream "~A ~A" (name j) (characteristics j))))

(defclass isa (property) ())

;; (defvar *sibling* (make-instance '*relationship :name 'sibling))
;; (defvar *child* (make-instance '*relationship :name 'child))
;; (defvar *spouse* (make-instance 'relationship :name 'spouse))
;; (defvar *father* (make-instance 'relationship :name 'father))
;; (defvar *mother* (make-instance 'relationship :name 'mother))

(let ((properties (make-hash-table)))
  (defun add-property (name property)
    (setf (gethash name properties) property))
  (defun get-property (name)
    (gethash name properties)))

(defmacro defproperty (name type &key characteristics)
  (let ((chars characteristics))
    (if (null chars)
        `(add-property ',name (make-instance ',type :name ',name))
        `(add-property ',name (make-instance ',type :name ',name :characteristics ',chars)))) )

(defproperty sibling *relationship :characteristics (+invert-value t))
(defproperty child *relationship)
(defproperty spouse relationship :characteristics (+invert-value t))
(defproperty father relationship)
(defproperty mother relationship)

(defproperty carpenter job)
(defproperty plumber job)


(defgeneric ddput (node property value &optional recursive-p)
  (:documentation "Add the PROPERTY with the given VALUE to NODE."))
(defmethod ddput ((p symbol) (s symbol) value &optional recursive-p)
  (ddput (find-person p) (get-property s) value recursive-p))
(defmethod ddput ((p person) (r relationship) value &optional recursive-p)
  (let ((characteristics (characteristics r)))
    (loop for (characteristic v) on characteristics by #'cddr
          do (process-characteristic r characteristic v p value recursive-p))
    (funcall (fdefinition `(setf ,(name r))) value p)))
(defmethod ddput ((p person) (r *relationship) value &optional recursive-p)
  (let ((characteristics (characteristics r)))
    (loop for (characteristic value) on characteristics by #'cddr
          do (format t "~4T~S~20T~S~%" characteristic value))
    (funcall (fdefinition `(setf ,(name r))) value p)))

; New spouse -> invert, save old for _both_!!!!
(defun process-characteristic (prop ch v p value &optional recursive-p)
  (declare (ignore v)) ;  ??
  (case ch
    (+invert-value (unless recursive-p
                     (ddput value (name prop) (name p) t)))
    (+save-property (let ((old-value (funcall (name prop) p)))
                      (when old-value
                        (setf (getf (saved-slots p) (name prop)) old-value)
                        (funcall (fdefinition `(setf ,(name prop))) nil (find-person old-value)))) )))

(defgeneric ppp (obj)
  (:documentation "Pretty print the properties associated with an object."))
(defmethod ppp ((p person))
  (format t "~S~%" (name p))
  (dolist (property '(age sex father mother spouse children siblings jobs))
    (when (slot-boundp p property)
      (format t "~4T~S~20T~S~%" property (funcall property p)))) )

(defmacro pppq (node)
  `(ppp ',(find-person node)))

