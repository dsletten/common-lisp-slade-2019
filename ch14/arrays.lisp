;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               arrays.lisp
;;;;
;;;;   Started:            Sun May 17 06:53:42 2020
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
(load "/home/slytobias/lisp/packages/strings.lisp")

(defpackage :arrays (:use :common-lisp :test :strings))

(in-package :arrays)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    2-dimensional array class. (Fixed Slade's code...)
;;;    
(defclass 2d-array ()
  ((rows :initarg :rows :reader rows)
   (columns :initarg :columns :reader columns)
   (array))
  (:documentation "Home-brewed two-dimensional array."))

(defun make-2d-array (rows columns)
  (make-instance '2d-array :rows rows :columns columns))

(defmethod initialize-instance :after ((a 2d-array) &rest initargs)
  (declare (ignore initargs))
  (with-slots (rows columns array) a
    (setf array (make-array rows))
    (dotimes (i rows)
      (setf (aref array i) (make-array columns)))) )

(defun 2d-array-p (obj)
  (typep obj '2d-array))

(defmethod print-object ((a 2d-array) stream)
  (with-slots (rows columns array) a
    (print-unreadable-object (a stream :type t)
      (format stream "(~Ax~A) ~A" rows columns array))))

(defmethod 2d-ref ((a 2d-array) (i integer) (j integer))
  (with-slots (array) a
    (aref (aref array i) j)))

(defmethod 2d-ref :around ((a 2d-array) (i integer) (j integer))
  (validate-indexes a i j)
  (call-next-method))

(defun validate-indexes (a i j)
  (with-slots (rows columns) a
    (assert (typep i `(integer 0 (,rows)))
            (i)
            "2d-ref error. row value: ~D is out of range (0-~D)" i rows)
    (assert (typep j `(integer 0 (,columns)))
            (j)
            "2d-ref error. column value: ~D is out of range (0-~D)" j columns)))

(defmethod (setf 2d-ref) :around (val (a 2d-array) (i integer) (j integer))
  (validate-indexes a i j)
  (call-next-method))

(defmethod (setf 2d-ref) (val (a 2d-array) (i integer) (j integer))
  (with-slots (array) a
    (setf (aref (aref array i) j) val)))

(defmethod 2d-array-fill ((a 2d-array) val)
  (with-slots (array) a
    (map nil #'(lambda (row) (fill row val)) array)))

;;;
;;;    14.8.4
;;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    3-dimensional array class.
;;;    
(defclass 3d-array ()
  ((dimensions :reader dimensions :initarg :dimensions)
   (array))
  (:documentation "3-dimensional array."))

(defun make-3d-array (x y z &key (initial-element 0d0))
  (make-instance '3d-array :dimensions (list x y z) :initial-element initial-element))

(defmethod initialize-instance :after ((a 3d-array) &rest initargs &key initial-element)
  (declare (ignore initargs))
  (with-slots (array dimensions) a
    (destructuring-bind (x y z) dimensions
      (setf array (make-array x))
      (dotimes (i x)
        (setf (aref array i) (make-array y))
        (dotimes (j y)
          (setf (aref (aref array i) j) (make-array z :initial-element initial-element)))) )))

(defmethod print-object ((a 3d-array) stream)
  (with-slots (dimensions array) a
    (print-unreadable-object (a stream :type t)
      (format stream "(~A) ~A" (join dimensions "x") array))))

(defun 3d-array-p (obj)
  (typep obj '3d-array))

(defmethod 3d-ref ((a 3d-array) (i integer) (j integer) (k integer))
  (with-slots (array) a
    (aref (aref (aref array i) j) k)))

(defmethod 3d-ref :around ((a 3d-array) (i integer) (j integer) (k integer))
  (validate-indexes-3 a i j k)
  (call-next-method))

(defun validate-indexes-3 (a i j k)
  (with-slots (dimensions) a
    (destructuring-bind (x y z) dimensions
      (assert (<= 0 i (1- x))
              (i)
              "3d-ref error. Dimension 0 value: ~D is out of range (0-~D)" i x)
      (assert (<= 0 j (1- y))
              (j)
              "3d-ref error. Dimension 1 value: ~D is out of range (0-~D)" j y)
      (assert (<= 0 k (1- z))
              (k)
              "3d-ref error. Dimension 2 value: ~D is out of range (0-~D)" k z))))

(defmethod (setf 3d-ref) :around (val (a 3d-array) (i integer) (j integer) (k integer))
  (validate-indexes-3 a i j k)
  (call-next-method))

(defmethod (setf 3d-ref) (val (a 3d-array) (i integer) (j integer) (k integer))
  (with-slots (array) a
    (setf (aref (aref (aref array i) j) k) val)))

(defmethod 3d-array-fill ((a 3d-array) val)
  (with-slots (dimensions array) a
    (dotimes (i (first dimensions))
      (map nil #'(lambda (v) (fill v val)) (aref array i)))) )

;;;
;;;    14.8.5
;;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    N-dimensional array class.
;;;    
(defclass nd-array ()
  ((dimensions :reader dimensions :initarg :dimensions)
   (array))
  (:documentation "N-dimensional array."))

;(defun make-nd-array (x y z &key (initial-element 0d0))
(defun make-nd-array (&rest dimensions)
  (make-instance 'nd-array :dimensions dimensions))

;(defmethod initialize-instance :after ((a nd-array) &rest initargs &key initial-element)
(defmethod initialize-instance :after ((a nd-array) &rest initargs)
  (declare (ignore initargs))
  (with-slots (array dimensions) a
    (setf array (initialize-array (first dimensions) (rest dimensions)))) )

(defun initialize-array (dimension dimensions)
  (let ((a (make-array dimension)))
    (unless (null dimensions)
      (dotimes (i dimension)
        (setf (aref a i) (initialize-array (first dimensions) (rest dimensions)))) )
    a))

(defmethod print-object ((a nd-array) stream)
  (with-slots (dimensions array) a
    (print-unreadable-object (a stream :type t)
      (format stream "(~A) ~A" (join dimensions "x") array))))

(defun nd-array-p (obj)
  (typep obj 'nd-array))

(defmethod nd-ref ((a nd-array) &rest indexes)
  (with-slots (array) a
    (reduce #'aref indexes :initial-value array)))

(defmethod nd-ref :around ((a nd-array) &rest indexes)
  (validate-indexes-n a indexes)
  (call-next-method))

(defun validate-indexes-n (a indexes)
  (with-slots (dimensions) a
    (assert (= (length indexes) (length dimensions)) () "nd-ref error. Incompatible indexes: ~A Should be ~D dimensions" indexes (length dimensions))
    (loop for i from 0
          for dimension in dimensions
          for index in indexes
          do (assert (<= 0 index (1- dimension))
              ()
              "nd-ref error. Dimension ~D value: ~D is out of range (0-~D)" i index dimension))))

(defmethod (setf nd-ref) :around (val (a nd-array) &rest indexes)
  (validate-indexes-n a indexes)
  (call-next-method))

(defmethod (setf nd-ref) (val (a nd-array) &rest indexes)
  (with-slots (array) a
    (set-nested-array array (first indexes) (rest indexes) val)))

(defun set-nested-array (a dimension dimensions val)
  (cond ((null dimensions) (setf (aref a dimension) val))
        (t (set-nested-array (aref a dimension) (first dimensions) (rest dimensions) val))))

(defmethod nd-array-fill ((a nd-array) val)
  (with-slots (dimensions array) a
    (fill-array array (first dimensions) (rest dimensions) val)))

(defun fill-array (a dimension dimensions val)
  (cond ((null dimensions) (fill a val))
        (t (dotimes (i dimension)
             (fill-array (aref a i) (first dimensions) (rest dimensions) val)))) )

(defun map-nd-array (a f)
  (with-slots (dimensions (array-a array)) a
    (let ((b (apply #'make-nd-array dimensions)))
      (with-slots ((array-b array)) b
        (map-array array-a array-b (first dimensions) (rest dimensions) f))
      b)))

(defun map-array (a b dimension dimensions f)
  (cond ((null dimensions) (dotimes (i dimension)
                             (setf (aref b i)
                                   (funcall f (aref a i)))) )
        (t (dotimes (i dimension)
             (map-array (aref a i) (aref b i) (first dimensions) (rest dimensions) f)))) )
