;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               lazy.lisp
;;;;
;;;;   Started:            Sun Jun 21 00:41:01 2020
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

(defpackage :lazy (:use :common-lisp :test :strings))

(in-package :lazy)

(defclass delayed-state ()
  ((next :initarg :next :reader next :documentation "generator function to produce the next value"))
  (:documentation "a class for delayed evaluation"))

(defmethod print-object ((d delayed-state) stream)
  (print-unreadable-object (d stream)
    (format stream "Delayed")))

(defun delayed-p (obj)
  (typep obj 'delayed-state))

(defmacro delay (form)
  `(make-instance 'delayed-state :next #'(lambda () ,form)))

(defgeneric force (delayed-state))
(defmethod force ((d delayed-state))
  (funcall (next d)))

(defun integers2 (n)
  (cons n (delay (integers2 (1+ n)))) )

;; (defmacro lazy-pop (n)
;;   (let ((s (gensym)))
;;     `(prog1
;;        (car ,n)
;;      (setf ,n (force (cdr ,n)))) )

(defun odd-numbers (n)
  (cons n (delay (odd-numbers (+ n 2)))) )

(defclass lazy-2d-array ()
  ((rows :initarg :rows :reader rows)
   (columns :initarg :columns :reader columns)
   (fill-value :initform nil :accessor fill-value)
   (fill-flag :initform nil :accessor fill-flag)
   (array))
  (:documentation "A two-dimensional lazy array."))

(defun make-lazy-2d-array (rows cols)
  (make-instance 'lazy-2d-array :rows rows :columns cols))

(defmethod initialize-instance :after ((a lazy-2d-array) &rest initargs)
  (declare (ignore initargs))
  (with-slots (array rows columns) a
    (setf array (make-array rows))
    (dotimes (i rows)
      (setf (aref array i) (delay (make-array columns)))) ))

(defun lazy-2d-array-p (obj)
  (typep obj 'lazy-2d-array))

(defmethod print-object ((a lazy-2d-array) stream)
  (with-slots (rows columns array) a
    (print-unreadable-object (a stream :type t)
      (format stream "(~Ax~A) ~A" rows columns array))))

(defun check-array-bounds (a i j)
  (check-bounds i (rows a) "rows")
  (check-bounds j (columns a) "columns"))

(defun check-bounds (i max dimension)
  (assert (<= 0 i (1- max)) (i) "lazy-2d-ref error. ~A value: ~D is out of range (0-~D)" dimension i (1- max)))

(defmethod lazy-2d-ref ((a lazy-2d-array) (i integer) (j integer))
  (check-array-bounds a i j) ; AROUND method?
  (aref (lazy-ref a i) j))

(defmethod lazy-ref :before ((a lazy-2d-array) (i integer))
  (with-slots (array) a
    (when (delayed-p (aref array i))
      (setf (aref array i) (force (aref array i)))
      (when (fill-flag a)
        (fill (aref array i) (fill-value a)))) ))

(defmethod lazy-ref ((a lazy-2d-array) (i integer))
  (with-slots (array) a
    (aref array i)))

(defmethod (setf lazy-2d-ref) (val (a lazy-2d-array) (i integer) (j integer))
  (check-array-bounds a i j)
  (setf (aref (lazy-ref a i) j) val))

(defmethod lazy-2d-array-fill ((a lazy-2d-array) val)
 (setf (fill-flag a) t
       (fill-value a) val)
 (with-slots (array) a
   (loop for v across array
         unless (delayed-p v)
         do (fill v val))))

;;;
;;;    14.8.17
;;;    (Initial ND-ARRAY class adapted from 14.8.5)
;;;
(defclass lazy-nd-array ()
  ((dimensions :reader dimensions :initarg :dimensions)
   (fill-value :initform nil :accessor fill-value)
   (fill-flag :initform nil :accessor fill-flag)
   (contents))
  (:documentation "N-dimensional array."))

(defun make-lazy-nd-array (&rest dimensions)
  (make-instance 'lazy-nd-array :dimensions dimensions))

(defmethod initialize-instance :after ((a lazy-nd-array) &rest initargs)
  (declare (ignore initargs))
  (with-slots (contents dimensions) a
    (setf contents (make-array (first dimensions)))
    (dotimes (i (first dimensions))
      (setf (aref contents i) (delay (reify (rest dimensions)))) )))

;;;
;;;    We don't know the indices at which these subarrays will be located.
;;;    Simply collect them here.
;;;    
(defun reify (dimensions)
  (loop for dimension in dimensions collect (make-array dimension)))

;; (defun initialize-array (dimension dimensions)
;;   (let ((a (make-array dimension)))
;;     (unless (null dimensions)
;;       (dotimes (i dimension)
;;         (setf (aref a i) (initialize-array (first dimensions) (rest dimensions)))) )
;;     a))

(defmethod rank ((a lazy-nd-array))
  (length (dimensions a)))

(defmethod print-object ((a lazy-nd-array) stream)
  (with-slots (dimensions contents) a
    (print-unreadable-object (a stream :type t)
      (format stream "(~A) ~A" (join dimensions "x") contents))))

(defun lazy-nd-array-p (obj)
  (typep obj 'lazy-nd-array))

(defmethod lazy-nd-ref ((a lazy-nd-array) &rest indexes)
  (force-lazy-nd-ref a indexes)
  (with-slots (contents) a
    (reduce #'aref indexes :initial-value contents)))

(defmethod lazy-nd-ref :around ((a lazy-nd-array) &rest indexes)
  (validate-indexes-n a indexes)
  (call-next-method))

(defmethod force-lazy-nd-ref ((a lazy-nd-array) (indexes list))
  (with-slots (contents) a
    (when (delayed-p (aref contents (first indexes)))
      (let ((final-dimension (reduce #'(lambda (v args) 
                                         (destructuring-bind (index . vector) args 
                                           (setf (aref v index) vector))) ; Each vector becomes next arg to REDUCE.
                                     (mapcar #'cons indexes (force (aref contents (first indexes)))) ; Ignore last index...
                                     :initial-value contents)))
        (when (fill-flag a)
          (fill final-dimension (fill-value a)))) )))

(defun validate-indexes-n (a indexes)
  (with-slots (dimensions) a
    (assert (= (length indexes) (length dimensions)) () "lazy-nd-ref error. Incompatible indexes: ~A Should be ~D dimensions" indexes (length dimensions))
    (loop for i from 0
          for dimension in dimensions
          for index in indexes
          do (assert (<= 0 index (1- dimension))
              ()
              "lazy-nd-ref error. Dimension ~D value: ~D is out of range (0-~D)" i index (1- dimension)))) ) ; ??????

(defmethod lazy-nd-array-fill ((a lazy-nd-array) val)
 (setf (fill-flag a) t
       (fill-value a) val))
 ;; (with-slots (array) a
 ;;   (loop for v across array
 ;;         unless (delayed-p v)
 ;;         do (fill v val))))

;; (defmethod (setf lazy-nd-ref) (val (a lazy-nd-array) &rest indexes)
;;   (force-lazy-nd-ref a indexes)
;;   (with-slots (contents) a
;;     (setf (aref (reduce #'aref (butlast indexes) :initial-value contents) (first (last indexes))) val)))

(defmethod (setf lazy-nd-ref) (val (a lazy-nd-array) &rest indexes)
  (force-lazy-nd-ref a indexes)
  (with-slots (contents) a
    (set-nested-array contents (first indexes) (rest indexes) val)))

(defmethod (setf lazy-nd-ref) :around (val (a lazy-nd-array) &rest indexes)
  (validate-indexes-n a indexes)
  (call-next-method))

(defun set-nested-array (a dimension dimensions val)
  (cond ((null dimensions) (setf (aref a dimension) val))
        (t (set-nested-array (aref a dimension) (first dimensions) (rest dimensions) val))))

;;;;
(defmethod nd-array-fill ((a lazy-nd-array) val)
  (with-slots (dimensions contents) a
    (fill-array contents (first dimensions) (rest dimensions) val)))

(defun fill-array (a dimension dimensions val)
  (cond ((null dimensions) (fill a val))
        (t (dotimes (i dimension)
             (fill-array (aref a i) (first dimensions) (rest dimensions) val)))) )

(defun map-nd-array (a f)
  (with-slots (dimensions (array-a contents)) a
    (let ((b (apply #'make-nd-array dimensions)))
      (with-slots ((array-b contents)) b
        (map-array array-a array-b (first dimensions) (rest dimensions) f))
      b)))

(defun map-array (a b dimension dimensions f)
  (cond ((null dimensions) (dotimes (i dimension)
                             (setf (aref b i)
                                   (funcall f (aref a i)))) )
        (t (dotimes (i dimension)
             (map-array (aref a i) (aref b i) (first dimensions) (rest dimensions) f)))) )
