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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;
;;;    When reading or writing an array element ensure that the appropriate part
;;;    of the array has been reified.
;;;    
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
  (:documentation "N-dimensional lazy array."))

(defun make-lazy-nd-array (&rest dimensions)
  (make-instance 'lazy-nd-array :dimensions dimensions))

(defmethod initialize-instance :after ((a lazy-nd-array) &rest initargs)
  (declare (ignore initargs))
  (with-slots (contents dimensions) a
    (setf contents (reify-dimension a dimensions))))

;;;
;;;    Reify next dimension of the lazy array. Returns a vector to instantiate the dimension.
;;;    If this is not the final dimension, then fill with DELAY objects capable of reifying the next dimension when needed.
;;;    For final dimension, fill with FILL-VALUE as appropriate.
;;;    
(defun reify-dimension (a dimensions)
  (assert (not (null dimensions)) () "No dimension to reify. Empty dimensions list.")
  (with-slots (fill-value fill-flag) a
    (destructuring-bind (dimension . more) dimensions
      (let ((contents (make-array dimension)))
        (if (null more)
            (if fill-flag
                (fill contents fill-value)
                contents)
            (dotimes (i dimension contents)
              (setf (aref contents i) (delay (reify-dimension a more)))) )))) )

(defmethod rank ((a lazy-nd-array))
  (length (dimensions a)))

(defmethod print-object ((a lazy-nd-array) stream)
  (with-slots (dimensions contents) a
    (print-unreadable-object (a stream :type t)
      (format stream "(~A) ~A" (join dimensions "x") contents))))

(defun lazy-nd-array-p (obj)
  (typep obj 'lazy-nd-array))

;;;
;;;    The read/write methods below are essentially the same as for the non-lazy N-dimensional array.
;;;    Once the subarrays have been FORCEd, the indexing is the same either way.
;;;    
(defmethod lazy-nd-ref ((a lazy-nd-array) &rest indexes)
  (force-lazy-nd-ref a indexes)
  (with-slots (contents) a
    (reduce #'aref indexes :initial-value contents)))

(defmethod lazy-nd-ref :around ((a lazy-nd-array) &rest indexes)
  (validate-indexes-n a indexes)
  (call-next-method))

;;;
;;;    Ensure that the indexed element actually exists before attempting to read or write it.
;;;    (This has to be done every time?!)
;;;    
(defmethod force-lazy-nd-ref ((a lazy-nd-array) (indexes list))
  (labels ((force-dimension (a indexes)
             (unless (endp indexes)
               (destructuring-bind (index . more) indexes
               (when (delayed-p (aref a index))
                 (setf (aref a index) (force (aref a index))))
               (force-dimension (aref a index) more)))) )
  (with-slots (contents) a
    (force-dimension contents indexes))))

(defun validate-indexes-n (a indexes)
  (with-slots (dimensions) a
    (assert (= (length indexes) (length dimensions)) () "lazy-nd-ref error. Incompatible indexes: ~A Should be ~D dimensions" indexes (length dimensions))
    (loop for i from 0
          for dimension in dimensions
          for index in indexes
          do (assert (<= 0 index (1- dimension))
              ()
              "lazy-nd-ref error. Dimension ~D value: ~D is out of range (0-~D)" i index (1- dimension)))) )

;;;
;;;    Too klunky...
;;;    
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

(defmethod lazy-nd-array-fill ((a lazy-nd-array) val)
  (setf (fill-flag a) t
        (fill-value a) val)
  (labels ((fill-array (a)
             (dotimes (i (length a))
               (if (vectorp (aref a i))
                   (fill-array (aref a i))
                   (unless (delayed-p (aref a i))
                     (setf (aref a i) val)))) ))
    (with-slots (contents) a
      (fill-array contents))))
