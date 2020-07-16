;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               symmetric-learner.lisp
;;;;
;;;;   Started:            Sat Mar 17 20:13:05 2012
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   Simplifies the learning process by reducing board states to their fundamentals.
;;;;   In essence, there are only 3 types of cells: center, corner, edge.
;;;;
;;;;   The response to the human player opening with the upper-left corner is no different
;;;;   than opening with the lower-right corner. This SYMMETRIC-LEARNER rotates the initial
;;;;   state so that there are effectively only 3 opening moves.
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
;;;;   Relies on vector syntax from lang.lisp: [1 2 3]
;;;;
(in-package :tic-tac-toe)

(defclass symmetric-learner (learner)
  ((rotation-map :initform nil)))

(defmethod play :before ((l symmetric-learner) &optional trainer)
  (with-slots (rotation-map) l
    (setf rotation-map nil)))

; Opening center move?!

(defmethod get-next-state :before ((l symmetric-learner) state move)
  (with-slots (rotation-map) l
    (when (null rotation-map)
      (canonicalize l state move))))

(defun canonicalize (learner state move)
  (with-slots (rotation-map) learner
    (setf rotation-map
          (rotate (case move
                    ((2 5) 2)
                    ((8 7) 4)
                    ((6 3) 6)
                    (otherwise 0)))) ))

(defmethod get-next-state ((l symmetric-learner) state move)
  (with-slots (rotation-map) l
    (let ((next-state (row-major-aref state (actual->rotated rotation-map move))))
      (if (arrayp next-state)
          next-state
          nil))))

(defmethod possible-moves ((l symmetric-learner) state)
  (with-slots (rotation-map) l
    (let ((moves (call-next-method)))
      (report moves)
      (mapcar #'(lambda (move) (rotated->actual rotation-map move)) moves))))

(defmethod commit ((l symmetric-learner) state player position)
  (with-slots (rotation-map) l
    (let ((new-state (make-new-state state))
          (rotated-position (actual->rotated rotation-map position)))
      (report "Commit ~D -> ~D~%" position rotated-position)
      (setf (row-major-aref state rotated-position) new-state
            (row-major-aref new-state rotated-position) (player-char player))
      new-state)))

(defmethod roll-back ((l symmetric-learner) rollbacks)
  (print "[SL] Rolling back")
  (if (null rollbacks)
      'self-destruct
      (destructuring-bind ((state i) . further-rollbacks) rollbacks
        (with-slots (rotation-map) l
          (setf (row-major-aref state (actual->rotated rotation-map i)) nil)) ; Mark the move as untenable.
        (if (null (possible-moves l state))
            (roll-back l further-rollbacks)
            state))))

;;;
;;;    Clockwise loop around center.
;;;    
(defconstant board-index-vector [0 1 2 5 8 7 6 3])

;;;
;;;    OFFSET should always be an even number!
;;;    (rotate 0) => #(0 1 2 5 8 7 6 3)
;;;    (rotate 2) => #(2 5 8 7 6 3 0 1)
;;;    
(defun rotate (offset)
  "Returns a vector of indexes rotated by OFFSET positions."
  (assert (evenp offset))
  (let* ((length (length board-index-vector))
         (result (make-array length)))
    (dotimes (i length result)
      (setf (aref result i) (aref board-index-vector (mod (+ i offset) length)))) ))

(defun rotated->actual (rotated i)
  (if (= i 4)
      i
      (aref rotated (position i board-index-vector))))

;;;
;;;    Map board position in rotated board back to position in actual board.
;;;    Example:
;;;    Board rotated by 2 positions
;;;     2 | 5 | 8               6 | 3 | 0
;;;    -----------             -----------
;;;     1 | 4 | 7      =>       7 | 4 | 1
;;;    -----------             -----------
;;;     0 | 3 | 6               8 | 5 | 2
;;;     
(defun actual->rotated (rotated i)
  (if (= i 4)
      i
      (aref board-index-vector (position i rotated))))

(defun rotate-board (board offset)
  (let ((result (make-array '(3 3))); :element-type 'character)))
        (rotated-indexes (rotate offset)))
    (dotimes (i (array-total-size result) result)
      (setf (row-major-aref result i) (row-major-aref board (actual->rotated rotated-indexes i)))) ))


