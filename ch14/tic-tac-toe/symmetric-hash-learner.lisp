;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               symmetric-hash-learner.lisp
;;;;
;;;;   Started:            Mon Apr  9 21:14:10 2012
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
(in-package :tic-tac-toe)

(defclass symmetric-hash-learner (hash-learner symmetric-learner)
  ())

(defmethod get-next-state ((l symmetric-hash-learner) state move)
  (with-slots (rotation-map) l
    (let ((rotated-move (actual->rotated rotation-map move)))
      (call-next-method l state rotated-move))))

;;     (or (evaluate-next-state l state rotated-move 'human)
;;         (evaluate-next-state l state rotated-move 'computer))))
;      (error "How did we get here?")))
  
(defmethod commit ((l symmetric-hash-learner) state player position)
  (with-slots (rotation-map) l
    (let ((rotated-position (actual->rotated rotation-map position)))
      (report "Commit ~D -> ~D~%" position rotated-position)
      (call-next-method l state player rotated-position))))

;; (defmethod possible-moves ((l symmetric-hash-learner) state)
;;   (let ((moves (call-next-method)))
;;     (report moves)
;;     (mapcar #'(lambda (move) (rotated->actual (slot-value l 'rotation-map) move)) moves)))

