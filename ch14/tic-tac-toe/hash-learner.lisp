;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               hash-learner.lisp
;;;;
;;;;   Started:            Sun Apr  1 02:19:22 2012
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   Unlike the nested subarrays of the LEARNER class, the HASH-LEARNER maintains a map from hashes of boards
;;;;   to the corresponding boards (see below). As each move is added to a given state, a new hash is computed
;;;;   to locate the next state to examine.
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
;;;;   Each potential board state is encoded as a unique product of prime numbers. Each cell
;;;;   is associated with a prime, which is raised to the 0th, first, or second power depending
;;;;   on whether that cell is empty, occupied by the human, or occupied by the computer, respectively.
;;;;
;;;;   2 |  3 |  5  
;;;; --------------
;;;;   7 | 11 | 13
;;;; --------------
;;;;  17 | 19 | 23
;;;;
;;;; 1 => #2A((T T T) (T T T) (T T T))              Empty board (2^0)*(3^0)*...
;;;; 2 => #2A((#\X T T) (T T T) (T T T))            (2^1)
;;;; 4 => #2A((#\O T T) (T T T) (T T T))            (2^2)
;;;; 578 => #2A((#\X T T) (T T T) (#\O T T))        (2^1)*(17^2)
;;;; 6358 => #2A((#\X T NIL) (T #\X T) (#\O T T))   (2^1)*(11^1)*(17^2)
;;;; 158950 => #2A((#\X T #\O) (T #\X T) (#\O T T)) (2^1)*(5^2)*(11^1)*(17^2)
;;;;
;;;;   The program doesn't do this, but it could detect winning rows/columns/diagnoals by factoring:
;;;;   (hash-board #2A((T T T) (T T T) (#\X #\X #\X))) => 7429
;;;;   Thus any board that was a multiple of 7429 would have the human holding the bottom row.
;;;;   (hash-board #2A((#\O T T) (T T #\O) (#\X #\X #\X))) => 5022004
;;;;   (/ 5022004 7429) => 676

(in-package :tic-tac-toe)

(defconstant primes [2 3 5 7 11 13 17 19 23])

(defun power (player)
  (case player
    (:human 1)
    (:computer 2)
    (otherwise 0)))

(defun hash-board (board)
  "Given a 3X3 BOARD, compute its hash as a product of primes based on occupied cells."
  (do ((size (array-total-size board))
       (i 0 (1+ i))
       (product 1 (* product (expt (svref primes i) (power (char-player (row-major-aref board i)))) )))
      ((= i size) product)))

(deftest test-hash-board ()
  (check
   (= (hash-board #2A((T T T) (T T T) (T T T))) 1)
   (= (hash-board #2A((#\X T T) (T T T) (T T T))) 2)
   (= (hash-board #2A((#\O T T) (T T T) (T T T))) 4)
   (= (hash-board #2A((#\X #\X #\X) (T T T) (T T T))) 30) ; Winning rows for human (Board would be a multiple of 30, 1001, 7429)
   (= (hash-board #2A((T T T) (#\X #\X #\X) (T T T))) 1001)
   (= (hash-board #2A((T T T) (T T T) (#\X #\X #\X))) 7429)))

(defun copy-board (board)
  (let ((new-state (make-array '(3 3) :initial-element t)))
    (dotimes (i 3)
      (dotimes (j 3)
        (when (characterp (aref board i j))
          (setf (aref new-state i j) (aref board i j)))) )
    new-state))

(defun char-player (ch)
  (case ch
    (#\X :human)
    (#\O :computer)
    (otherwise :empty)))

(defclass hash-learner (learner)
  ((state-hash :initform {})))

;;;
;;;    Initial empty board has a hash of 1. See above.
;;;    
(defmethod initialize-instance :after ((l hash-learner) &key)
  (with-slots (state-hash state) l
    (setf (gethash 1 state-hash) state)))

(defmethod play ((learner hash-learner) &optional (trainer (make-instance 'human-trainer)))
  (with-slots (state-hash) learner
    (opponent-move learner (gethash 1 state-hash) trainer (make-game))))

(defmethod get-next-state ((l hash-learner) state move)
  (or (evaluate-next-state l state move :human)
      (evaluate-next-state l state move :computer)))
;      (error "How did we get here?")))

(defun add-move-to-state (state move player)
  (let ((new-state (copy-board state)))
    (setf (row-major-aref new-state move) (player-char player))
    new-state))

(defun evaluate-next-state (learner state move player)       
  (with-slots (state-hash) learner
    (let ((new-state (add-move-to-state state move player)))
      (gethash (hash-board new-state) state-hash))))

(defmethod commit ((l hash-learner) state player position)
  (with-slots (state-hash) l
    (let ((new-state (add-move-to-state state position player)))
      (setf (gethash (hash-board new-state) state-hash) new-state)
      (report "Commit ~D~%" position)
      (report new-state)
      new-state)))

;; (defun evaluate-next-state (learner state move player)       
;;   (with-slots (state-hash) learner
;;     (let ((new-state (copy-board state)))
;;       (setf (row-major-aref new-state move) (player-char player))
;;       (gethash (hash-board new-state) state-hash))))

;; (defmethod commit ((l hash-learner) state player position)
;;   (let ((new-state (copy-board state)))
;;     (setf (row-major-aref new-state position) (player-char player))
;;     (let ((key (hash-board new-state)))
;;       (setf (gethash key (slot-value l 'state-hash)) new-state))
;;     (report "Commit ~D~%" position)
;;     (report new-state)
;;     new-state))

(defun trace-game (learner &rest moves)
  (with-slots (state-hash) learner
    (let ((board (make-array '(3 3) :initial-element t))
          (human-player t))
      (dolist (move moves)
        (if human-player
            (setf (row-major-aref board move) #\X)
            (setf (row-major-aref board move) #\O))
        (setf human-player (not human-player))
        (print (hash-board board))
        (terpri)
        (print-board (gethash (hash-board board) state-hash))
        (terpri)))) )



