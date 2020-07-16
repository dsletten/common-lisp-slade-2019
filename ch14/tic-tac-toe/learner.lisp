;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               learner.lisp
;;;;
;;;;   Started:            Sat Mar 10 01:43:34 2012
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   The basic LEARNER class encodes its experience as a "tree" of nested arrays of arrays.
;;;;   These represent paths encountered in previous play.
;;;;
;;;;   The initial STATE is simply a virgin board where any move is possible:
;;;;   #2A((T T T) (T T T) (T T T))
;;;;   Assume that the human opponent opens with the upper left hand cell (0,0) or index 0. This
;;;;   generates a new state for the game wherein the first cell is no longer available:
;;;;   #2A((#\X T T) (T T T) (T T T))
;;;;   This nested state is then encoded in the top-level state as the first exploration of what
;;;;   could happen when the human makes this opening move:
;;;;   #2A((#2A((#\X T T)
;;;;            (T T T)
;;;;            (T T T)) T T)
;;;;       (T T T)
;;;;       (T T T))
;;;;
;;;;   In other words, now the 0th element of the top-level STATE is the path of subsequent states that could
;;;;   occur. Many will lead to the LEARNER's failure and will consequently be pruned with experience.
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
;;;;   - Each time program loses have it add the opponent's strategy to its repertoire for future offense.
;;;;   - For offensive program, if opp. blocks, check to make sure that strategy is in its
;;;;     defensive repertoire.
;;;;
;;;;
(in-package :tic-tac-toe)

(defclass learner ()
  ((state :initform (make-new-state))
   (game-count :reader game-count :initform 0)))

(defun make-new-state (&optional current-state)
  "Create a 3X3 board representing a particular state in a game. May copy an existing board."
  (let ((new-state (make-array '(3 3) :initial-element t)))
    (unless (null current-state)
      (dotimes (i 3)
        (dotimes (j 3)
          (when (characterp (aref current-state i j))
            (setf (aref new-state i j) (aref current-state i j)))) ))
    new-state))

;;;
;;;    Start a new game. Play ping pongs between OPPONENT-MOVE and LEARNER-MOVE until game is over.
;;;    
(defgeneric play (learner &optional trainer))
(defmethod play ((learner learner) &optional (trainer (make-instance 'human-trainer)))
  (with-slots (state) learner
    (opponent-move learner state trainer (make-game))))

(defmethod play :after ((learner learner) &optional trainer)
  (declare (ignore trainer))
  (with-slots (game-count) learner
    (incf game-count)))

(defun train (learner count &optional (trainer (make-instance 'human-trainer)))
  (let ((wins 0)
        (losses 0))
    (repeat count
      (let ((result (play learner trainer)))
        (cond ((eq result :computer) (incf wins))
              ((eq result :human) (incf losses)))) )
    (format t "Games played: ~D~%" count)
    (format t "Games won: ~D~%" wins)
    (format t "Games lost: ~D~%" losses)))
  
(defun opponent-move (learner state trainer game &optional rollbacks)
  (let ((move (get-move trainer game)))
    (make-move game :human move)
    (if (game-over-p game)
        (progn (print game)
               (when (wins game :human) ; Game may have been a tie!
                 (format t "Human wins! Learning...~%")
                 (report (roll-back learner rollbacks))
                 :human))
        (let ((new-state (or (get-next-state learner state move)
                             (commit learner state :human move))))
          (learner-move learner new-state trainer game rollbacks)))) )

(defgeneric get-next-state (learner state move)
  (:documentation "Look for a nested subarray representative of the next game state to examine."))
(defmethod get-next-state ((l learner) state move)
  (declare (ignore l)) ; ????
  (let ((next-state (row-major-aref state move)))
    (if (arrayp next-state)
        next-state
        nil)))

(defgeneric commit (learner state player position)
  (:documentation "Add a nested subarray to current state representative of a previously unexplored path. The new state is a copy of current state with the new move recorded."))
(defmethod commit ((l learner) state player position)
  (declare (ignore l)) ; ????
  (let ((new-state (make-new-state state)))
    (setf (row-major-aref state position) new-state
          (row-major-aref new-state position) (player-char player))
    new-state))

;;;
;;;    Unless game has ended, each call to LEARNER-MOVE makes another call to OPPONENT-MOVE.
;;;    With each call, a list of the STATE and the current MOVE are added to the top of the
;;;    stack of ROLLBACKS to be undone if the learner loses.
;;;    
(defun learner-move (learner state trainer game rollbacks)
  (let ((moves (possible-moves learner state)))
    (report "Possible moves: ~A~%" moves)
    (if (null moves)
;;         rollbacks ; Should not happen! <-- Could happen with hash-learner. Different paths to same state.
        (progn (format t "I have reached a hopeless position~%")
               (roll-back learner rollbacks))
        (let ((move (choose-move moves)))
          (make-move game :computer move)
          (print game)
          (let ((new-state (or (get-next-state learner state move)
                               (commit learner state :computer move))))
            (if (game-over-p game)
                (progn (format t "Trainee wins!~%")
                       (report new-state)
                       :computer)
                (opponent-move learner new-state trainer game (cons (list state move) rollbacks)))) ))))

(defun choose-move (moves)
  "Learner always chooses a random move from among the possible."
  (elt moves (random (length moves))))

(defgeneric possible-moves (learner state) 
  (:documentation "Given the current STATE, determine which moves are still available. An available position is either T (previously unexplored path) or a subarray (previously explored path)."))
(defmethod possible-moves ((l learner) state)
  (declare (ignore l)) ; ????
  (loop for i from 0 below (array-total-size state)
        unless (or (null (row-major-aref state i))
                   (characterp (row-major-aref state i)))
        collect i))

;;;
;;;    ROLLBACKS is a stack of STATE/MOVE pairs. This function undoes the latest move
;;;    that led to failure. If no possible moves remain in that STATE it is abandoned
;;;    and the learner rolls back recursively to the next choice point.
;;;    Returns the latest STATE that still has some moves available.
;;;    
(defgeneric roll-back (learner rollbacks))
(defmethod roll-back ((l learner) rollbacks)
  (print "Rolling back")
  (if (null rollbacks)
      'self-destruct
      (destructuring-bind ((state i) . further-rollbacks) rollbacks
        (setf (row-major-aref state i) nil) ; Mark the move as untenable.
        (if (null (possible-moves l state))
            (roll-back l further-rollbacks)
            state))))

(defun print-state (state)
  (let ((queue (collections:make-linked-queue)))
    (format t (with-output-to-string (result)
      (dotimes (i (array-total-size state))
        (cond ((arrayp (row-major-aref state i))
               (format result " | ")
               (collections:enqueue queue (row-major-aref state i)))
              (t (format result " ~A " (row-major-aref state i)))) )
      (format result "~%")))
    (dolist (sub-array (collections:elements queue))
      (print-state sub-array))))
    

(defun compute-array-depth (obj)
  (cond ((arrayp obj) (1+ (apply #'max (loop for i from 0 to (array-total-size obj)
                                             collect (compute-array-depth (row-major-aref obj i)))) ))
        (t 0)))

;; (compute-array-depth "asdfa") => 1
;; (compute-array-depth [0 1 [2 3] [4 [5]]]) => 3

(defun get-terminal-row (state moves)
  (cond ((null moves) state)
        (t (get-terminal-row (aref state (floor (first moves) 3) (mod (first moves) 3)) (rest moves)))) )

(defgeneric serialize (learner filename))
(defmethod serialize ((l learner) (filename string))
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-slots (game-count state) l
      (print game-count out)
      (print state out))))

(defun deserialize (filename)
  (with-open-file (in filename)
    (let ((result (make-instance 'learner))) ; What kind?
      (with-slots (game-count state) result
        (setf game-count (read in)
              state (read in)))
      result)))
