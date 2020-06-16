#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               solitaire.lisp
;;;;
;;;;   Started:            Wed May 13 02:54:12 2020
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
;;;;   https://en.wikipedia.org/wiki/Klondike_(solitaire)
;;;;   https://en.wikipedia.org/wiki/Glossary_of_patience_terms
;;;;
(load "/home/slytobias/lisp/packages/lang")
(load "/home/slytobias/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/packages/cards.lisp")

(defpackage :solitaire (:use :common-lisp :lang :test))

(in-package :solitaire)

(defconstant tableau-rows 40)

(defclass game ()
  ((stock :reader stock :initform (make-instance 'cards:deck))
   (waste :reader waste :initform '())
   (foundations :reader foundations :initform (make-array 4 :initial-element '()))
   (tableau :reader tableau)))

(defmethod initialize-instance :after ((g game) &rest initargs)
  (declare (ignore initargs))
  (with-slots (tableau) g
    (setf tableau (make-tableau g))))

;;;
;;;    Deal 3 cards from stock to waste.
;;;    
(defgeneric deal (game))
(defmethod deal ((g game))
  (with-slots (stock waste) g
    (loop repeat 3
          until (cards:emptyp stock)
          for card = (cards:deal stock)
          do (cards:turn-up card)
             (push card waste))))

(defmethod deal :around ((g game))
;  (assert (not (zerop (cards:remaining (stock g)))) )
;  (when (zerop (cards:remaining (stock g)))
  (when (cards:emptyp (stock g))
    (recycle-waste g))
  (call-next-method))

;;; Debug only??
(defgeneric deal-all (game))
(defmethod deal-all ((g game))
  (with-slots (stock) g
    (loop until (cards:emptyp stock)
;    (loop until (zerop (cards:remaining stock))
          do (deal g))))

(defgeneric recycle-waste (game))
(defmethod recycle-waste ((g game))
  (with-slots (stock waste) g
    (let ((reverse-waste '()))
      (loop until (null waste)
            do (push (pop waste) reverse-waste))
      (dolist (card reverse-waste)
        (cards:turn-down card)
        (cards:add stock card)))) )

(defmethod recycle-waste :around ((g game))
;  (assert (zerop (cards:remaining (stock g))))
  (assert (cards:emptyp (stock g)))
  (call-next-method))

(defclass tableau ()
  ((piles :reader piles)))

(defun make-tableau (game)
  (let ((p (make-array 7 :initial-element '())))
    (with-slots (stock) game
      (loop for i from 0 below (length p)
            do (loop for j from i below (length p)
                     do (push (cards:deal stock) (aref p j)))) )
    (loop for i from 0 below (length p)
          do (cards:turn-up (first (aref p i))))
    (let ((tableau (make-instance 'tableau)))
      (with-slots (piles) tableau
        (setf piles p))
      tableau)))

(defun render-card (card streams i)
  (let ((rank (cards:rank card)))
    (format (aref streams i) "+---+  ")
    (format (aref streams (+ i 1)) "|~2A~A|  " (if (numberp rank) rank (char (symbol-name rank) 0)) (cards:label card))
    (format (aref streams (+ i 2)) "|   |  ")
    (format (aref streams (+ i 3)) "+---+  ")))

(defun render-partial-card (card streams i)
  (let ((rank (cards:rank card)))
    (cond ((cards:face-up card)
           (format (aref streams i) "+---+  ")
           (format (aref streams (+ i 1)) "|~2A~A|  " (if (numberp rank) rank (char (symbol-name rank) 0)) (cards:label card)))
          (t (format (aref streams i) "+---+  ")
             (format (aref streams (+ i 1)) "|\\\\\\|  ")))) )

(defun render-waste-card (card streams)
  (let ((rank (cards:rank card)))
    (format (aref streams 0) "+---+  ")
    (format (aref streams 1) "|~2A |  " (if (numberp rank) rank (char (symbol-name rank) 0)))
    (format (aref streams 2) "|~A  |  " (cards:label card))
    (format (aref streams 3) "+---+  ")))

(defun render-partial-waste-card (card streams)
  (let ((rank (cards:rank card)))
    (format (aref streams 0) "+--")
    (format (aref streams 1) "|~2A" (if (numberp rank) rank (char (symbol-name rank) 0)))
    (format (aref streams 2) "|~A " (cards:label card))
    (format (aref streams 3) "+--")))

(defun render-pile (pile streams)
  (let ((row-count 0))
    (unless (null pile)
      (let ((cards (reverse (rest pile))))
        (loop for card in cards
           for row = 0 then (+ row 2)
           do (render-partial-card card streams row)
              (incf row-count 2))
        (render-card (first pile) streams row-count)
        (incf row-count 4)))
    (loop for i from row-count below tableau-rows
          do (format (aref streams i) "       "))))

(defun render-blank-foundation (streams)
  (format (aref streams 0) "+---+  ")
  (format (aref streams 1) "|   |  ")
  (format (aref streams 2) "|   |  ")
  (format (aref streams 3) "+---+  "))

(defun render-blank-stock (streams)
  (render-blank-foundation streams))

(defun render-stock (stock streams)
;  (if (zerop (cards:remaining stock))
  (if (cards:emptyp stock)
      (render-blank-stock streams)
      (progn
        (format (aref streams 0) "+---+  ")
        (format (aref streams 1) "|\\\\\\|  ")
        (format (aref streams 2) "|\\\\\\|  ")
        (format (aref streams 3) "+---+  "))))

(defun render-waste-space (streams)
  (dotimes (i 4)
    (format (aref streams i) "           ")))

(defun render-space (streams)
  (dotimes (i 4)
    (format (aref streams i) "       ")))

;;;
;;;    The tableau must be printed row-by-row, but the piles are arranged as columns.
;;;    To accomplish this we define an array of STRING-OUTPUT-STREAMs--one per row--and append partial rows
;;;    corresponding to each pile. Once each pile has been appended, the rows of the tableau
;;;    can be printed.
;;;    
(defun render-tableau (tableau)
  (with-slots (piles) tableau
    (let ((display (make-array tableau-rows)))
      (loop for row from 0 below (length display) do (setf (aref display row) (make-string-output-stream)))
      (dotimes (i 7)
        (render-pile (aref piles i) display))
      (render-tableau-label)
      (dotimes (i tableau-rows) (write-line (get-output-stream-string (aref display i)))) )))

(defun render-waste (waste display)
  (if (null waste)
      (render-waste-space display)
      (destructuring-bind (&optional first second third &rest more) waste
        (declare (ignore more))
        (dolist (card (list third second))
          (unless (null card)
            (render-partial-waste-card card display)))
        (render-waste-card first display))))

(defun render-foundation (foundation streams)
  (if (null foundation)
      (render-blank-foundation streams)
      (render-card (first foundation) streams 0)))

(defun render-foundations (foundations display)
  "Print current state of the given FOUNDATIONS. Order of suits is arbitrary."
  (loop for foundation across foundations
     do (render-foundation foundation display)))

(defun render-foundations-waste-stock (game)
  (let ((display (make-array 4)))
    (loop for row from 0 below (length display) do (setf (aref display row) (make-string-output-stream)))
    (render-foundations (foundations game) display)
    (render-space display)
    (render-waste (waste game) display)
    (render-stock (stock game) display)
    (dotimes (i 4) (write-line (get-output-stream-string (aref display i)))) ))

(defun render-tableau-label ()
  "Number the piles in the tableau."
  (format t "~%  1      2      3      4      5      6      7~%"))

(defun render-game (game)
  (render-foundations-waste-stock game)
  (render-tableau (tableau game)))

;;;
;;;    Is CARD2 compatible _on_ CARD1? (Not symmetric!)
;;;    Compatible suits, descending order.
;;;    
(defgeneric compatiblep (card1 card2))
(defmethod compatiblep ((c1 null) (c2 cards:card))
  (eq (cards:rank c2) 'cards:king))
(defmethod compatiblep ((c1 cards:card) (c2 cards:card))
  (with-slots ((rank1 cards:rank) (suit1 cards:suit)) c1
    (with-slots ((rank2 cards:rank) (suit2 cards:suit)) c2
      (case suit1
        ((cards:hearts cards:diamonds) 
         (case suit2
           ((cards:hearts cards:diamonds) nil)
           (otherwise (compatible-rank-p rank1 rank2))))
        ((cards:clubs cards:spades)
         (case suit2
           ((cards:clubs cards:spades) nil)
           (otherwise (compatible-rank-p rank1 rank2)))) ))))

;;;
;;;    Is CARD2 compatible _on_ CARD1 on a foundation?
;;;    Different definition than above, i.e., matching suit, ascending order.
;;;    
(defgeneric foundation-compatible-p (card1 card2))
(defmethod foundation-compatible-p ((c1 null) (c2 cards:card))
  (eq (cards:rank c2) 'cards:ace))
(defmethod foundation-compatible-p ((c1 cards:card) (c2 cards:card))
  (with-slots ((rank1 cards:rank) (suit1 cards:suit)) c1
    (with-slots ((rank2 cards:rank) (suit2 cards:suit)) c2
      (and (eq suit1 suit2)
           (compatible-rank-p rank2 rank1)))) )

;;;
;;;   Is RANK1 higher than RANK2 and consecutive?
;;;   (Aces low.)
;;;   
(defun compatible-rank-p (rank1 rank2)
  (eql rank1 (second (member rank2 '(cards:ace 2 3 4 5 6 7 8 9 10 cards:jack cards:queen cards:king)))) )

(defun move (game from-pile to-pile)
  (with-slots (tableau) game
    (with-slots (piles) tableau
      (cond ((eq from-pile 'w) (if (eq to-pile 'f)
                                   (move-waste-to-foundations game)
                                   (move-waste-to-pile game to-pile)))
            ((eq to-pile 'f) (move-pile-to-foundations game from-pile)); to-pile))
            (t (move-pile-to-pile piles from-pile to-pile)))) ))

(defun move-pile-to-pile (piles from-pile to-pile)
  (let ((column '())
        (destination-card (first (aref piles to-pile))))
    (unless (null (aref piles from-pile))
      (loop for card = (first (aref piles from-pile))
            while (cards:face-up card)
            do (push (pop (aref piles from-pile)) column)
            until (compatiblep destination-card card))
      (cond ((or (null column)
                 (not (compatiblep destination-card (first column)))) ; Bad move! Restore from pile
             (dolist (card column)
               (push card (aref piles from-pile))))
            (t (unless (or (null (aref piles from-pile))
                           (cards:face-up (first (aref piles from-pile))))
                 (cards:turn-up (first (aref piles from-pile))))
               (dolist (card column)
                 (push card (aref piles to-pile)))) ))))

(defun move-waste-to-pile (game to-pile)
  (with-slots (waste tableau) game
    (with-slots (piles) tableau
      (let ((destination-card (first (aref piles to-pile))))
        (when (compatiblep destination-card (first waste))
          (push (pop waste) (aref piles to-pile)))) )))

(defun move-pile-to-foundations (game from-pile)
  (with-slots (foundations tableau) game
    (with-slots (piles) tableau
      (let* ((card (first (aref piles from-pile))) ; Empty pile???
             (existing-foundation (find-foundation foundations (cards:suit card))))
        (if existing-foundation
            (when (foundation-compatible-p (first (aref foundations existing-foundation)) card)
              (push card (aref foundations existing-foundation))
              (pop (aref piles from-pile))
              (unless (null (aref piles from-pile))
                (cards:turn-up (first (aref piles from-pile)))) )
            (when (eq (cards:rank card) 'cards:ace)
              (push card (aref foundations (next-available-foundation foundations)))
              (pop (aref piles from-pile))
              (unless (null (aref piles from-pile))
                (cards:turn-up (first (aref piles from-pile)))) )))) ))

(defun move-foundation-to-piles (game foundation to-pile)
  (with-slots (foundations tableau) game
    (with-slots (piles) tableau
      (let ((destination-card (first (aref piles to-pile))) ; Empty pile???
            (card (first (aref foundations foundation))))
        (when (and destination-card
                   card 
                   (compatiblep destination-card card))
          (push card (aref piles to-pile))
          (pop (aref foundations foundation)))) )))

(defun move-waste-to-foundations (game)
  (with-slots (foundations waste) game
    (let* ((card (first waste))
           (foundation (or (find-foundation foundations (cards:suit card)) (next-available-foundation foundations)))) ; Why is this different??
      (when (foundation-compatible-p (first (aref foundations foundation)) (first waste))
        (push (pop waste) (aref foundations foundation)))) ))

(defun find-foundation (foundations suit)
  (dotimes (i (length foundations) nil)
    (when (and (not (null (aref foundations i)))
               (eq (cards:suit (first (aref foundations i))) suit))
      (return i))))

(defun next-available-foundation (foundations)
  (dotimes (i (length foundations) nil)
    (when (null (aref foundations i))
      (return i))))

;;;
;;;    Only handles one pass at a time.
;;;    
(defun auto-complete (game)
  (with-slots (foundations tableau stock waste) game
    (with-slots (piles) tableau
      (when (and (cards:emptyp stock) (null waste))
        (loop for pile across piles
              for i from 0
              unless (null pile)
              do (move-pile-to-foundations game i)))) ))

(defun read-move () 
  (let ((input (make-string-input-stream (prompt-read "Move: " :allow-empty nil))))
    (loop for arg = (read input nil nil) until (null arg) collect arg)))

(defun process-move (game args)
  (case (length args)
    (1 (case (first args)
         (q (throw 'game 'quit))
         (a (auto-complete game))
         (d (deal game))
         (f (move-waste-to-foundations game))
         ((1 2 3 4 5 6 7) (move-waste-to-pile game (1- (first args)))) ))
    (2 (destructuring-bind (src dest) args
         (case dest
           (f (move-pile-to-foundations game (1- src)))
           ((1 2 3 4 5 6 7) (case src
                              ((1 2 3 4 5 6 7) (move-pile-to-pile (piles (tableau game)) (1- src) (1- dest)))) ))))
    (3 (destructuring-bind (src index dest) args
         (ecase src
           (f (move-foundation-to-piles game (1- index) (1- dest)))) ))
    (otherwise (format t "Whaaa?~%"))))

(defun play ()
  (let ((game (make-instance 'game)))
    (catch 'game
      (loop
         (render-game game)
         (process-move game (read-move)))) ))

(play)

