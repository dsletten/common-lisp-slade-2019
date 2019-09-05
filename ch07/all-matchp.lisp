;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               all-matchp.lisp
;;;;
;;;;   Started:            Mon Aug 26 21:56:41 2019
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   Compare the MATCHP functions used for Eliza (capture matches).
;;;;   See also all-matchp.lisp in ch.4 (~/lisp/books/Slade/2019/ch04/all-matchp.lisp)
;;;;   for simpler functions that don't capture matches.
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
;;;;       Four levels of sophistication here.
;;;;       1. Succeed/fail without capturing matches.
;;;;       2. Capture all matches (including for multiple wildcards) in single result.
;;;;       3. Correctly capture matches for each wildcard in separate groups. Return () for successful
;;;;          match with no capture. The symbol FAIL indicates failure instead.
;;;;       4. Similar to 3. but T returned for basic match, NIL for failure.
;;;;       5. Multiple values returned. Primary value indicates match/fail. Secondary value includes
;;;;          capture groups for successful match.
;;;;
;;;;    Versions #1d and #7b are best of breed. Multiple-valued. Primary value indicates success or failure of match.
;;;;    Secondary value contains capture groups.
;;;;
(load "/Users/dsletten/lisp/packages/test.lisp")
;(load "/Users/dsletten/lisp/packages/collections.lisp")

(defpackage :all-matchp (:use :common-lisp :test))
;(defpackage :all-matchp (:use :common-lisp :test :collections))

(in-package :all-matchp)

;;;
;;;    Canonical (non-capturing) MATCHP
;;;    
;;;
;;;   5 cases:
;;;   1. PATTERN is '()
;;;   2. PATTERN has wild first elt.
;;;      2a. INPUT is '(). (Any remaining elts in PATTERN must be wild for match to succeed, i.e.,
;;;          eventually case 1. Otherwise, eventually case 3.)
;;;      2b. 0 elts in INPUT match wildcard. Try to match rest of PATTERN.
;;;      2c. 1+ elts in INPUT match wildcard . Try to match wildcard against elts of INPUT.
;;;
;;;           Does order of 2b, 2c matter????            <--- It matters for capturing
;;;           With 2b first:                (MATCHP '(A B *WILD* C D *WILD*) '(A B C D C D C D)) => (NIL (C D C D))
;;;           With 2c first (Greedy match): (MATCHP '(A B *WILD* C D *WILD*) '(A B C D C D C D)) => ((C D C D) NIL)
;;;
;;;   The remaining cases invovle a PATTERN with a literal first elt.
;;;   3. PATTERN is neither empty nor has a wild first elt. An empty INPUT list has nothing to
;;;      match the first elt of PATTERN, thus the match fails. This case would be eliminated by
;;;      case 4. except in the pathological case where pattern contains NIL (the empty list)
;;;      as an elt. We must distinguish an empty INPUT rather than match (FIRST '()) w/
;;;      (FIRST '(() ...)).
;;;   Neither PATTERN nor INPUT is empty below.
;;;   4. PATTERN has literal first elt that matches first elt of INPUT. Keep checking.
;;;   5. PATTERN has literal first elt that does not match first elt of INPUT. Fail.
;;;
;;;   Note: 2a. and 2b. must be handled separately, otherwise
;;;         2a. will mistakenly fall through to 2c. and into an
;;;         infinite loop.
;;;
;;;         Specifically, (matchp '(*wild* a) '()) must fail at 2a. not 2b.
;;;
;; (defun matchp (pattern input)
;;   (cond ((endp pattern) (endp input))  ;1
;;         ((wildp (first pattern)) ;2
;;          (if (endp input)
;;              (matchp (rest pattern) input) ;2a
;;              (or (matchp (rest pattern) input) ;2b
;;                  (matchp pattern (rest input)))) ) ;2c
;;         ((endp input) nil) ;3
;;         ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input))) ;4
;;         (t nil))) ;5

;;; ================================================================================================================================================================
;;;;   Started:            Fri Oct 22 16:50:38 2004
;;;
;;;    Modified based on Slade's version. If there are more than one *wild*
;;;    instance in a pattern, all matching elements get put in one list:
;;;    (matchp '(*wild* like *wild* coach) '(i don't like my crazy coach)) =>
;;;      (I DON 'T MY CRAZY)
;;;
;;;    This is fundamentally the same as the 2nd 2010 version. (Aside from not handling (NULL I) quite right.)
;;;    
(defun matchp (pattern input)
  "Determine whether INPUT matches PATTERN. PATTERN may include wild cards."
  (assert (listp pattern)
          (pattern)
          "PATTERN should be a list.")
  (assert (listp input)
          (input)
          "INPUT should be a list.")
  (labels ((matchp-aux (p i)
             (cond ((null p) (null i))
                   ((null i) (equal p '(*wild*)))
                   ((equal p '(*wild*)) i)
                   ((equal (car p) (car i)) (matchp-aux (cdr p) (cdr i)))
                   ((equal (car p) '*wild*)
                    (or (matchp-aux (cdr p) i)
                        (let ((result (matchp-aux p (cdr i))))
                          (if (not (null result))
                              (if (listp result)
                                  (cons (car i) result)
                                  (list (car i)))
                              nil))))
                   (t nil))))
    (matchp-aux pattern input)))

;;; ================================================================================================================================================================
;;;;   Started:            Wed Sep  8 16:06:07 2010
(defun wildp (obj)
  (eq obj '*wild*))

;;;
;;;    New version of MATCHP returns true for match and false for no match.
;;;    Specifically, for match with wildcard, returns subsequence of input that
;;;    matched the wildcard.
;;;
;;;    Assumes single wildcard per pattern at most. (Matches to multiple wildcards all combined in single list!)
;;;
;;;    Wildcard match with empty subsequence simply returns T:
;;;    (matchp '(a *wild*) '(a)) => T
;;;    
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))
        ((wildp (first pattern))
         (or (matchp (rest pattern) input)
             (and (not (endp input))
                  (capture (first input) (matchp pattern (rest input)))) ))
        ((endp input) nil)
        ((equal (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t nil)))

(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))
        ((endp input) (and (wildp (first pattern))
                           (matchp (rest pattern) input)))
        ((wildp (first pattern)) (or (matchp (rest pattern) input)
                                     (capture (first input) (matchp pattern (rest input)))) )
        ((equal (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t nil)))

;;
;;    Canonical
;;    
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))
        ((wildp (first pattern))
         (if (endp input)
             (matchp (rest pattern) input)
             (or (matchp (rest pattern) input)
                 (capture (first input) (matchp pattern (rest input)))) ))
        ((endp input) nil)
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input))) 
        (t nil)))

;;;
;;;    This CAPTURE is incompatible with later function definition!
;;;    
(defun capture (elt result)
  (cond ((null result) nil)
        ((eq result t) (list elt))
        (t (cons elt result))))

;;;
;;;    New new version of MATCHP handles multiple wildcards. Returns list of lists on
;;;    success.
;;;

;;;
;;;    List returned on match, symbol FAIL otherwise.
;;;    Thus empty list is successful result.
;;;    List of lists contains matches for wild cards.
;;;
;;;    Good
;;;    (matchp '(* b c * d) '(a b b c c d c d)) => ((a b) (c d c))
;;;    
(defun failp (result)
  (eq result 'fail))

(defun matchp (pattern input)
  (cond ((endp pattern) (if (endp input) '() 'fail))
        ((wildp (first pattern))
         (cond ((endp input) (matchp (rest pattern) input))
               (t (let ((result (matchp (rest pattern) input)))
                    (if (failp result)
                        (let ((result (matchp pattern (rest input))))
                          (if (failp result)
                              result
                              (cons (cons (first input) (first result)) (rest result))))
                        (cons '() result)))) ))
        ((endp input) 'fail) ; This was the only missing clause. Otherwise this follows the canonical definition!!
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t 'fail)))

(deftest test-matchp ()
  (check
   (equal (matchp '(a b c) '(a b c)) '())
   (equal (matchp '(a b c) '(a b c d)) 'fail)
   (equal (matchp '(a b c d) '(a b c)) 'fail)
   (equal (matchp '(a *wild*) '(a b c)) '((b c)))
   (equal (matchp '(a *wild*) '(a)) '())    ;????
   (equal (matchp '(a *wild* b) '(a b c d b)) '((b c d)))
   (equal (matchp '(a *wild* b) '(a b c d e)) 'fail)
   (equal (matchp '(*wild* b *wild*) '(a b c d e)) '((a) (c d e)))
   (equal (matchp '(*wild*) '(a b c)) '((a b c)))
   (equal (matchp '(i do not like *wild* coach because he *wild* all of the time which is *wild*) '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me))
          '((MY CRAZY) (LIKES TO TELL BAD JOKES) (VERY ANNOYING TO ME)))
   (equal (matchp '(*wild* a *wild*) '(b c a d)) '((b c) (d)))
   (equal (matchp '(a b *wild* c d *wild*) '(a b c d c d c d)) '(() (c d c d)))
   (equal (matchp '(*wild* a *wild* a) '(a a a a a a a)) '(() (A A A A A)))
   (equal (matchp '(*wild* a *wild* a *wild*) '(a a a a a a a b c d)) '(() () (A A A A A B C D)))) )

;; (deftest test-matchp ()
;;   (check
;;    (matchp '(a b c) '(a b c))
;;    (not (matchp '(a b c) '(a b c d)))
;;    (not (matchp '(a b c d) '(a b c)))
;;    (matchp '(a *wild*) '(a b c))
;;    (matchp '(a *wild*) '(a))
;;    (matchp '(a *wild* b) '(a b c d b))
;;    (not (matchp '(a *wild* b) '(a b c d e)))
;;    (matchp '(*wild* b *wild*) '(a b c d e))
;;    (matchp '(*wild*) '(a b c))))

;;; ================================================================================================================================================================
;;;;   Started:            Sun Oct  2 02:01:29 2011
(defun wildp (obj)
  (eq obj '*wild*))

;;;
;;;    Single wild card capture.
;;;    (More precisely, all matches for any wildcards captured in single result list.)
;;;    
;;;  #1 This sort of "works" but not in the way it was intended. Specifically, the "accumulator" RESULT
;;;     is pointless. Every recursive call simply calls again with the value of RESULT that it received!
;;;     Nothing ever happens to RESULT! The actual result simply accrues as the recursive calls
;;;     eventually return.
;;;  
(defun matchp (pattern input)
  (labels ((matchp-aux (pattern input result)
             (cond ((endp pattern) (if (endp input)
                                       (values t result)
                                       (values nil nil)))
;             (cond ((endp pattern) (values (endp input) result))
                   ((endp input) (if (wildp (first pattern))
                                     (matchp-aux (rest pattern) input result)
                                     (values nil nil)))
                   ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result))
;                   ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
                   ((wildp (first pattern))
                    (multiple-value-bind (match result) (matchp-aux (rest pattern) input result)
                      (if match
                          (values match result)
                          (multiple-value-bind (match result) (matchp-aux pattern (rest input) result)
                            (if match
                                (values match (cons (first input) result))
                                (values nil nil)))) ))
                   (t (values nil nil)))) )
    (matchp-aux pattern input '())))

;;;
;;;  #1a (Canonical rewrite) -- Still lumps all results together...
;;;  
(defun matchp (pattern input)
  (labels ((matchp-aux (pattern input result)
             (cond ((endp pattern)
                    (if (endp input)
                        (values t result)
                        (values nil nil)))
                   ((wildp (first pattern))
                    (if (endp input)
                        (matchp-aux (rest pattern) input result)
                        (multiple-value-bind (match result) (matchp-aux (rest pattern) input result)
                          (if match
                              (values match result)
                              (multiple-value-bind (match result) (matchp-aux pattern (rest input) result)
                                (if match
                                    (values match (cons (first input) result))
                                    (values nil nil)))) )))
                   ((endp input) (values nil nil))
                   ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result))
                   (t (values nil nil)))) )
    (matchp-aux pattern input '())))

;;;
;;;  #1b Hybrid with 2010 version --- Separate matches preserved.
;;;  Multiple values
;;;
(defun matchp (pattern input)
  (cond ((endp pattern) (if (endp input)
                            (values t '())
                            (values nil nil)))
        ((wildp (first pattern))
;         (cond ((endp input) (matchp (rest pattern) input)) ; Inadequate!!
         (cond ((endp input) (multiple-value-bind (match result) (matchp (rest pattern) input)
                               (if match
                                   (values t (cons '() result))
                                   (values nil nil))))
               (t (multiple-value-bind (match result) (matchp (rest pattern) input)
                    (if match
                        (values t (cons '() result))
                        (multiple-value-bind (match result) (matchp pattern (rest input))
                          (if match
                              (values t (cons (cons (first input) (first result)) (rest result)))
                              (values nil nil)))) ))))
        ((endp input) (values nil nil))
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t (values nil nil))))

;;;    2nd wildcard grabs all of the values!
;(matchp '(*wild* *wild*) '(a b c)) => T; (NIL (A B C))

;;;
;;;  #1c Hybrid with 2010 version --- Separate matches preserved.
;;;  Multiple values
;;;
(defun matchp (pattern input)
  (cond ((endp pattern) (if (endp input)
                            (values t '())
                            (values nil nil)))
        ((wildp (first pattern)) (if (endp input)
                                     (add-empty-group-or-fail (rest pattern) input)
                                     (ignore-or-capture pattern input)))
        ((endp input) (values nil nil))
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t (values nil nil))))

(defun add-empty-group (result)
  (cons '() result))

(defun add-empty-group-or-fail (pattern input)
  (multiple-value-bind (match result) (matchp pattern input)
    (if match
        (values t (add-empty-group result))
        (values nil nil))))

(defun ignore-or-capture (pattern input)
  (multiple-value-bind (match result) (matchp (rest pattern) input)
    (if match
        (values t (add-empty-group result))
        (capture-or-fail pattern input))))

(defun capture-or-fail (pattern input)
  (multiple-value-bind (match result) (matchp pattern (rest input))
    (if match
        (values t (add-elt-to-current-group (first input) result))
        (values nil nil))))

(defun add-elt-to-current-group (elt result)
  (destructuring-bind (current-group . rest) result
    (cons (cons elt current-group) rest)))

;;;    Greedy
;;;
;;;  #1d Hybrid with 2010 version --- Separate matches preserved. Best version!!
;;;  Multiple values
;;;  Primary value is T/NIL. If successful, secondary value captures matches.
;;;
(defun matchp (pattern input)
  (cond ((endp pattern) (if (endp input)
                            (values t '())
                            (values nil nil)))
        ((wildp (first pattern)) (if (endp input)
                                     (add-empty-group-or-fail (rest pattern) input)
                                     (capture-or-ignore pattern input)))
        ((endp input) (values nil nil))
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t (values nil nil))))

(defun capture-or-ignore (pattern input)
  (multiple-value-bind (match result) (matchp pattern (rest input))
    (if match
        (values t (add-elt-to-current-group (first input) result))
        (ignore-or-fail pattern input))))

(defun ignore-or-fail (pattern input)
  (multiple-value-bind (match result) (matchp (rest pattern) input)
    (if match
        (values t (add-empty-group result))
        (values nil nil))))

;;;  #2 Kind of captures multiple matches --- backwards!
;;;  This kind of does the tail-recursive accumulator from #1 correctly.
(defun matchp (pattern input)
  (labels ((matchp-aux (pattern input result)
             (cond ((endp pattern) (if (endp input)
                                       (or result t)
                                       nil))
                   ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result))
                   ((wildp (first pattern))
                    (cond ((endp input) (cons '() result))
                          (t (or (matchp-aux (rest pattern) input (cons '() result))
                                 (matchp-aux pattern (rest input) (cons (cons (first input) (first result)) (rest result)))) )))
                   (t nil))))
    (matchp-aux pattern input '())))

;;;  #2a (Canonical rewrite)
;;;      There is an issue with the tail-recursive versions and not the others (e.g., #1c), because the
;;;      non-tail-recursive versions "capture" matches as the function returns back up the stack from
;;;      deeper calls. The necessary capture groups have already been added deeper down.
;;;
;;;      But in the tail-recursive version, the function has to "capture" the element now prior to the next
;;;      recursive call, and the first captured element must add its own group to the results.
;;;      
(defun matchp (pattern input)
  (labels ((matchp-aux (pattern input result)
             (cond ((endp pattern) (if (endp input)
                                       (or result t)
                                       nil))
                   ((wildp (first pattern))
                    (cond ((endp input) (matchp-aux (rest pattern) input (add-empty-group result))) ; ?
                          (t (or (matchp-aux (rest pattern) input (add-empty-group result)) ; <--- This can't be tail-recursive?!
                                 (matchp-aux pattern (rest input) (cons (cons (first input) (first result)) (rest result)))) ))) ; (first '()) => ()!
;                                 (matchp-aux pattern (rest input) (add-elt-to-current-group (first input) result)))) ))
                   ((endp input) nil)
                   ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result))
                   (t nil))))
    (matchp-aux pattern input '())))

;;;
;;;    2b Queue
;;;    
;; (defun matchp (pattern input)
;;   (labels ((matchp-aux (pattern input result flag)
;;              (cond ((endp pattern) (if (endp input)
;;                                        result
;;                                        nil))
;;                    ((wildp (first pattern))
;;                     (cond ((endp input) (matchp-aux (rest pattern) input (enqueue result '())
;;                           (t (or (matchp-aux (rest pattern) input (add-empty-group result)) ; <--- This can't be tail-recursive?!
;;                                  (matchp-aux pattern (rest input) (cons (cons (first input) (first result)) (rest result)))) ))) ; (first '()) => ()!
;; ;                                 (matchp-aux pattern (rest input) (add-elt-to-current-group (first input) result)))) ))
;;                    ((endp input) nil)
;;                    ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result))
;;                    (t nil))))
;;     (matchp-aux pattern input (make-linked-queue) t)

;;;
;;;  #2c
;;;  
(defun matchp (pattern input)
  (labels ((matchp-aux (pattern input result flag)
             (cond ((endp pattern) (if (endp input)
                                       (or result t)
                                       nil))
                   ((wildp (first pattern))
                    (cond ((endp input) (if flag
                                            (matchp-aux (rest pattern) input result nil)
                                            (matchp-aux (rest pattern) input (add-empty-group result) nil)))
                          (t (or (if flag
                                     (matchp-aux (rest pattern) input result nil)
                                     (matchp-aux (rest pattern) input (add-empty-group result) nil))
                                 (if flag
                                     (matchp-aux pattern (rest input) (add-elt-to-current-group (first input) result) t)
                                     (matchp-aux pattern (rest input) (add-elt-to-current-group (first input) (add-empty-group result)) t)))) ))
                   ((endp input) nil)
                   ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result flag))
                   (t nil))))
    (let ((result (matchp-aux pattern input '() nil)))
      (if (consp result)
          (mapcar #'nreverse (nreverse result))
          result))))

;;;
;;;  #2d Greedy
;;;  
(defun matchp (pattern input)
  (labels ((matchp-aux (pattern input result flag)
             (cond ((endp pattern) (if (endp input)
                                       (or result t)
                                       nil))
                   ((wildp (first pattern))
                    (cond ((endp input) (if flag
                                            (matchp-aux (rest pattern) input result nil)
                                            (matchp-aux (rest pattern) input (add-empty-group result) nil)))
                          (t (or (if flag
                                     (matchp-aux pattern (rest input) (add-elt-to-current-group (first input) result) t)
                                     (matchp-aux pattern (rest input) (add-elt-to-current-group (first input) (add-empty-group result)) t)) ; See ADD-ELT-TO-NEW-GROUP below!
                                 (if flag
                                     (matchp-aux (rest pattern) input result nil)
                                     (matchp-aux (rest pattern) input (add-empty-group result) nil)))) ))
                   ((endp input) nil)
                   ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result flag))
                   (t nil))))
    (let ((result (matchp-aux pattern input '() nil)))
      (if (consp result)
          (mapcar #'nreverse (nreverse result))
          result))))

;;;  #3 --- WTF?!
(defun matchp (pattern input)
  (let ((matches '()))
    (labels ((matchp-aux (pattern input result)
               (cond ((endp pattern) (if (endp input) (or result t) nil))
                     ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result))
                     ((wildp (first pattern))
                      (cond ((endp input) (cons '() result))
                            (t (let ((match (matchp-aux (rest pattern) input (cons '() result))))
                                 (when match (push match matches)))
                               (let ((match (matchp-aux pattern (rest input) (cons (cons (first input) (first result)) (rest result)))) )
                                 (when match (push match matches))))))
                   (t nil))))
    (matchp-aux pattern input '()))
    matches))

(deftest test-matchp ()
  (check
   (matchp '(a b c) '(a b c))
   (not (matchp '(a b c) '(a b c d)))
   (not (matchp '(a b c d) '(a b c)))
   (equal (nth-value 1 (matchp '(a *wild*) '(a b c))) '(b c))
   (equal (nth-value 1 (matchp '(a *wild*) '(a))) '())
   (equal (nth-value 1 (matchp '(a *wild* b) '(a b c d b))) '(b c d))
   (not (matchp '(a *wild* b) '(a b c d e)))
   (matchp '(*wild* b *wild*) '(a b c d e))
   (equal (nth-value 1 (matchp '(*wild*) '(a b c))) '(a b c))))

;;;  #4 --- ?????
(defun matchp (pattern input result)
  (cond ((endp pattern) (endp input))
        ((endp input) (if (wildp (first pattern)) (matchp (rest pattern) input result) nil))
        ((wildp (first pattern)) (remove nil (list (capture (first input) pattern (rest input) result)
                                                   (non-capture (rest pattern) input result))))
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input) result))
        (t nil)))

(defun capture (elt pattern input result)
  (matchp pattern input (cons (cons elt (first result)) (rest result))))

(defun non-capture (pattern input result)
  (matchp pattern input (cons '() result)))

;;;  #5 Not quite right...
(defun matchp (pattern input)
  (labels ((matchp-aux (pattern input result)
             (cond ((endp pattern) (if (endp input) (or result t) nil))
                   ((endp input) (if (wildp (first pattern)) (matchp-aux (rest pattern) input result) nil))
                   ((wildp (first pattern)) (or (capture (first input) pattern (rest input) result)
                                                (non-capture (rest pattern) input result)))
                   ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result))
                   (t nil)))
           (capture (elt pattern input result)
             (matchp-aux pattern input (cons (cons elt (first result)) (rest result))))
           (non-capture (pattern input result)
             (matchp-aux pattern input (cons '() result))))
    (mapcar #'reverse (reverse (matchp-aux pattern input '())))))

;;;  #6 Not quite right...
(defun matchp (pattern input)
  (labels ((matchp-aux (pattern input result)
             (cond ((endp pattern) (if (endp input) (or result t) nil))
                   ((endp input) (if (wildp (first pattern)) (matchp-aux (rest pattern) input result) nil))
                   ((wildp (first pattern)) (or (matchp-aux pattern (rest input) (cons (cons (first input) (first result)) (rest result)))
                                                (matchp-aux (rest pattern) input (cons '() result))))
                   ((eql (first pattern) (first input)) (matchp-aux (rest pattern) (rest input) result))
                   (t nil))))
    (mapcar #'reverse (reverse (matchp-aux pattern input '(()))))))

;;;  #7 --- Good -- greedy
;;;  This captures the FLAG state from 2c/2d to properly allow tail-recursion!
(defun matchp (pattern input)
  (labels ((ignore (pattern input result)
             (cond ((endp pattern) (if (endp input) (or result t) nil))
                   ((endp input) (if (wildp (first pattern)) (ignore (rest pattern) input (add-empty-group result)) nil))
                   ((wildp (first pattern)) (or (capture pattern (rest input) (add-elt-to-new-group (first input) result))
                                                (ignore (rest pattern) input (add-empty-group result))))
                   ((endp input) nil)
                   ((eql (first pattern) (first input)) (ignore (rest pattern) (rest input) result))
                   (t nil)))
           (capture (pattern input result)
             (cond ((endp pattern) (if (endp input) (or result t) nil))
                   ((endp input) (if (wildp (first pattern)) (ignore (rest pattern) input result) nil))
                   ((wildp (first pattern)) (or (capture pattern (rest input) (add-elt-to-current-group (first input) result))
                                                (ignore (rest pattern) input result)))
                   ((endp input) nil)
                   ((eql (first pattern) (first input)) (ignore (rest pattern) (rest input) result))
                   (t nil))))
    (let ((result (ignore pattern input '())))
      (if (symbolp result)
          result
          (mapcar #'reverse (reverse result)))) ))

;;;
;;;  #7a Canonical greedy -- Equivalent to 2d??
;;;     2d: MATCHP-AUX FLAG=NIL => IGNORE,
;;;                    FLAG=T => CAPTURE
;;;  
(defun matchp (pattern input)
  (labels ((ignore (pattern input result)
             (cond ((endp pattern) (if (endp input) (or result t) nil))
                   ((wildp (first pattern))
                    (cond ((endp input) (ignore (rest pattern) input (add-empty-group result)))
                          (t (or (capture pattern (rest input) (add-elt-to-new-group (first input) result))
                                 (ignore (rest pattern) input (add-empty-group result)))) ))
                   ((endp input) nil)
                   ((eql (first pattern) (first input)) (ignore (rest pattern) (rest input) result))
                   (t nil)))
           (capture (pattern input result)
             (cond ((endp pattern) (if (endp input) (or result t) nil))
                   ((wildp (first pattern))
                    (cond ((endp input) (ignore (rest pattern) input result))
                          (t (or (capture pattern (rest input) (add-elt-to-current-group (first input) result))
                                 (ignore (rest pattern) input result)))) )
                   ((endp input) nil)
                   ((eql (first pattern) (first input)) (ignore (rest pattern) (rest input) result))
                   (t nil))))
    (let ((result (ignore pattern input '())))
      (if (symbolp result) ; T or NIL for simple succeed/fail w/o wildcard capture
          result
          (mapcar #'nreverse (nreverse result)))) ))

;;;    Moved above
;; (defun add-elt-to-current-group (elt result)
;;   (destructuring-bind (current-group . rest) result
;;     (cons (cons elt current-group) rest)))

(defun add-elt-to-new-group (elt result)
  (cons (list elt) result))

;;;    Moved above
;; (defun add-empty-group (result)
;;   (cons '() result))

;;;
;;;  #7b Hybrid -- multiple values + tail recursion (???).
;;;  New best??
;;;  
(defun matchp (pattern input)
  (labels ((ignore (pattern input result)
             (cond ((endp pattern) (if (endp input) (values t result) (fail)))
                   ((wildp (first pattern))
                    (cond ((endp input) (ignore (rest pattern) input (add-empty-group result)))
                          (t (multiple-value-bind (match result1) (capture pattern (rest input) (add-elt-to-new-group (first input) result))
                               (if match
                                   (values match result1)
                                   (ignore (rest pattern) input (add-empty-group result)))) )))
                   ((endp input) nil)
                   ((eql (first pattern) (first input)) (ignore (rest pattern) (rest input) result))
                   (t (fail))))
           (capture (pattern input result)
             (cond ((endp pattern) (if (endp input) (values t result) (fail)))
                   ((wildp (first pattern))
                    (cond ((endp input) (ignore (rest pattern) input result))
                          (t (multiple-value-bind (match result1) (capture pattern (rest input) (add-elt-to-current-group (first input) result))
                               (if match
                                   (values match result1)
                                   (ignore (rest pattern) input result)))) ))
                   ((endp input) nil)
                   ((eql (first pattern) (first input)) (ignore (rest pattern) (rest input) result))
                   (t (fail))))
           (fail ()
             (values nil nil)))
    (multiple-value-bind (match result) (ignore pattern input '())
      (if match
          (values match (mapcar #'nreverse (nreverse result)))
          (fail)))) )

;;;  #8 -- Good -- greedy
(defun matchp (pattern input)
  (labels ((process (pattern input result &key (state :ignore))
             (cond ((endp pattern) (if (endp input)
                                       (or result t)
                                       nil))
                   ((endp input) (if (wildp (first pattern))
                                     (process (rest pattern) input (ecase state
                                                                     (:ignore (add-empty-group result))
                                                                     (:capture result)))
                                     nil))
                   ((wildp (first pattern)) (or (process pattern
                                                         (rest input)
                                                         (ecase state
                                                           (:ignore (add-elt-to-new-group (first input) result))
                                                           (:capture (add-elt-to-current-group (first input) result)))
                                                         :state :capture)
                                                (process (rest pattern)
                                                         input
                                                         (ecase state
                                                           (:ignore (add-empty-group result))
                                                           (:capture result))
                                                         :state :ignore)))
                   ((eql (first pattern) (first input)) (process (rest pattern) (rest input) result))
                   (t nil))))
    (let ((result (process pattern input '())))
      (if (symbolp result)
          result
          (mapcar #'reverse (reverse result)))) ))

;;;
;;;  #8a -- Canonical. This is really the same as 2d, just a different name for FLAG!
;;;  
(defun matchp (pattern input)
  (labels ((process (pattern input result &key (state :ignore))
             (cond ((endp pattern) (if (endp input)
                                       (or result t)
                                       nil))
                   ((wildp (first pattern))
                    (if (endp input)
                        (process (rest pattern)
                                 input
                                 (ecase state
                                   (:ignore (add-empty-group result))
                                   (:capture result)))
                        (or (process pattern
                                     (rest input)
                                     (ecase state
                                       (:ignore (add-elt-to-new-group (first input) result))
                                       (:capture (add-elt-to-current-group (first input) result)))
                                     :state :capture)
                            (process (rest pattern)
                                     input
                                     (ecase state
                                       (:ignore (add-empty-group result))
                                       (:capture result))
                                     :state :ignore))))
                   ((endp input) nil)
                   ((eql (first pattern) (first input)) (process (rest pattern) (rest input) result))
                   (t nil))))
    (let ((result (process pattern input '())))
      (if (symbolp result)
          result
          (mapcar #'reverse (reverse result)))) ))

(deftest test-matchp ()
  (check
   (equal (matchp '(a b c) '(a b c)) t)
   (equal (matchp '(a b c) '(a b c d)) nil)
   (equal (matchp '(a b c d) '(a b c)) nil)
   (equal (matchp '(a *wild*) '(a b c)) '((b c)))
   (equal (matchp '(a *wild*) '(a)) '(()))
   (equal (matchp '(a *wild* b) '(a b c d b)) '((b c d)))
   (equal (matchp '(a *wild* b) '(a b c d e)) nil)
   (equal (matchp '(*wild* b *wild*) '(a b c d e)) '((a) (c d e)))
   (equal (matchp '(*wild*) '(a b c)) '((a b c)))
   (equal (matchp '(i do not like *wild* coach because he *wild* all of the time which is *wild*) '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me))
          '((MY CRAZY) (LIKES TO TELL BAD JOKES) (VERY ANNOYING TO ME)))
   (equal (matchp '(*wild* a *wild*) '(b c a d)) '((b c) (d)))
   (equal (matchp '(a b *wild* c d *wild*) '(a b c d c d c d)) '((c d c d) ()))
   (equal (matchp '(*wild* a *wild* a) '(a a a a a a a)) '((A A A A A) NIL))
   (equal (matchp '(*wild* a *wild* a *wild*) '(a a a a a a a b c d)) '((A A A A A) NIL (B C D)))) )

;;;
;;;    Modified from 2010
;;;    
;;;  #9 -- Good
(defun match-greedy (pattern input)
  (cond ((endp pattern) (if (endp input) '() 'fail))
        ((wildp (first pattern))
         (cond ((endp input) (let ((result (match-greedy (rest pattern) input)))
                               (if (eq result 'fail)
                                   result
                                   (cons '() result))))
               (t (let ((result (match-greedy pattern (rest input))))
                    (if (eq result 'fail)
                        (let ((result (match-greedy (rest pattern) input)))
                          (if (eq result 'fail)
                              result
                              (cons '() result)))
                        (cons (cons (first input) (first result)) (rest result)))) )))
        ((eql (first pattern) (first input)) (match-greedy (rest pattern) (rest input)))
        (t 'fail)))

;;;
;;;  #9a This is simply #9 decomposed. -- Good
;;;  
(defun match-greedy (pattern input)
  (cond ((endp pattern) (if (endp input) '() 'fail))
        ((wildp (first pattern))
         (cond ((endp input) (add-empty-group-or-fail-single (match-greedy (rest pattern) input)))
               (t (let ((result (match-greedy pattern (rest input))))
                    (if (eq result 'fail)
                        (add-empty-group-or-fail-single (match-greedy (rest pattern) input))
                        (add-elt-to-current-group (first input) result)))) ))
        ((eql (first pattern) (first input)) (match-greedy (rest pattern) (rest input)))
        (t 'fail)))

;;;
;;;    Name conflict with multiple-valued version above!!
;;;    
(defun add-empty-group-or-fail-single (result)
  (if (eq result 'fail)
      result
      (add-empty-group result)))

(deftest test-match-greedy ()
  (check
   (equal (match-greedy '(a b c) '(a b c)) '())
   (equal (match-greedy '(a b c) '(a b c d)) 'fail)
   (equal (match-greedy '(a b c d) '(a b c)) 'fail)
   (equal (match-greedy '(a *wild*) '(a b c)) '((b c)))
   (equal (match-greedy '(a *wild*) '(a)) '(()))
   (equal (match-greedy '(a *wild* b) '(a b c d b)) '((b c d)))
   (equal (match-greedy '(a *wild* b) '(a b c d e)) 'fail)
   (equal (match-greedy '(*wild* b *wild*) '(a b c d e)) '((a) (c d e)))
   (equal (match-greedy '(*wild*) '(a b c)) '((a b c)))
   (equal (matchp '(*wild* *wild*) '(a b c)) '((a b c) ()))
   (equal (match-greedy '(i do not like *wild* coach because he *wild* all of the time which is *wild*) '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me))
          '((MY CRAZY) (LIKES TO TELL BAD JOKES) (VERY ANNOYING TO ME)))
   (equal (match-greedy '(*wild* a *wild*) '(b c a d)) '((b c) (d)))
   (equal (match-greedy '(a b *wild* c d *wild*) '(a b c d c d c d)) '((c d c d) ()))
   (equal (match-greedy '(*wild* a *wild* a) '(a a a a a a a)) '((A A A A A) ()))
   (equal (match-greedy '(*wild* a *wild* a *wild*) '(a a a a a a a b c d)) '((A A A A A) () (B C D)))) )

;;; ================================================================================================================================================================
;;;;   Started:            Tue Aug 13 23:28:33 2019
;; (defun wild-card-p (obj)
;;   (eq obj '*wild*))
;; (defun matchp (pattern target)
;;   (cond ((endp pattern) (endp target))
;;         ((endp target) (and (wild-card-p (first pattern))
;;                             (matchp (rest pattern) target)))
;;         ((eq (first pattern) (first target))
;;          (matchp (rest pattern) (rest target)))
;;         ((wild-card-p (first pattern))
;;          (or (matchp (rest pattern) target)
;;              (matchp pattern (rest target))))
;;         (t nil)))


(deftest test-matchp ()
  (check
   (matchp '(a b c) '(a b c))
   (not (matchp '(a b c) '(a b c d)))
   (not (matchp '(a b c d) '(a b c)))
   (matchp '(a *wild*) '(a b c))
   (matchp '(a *wild*) '(a))
   (matchp '(a *wild* b) '(a b c d b))
   (not (matchp '(a *wild* b) '(a b c d e)))
   (matchp '(*wild* b *wild*) '(a b c d e))
   (matchp '(*wild*) '(a b c))
   (matchp '(a b *wild* c d) '(a b c d))
   (not (matchp '(a b ()) '(a b)))
   (matchp '(*wild*) '())
   (matchp '(*wild* *wild*) '())
   (matchp '(i do not like *wild* coach because he *wild* all of the time which is *wild*)
           '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me))
   (matchp '(*wild* a *wild*) '(b c a d))
   (matchp '(a b *wild* c d *wild*) '(a b c d c d c d))
   (matchp '(*wild* a *wild* a) '(a a a a a a a))
   (matchp '(*wild* a *wild* a *wild*) '(a a a a a a a b c d))))

(deftest test-matchp-capture ()
  (check
   (equal (matchp '(a b c) '(a b c)) t)
   (equal (matchp '(a b c) '(a b c d)) nil)
   (equal (matchp '(a b c d) '(a b c)) nil)
   (equal (matchp '(a *wild*) '(a b c)) '((b c)))
   (equal (matchp '(a *wild*) '(a)) '(()))
   (equal (matchp '(a *wild* b) '(a b c d b)) '((b c d)))
   (equal (matchp '(a *wild* b) '(a b c d e)) nil)
   (equal (matchp '(*wild* b *wild*) '(a b c d e)) '((a) (c d e)))
   (equal (matchp '(*wild*) '(a b c)) '((a b c)))
   (equal (matchp '(a b *wild* c d) '(a b c d)) '(()))
   (equal (matchp '(a b ()) '(a b)) nil)
   (equal (matchp '(*wild*) '()) '(()))
   (equal (matchp '(*wild* *wild*) '()) '(() ()))
   (equal (matchp '(*wild* *wild*) '(a b c)) '(() (a b c)))
   (equal (matchp '(i do not like *wild* coach because he *wild* all of the time which is *wild*) '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me))
          '((MY CRAZY) (LIKES TO TELL BAD JOKES) (VERY ANNOYING TO ME)))
   (equal (matchp '(*wild* a *wild*) '(b c a d)) '((b c) (d)))
   (equal (matchp '(a b *wild* c d *wild*) '(a b c d c d c d)) '(() (c d c d)))
   (equal (matchp '(*wild* a *wild* a) '(a a a a a a a)) '(() (A A A A A)))
   (equal (matchp '(*wild* a *wild* a *wild*) '(a a a a a a a b c d)) '(() () (A A A A A B C D)))) )

(deftest test-matchp-capture-greedy ()
  (check
   (equal (matchp '(a b c) '(a b c)) t)
   (equal (matchp '(a b c) '(a b c d)) nil)
   (equal (matchp '(a b c d) '(a b c)) nil)
   (equal (matchp '(a *wild*) '(a b c)) '((b c)))
   (equal (matchp '(a *wild*) '(a)) '(()))
   (equal (matchp '(a *wild* b) '(a b c d b)) '((b c d)))
   (equal (matchp '(a *wild* b) '(a b c d e)) nil)
   (equal (matchp '(*wild* b *wild*) '(a b c d e)) '((a) (c d e)))
   (equal (matchp '(*wild*) '(a b c)) '((a b c)))
   (equal (matchp '(a b *wild* c d) '(a b c d)) '(()))
   (equal (matchp '(a b ()) '(a b)) nil)
   (equal (matchp '(*wild*) '()) '(()))
   (equal (matchp '(*wild* *wild*) '()) '(() ()))
   (equal (matchp '(*wild* *wild*) '(a b c)) '((a b c) ()))
   (equal (matchp '(i do not like *wild* coach because he *wild* all of the time which is *wild*) '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me))
          '((MY CRAZY) (LIKES TO TELL BAD JOKES) (VERY ANNOYING TO ME)))
   (equal (matchp '(*wild* a *wild*) '(b c a d)) '((b c) (d)))
  (equal (matchp '(a b *wild* c d *wild*) '(a b c d c d c d)) '((c d c d) ()))
  (equal (matchp '(*wild* a *wild* a) '(a a a a a a a)) '((A A A A A) ()))
  (equal (matchp '(*wild* a *wild* a *wild*) '(a a a a a a a b c d)) '((A A A A A) () (B C D)))) )

(deftest test-values-matchp ()
  (check
   (equal (multiple-value-list (matchp '(a b c) '(a b c))) '(t nil))
   (equal (multiple-value-list (matchp '(a b c) '(a b c d))) '(nil nil))
   (equal (multiple-value-list (matchp '(a b c d) '(a b c))) '(nil nil))
   (equal (multiple-value-list (matchp '(a *wild*) '(a b c))) '(t ((b c))))
   (equal (multiple-value-list (matchp '(a *wild*) '(a))) '(t (())))
   (equal (multiple-value-list (matchp '(a *wild* b) '(a b c d b))) '(t ((b c d))))
   (equal (multiple-value-list (matchp '(a *wild* b) '(a b c d e))) '(nil nil))
   (equal (multiple-value-list (matchp '(*wild* b *wild*) '(a b c d e))) '(t ((a) (c d e))))
   (equal (multiple-value-list (matchp '(*wild*) '(a b c))) '(t ((a b c))))
   (equal (multiple-value-list (matchp '(a b *wild* c d) '(a b c d))) '(t (())))
   (equal (multiple-value-list (matchp '(a b ()) '(a b))) '(nil nil))
   (equal (multiple-value-list (matchp '(*wild*) '())) '(t (())))
   (equal (multiple-value-list (matchp '(*wild* *wild*) '())) '(t (() ())))
   (equal (multiple-value-list (matchp '(*wild* *wild*) '(a b c))) '(t (() (a b c))))
   (equal (multiple-value-list (matchp '(i do not like *wild* coach because he *wild* all of the time which is *wild*)
                                       '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me)))
          '(t ((my crazy) (likes to tell bad jokes) (very annoying to me))))
   (equal (multiple-value-list (matchp '(*wild* a *wild*) '(b c a d))) '(t ((b c) (d))))
   (equal (multiple-value-list (matchp '(a b *wild* c d *wild*) '(a b c d c d c d))) '(t (() (c d c d))))
   (equal (multiple-value-list (matchp '(*wild* a *wild* a) '(a a a a a a a))) '(t (() (a a a a a))))
   (equal (multiple-value-list (matchp '(*wild* a *wild* a *wild*) '(a a a a a a a b c d))) '(t (() () (a a a a a b c d)))) ))

(deftest test-values-greedy-matchp ()
  (check
   (equal (multiple-value-list (matchp '(a b c) '(a b c))) '(t nil))
   (equal (multiple-value-list (matchp '(a b c) '(a b c d))) '(nil nil))
   (equal (multiple-value-list (matchp '(a b c d) '(a b c))) '(nil nil))
   (equal (multiple-value-list (matchp '(a *wild*) '(a b c))) '(t ((b c))))
   (equal (multiple-value-list (matchp '(a *wild*) '(a))) '(t (())))
   (equal (multiple-value-list (matchp '(a *wild* b) '(a b c d b))) '(t ((b c d))))
   (equal (multiple-value-list (matchp '(a *wild* b) '(a b c d e))) '(nil nil))
   (equal (multiple-value-list (matchp '(*wild* b *wild*) '(a b c d e))) '(t ((a) (c d e))))
   (equal (multiple-value-list (matchp '(*wild*) '(a b c))) '(t ((a b c))))
   (equal (multiple-value-list (matchp '(a b *wild* c d) '(a b c d))) '(t (())))
   (equal (multiple-value-list (matchp '(a b ()) '(a b))) '(nil nil))
   (equal (multiple-value-list (matchp '(*wild*) '())) '(t (())))
   (equal (multiple-value-list (matchp '(*wild* *wild*) '())) '(t (() ())))
   (equal (multiple-value-list (matchp '(*wild* *wild*) '(a b c))) '(t ((a b c) ())))
   (equal (multiple-value-list (matchp '(i do not like *wild* coach because he *wild* all of the time which is *wild*)
                                       '(i do not like my crazy coach because he likes to tell bad jokes all of the time which is very annoying to me)))
          '(t ((my crazy) (likes to tell bad jokes) (very annoying to me))))
   (equal (multiple-value-list (matchp '(*wild* a *wild*) '(b c a d))) '(t ((b c) (d))))
   (equal (multiple-value-list (matchp '(a b *wild* c d *wild*) '(a b c d c d c d))) '(t ((c d c d) ())))
   (equal (multiple-value-list (matchp '(*wild* a *wild* a) '(a a a a a a a))) '(t ((a a a a a) ())))
   (equal (multiple-value-list (matchp '(*wild* a *wild* a *wild*) '(a a a a a a a b c d))) '(t ((a a a a a) () (b c d)))) ))
