;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               all-matchp.lisp
;;;;
;;;;   Started:            Sun Aug 25 21:02:12 2019
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

(defpackage :all-matchp (:use :common-lisp :test))

(in-package :all-matchp)

;   STARTED:            010917
(defun matchp (pattern input)
  (cond ((null pattern) (null input)) ;1
        ((equal (car pattern) (car input)) ;4
         (matchp (cdr pattern) (cdr input)))
        ((equal (car pattern) '*wild*)
         (cond ((null input) (null (cdr pattern))) ;(2a)
               ((equal (cadr pattern) (cadr input)) ;Reaches too far-->Doesn't allow *wild* to match 0 elts
                (or (matchp (cddr pattern) (cddr input)) ;(2b)
                    (matchp pattern (cdr input)))) ;(2c) Backtrack?     (*w* b a) vs. (a b c b a)
               (t (matchp pattern (cdr input)))) ) ;(2c)
        (t nil)) ) ;(5)

;;;
;;;    This (essentially) is Slade's solution.
;;;    (His _actual_ solution is below!!)
;;;    
(defun slade-matchp (pattern input)
  (cond ((null pattern) (null input)) ;1
        ((equal (car pattern) (car input)) ;4
         (slade-matchp (cdr pattern) (cdr input)))
        ((equal (car pattern) '*wild*)
         (cond ((null input) (null (cdr pattern))) ;(2a) Fails (*wild* *wild*)
               ((or (slade-matchp (cdr pattern) input) ;2b
                    (slade-matchp pattern (cdr input)))) )) ;2c
        (t nil)) ) ;5

;;;;   Started:            Mon Mar  1 02:25:30 2004
;;;    The first version is correct. The others are not.      ?!?
;;;    This fails the master test suite (2010).
;;;    This is almost same as Slade's version.
;;;    
(defun matchp (pattern input)
  (cond	((null pattern) (null input)) ;1
        ((eq (car pattern) (car input)) ;4
         (matchp (cdr pattern) (cdr input)))
        ((eq (car pattern) '*wild*)
         (if (null input)
             (null (cdr pattern)) ;(2a)
             (or (matchp pattern (cdr input)) ;2c
                 (matchp (cdr pattern) input)))) ;2b
        (t nil)))

;;
;;   This only partially works IF the recursive calls are swapped from above.
;;   Even then it fails for e.g., (matchp '(a *wild* b) '(a b c d b)). It
;;   goes into an infinite loop checking (*wild* b) and ().
;;   
;; (defun matchp (pattern input)
;;   (cond	((null pattern) (null input))
;;         ((eq (car pattern) (car input))
;;          (matchp (cdr pattern) (cdr input)))
;;         ((eq (car pattern) '*wild*)
;;          (or (and (null input) (null (cdr pattern)))
;;              (matchp pattern (cdr input)) ; <--- Can't keep going if INPUT is ()!!
;;              (matchp (cdr pattern) input)))
;;         (t nil)))

;;
;;    This fails even more.
;;    
;; (defun matchp (pattern input)
;;   (cond	((null pattern) (null input))
;;         ((eq (car pattern) (car input))
;;          (matchp (cdr pattern) (cdr input)))
;;         ((eq (car pattern) '*wild*)
;;          (or (matchp (cdr pattern) input)
;;              (matchp pattern (cdr input)))) ; <--- Can't keep going if INPUT is ()!!
;;         (t nil)))

;;
;;    Re-ordered based on Oz implementation (irrelevant?)
;;    No. This is not correct. It goes into infinite loop:
;;    (matchp '(a *wild* b) '(a b c d b))
;;    (The test is simply wrong, as above)
;; (defun matchp (pattern input)
;;   (cond	((null pattern) (null input))
;;         ((eq (car pattern) '*wild*)
;;          (or (and (null input) (null (cdr pattern))) ;<---Wrong
;;              (matchp pattern (cdr input)) ; <--- Can't keep going if INPUT is ()!!
;;              (matchp (cdr pattern) input)))
;;         ((eq (car pattern) (car input))
;;          (matchp (cdr pattern) (cdr input)))
;;         (t nil)))
;;
;;    Although reordering the clauses of the working version above doesn't
;;    make a difference:
;; (defun matchp (pattern input)
;;   (cond	((null pattern) (null input))
;;         ((eq (car pattern) '*wild*)
;;          (if (null input)
;;              (null (cdr pattern)) ;<---Wrong
;;              (or (matchp pattern (cdr input)) ;2c
;;                  (matchp (cdr pattern) input)))) ;2b
;;         ((eq (car pattern) (car input))
;;          (matchp (cdr pattern) (cdr input)))
;;         (t nil)))

;;;;   Started:            Fri Sep 10 19:01:51 2004
;;;    This fails the master test suite (2010).
(defun matchp (pattern input)
  "Determine whether INPUT matches PATTERN. PATTERN may include wild cards."
  (assert (listp pattern)
          (pattern)
          "PATTERN should be a list.")
  (assert (listp input)
          (input)
          "INPUT should be a list.")
  (labels ((matchp-aux (p i)
             (cond ((null p) (null i)) ;1
                   ((null i) (equal p '(*wild*))) ;(2a)
                   ((eql (car p) (car i)) (matchp-aux (cdr p) (cdr i))) ;4
                   ((eql (car p) '*wild*)
                    (or (matchp-aux (cdr p) i) ;2b
                        (matchp-aux p (cdr i)))) ;2c
                   (t nil))))
    (matchp-aux pattern input)))

;;;;   Started:            Thu Jul  5 18:19:16 2007
;;;    This passes the master test suite (2010).
;;;    
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input)) ;1
        ((endp input) (if (eql (first pattern) '*wild*) ; This test isn't absolutely necessary here in Lisp since (first '()) is defined. But it makes sense (It is necessary in Oz).
                                                        ; The above comment must be referring to (ENDP INPUT)?! The test for (EQL (FIRST PATTERN) '*WILD*) is critical! 
                          (matchp (rest pattern) input) ;2a
                          nil))
        ((eql (first pattern) (first input)) ;4
         (matchp (rest pattern) (rest input)))
        ((eql (first pattern) '*wild*)
         (or (matchp (rest pattern) input) ;2b
             (matchp pattern (rest input)))) ;2c
        (t nil))) ;5

(defun slade-matchp (pattern list)
  (cond ((and (endp pattern) (endp list)) t) ; !! This allows (matchp '() '(a)) to fall through to final COND clause!
        ((equalp (car pattern) (car list))
         (slade-matchp (cdr pattern) (cdr list)))
        ((eq (car pattern) '*wild*)
         (cond ((endp list) (endp (cdr pattern)))
               (t (or (slade-matchp (cdr pattern) list)
                      (slade-matchp pattern (cdr list))))))
        (t nil)))

;;;;   Started:            Fri Feb 27 22:54:21 2009
;;;    This fails the master test suite (2010).
;;;
(defconstant wild-card '*wild*)
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input)) ;1
        ((endp input) (and (eq (first pattern) wild-card) ;(2a)
                           (endp (rest pattern))))
        ((eq (first pattern) (first input)) ;4
         (matchp (rest pattern) (rest input)))
        ((eq (first pattern) wild-card)
         (or (matchp (rest pattern) input) ;2b
             (matchp pattern (rest input)))) ;2c
        (t nil))) ;5

;;;;   Started:            Wed Jun 23 00:38:59 2010
;;;
(defun wildp (obj)
  (eq obj '*wild*))

;; (defun matchp (pattern input)
;;   (cond ((endp pattern) (endp input))
;;         ((endp input) nil) ; This is the first version to get this simplification right? <-- Ha! Broken...
;;         ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
;;         ((wildp (first pattern)) (or (matchp (rest pattern) input)
;;                                      (matchp pattern (rest input))))
;;         (t nil)))

;;;
;;;    The 3 below all pass the master test suite.
;;;    
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input)) ;1
        ((wildp (first pattern)) (or (matchp (rest pattern) input) ;2a/b (This only works because of the next test!)
                                     (and (not (endp input)) (matchp pattern (rest input)))) ) ;2c avoids infinite loop!
        ((endp input) nil) ;3
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input))) ;4
        (t nil))) ;5

;;;
;;;    Alternatively...
;;;    
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input)) ;1
        ((endp input) (and (wildp (first pattern))
                           (matchp (rest pattern) input))) ;2a/3
        ((wildp (first pattern)) (or (matchp (rest pattern) input) ;2b
                                     (matchp pattern (rest input)))) ;2c
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input))) ;4
        (t nil))) ;5

;;;
;;;   5 cases:
;;;   1. PATTERN is '()
;;;   2. PATTERN has wild first elt.
;;;      2a. INPUT is '(). (Any remaining elts in PATTERN must be wild for match to succeed, i.e.,
;;;          eventually case 1. Otherwise, eventually case 3.)
;;;      2b. 0 elts in INPUT match wildcard. Try to match rest of PATTERN.
;;;      2c. 1+ elts in INPUT match wildcard . Try to match wildcard against elts of INPUT.
;;;
;;;Does order of 2b, 2c matter????
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
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input))  ;1
        ((wildp (first pattern)) ;2
         (if (endp input)
             (matchp (rest pattern) input) ;2a
             (or (matchp (rest pattern) input) ;2b
                 (matchp pattern (rest input)))) ) ;2c
        ((endp input) nil) ;3
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input))) ;4
        (t nil))) ;5

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
;;    (matchp '(*wild*) '(a b c))
;;    (matchp '(a b *wild* c d) '(a b c d))
;;    (not (matchp '(a b ()) '(a b)))) )

;;;
;;;    Master test suite. Culled from all implementations (190824)
;;;    
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

;;;;   Started:            Fri Jun 17 01:16:23 2011
;;;    Both of these pass the master test suite (2010).
;;;
(defun matchp (pattern input)
  (cond ((null input) (or (null pattern) (and (wildp (first pattern)) (matchp (rest pattern) input))) ) ;1/2a/3
        ((null pattern) nil) ;(1)?
        ((wildp (first pattern)) (or (matchp (rest pattern) input) ;2b
                                     (matchp pattern (rest input)))) ;2c
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input))) ;4
        (t nil))) ;5

;;;
;;;    Same as first 2010 version.
;;;    
(defun matchp (pattern input)
  (cond ((endp pattern) (endp input)) ;1
        ((wildp (first pattern)) (or (matchp (rest pattern) input)
                                     (and (not (endp input))
                                          (matchp pattern (rest input)))) )
        ((endp input) nil)
        ((eql (first pattern) (first input)) (matchp (rest pattern) (rest input)))
        (t nil)))

(defun wildp (s)
  (eq s '*wild*))
