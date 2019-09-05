;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               sort-file.lisp
;;;;
;;;;   Started:            Tue Aug 13 00:50:27 2019
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
(load "/Users/dsletten/lisp/packages/io.lisp")
(load "/Users/dsletten/lisp/packages/strings.lisp")

(defpackage :sort-file (:use :common-lisp :test :io :strings))

(in-package :sort-file)

;;;
;;;    Schwartzian Transform AKA decorate-sort-undecorate
;;;    
(defun sort-file (in out test1 test2)
  (let ((lines (read-file in)))
    (sort-by-name lines)))

(defun sort-by-name (l)
  (mapcar #'first (sort (mapcar #'(lambda (record) (list record (split record #\|))) l)
                        #'(lambda (record1 record2)
                            (destructuring-bind (l1 (first1 last1 . rest1)) record1
                              (destructuring-bind (l2 (first2 last2 . rest2)) record2
                                (cond ((string< last1 last2) t)
                                      ((string> last1 last2) nil)
                                      (t (string< first1 first2)))) )))) )

(deftest test-sort-by-name ()
  (check
   (equal (sort-by-name (copy-list '("Mary|Jones|345-9090|123 Pine Street"
                                     "Deborah|Smith|782-1234|456 Elm Street, 12-B"
                                     "Susan|Brown|889-4321|789 Maple Street"
                                     "Jane|Smith|345-7766|1212 Grove Terrace"
                                     "Mary|White|889-3758|321 Avenue of Trees"
                                     "Louise|Brown|782-3299|43 Oak Drive")))
          '("Louise|Brown|782-3299|43 Oak Drive"
            "Susan|Brown|889-4321|789 Maple Street"
            "Mary|Jones|345-9090|123 Pine Street"
            "Deborah|Smith|782-1234|456 Elm Street, 12-B"
            "Jane|Smith|345-7766|1212 Grove Terrace"
            "Mary|White|889-3758|321 Avenue of Trees"))))
