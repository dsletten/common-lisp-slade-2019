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
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :solitaire (:use :common-lisp :test))

(in-package :solitaire)

Package cards in ~/lisp/packages
-DECK class
-CARD class

Methods
SHUFFLE
DEAL


Subset of a deck for Solitaire (active cards not yet laid down)?
