;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/suspend.lsp   ; see DUAL/archit/suspend.txt
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Entry point to 'suspendable computations'.
;;; DEPENDS-ON: DUAL/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    25-01-98 [1.1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;    S U S P E N D A B L E   C O M P U T A T I O N S    ;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")


;;;; This file defines the notion of SUSPENDABLE COMPUTATION and documents
;;;; a specialized programming language for writing 'suspendable programs'.
;;;; The language is an extension of LISP and is called S-LISP ('suspendable
;;;; LISP').
;;;;
;;;; S-LISP currently supports four primitives for writing suspendable programs:
;;;;   + S-PROGN  -- a suspendable analog to PROGN
;;;;   + S-EVAL   -- a suspendable analog to EVAL
;;;;   + S-VALUES -- a suspendable analog to VALUES
;;;;   + SUSPENDED-VALUE-BIND -- a suspendable analog to MULTIPLE-VALUE-BIND.
;;;;
;;;; These additional constructs are implemented in several different files:
;;;;   + DUAL/ARCHIT/SUSPEND.LSP (this file) -- Provides an entry point
;;;;       to S-LISP and defines some common variables and predicates.
;;;;   + DUAL/ARCHIT/BRACKETS.LSP -- Documents and implements a shorthand
;;;;       notation for S-EVAL using square brackets.
;;;;   + DUAL/ARCHIT/MAILBOX.LSP -- Documents and implements S-VALUES and
;;;;       SUSPENDED-VALUE-BIND, as well as a portion of S-PROGN.
;;;;   + DUAL/ARCHIT/SPROGN.LSP -- Implements the main machinery for S-PROGN.
;;;;
;;;; The implementation of S-LISP is spread in many files but it has proven
;;;; almost impossible to write a coherent documentation of the language
;;;; piece by piece.  Therefore, there is an additional text file that
;;;; introduces the basic concepts of suspendable computation.  The program
;;;; files (.LSP) cited above document only the features that are implemented
;;;; directly in the respective file.

;;;; >>>>>>>>>>>    See file  DUAL/ARCHIT/SUSPEND.TXT      <<<<<<<<<<<<
;;;; >>>>>>>>>>>    for unified documentation of S-LISP    <<<<<<<<<<<<


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: s-eval, *ignore-suspensions*,
;;          suspend-form-p, simple-form-p, *suspension-primitives*,
;;          S-PROGN-return-type
;;
;;    ;; s-progn, *S-PROGN-recognized-functors*  -- see DUAL/ARCHIT/SPROGN.LSP
;;    ;; s-values, suspended-value-bind          -- see DUAL/ARCHIT/MAILBOX.LSP
;;

;; S-PROGN-return-type  is a type specifier...

;; .....
;; .....

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

(deftype  S-PROGN-return-type ()
  '(member nil :suspended :suspension-ignored))

(defvar  *ignore-suspensions* nil
  "When non-NIL, S-PROGNs transform themselves into PROGNs, etc." )

(defconstant  *suspension-primitives*
             '(s-progn  s-eval  s-values  suspended-value-bind )
  "Special constructs for 'suspendable symbolic computation'." )
  ;; More suspension primitives may be added in future versions.

(defun suspend-form-p (form)
  "Is this a form beginning with a suspension primitive?"
  (and (consp form)
       (find (first form) *suspension-primitives*) ))

(defun simple-form-p (form)
  "Is this a form without any suspensions?"
  (cond ((not (consp form))           t)
        ((eq 'quote (first form))     t)
        ((suspend-form-p form)        nil)
        ((every #'simple-form-p form) t)         ; recurse
        (t                            nil) ))    ; EVERY has failed


;;;; **********   S-EVAL macro   ***********
;;
;; By definition, S-EVAL is valid only within the lexical scope of a
;; S-PROGN (see DUAL/ARCHIT/SPROGN.LSP).  The S-PROGN compiler resolves
;; all occurrences of S-EVAL into stack operations (see DUAL/ARCHIT/AGENDA.LSP).
;; Therefore, S-EVAL should passed to the standard LISP evaluator only
;; when the flag *IGNORE-SUSPENSIONS* is non-NIL (and hence S-PROGN fizzles
;; into an ordinary PROGN).  When the flag is NIL (the default), any
;; free-standing S-EVAL is an error.

(defmacro s-eval (consumption form)
  "Suspended evaluation of FORM. Must be used inside a S-PROGN."
  `(if *ignore-suspensions*
      ,form
       (error "S-EVAL without corresponding S-PROGN: ~S"
              '(s-eval ,consumption ,form)) ))


;; See file  DUAL/ARCHIT/BRACKETS.LSP for a shorthand for S-EVAL.
;; See files DUAL/ARCHIT/MAILBOX.LSP and =/SPROGN.LSP for the S-PROGN compiler.

;;;;;;;  End of file DUAL/ARCHIT/SUSPEND.LSP
