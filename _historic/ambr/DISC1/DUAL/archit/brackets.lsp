;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/brackets.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Basic agent features and basic housekeeping facilities.
;;; DEPENDS-ON: DUAL/start_me.lsp, DUAL/proclaim.lsp, DUAL/archit/suspend.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    02-04-97
;;; UPDATED:    25-01-97 [1.1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;;;;;
;;; TO DO:      Finish the documentation.  Decide how much should be documented
;;;               in DUAL/ARCHIT/SUSPEND.TXT and how much -- here.
;;; TO DO:      Design and implement proclamations for SETF methods. For inst:
;;;               (proclaim-consumption '(setf get-filler) ... )


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;    [ ... ]   M A C R O   C H A R A C T E R S     ;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")


;;;; >>>>>>>>>>>    See file  DUAL/ARCHIT/SUSPEND.TXT      <<<<<<<<<<<<
;;;; >>>>>>>>>>>    for unified documentation of S-LISP    <<<<<<<<<<<<

;;;; This file defines a read-macro that provides shorthand notation for S-EVAL.
;;;; See DUAL/ARCHIT/SUSPEND and =/SPROGN.LSP for details on S-EVAL and S-PROGN.
;;;;
;;;; This file has a small documentation section -- the features implemented
;;;; here are documented under the general topic of 'suspendable computation'.
;;;; See DUAL/ARCHIT/SUSPEND.LSP for comprehensive documentation.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: #\[
;;          proclaim-consumption, get-consumption,
;;          numeralize-consumption-specifier
;;
;; See files DUAL/PROCLAIM.LSP and DUAL/CONSUM.LSP for PROCLAIM-CONSUMPTION.

;; [ ... ]
;;
;;   A macro character that is a shorthand for S-EVAL (see DUAL/ARCHIT/SUSPEND).
;;   The LISP reader is modified so that when it encounters an opening square
;;   bracket it reads s-expressions until a corresponding closing bracket is
;;   found. The product of this process will be called 'bracketed list' below.
;;
;;   There are three kinds of bracketed lists:
;;     -- with explicit consumption -- they begin with a number: [0.1 f x y]
;;     -- with implicit consumption -- they begin with a symbol: [f x y]
;;     -- with zero consumption -- they begin with the keyword :EXPLICIT-S-PROGN
;;   An error is signaled if a bracketed list does not begin with a number or
;;   a symbol. An error is signaled also when there is a closing bracket
;;   without corresponding opening one.
;;
;;   All kinds of bracketed lists are converted into calls to S-EVAL.
;;
;;   #+:DUAL-FLEXIBLE  ==>  consumptions computed at run-time ...
;;   #-:DUAL-FLEXIBLE  ==>  consumptions computed at compile-time ...
;;
;;   ...  (see also DUAL/ARCHIT/SUSPEND.LSP)
;;


;; PROCLAIM-CONSUMPTION (operation-name consumption-specifier)  -->
;;                                                  -->  consumption-specifier
;;
;;   A function for setting entries in the 'consumption table'.
;;   OPERATION-NAME should be a symbol; if it isn't, an error is signaled.
;;   CONSUMPTION-SPECIFIER should be one of the following:
;;     -- NIL -- meaning that OPERATION-NAME should be excluded from the table.
;;     -- number -- proclaiming that the consumption of the operation named by
;;                  OPERATION-NAME is constant and is equal to that number.
;;     -- :EXPLICIT-S-PROGN -- proclaiming that the operation under question
;;               is implemented by an explicit call to the 'suspendable
;;               primitive' S-PROGN (see DUAL/ARCHIT/SUSPEND and =/SPROGN.LSP).
;;               This explicit S-PROGN will take care to consume the appropriate
;;               amount of energy.  Therefore, no additional energy must be
;;               consumed before entering the operation. In other words, the
;;               keyword :EXPLICIT-S-PROGN is equivalent to zero consumption.
;;
;;   PROCLAIM-CONSUMPTION returns the new table entry for OPERATION-NAME.
;;
;;   Examples:
;;     (proclaim-consumption 'if 0.5)  -->  0.5
;;     ;; and now  (get-consumption 'if)  -->  0.5
;;
;;     (proclaim-consumption 'if nil)  -->  nil     ; disclaiming the old value
;;     ;; and now  (get-consumption 'if)    -->  nil
;;     ;; also     (get-consumption 'if T)  -->  0.0
;;
;;     (proclaim-consumption 'agent-visible-p *default-READ-consumption*) -> 0.1
;;     ;; and now  (get-consumption 'agent-visible-p)  -->  0.1
;;
;;     (proclaim-consumption 'handle-symbol :explicit-S-PROGN) --> :explicit-...
;;     ;; and now  (get-consumption 'handle-symbol)    -->  :explicit-S-PROGN
;;     ;; also     (get-consumption 'handle-symbol T)  -->  0.0
;;
;;   See the file DUAL/CONSUM.LSP for more examples.


;; GET-CONSUMPTION  (operation-name &optional numeralize-p)  -->
;;                                                  -->  consumption-specifier
;;
;;   A function that reads the consumption specifier for a given operation
;;   from the 'consumption table'.
;;   Calls to this function are generated by the macroexpansion of bracketed
;;   lists with implicit consumption. For example:
;;     [f x y]  -->  (s-eval (get-consumption 'f) (f x y))
;;
;;   OPERATION-NAME should be a symbol although this is not checked.
;;   (The check is done by the #\[ macro character.)
;;   NUMERALIZE-P should be either T or NIL. It defaults to NIL.
;;   The function returns a (numeralized) consumption specifier.
;;
;;   When NUMERALIZE-P is NIL (the default), the 'raw' entry from the
;;     consumption table is returned. When the table does not contain any
;;     entry for OPERATION-NAME, the function returns NIL.
;;   When NUMERALIZE-P is T, non-numeric consumption specifiers are converted
;;     into appropriate numbers. See NUMERALIZE-CONSUMPTION-SPECIFIER below.
;;
;;   ...  (see also DUAL/ARCHIT/SUSPEND.LSP)
;;

;; NUMERALIZE-CONSUMPTION-SPECIFIER (consumption-spec)  -->  number
;;
;;   A function that converts non-numeric consumption specifiers into
;;   appropriate numeric values.
;;
;;   CONSUMPTION-SPEC should be a consumption specifier. If not -- error.
;;   The function returns a number (unless an error is signaled).
;;
;;   More concretely:
;;     -- NIL is converted into 0.0
;;     -- :EXPLICIT-S-PROGN is converted into 0.0
;;     -- numbers are returned without change
;;     -- an error is signaled for all other values of CONSUMPTION-SPEC.
;;

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;; **********  Altering the LISP read table  ***********
;;
(eval-when (compile load eval)

  (defun bracket-reader (bracketed-list)
    "Attached to the #\[ macro-character."
    (declare (type list bracketed-list)
             (values list))
    (when (null bracketed-list)
      (error "Empty brackets: []" ))
    (let ((head (first bracketed-list))
          (tail (rest bracketed-list)) )
      (cond ((numberp head)     ; explicit consumption
                ;  [0.123 fun arg1 arg2]  -->  (s-eval 0.123 (fun arg1 arg2))
                (list 's-eval head tail))
            ((eq head :explicit-S-PROGN)
                ; [:explicit-S-PROGN f x] --> (s-eval :explicit-S-PROGN (f x))
                (list 's-eval :explicit-S-PROGN tail))
            ((symbolp head)    ; implicit consumption
                ; [f x]  -->  (s-eval (resolve-consumption f) (f x))
                (list 's-eval (resolve-consumption head)
                              bracketed-list))
        (t (error "DUAL-CORE::BRACKET-READER: ~
                   The first thing after a [ should be a number or a symbol: ~S"
                  head)) )))

  (set-macro-character #\[                           ; see p.378/542 in CLtL1/2
                       #'(lambda (stream char)
                            (declare (ignore char))
                            (bracket-reader
                               (read-delimited-list #\] stream t))) )

  (set-macro-character #\] (get-macro-character #\) ) )
) ; eval-when


;;;; *********  Functions supporting the consumption table  **********
;;
;; In this implementation, the consumption table is distributed across
;; the property lists of the affected symbols.
;; The consumption of the operation FOO is attached to the CONSUMPTION
;; property of the symbol FOO.
;;

;; RESOLVE-CONSUMPTION depends on the compiler policy DUAL-FLEXIBLE.
;; See DUAL/START_ME.LSP for details on this and other compilation policies.

(eval-when (compile load eval)
  #-:DUAL-FLEXIBLE
  (defun resolve-consumption (operation-name)
    "Formulate a LISP form that calculates the consumption of an operation."
    (get-consumption operation-name t) )         ; resolve all in compile time

  #+:DUAL-FLEXIBLE
  (defun resolve-consumption (operation-name)
    "Formulate a LISP form that calculates the consumption of an operation."
    (let ((consum-spec (get-consumption operation-name)))
      (if (eq consum-spec :explicit-S-PROGN)
          :explicit-S-PROGN                      ; open-code :EXPLICIT-S-PROGNs
          `(get-consumption ',operation-name t)  ; else resolve in run time
      )))  ; +:DUAL-FLEXIBLE
)   ; eval-when


;; The definitions of GET-CONSUMPTION and PROCLAIM-CONSUMPTION are moved to
;; the file DUAL/PROCLAM.LSP in order to load it earlier and thus enable the
;; file DUAL/CONSUM.LSP be loaded prior to any files from the DUAL/ARCHIT
;; directory.
#|
(defun get-consumption (operation-name &optional numeralize-p)
  (declare (type symbol operation-name)
           (values (or single-float (member nil :explicit-S-PROGN))) )
  (let ((table-entry (get operation-name 'consumption)))
    (if numeralize-p
        (numeralize-consumption-specifier table-entry)
        table-entry) ))

(defun proclaim-consumption (operation-name consumption-specifier)
  "Proclaims how much energy is needed for an operation to complete."
  (assert (symbolp operation-name))
  (cond ((eq consumption-specifier 'nil)
             (remprop operation-name 'consumption)  nil )
        ((numberp consumption-specifier)
             (assert (not (minusp consumption-specifier)))
             (setf (get operation-name 'consumption)
                   (coerce consumption-specifier 'single-float)) )
        ((eq consumption-specifier ':explicit-S-PROGN)
             (setf (get operation-name 'consumption)
                   ':explicit-S-PROGN) )
        (t   (error
               "PROCLAIM-CONSUMPTION: ~S is not a valid consumption specifier."
               consumption-specifier)) ))
|#


(defun numeralize-consumption-specifier  (specifier)
  "Convert non-numeric consumption specifiers into appropriate numeric values."
  (cond ((numberp specifier)               specifier)
        ((eq specifier ':explicit-S-PROGN) 0.0)
        ((eq specifier 'nil)
            #+:DUAL-DEBUG
              (warn "Converting a NIL consumption specifier into zero.")
            0.0)
        (t  (error "NUMERALIZE-CONSUMPTION-SPECIFIER: Invalid specifier -- ~S"
                   specifier)) ))


;;;;;;;  End of file DUAL/ARCHIT/BRACKETS.LSP
