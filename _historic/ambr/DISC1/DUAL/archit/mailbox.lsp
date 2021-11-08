;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/mailbox.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    The 'mailbox technique' for exporting values of S-PROGN forms.
;;; DEPENDS-ON: DUAL/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    25-01-98 [1.1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;;;;;
;;; TO DO:      Finish the documentation.  Decide how much should be documented
;;;             in DUAL/ARCHIT/SUSPEND.TXT and how much -- here.


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;   MAILBOXES -- EXPORTING  VALUES  FROM  S-PROGN  FORMS   ;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")


;;;; >>>>>>>>>>>    See file  DUAL/ARCHIT/SUSPEND.TXT      <<<<<<<<<<<<
;;;; >>>>>>>>>>>    for unified documentation of S-LISP    <<<<<<<<<<<<

;;;; This file implements the 'mailbox technique' for exporting values of
;;;; suspendable computations defined by S-PROGN.  See DUAL/ARCHIT/SUSPEND,
;;;; =/BRACKETS, and =/SPROGN.LSP for details on S-EVAL and S-PROGN.
;;;;
;;;; This file has a small documentation section -- the features implemented
;;;; here are documented under the general topic of 'suspendable computation'.
;;;; See DUAL/ARCHIT/SUSPEND.LSP for comprehensive documentation.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: s-values, s-values!   ;; :empty-mbox
;;          suspended-value-bind,
;;          mailbox
;;
;;          s-progn    ;; This is only a preprocessor. The real S-PROGN is
;;                     ;; actually S-PROGN-AUX -- see DUAL/ARCHIT/SPROGN.LSP.
;;
;;

;; MAILBOX is a type specifier

;; .....
;; .....

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

(deftype  mailbox ()
  "Temporary data structure for exporting values from a S-PROGN."
  'cons )  ; mailboxes are implemented as cons cells


;;;; **********   UNDERSTANDING THE 'MAILBOX TECHNIQUE'   **************
;;
;; The current implementation uses the so-called 'mailbox technique' for
;; exporting suspended values out of a S-PROGN form.  The whole technique
;; belongs to the domain of the implementer and is hidden from the user
;; by the S-VALUES and SUSPENDED-VALUES-BIND primitives.  Therefore, it
;; is documented in the implementation section of the file.
;;
;; In order to understand it, you must know the basics of suspendable
;; computation and the S-PROGN macro.  See DUAL/ARCHIT/SUSPENDED.LSP,
;; =/SPROGN.LSP and in particular =/SUSPENDED.TXT for detailed explanations.
;; See also DUAL/ARCHIT/BRACKETS.LSP for the shorthand for S-EVAL.
;;
;;
;; FORMULATION OF THE PROBLEM
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Writing suspendable programs is difficult because many LISP intuitions
;; are violated.  In particular, S-PROGN (and hence all functions defined
;; on its basis) does not return any useful value.  Rather, it returns the
;; keyword :SUSPENDED.  The reason for this seemingly odd behavior is that
;; the 'real' values will be computed later, long after the original call
;; to S-PROGN is over.
;;
;; For example, the following definition will _not_ produce the intended effect:
;;   (defun suspended-combine-things (host thing1 thing2)    ; WRONG!!
;;     (s-progn host                                         ; WRONG!!
;;        [0.5 check-preconditions thing1 thing2]            ; WRONG!!
;;        [10 combine thing1 thing2] ))                      ; WRONG!!
;;
;; Defined in this way, the function will not return the result of applying
;; COMBINE to THING1 and THING2.  Rather, it will always return :SUSPENDED.
;; Unless CHECK-PRECONDITIONS and/or COMBINE do some side-effects, the
;; computation above is equivalent to idle wait until 10.5 units of energy
;; are produced and consumed.
;;
;; With the technique discussed in this file, one could write:
;;   (defun tricky-suspended-combine-things (host thing1 thing2)
;;     (s-progn host
;;        [0.5 check-preconditions thing1 thing2]
;;        (s-values [10 combine thing1 thing2]) ))   ; note S-VALUES
;;
;; Having done this, the caller of this function may use the value by:
;;   (s-progn host
;;      (suspended-value-bind (val)                  ; note SUSPENDED-VALUE-BIND
;;                 [tricky-suspended-combine-things host thing1 thing2]
;;        (use-combined-value val) ))
;;
;; Note that both operations must be executed by the same symb. processor HOST.
;;
;;
;; MAIN IDEA OF THE MAILBOX TECHNIQUE
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; There is a pair of complementary suspension primitives -- S-VALUES and
;; SUSPENDED-VALUES-BIND.  Their usage is similar to that of the Common LISP
;; constructs VALUES and MULTIPLE-VALUE-BIND (see section 7.10 in CLtL2).
;;
;; S-VALUES builds a temporary data structure called a 'mailbox' and arranges
;; that the value (or multiple values) will be stored in it when the suspended
;; computation is carried out.
;;
;; SUSPENDED-VALUE-BIND catches this mailbox, opens it at appropriate time,
;; and binds the useful value(s) to local variables which are then accessible
;; within the lexical scope of SUSPENDED-VALUE-BIND.
;;
;; The mailbox is a data structure containing a field that may be destructively
;; modified (using SETF).  This allows for the following sequence of events:
;;   1. S-EVAL creates an empty mailbox, arranges that it will be filled up
;;      later, and terminates.
;;   2. SUSPENDED-VALUE-BIND receives the empty mailbox. It arranges that
;;      the forms that will use the value have a pointer to the mailbox.
;;      These forms, however, are suspended and hence do not open the mailbox.
;;   3. The suspended forms that calculate the desired value get opportunity to
;;      run.  They produce the value and store it (destructively) into the box.
;;   4. The forms from the body of SUSPENDED-VALUE-BIND get opportunity to run.
;;      They open the mailbox and take the value(s) from there.
;;
;;
;; IMPLEMENTATION OF THE MAILBOX TECHNIQUE
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; The current implementation uses cons cells for mailboxes.
;; New mailboxes are constructed by:   (cons :empty-mbox nil)
;; Values are stored in mailboxes by:  (setf (car mbox) (car value-list))
;;                                     (setf (cdr mbox) (cdr value-list))
;; Thus in the end the mail box contains a list of (one or more) values.
;;
;; The difficult part of the job is to compile suspendable forms that
;; use S-VALUES.  SUSPENDED-VALUE-BIND is relatively easier -- it may be
;; compiled using local information only and is implemented as a macro.
;; For S-VALUES, however, one must work at the level of the enclosing S-PROGN.
;;
;; This is done by the 'S-PROGN pre-processor' that runs before the main
;; S-PROGN compiler.


;;;;;;;;  **********   S-PROGN  PRE-PROCESSOR   ***********
;;
;; Somewhat confusingly, the pre-processor is implemented by a macro named
;; S-PROGN (as it runs first), while the main S-PROGN compiler is again a
;; macro but is called S-PROGN-AUX. The pre-processor is defined in this file
;; and the main compiler is defined in DUAL/ARCHIT/SPROGN.LSP.
;;
;; The scope of S-VALUES is the lexical scope of the innermost surrounding
;; S-PROGN minus the bodies of local function definitions, if any.
;; The pre-processor inspects S-PROGN's body of and converts all occurrences
;; of S-VALUES, if any, into S-VALUES-AUX.  It also renames S-PROGN
;; to S-PROGN-AUX in order to avoid infinite regress.  Finally, it wraps the
;; S-PROGN-AUX into a LET form that establishes a variable for the mailbox.
;;
;; The communication between the pre-processor and the main compiler is
;; established by an additional parameter to S-PROGN-AUX. It is the very first
;; parameter and is called PRE-PROC-INFO.  It takes two possible values in the
;; current implemenation:
;;   NIL -- no mailbox will be exported; return a dummy value (e.g. :SUSPENDED)
;;   T   -- the pre-processor has arranged for exportation of a mailbox
;;
;; Sample macro-expansion:
;;                                   (let ((#:MBOX43 (cons :empty-mbox nil)))
;;  (s-progn host             -->      (s-progn-aux T host
;;     (if [test]                         (if [test]
;;         (s-values 'yes)                    (s-values-aux #:MBOX43 'yes)
;;         (s-values 'no)))                   (s-values-aux #:MBOX43 'no)))
;;                                     #:MBOX43)

(defmacro s-progn (carrier &rest body)
  "The S-PROGN pre-processor. See S-PROGN-AUX for the main compiler."
  (cond ((endp body)  nil)
        ((notany #'find-S-VALUES body)        ; no pre-processing necessary
           `(s-progn-aux NIL ,carrier         ; PRE-PROC-INFO is NIL
               ,.body) )
        (t  ; S-VALUES found somewhere in the body -- substitute with ...-AUX.
           (let ((mbox (gensym "MBOX")))
             `(let ((,mbox (cons ':empty-mbox nil)))
                (s-progn-aux T ,carrier       ; PRE-PROC-INFO is T
                   ,.(mapcar #'(lambda (form) (preprocess-form mbox form))
                             body))
                ,mbox))) ))


(defun  find-S-VALUES (form)
  "Does this FORM contain S-VALUES ?"
  (declare (values boolean))
  (cond ((not (consp form))            nil)
        ((eq 's-values (first form))    T )
        ((eq 's-values! (first form))
           (cerror "Treat S-VALUES! as an ordinary function and continue."
                   "S-VALUES! inside S-PROGN: ~S."
                   form )
                                       nil)
        ((eq 'quote    (first form))   nil)      ; protected environment
        ((member (first form)
             '(s-progn s-progn-aux))   nil)      ; self-contained sub-form
        ((member (first form)
             '(flet labels))                     ; FLET and LABELS receive
           (find-S-VALUES-within-FLET/LABELS form))   ; special treatment
        ((some #'find-S-VALUES form)    T )      ; recurse
        (t                             nil) ))   ; SOME has failed

(defun  find-S-VALUES-within-FLET/LABELS (form)
  ;; The functor of FORM is FLET or LABELS.
  ;; Algorithm: Consider S-VALUES in the main body of FLET/LABELS but
  ;;            ignore any S-VALUES within the bodies of the local functions.
  (declare (values boolean))
  (some #'find-S-VALUES
        (rest (rest form))) )   ; skip the functor and the function-definitions


(defun  preprocess-form (mbox-var form)
  "Substitute all occurrences of S-VALUES inside FORM with S-VALUES-AUX."
  (declare (type symbol mbox-var))   ; a gensym variable bound to a mailbox
  (cond ((not (find-S-VALUES form))
            form)
        ((eq 's-values (first form))
            (when (some #'find-S-VALUES (rest form))  ; check arguments
              (error "Nested S-VALUES are not supported: ~S."
                     form ))
            ;; (s-values val1 val2)  -->  (s-values-aux #:MBOX43 val1 val2)
            (list* 's-values-aux mbox-var (rest form)) )
        ((mapcar #'(lambda (inner-form)
                     (preprocess-form mbox-var inner-form))   ; recurse
                 form)) ))



;;;; **********   EXPORTING SUSPENDED VALUES   ***********
;;
;; That is easy after the body of S-PROGN has been pre-processed.

;; By definition, S-VALUES is valid only within the lexical scope of a S-PROGN.
;; Therefore, S-VALUES should never be passed to the standard LISP evaluator.
;; The macro definition below is provided for error-signaling purposes only.

(defmacro  s-values (&rest values)
  "Export suspended values. Must be used inside a S-PROGN."
  (cerror "Continue compilation and generate run-time error-signaling code."
          "S-VALUES without corresponding S-PROGN: ~S"
          (cons 's-values values))
  `(error "S-VALUES without corresponding S-PROGN: ~S"
          '(s-values ,.values)) )

(defun  s-values-aux (mbox &rest values)     ; the real exportation
  "Put values inside a mailbox by destructively modifying it."
  (declare (type mailbox mbox))
  (setf (car mbox) (car values))       ; in particular, NIL for no values
  (setf (cdr mbox) (cdr values))
  mbox )

(defun s-values! (&rest values)
  "Non-suspended imitation of S-VALUES."
  (declare (values mailbox))
  (if (null values)
      (cons nil nil)     ; NIL is not a mailbox
      values) )


;;;; **********   IMPORTING SUSPENDED VALUES   ***********
;;
;; This is the job of the macro SUSPENDED-VALUE-BIND.
;; Sample macro-expansion:
;;                                      (let ((#:MBOX12 :empty-mbox))
;; (suspended-value-bind (v1 v2)    ->    (setq #:MBOX12 [foo arg1 arg2 arg3])
;;            [foo arg1 arg2 arg3]        (let ((v1 (nth 0 #:MBOX12))
;;   (declare (ignore v1))                      (v2 (nth 1 #:MBOX12)))
;;   [use v2] )                             (declare (ignore v1))
;;                                          [use v2] ))

(defmacro  suspended-value-bind  (var-list s-values-form &rest body)
  "Suspension primitive for receiving values exported by S-VALUES."
  (unless (and (listp var-list)
               (every #'symbolp var-list))
    (error "Bad variable list in SUSPENDED-VALUE-BIND: ~S"
           var-list ))
  (unless (listp s-values-form)
    (error "Bad value-producing form in SUSPENDED-VALUE-BIND: ~S"
           s-values-form ))
  (let ((mbox (gensym "MBOX")))
    `(let ((,mbox ':empty-mbox))
       ,(SVB-setq-clause mbox s-values-form)
       #+:DUAL-DEBUG (SVB-check-mailbox ,mbox ',s-values-form)
       (let ,(SVB-var-bindings mbox var-list)
         ,.body )) ))


(defun  SVB-setq-clause (mbox-var s-values-form)
  ;; Construct the SETQ clause of SUSPENDED-VALUE-BIND.
  (if (eq 's-eval (first s-values-form))
      `(setq ,mbox-var ,s-values-form)
      `(s-eval :explicit-S-PROGN
               (setq ,mbox-var ,s-values-form)) ))

(defun  SVB-var-bindings (mbox-var var-list)
  ;; Construct the variable-binding section of SUSPENDED-VALUE-BIND.
  (declare (type symbol mbox-var))   ; a gensym variable bound to a mailbox
  (let ((count 0)
        (result nil) )
    (dolist (var var-list (nreverse result))
      (push `(,var (nth ,count ,mbox-var))
            result)
      (incf count) )))

(defun  SVB-check-mailbox (mbox s-values-form)
  ;; Warn on inappropriate mailboxes in SUSPENDED-VALUE-BIND.
  (cond ((eq ':empty-mbox mbox)
           (warn "SUSPENDED-VALUE-BIND: ~S failed to export a mailbox."
                 s-values-form ))
        ((or (not (consp mbox))
             (eq ':empty-mbox (first mbox)))
           (warn "SUSPENDED-VALUE-BIND: ~S failed to produce values."
                 s-values-form ))
        (t nil) ))    ; MBOX is OK.


;;;;;;;  End of file DUAL/ARCHIT/MAILBOX.LSP
