;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/sprogn.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    S-PROGN: a 'suspendable' analog to PROGN
;;; DEPENDS-ON: DUAL/defs.lsp, DUAL/archit/basic.lsp, DUAL/archit/agenda.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    20-03-97 [1.0]
;;; UPDATED:    15-12-97 [1.1.0]
;;; UPDATED:    26-01-98 [1.1.1] S-PROGN --> S-PROGN-AUX  (see =/MAILBOX.LSP)
;;;                              MACROEXPAND-S-PROGN --> COMPILE-S-PROGN , etc.
;;;                              Documentation moved to DUAL/ARCHIT/SUSPEND.TXT
;;;                              Support suspended bindings in LET and LET*.
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;;
;;; TO DO:      Finish the documentation.  Decide how much should be documented
;;;               in DUAL/ARCHIT/SUSPEND.LSP and how much -- here.
;;; TO DO:      Make a file DUAL/ARCHIT/TEST/SPROGN.LSP, full of ASSERTions,
;;;               which systematically tests everything related to S-PROGN.


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;      S - P R O G N     M A C R O      ;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")


;;;; >>>>>>>>>>>    See file  DUAL/ARCHIT/SUSPEND.TXT      <<<<<<<<<<<<
;;;; >>>>>>>>>>>    for unified documentation of S-LISP    <<<<<<<<<<<<

;;;; This file does not define new concepts. It defines a powerful macro --
;;;; S-PROGN -- which is useful for writing 'suspendable' programs.
;;;; S-PROGN stands for 'suspendable PROGN' (cf. Chapter 9 in "Artificial
;;;; Intelligence Programming" by Charniak et al.(1987) ).
;;;;
;;;; ...
;;;;


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: s-progn-aux,            ;; s-progn  is defined in ARCHIT/MAILBOX.LSP
;;          *S-PROGN-recognized-functors*
;;

;; *S-PROGN-RECOGNIZED-FUNCTORS*
;;
;;   A constant that keeps the list of Common LISP special forms (and some
;;   standard macros) which may appear inside a S-PROGN.
;;   All 'suspendable' programs should be written in terms of these constructs.
;;
;;   In the current implementation, it is bound to the list:
;;      (PROGN S-PROGN-AUX S-EVAL  SETQ SETF  IF
;;       LET LET* FLET LABELS BLOCK  DOLIST DOTIMES )
;;
;;   The functors listed at the second line are subject to certain restrictions
;;   (see the description of S-PROGN below for more details).
;;   Future implementations may augment *S-PROGN-RECOGNIZED-FUNCTORS* as well
;;   as relax some of the current restrictions.


;; S-PROGN-AUX (carrier &body body)  -->  :SUSPENDED
;;
;;   A macro that is the main 'suspension functor'.
;;   Think of it as a rudimentary compiler for 'suspendable' programs.
;;   S-PROGN expands into calls to the functions defined in ARCHIT/AGENDA.LSP.
;;   In the end, a call to S-PROGN produces one of three values:
;;                                    NIL, :SUSPENDED, or :SUSPENSION-IGNORED.
;;   ....
;;   ....
;;

;;   See Exercise 9.4 in Charniak et al. (1987, p. 174) which has been the
;;   starting point for all code in this file.
;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

(defconstant  *S-PROGN-recognized-functors* '(progn
                                              s-progn-aux  ; S-PROGN is a macro
                                              s-eval
                                              setq setf
                                              if
                                              let  let*
                                              flet labels
                                              block
                                              dolist
                                              dotimes )
"Special forms that S-PROGN knows how to handle.
The rest special forms may not appear inside S-PROGN." )


;; This file is very suitable for block-compilation. There are only three
;; routines that are to be called from outside the file: S-PROGN-AUX,
;; CARRIER-EQ, and CARRIER->PROCESS.  They all belong to the domain of the
;; implementation -- they are created by macro-expansions and are not
;; advertized in the external protocol.
;;
;; The numerous small functions below are auxiliary and will be compiled inline
;; by the Python compiler. (In particular, note the functions whose names
;; are of the form HANDLE-xxx.)

#+(and :CMU  (not :DUAL-DEBUG))
(declaim (ext:start-block  s-progn-aux           ; cited in macroexpansions
                           carrier-eq            ; cited in macroexpansions
                           carrier->process      ; cited in macroexpansions
                           compile-s-progn               ; just in case
                           compile-suspendable-form      ; just in case
                           construct-equivalent-form ))  ; just in case



;;;;;;;    ***********   MAIN  ROUTINES   ***********
;;
;; The macro called S-PROGN is actually only a pre-processor.  The entry
;; point to the main compiler for 'suspendable LISP' is another macro called
;; S-PROGN-AUX.
;;
;; The file DUAL/ARCHIT/MAILBOX.LSP implements the pre-processor.  In other
;; words, it defines  S-PROGN, S-VALUES, and SUSPENDED-VALUE-BIND.
;; The file DUAL/ARCHIT/SPROGN.LSP (this file) implements the main compiler.
;; In other words, it defines the macro S-PROGN-AUX.
;;
;; S-PROGN macroexpands into calls to S-PROGN-AUX.
;;
;; The communication between the pre-processor and the main compiler is
;; established by an additional parameter to S-PROGN-AUX.  It is the very first
;; parameter and is called PRE-PROC-INFO.  It takes two possible values in the
;; current implemenation:
;;   NIL -- no mailbox will be exported; return a dummy value (e.g. :SUSPENDED)
;;   T   -- the pre-processor has arranged for exportation of a mailbox
;;
;; Sample macro-expansion:
;;                                   (let ((#:MBOX43 (cons :empty-mbox nil)))
;;  (s-progn host             -->      (s-progn-aux host
;;     (if [test]                         (if [test]
;;         (s-values 'yes)                    (s-values-aux #:MBOX43 'yes)
;;         (s-values 'no)))                   (s-values-aux #:MBOX43 'no)))
;;                                     #:MBOX43)

(defmacro s-progn-aux (pre-proc-info carrier &body body)
  "Main compiler for 'suspendable LISP'. See S-PROGN and S-EVAL."
  (if pre-proc-info    ; flag set by the pre-processor (see ARCHIT/MAILBOX.LSP)
      (s-progn-returning-a-mailbox   carrier body)
      (s-progn-returning-dummy-value carrier body) ))


(defun  s-progn-returning-a-mailbox (carrier body)
  ;; Computes the macroexpansion of S-PROGN-AUX when PRE-PROC-INFO is non-NIL.
  (let ((proc (gensym "PROC")))
    `(if *ignore-suspensions*
         (progn
           ,.body )                                    ; do not compile
         (let ((,proc (carrier->process ,carrier)))
           ,.(compile-s-progn proc body))) ))          ; compile

(defun  s-progn-returning-dummy-value (carrier body)
  ;; Computes the macroexpansion of S-PROGN-AUX when PRE-PROC-INFO is NIL.
  (let ((proc (gensym "PROC")))
    `(if *ignore-suspensions*
         (progn
           ,@body                                      ; do not compile
           :suspension-ignored)   ; dummy return value
         (let ((,proc (carrier->process ,carrier)))
           ,.(compile-s-progn proc body)               ; compile
           :suspended)) ))        ; dummy return value


(defun compile-s-progn (proc forms)
  "A compiler from 'suspendable' LISP to stack-oriented LISP."
  ;; Returns a list which is suitable to be spliced in a PROGN body.
  (declare (type list forms)
           (values list) )
  (assert (symbolp proc))   ; a gensym variable bound to a suspended process
  (multiple-value-bind  (simple-forms s-form rest-forms)
                        (partition-S-PROGN-body forms)
    (let ((compiled-forms '()))
      (unless (null simple-forms)                   ; arrange for simple-forms
        (push (list* 'push-to-stack proc simple-forms)
              compiled-forms) )
      (unless (null s-form)                         ; arrange for s-form
        (setq compiled-forms
              (nconc (compile-suspendable-form proc s-form)
                     compiled-forms)) )
      (unless (null rest-forms)                     ; arrange for rest-forms
        (setq compiled-forms
              (nconc (compile-s-progn proc rest-forms)
                     compiled-forms)) )
      compiled-forms )))


(defun partition-S-PROGN-body (forms)
  "Return three values: list of simple forms, s-form, and rest forms."
  (declare (type list forms)
           (values list list list) )
  (let ((simple-forms nil)
        (s-form nil) )
        ; the lambda variable FORMS serves as REST-FORMS
    (loop
      (if (endp forms)
          (return (values (nreverse simple-forms) s-form nil))
          (let ((current-form (pop forms)))
            (if (simple-form-p current-form)
                (push current-form simple-forms)   ; and loop
                (return (values (nreverse simple-forms) current-form forms))
            ))) )))


(defun compile-suspendable-form (proc s-form)
  "Translate a suspendable form into a sequence of PUSH-TO-STACK requests."
  (declare (type symbol proc)    ; a variable bound to a suspended process
           (type list s-form)    ; a form that does not satisfy SIMPLE-FORM-P
           (values list) )       ; a list of forms to be spliced into a PROGN
  (let ((functor (first s-form)))
    (cond ((find functor *S-PROGN-recognized-functors*)
             (s-progn-dispatch proc functor (rest s-form)))
          ((special-operator-p functor)   ; unsupported special form?
             (error "~S cannot appear inside S-PROGN: ~S"
                    functor s-form))
          ((macro-function functor)         ; e.g. COND, CASE, etc.
             ;; This covers S-PROGN and SUSPENDED-VALUE-BIND among other macros.
             (compile-s-progn proc
                 (list (macroexpand-1 s-form))))
          (t ; Assume S-FORM is a function call (see p.57/73 in CLtL1/2).
             ; Dig out suspended arguments and retry.
             (compile-s-progn proc
                 (list (construct-equivalent-form s-form)))) )))


(defun s-progn-dispatch (proc functor body)
  "Invoke HANDLE-xxx depending on FUNCTOR."
  (declare (type symbol proc)    ; a variable bound to a suspended process
           (type symbol functor) ; e.g. IF, LET, S-EVAL, etc.
           (type list body)
           (values list) )       ; list of forms to be spliced into PROGN body
  (ecase functor
     (s-eval        (handle-S-EVAL      proc body))
     ((setq setf)   (handle-SETQ/SETF   proc body functor))
     (if            (handle-IF          proc body))
     (let           (handle-LET         proc body))
     (let*          (handle-LET*        proc body))
     ((flet labels) (handle-FLET/LABELS proc body functor))
     (dolist        (handle-DOLIST      proc body))
     (dotimes       (handle-DOTIMES     proc body))
     (progn         (handle-PROGN       proc body))
     (s-progn-aux   (handle-S-PROGN-AUX proc body))
     (block         (handle-BLOCK       proc body))
))


;;;;  ********  Functor handlers, see *S-PROGN-recognized-functors*  *******

(defun handle-PROGN (proc body)
  ;; The functor of BODY is PROGN and at least one form in BODY is suspended.
  ;; (s-progn proc (progn forms))  ->  (s-progn proc forms)  ->  ...
  (if (null body)
      nil
      (compile-s-progn proc body)) )
   
(defun handle-BLOCK (proc body)
  ;; The functor of BODY is BLOCK and at least one form in BODY is suspended.
  (unless (not-inside-p '(return return-from) 
                        (rest body) )   ; skip BLOCK's name
     (error "BLOCK may appear inside S-PROGN only when there aren't any RETURNs: ~S"
            (cons 'block body)) )
  (handle-PROGN proc                    ; Without returns, BLOCK is like PROGN
                (rest body)) )          ; skip BLOCK's name

(defun handle-S-PROGN-AUX (proc flag+carrier+body)
  ;; The functor of BODY is S-PROGN-AUX.
  ;; Plain S-PROGN is a macro and is expanded to produce S-PROGN-AUX.
  ;; Nested S-PROGN-AUX is like PROGN, provided the two carriers match.
  (let ((pre-proc-info (first flag+carrier+body))
        (inner-carrier (second flag+carrier+body))
        (body (rest (rest flag+carrier+body))) )
    (declare (ignore pre-proc-info))
    (if (null body)
        nil
        (handle-PROGN proc
          (cons `(unless (carrier-eq ,proc ,inner-carrier)
                   (cerror "Use outer processor and continue."
                     "Host mismatch in nested S-PROGNs: outer = ~S, inner = ~S."
                     ,proc ,inner-carrier ))
                body)) )))


;;;;  Handle S-EVAL -- the 'suspension primitive'.
;;
;;                                     (progn
;;    (s-eval consumption form)  -->     (push-to-stack proc form)
;;                                       (push-to-stack proc
;;                                          (consume-energy-... consumption)) )

(defun handle-S-EVAL (proc body)
  ;; The functor of BODY is S-EVAL
  (unless (= 2 (length body))
    (error "Malformed S-EVAL: ~S"
           (cons 's-eval body) ))
  (handle-S-EVAL-aux proc (first body) (second body)) )

(defun handle-S-EVAL-aux (proc consumption form)
  (cond ((zero-consumption-p consumption)        ; (s-eval 0 form)
            (compile-s-progn proc (list form)) )
        ((simple-form-p form)                    ; (s-eval C simple-form)
                  (list `(push-to-stack ,proc ,form)
                        `(push-to-stack ,proc
                            (consume-energy-with-losses ,proc ,consumption))) )
        ((eq 's-eval (first form))               ; (s-eval C1 (s-eval C2 ...))
           (unless (= 3 (length form))
             (error "Malformed S-EVAL: ~S"  form))
           ;; (s-eval C1 (s-eval C2 foo))  ->  (s-eval C1+C2 foo)
           (handle-S-EVAL-aux proc
                              (combine-consumptions consumption     ; (+ C1 C2)
                                                    (second form))
                              (third form)) )
        ((suspend-form-p form)                   ; (s-eval C (S-PROGN ...))
           (error "~S cannot appear inside S-EVAL: ~S"
                  (first form)
                  (list 's-eval consumption form)) )
        ;; else the suspension is not at top level
        ((special-operator-p (first form))       ; (s-eval C (progn/if/let ...))
           ;; Arrange for CONSUMPTION and enter the special form.
           (nconc (compile-s-progn proc (list form))
                  (list `(push-to-stack ,proc
                               (consume-energy-with-losses ,proc 
                                                           ,consumption)))) )
        ((macro-function (first form))
           (handle-S-EVAL-aux proc consumption (macroexpand-1 form)))
        (t ; Assume FORM is a function call  (see p.57/73 in CLtL1/2).
           ; Dig out suspended arguments and retry.
           (handle-S-EVAL-aux proc consumption
                              (construct-equivalent-form form))) ))

(defun combine-consumptions (C1 C2)
  ;; (s-eval C1 (s-eval C2 form))  -->  (s-eval (+ C1 C2) form).
  (cond ((zero-consumption-p C2)          C1)
        ((and (numberp C1) (numberp C2))  (+ C1 C2))
        (t                                (list '+ C1 C2)) ))

(defun zero-consumption-p (consumption-specifier)
  (cond ((numberp consumption-specifier)
             (zerop consumption-specifier))
        ((eq consumption-specifier ':explicit-S-PROGN)  ;see ARCHIT/BRACKETS.LSP
             t)
        (t   nil) ))    ; e.g.  (get-consumption 'foo)


;;;;  Handle suspended arguments within forms
;;
;;    (fun (s-eval C arg))  -->  (s-eval C (fun arg))  -->  ...
;;
;;                               (let (#:var1 #:var2)
;;    (fun (s-eval C1 a)    -->    (setq #:var1 (s-eval C1 a))   -->  ...
;;         (s-eval C2 b))          (setq #:var2 (s-eval C2 b))
;;                                 (fun #:var1 #:var2) )

(defun construct-equivalent-form (form)
  ;; Construct a semantically equivalent form with fewer nested S-EVALs.
  (let ((functor (first form))
        (args (rest form))
        (new-vars nil)
        (setq-clauses nil)
        (new-args nil) )
    (labels ((arg->var (arg &aux (g-var (gensym "ARG")))
                  ;; (f ... arg ...)  -->  (f ... #:var ...)  --> ...
                  (push g-var new-vars)
                  (push (list 'setq g-var arg) setq-clauses)
                  (push g-var new-args) )
             (analyze-arg (arg)
               (cond ((simple-form-p arg)
                        (push arg new-args))
                     ((eq 's-eval (first arg))
                        (arg->var arg))
                     ((suspend-form-p arg)
                        (error "Suspended forms do not return values: ~S in ~S"
                               arg form))
                     ;; else the suspension is not at top level
                     ((special-operator-p (first arg)) ; LET, IF, PROGN, etc.
                        ; Note that LET stops the recursion within ANALYZE-ARG.
                        (arg->var arg))  ; pass the trouble to HANDLE-SETQ/SETF
                     ((macro-function (first arg))
                        (analyze-arg (macroexpand-1 arg)))
                     (t ; Assume ARG is a function call (see p.57/73 in CLtL1/2)
                        ; Transform it into a LET or S-EVAL, and recurse.
                        (analyze-arg
                          (construct-equivalent-form arg))) )))
      ;; Main body of CONSTRUCT-EQUIVALENT-FORM
      (dolist (arg args)               ; pass 1 -- collect NEW-VARS, etc.
        (analyze-arg arg))
      (construct-equivalent-form-aux   ; pass 2 -- construct the new form
                 functor
                 args  (nreverse new-args)
                 (nreverse new-vars)  (nreverse setq-clauses)) )))


(defun construct-equivalent-form-aux (functor orig-args new-args
                                              new-vars  setq-clauses)
  (case (length new-vars)
    (0  ; no suspended arguments detected (which is anomalous)
        (cons functor orig-args))
    (1  ; only one suspended argument -- try to avoid introducing a variable
        (let ((s-form (third (first setq-clauses))))
          (if (and (eq 's-eval (first s-form))
                   (member s-form orig-args))
              (construct-equivalent-S-EVAL-form functor orig-args s-form)
              (construct-equivalent-LET-form new-vars setq-clauses
                                                      functor new-args)) ))
    (t  ; many suspended arguments -- gensym variables inevitable
        (construct-equivalent-LET-form new-vars setq-clauses
                                       functor  new-args)) ))

(defun construct-equivalent-LET-form  (new-vars setq-clauses functor new-args)
  `(let ,new-vars
        ,.setq-clauses
        (,functor ,.new-args)))

(defun construct-equivalent-S-EVAL-form (functor orig-args s-eval-clause)
  ;; ORIG-ARGS contains only one suspended form, it is of type S-EVAL,
  ;; and is EQ to S-EVAL-CLAUSE.
  (unless (= 3 (length s-eval-clause))
    (error "Malformed S-EVAL clause in ~S:"
           (cons functor orig-args)) )
  ;; (fun (s-eval C nested-form))  -->  (s-eval C (fun nested-form))
  (let ((nested-form (third s-eval-clause)))
    (list 's-eval
          (second s-eval-clause)   ; consumption
          (cons functor
                (substitute nested-form s-eval-clause orig-args))) ))


;;;;  Handle conditional statements -- IF and all macros built on top of it.
;;
;;    (if (s-eval C test)
;;        then              -->     (s-eval C (if test then else))
;;        else )

(defun handle-IF (proc body)
  ;; The functor of BODY is IF and at least one form in BODY is suspended.
  (unless (<= 2 (length body) 3)
    (error "Malformed IF expression: ~S"  (cons 'if body) ))
  (flet ((handle-branch (form)
           (if (simple-form-p form)
               form
               (wrap-PROGN (compile-s-progn proc (list form)))) ))
    (handle-IF-test-clause proc
                           (first body)                        ; test
                           (handle-branch (second body))       ; then
                           (handle-branch (third  body))) ))   ; else

(defun handle-IF-test-clause (proc test then else)
  ;; THEN and ELSE are macroexpanded (and hence simple forms). TEST is not.
  (cond ((simple-form-p test)
            (list `(push-to-stack ,proc (if ,test ,then ,else))) )
        ((eq 's-eval (first test))               ; (if (s-eval ...) ...)
           (unless (= 3 (length test))
             (error "Malformed S-EVAL in test clause: ~S"  test ))
           ;; (if (s-eval C form) then else)  ->  (s-eval C (if form then else))
           (handle-S-EVAL-aux proc
                              (second test)  ; consumption
                              (list 'if (third test)
                                        then  else)) )
        ((suspend-form-p test)                   ; (if (S-PROGN ...) ...)
           (error "~S cannot appear in test clauses: ~S"
                  (first test)
                  (list 'if test then else)) )
        ;; else suspension is not at top level in TEST -- dig it out
        (t (handle-IF-test-clause proc
                                  (construct-equivalent-form test)
                                  then  else)) ))


;;;;  Handle assignment constructs -- SETQ and SETF.
;;
;;    (setq var (s-eval C form))  -->  (s-eval C (setq var form))

(defun handle-SETQ/SETF (proc body functor)
  (declare (type (member setq setf) functor))
  ;; BODY is a non-empty list of var-form pairs.
  ;; At least one form in BODY is suspended.
  (cond ((endp body)   nil)     ; no more var-form pairs
        ((< (length body) 2)
           (error "Value-form missing in ~S."
                  (cons functor body)) )
        (t ;; (setq v1 f1 v2 f2)  -->  (progn (setq v1 f1) (setq v2 f2))
           (nconc (handle-SETQ/SETF            ; recurse on rest vars-n-forms
                     proc (rest (rest body)) functor)
                  (handle-SETQ/SETF-aux        ; handle first var-form pair
                     proc functor (first body) (second body)))) ))

(defun handle-SETQ/SETF-aux (proc functor var form)
  (declare (type (member setq setf) functor))
  (cond ((not (simple-form-p var))               ; (setf (s-eval ...) 'foo)
           (error "Suspended form appears as a generalized variable in ~S: ~S."
                  functor var) )
        ((simple-form-p form)                    ; (setf simple-var simple-form)
           (list `(push-to-stack ,proc
                    (,functor ,var ,form))) )
        ((eq 's-eval (first form))               ; (setf var (s-eval ...))
           (unless (= 3 (length form))
             (error "Malformed S-EVAL: ~S"  form ))
           (eliminate-S-EVAL-in-SETQ/SETF
                  proc functor var (second form) (third form)) )
        ((suspend-form-p form)                   ; (setf var (S-PROGN ...))
           (error "Suspended forms do not return values: ~S"
              (list functor var form)) )
        ;; else the suspension is not at top level
        ((member (first form) '(let let* flet labels))
           (eliminate-LET-in-SETQ/SETF proc functor var form))
        ((special-operator-p (first form))       ; IF, PROGN, etc.
           (cerror "Compile it anyway, with consumptions left to right."
                   "Cannot figure out the exact order of consumptions in ~S."
                   (list functor var form))
           (handle-SETQ/SETF-aux proc functor var
                                 (construct-equivalent-form form)))
        ((macro-function (first form))
           (handle-SETQ/SETF-aux proc functor var (macroexpand-1 form)))
        (t ; Assume ARG is a function call  (p.57/73 in CLtL1/2).
           ; Dig out suspended arguments and retry.
           (handle-SETQ/SETF-aux proc functor var
                                 (construct-equivalent-form form))) ))


(defun eliminate-S-EVAL-in-SETQ/SETF  (proc functor var consumption form)
  ;; (setq var (s-eval C form))  ->  (s-eval C (setq var form))  -> ...
  (handle-S-EVAL-aux proc consumption
                          (list functor var form)) )

(defun eliminate-LET-in-SETQ/SETF  (proc functor var form)
  ;; (setq var (let ... value-form))  -->  (let ... (setq var value-form))
  (let* ((new-LET-form (copy-list form))  ; to be destructively modified below
         (tail (last new-LET-form))
         (old-value-form (first tail))    ; last form in LET's body
         (new-value-form (list functor var old-value-form)) )
   ;; Destructively modify COPY-OF-ORIGINAL-LET to produce a new LET form.
      (setf (car tail) new-value-form)    ; OLD-VALUE-FORM --> NEW-VALUE-FORM
   ;; Check for name collisions (VAR usually is a gensym but ...)
      (when (member var form)
        (error "DUAL-CORE::ELIMINATE-LET-IN-SETQ/SETF:  Name collision in ~S."
               (list functor var form)) )
   ;; Compile new LET form.
      (let ((LET-functor (first new-LET-form)))
        (ecase LET-functor
          (let      (handle-LET   proc (rest new-LET-form)))
          (let*     (handle-LET*  proc (rest new-LET-form)))
          ((flet
            labels) (handle-FLET/LABELS proc (rest new-LET-form)
                                             LET-functor)) ))))


;;;;  Handle local variable definitions -- LET and LET*
;;
;;    (s-progn proc                      (let ((var simple-form))
;;       (let ((var simple-form))   -->    (s-progn proc            -->  ...
;;         (s-eval C body) ))                 (s-eval C body) ))

(defun handle-LET (proc body)
  ;;  The functor of BODY is LET and at least one form in BODY is suspended.
  (let ((vars-n-vals (first  body))
        (LET-body (rest body)) )
    (multiple-value-bind (heading-simple-VVs  first-suspended-VV
                           medium-simple-VVs  rest-suspended-VVs )
                         (parse-vars-n-vals vars-n-vals)
       ;; Ensure that there is at most one suspended variable-binding form.
       (unless (null rest-suspended-VVs)
         (error "Too many suspended forms in the variable-binding section ~
                 of a LET: ~S. Use LET* or SETQ instead."
                (cons 'let body)) )
       (if (null first-suspended-VV)         ; if all VARS-N-VALS are simple
           (handle-LET/LET*-body proc        ; then LET-BODY must be suspended
                   'let
                   vars-n-vals
                   LET-body)
           (handle-LET/LET*-susp-VV proc     ; else handle FIRST-SUSPENDED-VV
                   'let
                   heading-simple-VVs first-suspended-VV medium-simple-VVs
                   LET-body) ))))

(defun handle-LET* (proc body)
  ;;  The functor of BODY is LET* and at least one form in BODY is suspended.
  (let ((vars-n-vals (first  body))
        (LET-body (rest body)) )
    (multiple-value-bind (heading-simple-VVs  first-suspended-VV
                           medium-simple-VVs  rest-suspended-VVs )
                         (parse-vars-n-vals vars-n-vals)
       (if (null first-suspended-VV)         ; if all VARS-N-VALS are simple
           (handle-LET/LET*-body proc        ; then LET-BODY must be suspended
                   'let*
                   vars-n-vals
                   LET-body)
           (handle-LET/LET*-susp-VV proc     ; else handle FIRST-SUSPENDED-VV
                   'let*
                   heading-simple-VVs first-suspended-VV medium-simple-VVs
                   (if (null rest-suspended-VVs)    ; construct nested LET* ?
                       LET-body
                       (list `(let* ,rest-suspended-VVs
                                ,.LET-body)))) ))))


(defun handle-LET/LET*-body (proc functor vars-n-vals LET-body)
  ;; All VARS-N-VALS are simple forms, LET-BODY is suspendable.
  (declare (type (member let let*) functor))
  ;; (s-progn (let (vars) body))  -->  (let (vars) (s-progn body))  -->  ...
  (let* ((compiled-LET-body (compile-s-progn proc LET-body))
         (new-LET-form (list* functor vars-n-vals compiled-LET-body)) )
    (list `(push-to-stack ,proc ,new-LET-form)) ))

(defun handle-LET/LET*-susp-VV (proc functor
                                simple-VVs suspended-VV rest-simple-VVs
                                LET-body)
  ;;                                     (s-eval C
  ;; (let ((var1 simple-form)       -->     (let ((var1 simple-form)    --> ...
  ;;       (var2 (s-eval C form))                 (var2 form)
  ;;       (var3 simple-form) )                   (var3 simple-form) )
  ;;   body )                                 body) )
  (multiple-value-bind (consumption new-VV)
                       (parse-suspended-var-n-val suspended-VV)
    (handle-S-EVAL-aux proc
                       consumption
                       `(,functor (,.simple-VVs
                                   ,new-VV
                                   ,.rest-simple-VVs )
                          ,.LET-body) )))


(defun parse-vars-n-vals (vars-n-vals)
  ;; Analyze the variable-binding section of a LET or LET*.
  (declare (type list vars-n-vals)
           (values list list list list))    ; simple, susp, simple, susp
  (multiple-value-bind (heading-simple-VVs suspended-VVs)
                       (parse-vars-n-vals-aux nil vars-n-vals)
    (if (null suspended-VVs)
        (values heading-simple-VVs nil nil nil)     ; all VVs are simple
        (let ((first-suspended-VV (first suspended-VVs)))
          (multiple-value-bind (medium-simple-VVs rest-suspended-VVs)
                               (parse-vars-n-vals-aux nil (rest suspended-VVs))
            (values heading-simple-VVs
                    first-suspended-VV
                    medium-simple-VVs
                    rest-suspended-VVs) )))))

(defun parse-vars-n-vals-aux (simple-vvs rest-vvs)
  (declare (type list simple-vvs rest-vvs)
           (values list list))
  (if (endp rest-VVs)
      (values (nreverse simple-VVs) nil)
      (let ((curr-VV (first rest-VVs)))
        (if (or (symbolp curr-VV)                   ; varname
                (simple-form-p (second curr-VV)))   ; (varname simple-form)
            (parse-vars-n-vals-aux (cons curr-VV simple-VVs)
                                   (rest rest-VVs))
            (values (nreverse simple-VVs) rest-VVs) ))))

(defun parse-suspended-var-n-val (suspended-VV)
  (declare (type cons suspended-VV)     ; (var-name (s-eval C form))
           (values t cons) )            ; (values consumption new-VV)
  (let ((var-name (first suspended-VV))
        (S-EVAL-form (second suspended-VV)) )
    (unless (and (= 2 (length suspended-VV))
                 (symbolp var-name)
                 (= 3 (length S-EVAL-form))
                 (eq 's-eval (first S-EVAL-form)) )
      (error "Malformed local-variable binding: ~S"  suspended-VV ))
    (values (second S-EVAL-form)        ; consumption
            (list var-name              ; new-VV
                  (third S-EVAL-form))) ))


;;;;  Handle local function definitions -- FLET and LABELS.
;;
;;    (s-progn proc                      (let ((var simple-form))
;;       (let ((var simple-form))   -->    (s-progn proc            -->  ...
;;         (s-eval C body) ))                 (s-eval C body) ))

(defun handle-FLET/LABELS (proc body functor)
  ;; At least one form in BODY is suspended.
  (declare (type (member flet labels) functor))
  (let ((new-fun-defs (mapcar #'(lambda (fun-def)
                                  (compile-local-fun-def proc fun-def))
                              (first body)) )    ; original local-fun defs
        (new-body (compile-s-progn proc (rest body))) )
    ;; (s-progn (flet ...))  - ->  (flet (s-progn ...))
    (list `(push-to-stack ,proc
                 (,functor            ; FLET or LABELS
                  ,new-fun-defs       ; modified local-fun defs
                  ,.new-body))) ))    ; new main body

(defun compile-local-fun-def (proc fun-def)
  (unless (and (listp fun-def)
               (> (length fun-def) 2)
               (symbolp (first fun-def)) )
    (error "Malformed local function definition: ~S"  fun-def ))
  (let ((fun-name    (first fun-def))
        (lambda-list (second fun-def))
        (fun-body    (rest (rest fun-def))) )   ; list of forms
    (unless (every #'simple-form-p lambda-list)
      (error "Suspended forms are not allowed in lambda lists: ~S"
             fun-def ))
    ;; The real work -- analyze FUN-BODY to decide whether to compile or not
    (cond ((every #'simple-form-p fun-body)
              fun-def )                             ; nothing to compile
          ((and (= 1 (length fun-body))
                (eq 'S-PROGN                        ; explicit S-PROGN ?
                    (first (first fun-body))))
              fun-def )                             ; it'll take care of itself
          (t  ; imitate that FUN-BODY is wrapped in a S-PROGN-AUX
             `(,fun-name ,lambda-list
                 ,.(compile-s-progn proc fun-body)  ; the real work
                 :suspended) )                     ; S-PROGN-AUX return value
    )))


;;;; Recursive implementation of DOLIST
;;      Restriction: no RETURNs or GOs in the body.

(defun handle-DOLIST (proc body)
  ;; The functor of BODY is DOLIST and at least one form in BODY is suspended.
  (declare (type list body))
  (let ((header (first body))
        (dolist-body (rest body)) )
    (unless (and (listp header)
                 (<= 2 (length header) 3) )
      (error "Malformed DOLIST header in ~S."
             (cons 'dolist body)) )
    (unless (not-inside-p '(return return-from go) dolist-body)
      (error "RETURNs and GOs are not allowed inside S-PROGN: ~S"
             (cons 'dolist body)) )
    (handle-DOLIST-aux proc
                       (first header)    ; var
                       (second header)   ; list-form
                       (third header)    ; result-form, if any
                       dolist-body) ))

(defun handle-DOLIST-aux (proc var list-form result-form dolist-body)
  (let ((result-tail (if result-form
                         (list result-form)
                         nil))
        (labels-form (construct-recursive-DOLIST
                                var list-form dolist-body)) )
    (handle-LET proc
            (list* (list var)             ; (let (var)
                   labels-form            ;   (labels ((#:DOLIST ...)) ...)
                   result-tail)) ))       ;   result-form )

(defun construct-recursive-DOLIST (var list-form body)
  (let ((g-recurse (gensym "DOLIST"))
        (tail-var  (gensym "DOLIST")) )
  `(labels  ((,g-recurse (,tail-var)
                (cond ((endp ,tail-var)
                         (setq ,var nil))     ; see p.126/169 in CLtL1/2
                      (t (setq ,var (first ,tail-var))
                         ,@body
                         (,g-recurse (rest ,tail-var))) )))
     (,g-recurse ,list-form)) ))


;;;; Recursive implementation of DOTIMES
;;      Restriction: no RETURNs or GOs in the body.

(defun handle-DOTIMES (proc body)
  ;; The functor of BODY is DOTIMES and at least one form in BODY is suspended.
  (declare (type list body))
  (let ((header (first body))
        (dotimes-body (rest body)) )
    (unless (and (listp header)
                 (<= 2 (length header) 3) )
      (error "Malformed DOTIMES header in ~S."
             (cons 'dotimes body)) )
    (unless (not-inside-p '(return return-from go) dotimes-body)
      (error "RETURNs and GOs are not allowed inside S-PROGN: ~S"
             (cons 'dotimes body)) )
    (handle-DOTIMES-aux proc
                        (first header)    ; var
                        (second header)   ; count-form
                        (third header)    ; result-form, if any
                        dotimes-body) ))

(defun handle-DOTIMES-aux (proc var count-form result-form dotimes-body)
  (let ((result-tail (if result-form
                         (list result-form)
                         nil))
        (labels-form (construct-recursive-DOTIMES
                                var count-form dotimes-body)) )
    (handle-LET proc
            (list* (list (list var 0))    ; (let ((var 0))
                   labels-form            ;   (labels ((#:DOTIMES ...)) ...)
                   result-tail)) ))       ;   result-form )

(defun construct-recursive-DOTIMES (var count-form body)
  (let ((g-recurse (gensym "DOTIMES"))
        (count-var (gensym "DOTIMES")) )
  `(labels  ((,g-recurse (,count-var)
                (cond ((>= ,var ,count-var) nil)
                      (t ,@body
                         (setq ,var (+ 1 ,var))
                         (,g-recurse ,count-var)) )))
     (,g-recurse ,count-form)) ))


;;;; Auxiliary routines

(defun wrap-PROGN (forms)
  "Wraps a PROGN around FORMS if needed."
  (declare (type list forms))
  (case (length forms)
    (0  nil)
    (1  (first forms))
    (t  (cons 'progn forms)) ))

(defun not-inside-p  (non-grata tree)
  "True iff TREE does not contain any of the atoms listed in NON-GRATA."
  (declare (type list non-grata)
           (type (or atom list) tree)   ; that is, TREE is of type T
           (values boolean) )
  (cond ((null tree)  t)                               ; not in this branch
        ((atom tree) (not (member tree non-grata)) )
        ((every #'(lambda (x) (not-inside-p non-grata x))   ; recurse
                tree)    t)         
        (t nil) ))                                     ; EVERY has failed


(defun carrier->process (carrier)
  "Takes an agent or a process and always returns a process."
  (declare (type (or base-agent suspended-process) carrier)
           (values suspended-process) )
  (cond ((agentp carrier) (find-or-make-process carrier))
        ((suspended-process-p carrier) carrier)
        (t (error "DUAL-CORE::CARRIER->PROCESS: ~
                   ~S is neither an agent nor a suspended process."
                  carrier)) ))

(defun carrier-eq (carrier1 carrier2)
  "EQ for 'carriers', where  'carrier' = 'an agent or suspended-process'."
  (declare (values boolean) )
  (cond ((and (suspended-process-p carrier1) (suspended-process-p carrier2))
           (eq carrier1 carrier2))
        ((and (agentp carrier1) (agentp carrier2))
           (eq carrier1 carrier2))
        ((and (agentp carrier1) (suspended-process-p carrier2))
           (eq carrier1 (process-host carrier2)))
        ((and (suspended-process-p carrier1) (agentp carrier2))
           (eq (process-host carrier1) carrier2))
        (t nil) ))    ; type mismatch


#+(and :CMU  (not :DUAL-DEBUG)) (declaim (ext:end-block))
;; End of the block-compilation block


;;;;;;;  End of file DUAL/ARCHIT/SPROGN.LSP
