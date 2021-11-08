;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/gfun_spy.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Generic function spies -- a tool for verbosers, counters, etc.
;;; DEPENDS-ON: DUAL/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    17-03-98
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;;;;
;;; TO DO:      Document the external interface.
;;;             (Avoid the term 'gfun-spy'. Use 'spy team' instead.)


       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;   G E N E R I C   F U N C T I O N   S P I E S   ;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-INTERFACE")

;;;; This file provides facilities for 'spying' generic functions. ...
;;;; ...
;;;;
;;;; See also files DUAL/INTRFACE/SPY_TOOL, =/VERBOSE, and =/COUNTER.LSP.

;;;; Limitation: Function names of the form (SETF FOO) are not supported.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: begin-spying, stop-spying,
;;          activate-spies, deactivate-spies,
;;          add-spy, remove-spy,
;;          spy-status, general-spy-status, report-spy-status


;; BEGIN-SPYING (gfun-name &key lambda-list cerror-p)  -->  gfun-spy
;;
;;   A function that prepares the stage for spying a generic function.
;;
;;   The function returns an implementation-dependent object describing the spy.
;;   ...

;; STOP-SPYING  (gfun-name)          -->  T or NIL
;;
;;   A function that undoes the effect of BEGIN-SPYING.
;;
;;   ...   (Similar to TRACE and UNTRACE.)


;; ACTIVATE-SPIES (gfun-name)    -->  T or NIL
;; DEACTIVATE-SPIES (gfun-name)  -->  T or NIL
;;
;;   ...


;; ADD-SPY  (gfun-name identifier qualifier closure
;;                     &key comment priority activate-p)  -->  individual-spy
;;
;;   A function that adds a spy to the 'team' spying a generic function.
;;
;;   The function returns an implementation-dependent object describing the spy.
;;   ...

;; REMOVE-SPY  (gfun-name identifier qualifier &optional deactivate-p)  -->
;;                                                                -->  T or NIL
;;
;;   A function that removes a spy from the 'team' spying a generic function.
;;
;;   ...


;; SPY-STATUS  (gfun-name identifier)  -->  (values list active-p comments)
;;
;;   A function that ...

;; GENERAL-SPY-STATUS (gfun-name)  -->  descriptor
;;
;;   A function that ...

;; REPORT-SPY-STATUS (gfun-name &optional stream)  -->  (values)
;;
;;   A function that ...

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;



;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;;;  ***** INTERNAL DOCUMENTATION OF THE SPY MECHANISM  *****
;;
;; The mechanism for spying generic functions is based on :AROUND methods.
;; These methods are explicitly added to and removed from generic function
;; metaobjects via the meta-object protocol (MOP).
;;
;; The current file is the only part of DUAL's implementation (ver. 1.1.2)
;; that uses the meta-object protocol.
;;
;; The implementation adopted here (using :AROUND methods) is completely
;; transparent for the generic functions of the DUAL core.  In particular,
;; all generic functions in the program (both core and interface) are instances
;; of the class STANDARD-GENERIC-FUNCTION.
;;
;; To spy a generic function, BEGIN-SPYING defines an :AROUND method for it.
;; This method wraps itself around the primary method for the generic function
;; and provides opportunity for a 'team of spies' to run whenever it is invoked.
;; To minimize the risk of incidentally shadowing some core :AROUND method, the
;; spy method's specializers for all required arguments are T.
;;
;; The 'team of spies' is implemented as an object of the class GFUN-SPY.
;; Gfun-spies are data structures with four fields:
;;   + NAME   -- the name of the generic function being spied
;;   + SPY-CELL -- a cons cell containing two lists of individual spies
;;   + IMPLANTED-P -- a boolean indicating whether the spy method is 'implanted'
;;   + METHOD -- the :AROUND method that funcalls FUN
;;
;; Each 'individual spy' is a data structure (a list) containing four fields:
;;   + CLOSURE    -- a piece of procedural spying knowledge
;;   + IDENTIFIER -- a symbol that identifies the spy (e.g. 'VERBOSE, 'COUNTER)
;;   + QUALIFIER  -- one of the keywords :BEFORE or :AFTER
;;   + COMMENT    -- optional string
;; Individual spies are independent from one another and are listed in the
;; SPY-CELL of the GFUN-SPY object.  There are two kinds of individual spies:
;; :before and :after.  The former are stored in the CAR of the SPY-CELL while
;; the latter -- in the CDR.  Note that the :after spies have access to the
;; value produced by the primary method of the generic function being spied.
;; In this sense, they are actually hybrid between canonical :AFTER and :AROUND
;; methods.  See the external documentation of ADD-SPY, REMOVE-SPY, and
;; SPY-STATUS for more details.



;;;;;;;;  ***** IMPLEMENTATION OF INDIVIDUAL SPIES ******
;;
;; Individual spies are implemented as four-element lists:

(defun make-individual-spy  (identifier qualifier closure &optional comment)
  (declare (type symbol identifier)                 ; e.g. VERBOSE, COUNT, etc.
           (type (member :before :after) qualifier)
           (type function closure)
           (type (or null string) comment) )
  (list closure identifier qualifier comment) )

(declaim (inline spy-closure spy-identifier spy-qualifier spy-comment))
(defun  spy-closure    (spy) (first  spy))
(defun  spy-identifier (spy) (second spy))
(defun  spy-qualifier  (spy) (third  spy))
(defun  spy-comment    (spy) (fourth spy))



;;;;;;;;  ***** IMPLEMENTATION OF SPY TEAMS ******
;;
;; Spy teams are implemented by the class GFUN-SPY.
;; There is a hash table indexed by generic-function names.

(eval-when (compile load eval)
  (defclass  gfun-spy  (DUAL-interface-object)
    ((name        :initarg     :name
                  :reader      gfun-spy-name
                  :type        symbol
                  :initform    (required-argument) )
     (spy-cell    :reader      gfun-spy-cell      ; The cell does not change
                  :type        cons               ; its CAR and CDR do change
                  :initform    (cons nil nil) )   ; (before . after)
     (implanted-p :accessor    gfun-spy-implanted-p
                  :type        boolean
                  :initform    NIL )
     (method      :accessor    gfun-spy-method
                  :type        (or null standard-method)
                  :initform    nil )
    )
    (:documentation "Data structure for spying generic functions." ))
) ; eval-when


(defmethod print-object  ((gfun-spy gfun-spy) stream)
  (let ((*print-case* :downcase))
    (if (slot-boundp gfun-spy 'name)
        (format stream  "#<gfun-spy ~A>"
                        (gfun-spy-name gfun-spy))
        (format stream  "#<malformed gfun-spy>") )))


(defvar  *gfun-spy-table*
         (make-hash-table  :size  20
                           :rehash-size 1.5
                           :test  #'eq )          ; Keys are symbols
         "Internal hash table for the purposes of the 'spying' mechanism." )

(defun find-gfun-spy (gfun-name)        ; Don't confuse with FIND-SPY below.
  (declare (type symbol gfun-name)
           (values (or null gfun-spy)) )
  (values (gethash gfun-name *gfun-spy-table*)) )


;; MAKE-GFUN-SPY constructs a new instance of GFUN-SPY and registers it into
;; the hash table.  It assumes that  (find-gfun-spy gfun-name)  returns NIL.
;; It checks whether GFUN-NAME names a generic function and signals an error
;; if it is not so.  GFUN-NAME must be a symbol; names like (SETF FOO) cannot
;; be spied in this implementation.  The new spy is not 'implanted', i.e.
;; the generic function itself is not changed.  See IMPLANT-GFUN-SPY below.

(defun gfun-name-p (symbol)
  (declare (type symbol symbol)       ; names like (SETF FOO) aren't supported
           (values boolean) )
  (and (fboundp symbol)
       #+(and :CMU :PCL (not :CLOS))
         (PCL::standard-generic-function-p (symbol-function symbol))
       #-(and :CMU :PCL (not :CLOS))
         (typep (symbol-function symbol) 'standard-generic-function)
       T))

(defun make-gfun-spy (gfun-name)
  (declare (type symbol gfun-name)    ; names like (SETF FOO) aren't supported
           (values gfun-spy) )
  ;; Assumption:  (find-gfun-spy gfun-name)  -->  NIL
  (unless (gfun-name-p gfun-name)
    (error "DUAL-INTERFACE::MAKE-GFUN-SPY: ~S does not name a generic function."
           gfun-name ))
  (setf (gethash gfun-name *gfun-spy-table*)
        (make-instance 'gfun-spy :name gfun-name)) )


(defun remove-gfun-spy (gfun-name)
  (declare (type symbol gfun-name)
           (values boolean) )
  (let ((gfun-spy (find-gfun-spy gfun-name)))
    (if (null gfun-spy)
        nil
        (let ((spy-method (gfun-spy-method gfun-spy))
              (gfun-metaobject (symbol-function (gfun-spy-name gfun-spy))) )
          (remove-method gfun-metaobject spy-method)
          (setf (gfun-spy-method gfun-spy) nil)   ; prevent accidental re-activ.
          (remhash gfun-name *gfun-spy-table*)
          T)) ))



;;;;;;;;  ********  CONSTRUCTION OF SPY METHODS  *********
;;
;; The whole spy mechanism hinges on the so-called SPY METHODS.  One such
;; method is defined for each generic function being spied.  Each spy method
;; is characterized by the following:
;;  -- It is as unspecific as possible. Normally, all required arguments
;;     are unspecialized (i.e. implicitly specialized to T).
;;  -- It is an :AROUND method.
;;  -- Its body invokes the individual spies stored in the SPY-CELL as shown
;;     below.  To improve efficiency, the SPY-CELL is wired into the body of
;;     the new :AROUND method and cannot change.  Its CAR and CDR can be
;;     destructively modified, however, and this is the way to add and remove
;;     individual spies.


;; IMPLANT-GFUN-SPY takes a fresh gfun-spy (produced by MAKE-GFUN-SPY) and
;; adds a spy method to the corresponding generic function by calling
;; DEFINE-SPY-METHOD.  It then stores the new spy method in the GFUN-SPY-METHOD
;; slot of the spy object and sets the IMPLANTED-P flags to T.
;;
;; The optional LAMBDA-LIST may be used to override the default lambda list.
;; (By default, an unspecified lambda list is used, which amounts to setting
;;  the specializer of each required argument to T.)  This option is useful,
;; among other things, when the spy method would shadow an existing :AROUND
;; method of the generic function.

(defun implant-gfun-spy (gfun-spy &optional (lambda-list T))
  (declare (type gfun-spy gfun-spy)
           (type (or list (member T)) lambda-list)
           (values standard-method) )
  (let* ((gfun-name (gfun-spy-name gfun-spy))
         (gfun-metaobject (symbol-function gfun-name))
         (lambda-list-aux (if (eq T lambda-list)
                              (generic-function-lambda-list gfun-metaobject)
                              lambda-list))    ; supplied by the user
         (specializer-names (extract-specializer-names lambda-list-aux)) )
    (when (spy-method-shadows-old-method-p gfun-metaobject specializer-names)
      (error "Cannot implant a spy method for ~S with specializers ~S because ~
              it already has an :AROUND method on these specializers."
              gfun-name specializer-names ))
    ;; Do the real work
    (let ((new-method (define-spy-method gfun-spy
                                         lambda-list-aux)))
      (setf (gfun-spy-method gfun-spy) new-method)
      (setf (gfun-spy-implanted-p gfun-spy) T)
      new-method )))

(defun spy-method-shadows-old-method-p (gfun-metaobject specializer-names)
  (find-method gfun-metaobject
               '(:around)
               (mapcar #'(lambda (name) (find-class name nil))
                       specializer-names)
               nil) )    ; do not signal errors


;; DEFINE-SPY-METHOD creates a spy method via a call to DEFMETHOD.
;; The new method is automatically added to the set of methods of the
;; spied generic function (according to the specification of DEFMETHOD).
;; The body of the method is defined in a lexical environment including
;; a binding for SPY-CELL and thus the cell is wired into the method.
;; Individual spies are invoked via the functions APPLY-BEFORE-SPIES and
;; APPLY-AFTER-SPIES which are defined later in this file.
;; Note the explicit call to EVAL that executes the DEFMETHOD form.

(defun define-spy-method  (gfun-spy lambda-list)
  (declare (type gfun-spy gfun-spy)
           (type list lambda-list)
           (values standard-method) )
  (multiple-value-bind  (req-args op-args rest-arg)
                        (parse-lambda-list lambda-list)
   (let ((gfun-name (gfun-spy-name gfun-spy))
         (SPY-CELL  (gfun-spy-cell gfun-spy))
         (method-lambda (build-method-lambda-list req-args op-args rest-arg))
         (apply-var-form (build-apply-var-form req-args op-args rest-arg))
         (apply-gvar (gensym))
         (primary-gvar (gensym)) )
     (eval `(defmethod ,gfun-name :around ,method-lambda
              (let ((,apply-gvar ,apply-var-form))
                (apply-before-spies ',SPY-CELL ,apply-gvar)
                (let ((,primary-gvar (call-next-method))) ; call primary method
                  (apply-after-spies ',SPY-CELL ,primary-gvar ,apply-gvar)
                  ,primary-gvar))) ))))                  ; return primary value


(defun build-method-lambda-list (req-args op-args rest-arg)
  (let ((result nil))
    (when rest-arg
      (setq result (list '&rest rest-arg)) )
    (when op-args
      (setq result (append op-args result))
      (push '&optional result))
    (append req-args result) ))

(defun build-apply-var-form (req-args op-args rest-arg)
  (flet ((pile-off (arg)
           (if (symbolp arg)
               arg
               (first arg)) ))    ; e.g.  (req class-name)  or  (opt init svar)
    (let ((result nil))
      (dolist (arg req-args)
        (push (pile-off arg) result))
      (dolist (arg op-args)
        (push (pile-off arg) result))
      (when rest-arg
        (push rest-arg result))
      (setq result (nreverse result))
      (if rest-arg
          (push 'list* result)      ; (list* arg1 arg2 arg3 rest-args)
          (push 'list  result))     ; (list  arg1 arg2 arg3)
      result )))


;; PARSE-LAMBDA-LIST analyzes a (possibly specialized) lambda list and returns
;; three values:
;;  + REQ-ARGS -- a list of (possibly specialized) required arguments;
;;  + OP-ARGS  -- a list of &optional parameters;
;;  + REST-ARG -- a name (or NIL) for the &rest parameter
;; &KEY parameters are converted into &REST. &AUX parameters are not allowed.

(defun  parse-lambda-list  (raw-lambda-list)
  (declare (values list list symbol))
  (let ((req-args nil)
        (op-args  nil)
        (rest-arg nil)
        (curr-token nil) )
    ;; Collect required arguments
    (do  ()                           ; no DO bindings
         ((endp raw-lambda-list) (setq curr-token nil))
      (setq curr-token (pop raw-lambda-list))
      (if (member curr-token lambda-list-keywords)
          (return)                    ; break the loop
          (push curr-token req-args)))
    ;; Collect optional arguments
    (when (eq '&optional curr-token)
      (do  ()                         ; no DO bindings
           ((endp raw-lambda-list) (setq curr-token nil))
        (setq curr-token (pop raw-lambda-list))
        (if (member curr-token lambda-list-keywords)
            (return)                  ; break the loop
            (push curr-token op-args)) ))
    ;; Check for &key or &rest parameters. Signal errors on &aux, etc.
    (cond ((null curr-token))             ; no more tokens
          ((eq '&rest curr-token) (setq rest-arg (pop raw-lambda-list)))
          ((eq '&key  curr-token) (setq rest-arg (gensym "KEY->REST")))
          (t  (cerror "Ignore it and continue."
                      "DUAL-INTERFACE::PARSE-LAMBDA-LIST: Illegal token ~S."
                      curr-token)) )
    ;; Return three values
    (values (nreverse req-args)
            (nreverse op-args)
            rest-arg) ))



;;;;;;;  ******  USING THE SPY METHOD TO INVOKE INDIVIDUAL SPIES  *******
;;
;; The gfun-spy keeps a set of individual spies.  They are stored in the
;; SPY-CELL of the gfun-spy.  The spy cell is a cons whose CAR stores the
;; list of :BEFORE spies and whose CDR stores the list of :AFTER spies.
;; The spy cell is wired into the gfun-spy method and thus cannot change.
;; Individual spies are added or removed by destructively modifying the
;; CAR and/or CDR of the spy cell -- see ADD- and REMOVE-INDIVIDUAL-SPY below.
;;
;; The body of the gfun-spy-method can be expressed by the following pseudocode:
;;   (apply-before-spies SPY-CELL ARG-LIST)
;;   (let ((val (call-next-method)))        ; call the primary method
;;     (apply-after-spies SPY-CELL val ARG-LIST)
;;     val)                                 ; return primary value
;; Where SPY-CELL is the spy cell of the gfun-spy and ARG-LIST is bound to the
;; argument list of the generic function call.

(defun apply-before-spies (spy-cell arg-list)
  "Applies the closures stored in the CAR of SPY-CELL to ARG-LIST."
  (declare (type cons spy-cell)
           (type list arg-list) )
  (ignore-errors
    (dolist (spy (cAr spy-cell))
      (apply (spy-closure spy) arg-list)) ))

(defun apply-after-spies (spy-cell return-value arg-list)
  "Applies the closures stored in the CDR of SPY-CELL to RETURN-VALUE and ARGS."
  (declare (type cons spy-cell)
           (type list arg-list) )
  (ignore-errors
    (dolist (spy (cDr spy-cell))
      (apply (spy-closure spy) return-value arg-list)) ))


;; Sometimes both before and after spy lists are empty.  This happens when one
;; wants to have the ability to occasionally spy a generic function and has
;; therefore created a gfun-spy for it but, on the other hand, the function
;; is actually _not_ spied most of the time. (E.g. verbose spy for ADD-LINK.)
;; To improve efficiency in such cases, the spy method may be (temporarily)
;; removed from the set of methods for the generic function via REMOVE-METHOD.
;; The method metaobject is retained in the METHOD field of the gfun-spy and
;; may be re-implanted later via ADD-METHOD.  The flag IMPLANTED-P shows
;; whether the spy method is implanted or not.
;; The functions ACTIVATE-SPY-METHOD and DEACTIVATE-SPY-METHOD are used to
;; switch from one mode to the other.
;;
;; Note that this mechanism may be used even when the spy cell is not empty.
;; Calling DEACTIVATE-SPY-METHOD prevents all individual spies from running.
;; ACTIVATE-SPY-METHOD puts them back to operation at once.

(defun activate-spy-method (gfun-spy)
  "Add (if needed) the spy method to the spied generic fununction."
  (declare (type gfun-spy gfun-spy)
           (values boolean) )
  (if (gfun-spy-implanted-p gfun-spy)
      nil                           ; nothing needs to be done
      (let ((spy-method (gfun-spy-method gfun-spy))
            (gfun-metaobject (symbol-function (gfun-spy-name gfun-spy))))
        (cond ((null spy-method)
                 (cerror
                   "Construct a new spy method, implant it, and continue."
                   "DUAL-INTERFACE::ACTIVATE-SPY-METHOD: ~S has no spy method."
                   gfun-spy)
                 (implant-gfun-spy gfun-spy))  ; use default LAMBDA-LIST
              (t (add-method gfun-metaobject
                             spy-method)
                 (setf (gfun-spy-implanted-p gfun-spy) T)))
        T )))      ; indicate that the generic function has been modified

(defun deactivate-spy-method (gfun-spy)
  "Remove (if needed) the spy method from the spied generic fununction."
  (declare (type gfun-spy gfun-spy)
           (values boolean) )
  (if (not (gfun-spy-implanted-p gfun-spy))
      nil                           ; nothing needs to be done
      (let ((spy-method (gfun-spy-method gfun-spy))
            (gfun-metaobject (symbol-function (gfun-spy-name gfun-spy))))
        (remove-method gfun-metaobject
                       spy-method)
        (setf (gfun-spy-implanted-p gfun-spy) NIL)
        T )))      ; indicate that the generic function has been modified



;;;;;;;  ******  ADDING AND REMOVING INDIVIDUAL SPIES  *******
;;
;; By default, addition of _any_ individual spy activates the main spy method.
;; By default, removal of the _last_ individual spy deactivates the main method.
;; The &optional arguments ACTIVATE-P and DEACTIVATE-P can suppress this.
;;
;; It is ensured that all individual spies are uniquely indentified by the
;; combination of their SPY-IDENTIFIER and SPY-QUALIFIER.

(defun find-individual-spy (gfun-spy identifier qualifier)   ; /= FIND-SPY
  (declare (type gfun-spy gfun-spy)
           (values list) )     ; (or individual-spy null)
  (ecase qualifier
    (:before  (find identifier (cAr (gfun-spy-cell gfun-spy))
                               :key #'spy-identifier))
    (:after   (find identifier (cDr (gfun-spy-cell gfun-spy))
                               :key #'spy-identifier)) ))

(defun remove-individual-spy (gfun-spy identifier qualifier
                              &optional (deactivate-p T) )
  (declare (type gfun-spy gfun-spy)
           (values boolean) )
  (let ((ind-spy (find-individual-spy gfun-spy identifier qualifier))
        (spy-cell (gfun-spy-cell gfun-spy)) )
    (cond ((null ind-spy)  nil)
          (t (ecase qualifier
               (:before  (setf (cAr spy-cell)
                               (remove ind-spy (cAr spy-cell))) )
               (:after   (setf (cDr spy-cell)
                               (remove ind-spy (cDr spy-cell))) ))
             (when (and deactivate-p
                        (null (cAr spy-cell))     ; no :before spies
                        (null (cDr spy-cell)) )   ; no :after spies
               (deactivate-spy-method gfun-spy))
             T ))))   ; indicate that the spy-cell has been modified


(defun add-individual-spy (gfun-spy ind-spy
                           &key (activate-p T) (priority :cerror))
  (declare (type gfun-spy gfun-spy)
           (type cons ind-spy)     ; a list of four fields, see above
           (values cons) )         ; individual-spy
  (let* ((identifier (spy-identifier ind-spy))
         (qualifier  (spy-qualifier  ind-spy))
         (old-spy (find-individual-spy gfun-spy identifier qualifier)) )
    (if (null old-spy)
        (add-individual-spy-1 gfun-spy ind-spy qualifier activate-p)
        (ecase priority
          (:old    (when activate-p
                     (activate-spy-method gfun-spy))
                   old-spy )                                    ; retain old
          (:new    (add-individual-spy-2 gfun-spy ind-spy qualifier activate-p))
          (:cerror (cerror "Remove the old spy and continue."
                           "Cannot add spy ~S to ~S because it already has ~S."
                           ind-spy (gfun-spy-name gfun-spy) old-spy )
                   (add-individual-spy-2 gfun-spy ind-spy qualifier activate-p))
         ))))

(defun add-individual-spy-1 (gfun-spy ind-spy qualifier activate-p)
  (declare (type gfun-spy gfun-spy))
  ;; Add IND-SPY without bothering about old spies.
  (let ((spy-cell (gfun-spy-cell gfun-spy)))
    (ecase qualifier
      (:before  (push ind-spy (cAr spy-cell)))
      (:after   (push ind-spy (cDr spy-cell))) )
    (when activate-p
      (activate-spy-method gfun-spy))
    ind-spy ))

(defun add-individual-spy-2 (gfun-spy ind-spy qualifier activate-p)
  ;; First remove the old spy and then call ADD-INDIVIDUAL-SPY-1.
  (remove-individual-spy  gfun-spy
                          (spy-identifier ind-spy)
                          qualifier
                          NIL )       ; do not deactivate the main spy method
  (add-individual-spy-1 gfun-spy ind-spy qualifier activate-p) )



;;;;;;  *****  IMPLEMENTATION OF THE EXTERNAL PROTOCOL  ******
;;

(defun begin-spying (gfun-name &key (lambda-list T) (cerror-p t) )
  "Implant a 'spy method' into a generic function."
  (declare (type symbol gfun-name)    ; Names like (SETF FOO) are not supported
           (type (or list (member T)) lambda-list)
           (values gfun-spy) )
  (let ((gfun-spy (find-gfun-spy gfun-name)))
    (cond ((null gfun-spy)            ; Make from scratch
               (setq gfun-spy (make-gfun-spy gfun-name))
               (implant-gfun-spy gfun-spy lambda-list) )
          (t  (when cerror-p
                (cerror "Continue using the old spy team."
                        "BEGIN-SPYING: ~S is already being spied."
                        gfun-name ))
              (activate-spy-method gfun-spy) ))  ; in case it is deactivated
    gfun-spy ))

(defun stop-spying (gfun-name)
  "Undo the effect of BEGIN-SPYING."
  (remove-gfun-spy gfun-name))

(defun activate-spies (gfun-name)
  "Undo the effect of DEACTIVATE-SPIES."
  (declare (values boolean))
  (let ((gfun-spy (find-gfun-spy gfun-name)))
    (if (null gfun-spy)
        (cerror "Return NIL and continue."
                "ACTIVATE-SPIES: ~S is not being spied at all."
                gfun-name)
        (activate-spy-method gfun-spy) )))

(defun deactivate-spies (gfun-name)
  "Disable the spies of a generic function. May be enabled via ACTIVATE-SPIES."
  (declare (values boolean))
  (let ((gfun-spy (find-gfun-spy gfun-name)))
    (if (null gfun-spy)
        (cerror "Return NIL and continue."
                "DEACTIVATE-SPIES: ~S is not being spied at all."
                gfun-name)
        (deactivate-spy-method gfun-spy) )))


(defun add-spy (gfun-name identifier qualifier closure
                &key (comment nil)  (activate-p T)  (priority :cerror) )
  "Add an individual spy to the spy 'team' of a generic function."
  (declare (type symbol gfun-name    ; Names like (SETF FOO) are not supported
                        identifier)  ; e.g. VERBOSE, COUNTER, etc.
           (type (member :before :after) qualifier)
           (type function closure)   ; Spying procedural knowledge.
           (values list) )           ; (or null individual-spy)
  (let ((gfun-spy (find-gfun-spy gfun-name))
        (ind-spy  (make-individual-spy identifier qualifier closure comment)) )
    (if (null gfun-spy)
        (cerror "Return NIL and continue."
                "ADD-SPY: ~S is not prepared for spying."
                gfun-name)
        (add-individual-spy gfun-spy
                            ind-spy
                            :activate-p activate-p
                            :priority   priority ))))


(defun remove-spy  (gfun-name identifier qualifier &optional (deactivate-p T) )
  "Remove an individual spy from the spy 'team' of a generic function."
  (let ((gfun-spy (find-gfun-spy gfun-name)))
    (if (null gfun-spy)
        nil
        (remove-individual-spy gfun-spy identifier qualifier deactivate-p) )))


(defun spy-status (gfun-name identifier)
  "Check whether a generic function has spies of particular kind."
  (declare (type symbol gfun-name identifier)
           (values list boolean list) )
  (let ((gfun-spy (find-gfun-spy gfun-name)))
    (if (null gfun-spy)
        (values nil nil nil)
        (let ((before-spy (find-individual-spy gfun-spy identifier :before))
              (after-spy  (find-individual-spy gfun-spy identifier :after ))
              (first-return-value  nil)
              (second-return-value (gfun-spy-implanted-p gfun-spy))
              (third-return-value  nil) )
          (unless (null after-spy)
            (push ':after first-return-value)
            (push (spy-comment after-spy) third-return-value) )
          (unless (null before-spy)
            (push ':before first-return-value)
            (push (spy-comment before-spy) third-return-value) )
          (values (nreverse first-return-value)   ; (:before :after)
                  second-return-value             ; implanted-p
                  (nreverse third-return-value)) ))))  ; comments

(defun general-spy-status (gfun-name)
  "Returns a list describing the spy status of a generic function."
  (let ((gfun-spy (find-gfun-spy gfun-name)))
    (if (null gfun-spy)
        nil
        (list :active-p (gfun-spy-implanted-p gfun-spy)
              :before   (mapcar #'spy-identifier
                                (cAr (gfun-spy-cell gfun-spy)))
              :after    (mapcar #'spy-identifier
                                (cDr (gfun-spy-cell gfun-spy))) ))))

(defun report-spy-status (gfun-name &optional (stream *standard-output*))
  "Prints a table describing the spy status of a generic function."
  (declare (values))
  (let ((gfun-spy (find-gfun-spy gfun-name)))
    (if (null gfun-spy)
        (format stream "~&;; ~S is currently not being spied.~%"
                gfun-name)
        (let* ((spy-cell (gfun-spy-cell gfun-spy))
               (before-spies (cAr spy-cell))
               (after-spies  (cDr spy-cell)) )
          (format stream "~&;; ~S is prepared for being spied.~%"
                  gfun-name)
          (format stream
                  ";;  The main spy method is currently ~:[in~;~]active.~%"
                  (gfun-spy-implanted-p gfun-spy))
          (if (and (null before-spies)
                   (null after-spies ))
              (format stream
                      ";;  There are no individual spies at the moment.~%")
              (format stream ";;  The individual spies are:~%" ))
          (dolist (spy before-spies)
            (format stream   ";;    :before ~10A ~@[~A~]~%"
                    (spy-identifier spy) (spy-comment spy)))
          (dolist (spy after-spies)
            (format stream   ";;    :after  ~10A ~@[~A~]~%"
                    (spy-identifier spy) (spy-comment spy))) )))
  (values) )

;;;;;;;  End of file DUAL/INTRFACE/GFUN_SPY.LSP
