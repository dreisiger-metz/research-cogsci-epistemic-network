;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/spy_tool.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Generic function spies -- a tool for verbosers, counters, etc.
;;; DEPENDS-ON: DUAL/intrface/gfun_spy.lsp, =/coalitn.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    21-03-98 [1.1.2]
;;; UPDATED:    21-06-98 Support for coalitions and agents in spy templates.
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...
;;;;;;;;
;;; TO DO:      Document the external interface.


       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;;;        S  P  Y     T  O  O  L  S        ;;;;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-INTERFACE")

;;;; The main concept defined in this file is SPY-TOOL -- a tool for spying
;;;; a generic function for some particular purpose (e.g. VERBOSE, COUNTER).
;;;; Spy tools are based on 'spy teams' (or 'generic function spies') -- see
;;;; file DUAL/INTRFACE/GFUN_SPY.LSP.
;;;;
;;;; ...
;;;;
;;;; See DUAL/INTRFACE/VERBOSE and =/COUNTER.LSP for applications of spy tools.



;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: spy-tool, make-spy-tool, install-spy-tool, uninstall-spy-tool ;
;;          spy-tool-name, spy-tool-identifier,
;;          spy-tool-template, prepare-spy-template, combine-spy-templates,
;;          change-spy-template, spy-template-match-p ;
;;          activate-spy-tool, deactivate-spy-tool, spy-tool-active-p


;; SPY-TOOL
;;
;;   A symbol that is the proper name of the base class of all spy tools.
;;   As such, it is a valid type specifier.
;;   It may be used as a second argument to the function MAKE-SPY-TOOL.
;;   It is also very useful when defining new types (i.e. classes) of
;;   spy tools. (See files DUAL/INTRFACE/VERBOSE.LSP and =/COUNTER.LSP.)


;; MAKE-SPY-TOOL (name class-name gfun-name &key lambda-list cerror-p) -->
;;                                                                 --> spy-tool
;;
;;   A function that makes a spy tool and associates it with a 'spy team' by
;;   calling BEGIN-SPYING (see DUAL/INTRFACE/GFUN_SPY.LSP).
;;
;;   Returns an implementation-dependent object representing the new spy tool.
;;   ...
;;
;;   The call to MAKE-SPY-TOOL is usually followed immediately by a call to
;;   INSTALL-SPY-TOOL as shown in the example below.
;;

;; INSTALL-SPY-TOOL  (spy-tool qualifier &key closure comment template
;;                                            activate-p priority)  --> spy-tool
;;
;;   A generic function that ....
;;
;;   CLOSURE must be supplied (even though it is a keyword argument) and must
;;    be bound to a closure.  It is passed to the function ADD-SPY (see file
;;    DUAL/INTRFACE/GFUN_SPY.LSP).
;;   ...
;;
;;   Example:
;;    (let ((new-spy (make-spy-tool 'WM-watchdog
;;                                  'spy-tool
;;                                  'add-to-WM)))  ; the gener.fun being spied
;;      (install-spy-tool new-spy :after
;;        :comment  "Watch when temporary or MP agents enter WM."
;;        :template '(temp-dual-agent MP-dual-agent)
;;        :closure  #'(lambda (added-p agent &rest args)
;;                      (declare (ignore args))
;;                      (when (spy-template-match-p (spy-tool-template new-spy)
;;                                                  agent)
;;                        (if added-p     ; the primary value of ADD-TO-WM
;;                            (format t "~&;; Agent ~S added to WM.~%"
;;                                      agent)
;;                            (format t "~&;; Agent ~S already is in WM.~%"
;;                                      agent)))) ))
;;

;; UNINSTALL-SPY-TOOL (spy-tool &optional deactivate-p)  -->  T or NIL
;;
;;   A generic function that undoes the effect of INSTALL-SPY-TOOL.
;;
;;   ...   (Similar to TRACE and UNTRACE.)


;; SPY-TOOL-ACTIVE-P (spy-tool)  -->  (values ... ...)
;;
;;   ...

;; ACTIVATE-SPY-TOOL (...)    -->  T or NIL
;; DEACTIVATE-SPY-TOOL (...)  -->  T or NIL
;;
;;   A generic function that ....
;;   ...

;; SPY-TOOL-NAME (spy-tool)  -->  name
;; ...
;; SPY-TOOL-IDENTIFIER (spy-tool)  -->  identifier
;; ...
;; SPY-TOOL-TEMPLATE (spy-tool)  -->  template
;; ....

;; CHANGE-SPY-TEMPLATE (spy-tool new-template &optional activate-p)  -->
;;                                                      -->  new-template
;;
;;  ...

;; SPY-TEMPLATE-MATCH-P  (template &rest args)  -->  match
;;
;;  ...


;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric spy-tool-identifier (spy-tool)
  (:documentation "A symbol identifying this particular kind of spy tools." ))

(defgeneric install-spy-tool (spy-tool qualifier
                              &key closure comment activate-p priority template)
  (:documentation "Establish the individual spy method of SPY-TOOL." ))

(defgeneric uninstall-spy-tool (spy-tool &optional deactivate-p)
  (:documentation
    "Undo the effect of INSTALL-SPY-TOOL (without calling STOP-SPYING)." ))

(defgeneric activate-spy-tool (spy-tool &optional activate-gfun-spy-p)
  (:documentation "Activates the individual spy method of SPY-TOOL." ))

(defgeneric deactivate-spy-tool (spy-tool &optional deactivate-gfun-spy-p)
  (:documentation "Dectivates the individual spy method of SPY-TOOL." ))

(defgeneric change-spy-template (spy-tool new-template
                                          &optional activate-p)
  (:documentation "Sets the template of SPY-TOOL." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;



;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;;;  Class definition and accessor methods

(eval-when (compile load eval)
  (defclass  spy-tool  (DUAL-interface-object)
    ((name        :reader      spy-tool-name
                  :initarg     :name
                  :type        symbol
                  :initform    (required-argument) )
     (gfun-spy    :reader      spy-tool-gfun-spy   ; DUAL/INTRFACE/GFUN_SPY.LSP
                  :initarg     :gfun-spy
                  :type        gfun-spy
                  :initform    (required-argument) )
     (indiv-spy   :accessor    spy-tool-indiv-spy
                  :type        list                ; DUAL/INTRFACE/GFUN_SPY.LSP
                  :initform    NIL )
     (template    :reader      spy-tool-template   ; for the external protocol
                                                   ; see CHANGE-SPY-TEMPLATE
                  :accessor    spy-tool-template-1 ; for the internal protocol
                  :type        T
                  :initform    nil )
    )
   (:documentation "Tool for spying generic functions for particular purpose."))
) ; eval-when


;;;;;;;  Constructor

(defun make-spy-tool (name class-name gfun-name
                                      &key (lambda-list T) (cerror-p nil) )
  (declare (type symbol name class-name gfun-name)
           (values spy-tool) )
  (let ((gfun-spy (begin-spying gfun-name 
                                :lambda-list lambda-list :cerror-p cerror-p)))
    (if (and (find-class class-name nil)
             (subtypep class-name 'spy-tool))
        (make-instance class-name :name      name
                                   :gfun-spy  gfun-spy)
        (error "DUAL-INTERFACE::MAKE-SPY-TOOL: ~S is not a subclass of SPY-TOOL."
               class-name) )))


;;;;;;;  Print method

(defmethod print-object  ((stool spy-tool) stream)
  (let ((*print-case* :downcase)
        (identifier (spy-tool-identifier stool)))
    (format stream  "#<~A ~A>"
            identifier
            (if (slot-boundp stool 'name)
                (spy-tool-name stool)
                "(no name)"))))


;;;;;;;  The spy-tool identifier is a symbol unique for each subclass.
;;
;; Only a default method is supplied here.
;; Classes inheriting from SPY-TOOL should provide their own methods.
;; SPY-TOOL-IDENTIFIER should always return without errors when applied
;;   to a spy-tool. (Otherwise PRINT-OBJECT will fail.)

(defmethod spy-tool-identifier ((stool spy-tool))
  'spy-tool)

(defmethod spy-tool-identifier ((x t))
  (error "SPY-TOOL-IDENTIFIER: ~S is not a spy tool." ))


;;;;;;;  Installing and uninstalling spy tools
;;

(defmethod install-spy-tool ((spy-tool spy-tool)  qualifier
                             &key (closure     (required-argument))
                                  (comment     nil)
                                  (activate-p  T)
                                  (priority    :cerror)
                                  (template    nil)     )
  (declare (type symbol qualifier priority)
           (type function closure) )
  (let* ((gfun-name (gfun-spy-name (spy-tool-gfun-spy spy-tool)))
         (indiv-spy (add-spy gfun-name
                             (spy-tool-identifier spy-tool)  ; e.g. VERBOSE
                             qualifier                       ; :BEFORE or :AFTER
                             closure
                             :comment    comment
                             :priority   priority
                             :activate-p activate-p )) )
    (setf (spy-tool-indiv-spy  spy-tool) indiv-spy)
    (setf (spy-tool-template-1 spy-tool) template )
    spy-tool ))

(defmethod install-spy-tool ((x t) qualifier &rest key-args)
  (declare (ignore qualifier key-args))
  (error "INSTALL-SPY-TOOL: ~S is not a spy tool." ))


(defmethod  uninstall-spy-tool ((spy-tool spy-tool)
                                &optional (deactivate-p T))
  (let ((identifier (spy-tool-identifier spy-tool))
        (indiv-spy  (spy-tool-indiv-spy  spy-tool)) )
    (if (null indiv-spy)
        nil
        (let ((gfun-name (gfun-spy-name (spy-tool-gfun-spy spy-tool)))
              (qualifier (spy-qualifier indiv-spy)) )
          (remove-spy gfun-name identifier qualifier deactivate-p)
          (setf (spy-tool-indiv-spy spy-tool) nil)
          T) )))

(defmethod uninstall-spy-tool ((not-found null) &optional deactivate-p)
  (declare (ignore deactivate-p))
  nil )    ; E.g. (uninstall-spy-tool (find-verbose-tool 'no-such-spy))

(defmethod uninstall-spy-tool ((x t) &optional deactivate-p)
  (declare (ignore deactivate-p))
  (error "UNINSTALL-SPY-TOOL: ~S is not a spy tool." x ))


;;;;;;;;  Activating and deactivating spy tools
;;

(defun spy-tool-active-p (spy-tool)
  "Is the spy method of SPY-TOOL implanted into the respective spy team?"
  (declare (type spy-tool spy-tool)
           (values boolean boolean) )    ; (values individual-p gfun-p)
  (let ((gfun-spy   (spy-tool-gfun-spy   spy-tool))
        (indiv-spy  (spy-tool-indiv-spy  spy-tool)) )
    (values (and indiv-spy
                 (find-individual-spy gfun-spy (spy-tool-identifier spy-tool)
                                               (spy-qualifier indiv-spy))
                 T)
            (gfun-spy-implanted-p gfun-spy)) ))


(defmethod  activate-spy-tool ((spy-tool spy-tool)
                               &optional (activate-gfun-spy-p T) )
  (declare (values boolean))
  (let ((gfun-spy   (spy-tool-gfun-spy   spy-tool))
        (indiv-spy  (spy-tool-indiv-spy  spy-tool)) )
    (cond ((null indiv-spy)
              (cerror "Return NIL and continue."
                      "ACTIVATE-SPY-TOOL: ~S has no individual spy method."
                      spy-tool )
              nil )
          ((not (spy-tool-active-p spy-tool))
              (add-individual-spy gfun-spy indiv-spy
                                  :activate-p activate-gfun-spy-p)
              T )
          (activate-gfun-spy-p
              (activate-spy-method gfun-spy)
              nil)
          (t  nil) )))


(defmethod  deactivate-spy-tool ((spy-tool spy-tool)
                                 &optional (deactivate-gfun-spy-p T) )
  (declare (values boolean))
  (let* ((gfun-spy   (spy-tool-gfun-spy   spy-tool))
         (indiv-spy  (spy-tool-indiv-spy  spy-tool))
         (identifier (spy-tool-identifier spy-tool))
         (qualifier  (spy-qualifier indiv-spy)) )
    (cond ((null indiv-spy)
              (cerror "Return NIL and continue."
                      "DEACTIVATE-SPY-TOOL: ~S has no individual spy method."
                      spy-tool )
              nil )
          ((find-individual-spy gfun-spy identifier qualifier)
              (remove-individual-spy gfun-spy identifier qualifier
                                                         deactivate-gfun-spy-p)
              T )
          (t  nil) )))



;;;;;;;;  ********  SPY TOOL TEMPLATES  *********
;;
;;  [This text belongs to the external protocol, not the implementation.]
;;
;;  The template mechanism contributes a lot to the flexability of spy methods.
;;  If the spy methods are defined (by the user) using SPY-TEMPLATE-MATCH-P,
;;  they may allow the user to control dynamically the spying mechanism.
;;  In the current implementation, there are three kinds of templates:
;;   + NIL  -- SPY-TEMPLATE-theyMATCH-P always returns NIL.  Therefore, the
;;               test clause in the individual spy method that depends on the
;;               template will always fail.  To improve efficiency, the whole
;;               individual spy method may be deactivated (via DEACTIVATE-SPY-
;;               TOOL) in such cases.  If all individual spies of a spy team
;;               (see DUAL/INTRFACE/GFUN_SPY.LSP) are deactivated, the main spy
;;               method may also be deactivated (via DEACTIVATE-SPIES) thus
;;               removing any overhead from the generic function.
;;   + T    -- SPY-TEMPLATE-MATCH-P always returns T. Therefore, the individual
;;               spy method that depends on it will run unconditionally.
;;   + list -- SPY-TEMPLATE-MATCH-P will discriminate between the arguments of
;;               the generic function call depending on the template. Therefore,
;;               the individual spy method will trigger only on the listed
;;               classes of arguments.

(defun spy-template-match-p (template &rest arguments)
  "Do ARGUMENTS match the spy TEMPLATE?"
  (declare (type (or boolean list) template)
           (values boolean) )
  (flet ((match-p (arg)
           (dolist (template-elt template NIL)
             (typecase template-elt
               (class  (when (typep arg template-elt) (return T)) )
               (symbol (when (typep arg template-elt) (return T)) )
               (coalition (when (member arg (coalition-members template-elt))
                            (return T)) )
               (DUAL-object (when (eq arg template-elt) (return T)) )
               (t nil))) )) ; else loop
    (cond ((null template) nil)
          ((eq template t) t)
          ((listp template) (some #'match-p arguments))
          (t (error "SPY-TEMPLATE-MATCH-P: Bad template: ~S." template)) )))


(defun  prepare-spy-template (items)
  "Transform a list of ITEMS into a spy-tool template."
  (declare (type list items)
           (values list) )
  (let ((good-items nil)
        (bad-items  nil) )
    (dolist (item items)
      (typecase item
        (coalition      (push item good-items)) ; see DUAL/INTRFACE/COALITN.LSP
        (DUAL-object    (push item good-items)) ; agent, marker, etc.
        (standard-class (push item good-items)) ; class metaobject
        (symbol  (if (find-class item nil)      ; class-name ?
                     (push (find-class item)    ; use class metaobject itself
                           good-items)
                     (push item bad-items)))
        (t  (push item bad-items)) ))
    (when bad-items
      (cerror "Ignore them and return only the legal ones."
              "PREPARE-SPY-TEMPLATE: Illegal items ~S."
              bad-items ))
    (nreverse good-items) ))

(defun  combine-spy-templates (old-template new-template operation)
  "Set, add or remove items to OLD-TEMPLATE according to NEW-TEMPLATE."
  (declare (type (member :set :add :remove) operation)
           (type (or boolean list) old-template)
           (values (or boolean list)) )   ; combined template
  (cond ((eq T new-template)
           (ecase operation
             (:set      T)
             (:add      T)                        ; foo + T = T
             (:remove nil) ))                     ; foo - T = nil
        ((eq operation :set)
           (prepare-spy-template new-template))   ; ignore OLD-TEMPLATE
        ((eq T old-template)
           (ecase operation
             (:add    T)                          ; T + foo = T
             (:remove (null new-template)) ))     ; T - nil = T; T - foo = nil
        ((listp new-template)
           (let ((prepared-template (prepare-spy-template new-template)))
             (ecase operation
               (:add    (union old-template prepared-template))
               (:remove (set-difference old-template prepared-template)) )))
        (t (combine-spy-templates old-template
                        (list new-template)       ; bare-item --> (bare-item)
                        operation)) ))



(defmethod  change-spy-template ((spy-tool spy-tool)
                                 (new-template null)
                                 &optional (activate-p T) )
  (setf (spy-tool-template-1 spy-tool) nil)
  (when activate-p                        ; Deactivate the spy method as
    (deactivate-spy-tool spy-tool T))     ; the template is NIL anyway.
  nil )

(defmethod  change-spy-template ((spy-tool spy-tool)
                                 (new-template (eql T))
                                 &optional (activate-p T) )
  (setf (spy-tool-template-1 spy-tool) T)
  (when activate-p                      ; Activate the spy method (if needed).
    (activate-spy-tool spy-tool T))
  T )

(defmethod  change-spy-template ((spy-tool spy-tool)
                                 (new-template list)   ; non-empty
                                 &optional (activate-p T) )
  (let ((prepared-template (prepare-spy-template new-template)))
    (setf (spy-tool-template-1 spy-tool) prepared-template)
    (when activate-p                     ; Activate the spy method (if needed).
      (activate-spy-tool spy-tool T))
     prepared-template))

(defmethod  change-spy-template ((x t) (y t)
                                 &optional (activate-p T) )
  (declare (ignore activate-p))
  (error 
    "CHANGE-SPY-TEMPLATE: ~S is not a spy tool or ~S is not a valid template." 
    x y))

(defsetf spy-tool-template (spy-tool) (new-template)
  `(progn
     (cerror "Translate into a call to CHANGE-SPY-TEMPLATE and continue."
             "SPY-TOOL-TEMPLATE cannot be used with SETF.")
     (change-spy-template ,spy-tool ,new-template) ))  ; default ACTIVATE-P


;;;;;;;  End of file DUAL/INTRFACE/SPY_TOOL.LSP
