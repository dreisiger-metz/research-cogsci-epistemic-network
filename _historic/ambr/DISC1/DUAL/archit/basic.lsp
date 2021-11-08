;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/basic.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Basic agent features and basic housekeeping facilities.
;;; DEPENDS-ON: DUAL/packages.lsp, DUAL/defs.lsp, DUAL/general.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    14-02-97 [1.0]
;;; UPDATED:    15-09-97 [1.1]
;;; UPDATED:    18-04-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...
;;;;;;;;;;;;
;;; TO DO:      Avoid the risk of infinite loops in GENNAME.
;;;


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;   AGENT- AND HOUSEKEEPING BASICS   ;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is BASE-AGENT.
;;;; The file defines functions for constructing and dealing with base agents.
;;;;
;;;; The base agent is an agent which is designed to be a foundation for other
;;;; agents. It is of little use in itself since it has only a name and a
;;;; comment. It is useful mostly for providing a hook for house-keeping and
;;;; printing functions like FIND-AGENT and DUAL-DESCRIBE.
;;;; Other program modules will provide more interesting kinds of agents like
;;;; CONN-AGENT, SYMB-AGENT, and DUAL-AGENT. The latter inherit structure and
;;;; functionality from base agents.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: base-agent, make-base-agent, agentp,
;;          agent-name, agent-comment,
;;          find-agent, #$, do-all-agents
;;          remove-agent, remove-all-agents,
;;          genname, *dead-name-registry* ;
;;          agent-flag, *flag1*, *flag2*, *flag3*, *flag4* ;
;;          agent-descriptor-string
;;
;; Note also the constant *expected-number-of-agents* defined in DUAL/DEFS.LSP.

;; MAKE-BASE-AGENT (name &key :comment :prompt-p)  -->  new-agent
;;
;;   A function that creates a base agent and registers it into the general
;;   registry of all agents in the system.
;;   NAME must be a symbol (but not  NIL).
;;   :PROMPT-P must be either T or NIL; if not supplied, it defaults to T.
;;   If NAME is already in use, MAKE-BASE-AGENT signals an error depending on
;;     the value of :PROMPT-P. If it is T (the default), a continuable error
;;     is signaled giving the user an opportunity to supply a new value
;;     for NAME. If :PROMPT-P is NIL, a non-continuable error is signaled.
;;   :COMMENT must be a string; if not supplied, it defaults to NIL.
;;   MAKE-BASE-AGENT returns the new agent.
;;
;;   After a successful call to MAKE-BASE-AGENT, the following conditions hold:
;;     (find-agent name)          -->  new-agent
;;     (agentp new-agent)         -->  T
;;     (agent-name new-agent)     -->  name
;;     (agent-comment new-agent)  -->  comment  ; or NIL
;;
;;   The function GENNAME (see below) may be used to generate names that are
;;   guaranteed to be 'safe' when passed to MAKE-BASE-AGENT.

;; AGENTP  (thing)  -->  T or NIL
;;
;;   A predicate that checks whether something is an agent.
;;
;;   It is always true that, provided MAKE-xxx-AGENT evaluates without errors:
;;      (agentp (make-base-agent 'b-ag))  -->  T
;;      (agentp (make-conn-agent 'c-ag))  -->  T
;;      (agentp (make-symb-agent 's-ag))  -->  T
;;      (agentp (make-DUAL-agent 'd-ag))  -->  T
;;      (agentp nil)                      -->  NIL
;;      (agentp 'agent-name)              -->  NIL


;; AGENT-NAME (agent)  -->  name
;;
;;   A generic function that reads the name of AGENT. Returns a symbol.
;;   Each agent always has a name and two different agents cannot have the
;;   same name. The name is assigned to the agent at the moment of its
;;   construction (by a call to MAKE-BASE-AGENT) and cannot be modified.
;;   It is always true that:
;;      (find-agent (agent-name agent))      -->  agent
;;      (agent-name (find-agent name))       -->  name
;;      (agent-name nil)                     signals an error
;;      (setf (agent-name agent) 'new-name)  signals an error

;; AGENT-COMMENT (agent)  -->  comment
;; (SETF AGENT-COMMENT)
;;
;;   A generic function that accesses the comment of AGENT.
;;   Returns a string or NIL. May be used with SETF.
;;   Signals an error if AGENT is not a base agent.

;; FIND-AGENT (name)  -->  agent or NIL
;;
;;   A function that looks for an agent whose name is NAME. If an agent
;;   with the same (eq) AGENT-NAME has been registered in the system, it
;;   is returned as the value of FIND-AGENT. If no such agent is found,
;;   FIND-AGENT returns NIL.
;;   If NAME is not a symbol, an error is signaled.
;;
;;   #$name  is a synonym for  (find-agent 'name)
;;   It is always true that:
;;      (find-agent (agent-name agent))      -->  agent
;;      (agent-name (find-agent name))       -->  name
;;
;; #$ name   - - ->  (find-agent 'name)
;;
;;   The character combination #$ is a synonym for (find-agent (quote ...)).
;;   For example:
;;      (describe #$G027)  is equivalent to  (describe (find-agent 'G027))

;; REMOVE-AGENT (agent)  -->  T or NIL
;;
;;   A generic function that removes the agent from the general registry.
;;   Returns T if there was such an agent or NIL otherwise. After an agent
;;   has been removed, it is not accessible via FIND-AGENT.
;;   Other modules of the program are encouraged to provide :AFTER methods
;;   to clean up the traces of the 'dead' agent.
;;
;;   As a side effect, AGENT's name is added to *DEAD-NAME-REGISTRY* (using
;;   pushnew). Thus, it will not be used by future GENNAMEs (see below).
;;   See also REMOVE-ALL-AGENTS.
;;

;; DO-ALL-AGENTS (var [resultform]) {form}*   -->  dependent-on-RESULTFORM
;;
;;   A macro that iterates through all agents in the system (in an unspecified
;;   order).  Similar to DOLIST.
;;
;;   VAR should be a symbol. It is not evaluated and serves as a variable name.
;;   RESULTFORM is an arbitrary LISP form. When not supplied it defaults to NIL.
;;
;;   DO-ALL-AGENTS executes the forms in the body once for each agent in the
;;   system, in some implementation-dependent order, with the variable VAR
;;   bound to the agent.  Then RESULTFORM (a single form, not an implicit
;;   PROGN) is evaluated, and the result is the value of the DO-ALL-AGENTS form.
;;   When the RESULTFORM is evaluated, the control variable VAR is still bound
;;   and has the value NIL.  If RESULTFORM is omitted, the result is NIL.
;;
;;   An explicit RETURN statement may be used to terminate the loop and return
;;   a specified value.  Declarations may appear at the front of the body. Tags
;;   and GO's are not allowed (i.e. DO-ALL-AGENTS is not an implicit TAGBODY).
;;
;;   The user-supplied forms in the body may _not_ add or delete agents from
;;   the system with one exception -- the very agent being processed may be
;;   removed by a call to REMOVE-AGENT. (See pp. 178 and 438 in CLtL2.)
;;
;;   Examples:
;;     (do-all-agents (agent)         -->  returns NIL after printing all
;;       (print (agent-name agent)))         agent names
;;     (do-all-agents (agent)         -->  returns NIL after invoking DO-STUFF
;;       (when (suitable-p agent)            on all agents satisfying SUITABLE-P
;;         (do-stuff agent)))
;;     (do-all-agents (agent)         -->  returns NIL after removing all
;;       (when (offending-p agent)           agents that satisfy OFFENDING-P
;;         (remove-agent agent)))
;;     (let ((count 0))               -->  returns the total number of agents
;;       (do-all-agents (ag count)
;;         (declare (ignore ag))
;;         (incf count)))
;;     (do-all-agents (agent nil)     -->  returns an agent satisfying WANTED-P
;;       (when (wanted-p agent)              or NIL if noone satisfies
;;         (return agent)))
;;
;;   See also DO-ALL-WM in DUAL/ARCHIT/WORK_MEM.LSP and
;;     DO-ALL-PROCESSES in DUAL/ARCHIT/AGENDA.LSP.


;; REMOVE-ALL-AGENTS ()  -->  NIL
;;
;;   A function that removes all agents in the system. Always returns NIL.
;;   After a call to REMOVE-ALL-AGENTS, FIND-AGENT returns NIL on every symbol.
;;
;;   REMOVE-ALL-AGENTS does not remove the agents one by one (via REMOVE-AGENT)
;;     but rather clears them all at once.  Thus, it is not equivalent to:
;;     (do-all-agents (ag) (remove-agent ag))
;;
;;   See also CLEAR-EVERYTHING-IN-DUAL in DUAL/TOPLEVEL.LSP


;; *DEAD-NAME-REGISTRY*  -->  list of symbols
;;
;;   A global variable that keeps the names of the 'dead' agents, i.e. the
;;   names of agents that have existed in the past but have been removed
;;   from the system. This registry is used by GENNAME to avoid re-using
;;   such names.
;;
;;   The value of *DEAD-NAME-REGISTRY* is a (possibly empty) list of symbols.
;;   The list grows through calls to REMOVE-AGENT and is set to NIL by the
;;   function CLEAR-EVERYTHING-IN-DUAL (defined in DUAL/TOPLEVEL.LSP).
;;
;;   It is always true that:
;;      (progn (clear-everything-in-DUAL) *dead-name-registry*)   -->  '()
;;      (member (genname ....) *dead-name-registry*)              -->  NIL
;;      (progn (remove-agent agent)
;;             (member (agent-name agent) *dead-name-registry*))  -->  T
;;

;; GENNAME (format-string arg1 &optional count &rest more-args)  -->
;;                                                   -->  (values name count1)
;;
;;   A function that generates a new agent name on the basis of a FORMAT-style
;;   template. GENNAME is a vague analog to the COMMON-LISP function GENTEMP.
;;
;;   FORMAT-STRING should be a string acceptable to FORMAT (see below).
;;   ARG1 and MORE-ARGS may be any LISP objects.
;;   COUNT should be either NIL (the default) or an integer. If not, error.
;;   The function returns two values described below.
;;
;;   The purpose of GENNAME is to produce a symbol satisfying the following
;;   four criteria:
;;     -- It does not name any agent in the system. In other words, FIND-AGENT
;;        applied to that symbol returns NIL.
;;     -- It is not a member of *DEAD-NAME-REGISTRY*.
;;     -- Its print name (see SYMBOL-NAME in CLtL) conforms to the template
;;        specified by the user in FORMAT-STRING.
;;     -- It is not the symbol NIL.
;;   In the presentation that follows, let's call such a symbol 'appropriate'.
;;
;;   GENNAME loops, incrementing COUNT on each iteration, until an appropriate
;;   symbol is found. When this happens, GENNAME returns two values:
;;     1. NAME   -- the appropriate symbol.
;;     2. COUNT1 -- the value of COUNT that produced the print-name of NAME.
;;
;;   More precisely, GENNAME works according to the following algorithm:
;;     1. If COUNT is neither NIL nor an integer, an error is signaled.
;;     2. A print name (call it PRINT-NAME) is produced as if by the form:
;;         (string-upcase (apply #'format format-string arg1 count more-args))
;;     3. If a symbol with PRINT-NAME is not accessible in the current package
;;         (as indicated by FIND-SYMBOL), then a new symbol is interned in
;;         the current package and returned as the first value of GENNAME.
;;         COUNT is returned as the second value of GENNAME.
;;         (The new symbol is 'appropriate' because agent names necessarily
;;          are 'old' symbols.)
;;     4. If a symbol with PRINT-NAME is accessible in the current package,
;;        then this symbol is checked for being used as an agent name.
;;        If it is not used (i.e. FIND-AGENT applied to it returns NIL), then
;;        it is 'appropriate' and is returned as the first value of GENNAME.
;;        COUNT is returned as the second value of GENNAME.
;;     5. Now, since the tests at steps 3. and 4. have failed, the print name
;;        produced at step 2. is not appropriate. Modify the value of COUNT
;;        and loop back to step 2.
;;        The value of COUNT is modified as follows:
;;          -- If its current value is NIL (either supplied by the caller or
;;             defaulted), then the new value of COUNT is 1.
;;          -- Otherwise, the current value of COUNT is an integer; increment
;;             it with one.
;;
;;   It is the responsibility of the caller of GENNAME to insure that COUNT
;;   is referenced in FORMAT-STRING in such a way that the call to FORMAT
;;   at step 2. above produces a different string for different values of COUNT.
;;   If this condition is not met, GENNAME will enter an infinite loop.
;;
;;   FORMAT-STRING should satisfy the following conditions:
;;     -- It should be suitable for passing to FORMAT.
;;     -- It should contain format directives that 'consume' ARG1, COUNT, and
;;        any MORE-ARGS without errors.
;;     -- It should ensure that different values of COUNT produce diff strings.
;;     -- It should never produce the three-letter string "NIL".
;;
;;   Examples:
;;     (genname "~A-~D"  "container" 3)  -->  CONTAINER-3     or, if used,
;;                                       -->  CONTAINER-4     or, if used,
;;                                       -->  CONTAINER-5     etc.
;;
;;     (genname "~A~@[-~D~]"  'foo)      -->  FOO       or, if used,
;;                                       -->  FOO-1     or, if used,
;;                                       -->  FOO-2     etc.
;;
;;     (genname "~A<-~@[~D~]->~A"
;;              "tumor" nil "fortress")  -->  TUMOR<-->FORTRESS     or, if used,
;;                                       -->  TUMOR<-1->FORTRESS    or, if used,
;;                                       -->  TUMOR<-2->FORTRESS    etc.
;;
;;     (genname "~*~:R~:*~:*-~A" "hack" 1)  -->  FIRST-HACK    or, if used,
;;                                          -->  SECOND-HACK   or, if used,
;;                                          -->  THIRD-HACK    etc.
;;
;;
;;   It is always true that, provided GENNAME returns without errors:
;;      (symbolp (genname ....))         -->  T
;;      (find-agent (genname ....))      -->  NIL
;;      (make-base-agent (genname ...))  -->  agent  (never an error)
;;      (nth-value 1 (genname ...))      -->  NIL or an integer
;;      (multiple-value-bind
;;                (symbol count)
;;                (genname format-string arg1 init-count arg2 ...)
;;        (string-equal
;;           (symbol-name symbol)
;;           (format nil format-string arg1 count arg2 ...)))  -->  T
;;

;; AGENT-FLAG (agent flag-number)  -->  0 or 1
;; (SETF AGENT-FLAG)
;;
;;   A generic function that accesses specified AGENT's flag.
;;   Flags have nothing to do with the 'conceptual' specification of DUAL.
;;   Although they compromise the modularity of the programs, flags sometimes
;;   offer substantial improvement in terms of efficiency.
;;   The current implementation of DUAL uses several flags. They do not belong
;;   to the external protocol and, therefore, are not documented here (but in
;;   the implementation section of the appropriate files).
;;   Typically, the flags are used to establish that some clean-up is due
;;   but it is more efficient to perform the clean-up later. It is likely
;;   that similar cases will occur with the implementation of DUAL-based
;;   models. Anticipating that need, the flag-related facility is advertized
;;   in the external protocol of the architecture.
;;
;;   AGENT should be a base agent.
;;   FLAG-NUMBER should be one of the following constants:
;;      *FLAG1*, *FLAG2*, *FLAG3*, *FLAG4*
;;   Each flag is independent of the others and can take the values 0 and 1.
;;   AGENT-FLAG may be used with SETF to alter the value of the flag.


;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric  remove-agent  (agent)
  (:documentation
"Removes the agent from the general registry.
Returns T if there was such agent or NIL otherwise." ))

(defgeneric agent-descriptor-string (agent)
  (:documentation "A string describing AGENT's type." ))
  ;; Each new class that inherits from BASE-AGENT is recommended to
  ;; define its own primary method according to the template below.

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass base-agent  (Dual-object)
    ((name        :initarg     :name
                  :reader      agent-name
                  :type        symbol
                  :initform    (required-argument) )
     (comment     :accessor    agent-comment
                  :type        (or string null)    ; NIL stands for 'no comment'
                  :initform    nil     )
     (flags       :type        simple-bit-vector
                  :initform    (make-array 7   ;!! increment this for more flags
                                           :element-type 'bit
                                           :initial-element 0)
                  :documentation "Entirely for implementational purposes." )
    )
    (:documentation "Base class for all agents.")
  ) ; defclass
) ; eval-when


;;;;  Type-checking methods for the accessors

(defmethod  agent-name ((x t))
  (error "AGENT-NAME:  ~S is not an agent." x))

(defmethod  (setf agent-name) (new-value (x t))
  (declare (ignore new-value))
  (error "AGENT-NAME cannot be used with SETF. Agent names are immutable.") )

(defmethod  agent-comment ((x t))
  (if x
    (error "AGENT-COMMENT:  ~S is not an agent." x)
    (error "AGENT-COMMENT applied to NIL (perhaps #$missing-agent).") ))

(defmethod  (setf agent-comment) (new-value (x t))
  (declare (ignore new-value))
  (if x
    (error "(SETF AGENT-COMMENT):  ~S is not an agent." x)
    (error "(SETF AGENT-COMMENT) applied to NIL (perhaps #$missing-agent).") ))

(defmethod  agent-comment ((symb symbol))
  (cerror  "Try (agent-comment (find-agent ~S))."
           "AGENT-COMMENT cannot work on symbols (~S)."
           symb)
  (agent-comment (find-agent symb)) )


;;;;;;  Printing methods

(defmethod  agent-descriptor-string ((agent base-agent))
  (if (subtypep 'base-agent (type-of agent))    ; i.e. EXACTLY 'base-agent
      "a 'base' agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod print-object  ((ag base-agent) stream)
  (let ((*print-case* :downcase))
    (format stream  "#$~S"
            (if (slot-boundp ag 'name)
                (agent-name ag)
                "(no name)"))))

(defmethod DUAL-describe  ((ag base-agent)
                           &optional (stream *standard-output*))
  (format stream "~&~S is ~A named ~A."
          ag  (agent-descriptor-string ag)  
              (if (slot-boundp ag 'name)
                  (agent-name ag)
                  "(no name)") )
  (format stream "~&  Its comment string is: ~A~%"
          (if (and (slot-boundp ag 'comment)
                   (agent-comment ag))          ; supplied?
              (agent-comment ag)
              "(no comment)"))
  (values))


;;;;  Type predicate

(declaim (inline agentp))
(defun agentp (thing)
  (if (typep thing 'base-agent) t nil))


;;;;;;;;;;;   Housekeeping functions
;;
;;  The global registry is implemented as a hash table indexed by AGENT-NAME.
;;  The initial size of the hash table is taken from the global constant
;;  *EXPECTED-NUMBER-OF-AGENTS* , which is advertised in the external
;;  protocol (see file DEFS.LSP). The hash table itself is bound to the
;;  global variable *GLOBAL-REGISTRY-OF-ALL-AGENTS*, which is in the domain
;;  of the implementation. Users interact with the registry through the
;;  functions of the external protocol: MAKE-BASE-AGENT, FIND-AGENT,
;;  REMOVE-AGENT, and DO-ALL-AGENTS.

(defvar  *global-registry-of-all-agents*
         (make-hash-table  :size  *expected-number-of-agents*
                           :rehash-size 1.5
                           :test  #'eq )          ; Keys are symbols
         "Hash table of all agents indexed by (the symbol) agent-name.")

(defvar *dead-name-registry*  nil
  "A list of symbols that have been used as agent names in the past." )


(defun find-agent (agent-name)
  "Looks for an agent by (the symbol) NAME. Returns the agent or NIL."
  (declare (values (or base-agent null)))
  (assert (symbolp agent-name))
  (values (gethash agent-name *global-registry-of-all-agents*))
)

;;; #$NAME  is a synonym for  (FIND-AGENT 'NAME).
;;; See the example at p. 364 of CLtL1.
(eval-when (compile load eval)
  (set-dispatch-macro-character  #\# #\$
                        #'(lambda (stream subchar arg)
                            (declare (ignore subchar arg))
                            `(find-agent ',(read stream t nil t)))) )


(defun make-base-agent  (name &key comment (prompt-p t))
  "Makes a base agent and registers it into the total pool of agents."
  (declare (values base-agent)
           (type (or null string) comment) )
  (let ((new-agent (allocate-agent name 'base-agent prompt-p)))
    (when  comment                          ; COMMENT supplied?
      (setf (agent-comment new-agent) comment))
    new-agent))


(defun  allocate-agent  (agent-name agent-type prompt-p)
  "Low-level constructor of agents. Use MAKE-xxx-AGENT instead."
  (flet ((valid-name-p () (and (symbolp agent-name)
                               (not (null agent-name))      ; NIL is not valid
                               (null (find-agent agent-name))) ))
    (if prompt-p
        (assert  (valid-name-p)
          (agent-name)   ; continuable error
          "DUAL-CORE::ALLOCATE-AGENT: ~S is not a symbol or is already in use."
          agent-name)
        (assert  (valid-name-p)
          ()   ; non-continuable error
          "DUAL-CORE::ALLOCATE-AGENT: ~S is not a symbol or is already in use."
           agent-name) )
 ;; All checks successful, do the real work:
    (let ((new-agent (make-instance agent-type :name agent-name)))
      (setf (gethash agent-name *global-registry-of-all-agents*) new-agent)
      new-agent)
  ))


(defmethod  remove-agent  ((agent base-agent))
  (let ((name (agent-name agent)))
    (pushnew name *dead-name-registry*)
    (remhash name *global-registry-of-all-agents*) ))

(defmethod  remove-agent  ((nl null))
  nil )                        ; unexisting agent

(defmethod  remove-agent  ((x t))
  (error "REMOVE-AGENT:  ~S is not an agent." x))


(defun remove-all-agents ()
  "Removes all agents in the system. Always returns NIL."
  (clrhash *global-registry-of-all-agents*)
  nil )
  ;; see CLEAR-EVERYTHING-IN-DUAL in DUAL/TOPLEVEL.LSP


;;;;;;;;;   Iteration across all agents in the registry   ;;;;;;;
;;

(defun  DOLIST-finalization (var-name result-form)
  "Generates a finalization clause for DOLIST-like constructs."
  (declare (type symbol var-name)     ; the name of the DOLIST variable
           (type T result-form) )     ; the third thing in the DOLIST header
  (labels ((VAR-NAME-used-p (form)
             (cond ((eq form var-name)  T )                ; found
                   ((atom form)        NIL)
                   (t (some #'VAR-NAME-used-p form)) )))   ; recurse
    (if (VAR-NAME-used-p result-form) ; CLtL1 states (p.126): "When the result
        `(let ((,var-name nil))       ; form is evaluated, the control variable
           ,result-form)              ; is still bound, and has the value NIL."
        result-form )))               ; Otherwise, however, don't clutter.


(defmacro do-all-agents  (header &body body)
  "Iterates over all agents. Similar to DOLIST."
  (unless (and (listp header)
               (<= 1 (length header) 2)
               (symbolp (first header)) )    ; var-name
    (error "Malformed header in ~S."
           (list* 'do-all-agents header body) ))
  (let ((var-name (first header))
        (result-form (second header))   ; possibly NIL for one-element headers
        (dummy-arg (gensym "DUMMY")) )
    `(block nil
       (maphash #'(lambda (,dummy-arg ,var-name)
                    (declare (ignore ,dummy-arg))
                    ,@body)
                *global-registry-of-all-agents* )
       ,(DOLIST-finalization var-name result-form)) ))

#+:ACLPC
(eval-when (load)      ; provide a more readable message for the status line
  (when allegro:*save-lambda-lists*
    (setf (get 'do-all-agents 'allegro:lambda-list)
          '((var &optional result) &rest body)) ))

#|  ;; Dead code (which, by the way, worked very well).
(defun do-all-agents (fun)
  "Calls FUN over all agents available in the system.
   FUN should be a function of one parameter -- an agent."
  (declare (type (function (base-agent) t) fun) )
  (maphash  #'(lambda (key ag)
                (declare (ignore key)
                         (type base-agent ag))
                (funcall fun ag))
            *global-registry-of-all-agents*)
)
;; Example of usage:  (do-all-agents-fun #'print)
|#


;;;;;;;;;   Generation of fresh agent names    ;;;;;;;;;;
;;
;; Examples:
;;   (genname "~A-~D"  "container" 3)  -->  container-3     or, if it is used,
;;                                     -->  container-4     or, if it is used,
;;                                     -->  container-5     etc.
;;
;;   (genname "~A<-~@[~D~]->~A"
;;            "tumor" nil "fortress")  -->  tumor<-->fortress     or, if used,
;;                                     -->  tumor<-1->fortress    or, if used,
;;                                     -->  tumor<-2->fortress    etc.

(defun  genname (format-string arg1 &optional (count nil) &rest more-args)
  "Generates a fresh agent name. Similar to GENTEMP from Common LISP."
  (declare (type string format-string)
           (type (or null fixnum) count)
           (values symbol (or null fixnum)) )
  (unless (or (null count) (integerp count))
    (error "GENNAME: ~S is neither NIL nor an integer." count) )
  (let ((symbol (genname-aux
                  (string-upcase
                    (apply #'format nil format-string arg1 count more-args)))))
    (cond ((not (null symbol)) (values symbol count))
          ((null count) (apply #'genname format-string arg1 1 more-args))
          ((integerp count) (apply #'genname format-string arg1
                                                    (+ 1 count) more-args)) )))

(defun genname-aux (print-name)
  (declare (type string print-name)
           (values symbol) )   ; a semi-predicate
  ;; Assume that PRINT-NAME is not string-= to "NIL".
  (let ((symbol (find-symbol print-name)) )
    (cond ((null symbol) (intern print-name)) ; brand new symbol --> not in use
          ((find-agent symbol) nil)                   ; old symbol, now in use
          ((member symbol *dead-name-registry*) nil)  ; old symbol, used before
          (t symbol) )))                              ; old symbol, not in use



;;;;;;;;;   Flag-related facilities    ;;;;;;;;;;
;;;;
;;;;  Flags have nothing to do with the 'conceptual' specification of DUAL.
;;;;  They are useful for implementational purposes only and are documented
;;;;  in the implementation sections of appropriate files.
;;;;
;;;;  Although they compromise the modularity of the program, flags sometimes
;;;;  offer substantial improvement in terms of efficiency.
;;;;  Typically, the flags are used to record that some clean-up is due
;;;;  but it is more efficient to perform the clean-up later.
;;;;

(defun agent-flag (ag flag-number)
  (sbit (slot-value ag 'flags)
        flag-number) )

(defun |(setf agent-flag)| (ag flag-number new-value)
  (setf (sbit (slot-value ag 'flags)
              flag-number)
          new-value) )

(defsetf agent-flag |(setf agent-flag)| )


;;;; Flags in current use

(defconstant  *t-link-flag*  0
  "Temporary links have been added. They have to be cleared when exiting WM." )
  ;; Set by :after methods to ADD-G-SLOT, ADD-FACET, etc. in ARCHIT/MCRFRAME.LSP
  ;; Used and cleared by REMOVE-ALL-TEMPORARY-LINKS, defined in ARCHIT/LINKS.LSP
  ;; See also CLEAR-VOLATILE-MEMORY in DUAL/ARCHIT/SYMPROC1.LSP.
  ;; The purpose is to eliminate redunundant calls to REMOVE-ALL-TEMPORARY-LINKS

(defconstant *on-agenda-flag* 1
  "The symbolic processor of the agent is currently working." )
  ;; Set by MAKE-PROCESS in DUAL/ARCHIT/AGENDA.LSP
  ;; Used by TRIGGER-SYMBOLIC-PROCESSOR on :SYMBOL-INPUT events.
  ;; Cleared by REMOVE-FROM-AGENDA (see DUAL/ARCHIT/AGENDA.LSP).
  ;; This flag is used instead of direct searches through *AGENDA*.

(defconstant *recruited-flag* 2
  "The agent has been recruited and is not available." )
  ;; Set by RECRUIT-NODE-CONSTRUCTOR in DUAL/ARCHIT/NC_AGENT.LSP
  ;; Cleared by RECEIVE-SYMBOL in DUAL/ARCHIT/NC_AGENT.LSP
  ;; This flag serves as a lock against race conditions.

(defconstant *flag1* 3 )
(defconstant *flag2* 4 )
(defconstant *flag3* 5 )
(defconstant *flag4* 6 )
;;;;; !!!   !!!   !!!   !!!
;; For more flags, increment the array size of FLAGS in  (defclass base-agent).
;;;;; !!!   !!!   !!!   !!!


;;;;;;;  End of file DUAL/ARCHIT/BASIC.LSP
