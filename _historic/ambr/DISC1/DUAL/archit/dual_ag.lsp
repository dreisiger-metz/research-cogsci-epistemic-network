;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/DUAL_ag.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    DUAL agents -- everything put together.
;;; DEPENDS-ON: defs.lsp, DUAL/archit/hybrid.lsp, DUAL/archit/work_mem.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    24-04-97 [1.0]
;;; UPDATED:    19-12-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;
;;; TO DO:      Document the peculiarities of special DUAL agents.
;;; TO DO:      Add new keyword params to MAKE-DUAL-AGENT and its subordinates.
;;;


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;;       D  U  A  L      A  G  E  N  T  S        ;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is DUAL-AGENT.
;;;; The file defines a constructor for such agents as well as some :BEFORE
;;;; and :AFTER methods for the generic functions defined in other files.
;;;; It also defines the concept of SPECIAL-DUAL-AGENT.
;;;;
;;;; As postulated in the specification of the architecture ("DUAL Report #1")
;;;; DUAL agent are hybrid entities that have many aspects.  Note, in parti-
;;;; cular, the following two axes:
;;;;   -- symbolic vs. connectionist aspect of a DUAL agent   and
;;;;   -- declarative vs. procedural aspect of a DUAL agent.
;;;; These two axes are orthogonal, as shown in Table 3.1. from DR#1:
;;;;        +---------------+-------------------------------+
;;;;        |               | Representation   Processing   |
;;;;        +---------------+-------------------------------+
;;;;        | Connectionist |  activation      spreading    |
;;;;        | aspect        |  level           activation   |
;;;;        +- - - - - - - -+- - - - - - - - - - - - - - - -+
;;;;        | Symbolic      |  symbolic        symbol       |
;;;;        | aspect        |  structures      manipulation |
;;;;        +---------------+-------------------------------+
;;;;
;;;; This theoretical specification is implemented by a hierarchy of classes.
;;;; At the top of the hierarchy is BASE-AGENT (see DUAL/ARCHIT/BASIC.LSP).
;;;; The class CONNECTIONIST-AGENT implements the first row in the table.
;;;; The class MICRO-FRAME implements the symbolic representational cell, and
;;;; the class SYMBOLIC-PROCESSOR -- the symbolic processing cell.  The latter
;;;; two aspects are combined in SYMBOLIC-AGENT (see DUAL/ARCHIT/MCRFRAME.LSP,
;;;; =/SYMPROCx.LSP, and =/SYMBOLIC.LSP, respectively).
;;;; The class HYBRID-AGENT integrates the connectionist and symbolic aspects.
;;;; The class WM-AGENT adds functionality needed for participation in working
;;;; memories (see DUAL/ARCHIT/HYBRID.LSP and =/WORK_MEM.LSP).
;;;; All this culminates into the class DUAL-AGENT (defined in this file).
;;;; Other files in the DUAL/ARCHIT directory continue this lineage.  In parti-
;;;; cular, there are TEMP-DUAL-AGENTs, node-constructors, and marker-passers.
;;;;
;;;; The diagram below illustrates the main classes of agents and the taxonomic
;;;; relations between them.
;;;;
;;;;                        base-agent                  slot-bundle
;;;;                      /     |         \            /
;;;;                    /    symbolic-      micro-frame
;;;;                  /      processor       /
;;;;                /              \       /
;;;;     connectionist-agent      symbolic-agent
;;;;             |        \         /
;;;;         WM-agent      hybrid-agent
;;;;               \       /
;;;;  *********    DUAL-agent    *************** abstraction barrier *********
;;;;              /    |      \
;;;;            /      |        \
;;;;      temp-       special-     NCR-sending-
;;;;    DUAL-agent   DUAL-agent       agent
;;;;        |            |              |
;;;;      dead-       node-        MP-DUAL-agent
;;;;    temp-agent   constructor
;;;;
;;;;
;;;; Typically, each major class is defined in a separate file as shown below.
;;;; All files are in the DUAL/ARCHIT directory (see DUAL/DEFSYS.LSP):
;;;;    base-agent          in  basic.lsp
;;;;    connectionist-agent in  connect.lsp               ; see also act_fun.lsp
;;;;    micro-frame         in  filler.lsp, slots.lsp, mcrframe.lsp, links.lsp
;;;;    symbolic-processor  in  symproc1.lsp, agenda.lsp, sprogn.lsp, symproc2.l
;;;;    symbolic-agent      in  symbolic.lsp
;;;;    hybrid-agent        in  hybrid.lsp
;;;;    WM-agent            in  work_mem.lsp
;;;;    DUAL-agent          in  dual_ag.lsp (this file)
;;;;    special-DUAL-agent  in  dual_ag.lsp (this file)
;;;;    temp-DUAL-agent     in  temp_ag.lsp
;;;;    dead-temp-agent     in  temp_ag.lsp
;;;;    node-constructor    in  nc_agent.lsp
;;;;    NCR-sending-agent   in  nc_agent.lsp
;;;;    MP-DUAL-agent       in  mp_agent.lsp
;;;;


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: DUAL-agent, DUAL-agent-p, valid-DUAL-agent-type-p,
;;          make-DUAL-agent, initialize-agent, establish-agent-integrity,
;;          special-DUAL-agent, special-agent-p
;;
;; The functions defined in this file refer to the following global variables:
;;   *DEFAULT-AGENT-EFFICIENCY*  defined in DEFS.LSP,
;;   *WM-THRESHOLD*  defined in DEFS.LSP,
;;   *WM*  defined in ARCHIT/WORK_MEM.LSP.

;; DUAL-AGENT
;;
;;   A symbol that is the proper name of the class of all DUAL agents.
;;   (See CLOS specification.) As such, it is a valid type specifier.
;;   It may be used as a second argument to the function MAKE-DUAL-AGENT.
;;   It is also very useful when defining new types (i.e. classes) of
;;   DUAL agents (see ARCHIT/NEW_TYPE.LSP). In fact, the symbol DUAL-AGENT
;;   is advertized in the external protocol mainly to facilitate definition
;;   of new DUAL-agent types.
;;
;;   Explicit use of DUAL-AGENT should be avoided. Use the appropriate data-
;;   abstraction routines instead. For instance:
;;     (DUAL-agent-p x)    is to be preferred to   (typep x 'DUAL-agent)
;;

;; DUAL-AGENT-P (thing)  -->  T or NIL
;;
;;   A predicate that checks whether something is a DUAL agent.
;;   THING may be arbitrary LISP object.
;;   DUAL-AGENT-P implies AGENT-P (see ARCHIT/BASIC.LSP).
;;
;;   It is always true that, providing MAKE-xxx-AGENT evaluates without errors:
;;      (if (DUAL-agent-p x) (agent-p x) t)                          -->  T
;;      (if (agent-p x) nil (DUAL-agent-p x))                        -->  NIL
;;      (DUAL-agent-p (make-base-agent 'b-ag))                       -->  NIL
;;      (DUAL-agent-p (make-conn-agent 'c-ag))                       -->  NIL
;;      (DUAL-agent-p (make-symb-agent 's-ag))                       -->  NIL
;;      (DUAL-agent-p (make-hybrid-agent 'h-ag))                     -->  NIL
;;      (DUAL-agent-p (make-DUAL-agent 'ag1 'DUAL-agent))            -->  T
;;      (DUAL-agent-p (make-DUAL-agent 'ag2 'special-DUAL-agent))    -->  T
;;      (DUAL-agent-p (make-DUAL-agent 'ag3 'temp-DUAL-agent))       -->  T
;;      (DUAL-agent-p (make-DUAL-agent 'ag4 'MP-DUAL-agent))         -->  T
;;      (DUAL-agent-p nil)                                           -->  NIL
;;      (DUAL-agent-p 'agent-name)                                   -->  NIL
;;

;; VALID-DUAL-AGENT-TYPE-P (thing)  -->  T or NIL
;;
;;   A predicate that checks whether something is a valid DUAL-agent type.
;;   THING may be arbitrary LISP object.
;;
;;   VALID-DUAL-AGENT-TYPE-P is equivalent to the following form:
;;     (if (and (symbolp thing)
;;              (find-class thing nil)
;;              (subtypep thing 'DUAL-agent)) 
;;         t 
;;         nil)
;;
;;   The symbol 'DUAL-AGENT itself is a valid DUAL-agent-type, of course.
;;   Other valid DUAL-agent types include 'TEMPORARY-DUAL-AGENT (defined
;;   in ARCHIT/TEMP_AG.LSP) and 'MP-DUAL-AGENT (defined in ARCHIT/MP_AGENT.LSP).
;;   Programmers may define new DUAL-agent types (see ARCHIT/NEW_TYPE.LSP).


;; MAKE-DUAL-AGENT (name agent-type &rest initargs 
;;                             &key :prompt-p &allow-other-keys)  -->  new-agent
;;
;;   A function that constructs (and registers) a DUAL agent.
;;   NAME must be a symbol (but not  NIL).
;;   :PROMPT-P must be either T or NIL; if not supplied, it defaults to T.
;;   If NAME is already in use, MAKE-DUAL-AGENT signals an error depending on
;;     the value of :PROMPT-P. If it is T (the default), a continuable error
;;     is signaled giving the user an opportunity to supply a new value
;;     for NAME. If :PROMPT-P is NIL, a non-continuable error is signaled.
;;   AGENT-TYPE must satisfy VALID-DUAL-AGENT-TYPE-P (see above).
;;     If it isn't, MAKE-DUAL-AGENT signals an error depending on the value
;;     of :PROMPT-P. If it is T (the default), a continuable error is signaled
;;     giving the user an opportunity to supply a new value for AGENT-TYPE.
;;     If :PROMPT-P is NIL, a non-continuable (fatal) error is signaled.
;;   INITARGS should be acceptable by INITIALIZE-AGENT (see below).
;;   MAKE-DUAL-AGENT returns the new DUAL agent.
;;
;;   MAKE-DUAL-AGENT works according to the following algorithm:
;;     1. It checks whether AGENT-TYPE satisfies VALID-DUAL-AGENT-TYPE-P.
;;        If it doesn't, an error is signaled depending on :PROMPT-P.
;;     2. It checks whether NAME is a symbol and not in used by another
;;        agent (via a call to FIND-AGENT, see DUAL/ARCHIT/BASIC.LSP).
;;        If NAME is not a symbol, or is the symbol NIL, or there already is an
;;        agent with that name, an error is signaled depending on :PROMPT-P.
;;     3. If the checks 1. and 2. above have been successful, MAKE-DUAL-AGENT
;;        constructs a brand new agent of the type denoted by AGENT-TYPE.
;;        In the documentation that follows,this agent will be called NEW-AGENT.
;;     4. NEW-AGENT is registered into the general register of all agents
;;        in the system (cf. MAKE-BASE-AGENT defined in DUAL/ARCHIT/BASIC.LSP).
;;        From now on,  (EQ (FIND-AGENT NAME) NEW-AGENT)  will return T.
;;     5. The generic function INITIALIZE-INSTANCE is applied to NEW-AGENT
;;        and INITARGS, as if by the form:
;;          (apply #'initialize-agent new-agent initargs)
;;        Note that if :PROMPT-P has been supplied to MAKE-DUAL-AGENT, it
;;        will be passed over to INITIALIZE-AGENT. If it has not been supplied
;;        explicitly to MAKE-DUAL-AGENT, it will not be supplied explicitly
;;        to INITIALIZE-AGENT either. Thus, the latter may define its own
;;        default for :PROMPT-P.
;;     6. The generic function ESTABLISH-AGENT-INTEGRITY is applied to the
;;        initialized agent as if by the form:
;;          (establish-agent-integrity new-agent)
;;     7. The agent -- constructed, initialized, and integrated as described --
;;        is returned from MAKE-DUAL-AGENT.
;;
;;   Programmers may customize the process of agent creation by supplying
;;   additional methods to the generic functions INITIALIZE-AGENT and
;;   ESTABLISH-AGENT-INTEGRITY documented below.
;;
;;   After a successful call to MAKE-DUAL-AGENT, the following holds:
;;     (find-agent name)             -->  new-agent
;;     (agentp new-agent)            -->  T
;;     (DUAL-agent-p new-agent)      -->  T
;;     (agent-name new-agent)        -->  name
;;
;;   Examples:
;;   ~~~~~~~~~
;;   Provided there isn't any changes to the predefined agent-creation routine
;;   (that is, there are no user-defined methods for INITIALIZE-AGENT and
;;   ESTABLIST-AGENT-INTEGRITY), the following sample calls to MAKE-DUAL-AGENT
;;   would produce the results shown after each call.
;;
;;     (make-DUAL-agent 'ag1 'DUAL-agent)  -->  #$ag1
;;        (agent-name #$ag1)        -->  ag1
;;        (DUAL-agent-p #$ag1)      -->  T
;;        (agent-comment #$ag1)     -->  NIL
;;        (agent-activation #$ag1)  -->  0.0
;;        (agent-output #$ag1)      -->  0.0
;;        (agent-visible-p #$ag1)   -->  NIL
;;        (now-in-WM #$ag1)         -->  NIL
;;        (agent-G-slots #$ag1)     -->  NIL
;;        (agent-S-slots #$ag1)     -->  NIL
;;        (agent-neighbors #$ag1)   -->  NIL
;;        (agent-input-zone #$ag1)  -->  NIL
;;        (agent-efficiency #$ag2)  -->  0.5  ;; see *DEFAULT-AGENT-EFFICIENCY*
;;
;;     (make-DUAL-agent 'ag2 'DUAL-agent
;;                :comment "Hello, there!"
;;                :initial-act 1.0
;;                :G-slots (list (make-G-slot :a-link
;;                                  :filler (make-conref #$ag1 0.5)))) --> #$ag2
;;        (agent-name #$ag2)        -->  ag2
;;        (DUAL-agent-p #$ag2)      -->  T
;;        (agent-comment #$ag2)     -->  "Hello, there!"
;;        (agent-activation #$ag2)  -->  1.0
;;        (agent-output #$ag2)      -->  1.0
;;        (agent-visible-p #$ag2)   -->  T
;;        (now-in-WM #$ag2)         -->  #<primary-WM>    ; see *WM* in WORK_MEM
;;        (agent-G-slots #$ag2)     -->  (#<G-slot a-link>)
;;        (agent-S-slots #$ag2)     -->  NIL
;;        (agent-neighbors #$ag2)   -->  ((#$ag1 . 1.0))  ; 0.5 is normalized
;;        (agent-input-zone #$ag2)  -->  NIL
;;        (agent-efficiency #$ag2)  -->  0.5  ;; see *DEFAULT-AGENT-EFFICIENCY*
;;
;;   The macro DEFAGENT (defined in DUAL/INTRFACE/DEFAGENT.LSP) serves as an
;;   interface to MAKE-DUAL-AGENT and should be the construct of choice for
;;   most purposes (as is evident from the second example above).
;;

;; INITIALIZE-AGENT (new-agent &rest initargs)  -->  new-agent
;;
;;   A generic function that is called by MAKE-DUAL-AGENT to initialize the
;;   newly constructed agent. It is not intended to be called directly.
;;   Programmers may supply additional methods (preferably :AFTER methods)
;;   for this generic function in order to customize the initialization process.
;;
;;   NEW-AGENT should be a DUAL-agent; if it isn't, an error is signaled.
;;   INITARGS is a list of alternating keywords and values. The predefined
;;     primary method for INITIALIZE-AGENT recognizes the following keywords:
;;     :COMMENT -- must be a string; if not supplied, it defaults to NIL.
;;     :INITIAL-ACT -- must be a number in the interval [*MIN-ACT*, *MAX-ACT*];
;;                     if not supplied, it defaults to 0.0.
;;     :G-SLOTS -- must be a list of G-slots (see DUAL/ARCHIT/SLOTS.LSP)
;;     :S-SLOTS -- must be a list of S-slots (see DUAL/ARCHIT/SLOTS.LSP)
;;                  When not supplied, G- and S-SLOTS default to the empty list.
;;     :PROMPT-P -- must be either T or NIL.
;;     These keywords are identical to the keywords recognized by the
;;     (non-generic) function MAKE-HYBRID-AGENT, defined in ARCHIT/HYBRID.LSP.
;;
;;   INITIALIZE-AGENT returns NEW-AGENT. That is, the result of the generic
;;   function is always EQ to its first argument.
;;
;;   There is a predefined primary method for INITIALIZE-INSTANCE. It has
;;   the following method signature:
;;     initialize-agent ((new-agent DUAL-agent) &key (comment nil)
;;                                                   (initial-act 0.0)
;;                                                   (G-slots nil)
;;                                                   (S-slots nil)
;;                                                   prompt-p      )
;;   The predefined primary method sets (some) of NEW-AGENT's slots with
;;   corresponding values. Each keyword argument affects only one slot.
;;   The predefined primary method ignores the argument :PROMPT-P; it is
;;   mentioned in the lambda list because it is occasionally passed over
;;   by MAKE-DUAL-AGENT.
;;
;;   The generic function ESTABLISH-AGENT-INTEGRITY is usually invoked after
;;   the call to INITIALIZE-AGENT to finish the agent-construction process.
;;
;;   Programmers may define additional methods (preferably :AFTER methods) for
;;   INITIALIZE-AGENT to customize the initialization process.
;;   Each method for INITIALIZE-AGENT can use the keyword arguments listed
;;   above, as well as accept additional keyword arguments of its own.
;;   If it does not want to deal with any keyword arguments, it should specify
;;   &REST but not &KEY in its method signature in order to conform with the
;;   generic function lambda-list (see "Congruent lambda-lists" in CLOS
;;   reference manual (chapter 28 in CLtL2)).
;;

;; ESTABLISH-AGENT-INTEGRITY (new-agent)  -->  new-agent
;;
;;   A generic function that is called by MAKE-DUAL-AGENT to establish the
;;   integrity of the newly constructed and initialized (by INITIALIZE-AGENT)
;;   agent. It is not intended to be called directly.
;;   Programmers may supply additional methods (preferably :AFTER methods)
;;   for this generic function in order to customize the process of
;;   establishing NEW-AGENT's integrity.
;;
;;   NEW-AGENT should be a DUAL-agent; if it isn't, an error is signaled.
;;   ESTABLISH-AGENT-INTEGRITY returns NEW-AGENT. That is, the result of the
;;   generic function is always EQ to its argument.
;;
;;   There is a predefined primary method for ESTABLISH-AGENT-INTEGRITY that
;;   specializes on the class DUAL-AGENT. This method works as if it were
;;   defined as follows, except that certain optimizations are permitted:
;;
;;     (defmethod establish-agent-integrity ((new-agent DUAL-agent))
;;       (establish-microframe-integrity new-agent         ; see DUAL/ARCHIT/
;;                                       :just-created)    ;       MCRFRAME.LSP
;;       (when (>= (agent-activation new-agent)
;;                 *WM-threshold*)
;;         (add-to-WM new-agent *WM*) )          ; see DUAL/ARCHIT/WORK_MEM.LSP
;;       new-agent )
;;
;;   When the initial activation of the agent is high enough, the agent is
;;   added to the working memory that is bound to the global variable *WM*.
;;   Usually, this is the primary working memory of the system (see DUAL/ARCHIT/
;;   WORK_MEM.LSP). If you want the agent to 'go' to another WM, use something
;;   like:   (let ((*WM* my-WM)) (make-DUAL-agent .....))
;;

;; REMOVE-FROM-WM :AFTER ((agent DUAL-agent))
;;
;;   An :AFTER method for the generic function REMOVE-FROM-WM (defined in
;;   DUAL/ARCHIT/WORK_MEM.LSP). It calls CLEAR-VOLATILE-MEMORY on AGENT.
;;   It also terminates all suspended processes executed by AGENT's processor.
;;
;;   According to the specification of the architecture, the contents of the
;;   volatile memory of a DUAL agent is cleared whenever its activation level
;;   drops below the WM threshold.
;;

;; SPECIAL-DUAL-AGENT
;;
;;   A symbol that is the proper name of the class of all special DUAL agents.
;;   (See CLOS specification.) As such, it is a valid type specifier.
;;   Special DUAL agents are a subclass of the class of DUAL agents.
;;   It may be used as a second argument to the function MAKE-DUAL-AGENT.
;;   It is also very useful when defining new types (i.e. classes) of special
;;   DUAL agents (see, for instance, DUAL/ARCHIT/NC_AGENT.LSP).
;;
;;   Explicit use of SPECIAL-DUAL-AGENT should be avoided. Use the appropriate
;;   data-abstraction routines instead.

;; SPECIAL-AGENT-P (thing)  -->  T or NIL
;;
;;   A predicate that checks whether something is a special DUAL agent.
;;   THING may be arbitrary LISP object.
;;   SPECIAL-AGENT-P implies DUAL-AGENT-P.
;;
;;   It is always true that, providing MAKE-xxx-AGENT evaluates without errors:
;;      (if (special-agent-p x) (agent-p x) t)                  -->  T
;;      (if (agent-p x) nil (special-agent-p x))                -->  NIL
;;      (if (special-agent-p x) (DUAL-agent-p x) t)             -->  T
;;      (if (DUAL-agent-p x) nil (special-agent-p x))           -->  NIL
;;      (special-agent-p (make-DUAL-agent 'ag1 'DUAL-agent))    -->  NIL
;;      (special-agent-p (make-DUAL-agent 'ag2 'special-agent)) -->  T
;;      (special-agent-p (make-DUAL-agent 'ag4
;;                                        'temp-DUAL-agent))    -->  NIL
;;      (special-agent-p nil)                                   -->  NIL
;;      (special-agent-p 'agent-name)                           -->  NIL
;;

;;  ***  Peculiarities of the special DUAL agents  ***
;;
;;  ...

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric initialize-agent (new-agent &rest initargs)
  (:documentation "Initialize slots according to INITARGS."))

(defgeneric establish-agent-integrity (agent)
  (:documentation "Brings undefined AGENT's slots in tune with defined ones."))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;  *****    'ORDINARY'   D U A L   AGENTS    ****
;;;
;;;  This portion of the file defines the class DUAL-agent and its associates.
;;;

;;;;;;   Class definition

(eval-when (compile load eval)
  (defclass DUAL-agent (WM-agent hybrid-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation "The common (super)class of all DUAL agents."))
) ; eval-when

;;;;  Constructor and associated functions

(declaim (inline valid-DUAL-type-p))

(defun valid-DUAL-agent-type-p (thing)
  "Predicate that returns T iff THING is a valid DUAL-agent type."
  (and (symbolp thing)
       (find-class thing nil)
       (subtypep thing 'DUAL-agent)
       T ))


(defun make-DUAL-agent (name agent-type &rest initargs 
                                        &key (prompt-p t) &allow-other-keys )
  "Makes a DUAL agent and registers it into the total pool of agents."
  (declare (values DUAL-agent))
  (if prompt-p
      (assert  (valid-DUAL-agent-type-p agent-type)
              (agent-type)         ; continuable error
              "MAKE-DUAL-AGENT: ~S is not a valid DUAL-agent type."  agent-type)
      (unless (valid-DUAL-agent-type-p agent-type)
        (error "MAKE-DUAL-AGENT: ~S is not a valid DUAL-agent type."
               agent-type)) )      ; non-continuable error
  (let ((new-agent (allocate-agent name agent-type prompt-p)))  ; see BASIC.LSP
    (apply #'initialize-agent new-agent initargs)
    (establish-agent-integrity new-agent)
    new-agent ))


(defmethod initialize-agent ((new-agent DUAL-agent)
                             &key (comment     nil comment-supplied-p)
                                  (initial-act 0.0 initial-act-supplied-p)
                                  (G-slots     nil G-slots-supplied-p)
                                  (S-slots     nil S-slots-supplied-p) 
                                  prompt-p  )
  (declare (type (or null string) comment)
           (type number initial-act)
           (type list G-slots S-slots) 
           (ignore prompt-p) )
  (when  comment-supplied-p
    (setf (agent-comment new-agent) comment))
  (when  initial-act-supplied-p
    (assert (<= *min-act* initial-act *max-act*))
    (setf (agent-activation new-agent) (coerce initial-act 'single-float)) )
  (when  G-slots-supplied-p
    (dolist (G-slot G-slots)
      (setf (slot-owner G-slot) new-agent))
    (setf (frame-G-slots new-agent) G-slots))
  (when  S-slots-supplied-p
    (dolist (S-slot S-slots)
      (setf (slot-owner S-slot) new-agent))
    (setf (frame-S-slots new-agent) S-slots))
  (dual-core::initialize-T-LINK-flag new-agent)    ; see DUAL/ARCHIT/LINKS.LSP
  new-agent )


(defmethod establish-agent-integrity ((new-agent DUAL-agent))
  (establish-microframe-integrity new-agent
                                  :just-created)  ; see DUAL/ARCHIT/HYBRID.LSP
  (when (>= (agent-activation new-agent)
            *WM-threshold*)
    (add-to-WM new-agent *WM*) )                  ; see DUAL/ARCHIT/WORK_MEM.LSP
  new-agent )


;;;;  Type predicate

(declaim (inline DUAL-agent-p))
(defun DUAL-agent-p (thing)
  (if (typep thing 'DUAL-agent) t nil))


;;;;;;  Printing method

(defmethod  agent-descriptor-string ((agent DUAL-agent))
  (if (eq (type-of agent) 'DUAL-agent)
      "a DUAL agent"
      (format nil "an agent of type ~S" (type-of agent)) ))


;;;;;;  :AFTER methods for generic functions defined in DUAL/ARCHIT/WORK_MEM.LSP

(defmethod  remove-from-WM :after ((agent DUAL-agent)
                                   &optional (reset-p T) )
  (when reset-p
    (clear-volatile-memory agent)            ; see DUAL/ARCHIT/SYMPROC.LSP
    (do  ((process (find-process agent)      ; see DUAL/ARCHIT/AGENDA.LSP
                   (find-process agent)))        ; (while (find-process ...)
         ((null process))                        ;   (remove-from-agenda ...))
      (remove-from-agenda process)) ))



;;;;;;  *****    SPECIAL DUAL AGENTS    ****
;;;
;;;  This portion of the file defines the class special-DUAL-agent and
;;;  modifies a number of methods inherited from 'ordinary' DUAL agents.
;;;

;;;;;;   Class definition

(eval-when (compile load eval)
  (defclass special-DUAL-agent (DUAL-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation "The common (super)class of all special DUAL agents."))
) ; eval-when


;;;;  Customizing the initialization procedure

(defmethod initialize-agent :after
                            ((new-agent special-DUAL-agent)
                             &key (initial-act *max-act* initial-act-supplied-p)
                                  comment G-slots S-slots prompt-p )
  (declare (ignore initial-act comment G-slots S-slots prompt-p))
  (if initial-act-supplied-p
      nil   ; activation level specified explicitly
      (setf (agent-activation new-agent)
            (* *max-act* 0.8)) )     ; Leave some margin to the ceiling.
  (setf (dual-core::agent-WM new-agent)
        :special->not-in-WM) )


(defmethod establish-agent-integrity :before ((agent special-DUAL-agent))
  (let ((TYPE-slot (locate-mfr-component agent :type)))
    (if (null TYPE-slot)
        (add-G-slot agent :type  :filler :special    ; DUAL/ARCHIT/MCRFRAME.LSP
                    :order :lifo    ; put TYPE slot at front of the G-slot list
                    :priority :old  :notify-p nil)
        (add-filler-elt TYPE-slot :special           ; DUAL/ARCHIT/FILLER.LSP
                    :priority :old  :order :fifo
                    :notify-p nil) )))   ; the primary method will notify


;;;;  Type predicate

(declaim (inline special-agent-p))
(defun special-agent-p (thing)
  (if (typep thing 'special-DUAL-agent) t nil))


;;;;;;  Printing method

(defmethod  agent-descriptor-string ((agent special-DUAL-agent))
  (if (eq (type-of agent) 'special-DUAL-agent)
      "a special DUAL agent"
      (format nil "an agent of type ~S" (type-of agent)) ))


;;;;;;  Overriding methods for 'ordinary' DUAL agents.

(defmethod  agent-efficiency  ((special-ag special-DUAL-agent))
    ;; In future versions of the program, each agent will have its local
    ;; EFFICIENCY  slot so that efficiencies will be modifiable through learning
    ;; For now, however, the efficiency coefficient is static:
  *special-agent-efficiency* )       ; see DEFS.LSP and DUAL/ARCHIT/SYMPROC1.LSP


(defmethod  update-activation  ((special-ag special-DUAL-agent))
  (error "UPDATE-ACTIVATION applied to a special DUAL agent: ~S."
         special-ag))

(defmethod  remove-from-WM  ((special-ag special-DUAL-agent)
                             &optional (reset-p T) )
  (declare (ignore reset-p))
  (error "REMOVE-FROM-WM applied to a special DUAL agent: ~S."
         special-ag))

(defmethod  clear-volatile-memory  ((special-ag special-DUAL-agent))
  (error "CLEAR-VOLATILE-MEMORY applied to a special DUAL agent: ~S."
         special-ag))

(defmethod  handle-symbol ((special-ag special-DUAL-agent)
                           (wreq write-request) )
  (error "HANDLE-SYMBOL: ~S is a special DUAL agent and, therefore, ~
          don't handle write requests: ~S."
         special-ag  wreq))


;;;;;;;  End of file DUAL/ARCHIT/DUAL_AG.LSP
