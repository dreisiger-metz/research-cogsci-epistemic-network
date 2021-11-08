;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/hybrid.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Symbolic and connectionist aspects combined
;;; DEPENDS-ON: DUAL/archit/connect.lsp, DUAL/archit/symbolic.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    19-04-97 [1.0]
;;; UPDATED:    14-12-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;;      SYMBOLIC & CONNECTIONIST  ASPECTS      ;;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is HYBRID-AGENT.
;;;; The file defines a constructor for such agents as well as some generic
;;;; functions and methods.
;;;;
;;;; A hybrid agent has two complementary aspects -- symbolic and connectionist.
;;;; Thus, hybrid agents inherit structure and functionality from symbolic
;;;; agents (see DUAL/ARCHIT/SYMBOLIC.LSP) and connectionist agents (DUAL/
;;   ARCHIT/CONNECT.LSP).
;;;; Moreover, these two aspects are integrated into a coherent whole according
;;;; to the specification of DUAL (see DR#1). In particular, connectionist
;;;; links are determined on the basis of the micro-frame of the symbolic
;;;; aspect (see the function SLOTS->LINKS).
;;;;


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: hybrid-agent, make-hybrid-agent,
;;          slots->links
;;

;; MAKE-HYBRID-AGENT (name &key :comment :initial-act
;;                              :G-slots :S-slots :prompt-p)  -->  new-agent
;;
;;   A function that constructs (and registers) a hybrid agent.
;;   NAME must be a symbol (but not  NIL).
;;   :PROMPT-P must be either T or NIL; if not supplied, it defaults to T.
;;   If NAME is already in use, MAKE-HYBRID-AGENT signals an error depending on
;;     the value of :PROMPT-P. If it is T (the default), a continuable error
;;     is signalled giving the user an opportunity to supply a new value
;;     for NAME. If :PROMPT-P is NIL, a non-continuable error is signaled.
;;   :COMMENT must be a string; if not supplied, it defaults to NIL.
;;   :INITIAL-ACT must be a number in the interval [*MIN-ACT*, *MAX-ACT*] (see
;;   DUAL/DEFS.LSP); if not supplied, it defaults to 0.0.
;;   :G- and :S-SLOTS must be lists of G- or S-slots, respectively. When
;;   not supplied, they default to the empty list.
;;   MAKE-HYBRID-AGENT returns the new hybrid agent, after applying ESTABLISH-
;;     MICROFRAME-INTEGRITY to its micro-frame.  In addition, it initializes
;;     the OWNER fields of all G-SLOTS and S-SLOTS to NEW-AGENT.
;;
;;   After a successful call to MAKE-HYBRID-AGENT, the following holds:
;;     (find-agent name)             -->  new-agent
;;     (agentp new-agent)            -->  T
;;     (agent-name new-agent)        -->  name
;;     (agent-comment new-agent)     -->  comment  ; or NIL
;;     (agent-activation new-agent)  -->  initial-act
;;     (agent-G-slots new-agent)     -->  G-slots  ; or NIL
;;     (agent-S-slots new-agent)     -->  S-slots  ; or NIL
;;     (agent-neighbors new-agent)   -->  conref-list
;;     (agent-input-zone new-agent)  -->  NIL
;;


;; SLOTS->LINKS (agent &key :priority :normalize)  -->  conref-list
;;
;;   A generic function that sets the connectionist links of AGENT on
;;   the basis of its micro-frame.
;;   Programmers may define additional methods (preferably :AFTER methods) for
;;   that generic function for customization purposes.
;;
;;   AGENT should be a hybrid agent; if it isn't, an error is signaled.
;;   PRIORITY should be a function that takes two arguments of type single-float
;;     and returns  a single-float.
;;     PRIORITY defaults to the function #'MAX (see section 3.2.4.2 in DR#1).
;;     PRIORITY has similar interpretation as in the function ADJOIN-CONREF
;;     defined in DUAL/ARCHIT/CONREF.LSP. It differs in the default value and
;;     in that :OLD and :NEW are not allowed.
;;   NORMALIZE should take one of the following values: T, NIL, or a positive
;;     single float. When not supplied, it defaults to 1.0 (see DR#1).
;;     NORMALIZE has the same interpretation as in the function ADD-NEIGHBORS
;;     defined in DUAL/ARCHIT/CONNECT.LSP; only the default value is different.
;;   SLOTS->LINKS returns a list of connectionist references (conrefs). This
;;   is the list which will be used by the functions AGENT-NEIGHBORS and
;;   ACTIVATE-NEIGHBORS (see DUAL/ARCHIT/CONNNECT.LSP).
;;
;;   The predefined primary method for SLOTS->LINKS works according to the
;;   algorithm specified in section 3.2.4.2 of "DUAL Report #1":
;;     1. (Collect) Go through all slots and facets of AGENT's micro-frame
;;          (see ARCHIT/MCRFRAME.LSP) and collect all fillers of type
;;          :REFERENCE or :LIST-OF-REFERENCES (see also DUAL/ARCHIT/SLOTS.LSP).
;;          Discard the second (i.e. the slot) part of all extended symbolic
;;          references (see SIMPLIFY-SYMREF at DUAL/ARCHIT/SYMREF.LSP). Append
;;          everything in a single list of conrefs (whose weights are 'raw
;;          weights').
;;     2. (Remove duplicates) If the list from step 1. contains two or more
;;          references to the same micro-agent, collapse them into one.
;;          The raw weight of the collapsed reference is controlled by
;;          the :PRIORITY argument (see above; #'MAX by default).
;;     3. (Normalize) Normalize the 'raw' weights depending on the :NORMALIZE
;;          argument (see above; by default, the weights are changed in such
;;          a way that the sum of their absolute values becomes 1.0).
;;     4. Store the list of connectionist references obtained at step 3. in
;;          the connectionist aspect of AGENT and return this list.
;;          In this way, all subsequent calls to ACTIVATE-NEIGHBORS will
;;          send activation to the members of the list.
;;
;;   The exact order of the final list is implementation-dependent.
;;   ACTIVATE-NEIGHBORS (see ARCHIT/CONNECT.LSP) visits neighbors in that order.
;;   This should not lead to implementation-dependent behavior at the macro-
;;   level, however, due to the synchronous algorithm employed by SPREAD.
;;
;;   Compare with the function ADD-NEIGHBORS defined in DUAL/ARCHIT/CONNECT.LSP.
;;   See also DO-ALL-REFERENCES defined in DUAL/ARCHIT/MCRFRAME.LSP.
;;
;;   It is always true that:
;;      (let ((conref-list (slots->links agent)))
;;        (eq conref-list (agent-neighbors agent)))  -->  T
;;
;; It is important to note that SLOTS->LINKS is called whenever the micro-
;; frame of the hybrid agent is changed. In particular, SLOTS->LINKS is
;; invoked at the end of MAKE-HYBRID-AGENT, as well as in :AFTER methods
;; to the generic function ESTABLISH-MICROFRAME-INTEGRITY.
;; This ensures the integrity of the symbolic and connectionist aspects.
;;


;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric  slots->links (agent &key priority normalize)
  (:documentation "Slots filled with references --> weighted links."))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass hybrid-agent (connectionist-agent symbolic-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation "Connectionist & symbolic aspects combined; a base class."))
)

;;;;  Constructor

(defun make-hybrid-agent (name &key comment
                                    (initial-act 0.0)
                                    G-slots  S-slots
                                    (prompt-p t) )
  "Makes a hybrid agent and registers it into the total pool of agents."
  (declare (values hybrid-agent)
           (type (or null string) comment)
           (type number initial-act)
           (type list G-slots S-slots) )
  (let ((new-agent (allocate-agent name 'hybrid-agent prompt-p)))
    (when  comment                          ; COMMENT supplied?
      (setf (agent-comment new-agent) comment))

    (assert (<= *min-act* initial-act *max-act*))
    (setf (agent-activation new-agent) (coerce initial-act 'single-float))

    (when  G-slots                          ; G-SLOTS supplied?
      (dolist (G-slot G-slots)
        (setf (slot-owner G-slot) new-agent))
      (setf (frame-G-slots new-agent) G-slots))
    (when  S-slots                          ; S-SLOTS supplied?
      (dolist (S-slot S-slots)
        (setf (slot-owner S-slot) new-agent))
      (setf (frame-S-slots new-agent) S-slots))
    (dual-core::initialize-T-LINK-flag new-agent)    ; see DUAL/ARCHIT/LINKS.LSP
    (establish-microframe-integrity new-agent
                                    :just-created)

    new-agent))


;;;;;;  Printing methods

(defmethod  agent-descriptor-string ((agent hybrid-agent))
  (if (eq (type-of agent) 'hybrid-agent)
      "a hybrid agent"
      (format nil "an agent of type ~S" (type-of agent)) ))


;;;;;;  Inner workings of hybrid agents
;;
;;  Hybrid agents inherit almost all their functionality from connectionist
;;  and symbolic agent. The only thing that the class HYBRID-AGENT adds on
;;  is to maintain the consistency between the two complementary aspects.

(defmethod  establish-microframe-integrity :after ((agent hybrid-agent)
                                                   justification)
  (declare (ignore justification))  ; may be analyzed in future versions
  (slots->links agent) )

(defmethod  slots->links ((agent hybrid-agent)
                          &key (priority #'max)
                               (normalize 1.0) )
  ;; See subsections 3.2.3.5 and 3.2.4.2 in DR#1
  (declare (type function priority)    ; :NEW and :OLD are not applicable
           (type (or boolean single-float) normalize) )
  (remove-neighbors agent :all)                ; see DUAL/ARCHIT/CONNECT.LSP
  (add-neighbors agent                         ; see DUAL/ARCHIT/CONNECT.LSP
                 (collect-references agent)
                 :priority priority
                 :normalize normalize
                 :order :lifo         ; straighten up after COLLECT-REFERENCES
                 :destructive-p T) )  ; safe because COLLECT-REF'S takes care

(defmethod  slots->links ((x t) &key priority normalize)
  (declare (ignore priority normalize))
  (if x
    (error "SLOTS->LINKS:  ~S is not a hybrid agent." x)
    (error "SLOTS->LINKS applied to NIL (perhaps #$missing-agent).") ))


(defun collect-references (agent &aux result)
  "Traverse all slots and facets, collecting all fillers of type reference."
  (declare (values list))  ; of fresh conrefs
  (do-all-references (conref agent result)
    (push (make-conref     ; Use a fresh conref to allow destructive oper.
                (simplify-symref (conref-reference conref))
                (conref-weight conref))
          result)) )  ; reverse order, to be straightened up by SLOTS->LINKS



;;;;;;  Methods for generic functions defined in DUAL/ARCHIT/SYMPROC2.LSP
;;;;
;;;; The methods for RECEIVE-SYMBOL defined in DUAL/ARCHIT/SYMPROC2.LSP assume
;;;; that the receiver is always visible. This assumption is revised here.
;;;;

(defmethod receive-symbol :around ((receiver hybrid-agent)
                                   (symbol symbolic-structure))
  ;; Invoke the primary method from DUAL/ARCHIT/SYMPROC2.LSP only when visible.
  (if (agent-visible-p receiver)
      (call-next-method)  ; do it
      symbol) )           ; skip, only return the advertised value


;;;;;;;  End of file DUAL/ARCHIT/HYBRID.LSP
