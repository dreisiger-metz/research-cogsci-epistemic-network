;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/ambr_ag.lsp
;;; VERSION:    2.2.2  ; see AMBR/VERSION.LSP
;;; PURPOSE:    Various classes of AMBR agents
;;; DEPENDS-ON: DUAL; ambr/defs.lsp, ambr/proclaim.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    08-05-97
;;; UPDATED:    08-03-98 [2.2.1]
;;; UPDATED:    07-05-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...
;;;;;;
;;; TO_DO:      Update and finish the documentation


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;     VARIOUS  CLASSES  OF   A M B R   AGENTS     ;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR-CORE")

;;;; The key concept defined in this file is AMBR-AGENT and its various
;;;; subclasses.  The class AMBR-AGENT is built on DUAL-AGENT (see DUAL/ARCHIT/
;;;; DUAL_AG.LSP) and in turn is a building block for all agents used in the
;;;; AMBR models.
;;;; ...


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: AMBR-agent, AMBR-agent-p, agent-buffer,
;;          temp-AMBR-agent, dead-AMBR-agent, special-AMBR-agent,
;;          NCR-sending-AMBR-agent, MP-AMBR-agent,
;;          secretary-agent, concept-agent,
;;          instance-agent, temp-instance-agent, dead-instance-agent,
;;          corr-agent, temp-corr-agent, dead-corr-agent,
;;          hypoth-agent, dead-hypoth-agent,
;;          embryo-hypoth-agent, mature-hypoth-agent, winner-agent,
;;          ensure-TYPE-slot

;; AMBR-AGENT
;;
;;   A symbol that is the proper name of the class of agents used in AMBR.
;;   The class AMBR-AGENT is a subclass of DUAL-agent (see DUAL/ARCHIT/DUAL_AG).
;;
;;   The symbol AMBR-AGENT, being a proper name of a class, is a valid type
;;   specifier. It also is a valid DUAL-object type, that is, it may be used
;;   as a second argument to the function MAKE-DUAL-AGENT (defined in
;;   DUAL/ARCHIT/DUAL_AG.LSP).
;;   It is also used as a basis for defining new classes. For instance, the
;;   classes CONCEPT-AGENT and INSTANCE-AGENT are subclasses of AMBR-AGENT.
;;
;;   Explicit use of AMBR-AGENT should be avoided. Use the appropriate data-
;;   abstraction routines instead. For instance:
;;     (AMBR-agent-p x)    is to be preferred to   (typep x 'AMBR-agent)
;;
;;   It is always true that:
;;      (subtypep 'AMBR-agent 'DUAL-agent)        -->  T
;;      (valid-dual-object-type-p 'AMBR-agent)    -->  T
;;

;; AMBR-AGENT-P (thing)  -->  T or NIL
;;
;;   A predicate that checks whether something is a AMBR agent.
;;   THING may be arbitrary LISP object.
;;   AMBR-AGENT-P implies DUAL-AGENT-P (see DUAL/ARCHIT/DUAL_AG.LSP).
;;   (In turn, DUAL-AGENT-P implies AGENT-P (see DUAL/ARCHIT/BASIC.LSP).)
;;
;;   It is always true that, providing MAKE-DUAL-AGENT evaluates without errors:
;;      (if (AMBR-agent-p x) (DUAL-agent-p x) t)               -->  T
;;      (if (AMBR-agent-p x) (agentp x) t)                     -->  T
;;      (AMBR-agent-p (make-DUAL-agent 'ag1 'DUAL-agent))      -->  NIL
;;      (AMBR-agent-p (make-DUAL-agent 'ag1 'AMBR-agent))      -->  T
;;      (AMBR-agent-p (make-DUAL-agent 'ag1 'instance-agent))  -->  T
;;      (AMBR-agent-p (make-DUAL-agent 'ag1 'concept-agent))   -->  T
;;      (AMBR-agent-p nil)                                     -->  NIL
;;      (AMBR-agent-p 'agent-name)                             -->  NIL
;;

;; ...

;;;;;; Generic function(s) pertaining to the external protocol


;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

;;;;;;   *******  Class definitions  ********

;; Base classes of AMBR agents
(eval-when (compile load eval)
  (defclass AMBR-agent (DUAL-agent)
    ((buffer       :accessor   agent-buffer   ; see DUAL/ARCHIT/SYMPROC1.LSP
                   :type       list
                   :initform   nil  )
    )
    (:documentation
      "Agent used in the AMBR model. A base class built on DUAL-AGENT." ))

  (defclass special-AMBR-agent (AMBR-agent special-DUAL-agent)
    ()  ; no slots defined here, all slots are inherited
  )

  (defclass NCR-sending-AMBR-agent (AMBR-agent NCR-sending-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation
      "AMBR agent capable of sending node-construction requests." ))

  (defclass temp-AMBR-agent (AMBR-agent temp-DUAL-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation
      "AMBR agent that 'dies' whenever its activation level drops to zero." ))

  (defclass dead-AMBR-agent (temp-AMBR-agent dead-temp-agent)
    ()  ; No specific slots, all slots are inherited.
    (:documentation "The 'ghost' of a dead temp-AMBR-agent. Mix-in." ))
) ; eval-when


;; Entity agents and associates
(eval-when (compile load eval)
  (defclass secretary-agent (AMBR-agent)
    ()  ; no slots defined here, all slots are inherited
    ;; See AMBR/SECRETAR.LSP
    (:documentation "AMBR agent that keeps track of hypotheses. Mix-in class."))

  (defclass MP-AMBR-agent (NCR-sending-AMBR-agent MP-DUAL-agent)
    ()  ; no slots defined here, all slots are inherited
    ;; See AMBR/MARKER.LSP and AMBR/MARKPASS.LSP
    (:documentation "AMBR agent with marker-passing capabilities." ))

  ;; Earlier versions of AMBR used the class ENTITY-AGENT. Its superclasses
  ;; were MP-AMBR-AGENT and SECRETARY-AGENT. Its subclasses were CONCEPT-AGENT
  ;; and INSTANCE-AGENT. This class was abandoned, however, because instance
  ;; agents need not inherit from MP-AMBR agents. Instances only _emit_ markers.
  ;; If you need a common superclass of instance- and concept-agents, use
  ;; SECRETARY-AGENT.

  (defclass concept-agent (MP-AMBR-agent secretary-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation  "AMBR agent of :type :concept. (Cf. INSTANCE-AGENT)" ))

  (defclass instance-agent (secretary-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation  "AMBR agent of :type :instance. (Cf. CONCEPT-AGENT)" ))

  (defclass temp-instance-agent (instance-agent temp-AMBR-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation "Temporary instance agent." ))

  (defclass dead-instance-agent (temp-instance-agent dead-AMBR-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation "The 'ghost' of a dead instance-AMBR-agent." ))
) ; eval-when

;; See AMBR/SITUATN.TXT for documentation on the so-called 'situation agents'.

;; Correspondence agents -- see AMBR/CORRESP.LSP
(eval-when (compile load eval)
  (defclass corr-agent (AMBR-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation
        "AMBR agent representing a correspondence between two entities." ))

  (defclass temp-corr-agent (corr-agent temp-AMBR-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation "Temporary correspondence agent." ))

  (defclass dead-corr-agent (temp-corr-agent dead-AMBR-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation "The 'ghost' of a dead temp-corr-agent." ))
) ; eval-when


;; Hypothesis agents -- see AMBR/HYPOTH.LSP
(eval-when (compile load eval)
  (defclass hypoth-agent (temp-corr-agent)
    ((inhib-input  :accessor    inhib-input-zone
                   ;; Second input zone. See DUAL/ARCHIT/CONNECT.LSP
                  :type        single-float
                  :initform    0.0  )
    )
    (:documentation  "Node in a constraint satisfaction net. Base class." ))

  (defclass dead-hypoth-agent (hypoth-agent dead-corr-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation "The 'ghost' of a dead hypothesis agent." ))

  (defclass embryo-hypoth-agent (hypoth-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation "Hypothesis agent at the first stage of its life." ))

  (defclass mature-hypoth-agent (NCR-sending-AMBR-agent hypoth-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation "Hypothesis agent at the second stage of its life." ))

  (defclass winner-hypoth-agent (hypoth-agent)
    ()  ; no slots defined here, all slots are inherited
    ;; See AMBR/PROMOTN.LSP
    (:documentation "Hypothesis agent emerged as constraint-satisf. winner." ))
) ; eval-when


;;;;  *******  Type predicates  *******

(declaim (inline AMBR-agent-p))
(defun AMBR-agent-p (thing)
  (and (typep thing 'AMBR-agent) T) )

;; SPECIAL-AGENT-P is defined in DUAL/ARCHIT/DUAL_AG.LSP
;; TEMP-AGENT-P is defined in DUAL/ARCHIT/TEMP_AG.LSP
;; See also the predicate AGENT-TYPE in DUAL/KREPRES.LSP.


;;;;;;  *******  Printing methods  *******

(defmethod  agent-descriptor-string ((agent AMBR-agent))
  (if (eq (type-of agent) 'AMBR-agent)
      "an AMBR agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod  agent-descriptor-string ((agent special-AMBR-agent))
  (if (eq (type-of agent) 'special-AMBR-agent)
      "a special AMBR agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod  agent-descriptor-string ((agent temp-AMBR-agent))
  (if (eq (type-of agent) 'temp-AMBR-agent)
      "a temporary AMBR agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod  agent-descriptor-string ((agent dead-AMBR-agent))
  (if (eq (type-of agent) 'dead-AMBR-agent)
      "a dead AMBR agent"
      (format nil "an agent of type ~S" (type-of agent)) ))


;; No special method for SECRETARY-AGENT because it is a mix-in class.
;; No special method for NCR-SENDING-AMBR-AGENT because it is a mix-in class.
;; No special method for MP-AMBR-AGENT because it is a mix-in class.

(defmethod  agent-descriptor-string ((agent concept-agent))
  (if (eq (type-of agent) 'concept-agent)
      "an AMBR agent of :type :concept"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod  agent-descriptor-string ((agent instance-agent))
  (if (eq (type-of agent) 'instance-agent)
      "an AMBR agent of :type :instance"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod  agent-descriptor-string ((agent temp-instance-agent))
  (if (eq (type-of agent) 'temp-instance-agent)
      "a temporary AMBR agent of :type :instance"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod  agent-descriptor-string ((agent dead-instance-agent))
  (if (eq (type-of agent) 'dead-instance-agent)
      "a dead AMBR agent of :type :instance"
      (format nil "an agent of type ~S" (type-of agent)) ))


(defmethod  agent-descriptor-string ((agent corr-agent))
  (if (eq (type-of agent) 'corr-agent)
      "a correspondence agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod  agent-descriptor-string ((agent temp-corr-agent))
  (if (eq (type-of agent) 'temp-corr-agent)
      "a temporary correspondence agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod  agent-descriptor-string ((agent dead-corr-agent))
  (if (eq (type-of agent) 'dead-corr-agent)
      "a dead correspondence agent"
      (format nil "an agent of type ~S" (type-of agent)) ))


;; No special method for HYPOTH-AGENT because this is a base class.

(defmethod  agent-descriptor-string ((agent embryo-hypoth-agent))
  (if (eq (type-of agent) 'embryo-hypoth-agent)
      "an embryo of a hypothesis agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod  agent-descriptor-string ((agent mature-hypoth-agent))
  (if (eq (type-of agent) 'mature-hypoth-agent)
      "a mature hypothesis agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod  agent-descriptor-string ((agent winner-hypoth-agent))
  (if (eq (type-of agent) 'winner-hypoth-agent)
      "a winner hypothesis agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod  agent-descriptor-string ((agent dead-hypoth-agent))
  (if (eq (type-of agent) 'dead-hypoth-agent)
      "a dead hypothesis agent"
      (format nil "an agent of type ~S" (type-of agent)) ))



;;;;;;  *******  Support for AGENT-BUFFER  *******
;;
;; See DUAL/ARCHIT/SYMPROC1.LSP for documentation on local memory and buffers.
;;

(defmethod DUAL-describe :after ((agent AMBR-agent)
                                 &optional (stream *standard-output*) )
  (format stream "~&  Its buffer contains: ~S~%"
          (if (slot-boundp agent 'buffer)
              (agent-buffer agent)
              "(unbound slot)") )
  (values))

(defmethod  agent-buffer ((x t))
  (error "AGENT-BUFFER:  ~S is not an AMBR agent." x ))

(defmethod  (setf agent-buffer) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF AGENT-BUFFER):  ~S is not an AMBR agent." x ))


;; The buffer is volatile memory -- its contents is lost when the agent drops
;; out of the working memory.  See DUAL/ARCHIT/SYMPROC1.LSP for details.
;;
(defmethod  clear-volatile-memory :after ((agent AMBR-agent))
  (setf (agent-buffer agent) nil) )



;;;;;;  *******  Lethal transformations  *******
;;
;; See DUAL/ARCHIT/TEMP_AG.LSP for documentation on TEMP-AGENT-DYING-CLASS.
;; See AMBR/FIZZLE.LSP for voluntary destruction of temporary AMBR agents.

(defmethod temp-agent-dying-class ((agent temp-AMBR-agent))
  (declare (ignore agent))
  'dead-AMBR-agent )

(defmethod temp-agent-dying-class ((agent temp-instance-agent))
  (declare (ignore agent))
  'dead-instance-agent )

(defmethod temp-agent-dying-class ((agent temp-corr-agent))
  (declare (ignore agent))
  'dead-corr-agent )

(defmethod temp-agent-dying-class ((agent hypoth-agent))
  (declare (ignore agent))
  'dead-hypoth-agent )



;;;;;;  *******  Initializing the :TYPE slot  *******
;;
;; See DUAL/ARCHIT/DUAL_AG.LSP for documentation on ESTABLISH-AGENT-INTEGRITY.


(defun ensure-TYPE-slot (agent valid-tag &key (invalid-tags '())
                                              (signal-error-p t) (correct-p t)
                                              (notify-p T) )
  "Ensure that AGENT's TYPE slot is filled by appropriate tags."
  (let* ((TYPE-slot (add-G-slot agent :type
                                :priority :old      ; keep old slot, if any
                                :order :lifo        ; push to front, if new
                                :notify-p nil))     ; will be notified later
         (TYPE-filler (fholder-filler TYPE-slot))
         (collision (intersection invalid-tags TYPE-filler)) )
    (when collision
      (when signal-error-p
        (cerror
            "~*~*~*~:[Ignore the problem ~;Remove the bad tags~] and continue."
            "The tag(s) ~A cannot appear in the TYPE slot of ~A (~S)."
            collision (agent-descriptor-string agent) agent
            correct-p ))
      (when correct-p
        (setq TYPE-filler (set-difference TYPE-filler collision))) )
    (pushnew valid-tag TYPE-filler)
    (set-filler TYPE-slot TYPE-filler notify-p) ))


(defmethod establish-agent-integrity :before ((agent concept-agent))
  (ensure-TYPE-slot agent :concept
                          :invalid-tags '(:instance :temporary
                                          :corr :hypoth :embryo :mature :winner)
                          :notify-p nil ))    ; primary method will notify

(defmethod establish-agent-integrity :after ((agent instance-agent))
  (ensure-TYPE-slot agent :instance
                          :invalid-tags '(:concept 
                                          :corr :hypoth :embryo :mature :winner)
                          :notify-p nil ))    ; primary method will notify

(defmethod establish-agent-integrity :after ((agent corr-agent))
  (ensure-TYPE-slot agent :corr
                          :invalid-tags '(:instance :concept :situation)
                          :notify-p nil ))    ; primary method will notify

(defmethod establish-agent-integrity :after ((agent hypoth-agent))
  (ensure-TYPE-slot agent :hypoth
                          :invalid-tags '(:instance :concept :situation)
                          :notify-p nil ))    ; primary method will notify

(defmethod establish-agent-integrity :after ((agent embryo-hypoth-agent))
  (ensure-TYPE-slot agent :embryo
                          :invalid-tags '(:mature :winner)
                          :notify-p nil ))    ; primary method will notify

(defmethod establish-agent-integrity :after ((agent mature-hypoth-agent))
  (ensure-TYPE-slot agent :mature
                          :invalid-tags '(:embryo :winner)
                          :notify-p nil ))    ; primary method will notify

(defmethod establish-agent-integrity :after ((agent winner-hypoth-agent))
  (ensure-TYPE-slot agent :winner
                          :invalid-tags '(:embryo :mature)
                          :notify-p nil ))    ; primary method will notify

;; The :TEMPORARY tag is set by methods from DUAL/ARCHIT/TEMP_AG.LSP.
;; The :SPECIAL tag is set by methods from DUAL/ARCHIT/DUAL_AG.LSP.


;;;;;;;  End of file AMBR/AMBR_AG.LSP
