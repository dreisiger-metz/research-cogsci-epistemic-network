;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/promprop.lsp
;;; VERSION:    2.2.1  ; see AMBR/VERSION.LSP
;;; PURPOSE:    Propagating instance-promotions to concept-promotions.
;;; DEPENDS-ON: DUAL; ambr/secretar.lsp, ambr/marker.lsp, ambr/promotn.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    09-04-98 [2.2.1]
;;; UPDATED:    ...
;;;;;;;;
;;; TO DO:      Implement the promotion propagation mechanism.  It is only
;;;             sketched here.


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;     P R O M O T I O N   P R O P A G A T I O N     ;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; >>>>  Note:  The promotion propagation mechanism is not implemented   <<<<
;; >>>>         yet.  This file only outlines the general idea and does  <<<<
;; >>>>         the routine groundwork defining its symbolic structures. <<<<


(in-package "AMBR-CORE")

;;;; This file is a sequel to AMBR/PROMOTN.LSP.  It propagates promotions
;;;; from instance-agents to concept-agents.
;;;;
;;;; Promotion propagation is a form of constraint propagation.  (This is a
;;;; symbolic counterpart of the connectionist constraint satisfaction network.)
;;;; The main idea may be illustrated as follows:
;;;; Suppose that the following hypotheses about instance-agents have been
;;;; established for some reason:
;;;;   water-1<-->milk-2, water-1<-->milk-4, water-1<-->wine-2
;;;; The bottom-up structure correspondence mechanism (see AMBR/STR_CORR.LSP)
;;;; will then construct the following hypotheses about concept-agents:
;;;;   water<==>milk, water<==>wine
;;;;
;;;; Now suppose that the promotion mechanism (AMBR/RATING and =/PROMOTN.LSP)
;;;; promotes  WATER-1<-->MILK-4  as winner.  This means that in a sense the
;;;; model makes a commitment to map the instance WATER-1 to MILK-4.  To be
;;;; consistent, the model should then take the next step and map the concept
;;;; WATER to MILK.  The purpose of the PROMOTION PROPAGATION mechanism is
;;;; exactly this -- to propagate commitments (or 'promotions') made on the
;;;; level of instance-agents to the level of concept-agents.
;;;;
;;;; At the level of instance-agents, promotions are initiated by the rating
;;;; mechanism (see AMBR/RATING.LSP) -- the authorized secretaries send a
;;;; promotion incentive to one of the hypotheses on their record.  Secretary
;;;; authorization, however, depends on the affiliation of the agent.  Concept
;;;; agents are never affiliated to any situation and hence their secretaries
;;;; are never authorized to engage in the rating mechanism.  As a consequence,
;;;; the rating mechanism can never trigger any promotion at the level of
;;;; concept-agents.  This is the job of the promotion propagation mechanism
;;;; implemented in this file.
;;;;
;;;;
;;;; The work of the promotion propagation mechanism is outlined as follows:
;;;;   1. When a hypothesis is promoted, it sends METAMORPHOSIS-NOTIFICATION
;;;;       messages to the secretaries of the two correspondence elements.
;;;;       (See AMBR/PROMOTN.LSP for details on metamorphosis notifications.)
;;;;   2. When the secretary of an instance agent receives a metamorphosis
;;;;       notification, it extracts the relevant information from it and
;;;;       sends a PROMOTION-PROPAGATION message (or PROMPROP for short)
;;;;       to the respective concept agent.
;;;;      >>>>>  The promotion propagation is not implemented yet and   <<<<<
;;;;      >>>>>  hence the secretaries currently send no such messages. <<<<<
;;;;   3. When the secretary of a concept agent receives a 'promprop' message,
;;;;       it does a series of checks to decide whether it is warranted to
;;;;       make a commitment.  If it is, the secretary sends a promotion
;;;;       incentive message to the appropriate concept-level hypothesis.
;;;;
;;;; The checks mentioned at step 3. above are necessary because not every
;;;; instance-level promotion (and respectively promprop message) leads to
;;;; concept-level promotion.  The problem is that there may be several
;;;; instances of the same concept.  For example, suppose that the driver
;;;; situation involves two instances of WATER -- WATER-1 and WATER-2.
;;;; If WATER-1 maps to, say, MILK-1 and WATER-2 maps to MILK-33 then there
;;;; is no problem -- at the concept level, WATER maps to MILK.  If, however,
;;;; WATER-1 maps to MILK-1 but WATER-2 maps to TEA-7 then the situation is
;;;; ambiguous -- the concept WATER have equally strong justifications to
;;;; map either to MILK or to TEA.  In such ambiguous cases the promotion
;;;; propagation mechanism acts conservatively -- it does not promote any
;;;; of the two conflicting correspondences.  That is, neither WATER<==>MILK
;;;; nor WATER<==>TEA will be promoted.  (One of them might be promoted at a
;;;; later point by some other mechanism.)
;;;;
;;;; Note that promotion-propagation messages are received at the concept
;;;; agent asynchronously.  The early messages of this kind must wait until
;;;; the whole set of instances of the respective concept have made their
;;;; commitments and notified the secretary of the concept-agent.  In the
;;;; example above, the promprop message from WATER-1<==>MILK-1 has to wait
;;;; until the other instance -- WATER-2 -- also sends a promprop.  If the
;;;; second promprop is consistent with the first (i.e. WATER-2<==>MILK-33)
;;;; then the concept secretary will promote WATER<==>MILK.  If, however,
;;;; the second promprop is inconsistent with the first (i.e. WATER-2<==>TEA-7)
;;;; then no promotion occurs.
;;;; To generalize, the 'early' promprop messages wait until the 'last' such
;;;; message appears.  When all messages have arrived, the secretary checks
;;;; them for consistency.  Stated differently, only the 'last' promprop
;;;; may in fact lead to promotion of a hypothesis at the concept level.
;;;;
;;;; The last question to be clarified is "How does the secretary of the concept
;;;; agent 'know' how many authorized instances of this concepts are being
;;;; mapped in order to determine which promprop message is the last one?"
;;;; The concept agent is not guaranteed to have top-down :INSTANCE links to
;;;; all relevant instance agents but is guaranteed to have received markers
;;;; from all of them.  Indeed, each instance agent issues a marker when
;;;; entering the WM (see AMBR/MARKER.LSP).  The concept agent lies directly
;;;; on the path of the marker and, therefore, the marker will be received
;;;; and stored in the buffer.  Each marker carries a 'driver situation' which
;;;; may be checked via x-DRIVER-P (defined in AMBR/GOALINPT.LSP, see also
;;;; AUTHORIZED-P in AMBR/RATING.LSP).  Thus the set of markers stored in
;;;; the concept contain enough information about the set of authorized
;;;; instance agents.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: promotion-propagation, send-promotion-propagation,
;;          promprop-driver-sit, promprop-driver-sit,
;;          promprop-instance-hypoth, promprop-concept-hypoth ;
;;
;;          ...
;;

;; ...

;;;;;; Generic function(s) pertaining to the external protocol

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;  *******  PROMOTION-PROPAGATION  MESSAGES  *******
;;;
;;;  This portion of the file defines several classes of symbolic structures
;;;  (see DUAL/ARCHIT/SYMPROC1.LSP) exchanged between the agents as part of
;;;  the promotion propagation protocol.  Somewhat confusingly, these structures
;;;  are also called PROMOTION-PROPAGATIONs (or PROMPROPs for short).

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass  promotion-propagation  (promotion-symbolic-structure)
    ((driver-sit        :reader     promprop-driver-sit
                        :type       instance-agent       ; situation
                        :initarg    :driver-sit
                        :initform   (required-argument) )
     (driver-instance   :reader     promprop-driver-instance
                        :type       instance-agent
                        :initarg    :driver-instance
                        :initform   (required-argument) )
     (instance-hypoth   :reader     promprop-instance-hypoth
                        :type       winner-hypoth-agent
                        :initarg    :instance-hypoth
                        :initform   (required-argument) )
     (concept-hypoth    :accessor   promprop-concept-hypoth
                        :type       (or (member :not-computed NIL)
                                        hypoth-agent)
                        :initform   :not-computed   )
    )
   (:documentation
     "Symbolic structure used by the promotion propagation mechanism." ))
) ; eval-when


;;;;  Constructor

(defun send-promotion-propagation (receiver driver-sit driver-inst
                                                       instance-hypoth)
  "DRIVER-INST sends a promprop message to RECEIVER concept agent."
  (declare (type concept-agent receiver)
           (type instance-agent driver-sit driver-inst)
           (type hypoth-agent instance-hypoth)
           (values promotion-propagation) )
  (receive-symbol receiver (make-instance 'promotion-propagation
                                          :driver-sit      driver-sit
                                          :driver-inst     driver-inst
                                          :instance-hypoth instance-hypoth )))


;;;;;;  Printing methods

(defmethod  print-object ((promprop promotion-propagation) stream)
  (if (and (slot-boundp promprop 'driver-instance)
           (agentp (promprop-driver-instance promprop)) )
      (format stream "#<PP ~A>"
                     (agent-name (promprop-driver-instance promprop)))
      (format stream "#<malformed promotion-propagation>") ))


(defmethod DUAL-describe ((promprop promotion-propagation)
                          &optional (stream *standard-output*) )
  (format stream "~&~S is a ~A.~%"
          promprop (type-of promprop) )
  (format stream "~&  Its driver situation is: ~S~%"
          (if (slot-boundp promprop 'driver-sit)
              (promprop-driver-sit promprop)
              "(unbound)"))
  (format stream "~&  Its driver instance is: ~S~%"
          (if (slot-boundp promprop 'driver-instance)
              (promprop-driver-instance promprop)
              "(unbound)"))
  (format stream "~&  Its instance hypothesis is: ~S~%"
          (if (slot-boundp promprop 'instance-hypoth)
              (promprop-instance-hypoth promprop)
              "(unbound)"))
  (format stream "~&  Its concept hypothesis is: ~S~%"
          (if (slot-boundp promprop 'concept-hypoth)
              (promprop-concept-hypoth promprop)
              "(unbound)"))
  (values))


;;;;  Type-checking methods for the accessors

(defmethod  promprop-driver-sit ((x t))
  (error "PROMPROP-DRIVER-SIT: ~S is not a promotion propagation message." x ))

(defmethod  promprop-driver-instance ((x t))
  (error "PROMPROP-DRIVER-INSTANCE: ~S is not a promotion propagation message."
         x ))

(defmethod  promprop-instance-hypoth ((x t))
  (error "PROMPROP-INSTANCE-HYPOTH: ~S is not a promotion propagation message."
         x ))

(defmethod  promprop-concept-hypoth ((x t))
  (error "PROMPROP-CONCEPT-HYPOTH: ~S is not a promotion propagation message."
         x ))

(defmethod  (setf promprop-concept-hypoth) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF PROMPROP-CONCEPT-HYPOTH): ~S is not a promotion propagation."
         x ))


;;;;;;;  End of file AMBR/PROMPROP.LSP
