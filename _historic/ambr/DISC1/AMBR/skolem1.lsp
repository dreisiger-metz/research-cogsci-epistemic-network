;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/skolem1.lsp    ; see AMBR/skolem.txt and AMBR/skolem2.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Skolemization mechanism
;;; DEPENDS-ON: DUAL; ambr/krepres.lsp, ambr/corresp.lsp, ambr/hypoth.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    29-04-98 [2.2.2]
;;; UPDATED:    25-05-98 -- Split in two parts, see AMBR/SKOLEM2.LSP.
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...


         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;    S K O L E M I Z A T I O N   (part 1)   ;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR-CORE")

;;;; The key concept defined in this file is SKOLEMIZATION -- a technique
;;;; for constructing 'specific propositions' on the basis of 'general
;;;; propositions'.  This file is part 1 of two parts, see AMBR/SKOLEM2.LSP.
;;;;
;;;; Part 1 is the 'entry point' to the skolemization mechanism. In particular,
;;;;  it defines the function SEND-SKOLEM-INCENTIVE which is the trigger.
;;;;  It also defines some predicates (e.g. NEED-SKOLEMIZATION-P) used by other
;;;;  mechanisms to judge whether to send Skolem incentives or not.  Finally,
;;;;  part 1 defines the data structures (e.g. 'Skolem table') for part 2.
;;;;
;;;; Part 2 implements the main skolemization algorithms.
;;;;
;;;; See AMBR/SKOLEM.TXT for detailed introduction to Skolemization.
;;;;
;;;; The skolemization mechanism may be blocked by setting *SKOLEMIZATION-FLAG*
;;;; to NIL.  The default value for this flag is T (see AMBR/DEFS.LSP).


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: need-skolemization-p, general-corresp-p, prototype-corresp-p,
;;          remote-general-hypothesis-p, has-relevant-markers-p ;
;;
;;          Skolem-symbolic-structure,
;;          Skolem-incentive, Skolem-incentive-secretary, send-Skolem-incentive,
;;          Skolem-message, Skolem-message-1, Skolem-message-2,
;;          make-Skolem-message-1, make-Skolem-message-2,
;;          Skolem-message-sender, Skolem-message-situation,
;;          Skolem-message-concept ;
;;
;;          Skolem-table, make-Skolem-table, find-Skolem-table, Skolem-table-p,
;;          Skolem-table-hyp, Skolem-table-prop, Skolem-table-sit,
;;          Skolem-table-status, Skolem-table-slots,
;;          Skolem-table-old-props, Skolem-table-new-props ;
;;
;;          make-Skolem-slot-descriptor, make-Skolem-prop-descriptor,
;;          SSD-label, SSD-concept, SSD-prototype, SSD-instances,
;;          SPD-target, SPD-slots

;; ...

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;;  *****  P R E D I C A T E S  *****
;;;
;;;  This section defines some predicates used by AMBR/SKOLEM2.LSP or by other
;;;  mechanisms.  In particular, REMOTE-GENERAL-HYPOTHESIS-P is used by the
;;;  structure corr. mechanism (see AMBR/STR_CORR.LSP) and NEED-SKOLEMIZATION-P
;;;  by the rating mechanism (see AMBR/RATING.LSP).
;;;
;;;  See AMBR/SKOLEM.TXT for introduction to the terminology.
;;;  See AMBR/KREPRES.LSP for GENERAL-PROPOSITION-P and RANGE-SLOT-P.
;;;  See AMBR/CORRESP.LSP for CORRESP-ELT, *xxx-ELT-LABEL*, etc.


;; GENERAL-CORRESP-P takes a correspondence (usually hypothesis) agent and
;; returns a list of the labels of the elts satisfying GENERAL-PROPOSITION-P.
;;
;; The list returned by GENERAL-CORRESP-P may contain zero, one, or two labels.
;; When it has one element, CORRESP is a heterogeneous hypothesis (SKOLEM.TXT);
;; when it has two elements, CORRESP is a homogeneous hypothesis; when the list
;; is empty, CORRESP is not a general hypothesis at all.
;;
;; Example:
;;  (general-corresp-p #$rose-17-is-red<-->each-snowdrop-is-white)  -->
;;                                              -->  (*recipient-elt-label*)

(defun  general-corresp-p (corresp)
  "Does CORRESP involve propositions satisfying GENERAL-PROPOSITION-P ?"
  (declare (type corr-agent corresp)
           (values list) )
  (let ((driver-elt    (corresp-elt corresp *driver-elt-label*))
        (recipient-elt (corresp-elt corresp *recipient-elt-label*))
        (result nil) )
    (unless (or (null driver-elt) (null recipient-elt))  ; died in the interim
      (when (general-proposition-p driver-elt)
        (push *driver-elt-label* result))
      (when (general-proposition-p recipient-elt)
        (push *recipient-elt-label* result))
      result )))


;; PROTOTYPE-CORRESP-P is analogous to GENERAL-CORRESP-P but checks for
;; prototypical instances instead of general propositions.
;;
;; Example:
;;  (prototype-corresp-p #$red-17<-->prototypical-red)  -->
;;                                              -->  (*recipient-elt-label*)

(defun  prototype-corresp-p (corresp)
  "Does CORRESP involve prototypical instances?"
  (declare (type corr-agent corresp)
           (values list) )
  (let ((driver-elt    (corresp-elt corresp *driver-elt-label*))
        (recipient-elt (corresp-elt corresp *recipient-elt-label*))
        (result nil) )
    (unless (or (null driver-elt) (null recipient-elt))  ; died in the interim
      (when (agent-type driver-elt '(:PROTOTYPE :instance))
        (push *driver-elt-label* result))
      (when (agent-type recipient-elt '(:PROTOTYPE :instance))
        (push *recipient-elt-label* result))
      result )))


;; REMOTE-GENERAL-HYPOTHESIS-P is a predicate called by the structure-corresp.
;; mechanism (AMBR/STR_CORR.LSP).  It is used to screen out general hypotheses
;; that are too 'remote'.  A general hypothesis is too remote if:
;;  1. it is a heterogeneous general hypothesis (see AMBR/SKOLEM.TXT)  and
;;  2. the general proposition involved in it is not supported by markers.
;;
;; A general proposition is supported by markers when at least one of its
;; arguments is supported by markers.
;; A concept-argument (see AMBR/SKOLEM.TXT) is supported by markers if it has
;; received at least one marker (see AGENT-MARKERS in DUAL/ARCHIT/MP_AGENT.LSP).
;; A prototypical instance-argument is supported by markers if its parent
;; concept has at least one marker in its local memory.
;; (Compare with HAS-RELEVANT-MARKERS-P below.)

(defun  remote-general-hypothesis-p (hypoth)
 "Is this a heterogeneous general hypothesis which is not supported by markers?"
  (declare (type corr-agent hypoth)
           (values boolean) )
  #+:DUAL-DEBUG (assert (corresp-type hypoth '(:instance :relation)))
  (let ((label-list (general-corresp-p hypoth)))
   (ecase (length label-list)
     (0  nil)   ; specific hypothesis
     (2  nil)   ; homogeneous general hypothesis
     (1         ; heterogeneous general hypothesis
         (notany #'(lambda (S-slot)      ; not supported by markers ?
                     (agent-markers (argument->concept S-slot)))
                 (agent-S-slots (corresp-elt hypoth (first label-list))))) )))


;; NEED-SKOLEMIZATION-P is a predicate used by the rating mechanism to
;; determine whether to issue Skolem incentives or not -- see the function
;; CHECK-FOR-SKOLEM-RATING in AMBR/RATING.LSP.
;; Given that they are these incentives that trigger the whole skolemization
;; mechanism (see AMBR/SKOLEM.TXT), NEED-SKOLEMIZATION-P effectively controls
;; which general-hypotheses get skolemized.  (See also HAS-RELEVANT-MARKERS-P.)
;;
;; There are two kinds of hypotheses that need skolemization:
;;   1. heterogeneous general hypotheses involving propositions and
;;   2. heterogeneous hypotheses involving prototype instances.
;;       (Prototypical instances in turn serve as arguments to general props.)
;;
;; Note that instance-to-concept mappings do not need skolemization according
;; to these criteria.  More 'skolemizable' kinds may be added in the future.

(defun  need-skolemization-p  (hypoth)
  "Does HYPOTH need skolemization?"
  (declare (type corr-agent hypoth)
           (values boolean) )
  (let ((driver-elt (corresp-elt hypoth *driver-elt-label*))
        (recipient-elt (corresp-elt hypoth *recipient-elt-label*)))
    (cond ((or (null driver-elt) (null recipient-elt))
             nil)
          ((corresp-type hypoth :relation)                 ; propositions ?
             (= 1 (length (general-corresp-p hypoth))))    ; heterogeneous ?
          ((corresp-type hypoth :instance)                 ; objects ?
             (= 1 (length (prototype-corresp-p hypoth))))  ; heterogeneous ?
          (t nil) )))     ; object-to-relation mapping  (or error)


#|  ; Consider adding this conjunct to NEED-SKOLEMIZATION-P
(defun  MP-generated-hypothesis-p (hypoth)
  "Check whether all justifications are concept-agents"
  (declare (type corr-agent hypoth))
  (every #'(lambda (justif) (agent-type justif :concept))
         (corresp-justif hypoth)))
  ;; Note that when the justification set is empty, the function returns T.
  ;; (The set may become empty if all justifications die out.)
|#


;; HAS-RELEVANT-MARKERS-P checks whether a 'skolemizable' hypothesis would
;; augment (after skolemization) the description of a given situation.
;; For example, suppose that situation A involves a cup but not a bottle.
;; Then, the general proposition CUP-MADE-OF-CHINA is relevant while BOTTLE-
;; MADE-OF-GLASS is not.  This could be determined on the basis of the markers
;; stored in the local memories of the concept-agents involved in the two
;; general propositions -- the concept CUP would have a marker from CUP-A,
;; while BOTTLE would not have any marker originating from situation A.
;;
;; HAS-RELEVANT-MARKERS-P is a suspendable function that returns s-value.
;; (See DUAL/ARCHIT/SUSPEND.TXT and =/MAILBOX.LSP for documentation of S-VALUES)
;; The sequence of symbolic operations may be executed either by the symbolic
;; processor of the hypothesis-agent itself or by the processor of an
;; 'authorized' secretary (see SKOLEMIZE-ON-BALLOTAGE in AMBR/RATING.LSP).
;;
;; HAS-RELEVANT-MARKERS-P takes three arguments:
;;  + HYPOTH -- a hypothesis that satisfies NEED-SKOLEMIZATION-P
;;  + SITUATION -- a situation-agent defining the criterion for 'relevance'
;;  + HOST-PROCESSOR -- an agent that carries out the computation.
;; The function returns (via S-VALUES) T when HYPOTH is 'relevant' to SITUATION
;; and NIL otherwise.
;;
;; There are two kinds of hyp's that may be passed to HAS-RELEVANT-MARKERS-P.
;; These are the same kinds that satisfy NEED-SKOLEMIZATION-P, namely:
;;   1.  heterogeneous general hypotheses involving propositions and
;;   2. heterogeneous hypotheses involving prototype instances.
;;
;; Type 2 is handled by retrieving the parent concept of the prototype instance
;; and looking at the markers stored in its local buffer.  Type 1 is handled by
;; iterating through the arguments of the general proposition, retrieving the
;; respective concept, and looking at its markers.  More types of 'skolemizable'
;; hypotheses may be added in the future.
;;
;; (Implementation note: This is one of the most complicated functions in the )
;; (  whole program. The local functions exchange values among themselves and )
;; (  with the main function by SETQ-ing local variables.  Values cannot be   )
;; (  exchanged directly because all functions are suspendable.  The value of )
;; (  the main function is exported in a mailbox and must be caught by caller )
;; (  via SUSPENDED-VALUE-BIND. See DUAL/ARCHIT/SUSPEND.TXT and =/MAILBOX.LSP.)
;;
;; Compare with COLLECT-RELEVANT-INSTANCES in AMBR/SKOLEM2.LSP.

(eval-when (compile load eval)
  (proclaim-consumption 'has-relevant-markers-aux  :explicit-S-PROGN )
  (proclaim-consumption 'analyze-type-1-hypothesis :explicit-S-PROGN )
  (proclaim-consumption 'analyze-type-2-hypothesis :explicit-S-PROGN )
)
(defun  has-relevant-markers-p (hypoth situation host-processor)
  "Would HYPOTH, after skolemization, augment the description of SITUATION ?"
  (declare (type hypoth-agent hypoth)
           (type instance-agent situation)   ; situation-agent actually
           (type AMBR-agent host-processor)
           (values mailbox) )                ; (s-values boolean)
  #+:DUAL-DEBUG (assert (need-skolemization-p hypoth))
  (let ((found-marker-p nil)     ; set by HAS-RELEVANT-MARKERS-AUX
        (overall-result nil) )   ; set by ANALYZE-TYPE-x-HYPOTHESIS
   (s-progn host-processor
    (labels ((HAS-RELEVANT-MARKERS-AUX (concept)
               (let ((markers [agent-markers concept])
                     (found-p nil) )
                 (dolist (marker markers)  ; <--
                   (unless found-p
                     (when (eq situation
                               [agent-situation (marker-origin marker)])
                       (setq found-p T) )))
                 (setq found-marker-p found-p)))  ; export value to ANALYZE-...
             (ANALYZE-TYPE-1-HYPOTHESIS ()
               (let* ((label-list [general-corresp-p hypoth])   ; singleton list
                      (general-prop (corresp-elt hypoth (first label-list)))
                      (found-p nil) )
                 (dolist (S-slot (agent-S-slots general-prop))
                   (unless found-p
                     (let ((parent-concept [argument->concept S-slot]))
                       [has-relevant-markers-aux parent-concept]
                       (when found-marker-p
                         (setq found-p T)) )))
                 (setq overall-result found-p)))  ; export value to main fun
             (ANALYZE-TYPE-2-HYPOTHESIS ()
               (let* ((label-list [prototype-corresp-p hypoth]) ; singleton list
                      (prototype-inst (corresp-elt hypoth (first label-list)))
                      (parent-concept [instance->concept prototype-inst]) )
                 [has-relevant-markers-aux parent-concept]
                 (setq overall-result found-marker-p))) )  ; export val to main
      ;; Main body of HAS-RELEVANT-MARKERS-P
      (cond ((or (null (corresp-elt hypoth *driver-elt-label*))
                 (null (corresp-elt hypoth *recipient-elt-label*)))
               (s-values nil))                 ; died in the interim
            ([corresp-type hypoth :relation]
               [analyze-type-1-hypothesis]     ; put value in OVERALL-RESULT
               (s-values overall-result))
            ((corresp-type hypoth :instance)
               [analyze-type-2-hypothesis]     ; put value in OVERALL-RESULT
               (s-values overall-result))
            (t (cerror "Do not skolemize this hypothesis and continue."
                       "HAS-RELEVANT-MARKERS-P:  ~S is not skolemizable."
                       hypoth)
               (s-values nil)))) )))



;;;;;  ******   AUXILIARY  FUNCTION   ******
;;;
;;;  ARGUMENT->CONCEPT takes an S-slot in a general proposition and returns
;;;  two values.
;;;  When the S-slot has 'range' interpretation (see AMBR/SKOLEM.TXT), its
;;;   filler is a concept agent.  The function returns  (values concept nil).
;;;  When the S-slot has 'value' interpretation, its filler is a prototype
;;;   instance-agent.  The parent concept of this prototype is retrieved.
;;;   The function returns  (values concept prototype-instance).
;;;
;;;  This finction is not advertized in the external protocol but is used a
;;;  lot both by the predicates above and by the routines in AMBR/SKOLEM2.LSP.

(defun argument->concept (S-slot)
  "Retrieves the parent concept of an argument in a general proposition."
  (declare (type S-slot S-slot)                  ; owned by a general propositn
           (values concept-agent                 ; concept-argument
                   (or null instance-agent)) )   ; prototype-instance, if any
  (let ((arg (find-if #'simple-symref-p
                      (get-filler-refs! S-slot :C-COREF))))
    (cond ((range-slot-p S-slot)
             #+:DUAL-DEBUG
             (unless (agent-type arg :concept)
               (error "Range slot ~S.~(~A~) filled by non-concept ~S."
                      (slot-owner S-slot) (slot-label S-slot) arg ))
             (values arg nil) )
          ((agent-type arg '(:PROTOTYPE :instance))
             (values (instance->concept arg)
                     arg))
          (t (error "Bad argument ~S in ~A of a general proposition ~S."
                     arg (slot-label S-slot) (slot-owner S-slot))) )))



;;;;**************************************************************************
;;;;**************************************************************************
;;;;  The rest of the file defines the data structures for AMBR/SKOLEM2.LSP.
;;;;**************************************************************************

;;;;;;  ********   SKOLEM  INCENTIVES   ********
;;;
;;;  Skolem incentives are symbolic structures that trigger the skolemization
;;;  mechanism.  They are sent by the secretaries when the rating mechanism
;;;  determines that some general hypothesis is worth enough.
;;;  See AMBR/SKOLEM.TXT, =/SKOLEM2.LSP and =/RATING.LSP for more details.

;;;;;;   Class definition  (and accessor method)

(eval-when (compile load eval)
  (defclass  Skolem-symbolic-structure (symbolic-structure)
    ()
    (:documentation
      "Symbolic structure related to the skolemization mechanism. Base class."))

  (defclass  Skolem-incentive  (Skolem-symbolic-structure)
    ((secretary   :reader     Skolem-incentive-secretary
                  :type       secretary-agent
                  :initarg    :secretary
                  :initform   (required-argument) )
    )
   (:documentation
     "Symbolic structure triggering the skolemization mechanism." ))
) ; eval-when

(defmethod  Skolem-incentive-secretary ((x t))
  (error "SKOLEM-INCENTIVE-SECRETARY: ~S is not a Skolem incentive." x ))


;;;;  Constructor

(defun send-Skolem-incentive (hypothesis secretary)
  "SECRETARY sends a Skolem incentive to HYPOTHESIS."
  (declare (type corr-agent hypothesis)
           (type secretary-agent secretary)
           (values Skolem-incentive) )
  (receive-symbol hypothesis (make-instance 'Skolem-incentive
                                            :secretary secretary )))


;;;;;;  Printing methods

(defmethod  print-object ((incentive Skolem-incentive) stream)
  (if (and (slot-boundp incentive 'secretary)
           (agentp (Skolem-incentive-secretary incentive)) )
      (format stream "#<SkI ~A>"
                     (agent-name (Skolem-incentive-secretary incentive)))
      (format stream "#<malformed Skolem-incentive>") ))


(defmethod DUAL-describe ((incentive Skolem-incentive)
                          &optional (stream *standard-output*) )
  (format stream "~&~S is a ~A.~%"
          incentive (type-of incentive) )
  (format stream "~&  Its secretary is: ~S~%"
          (if (slot-boundp incentive 'secretary)
              (Skolem-incentive-secretary incentive)
              "(unbound)"))
  (values))



;;;;;  *****  SKOLEM MESSAGES  *****
;;
;;  There are two types of Skolem messages:
;;    - Skolem-message-1 is exchanged from the hypothesis to each Skolem
;;        instance and back.
;;    - Skolem-message-2 is exchanged from the hypothesis to each Skolem
;;        proposition and back.
;;  (See AMBR/SKOLEM.TXT for documentation of Sk. instances and propositions.)

;;;;;;   Class definitions  (and accessor methods)

(eval-when (compile load eval)
  (defclass  Skolem-message (Skolem-symbolic-structure)
    ((sender     :reader     Skolem-message-sender
                 :type       AMBR-agent
                 :initarg    :sender
                 :initform   (required-argument) )
     (situation  :reader     Skolem-message-situation
                 :type       instance-agent
                 :initarg    :situation
                 :initform   (required-argument) )
     (concept    :reader     Skolem-message-concept
                 :type       concept-agent
                 :initarg    :concept
                 :initform   (required-argument) )
    )
    (:documentation "Symbolic structure exchanged during skolemization." ))

  (defclass  Skolem-message-1 (Skolem-message)
    ()
    (:documentation
      "Message exchanged b/n a general hypothesis and a Skolem instance." ))

  (defclass  Skolem-message-2 (Skolem-message)
    ()
    (:documentation
      "Message exchanged b/n a general hypothesis and a Skolem proposition." ))
) ; eval-when

(defmethod  Skolem-message-sender ((x t))
  (error "SKOLEM-MESSAGE-SENDER: ~S is not a Skolem message." x ))

(defmethod  Skolem-message-situation ((x t))
  (error "SKOLEM-MESSAGE-SITUATION: ~S is not a Skolem message." x ))

(defmethod  Skolem-message-concept ((x t))
  (error "SKOLEM-MESSAGE-CONCEPT: ~S is not a Skolem message." x ))


;;;;  Constructors

(defun make-Skolem-message-1 (sender situation concept)
  (declare (type AMBR-agent sender situation concept)
           (values Skolem-message-1) )
  (make-instance 'Skolem-message-1
                 :sender    sender
                 :situation situation
                 :concept   concept ))

(defun make-Skolem-message-2 (sender situation concept)
  (declare (type AMBR-agent sender situation concept)
           (values Skolem-message-2) )
  (make-instance 'Skolem-message-2
                 :sender    sender
                 :situation situation
                 :concept   concept ))

;;;;;;  Printing methods

(defmethod  print-object ((Sk-msg-1 Skolem-message-1) stream)
  (if (and (slot-boundp Sk-msg-1 'sender)
           (agentp (Skolem-message-sender Sk-msg-1)) )
      (format stream "#<SM1 ~A>"
                     (agent-name (Skolem-message-sender Sk-msg-1)))
      (format stream "#<malformed Skolem-message-1>") ))

(defmethod  print-object ((Sk-msg-2 Skolem-message-2) stream)
  (if (and (slot-boundp Sk-msg-2 'sender)
           (agentp (Skolem-message-sender Sk-msg-2)) )
      (format stream "#<SM2 ~A>"
                     (agent-name (Skolem-message-sender Sk-msg-2)))
      (format stream "#<malformed Skolem-message-2>") ))

(defmethod DUAL-describe ((Sk-msg Skolem-message)
                          &optional (stream *standard-output*) )
  (format stream "~&~S is a ~A.~%"
          Sk-msg (type-of Sk-msg) )
  (format stream "~&  Its sender is: ~S~%"
          (if (slot-boundp Sk-msg 'sender)
              (Skolem-message-sender Sk-msg)
              "(unbound)"))
  (format stream "~&  Its situation is: ~S~%"
          (if (slot-boundp Sk-msg 'situation)
              (Skolem-message-situation Sk-msg)
              "(unbound)"))
  (format stream "~&  Its concept is: ~S~%"
          (if (slot-boundp Sk-msg 'concept)
              (Skolem-message-concept Sk-msg)
              "(unbound)"))
  (values))



;;;;;;  ********   S K O L E M   T A B L E S   ********
;;;
;;;  The purpose of Skolem tables is to keep track of the 'range' slots that
;;;  need skolemization (see AMBR/SKOLEM.TXT for details).
;;;  The tables are built by the respective hypothesis-agent and are stored
;;;  in its local buffer.
;;;
;;;  The skolemization mechanism is currently triggered only for _heterogeneous_
;;;  general correspondences -- see NEED-SKOLEMIZATION-P above.  Thus only one
;;;  of the corr. elements is a general proposition.  It is written in the
;;;  PROPOSITION field of the Skolem table.
;;;
;;;  The new temporary agents generated by the skolemization mechanism are
;;;  affiliated to the _recipient_ situation -- the situation that is currently
;;;  mapped to the driver situation.  The SITUATION field of the Skolem table
;;;  is filled at the beginning of the skolemization procedure with the current
;;;  recipient situation.  If the mechanism is triggered again later and the
;;;  recipient situation has changed, a new Skolem table will be built.
;;;  (Usually the general hypothesis fizzles out when done, so repeated
;;;   skolemization is rare.)
;;;
;;;  Each Skolem table is a data structure with seven fields:
;;;   + HYPOTHESIS  -- pointer to the 'host' hypothesis agent.  This field is
;;;                    not needed for the computations and is used for
;;;                    identification and debugging purposes only.
;;;   + PROPOSITION -- the general proposition that is being skolemized.
;;;   + SITUATION   -- the recipient situation.
;;;   + STATUS      -- keeps a tag reflecting the current skolemization status.
;;;                    The tag is one of: BEGIN, ABORT, WAIT, or DONE.
;;;   + SLOTS       -- a list of SKOLEM-SLOT-DESCRIPTORs (see below).  There is
;;;                    one such descriptor for each S-slot of PROPOSITION.
;;;   + OLD-PROPS   -- a list of SKOLEM-PROP-DESCRIPTORs for the existing props.
;;;   + NEW-PROPS   -- a list of SKOLEM-PROP-DESCRIPTORs for the new propositns.
;;;
;;;  See the section 'Filling up the Skolem table' in AMBR/SKOLEM2.LSP.

;;;;;;   Class definitions  (and accessor methods)

(eval-when (compile load eval)
  (defclass  Skolem-table  (Skolem-symbolic-structure)
    ((hypothesis  :reader     Skolem-table-hyp
                  :type       corr-agent
                  :initarg    :hypothesis
                  :initform   (required-argument) )
     (proposition :reader     Skolem-table-prop
                  :type       instance-agent      ; general proposition
                  :initarg    :proposition
                  :initform   (required-argument) )
     (situation   :reader     Skolem-table-sit
                  :type       instance-agent      ; see AMBR/SITUATN.TXT
                  :initarg    :situation
                  :initform   (required-argument) )
     (status      :accessor   Skolem-table-status
                  :type       (member :begin :abort :wait :done)
                  :initform   :begin  )
     (slots       :accessor   Skolem-table-slots
                  :type       list     ; of SKOLEM-SLOT-DESCRIPTORs (see below)
                  :initform   nil   )
     (old-props   :accessor   Skolem-table-old-props
                  :type       list     ; of SKOLEM-PROP-DESCRIPTORs (see below)
                  :initform   nil   )
     (new-props   :accessor   Skolem-table-new-props
                  :type       list     ; of SKOLEM-PROP-DESCRIPTORs (see below)
                  :initform   nil   )
    )
   (:documentation  "Data structure used by the skolemization mechanism." ))
   ;; Don't confuse SKOLEM-TABLE-PROP with SKOLEM-TABLE-NEW-PROPS.
) ; eval-when


;;;;  Type predicate

(declaim (inline Skolem-table-p))
(defun Skolem-table-p (thing)
  (and (typep thing 'Skolem-table) T))


;;;;  Constructors

(defun  make-Skolem-table  (hypothesis proposition situation)
  (declare (type corr-agent hypothesis)
           (type instance-agent proposition situation)
           (values Skolem-table) )
  (make-instance 'Skolem-table
                 :hypothesis  hypothesis
                 :proposition proposition
                 :situation   situation  ))

(defun  find-Skolem-table  (hypothesis situation &key (cerror-p T))
  "Look for a Skolem table in the local buffer of HYPOTHESIS."
  (declare (type corr-agent hypothesis)
           (type instance-agent situation)  ; see AMBR/SITUATN.TXT
           (values (or Skolem-table null)) )
  (let ((result (find-if #'(lambda (symb-struct)
                              (and (Skolem-table-p symb-struct)
                                   (eq situation
                                       (Skolem-table-sit symb-struct))))
                         (agent-buffer hypothesis))))
    (when (and (null result) cerror-p)
      (cerror "Return NIL and continue."
          "FIND-SKOLEM-TABLE: ~S does not have any Skolem table with sit. ~S."
          hypothesis situation))
    result ))

(defun  find-or-make-Skolem-table  (hypothesis situation)
  "Ensures that HYPOTHESIS's buffer contains a Skolem table."
  (declare (type corr-agent hypothesis)
           (type instance-agent situation)   ; see AMBR/SITUATN.TXT
           (values Skolem-table) )
  (let ((Skolem-table (find-Skolem-table hypothesis situation :cerror-p nil)))
    (when (null Skolem-table)      ; need to start from scratch ?
      (let* ((label-list  (general-corresp-p hypothesis))
             (proposition (corresp-elt hypothesis (first label-list))) )
        #+:DUAL-DEBUG (assert (= 1 (length label-list)))  ; heterogeneous hyp
        #+:DUAL-DEBUG (assert (general-proposition-p proposition))
        (setq Skolem-table         ; make an empty table
            (make-Skolem-table hypothesis proposition situation))
        (push Skolem-table (agent-buffer hypothesis)) ))
    Skolem-table ))


;;;;;;  Printing methods

(defmethod  print-object ((Skolem-table Skolem-table) stream)
  (if (and (slot-boundp Skolem-table 'proposition)
           (slot-boundp Skolem-table 'status)
           (agentp (Skolem-table-prop Skolem-table)) )
      (format stream "#<SkT ~A ~A>"
                     (agent-name (Skolem-table-prop Skolem-table))
                     (Skolem-table-status Skolem-table))
      (format stream "#<malformed Skolem-table>") ))

(defmethod DUAL-describe ((Skolem-table Skolem-table)
                          &optional (stream *standard-output*) )
  (format stream "~&~S is a ~A.~%"
          Skolem-table (type-of Skolem-table) )
  (format stream "~&  Its hypothesis  is: ~S~%"
          (if (slot-boundp Skolem-table 'hypothesis)
              (Skolem-table-hyp Skolem-table)
              "(unbound, which is anomalous)"))
  (format stream "~&  Its proposition is: ~S~%"
          (if (slot-boundp Skolem-table 'proposition)
              (Skolem-table-prop Skolem-table)
              "(unbound, which is anomalous)"))
  (format stream "~&  Its recipient situation is: ~S~%"
          (if (slot-boundp Skolem-table 'situation)
              (Skolem-table-sit Skolem-table)
              "(unbound, which is anomalous)"))
  (format stream "~&  Its status is: ~S~%"
          (if (slot-boundp Skolem-table 'status)
              (Skolem-table-status Skolem-table)
              "(unbound, which is anomalous)"))
  (format stream "~&  Its slots are: ~S~%"
          (if (slot-boundp Skolem-table 'slots)
              (Skolem-table-slots Skolem-table)
              "(unbound)"))
  (format stream "~&  Its old propositions are: ~S~%"
          (if (slot-boundp Skolem-table 'old-props)
              (Skolem-table-old-props Skolem-table)
              "(unbound)"))
  (format stream "~&  Its new propositions are: ~S~%"
          (if (slot-boundp Skolem-table 'new-props)
              (Skolem-table-new-props Skolem-table)
              "(unbound)"))
  (values))


;;;;  Type-checking methods for the accessors

(defmethod  Skolem-table-hyp ((x t))
  (error "SKOLEM-TABLE-HYP: ~S is not a Skolem table." x ))

(defmethod  Skolem-table-prop ((x t))
  (error "SKOLEM-TABLE-PROP: ~S is not a Skolem table." x ))

(defmethod  Skolem-table-sit ((x t))
  (error "SKOLEM-TABLE-SIT: ~S is not a Skolem table." x ))

(defmethod  Skolem-table-status ((x t))
  (error "SKOLEM-TABLE-STATUS: ~S is not a Skolem table." x ))

(defmethod  (setf Skolem-table-status) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF SKOLEM-TABLE-STATUS): ~S is not a Skolem table." x ))

(defmethod  Skolem-table-slots ((x t))
  (error "SKOLEM-TABLE-SLOTS: ~S is not a Skolem table." x ))

(defmethod  (setf Skolem-table-slots) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF SKOLEM-TABLE-SLOTS): ~S is not a Skolem table." x ))

(defmethod  Skolem-table-old-props ((x t))
  (error "SKOLEM-TABLE-OLD-PROPS: ~S is not a Skolem table." x ))

(defmethod  (setf Skolem-table-old-props) (old-value (x t))
  (declare (ignore old-value))
  (error "(SETF SKOLEM-TABLE-OLD-PROPS): ~S is not a Skolem table." x ))

(defmethod  Skolem-table-new-props ((x t))
  (error "SKOLEM-TABLE-NEW-PROPS: ~S is not a Skolem table." x ))

(defmethod  (setf Skolem-table-new-props) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF SKOLEM-TABLE-NEW-PROPS): ~S is not a Skolem table." x ))



;;;;;;  ********   SKOLEM SLOT DESCRIPTORS   ********
;;;
;;;  The SLOTS field of Skolem tables is filled with a list of 'slot desriptors'
;;;  A SKOLEM-SLOT-DESCRIPTOR is a data structure keeping the following fields:
;;;    + S-label   -- the label of the respective S-slot.
;;;    + concept   -- the concept-agent supplying instances for the skolemiztn.
;;;                     NIL when the concept is not visible.
;;;    + prototype -- a prototype-instance or NIL.
;;;    + instances -- a list of instances of CONCEPT. When there are relevant
;;;                   markers supplied by CONCEPT, the list keeps the origins
;;;                   of these markers.  When CONCEPT has no relevant markers,
;;;                   this field is initially bound to the empty list. Later
;;;                   on it is filled by a list of one Skolem instance.
;;;
;;; In the current implementation, Skolem slot descriptors are represented as
;;; four-element lists.

(defun  make-Skolem-slot-descriptor (S-label concept prototype
                                                     &optional instances)
  "Make a descriptor used in the SLOTS field of Skolem tables."
  (declare (type (or null concept-agent)  concept)
           (type (or null instance-agent) prototype)
           (type list instances) )        ; list of instance-agents or NIL
  (list S-label concept prototype instances) )
  ;; Used mainly by COLLECT-CONCEPT-ARGUMENTS in AMBR/SKOLEM2.LSP

(declaim (inline SSD-label SSD-concept SSD-prototype SSD-instances))
(defun  SSD-label (slot-descriptor)
  "Reader for the LABEL field of slot descriptors in a Skolem table."
  (first slot-descriptor) )
(defun  SSD-concept (slot-descriptor)
  "Reader for the CONCEPT field of slot descriptors in a Skolem table."
  (second slot-descriptor) )
(defun  SSD-prototype (slot-descriptor)
  "Reader for the PROTOTYPE field of slot descriptors in a Skolem table."
  (third slot-descriptor) )
(defun  SSD-instances (slot-descriptor)
  "Accessor for the INSTANCES field of slot descriptors in a Skolem table."
  (fourth slot-descriptor) )

;; Make SSD-INSTANCES usable with SETF
(defsetf SSD-instances (slot-descriptor)
         (new-instances)
  `(setf (fourth ,slot-descriptor) ,new-instances) )



;;;;;;  ********   SKOLEM PROPOSITION DESCRIPTORS   ********
;;;
;;;  The OLD-PROPS and NEW-PROPS fields of Skolem tables are filled with lists
;;;  of 'proposition descriptors'.  A SKOLEM-PROP-DESCRIPTOR is a data structure
;;;  keeping the following fields:
;;;    + target -- NIL or [proposition] instance-agent
;;;    + slots  -- an a-list keyed by S-labels describing the slots of TARGET.
;;;                (Compare with PROPOSITION-ARGS from AMBR/KREPRES.LSP.)
;;;
;;; In the current implementation, prop descriptors are represented as CONS
;;; cells. (They appear as lists because the CDR of the cons is a list itself.)

(defun  make-Skolem-prop-descriptor (target slots)
  "Make a descriptor used in OLD-PROPS and NEW-PROPS fields of Skolem tables."
  (declare (type (or null instance-agent) target)
           (type list slots) )           ; a-list keyed by S-label
  (cons target slots) )

(declaim (inline SPD-target SPD-slots))
(defun  SPD-target (prop-descriptor)
  "Accessor for the TARGET field of proposition descriptors in a Skolem table."
  (car prop-descriptor) )
(defun  SPD-slots (prop-descriptor)
  "Accessor for the SLOTS field of proposition descriptors in a Skolem table."
  (cdr prop-descriptor) )

;; Make SPD-TARGET and SPD-SLOTS usable with SETF
(defsetf SPD-target (prop-descriptor)
         (new-target)
  `(setf (car ,prop-descriptor) ,new-target) )
(defsetf SPD-slots (prop-descriptor)
         (new-slots)
  `(setf (cdr ,prop-descriptor) ,new-slots) )


;;;;;;;  End of file AMBR/SKOLEM1.LSP
