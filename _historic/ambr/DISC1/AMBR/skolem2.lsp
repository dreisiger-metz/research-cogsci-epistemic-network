;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/skolem2.lsp    ; see AMBR/skolem.txt and AMBR/skolem1.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Skolemization mechanism
;;; DEPENDS-ON: DUAL; ambr/marker.lsp, ambr/hypoth.lsp, ambr/skolem1.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    25-05-98 [2.2.2]  Second part of the old AMBR/SKOLEM.LSP.
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...
;;;;;;;
;;; TO DO:      Provide for base->target skolemization.
;;; TO DO:      Consider using alarm clocks and changing the Skolem status from
;;;             :WAIT to :ABORT if Skolem messages fail to arrive for too long.
;;; TO DO:      Distinguish between :CONJECTURE and :INFERENCE modalities
;;;             of Skolem instances depending on the general proposition.


         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;    S K O L E M I Z A T I O N   (part 2)   ;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR-CORE")

;;;; The key concept defined in this file is SKOLEMIZATION -- a technique
;;;; for constructing 'specific propositions' on the basis of 'general
;;;; propositions'.  This file is part 2 of two parts, see AMBR/SKOLEM2.LSP.
;;;;
;;;; Part 1 is the 'entry point' to the skolemization mechanism. In particular,
;;;;  it defines the function SEND-SKOLEM-INCENTIVE which is the trigger.
;;;;  It also defines some predicates (e.g. NEED-SKOLEMIZATION-P) used by other
;;;;  mechanisms to judge whether to send Skolem incentives or not.  Finally,
;;;;  part 1 defines the data structures (e.g. 'Skolem table') for part 2.
;;;;
;;;; Part 2 (this file) implements the main skolemization algorithms.
;;;;
;;;; See AMBR/SKOLEM.TXT for detailed introduction to Skolemization.
;;;;
;;;; The skolemization mechanism may be blocked by setting *SKOLEMIZATION-FLAG*
;;;; to NIL.  The default value for this flag is T (see AMBR/DEFS.LSP).


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: skolemize-hypothesis,
;;          ensure-instance-arguments,
;;          ensure-Skolem-propositions,
;;          finish-skolemization
;;
;; The functions in this file depend on the following parameters from DEFS.LSP:
;;   *Skolem-instance-weight*, *Skolem-proposition-weight*, *Skolem-INST-OF-
;;   weight*, *Skolem-SITUATION-weight*, and *Skolem-C-COREF-weight*.

;; ...

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric  ensure-instance-arguments (general-hypoth Skolem-table)
  (:documentation "Begin [really] to skolemize GENERAL-HYPOTH." ))
  ;; This function is made generic for the sole purpose of spying.
  ;; See AMBR/INTRFACE/SET_SPY.LSP.

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;  ******  TRIGGERING THE SKOLEMIZATION MECHANISM  ******
;;;
;;;  The skolemization mechanism is triggered by the coordinated effort of
;;;  many AMBR mechanisms -- see AMBR/SKOLEM.TXT and AMBR/RATING.LSP.
;;;  If we ignore the details, this process culminates in a SKOLEM-INCENTIVE
;;;  sent to the general hypothesis by an authorized secretary (RATING.LSP).
;;;  Thus, for the purposes of this file we may assume that skolemization
;;;  begins when a hypothesis-agent receives a SKOLEM-INCENTIVE.
;;;  (Skolem incentives are defined in AMBR/SKOLEM1.LSP.)
;;;
;;;  The rating mechanism is such that: (i) only hypotheses that satisfy the
;;;  predicate NEED-SKOLEMIZATION-P may receive skolem incentives and (ii)
;;;  a hypothesis-agent may receive several such incentives in arbitrary
;;;  moments in time.
;;;
;;;  This section deals with the initial receipt of Skolem incentives,
;;;  screening out redundant incentives, re-sending them, etc.
;;;
;;;  Two kinds of hypotheses satisfy the predicate NEED-SKOLEMIZATION-P (def.
;;;  in AMBR/SKOLEM2.LSP) and hence may expect to receive Skolem incentives.
;;;    1. heterogeneous general hypotheses involving propositions and
;;;    2. heterogeneous hypotheses involving prototype instances.
;;;        (Prototypical instances in turn serve as arguments to general props.)
;;;
;;;  They are the hypotheses of type 1. that carry out the skolemization
;;;  routines.  Those of type 2. merely re-send the Skolem incentives to their
;;;  respective type-1 hypothesis.

(eval-when (compile load eval)
  (proclaim-consumption 're-send-Skolem-incentive     :explicit-S-PROGN )
  (proclaim-consumption 're-send-Skolem-incentive-aux :explicit-S-PROGN )
  (proclaim-consumption 'handle-Skolem-incentive      :explicit-S-PROGN )
)
(defmethod handle-symbol ((hypoth hypoth-agent)
                          (incentive Skolem-incentive) )
  (declare (values S-PROGN-return-type))
  #+:DUAL-DEBUG (assert (need-skolemization-p hypoth))
  (s-progn hypoth
     (cond ([corresp-type hypoth :relation]    ; type-1 ?
               [handle-Skolem-incentive hypoth incentive])
           ((corresp-type hypoth :instance)    ; type-2 ?
               [re-send-Skolem-incentive hypoth incentive])
           (t  (cerror "Ignore the incentive and continue."
                  "~S has received ~S but is neither :RELATION nor :INSTANCE."
                  hypoth incentive)) )))


;; RE-SEND-SKOLEM-INCENTIVE handles the case in which a heterogeneous hypoth.
;; involving prototype instances has been sent a Skolem incentive.  In other
;; words, it is assumed that HYPOTH satisfies the following conjunction:
;;  (and (need-skolemization-p hypoth)      ; heterogeneous hypothesis
;;       (corresp-type hypoth :instance))   ; involving (prototype) instances
;;
;; Such hypotheses cannot skolemize because they are not about propositions but
;; about arguments -- e.g. red-17<-->prototype-white.  The task then is to
;; locate the proposition(s) that can skolemize and re-send the incentive there.
;;
;; This is done according to the following algorithm:
;;   0. Ensure that both hypothesis elements are still visible.
;;       (This is a routine check for hypoth. agents -- see AMBR/HYPOTH.LSP).
;;   1. Determine which of the two hypoth-elements is the prototype instance.
;;       The function PROTOTYPE-CORRESP-P returns the slot label of this elt.
;;   2. Retrieve the propositions in which the prototypical instance acts as
;;       argument.  The function ARGUMENT-PROPS (see AMBR/KREPRES.LSP) does
;;       this by looking in the C-COREF slot of the prototypical instance.
;;       (In our example the prototypical instance  PROTOTYPE-WHITE will
;;        have a C-COREF link to EACH-SNOWDROP-IS-WHITE.SLOT2)
;;       Theoretically, the instance may be involved in more than one props,
;;       so we have to deal with a list of propositions (although the list
;;       usually has only one element).
;;   3. Retrieve the 'driver situation' of the host hypothesis.
;;   4. For each proposition retrieved at step 2. that satisfies GENERAL-
;;      PROPOSITION-P do the following:
;;    4a. Retrieve the list of hypotheses registered at its secretary.
;;    4b. Filter out all 'irrelevant' hypotheses -- i.e. those not having the
;;         same driver situation as the one from step 3.
;;    4c. Re-send the Skolem incentive to the remaining (relevant) hypotheses,
;;         if any.
;;   5. Stop.  Do not store the Skolem incentive in the local buffer.

(defun  re-send-Skolem-incentive (hypoth incentive)
 (declare (type hypoth-agent hypoth)     ; of 'type-1'
          (type Skolem-incentive incentive)
          (values S-PROGN-return-type) )
 (let* ((label-list (prototype-corresp-p hypoth))                    ; step 1.
        (prototype-instance (corresp-elt hypoth                      ; step 1.
                                        (first label-list)))
        (driver-situation (corresp-driver hypoth)) )                 ; step 3.
   #+:DUAL-DEBUG (assert (and (= 1 (length label-list))
                              (agent-type prototype-instance :prototype)))
   (s-progn hypoth
      [ensure-hypothesis-elements hypoth]                            ; step 0.
      (let* ((symrefs [argument-props prototype-instance])           ; step 2.
             (propositions (mapcar #'symref-agent symrefs)) )        ; step 2.
        (dolist (prop propositions)                                  ; step 4.
          (when [general-proposition-p prop]                         ; step 4.
            [re-send-Skolem-incentive-aux hypoth                ; steps 4a-4c.
                                          incentive
                                          prop
                                          driver-situation])) ))))

(defun  re-send-Skolem-incentive-aux (hypoth incentive proposition driver-sit)
  (s-progn hypoth
     (let* ((symrefs [agent-hypotheses proposition])                ; step 4a.
            (hypotheses (mapcar #'symref-agent symrefs)) )          ; step 4a.
       (dolist (hyp hypotheses)
         (when (eq driver-sit [corresp-driver hyp])                 ; step 4b.
           [receive-symbol hyp incentive])) )))                     ; step 4c.


;; HANDLE-SKOLEM-INCENTIVE is the real entry point to the skolemization
;; mechanism.  It is applied to heterogeneous general hypotheses involving
;; propositions (see AMBR/SKOLEM.TXT for clarification of terminology).
;; In other words, HYPOTH satisfies the following conjunction:
;;  (and (need-skolemization-p hypoth)      ; heterogeneous (general) hypothesis
;;       (corresp-type hypoth :relation))   ; involving (general) propositions
;;
;; The first task is to detect (and ignore) redundant Skolem incentives.
;; Redundancy is judged with respect to some _recipient_ situation -- the sit.
;; which is currently mapped to the driver situation (call it MAP-SIT below).
;; The second task is to find or make the appropriate SKOLEM-TABLE for that
;; recipient situation and to proceed with SKOLEMIZE-HYPOTHESIS when warranted.
;;
;; More concretely, HANDLE-SKOLEM-INCENTIVE performs the following steps:
;;   1. Retrieve the driver situation of the host hypoth (call it DRIVER-SIT).
;;   2. Find MAP-SIT by applying DRIVER-ELT-MAPPING (see AMBR/SECRETAR.LSP)
;;       to DRIVER-SIT.
;;   3. If MAP-SIT is NIL then ignore the Skolem incentive and stop.  It is
;;       too early to skolemize.
;;   4. Look in the local buffer for a Skolem table whose 'situation' field
;;       is equal to MAP-SIT.  Make a new table if none is found.
;;   5. Analyze the 'status' field of the Skolem table found (or made) at
;;       step 4.
;;    5a. If the status is :WAIT or :DONE, ignore the Skolem incentive -- it
;;        is redundant. (See the documentation for FINISH-SKOLEMIZATION below.)
;;    5b. If the status is :BEGIN, proceed with SKOLEMIZE-HYPOTHESIS -- this
;;        is the first Skolem incentive received during the 'reign' of MAP-SIT.
;;    5c. If the status is :ABORT, proceed with SKOLEMIZE-HYPOTHESIS -- the
;;        last skolemization attempt has failed but now there is a new Skolem
;;        incentive which means that this hypothesis merits another try.

(defun  handle-Skolem-incentive (hypoth incentive)
  (declare (type hypoth-agent hypoth)    ; of 'type-2'
           (ignore incentive)            ; may be analyzed in future versions
           (values S-PROGN-return-type) )
  (s-progn hypoth
    (let* ((driver-sit (corresp-driver hypoth))                      ; step 1.
           (map-sit [driver-elt-mapping driver-sit]))                ; step 2.
      (unless (null map-sit)                                         ; step 3.
        (let ((table [find-or-make-Skolem-table hypoth map-sit]))    ; step 4.
          (ecase (Skolem-table-status table)                         ; step 5.
            ((:wait  :done )  nil)                                     ; 5a.
            ((:begin :abort)  [skolemize-hypothesis hypoth             ; 5bc.
                                                    table])) )))))


;;;;;;  ******  FILLING UP THE SKOLEM TABLE  ******
;;;
;;;  The Skolem table keeps all the information needed for the skolemization
;;;  mechanism.  It also serves as a lock against redundant Skolem incentives.
;;;  The work of the mechanism hinges on the Skolem table -- as the process
;;;  advances, more and more stuff accumulates in the table.  When all fields
;;;  are filled up, skolemization is complete.  The whole sequence begins with
;;;  the following broad steps:
;;;   0. Construction of an empty table and setting the PROPOSITION and
;;;        SITUATION fields -- carried out by HANDLE-SKOLEM-INCENTIVE via
;;;        FIND-OR-MAKE-SKOLEM-TABLE (see above).
;;;   1. Retrieving the 'concept arguments' (see AMBR/SKOLEM.TXT) and preparing
;;;        the SLOTS field -- carried out by COLLECT-CONCEPT-ARGUMENTS.
;;;        It's a list of 'Skolem slot descriptors' (SSD), see AMBR/SKOLEM1.LSP.
;;;        The INSTANCES subfield of each slot descriptor is left empty.
;;;   2. Checking whether all concept arguments are visible. If there are
;;;        invisible ones, set STATUS to :ABORT and terminate.
;;;   3. Retrieving 'relevant' (or 'useful') markers from the concept-args and
;;;        storing the respective instance-agents in the INSTANCES subfields
;;;        of slot descriptors -- carried out by COLLECT-RELEVANT-INSTANCES.
;;;   4. Checking whether the general proposition is 'supported', i.e. whether
;;;        there is at least one relev instance.  If none is found, set STATUS
;;;        to :ABORT and terminate. Carried out by SKOLEMIZE-HYPOTHESIS-AUX.
;;;   5,6,etc. Otherwise set STATUS to :WAIT and proceed with ENSURE-INSTANCE-
;;;        ARGUMENTS.  (This second phase is dealt with separately.)
;;;
;;;  Note that most operations involving the Skolem table are destructive.
;;;  Thus the table serves as a blackboard.
;;;  The class SKOLEM-TABLE itself is defined in AMBR/SKOLEM1.LSP.

(eval-when (compile load eval)
  (proclaim-consumption 'skolemize-hypothesis-aux   :explicit-S-PROGN )
  (proclaim-consumption 'collect-concept-arguments  :explicit-S-PROGN )
  (proclaim-consumption 'collect-relevant-instances :explicit-S-PROGN )
)
(defun  skolemize-hypothesis (hypoth table)
  "Transform a general hypothesis into a specific one."
  (declare (type mature-hypoth-agent hypoth)
           (type Skolem-table table)
           (values S-PROGN-return-type) )
  #+:DUAL-DEBUG (assert (member (Skolem-table-status table) '(:begin :abort)))
  (s-progn hypoth
    [collect-concept-arguments hypoth table]                     ; step 1.
    (if (notevery #'SSD-concept     ; i.e. #'visible-concept-p   ; step 2.
                  (Skolem-table-slots table))
        (setf (Skolem-table-status table) :ABORT)                ; step 2.
        [skolemize-hypothesis-aux hypoth table] )))              ; steps 3-5

(defun  skolemize-hypothesis-aux (hypoth table)
  (declare (type mature-hypoth-agent hypoth)
           (type Skolem-table table)
           (values S-PROGN-return-type) )
  (s-progn hypoth
    [collect-relevant-instances hypoth table]                    ; step 3.
    (cond ((notany #'SSD-instances (Skolem-table-slots table))   ; step 4.
              (setf (Skolem-table-status table) :ABORT))         ; step 4.
          (t  (setf (Skolem-table-status table) :WAIT)           ; step 5.
              [ensure-instance-arguments hypoth table]) )))      ; step 5...

(defun  collect-concept-arguments (hypoth table)   ; step 1.
  "Fill up the SLOTS field of a Skolem table. Check concept visibility."
  (declare (type mature-hypoth-agent hypoth)
           (type Skolem-table table)
           (values S-PROGN-return-type) )
 (flet ((slot->SSD (S-slot &aux (slot-label (slot-label S-slot)))
          (multiple-value-bind (concept-arg prototype-inst)
                               (argument->concept S-slot)
            (cond ((not (agent-visible-p concept-arg))
                      (make-Skolem-slot-descriptor slot-label nil nil))
                  ((and prototype-inst
                        (not (agent-visible-p prototype-inst)))
                      (make-Skolem-slot-descriptor slot-label nil nil))
                  (t  (make-Skolem-slot-descriptor slot-label     ; visible OK
                                 concept-arg prototype-inst)))) ))
   (let ((S-slots (agent-S-slots (Skolem-table-prop table))))
     (s-progn hypoth
        (s-eval (* (length S-slots) (get-consumption 'argument->concept t))
                (setf (Skolem-table-slots table)
                      (mapcar #'slot->SSD S-slots))) ))))

(defun  collect-relevant-instances (hypoth table)  ; step 2.
  "Fill up the SLOTS field of a Skolem table."     ; cf. HAS-RELEVANT-MARKERS-P
  (declare (type mature-hypoth-agent hypoth)
           (type Skolem-table table)
           (values S-PROGN-return-type) )
  (let ((map-sit (Skolem-table-sit table)))
    (s-progn hypoth
      (dolist (SSD (Skolem-table-slots table))
        (let* ((markers [agent-markers (SSD-concept SSD)])
               (all-instances (mapcar #'marker-origin markers))
               (relev-instances nil) )
          (dolist (inst all-instances)
            (when (agent-visible-p inst)
              (when (eq map-sit [agent-situation inst])
                (push inst relev-instances))))
          (setf (SSD-instances SSD)
                (sort relev-instances #'> :key #'agent-activation)))) )))



;;;;;;  ******  GENERATING SKOLEM INSTANCE-ARGUMENTS  ******
;;;
;;;  The purpose of skolemization is to generate specific propositions in place
;;;  of general ones (see AMBR/SKOLEM.TXT).  To that end, the concept-arguments
;;;  must be substituted with instance-arguments.  Part of the instance-agents
;;;  needed for that role are supplied by the marker-passing mechanism.  They
;;;  are collected by COLLECT-RELEVANT-INSTANCES (see above) and are stored
;;;  in the SLOTS field of the Skolem table.
;;;
;;;  Sometimes the marker-passing mechanism supplies instance-agents for all
;;;  arguments of the general proposition being skolemized.  In such cases no
;;;  Skolem instances need (and indeed should) be generated.  There even may
;;;  be an existing proposition matching the general rule. LOOK-FOR-NON-SKOLEM-
;;;  PROPOSITIONS collects such 'old propositions' and stores them in the
;;;  OLD-PROPS field of the Skolem table.
;;;
;;;  Sometimes, however, there are concept-arguments without any 'relevant'
;;;  markers (and hence ready instance-arguments).  In such cases ENSURE-
;;;  INSTANCE-ARGUMENTS must generate new agents called 'Skolem instances'
;;;  (see AMBR/SKOLEM.TXT).  This is done by formulating a node-construction
;;;  request and recruiting a node constructor.  The concept-argument becomes
;;;  the 'mentor' of the new instances (see DUAL/ARCHIT/NC_AGENT.LSP for
;;;  details on node construction).  See FORMULATE-SKOLEM-INSTANCE-NCR below.
;;;
;;;  The coordination of Skolem instances and the general hypothesis that has
;;;  generated them is done by exchanging 'Skolem messages of type 1'.
;;;
;;;  ENSURE-INSTANCE-ARGUMENTS is made generic in order to enable spying.
;;;  (See AMBR/INTRFACE/SET_SPY.LSP.)  It is convinient because it marks the
;;;  entry point to the 'real' skolemization.

(defmethod  ensure-instance-arguments ((hypoth mature-hypoth-agent)
                                       (table Skolem-table) )
  "Find an instance-arg for each concept-arg. Make Skolem instances if needed."
  (declare (values S-PROGN-return-type) )
  (let* ((map-sit (Skolem-table-sit table))
         (slot-descriptors (Skolem-table-slots table))
         (empty-concepts (identify-concepts-with-no-instances slot-descriptors)))
    (s-progn hypoth
      (if (null empty-concepts)
          [look-for-non-Skolem-propositions hypoth table]
          (dolist (concept empty-concepts)
            [send-NC-request hypoth
                  [formulate-Skolem-inst-NCR hypoth concept map-sit]]) ))))


;; IDENTIFY-CONCEPTS-WITH-NO-INSTANCES takes a list of slot descriptors and
;; returns a list of all 'empty' concepts.  An 'empty' concept in this context
;; means a concept agent who has no 'relevant' markers in its buffer and hence
;; the INSTANCES field of its respective slot descriptor is NIL.  A concept
;; that has some markers but their respective orignis are not active is also
;; considered empty.
;;
;; If two or more slot descriptors refer to the same concept, it is included
;; only once in the result.  In other words, if a concept appears several times
;; in a general proposition, only one Skolem instance will be constructed for
;; this concept.  This does not follow strictly the mathematical notion of
;; skolemization -- the formula  'Exists X and exists Y that P(X,Y)' is not
;; equivalent to  'Exists X that P(X,X)'.  General propositions with repeated
;; concept-args are unlikely to appear in the knowledge base, however, and so
;; I chose to keep things simple.  This may change in future versions.

(defun identify-concepts-with-no-instances (slot-descriptors)
  (let ((result nil))
    (dolist (SSD slot-descriptors)
      (setf (SSD-instances SSD)                           ; consider only the
            (delete-if (complement #'agent-visible-p)     ; visibile instances
                       (SSD-instances SSD)))
      (when (null (SSD-instances SSD))
        (pushnew (SSD-concept SSD) result) ))   ; duplicates included only once
    (nreverse result) ))



;; FORMULATE-SKOLEM-INST-NCR constructs a description of a Skolem instance.
;; (Compare with FORMULATE-SKOLEM-PROP-NCR below, FORMULATE-SC-NCR in AMBR/
;;  STR_CORR.LSP, and FORMULATE-MP-NCR in AMBR/MARKER.LSP.)
;;
;; The Skolem instances are temporary instance agents and are distinguished
;; by the tag :CONJECTURE in their MODALITY slot.  By convention, the names
;; of Skolem instances begin with a '*' (e.g. *SNOWDROP-23).
;;
;; Note the 'mail' for the new agent -- a SKOLEM-MESSAGE-1 giving it the name
;; of the general hypothesis that has created it and the situation-agent to
;; affiliate to. Note also that the creator and the mentor are not the same.

(defun formulate-Skolem-inst-NCR (parent-hypoth concept situation)
  "Formulates a NC-request for a Skolem instance of CONCEPT."
  (declare (type corr-agent parent-hypoth)
           (type concept-agent concept)
           (type instance-agent situation)  ; situation-agent actually
           (values node-construction-request) )
  (let ((mentor (make-conref (make-symref concept :t-link)
                             *Skolem-instance-weight*))
        (type-filler (cond ((agent-type concept :object)
                               '(:object :instance :temporary))
                           ((agent-type concept :relation)
                               '(:relation :instance :temporary))
                           ((agent-type concept :situation)
                               '(:situation :instance :temporary))
                           (t  '(:instance :temporary))))
        (INST-OF-filler (make-conref concept *Skolem-INST-OF-weight*)) )
    (make-NC-request
          mentor                                                  ; mentor
          'temp-instance-agent                                    ; agent-type
          (Skolem-inst-GENNAME-template concept)
          (list (list :TYPE      type-filler)                     ; slots
                (list :MODALITY  :conjecture)
                (list :INST-OF   INST-OF-filler)
                  ;; SITUATION link established by the new instance itself.
                ) ;; C-COREF link est. after the Skolem proposition is at hand.
          (list (make-Skolem-message-1 parent-hypoth              ; mail
                                       situation
                                       concept)) )))


(defun Skolem-inst-GENNAME-template (concept)
  "Construct a GENNAME template for the name of a Skolem instance."
  (list "*~A-~D"                 ; see GENNAME in DUAL/ARCHIT/BASIC.LSP
        (agent-name concept)         ; e.g:  *HEAT-SOURCE-1, *CUP-3, etc.
        1 ))


;;;;;; Handling Skolem messages of type 1.
;;
;;  This subsection deals with the 'registration' of the new Skolem instance-
;;  arguments.  They receive 'mail' from their parent general hypothesis in
;;  the form of a SKOLEM-MESSAGE-1.  Upon receipt of such message, the new
;;  Skolem instance does three things:
;;   1. reply to parent
;;   2. affiliate to the situation mentioned in the message  (AMBR/KREPRES.LSP)
;;   3. emit a T-marker (see AMBR/MARKER.LSP)

(defmethod  trigger-symbolic-processor ((agent temp-instance-agent)
                                        (event (eql :just-created)) )
  (declare (ignore agent event))
  nil )

(defmethod handle-symbol ((Skolem-instance temp-instance-agent)
                          (message Skolem-message-1) )
  (declare (values S-PROGN-return-type))
  (let ((parent-hypothesis (Skolem-message-sender message))
        (situation (Skolem-message-situation message))
        (concept   (Skolem-message-concept message)) )
    #+:DUAL-DEBUG (assert (eq concept (instance->concept Skolem-instance)))
    (s-progn Skolem-instance
       [receive-symbol parent-hypothesis                            ; step 1.
          [make-Skolem-message-1 Skolem-instance situation concept]]
       [affiliate-agent Skolem-instance                             ; step 2.
                        situation *Skolem-SITUATION-weight*]
       [send-T-marker Skolem-instance concept] )))                  ; step 3.


;; The parent general hypoth receives the message from the Skolem instance and:
;;  1. Check whether the Skolem instance is still alive.  If it has died
;;      at birth, formulate and send a new NC request.
;;  2. Otherwise register the Skolem instance in the Skolem table.
;;  3. If the respective slot-descriptor(s) in the table keep a prototype-
;;      instance, create a T-LINK from the prototype to the Skolem instance.
;;  4. Check whether this is the last Skolem instance to wait for.  If all
;;      Skolem arguments are at hand, proceed with ENSURE-SKOLEM-PROPOSITIONS.
;;      Otherwise wait for more Skolem-message-1's.

(defmethod handle-symbol ((hypoth hypoth-agent)
                          (message Skolem-message-1) )
  (declare (values S-PROGN-return-type))
  (let* ((instance  (Skolem-message-sender    message))
         (situation (Skolem-message-situation message))
         (concept   (Skolem-message-concept   message))
         (table  (find-Skolem-table hypoth situation))
         (slot-descriptors (Skolem-table-slots table)) )
    (s-progn hypoth
      (cond ([agent-visible-p instance]                               ; step 1.
                (dolist (SSD slot-descriptors)                        ; step 2.
                  (when (eq concept (SSD-concept SSD))
                    (pushnew instance (SSD-instances SSD))            ; step 2.
                    (when (SSD-prototype SSD)                         ; step 3.
                      [send-write-request (SSD-prototype SSD)
                            'add-link :t-link                         ; step 3.
                            instance  *Skolem-instance-weight*]) ))
                (unless (identify-concepts-with-no-instances          ; step 4.
                                                   slot-descriptors)
                  [ensure-Skolem-propositions hypoth table]) )        ; step 4.
            (t  [send-NC-request hypoth                               ; step 1.
                      [formulate-Skolem-inst-NCR hypoth       ; try again
                                                 concept
                                                 situation]]) ))))


;;;;;;  ******  LOOKING FOR NON-SKOLEM PROPOSITIONS  ******
;;;
;;;  Sometimes the rating mechanism sends Skolem incentives prematurely.
;;;  This could lead to generation of Skolem propositions that are identical
;;;  with existing propositions in the recipient situation.  The job of
;;;  LOOK-FOR-NON-SKOLEM-PROPOSITIONS is to collect all potentially relevant
;;;  old propositions and store them in the OLD-PROPS field of the Skolem
;;;  table.  It is invoked when ENSURE-INSTANCE-ARGUMENTS detects that all
;;;  instance-arguments come from markers and, therefore, belong to the
;;;  recipient situation.  (If at least one of the args is a Skolem instance,
;;;  none of the old propositions could be relevant and the OLD-PROPS field of
;;;  the table remains NIL.)  The control is then passed to ENSURE-SKOLEM-
;;;  PROPS which fills in the NEW-PROPS filed of the table and checks for
;;;  matches between old-props and new-props.
;;;

;; LOOK-FOR-NON-SKOLEM-PROPOSITIONS works according to the following algorithm:
;;  1. Retrieve the GENERAL-PROP from the Skolem table.
;;  2. Determine the 'relational concept' -- the parent of GENERAL-PROP.
;;  3. Retrieve the recipient SITUATION from the Skolem table.
;;  4. Read the markers from the buffer of RELAT-CONCEPT and get their origins.
;;  5. Collect all instance-agents (i.e. marker-origins) whose affiliation
;;      is the same as SITUATION.  Call such propositions 'relevant-props'.
;;  6. Make a proposition description (see AMBR/SKOLEM1.LSP) for each relevant
;;      proposition and store it in the OLD-PROPS field of the Skolem table.
;;  7. Proceed with ENSURE-SKOLEM-PROPOSITIONS.

(defun  look-for-non-Skolem-propositions (hypoth table)
  "Look for markers from existing propositions."
  (declare (type mature-hypoth-agent hypoth)
           (type Skolem-table table)
           (values S-PROGN-return-type) )
  (let* ((general-prop (Skolem-table-prop table))                  ; step 1.
         (relat-concept (instance->concept general-prop))          ; step 2.
         (situation (Skolem-table-sit table)) )                    ; step 3.
    (s-progn hypoth
      (let* ((markers [agent-markers relat-concept])               ; step 4.
             (all-props (mapcar #'marker-origin markers))          ; step 4.
             (prop-descriptors nil) )
        (dolist (prop all-props)
          (when (agent-visible-p prop)
            (when (eq situation [agent-situation prop])            ; step 5.
              (let ((prop-args [proposition-args prop]))           ; step 6.
                (push (make-Skolem-prop-descriptor prop prop-args) ; step 6.
                      prop-descriptors)))))
        (setf (Skolem-table-old-props table)                       ; step 6.
              (nreverse prop-descriptors))
        [ensure-Skolem-propositions hypoth table] ))))             ; step 7.


;; FIND-MATCHING-SPD is used in the main loop of ENSURE-SKOLEM-PROPOSITIONS
;; (see below).  For the time being (ver. 2.2.2) two proposition-descriptors
;; 'match' iff their SLOTS fields are equal (see EQUAL-ARGS in AMBR/KREPRES.LSP)
;; This simple and straightforward test doesn't work when the specific prop
;; is an exception to the general rule.  (For example, all cups are made of
;; china but CUP-3 is made of glass -- see AMBR/SKOLEM.TXT for details.)

(defun  find-matching-SPD (Skolem-table new-SPD)
  "Look in SKOLEM-TABLE-OLD-PROPS for a SPD that matches NEW-SPD."
  (find-if #'(lambda (old-SPD)
               (EQUAL-ARGS (SPD-slots old-SPD)
                           (SPD-slots new-SPD)) )
           (Skolem-table-old-props Skolem-table)) )


;; STRENGTHEN-NON-SKOLEM-PROPOSITION is called when a prop-descriptor from
;; Skolem-table-old-props matches a new prop-descriptor (from ...-new-props).
;; It does the following steps:
;;  1. Retrieve OLD-PROPOSITION from the old prop-descriptor
;;  2. Arrange for a T-LINK from the general proposition to OLD-PROPOSITION.
;;  3. Register OLD-PROPOSITION in the TARGET field of the new prop-descriptor.
;;
;; (Consider a fourth step -- remove the old SPD from Skolem-table-old-props. )
;; ( The same SPD will never be needed again as all new SPDs are different.   )
;; ( Thus, staying in Skolem-table-old-props, it will only slow down future   )
;; ( searches.  On the other hand, it is useful for debugging purposes to     )
;; ( keep the complete list of old prop-descriptors.                          )
;;

(defun  strengthen-non-Skolem-proposition (hypoth table old-SPD new-SPD)
  "T-LINK to an existing proposition instead of constructing Skolem props."
  ;; Assumption: (equal-args (SPD-slots old-SPD) (SPD-slots new-SPD))
  (let ((old-proposition (SPD-target old-SPD))                    ; step 1.
        (general-proposition (Skolem-table-prop table)) )
    (send-write-request general-proposition 'add-link :T-LINK     ; step 2.
                                            old-proposition
                                            *Skolem-proposition-weight*
                                            :priority #'max )
    (setf (SPD-target new-SPD) old-proposition) ))                ; step 3.



;;;;;;  ******  GENERATING SKOLEM PROPOSITIONS  ******
;;;
;;;  The last phase of the skolemization process constructs Skolem propositions.
;;;  A SKOLEM PROPOSITION is a proposition that parallels the general propo-
;;;  sition serving as a skolemization template but uses instance-arguments
;;;  instead of concept-arguments (see AMBR/SKOLEM.TXT for more details).
;;;
;;;  The entry point to this last phase is the function ENSURE-SKOLEM-
;;;  PROPOSITIONS.  It is called when all slot descriptors in the Skolem table
;;;  have at least one visible instance-argument.  (Implementationally, the
;;;  criterion is that IDENTIFY-CONCEPTS-WITH-NO-INSTANCES returns NIL.)
;;;  Usually there is one instance-argument for each S-slot of the proposition
;;;  but occasionally there may be two or more.  (This could happen when there
;;;  are several markers in the respective concept-argument.)  In the latter
;;;  case, a Skolem proposition is created for each argument combination.
;;;
;;;  To illustrate, suppose there are 3 slots in the general proposition.
;;;   Ex1: Each slot descriptor has only one instance:
;;;           ((:slot1 A) (:slot2 B) (:slot3 C))
;;;         Only one proposition will be constructed in this case -- P(A,B,C).
;;;
;;;   Ex2: Now suppose that slots 1 and 2 have two instances each:
;;;           ((:slot1 A1 A2) (:slot2 B1 B2) (:slot3 C))
;;;         This will give rise to four Skolem propositions:
;;;           P(A1,B1,C), P(A1,B2,C), P(A2,B1,C), and P(A2,B2,C).
;;;         These alternative propositions will compete for the place of their
;;;         parent according to the general rules of the constraint satisfaction
;;;         mechanism.  Such proliferation of propositions occurs very rarely
;;;         in practice.
;;;
;;;  The generation of Skolem propositions involves the following rough steps:
;;;   1. Formulation of SKOLEM-PROP-DESCRIPTORs for each argument combination.
;;;      (See AMBR/SKOLEM1.LSP for details on Skolem-prop-descriptors (SPDs).)
;;;   2. For each new SPD check whether it satisfies EQUAL-ARGS with some old
;;;      SPD from SKOLEM-TABLE-OLD-PROPS.  If such match is found, call
;;;      STRENGTHEN-NON-SKOLEM-PROPOSITION, transfer the SPD from OLD-PROPS to
;;;      NEW-PROPS in the Skolem table, and don't formulate NC-request.
;;;   3. When the check at step 2. fails for some new SPD, formulate a node-
;;;      construction request, recruit a node constructor, and send it the
;;;      request.  Each NC request contains 'mail' for the newly constructed
;;;      agent -- a SKOLEM-MESSAGE-2.
;;;   4. The newly constructed proposition handles the Skolem message, replies
;;;      to its parent and issues a marker to trigger the general mechanisms
;;;      for establishing new hypotheses, etc.
;;;   5. The parent general hypothesis receives the messages from the new
;;;      propositions and registers them in the Skolem table.  It also creates
;;;      some new links to tighten up Skolem instances to the Skolem propositn.
;;;   6. When all propositions planned at step 1. have either been matched at
;;;      step 2. or generated at step 3., the STATUS field of the Skolem table
;;;      is set to :DONE. This marks the end of the whole skolemization process.
;;;

(defun  ensure-Skolem-propositions (hypoth table)
  "Construct Skolem proposition(s) -- instance agent(s) of TYPE :RELATION."
  (declare (type mature-hypoth-agent hypoth)
           (type Skolem-table table)
           (values S-PROGN-return-type) )
  ;; Assumption: (identify-concepts-with-no-instances slot-descrs)  -->  NIL
  (s-progn hypoth
    (setf (Skolem-table-new-props table)                          ; step 1.
          (formulate-Skolem-prop-descriptors (Skolem-table-slots table)))
    (dolist (new-prop-descr (Skolem-table-new-props table))
      (let ((old-prop-descr (find-matching-SPD table new-prop-descr)))
        (if old-prop-descr                                        ; step 2.
            [strengthen-non-Skolem-proposition hypoth  table      ; step 2.
                                               old-prop-descr
                                               new-prop-descr]
            [send-NC-request hypoth                               ; step 3.
                     [formulate-Skolem-prop-NCR hypoth  table
                                                new-prop-descr]] )))
    (if (every #'SPD-target (Skolem-table-new-props table))       ; step 6.
        [finish-skolemization hypoth table]   ; all new SPDs matched by old SPDs
        nil )))       ; Steps 4-6 done through handling SKOLEM-MESSAGE-2's.


;; FORMULATE-SKOLEM-PROP-DESCRIPTORS takes a list of slot descriptors and
;; produces a list of proposition descriptors.  There is a prop-descr for each
;; combination of instance-arguments. (See the examples above but beware --
;; they use simplified representations.  The actual slot- and prop-descriptors
;; are more complex -- see AMBR/SKOLEM1.LSP.)
;; The slot-labels in the result are lexicographically ordered according to
;; the order of the initial slot descriptors.

(defun formulate-Skolem-prop-descriptors (Skolem-slot-descriptors)
  (declare (type list Skolem-slot-descriptors)
           (values list) )        ; of Skolem prop-descriptors
  #+:DUAL-DEBUG (assert (every #'SSD-instances Skolem-slot-descriptors))
  (let* ((broken-up-SSDs (mapcar #'break-up-SSD
                                 Skolem-slot-descriptors))
         (prop-descriptors (mapcar #'list
                                   (first broken-up-SSDs))) ) ; start FIRST
    (dolist (broken-up-SSD (rest broken-up-SSDs))             ; iterate REST
      (setq prop-descriptors
            (include-SSD-into-SPD broken-up-SSD prop-descriptors)))
    (mapcar #'(lambda (raw-prop-descriptor)
                #+:DUAL-DEBUG (assert (= (length Skolem-slot-descriptors)
                                         (length raw-prop-descriptor)))
                (make-Skolem-prop-descriptor NIL              ; null SPD-TARGET
                             (reverse raw-prop-descriptor)))  ; lexicogr order
            prop-descriptors) ))

(defun  break-up-SSD (Skolem-slot-descriptor)
  ;; (:slot1 #$cup nil '(#$c1 #$c2))  -->  ((:slot1 . #$c1) (:slot1 . #$c2))
  (let ((slot-label (SSD-label Skolem-slot-descriptor))
        (instances  (SSD-instances Skolem-slot-descriptor)) ) ; non-empty list
    (mapcar #'(lambda (instance) (cons slot-label instance))
            instances) ))

(defun  include-SSD-into-SPD (broken-up-SSD prop-descriptors)
  ;; Example:  SSD: ((:slot2 . b1) (:slot2 . b2))
  ;;           PDs: (((:slot1 . a1)) ((:slot1 . a2)))
  ;; --> (((:slot2 . b1) (:slot1 . a1)) ((:slot2 . b2) (:slot1 . a1))
  ;;      ((:slot2 . b1) (:slot1 . a2)) ((:slot2 . b2) (:slot1 . a2)))
  (flet ((PD->list-of-PDs (prop-descriptor)
           (mapcar #'(lambda (slot-subdescriptor)
                       (cons slot-subdescriptor prop-descriptor))
                   broken-up-SSD)))
    (mapcan #'PD->list-of-PDs
            prop-descriptors) ))

;; Test cases for FORMULATE-SKOLEM-PROP-DESCRIPTORS:
#|
(assert (equal (formulate-Skolem-prop-descriptors (list
                     (make-Skolem-slot-descriptor :slot1 nil nil '(inst1))
                     (make-Skolem-slot-descriptor :slot2 nil nil '(inst2)) ))
               (list (make-Skolem-prop-descriptor nil        ; null SPD-TARGET
                        '((:slot1 . inst1) (:slot2 . inst2)))) ))

(assert (equal (formulate-Skolem-prop-descriptors (list
                     (make-Skolem-slot-descriptor :slot1 nil nil '(a1 a2))
                     (make-Skolem-slot-descriptor :slot2 nil nil '(b1 b2))
                     (make-Skolem-slot-descriptor :slot3 nil nil '(c))     ))
               (mapcar #'(lambda (x) (make-Skolem-prop-descriptor nil x))
                       '(((:slot1 . a1) (:slot2 . b1) (:slot3 . c))
                         ((:slot1 . a1) (:slot2 . b2) (:slot3 . c))
                         ((:slot1 . a2) (:slot2 . b1) (:slot3 . c))
                         ((:slot1 . a2) (:slot2 . b2) (:slot3 . c)))) ))
|#


;; FORMULATE-SKOLEM-INST-NCR composes a node-construction request describing
;; a Skolem proposition.  (Compare with FORMULATE-SKOLEM-INST-NCR above.)
;;
;; The Skolem propositions are temporary instance agents and are distinguished
;; by the tag :CONJECTURE in their MODALITY slot.  They also have the tag
;; :RELATION in their TYPE slot as they are instances of relational concepts.
;; By convention, the names of Skolem propositions begin with a '*' (e.g.
;; *COLOR-OF-23), like the names of Skolem instances.
;;
;; Note the 'mail' for the new agent -- a SKOLEM-MESSAGE-2 giving it the name
;; of the general hypothesis that has created it and the situation-agent to
;; affiliate to. Note also that the creator and the mentor are not the same.

(defun formulate-Skolem-prop-NCR (parent-hypoth table prop-descriptor)
  "Formulates a NC-request for a Skolem instance of CONCEPT."
  (declare (type corr-agent parent-hypoth)
           (type Skolem-table table)
           (values node-construction-request) )
  (let* ((general-prop (Skolem-table-prop table))         ; e.g SNDROP-IS-WHITE
         (relat-concept (instance->concept general-prop)) ; e.g COLOR-OF
         (situation (Skolem-table-sit table))
         (mentor (make-conref (make-symref general-prop :t-link)
                                           *Skolem-proposition-weight*))
         (INST-OF-filler (make-conref relat-concept *Skolem-INST-OF-weight*))
         (raw-S-slot-list (SPD-slots prop-descriptor))  ; ((:slot1 . #$arg)...)
         (TYPE-facets    (mapcar #'(lambda (raw)           ;  cAr     cDr
                                     (list (car raw) :TYPE :aspect))
                                 raw-S-slot-list))
         (INST-OF-facets (mapcar #'(lambda (raw)
                                     (list (cAr raw) :INST-OF
                                           (make-conref
                                                 (make-symref relat-concept
                                                              (cAr raw))
                                                 *Skolem-C-COREF-weight*)))
                                 raw-S-slot-list))
         (C-COREF-facets (mapcar #'(lambda (raw)
                                     (list (cAr raw) :C-COREF
                                           (make-conref (cDr raw)
                                                 *Skolem-C-COREF-weight*)))
                                 raw-S-slot-list))
         (all-S-slots-and-their-facets (nconc TYPE-facets
                                              INST-OF-facets
                                              C-COREF-facets)) )
    (make-NC-request
          mentor                                                  ; mentor
          'temp-instance-agent                                    ; agent-type
          (Skolem-inst-GENNAME-template relat-concept)
          (list* (list :TYPE '(:temporary :instance :relation))   ; slots
                 (list :MODALITY  :conjecture)
                 (list :INST-OF   INST-OF-filler)
                 all-S-slots-and-their-facets )
          (list (make-Skolem-message-2 parent-hypoth              ; mail
                                       situation
                                       relat-concept)) )))


;;;;;; Handling Skolem messages of type 2.
;;
;;  This subsection deals with the 'registration' of the new Skolem propositns.
;;  They receive 'mail' from their parent general hypothesis in the form of
;;  a SKOLEM-MESSAGE-2.  Upon receipt of such message, the new Skolem propositn
;;  does four things:
;;   1. reply to parent
;;   2. affiliate to the situation mentioned in the message  (AMBR/KREPRES.LSP)
;;   3. emit a T-marker (see AMBR/MARKER.LSP)
;;   4. Create C-COREF links from the instance-arguments to the new proposition.
;;       (See the function PROPOSITION-ARGS in AMBR/KREPRES.LSP.)

(defmethod handle-symbol ((Skolem-proposition temp-instance-agent)
                          (message Skolem-message-2) )
  (declare (values S-PROGN-return-type))
  (let ((parent-hypothesis (Skolem-message-sender message))
        (situation (Skolem-message-situation message))
        (concept   (Skolem-message-concept message)) )
    #+:DUAL-DEBUG (assert (eq concept (instance->concept Skolem-proposition)))
    (s-progn Skolem-proposition
       [receive-symbol parent-hypothesis                            ; step 1.
          [make-Skolem-message-2 Skolem-proposition situation concept]]
       [affiliate-agent Skolem-proposition                          ; step 2.
                        situation *Skolem-SITUATION-weight*]
       [send-T-marker Skolem-proposition concept]                   ; step 3.
       (dolist (pair (proposition-args Skolem-proposition))         ; step 4.
         (let ((instance-arg (cDr pair))              ; vvv-- S-label
               (symref (make-symref Skolem-proposition (cAr pair))) )
           [send-write-request instance-arg
                               'add-link :c-coref                   ; step 4.
                               symref *Skolem-C-COREF-weight*])) )))


;; The parent general hypoth receives the message from the Skolem propositn and:
;;  1. Check whether the Skolem proposition is still alive.  If it has died
;;      at birth, check how many proposition descriptions are kept in the Skolem
;;      table.  If there is only one, set the STATUS field of the table to
;;      :ABORT and stop.  If there are several prop-descriptions, leave the
;;      STATUS as is (i.e. :WAIT) and stop.  (In this latter case, other Skolem
;;      messages may arrive, potentially with living props. Note, however, that
;;      FINISH-SKOLEMIZATION will never be invoked if there are dead props.)
;;  2. If the Skolem proposition is visible, register it in the Skolem table.
;;  3. Check whether this is the last Skolem proposition to wait for.  If the
;;      TARGET fields (see AMBR/SKOLEM1.LSP) of all proposition-descriptors in
;;      the Skolem table are non-NIL, proceed with FINISH-SKOLEMIZATION.
;;      Otherwise go to sleep in wait for more Skolem-message-2's.

(defmethod handle-symbol ((hypoth hypoth-agent)
                          (message Skolem-message-2) )
  (declare (values S-PROGN-return-type))
  (let* ((proposition (Skolem-message-sender message))
         (situation   (Skolem-message-situation message))
         (table (find-Skolem-table hypoth situation))
         (prop-descriptors (Skolem-table-new-props table)) )
    (s-progn hypoth
      (cond ([agent-visible-p proposition]                            ; step 1.
               (register-Skolem-proposition proposition               ; step 2.
                                            prop-descriptors)   ; destructively
               (when (every #'SPD-target prop-descriptors)            ; step 3.
                 [finish-skolemization hypoth table]) )               ; step 3.
            ((= 1 (length prop-descriptors))                          ; step 1.
               (setf (Skolem-table-status table) :ABORT)) ))))        ; step 1.
            ;; else PROPOSITION is dead but there may be others       ; step 1.

(defun register-Skolem-proposition (proposition prop-descriptors)
  "Destructively write PROPOSITION in the TARGET field of the relevant SPD."
  (declare (type instance-agent proposition)
           (type list prop-descriptors)          ; of Skolem prop-descriptors
           (values list) )                       ; EQ to PROP-DESCRIPTORS
  (let ((args (proposition-args proposition)))   ; ((:slot1 . #$arg1) ...)
    (dolist (SPD prop-descriptors (cerror "Leave it unregistered and continue."
                             "Cannot register the Skolem proposition ~S in ~S."
                                    proposition prop-descriptors))
      (when (every #'(lambda (pair)              ; matching SPD ?
                       (equal pair (assoc (cAr pair) (SPD-slots SPD))) )
                   args)
        (setf (SPD-target SPD) proposition)      ; register destructively
        (return)))                               ; and break the loop
    prop-descriptors ))                          ; return modified list



;;;;;;  ******  FINALIZING SKOLEMIZATION  ******
;;;
;;; In the current version [2.2.2] of the model, FINISH-SKOLEMIZATION does
;;; nothing but setting the STATUS field of the Skolem table to :DONE.
;;; Future versions may alter this behavior.
;;; Compare with FINISH-RATING in AMBR/RATING.LSP.
;;;
;;; Note that the Skolem table is not removed from the buffer.  This is to
;;; prevent repeated skolemizations -- see step 5a of HANDLE-SKOLEM-INCENTIVE.

(defun  finish-skolemization (general-hypothesis Skolem-table)
  "Stops the skolemization mechanism because all Skolem propositions are ready."
  (declare (ignore general-hypothesis))
  (setf (Skolem-table-status Skolem-table) :DONE) )


;;;;;;;  End of file AMBR/SKOLEM2.LSP
