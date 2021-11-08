;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/krepres.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP and AMBR/INTRFACE/KREPRES.LSP
;;; PURPOSE:    Access functions for AMBR knowledge representation.
;;; DEPENDS-ON: DUAL; ambr/defs.lsp, ambr/proclaim.lsp, ambr/ambr_ag.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    07-05-98 [2.2.2]  Subsume old AMBR/UTILS.LSP and add new funs.
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;    KNOWLEDGE  REPRESENTATION  UTILITIES    ;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  "AMBR-CORE")


;;;; SYMBOLS: agent-type, agent-modality, homogeneous-states-p ;
;;;;          agent-situation, affiliate-agent ;
;;;;          instance->concept, get-slot-superordinates ;
;;;;          proposition-args, equal-args, argument-props ;
;;;;          general-proposition-p, range-slot-p



;; AGENT-TYPE  (agent yes-tags &optional no-tags)  -->  T or NIL
;;
;;   A function that checks the TYPE slot of AGENT and looks for particular
;;   tag or tags.
;;
;;   AGENT should be an AMBR agent (in fact, micro-frame is enough).
;;   YES-TAGS and NO-TAGS should be either tags or lists of tags.  An isolated
;;     tag is considered shorthand for the respective one-element list.
;;     When not supplied, NO-TAGS defaults to the empty list.
;;
;;   The function returns T if and only if all of YES-TAGS and none of NO-TAGS
;;     are among the fillers of the TYPE slot of AGENT.
;;
;;   For example, suppose the TYPE slot of AGENT is filled by the following
;;   list of tags:  (:instance :relation :temporary).  Then:
;;     (agent-type agent :temporary)              -->   T      ; temporary ?
;;     (agent-type agent :concept)                -->  NIL     ; concept ?
;;     (agent-type agent '(:instance :relation))  -->   T      ; proposition ?
;;     (agent-type agent :instance :relation)     -->  NIL     ; non-rel inst ?
;;     (agent-type agent nil :hypoth)             -->   T      ; non-hypoth ?
;;     (agent-type whatever-agent nil)            -->   T
;;
;;   The function CORRESP-TYPE (see AMBR/CORRESP.LSP) is a conjunction of 
;;   two AGENT-TYPEs.
;;

;; AGENT-MODALITY  (agent yes-tags &optional no-tags)  -->  T or NIL
;;
;;   A function that checks the MODALITY slot of AGENT and looks for particular
;;   tag or tags.
;;
;;   AGENT, YES-TAGS, and NO-TAGS have the same interpretation as in AGENT-TYPE.
;;
;;   AGENT-MODALITY works according to the following algorithm:
;;     1. It retrieves the filler of AGENT's MODALITY slot (via GET-FILLER).
;;     2. If the filler is NIL (or the whole slot is missing), the function
;;          uses the one-element set '(:TRUE) by default; otherwise it uses
;;          the filler 'as is'.
;;     3. The set from step 2. is compared against YES-TAGS and NO-TAGS -- the
;;          function returns T if and only if all of YES-TAGS and none of
;;          members of this set.
;;
;;   For example, suppose the MODALITY slot of AGENT1 is empty while that of
;;   AGENT2 is filled by the list  (:conjecture :false).  Then:
;;     (agent-type agent1 :true)                  -->   T
;;     (agent-type agent1 :goal)                  -->  NIL
;;     (agent-type agent1 :true :goal)            -->   T
;;
;;     (agent-type agent2 :true)                  -->  NIL
;;     (agent-type agent2 '(:conjecture :false))  -->   T
;;     (agent-type agent2 :conjecture :false)     -->  NIL
;;     (agent-type agent2 nil :goal)              -->   T
;;

;; HOMOGENEOUS-STATES-P (instance1 instance2)  -->  T or NIL
;;
;; ...  from the MODALITY slots ...
;;   :init   + :init   --> T
;;   :goal   + :goal   --> T
;;   :result + :result --> T
;;   :goal   + :result --> T
;;   :init   + :goal   --> nil
;;   :init   + :result --> nil
;;
;; Used to determine *CSx-MENTOR-HOMOG-WEIGHT* vs. *CSx-MENTOR-HETEROG-WEIGHT*.
;; ...


;; AGENT-SITUATION  (instance-agent)  -->  situation-agent  or  NIL
;;
;;   A generic function that determines the affiliation of AGENT.
;;   See AMBR/SITUATN.TXT for docum. about situation-agents and affiliation.
;;
;;   INSTANCE-AGENT should be an instance-agent (see AMBR/AMBR_AG.LSP).
;;    If it isn't, an error is signaled.
;;
;;   The function retrieves the filler of the SITUATION slot of INSTANCE-AGENT
;;   and acts accordingly:
;;    - When the slot is filled by a single reference, AGENT-SITUATION returns
;;      this reference.  (The weight of the reference is discarded. That is,
;;      AGENT-SITUATION returns a 'symref' rather than 'conref'.)
;;    - When the slot is filled by a list of two or more references, AGENT-
;;      SITUATION signals a continuable error as each agent may be affiliated
;;      to at most one situation (see AMBR/SITUATN.TXT).  If continued from
;;      the error, AGENT-SITUATION returns the first reference from the list.
;;    - When the slot is empty or missing, AGENT-TYPE checks whether AGENT
;;      satisfies the predicate  (agent-type agent :situation).  If it does,
;;      AGENT-TYPE returns the agent itself -- it is implicitly self-affiliated.
;;      Otherwise the function returns NIL -- AGENT is a free-standing instance.
;;

;; AFFILIATE-AGENT  (instance-agent situation-agent weight)  -->
;;                                            -->  situation-agent  or  NIL
;;
;;   A generic function that affiliates an instance agent to a given situation
;;   by establishing a SITUATION link from the instance to the situation.
;;   See AMBR/SITUATN.TXT for docum. about situation-agents and affiliation.
;;
;;   INSTANCE-AGENT should be an instance-agent (see AMBR/AMBR_AG.LSP).  If
;;    it isn't, an error is signalled.
;;   SITUATION-AGENT should be a 'situation agent', i.e. an instance-agent
;;    having the tag :SITUATION in its TYPE slot (see AMBR/SITUATN.TXT).
;;    If it isn't, an error is signaled.
;;   WEIGHT should be a number suitable for passing to ADD-LINK (see DUAL/
;;    ARCHIT/LINKS.LSP).  If it isn't, an error is signaled.
;;
;;   If INSTANCE-AGENT is equal (EQ) to SITUATION-AGENT, no link is created
;;    and AFFILIATE-AGENT returns NIL.  (This is the only case in which the
;;    function returns NIL.)  Situation agents are implicitly affiliated to
;;    themselves -- see AGENT-SITUATION above.
;;   If INSTANCE-AGENT is already affiliated to SITUATION-AGENT, the function
;;    updates the WEIGHT of the link and returns SITUATION-AGENT.
;;   If INSTANCE-AGENT is affiliated to some other situation, a continuable
;;    error is signaled.  If continued from the error, AFFILIATE-AGENT first
;;    destroys the old affiliation and then establishes the new one.
;;
;;   There is a :AFTER method defined in AMBR/INTRFACE/KREPRES.LSP that adds
;;    the new member to the appropriate coalition-object (in the interface).
;;
;;   It is always true that (providing AFFILIATE-AGENT returns without errors):
;;    (progn (affiliate-agent instance situation weight)
;;           (agent-situation instance) )                 -->  situation


;; INSTANCE->CONCEPT  (instance &key cerror-p)  -->  concept or NIL
;;
;;   A function that reads the INST-OF slot of the instance-agent INSTANCE and
;;   retrieves the respective concept-agent.
;;
;;   INSTANCE should be an [instance] agent.
;;   CERROR-P is a flag controling whether the function invokes CERROR on
;;     illegal situations.  When not supplied, CERROR-P defaults to T.
;;
;;   AMBR's specification (section 4.2.2. in DR#1) postulates that each
;;   instance-agent must have an INST-OF slot and this slot must be filled by
;;   a reference to the 'parent' concept of the instance in question.
;;   According to that specification, INSTANCE->CONCEPT retrieves the filler
;;   of the INST-OF slot of INSTANCE and checks how many element it has.
;;    - If there is only one element (the normal case), it is returned.
;;    - If there are two or more elements and CERROR-P is T, a continuable
;;       error is signaled. If the user returns from the error or CERROR-P is
;;       NIL, the function returns the first element from the filler list.
;;    - If the INST-OF slot is empty or missing and CERROR-P is T, a continuable
;;       error is signaled. If the user returns from the error or CERROR-P is
;;       NIL, INSTANCE-CONCEPT returns NIL. This is the only case when it
;;       returns something that is not an agent.
;;
;;   Examples:
;;    (instance->concept #$water-3)          -->  #$water
;;    (instance->concept #$bottle-md-glass)  -->  #$made-of
;;    (instance->concept #$water)            -->  cerror, if continued -- NIL
;;    (instance->concept #$water :cerror-p nil)  -->  NIL
;;

;; GET-SLOT-SUPERORDINATES  (agent S-label)  -->  symref-list
;;
;;  ...


;; PROPOSITION-ARGS  (agent)  -->  a-list
;;
;;   ... Returns a freshly consed a-list keyed by S-labels.
;;
;;   Example:
;;   (proposition-args #$in-1)  -->  ((:slot1 . #$water-1) (:slot2 . #$cup-1))

;; EQUAL-ARGS (prop-args1 prop-args2)  -->  T or NIL
;;
;;   Equality test for the lists produced by PROPOSITION-ARGS.
;;
;;   Examples:
;;    (equal-args ((:slot1 . #$water-1) (:slot2 . #$cup-1))
;;                ((:slot2 . #$cup-1) (:slot1 . #$water-1)))   -->  T
;;    (equal-args ((:slot1 . #$foo) (:slot2 . #$foo))          --> NIL
;;

;; ARGUMENT-PROPS  (agent)  -->  list-of-symrefs
;;
;;   A function that retrieves the propositions in which AGENT is an argument,
;;   if any.
;;   ...
;;
;;   Example:
;;   (operand-propositions #$water-1)  -->  ((#$in-1 . :slot1))



;; RANGE-SLOT-P  (S-slot)  -->  T or NIL
;;
;;   ...  See AMBR/SKOLEM.TXT for 'range' vs. 'value' interpretation of S-slots.
;;

;; GENERAL-PROPOSITION-P  (proposition)  -->  T or NIL
;;
;;   ...  See AMBR/SKOLEM.TXT
;;
;;   Does PROPOSITION have at least one S-slot that satisfies RANGE-SLOT-P ?


;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric agent-situation  (instance-agent)
 (:documentation "The affiliation of the instance to some situation, if any."))
 ;; cf. AGENT-HYPOTHESES in AMBR/SECRETAR and AGENT-MARKER-COLOR in =/MARKER.LSP

(defgeneric affiliate-agent  (instance-agent situation-agent weight)
 (:documentation
   "Create a SITUATION link from INSTANCE-AGENT to SITUATION-AGENT." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

;;;;  AGENT-TAG and AGENT-MODALITY
;;
(defun  check-tag-sets  (main-set yes-tags no-tags)
  "Auxiliary function for AGENT-TYPE and AGENT-MODALITY."
  (declare (type list main-set)
           (type (or symbol list) yes-tags no-tags)
           (values boolean) )
  (let ((yes-tag-list (if (listp yes-tags) yes-tags (list yes-tags)))
        (no-tag-list  (if (listp no-tags)  no-tags  (list no-tags ))) )
    (and (subsetp yes-tag-list main-set)
         (null (intersection no-tag-list main-set))
         T )))

(defun agent-type (agent yes-tags &optional no-tags)
  "Check the TYPE slot for presence or absence of particular tag(s)."
  (declare (type (or symbol list) yes-tags no-tags)
           (values boolean))
  (check-tag-sets (get-filler agent :TYPE)
                  yes-tags
                  no-tags ))

(defun agent-modality (agent yes-tags &optional no-tags)
  "Check the MODALITY slot for presence or absence of particular tag(s)."
  (declare (type (or symbol list) yes-tags no-tags)
           (values boolean))
  (let ((defaulted-MODALITY-filler
           (cond ((get-filler agent :modality))     ; when nonNIL, use it as is
                 (t  '(:true)) )))                  ; default when missing slot
    (check-tag-sets defaulted-MODALITY-filler
                    yes-tags
                    no-tags )))

(defun  homogeneous-states-p (instance1 instance2)
  "True on INIT<-->INIT, GOAL<-->GOAL, RESULT<-->RESULT, and GOAL<-->RESULT."
  (declare (type instance-agent instance1 instance2)
           (values boolean) )
  (let ((modality1 (get-filler instance1 :modality))
        (modality2 (get-filler instance2 :modality)) )
    (flet ((init-p (tags) (member :init tags))
           (end-p  (tags) (or (member :goal   tags)
                              (member :result tags)) ))
      (cond ((and (init-p modality1) 
                  (init-p modality2))  T)    ;     init<-->init
            ((and (end-p  modality1) 
                  (end-p  modality2))  T)    ; goal/res<-->goal/res
            (t  NIL) ))))


;;;;  AGENT-SITUATION and AFFILIATE-AGENT
;;
(defmethod agent-situation ((agent instance-agent))
  (declare (values (or null instance-agent)))  ; symrefs should not happen
  (let ((SITUATION-filler (get-filler-refs! agent :situation)))
    (case (length SITUATION-filler)
      (0  ; SITUATION slot empty
          (if (agent-type agent :situation)    ; situation-agent ?
              agent                            ; implicitly affiliated to self
              nil))                            ; free-standing instance
      (1  (first SITUATION-filler))            ; explicitly affiliated
      (t  (cerror "Use the first reference from the list and continue."
                  "The SITUATION slot of ~S contains two or more refs: ~S."
                  agent SITUATION-filler)
          (first SITUATION-filler)) )))

(defmethod agent-situation ((x t))
  (error "AGENT-SITUATION: ~S is not an instance-agent." x ))


(defmethod affiliate-agent ((instance  instance-agent)
                            (situation instance-agent)
                            (weight    number) )
  (unless (agent-type situation :SITUATION)
    (error "AFFILIATE-AGENT:  ~S is not a situation agent."
           situation))
  (if (eq instance situation)
      nil                   ; implicit self-affiliation
      (let ((old-affiliation (agent-situation instance)))
        (cond ((null old-affiliation)
                  (add-link instance :SITUATION situation weight))
              ((eq old-affiliation situation)
                  (add-link instance :SITUATION situation weight
                            :priority :new))              ; modify-link-weight
              (t  (cerror "Replace the old affiliation and continue."
                      "Affiliation conflict in ~S -- old situation ~S, new ~S."
                      instance old-affiliation situation)
                  (remove-slot instance :SITUATION  :notify-p NIL) ; remove old
                  (add-link instance :SITUATION situation weight)) )
        situation)))

(defmethod affiliate-agent ((x t) (y t) (z t))
  (error
    "AFFILIATE-AGENT: ~S or ~S is not an instance-agent or ~S is not a number."
    x y z ))


;;;;  RETRIEVING SUPERORDINATES
;;
(defun instance->concept (instance &key (cerror-p t))
  "Reads the INST-OF slot of an instance to retrieve the corresponding concept."
  (declare (type instance-agent instance)
           (values (or null concept-agent)) )  ; symrefs should not happen
  (let ((INST-OF-filler (get-filler-refs! instance :inst-of)))
    (case (length INST-OF-filler)
      (0  (when cerror-p
            (cerror "Return NIL and continue."
               "INSTANCE->CONCEPT: The INST-OF slot of ~S is empty or missing."
               instance))
          nil)
      (1  (first INST-OF-filler))
      (t  (when cerror-p
            (cerror "Use the first reference from the list and continue."
                    "The INST-OF slot of ~S contains two or more refs: ~S."
                    instance INST-OF-filler))
          (first INST-OF-filler)) )))

(defun get-slot-superordinates (mframe S-label)
  "Gets the filler of the INST-OF or SUBC facet of a given S-slot."
  (declare (type micro-frame mframe)   ; see DUAL/ARCHIT/MCRFRAME.LSP
           (type symbol S-label)
           (values list) )             ; list of symrefs or NIL
  (let ((S-slot (locate-mfr-component mframe S-label)))
    (if (null S-slot)
        (cerror "Return NIL and continue."
                "GET-SLOT-SUPERORDINATE: ~S has no S-slot labeled ~S."
                mframe S-label)
        (let ((INST-OF-filler (get-filler-refs! S-slot :INST-OF))
              (SUBC-filler    (get-filler-refs! S-slot :SUBC   )) )
          (cond ((and INST-OF-filler SUBC-filler)      ; both non-empty
                    (cerror "Use the INST-OF facet and continue."
                            "~S.~A has both INST-OF and SUBC facets."
                            mframe S-label)
                    INST-OF-filler )
                (SUBC-filler   )            ; SUBC non-empty, return its filler
                (INST-OF-filler)            ; INST-OF non-empty
                (t nil) )))))               ; both empty, return NIL



;;;;;  PROPOSITIONS AND THEIR ARGUMENTS
;;
;;  See AMBR/SKOLEM.TXT for a definition of general propositions,
;;  'range' and 'value' slots, etc.  See also AMBR/SKOLEM.LSP.

(defun  proposition-args (mframe)
  "Gather the C-COREF fillers of the S-slots into an a-list keyed by S-label."
  ;; E.g. (proposition-args #$in-1) --> ((:slot1 . #$water-1) (:slot2 . #$cup-1))
  (declare (type micro-frame mframe)
           (values list) )             ; freshly consed a-list
  #+:DUAL-DEBUG (assert (agent-type mframe :relation))
  (flet ((do-one (S-slot)
           (let ((filler-list (get-filler-refs! S-slot :c-coref)))
             (cons (slot-label S-slot)
                   (find-if #'simple-symref-p filler-list)) )))
    (mapcar #'do-one (agent-S-slots mframe)) ))

(defun  equal-args  (prop-args1 prop-args2)
  "Equality test for the lists produced by PROPOSITION-ARGS."
  (declare (type list prop-args1 prop-args2)   ; a-lists keyed by S-labels
           (values boolean) )
  (and (= (length prop-args1) (length prop-args2))
       (every #'(lambda (pa1)
                  (and (consp pa1)       ; e.g. (:slot2 . #$cup-1)
                       (equal pa1
                              (assoc (cAr pa1) prop-args2))) )
              prop-args1)
       T ))


(defun  argument-props (agent)
  "List all propositions in which AGENT is an argument."
  (declare (values list))   ; of extended symref's
  (let ((result nil))
    (dolist (symref (get-filler-refs! agent :c-coref))
      (when (and (extended-symref-p symref)    ; e.g. (#$in-1 . :slot1)
                 (agent-type (symref-agent symref) '(:instance :relation)))
        (push symref result)))
    (nreverse result) ))

(defun  range-slot-p  (S-slot)
  "Does S-SLOT have a SUBC facet?"
  (declare  (type S-slot S-slot)
            (values boolean) )
  (let ((SUBC-facet    (locate-mfr-component S-slot :subc))
        (INST-OF-facet (locate-mfr-component S-slot :inst-of)) )
    (cond ((and SUBC-facet INST-OF-facet)
              (cerror "Ignore the SUBC facet and continue."
                      "~S.~(~A~) has both SUBC and INST-OF facets."
                      (slot-owner S-slot) (slot-label S-slot) )
              nil )                   ; 'value' interpretation
          (INST-OF-facet  nil)        ; 'value' interpretation
          (SUBC-facet      T )        ; 'range' interpretation
          (t               T ) )))    ; 'range' interpretation (by default)


(defun  general-proposition-p  (proposition)
  "Does PROPOSITION have at least one S-slot that satisfies RANGE-SLOT-P ?"
  (declare (type instance-agent proposition))
  (and (agent-type proposition '(:instance :relation))    ; proposition ?
       (some #'range-slot-p                               ; general ?
             (agent-S-slots proposition)) ))


;;;;;;; End of file AMBR/KREPRES.LSP
