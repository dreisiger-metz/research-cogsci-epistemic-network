;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/semantic/abstract.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Semantic memory -- abstract concepts such as OBJECT, SITUATION.
;;; DEPENDS-ON: ambr/packages.lsp, ambr/intrface/defagent.lsp, =/kb_util.lsp
;;; XREFS:      none, this is the first KB file
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    14-04-98 [3.0.0]  Used parts of old LTM.LSP, of course.
;;; UPDATED:    01-06-98  Added TO-REACH, IS-GOAL, RESULT, CAUSE-LIKE-REL, etc.
;;; UPDATED:    ...


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;      A B S T R A C T    C O N C E P T S      ;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package  "AMBR")

;;;; This file defines the most abstract concepts used in AMBR's semantic
;;;; memory.  These concepts stay at the top of the IS-A hierarchy.
;;;;
;;;; Normally, this file is loaded prior to any other knowledge-base file.


;;; WARNING!!!   The Eliza effect  can be dangerous  to your   WARNING!!!
;;; WARNING!!!   comprehension of cognitive modeling. Please   WARNING!!!
;;; WARNING!!!   examine  all symbols carefully  before use.   WARNING!!!
;;;
;;; With full awareness that AMBR agents are nothing but ungrounded symbols,
;;; we follow the common AI terminology and say that they stand for 'things
;;; in the world'.  We also use mnemonic agent names like FIRE, CAUSE, etc.
;;; Those names are irrelevant for the model itself; the program would work
;;; just as well (or as bad) had the agents been named AG001, AG002, etc.
;;; Indeed, the first version of AMBR (Kokinov, 1994) used such void names.
;;; It was very instructive from a philosophical point of view as it laid
;;; bare how little 'knowledge' the program actually had.  It was not very
;;; practical, however, because it hindered enourmously the process of
;;; developing, tuning, testing, and documenting the model.     [DR#1, p.67]


;;;;;;;;  -----  Check for ruins from old runs  -----  ;;;;;;;
;;
;;  This file defines the basic concepts for the whole knowledge base.
;;  It is designed to be loaded first. Therefore, there should be no
;;  other agents at this point.  The function CHECK-FOR-OLD-AGENTS is
;;  a tool that checks precisely that (see DUAL/INTRFACE/KB_UTIL.LSP).

(check-for-old-agents)   ; begin from scratch


;;;;;;;;   ********  *OTHER*  AGENTS  *********
;;
;; Any knowledge base of realistic (or even plausible)
;; size will undoubtedly contain millions of agents.
;;
;; The knowledge base of AMBR currently is far from this
;; size.  There is a vast number of agents that should
;; be in the LTM but are not defined.  The special agent
;; named *OTHER* stands for them all.  It acts like an
;; absolutely black absorber of activation.  Thus, a
;; link to *OTHER* represents a link to some LTM agent
;; that never passes the WM threshold in our experiments.
;;
;; For example, BIG-OBJECT may have associative links
;; to HEOPSE-PYRAMID, NIAGARA-FALLS, and JUMBO-JET.
;; None of the latter agents is defined here.  All of
;; them are represented by a single agent (*OTHER*)
;; that never gets into the working memory.  The many
;; links going out of BIG-OBJECT are represented by
;; a single link pointing to *OTHER*.  Without such
;; link, all activation of BIG-OBJECT will pour into
;; the few agents that are defined which is both
;; unrealistic and harmful.  (Harmful because it spoils
;; the energetic balance -- the network tends to become
;; too active because little activation dissipates into
;; the 'dark regions' of the LTM.)

(defagent   *other*    special-AMBR-agent
  "Stands for any of the inactive agents."
  :type     :special
)



;;;;   ********   SITUATIONS  AND  STATES   **********
;;
;; States (or scenes) are relatively small -- two or
;; three objects and a few relations between them.
;;
;; Situations are bigger and typically consist of
;; states.  The name 'situation' is used in a generic
;; sense -- AMBR situations cover also events, problems,
;; and other dynamic sequences.
;;
;; Situations are distinguished by the tag :SITUATION
;; in their TYPE slot.  This tag plays a role in the
;; mechanisms of the model.  Many instance agents are
;; 'affiliated' to a situation -- see AMBR/SITUATN.TXT
;; for more details on affiliation.
;;
;; Note that AMBR uses decentralized representations
;; and, therefore, situation-agents do not represent
;; situations as wholes.  Rather they serve as a marker
;; to the spatio-temporal contiguity of a set of things.
;; (See section 4.2.4.2 in "DUAL Report #1" and the
;;  comments in AMBR/SITUATN.TXT.)
;;

(defagent   situation    concept-agent
  :type     (:concept  :situation)
;;:subc     nil  -- this is the top
  :superc   (state
             problem
             episode )
  :a-link   (*other* 20.0)
  :slot1  "Aspect(s)"
    :type   :aspect
   ;:subc   nil
  :slot2  "Relation(s)"
    :type   :relation
   ;:subc   nil
)

(defagent   state        concept-agent
  :type     :concept
  :subc     situation
  :superc   (init-state
             inter-state
             end-state
             goal-state )
  :a-link   (*other* 2.0)
  :slot1  "Aspect(s)"
    :type   :aspect
    :subc   (situation . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (situation . :slot2)
)

(defagent   init-state   concept-agent
  :type     :concept
  :subc     state
  :c-coref  ((problem-description . :slot1) 0.5)
  :a-link   ((end-state   1.0)
             (inter-state 0.5)
             (to-reach    0.2)
             (*other*     2.0) )
  :slot1
    :type   :aspect
    :subc   (state . :slot1)
  :slot2
    :type   :relation
    :subc   (state . :slot2)
)

(defagent   end-state    concept-agent
  :type     :concept
  :subc     state
  :c-coref  ((problem-solution . :slot3) 0.5)
  :a-link   ((init-state  1.0)
             (goal-state  0.8)
             (result      1.0)
             (*other*     1.0) )
  :slot1
    :type   :aspect
    :subc   (state . :slot1)
  :slot2
    :type   :relation
    :subc   (state . :slot2)
)

(defagent   inter-state  concept-agent
  :type     :concept
  :subc     state
  :c-coref  ((problem-solution . :slot2) 0.2)
  :a-link   ((init-state  0.5)
             (end-state   0.5)
             (*other*     3.0) )
  :slot1
    :type   :aspect
    :subc   (state . :slot1)
  :slot2
    :type   :relation
    :subc   (state . :slot2)
)

(defagent   goal-state   concept-agent
  :type     :concept
  :subc     state
  :c-coref  ((problem-description . :slot2) 0.5)
  :a-link   ((init-state 0.5)
             (end-state  0.8)
             (is-goal    1.0)
             (to-reach   0.5)
             (*other*    1.0) )
  :slot1
    :type   :aspect
    :subc   (state . :slot1)
  :slot2
    :type   :relation
    :subc   (state . :slot2)
)


;;;;  ******   PROBLEMS AND SOLUTIONS   *******
;;
;; Note that problems have :SITUATION tags.

(defagent   problem     concept-agent
  :type     (:concept :situation)
  :subc     situation   ; generic
  :a-link   (*other* 2.0)
  :slot1  "Problem description"
    :type     :aspect
    :subc     (situation . :slot1)
    :c-coref  problem-description     ; range
  :slot2  "Problem solution"
    :type     :aspect      ; --vvv--
    :subc     (situation . :slot1)
    :c-coref  problem-solution        ; range
)

(defagent   problem-description  concept-agent
  :type     :concept
  :subc     situation   ; generic
  :c-coref  (problem . :slot1)
  :a-link   ((problem-solution 1.0)
             (*other* 2.0) )
  :slot1  "Initial state"
    :type     :aspect
    :subc     (situation . :slot1)
    :c-coref  init-state              ; range
  :slot2  "Goal state"
    :type     :aspect      ; --vvv--
    :subc     (situation . :slot1)
    :c-coref  goal-state              ; range
)

(defagent   problem-solution   concept-agent
  "May be successful or not depending on END-STATE."
  :type     :concept
  :subc     situation   ; generic
  :c-coref  (problem . :slot2)
  :a-link   ((problem-description 1.0)
             (*other* 2.0) )
  :slot1  "Actions taken"
    :type     :aspect
    :subc     (situation . :slot1)
    :c-coref  event                   ; range
  :slot2  "Intermediary states, if any"
    :type     :aspect
    :subc     (situation . :slot1)
    :c-coref  inter-state             ; range
  :slot3  "Result of the actions"
    :type     :aspect
    :subc     (situation . :slot1)
    :c-coref  end-state               ; range
)


;;;;  *******   EPISODES AND EVENTS   *******
;;
;; An event has a relatively small duration.
;; Episodes are bigger and typically consist of
;; events.
;; (Thus  episode:event::situation:state .)
;;
;; Note that episodes have :SITUATION tags.

;; The sequencing of events is usually repre-
;; sented by the temporal relation FOLLOWS
;; which is defined in AMBR/KB/TEMPORAL.LSP.

(defagent   episode    concept-agent
  :type     (:concept  :situation)
  :subc     situation   ; generic
  :superc   event
  :a-link   (*other* 3.0)
  :slot1  "Aspect(s)"
    :type   :aspect
    :subc   (situation . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (situation . :slot2)
)

(defagent   event      concept-agent
  :type     :concept
  :subc     episode    ; generic
  :c-coref  (problem-solution . :slot1)
  :a-link   (*other* 2.0)
  :slot1  "Aspect(s)"
    :type   :aspect
    :subc   (episode . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (episode . :slot2)
)


;;;;   ********   T H I N G S   **********
;;
;; Things of all kinds (concepts, instances,
;; abstract, concrete, solid, liquid, etc.)
;; have the tag :OBJECT in their TYPE slots.
;;
;; This tag is not used by the current (ver.
;; 2.2.1) mechanisms of the model.  By
;; contrast, :SITUATION and :RELATION are --
;; see e.g. AMBR/STR_CORR.LSP.

(defagent   thing    concept-agent
  "The superclass of all objects, liquids, etc."
  :type     (:concept  :object)
;;:subc     nil -- this is the top
  :superc   ((object  1.0)       ; i.e. solid
             (fluid   0.8)       ; liquid or gas
             (living-thing  0.5)
             (natural-thing 0.5)
             (artificial-thing 0.2) )
  :a-link   (*other* 10.0)
  :slot1  "Part(s)"
    :type   :aspect
   ;:subc   nil
  :slot2  "Relation(s)"
    :type   :relation
   ;:subc   nil
)

(defagent   fluid     concept-agent
  :type     (:concept  :object)
  :subc     thing
  :superc   (liquid
             gas    )
  :a-link   (*other* 2.0)
  :slot1  "Part(s)"
    :type   :aspect
    :subc   (thing . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (thing . :slot2)
)

(defagent   gas       concept-agent
  :type     (:concept  :object)
  :subc     fluid
  ;; subclasses defined in AMBR/KB/LIQUID.LSP
  :a-link   (*other* 2.0)
  :slot1  "Part(s)"
    :type   :aspect
    :subc   (fluid . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (fluid . :slot2)
)

(defagent   liquid    concept-agent
  :type     (:concept  :object)
  :subc     fluid
  ;; subclasses defined in AMBR/KB/LIQUID.LSP
  :a-link   (*other* 2.0)
  :slot1  "Part(s)"
    :type   :aspect
    :subc   (fluid . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (fluid . :slot2)
)

(defagent   natural-thing   concept-agent
  :type     (:concept  :object)
  :subc     thing
  :superc   natural-object
  :a-link   ((artificial-thing 1.0)
             (*other*  2.0) )
  :slot1  "Part(s)"
    :type   :aspect
    :subc   (thing . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (thing . :slot2)
)

(defagent   artificial-thing  concept-agent
  :type     (:concept  :object)
  :subc     thing
  :superc   artifact
  :a-link   ((natural-thing 1.0)
             (*other*       3.0) )
  :slot1  "Part(s)"
    :type   :aspect
    :subc   (thing . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (thing . :slot2)
)

(defagent   living-thing  concept-agent
  :type     (:concept  :object)
  :subc     thing
;;:superc   (plant     ; see KB/ANIMALS.LSP
;;           animal    ; see KB/ANIMALS.LSP
;;           human  )  ; see KB/HUMAN.LSP
  :a-link   (*other* 2.0)
  :slot1  "Part(s)"
    :type   :aspect
    :subc   (thing . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (thing . :slot2)
)


;;;;   ********   O B J E C T S   ********
;;
;; 'Object' here means 'solid object' as
;; opposed to 'fluid'.

(defagent   object    concept-agent
  :type     (:concept  :object)
  :subc     thing
  :superc   ((small-object  0.5)
             (big-object    0.5)
             (static-object 0.5)
             (moving-object 0.5) )
  :a-link   (*other* 5.0)
  :slot1  "Part(s)"
    :type   :aspect
    :subc   (thing . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (thing . :slot2)
)

(defagent   small-object    concept-agent
  :type     (:concept  :object)
  :subc     object
  :a-link   ((big-object 1.0)
             (*other*    3.0) )
  :slot1
    :type   :aspect
    :subc   (object . :slot1)
  :slot2
    :type   :relation
    :subc   (object . :slot2)
)

(defagent   big-object    concept-agent
  :type     (:concept  :object)
  :subc     object
  :a-link   ((small-object  1.0)
             (static-object 0.2)
             (*other*       3.0) )
  :slot1
    :type   :aspect
    :subc   (object . :slot1)
  :slot2
    :type   :relation
    :subc   (object . :slot2)
)

(defagent   static-object  concept-agent
  :type     (:concept  :object)
  :subc     object
  :a-link   ((moving-object 1.0)
             (big-object    0.2)
             (*other*       3.0) )
  :slot1
    :type   :aspect
    :subc   (object . :slot1)
  :slot2
    :type   :relation
    :subc   (object . :slot2)
)

(defagent   moving-object  concept-agent
  :type     (:concept  :object)
  :subc     object
  :a-link   ((static-object 1.0)
             (*other*       3.0) )
  :slot1
    :type   :aspect
    :subc   (object . :slot1)
  :slot2
    :type   :relation
    :subc   (object . :slot2)
)

(defagent   natural-object  concept-agent
  :type     (:concept  :object)
  :subc     (object
             natural-thing )
  :a-link   ((artifact 0.2)
             (*other*  1.0) )
  :slot1
    :type   :aspect
    :subc   ((object . :slot1)
             (natural-thing . :slot1) )
  :slot2
    :type   :relation
    :subc   ((object . :slot2)
             (natural-thing . :slot2) )
)

(defagent   artifact  concept-agent
  :type     (:concept  :object)
  :subc     (object
             artificial-thing )
  :a-link   ((natural-object 0.5)
             (*other*        1.0) )
  :slot1
    :type   :aspect
    :subc   ((object . :slot1)
             (artificial-thing . :slot1) )
  :slot2
    :type   :relation
    :subc   ((object . :slot2)
             (artificial-thing . :slot2) )
)



;;;;   ********   GENERAL  RELATIONS  **********
;;
;; Relations are distinguished by the tag :RELATION
;; in their TYPE slot. This tag plays a role in the
;; mechanisms of the model and in particular the
;; top-down structure correspondence (see AMBR/
;; STR_CORR.LSP).
;;
;; Relations are partitioned (somewhat arbitrarily)
;; into subclasses.  In particular:
;;  + spatial  relations -- e.g. LEFT-OF, SURROUND
;;  + temporal relations -- e.g. FOLLOWS, BEFORE
;;  + physical relations -- e.g. COLOR-OF, PART-OF
;;  + logical  relations -- e.g. AND, OR, CAUSE
;;
;; See files AMBR/KB/TEMP_REL, =/SPAT_REL,
;; =/PHYS_REL.LSP, etc.

(defagent   relation    concept-agent
  :type    (:concept  :relation)
;;:subc    nil -- this is the top
  :superc  ( ; (spatial-relation  0.5)  ; SPAT_REL.LSP
             ; (temporal-relation 0.5)  ; TEMP_REL.LSP
             ; (physical-relation 0.5)  ; PHYS_REL.LSP
            (logical-relation  0.5)
            (unary-relation    0.1)  ; 1 argument
            (binary-relation   0.2)  ; 2 args
            (ternary-relation  0.1)  ; 3 args
            (multi-relation    0.1)  ; > 3 args
            (*other* 3.0)  )
  :slot1   "operand(s)"
    :type     :aspect
   ;:subc     nil
)

(defagent   unary-relation    concept-agent
  :type     (:concept  :relation)
  :subc     relation
  :a-link   (*other* 3.0)
  :slot1   "operand"
    :type     :aspect
    :subc     (relation . :slot1)
)

(defagent   binary-relation    concept-agent
  :type     (:concept  :relation)
  :subc     relation
  :superc   (sym-bin-rel
             asym-bin-rel )
  :a-link   (*other* 2.0)
  :slot1   "operand 1"
    :type     :aspect
    :subc     (relation . :slot1)
  :slot2   "operand 2"
    :type     :aspect
    :subc     (relation . :slot1)
)                         ; --^^^--

(defagent   sym-bin-rel    concept-agent
  "Symmetric binary relation"
  :type     (:concept  :relation)
  :subc     binary-relation
  :a-link   ((asym-bin-rel 1.0)
             (*other* 2.0) )
  :slot1   "operand 1"
    :type     :aspect
    :peer     :slot2        ; <--- NB!
    :subc     (binary-relation . :slot1)
  :slot2   "operand 2"
    :type     :aspect
    :peer     :slot1        ; <--- NB!
    :subc     (binary-relation . :slot2)
)

(defagent   asym-bin-rel    concept-agent
  "Asymmetric binary relation"
  :type     (:concept  :relation)
  :subc     binary-relation
  :superc   (cause-like-rel 0.25)
  :a-link   ((sym-bin-rel 1.0)
             (*other* 2.0) )
  :slot1   "operand 1"
    :type     :aspect
    :subc     (binary-relation . :slot1)
  :slot2   "operand 2"
    :type     :aspect
    :subc     (binary-relation . :slot2)
)

(defagent   ternary-relation    concept-agent
  :type     (:concept  :relation)
  :subc     relation
  :a-link   (*other* 3.0)
  :slot1   "operand 1"
    :type     :aspect
    :subc     (relation . :slot1)
  :slot2   "operand 2"
    :type     :aspect
    :subc     (relation . :slot1)
  :slot3   "operand 3"
    :type     :aspect
    :subc     (relation . :slot1)
)

(defagent   multi-relation    concept-agent
  "Relation with more than three arguments"
  :type     (:concept  :relation)
  :subc     relation
  :a-link   (*other* 3.0)
  :slot1   "operands"
    :type     :aspect
    :subc     (relation . :slot1)
)


;;;;;;  ******  LOGICAL RELATIONS  ******
;;
;; See files AMBR/KB/TEMP_REL, =/SPAT_REL,
;; =/PHYS_REL.LSP for other kinds of rels.

(defagent   logical-relation    concept-agent
  :type     (:concept  :relation)
  :subc     relation
  :superc  (not
            and
            or
            cause )
  :a-link  ((cause-like-rel 1.0)
            (*other* 1.0) )
  :slot1   "operand(s)"
    :type     :aspect
    :subc     (relation . :slot1)
)

(defagent   not     concept-agent
  :type     (:concept  :relation)
  :subc     logical-relation
;;:instance #<genKB>
  :slot1  "Negated term"
    :type     :aspect
    :subc     (logical-relation . :slot1)
)

(defagent   and      concept-agent
  "Logical AND of two or more arguments"
  :type     (:concept  :relation)
  :subc     logical-relation
;;:instance #<genKB>
  :a-link   (sym-AND 1.0)
  :slot1   "operand 1"
    :type     :aspect
    :peer     (:slot2          ; symmetric
               :slot3 :slot4 )
    :subc     (logical-relation . :slot1)
  :slot2   "operand 2"
    :type     :aspect
    :peer     (:slot1          ; symmetric
               :slot3 :slot4)
    :subc     (logical-relation . :slot1)
  :slot3   "operand 3, if any"
    :type     :aspect
    :peer     (:slot1 :slot2 :slot4)
    :subc     (logical-relation . :slot1)
  :slot4   "operand 4 and more, if any"
    :type     :aspect
    :peer     (:slot1 :slot2 :slot3)
    :subc     (logical-relation . :slot1)
)

(defagent   or       concept-agent
  "Logical OR of two or more arguments"
  :type     (:concept  :relation)
  :subc     logical-relation
;;:instance #<genKB>
  :a-link   (sym-OR  1.0)
  :slot1   "operand 1"
    :type     :aspect
    :peer     (:slot2          ; symmetric
               :slot3 :slot4 )
    :subc     (logical-relation . :slot1)
  :slot2   "operand 2"
    :type     :aspect
    :peer     (:slot1          ; symmetric
               :slot3 :slot4)
    :subc     (logical-relation . :slot1)
  :slot3   "operand 3, if any"
    :type     :aspect
    :peer     (:slot1 :slot2 :slot4)
    :subc     (logical-relation . :slot1)
  :slot4   "operand 4 and more, if any"
    :type     :aspect
    :peer     (:slot1 :slot2 :slot3)
    :subc     (logical-relation . :slot1)
)

(defagent  sym-AND   instance-agent
  "AND is a symmetrical relation."
  :type     (:instance  :relation)
  :inst-of  symmetrical    ; see below
  :a-link   (sym-OR 0.5)
  :slot1
    :type     :aspect
    :inst-of  (symmetrical . :slot1)
    :c-coref  and
)

(defagent  sym-OR   instance-agent
  "OR is a symmetrical relation."
  :type     (:instance  :relation)
  :inst-of  symmetrical    ; see below
  :a-link   (sym-AND 0.5)
  :slot1
    :type     :aspect
    :inst-of  (symmetrical . :slot1)
    :c-coref  or
)


;;;;  CAUSE-like relations and CAUSE itself.

(defagent   cause-like-rel    concept-agent
  "Common supercl of CAUSE, FOLLOWS, TO-REACH."
  :type     (:concept  :relation)
  :subc     asym-bin-rel
  :superc  (cause
            to-reach
            ;; (follows 0.5)   ; see TEMP_REL.LSP
           )
  :a-link  ((logical-relation 1.0)
            (goal/result 1.0)
            (*other* 1.0) )
  :slot1   "antecedent-like"
    :type     :aspect
    :subc     (asym-bin-rel . :slot1)
  :slot2   "consequent-like"
    :type     :aspect
    :subc     (asym-bin-rel . :slot2)
)

(defagent   cause    concept-agent
  :type     (:concept  :relation)
  :subc     ((cause-like-rel   1.0)
             (logical-relation 1.0)
             ;; (follows 0.5)  ; see TEMP_REL.LSP
             )
;;:instance #<genKB>
  :a-link   ((to-reach 1.0)
             (result   1.0)
             (is-goal  0.5) )
  :slot1  "antecedent"
    :type     :aspect
    :subc     (((cause-like-rel   . :slot1) 1.0)
               ((logical-relation . :slot1) 1.0)
              ) ;; ((follows . :slot1) 0.5)
  :slot2  "consequent"
    :type     :aspect               ; --vvv--
    :subc     (((logical-relation . :slot1) 1.0)
               ((cause-like-rel   . :slot2) 1.0)
              ) ;; ((follows . :slot2) 0.5)
)

(defagent   to-reach    concept-agent
  "Starting from INITST try to reach GOALST."
  :type     (:concept  :relation)
  :subc     cause-like-rel
;;:instance #<genKB>
  :a-link   ((cause   1.0)
             (is-goal 1.0)
             (init-state 0.7)
             (goal-state 0.7)
             (result  0.5) )
  :slot1  "initial state"
    :type     :aspect
    :subc     (cause-like-rel . :slot1)
  :slot2  "goal state"
    :type     :aspect
    :subc     (cause-like-rel . :slot2)
)

(defagent   goal/result    concept-agent
  "Common superclass of IS-GOAL and RESULT."
  :type     (:concept  :relation)
  :subc     unary-relation
  :superc  (is-goal
            result )
  :a-link  ((cause-like-rel 1.0)
            (goal-state 0.5)
            (end-state  0.5)
            (*other*    1.0) )
  :slot1   "event or proposition"
    :type     :aspect
    :subc     (unary-relation . :slot1)
)

(defagent   is-goal    concept-agent
  "Wrapped around goal states and propositions."
  :type     (:concept  :relation)
  :subc     goal/result
  :a-link  ((goal-state 1.0)
            (result     1.0)
            (*other*    2.0) )
  :slot1   "event or proposition"
    :type     :aspect
    :subc     (goal/result . :slot1)
)

(defagent   result    concept-agent
  "Wrapped around end states and propositions."
  :type     (:concept  :relation)
  :subc     goal/result
  :a-link  ((end-state 1.0)
            (is-goal   1.0)
            (*other*   2.0) )
  :slot1   "event or proposition"
    :type     :aspect
    :subc     (goal/result . :slot1)
)




;;;;;;  ******  META-RELATIONS  ******
;;
;; These are relations describing properties
;; of other relations.  The agents standing
;; for these meta-relations may have procedu-
;; ral knowledge that captures the semantics.
;;
;; The semantics of the few meta-relations
;; introduced here may be loosely defined
;; as follows:
;;
;;  + SYMMETRICAL   is an attribute that
;;      applies to binary relations and
;;      represents the property that
;;     (R a b) <==> (R b a).  E.g. IN-TOUCH.
;;
;;  + MIRROR-REL  represents that one rel.
;;      transforms into another and vice
;;      versa when their respective arg's
;;      are swapped:  (R a b) <==> (Q b a).
;;      E.g.  BEFORE vs. AFTER.
;;      (Note the 'cross-slots technique'
;;       for handling mirror relations
;;       documented in AMBR/KB/TEMP_REL.LSP.)
;;
;;  + POLAR-REL  represents that two rel's
;;      (usually unary) mark the two ends of
;;      a continuum, e.g. BIG and SMALL.
;;      Thus, (big x) ==> (not (small x)).

(defagent   meta-relation    concept-agent
  "A relation describing other relations."
  :type    (:concept  :relation)
  :subc    relation
  :superc  (unary-meta-rel
            binary-meta-rel )
  :a-link  (*other* 2.0)
  :slot1   "relational operand(s)"
    :type     :aspect
    :subc     (relation . :slot1)
    :c-coref  relation           ; range
)

(defagent   unary-meta-rel    concept-agent
  :type     (:concept  :relation)
  :subc     meta-relation
  :superc   symmetrical
  :a-link   (*other* 3.0)
  :slot1   "relational operand"
    :type     :aspect
    :subc     (meta-relation . :slot1)
    :c-coref  relation           ; range
)

(defagent   binary-meta-rel    concept-agent
  :type     (:concept  :relation)
  :subc     meta-relation
  :superc   (mirror-rel
             polar-rel  )
  :a-link   (*other* 2.0)
  :slot1   "relational operand 1"
    :type     :aspect
    :subc     (meta-relation . :slot1)
    :c-coref  relation           ; range
  :slot2   "relational operand 2"
    :type     :aspect
    :subc     (meta-relation . :slot1)
    :c-coref  relation           ; range
)

(defagent  symmetrical   concept-agent
  "(R a b) <==> (R b a)"
  :type     (:concept  :relation)
  :subc     unary-meta-rel
  :instance ((sym-OR  0.2)
             (sym-AND 0.2)
            ) ;; more from other files
  :slot1   "relational operand"
    :type     :aspect
    :subc     (unary-meta-rel . :slot1)
    :c-coref  relation           ; range
)

(defagent   mirror-rel    concept-agent
  "(R a b) <==> (Q b a)"
  :type     (:concept  :relation)
  :subc     binary-meta-rel
;; instances defined in other files
  :a-link   ((polar-rel 0.5)
             (*other*   1.0) )
  :slot1   "relational operand 1"
    :type     :aspect
    :peer     :slot2       ; symmetrical
    :subc     (binary-meta-rel . :slot1)
    :c-coref  relation           ; range
  :slot2   "relational operand 2"
    :type     :aspect
    :peer     :slot1       ; symmetrical
    :inst-of  (binary-meta-rel . :slot2)
    :c-coref  relation           ; range
)

(defagent   polar-rel    concept-agent
  "Two ends of a spectrum (e.g. BIG and SMALL)."
  :type     (:concept  :relation)
  :subc     binary-meta-rel
;; instances defined in other files
  :a-link   ((mirror-rel 0.5)
             (*other*    1.0) )
  :slot1   "relational operand 1"
    :type     :aspect
    :peer     :slot2       ; symmetrical
    :subc     (binary-meta-rel . :slot1)
    :c-coref  relation           ; range
  :slot2   "relational operand 2"
    :type     :aspect
    :peer     :slot1       ; symmetrical
    :subc     (binary-meta-rel . :slot2)
    :c-coref  relation           ; range
)

;; meta-propositions about meta-relations

(defagent  sym-MIRROR-REL   instance-agent
  "MIRROR-REL is a symmetrical (meta) relation."
  :type     (:instance  :relation)
  :inst-of  symmetrical
  :a-link   (sym-POLAR-REL 0.5)
  :slot1
    :type     :aspect
    :inst-of  (symmetrical . :slot1)
    :c-coref  mirror-rel
)

(defagent  sym-POLAR-REL   instance-agent
  "POLAR-REL is a symmetrical (meta) relation."
  :type     (:instance  :relation)
  :inst-of  symmetrical
  :a-link   (sym-MIRROR-REL 0.5)
  :slot1
    :type     :aspect
    :inst-of  (symmetrical . :slot1)
    :c-coref  polar-rel
)



;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;  ---- Prepare for GENKB ---- ;;;;;;
;;

(GENKB-template
  :herald  "Concepts from KB/SEMANTIC/ABSTRACT.LSP, ver. 3.0.0."
  :templates '(
    (and       (:instance (*other* 10)) )
    (cause     (:instance (*other* 10)) )
    (not       (:instance (*other* 10)) )
    (or        (:instance (*other* 10)) )
    (to-reach  (:instance (*other* 10)) )
))

#|
;;;;;;;;;;;;   Appendix   ;;;;;;;;;;
;;
;; This file defines the following 53 agents
;; (ordered by name):

 *other*

 and
 artifact
 artificial-thing
 asym-bin-rel
 big-object
 binary-meta-rel
 binary-relation
 cause
 cause-like-rel
 end-state
 episode
 event
 fluid
 gas
 goal/result
 goal-state
 init-state
 inter-state
 is-goal
 liquid
 living-thing
 logical-relation
 meta-relation
 mirror-rel
 moving-object
 multi-relation
 natural-object
 natural-thing
 not
 object
 or
 polar-rel
 problem
 problem-description
 problem-solution
 relation
 result
 situation
 small-object
 state
 static-object
 sym-AND
 sym-bin-rel
 symmetrical
 sym-MIRROR-REL
 sym-OR
 sym-POLAR-REL
 ternary-relation
 thing
 to-reach
 unary-meta-rel
 unary-relation

|#

;;;;;;  End of file  AMBR/KB/SEMANTIC/ABSTRACT.LSP
