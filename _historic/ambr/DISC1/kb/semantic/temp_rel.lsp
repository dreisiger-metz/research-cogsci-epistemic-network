;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/semantic/temp_rel.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Semantic memory -- temporal relations such as FOLLOWS & BEFORE.
;;; DEPENDS-ON: ambr/packages.lsp, ambr/intrface/defagent.lsp, =/kb_util.lsp ;
;;;             ambr/kb/abstract.lsp
;;; XREFS:      ambr/kb/spat_rel.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    14-04-98 [3.0]  Used parts of old LTM.LSP.
;;; UPDATED:    ...


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;     T E M P O R A L   R E L A T I O N S     ;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package  "AMBR")

;;;; This file defines some temporal relations for the semantic memory of AMBR.
;;;;
;;;; Note in particular the relation FOLLOWS which is a superclass of CAUSE.
;;;;
;;;; See also file AMBR/KB/SPAT_REL.LSP which deals with spatial relations.


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


;; The agents defined in this file depend on the abstract concept-agents
;; provided by AMBR/KB/ABSTRACT.LSP.  Some of the most important of these are:

(require-agents '(relation
                  binary-relation
                  mirror-rel
                  polar-rel
                  event
                  cause
                  cause-like-rel
                  *other*
))


;;;;   *****   GENERAL TEMPORAL RELATIONS  *****
;;
;; The detailed lattice below prevents the marker
;; passing mechanism from generating too many
;; 'ACME-like' hypotheses.
;; (E.g. FOLLOWS will not be mapped to COLOR-OF
;; too easily only on the grounds that both are
;; asymmetric binary relations.)

(defagent   temporal-relation    concept-agent
  :type    (:concept  :relation)
  :subc    relation
  :superc  (unary-tem-rel
            bin-tem-rel
            (*other* 2.0) )
  :slot1   "operand(s)"
    :type     :aspect
    :subc     (relation . :slot1)
)
(with-agent  relation
  :superc  (temporal-relation 0.5)
)

(defagent   unary-tem-rel    concept-agent
  "Unary temporal relation (i.e. attribute)."
  :type     (:concept  :relation)
  :subc     ((temporal-relation 1.0)
             (unary-relation    0.1) )
  :a-link   (*other* 3.0)
  :slot1   "operand"
    :type     :aspect
    :subc     (((temporal-relation . :slot1) 1.0)
               ((unary-relation    . :slot1) 0.1) )
)

(defagent   bin-tem-rel    concept-agent
  "Binary temporal relation"
  :type     (:concept  :relation)
  :subc     ((temporal-relation 1.0)
             (binary-relation   0.1) )
  :superc   ((follows  1.0)
             (before   0.5)
             (after    0.5)
             (during   0.5)
             (*other*  2.0) )
  :slot1   "operand 1"
    :type     :aspect
    :subc     (((temporal-relation . :slot1) 1.0)
               ((binary-relation   . :slot1) 0.1) )
  :slot2   "operand 2"
    :type     :aspect
    :subc     (((temporal-relation . :slot1) 1.0)
               ((binary-relation   . :slot2) 0.1) )
)


;;;;   ******   FOLLOWS    ******
;;
;;  The relation FOLLOWS is important due to
;;  its kinship with CAUSE.
;;
;;  FOLLOWS is distinguished from BEFORE and AFTER
;;  in that the two events are in (almost) immediate
;;  succession.  For example:
;;   -- (follows Monday Tuesday)   is true
;;   -- (follows Monday Wednesday) is false
;;   -- (before  Monday Tuesday)   is true
;;   -- (before  Monday Wednesday) is also true

(defagent  follows   concept-agent
  "An event immediately follows another in time."
  :type     (:concept  :relation)
  :subc     ((bin-tem-rel    1.0)
             (cause-like-rel 0.5) )  ; ABSTRACT
  :superc   cause           ; from KB/ABSTRACT.LSP
;;:instance #<genKB>
  :a-link   ((after  0.5)
             (result 0.5) ) ; from KB/ABSTRACT.LSP
  :slot1  "earlier event"
    :type     :aspect
    :subc     (((bin-tem-rel    . :slot1) 1.0)
               ((cause-like-rel . :slot1) 0.5) )
  :slot2  "later event"
    :type     :aspect
    :subc     (((bin-tem-rel    . :slot2) 1.0)
               ((cause-like-rel . :slot2) 0.5) )
)

;; Make CAUSE a subclass of FOLLOWS.
;; In turn, FOLLOWS is a subclass of CAUSE-LIKE-REL.
;; This allows for interesting mappings between
;; problems, explanations, and episodes.
;; (See AMBR/KB/ABSTRACT.LSP for the definition of
;;  CAUSE, CAUSE-LIKE-REL, and other logical rels.)
;;
(with-agent  cause-like-rel
  :superc   (follows 0.5)
)
(with-agent  cause
  :subc     (follows 0.5)
  :slot1  ; the antecedent is the earlier event
    :subc   (follows . :slot1)
  :slot2  ; the consequent is the later event
    :subc   (follows . :slot2)
)


;;;;   *****   BEFORE and AFTER   *****
;;
;; See the remark about the distinction between
;; FOLLOWS and AFTER above.
;;
;; Note the technique used to represent the fact
;; that  (before X Y) <==> (after Y X).
;; There is a common superclass (called BEFORE/AFTER)
;; unique to only these two relations.
;; BEFORE.SLOT1 inherits from BEFORE/AFTER.SLOT1 ;
;; AFTER.SLOT1  inherits from BEFORE/AFTER.SLOT2 .
;; Therefore, this technique is sometimes referred
;; to as 'cross-slots technique'.
;;
;; Thus the marker-passing and structure-corresp.
;; mechanisms would generate the hypotheses A<==>Y
;; and B<==>X on the basis of BEFORE-AB<-->AFTER-XY.
;;
;; (Note that the function TRACK-SLOT-IN-PATH
;;   (defined in AMBR/MARKER.LSP) checks for
;;   :INST-OF facets but ignores :SUBC facets.)
;;
;; Note also the explicit proposition
;;  (mirror-rel before after)
;; See AMBR/KB/ABSTRACT.LSP for documentation
;; on the meta-relation MIRROR-REL.

(defagent  before/after   concept-agent
  "The common superclass of BEFORE and AFTER"
  :type     (:concept  :relation)
  :subc     bin-tem-rel
  :superc   (before
             after )
  :slot1  "earlier event"
    :type     :aspect
    :subc     (bin-tem-rel . :slot1)
  :slot2  "later event"
    :type     :aspect
    :subc     (bin-tem-rel . :slot2)
)

(defagent  before   concept-agent
  "E.g. (before Monday Wednesday)"
  :type     (:concept  :relation)
  :subc     before/after
  :c-coref  (mirror-BEFORE/AFTER . :slot1)
;;:instance #<genKB>
  :a-link   ((after   1.0)
             (follows 1.0) )
  :slot1  "earlier event"
    :type     :aspect
    :subc     (before/after . :slot1)
  :slot2  "later event"
    :type     :aspect
    :subc     (before/after . :slot2)
)

(defagent  after    concept-agent
  "E.g. (after Wednesday Monday)"
  :type     (:concept  :relation)
  :subc     before/after
  :c-coref  (mirror-BEFORE/AFTER . :slot2)
;;:instance #<genKB>
  :a-link   ((before  1.0)
             (follows 1.0) )
  :slot1  "later event"
    :type     :aspect
    :subc     (before/after . :slot2)
  :slot2  "earlier event"
    :type     :aspect
    :subc     (bin-tem-rel . :slot2)
)

(defagent   mirror-BEFORE/AFTER  instance-agent
  "(mirror-rel before after)"
  :type     (:instance :relation)
  :inst-of  mirror-rel
  :slot1
    :type     :aspect
    :inst-of  (mirror-rel . :slot1)
    :c-coref  before
  :slot2
    :type     :aspect
    :inst-of  (mirror-rel . :slot2)
    :c-coref  after
)
(with-agent  mirror-rel
  :instance  (mirror-BEFORE/AFTER 1.0)
)  ; MIRROR-BEFORE/AFTER is a rather prototypical
   ; instance of MIRROR-REL (see KB/ABSTRACT.LSP)


;;;;  ****  RELATIVE INTERVALS  ****
;;

(defagent  during   concept-agent
  "E.g. (during lecture-7 morning-session-3)"
  :type     (:concept  :relation)
  :subc     bin-tem-rel
;;:instance #<genKB>
  :slot1  "focal event"
    :type     :aspect
    :subc     (bin-tem-rel . :slot1)
  :slot2  "background event"
    :type     :aspect
    :subc     (bin-tem-rel . :slot2)
)

(defagent  start/end-together  concept-agent
  "Common supercl. of START- and END-TOGETHER."
  :type     (:concept  :relation)
  :subc     bin-tem-rel
  :superc   (start-together
             end-together  )
  :slot1  "event 1"
    :type     :aspect
    :peer     :slot2             ; symmetrical
    :subc     (bin-tem-rel . :slot1)
  :slot2  "event 2"
    :type     :aspect
    :peer     :slot1             ; symmetrical
    :subc     (bin-tem-rel . :slot2)
)

(defagent  start-together  concept-agent
  :type     (:concept  :relation)
  :subc     start/end-together
;;:instance #<genKB>
  :a-link   (end-together 1.0)
  :slot1  "event 1"
    :type     :aspect
    :peer     :slot2         ; symmetrical
    :subc     (start/end-together . :slot1)
  :slot2  "event 2"
    :type     :aspect
    :peer     :slot1         ; symmetrical
    :subc     (start/end-together . :slot2)
)

(defagent  end-together   concept-agent
  :type     (:concept  :relation)
  :subc     start/end-together
;;:instance #<genKB>
  :a-link   (start-together 1.0)
  :slot1  "event 1"
    :type     :aspect
    :peer     :slot2         ; symmetrical
    :subc     (start/end-together . :slot1)
  :slot2  "event 2"
    :type     :aspect
    :peer     :slot1         ; symmetrical
    :subc     (start/end-together . :slot2)
)


;;;;   *****   TEMPORAL ATTRIBUTES  *****
;;

(reference-agents 'from-KB/SPAT_REL
  '(short                   ; spatial analogs, see
    long                    ; AMBR/KB/SPAT_REL.LSP
    length-rel
))

(defagent  duration-rel  concept-agent
  "The superclass of BRIEF, PROLONGED, etc."
  :type     (:concept  :relation)
  :subc     unary-tem-rel
  :superc   (brief
             prolonged )
  :a-link   (length-rel 0.5) ; spatial analog
  :slot1
    :type     :aspect
    :subc     (unary-tem-rel . :slot1)
    :c-coref  event          ; range
)

(defagent  brief    concept-agent
  "That lasts for a short period of time"
  :type     (:concept  :relation)
  :subc     duration-rel
  :c-coref  (polar-BRIEF/PROL . :slot1)
;;:instance #<genKB>
  :a-link   ((short     0.5)  ; spatial analog
             (prolonged 0.5) )
  :slot1
    :type     :aspect
    :subc     (duration-rel . :slot1)
    :c-coref  event           ; range
)

(defagent  prolonged  concept-agent
  "That lasts for a long period of time"
  :type     (:concept  :relation)
  :subc     duration-rel
  :c-coref  (polar-BRIEF/PROL . :slot2)
;;:instance #<genKB>
  :a-link   ((long  0.5)    ; spatial analog
             (brief 0.5) )
  :slot1
    :type     :aspect
    :subc     (duration-rel . :slot1)
    :c-coref  event         ; range
)

(defagent   polar-BRIEF/PROL    instance-agent
  "(polar-rel brief prolonged)"
  :type     (:instance :relation)
  :inst-of  polar-rel
  :slot1
    :type     :aspect
    :inst-of  (polar-rel . :slot1)
    :c-coref  brief
  :slot2
    :type     :aspect
    :inst-of  (polar-rel . :slot2)
    :c-coref  prolonged
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references
  :exemption-id 'from-KB/SPAT_REL )


;;;;;;  ---- Prepare for GENKB ---- ;;;;;;
;;

(GENKB-template
  :herald  "Concepts from KB/SEMANTIC/TEMP_REL.LSP, ver. 3.0.0."
  :templates '(
    (after           (:instance (*other* 10)) )
    (before          (:instance (*other* 10)) )
    (brief           (:instance (*other* 10)) )
    (during          (:instance (*other* 10)) )
    (end-together    (:instance (*other* 10)) )
    (follows         (:instance (*other* 10)) )
    (prolonged       (:instance (*other* 10)) )
    (start-together  (:instance (*other* 10)) )
))

#|
;;;;;;;;;;;;   Appendix   ;;;;;;;;;;
;;
;; This file defines the following 16 agents
;; (ordered by name):

 after
 before
 before/after
 bin-tem-rel
 brief
 duration-rel
 during
 end-together
 follows
 mirror-BEFORE/AFTER
 polar-BRIEF/PROL
 prolonged
 start-together
 start/end-together
 temporal-relation
 unary-tem-rel

|#

;;;;;;  End of file  AMBR/KB/SEMANTIC/TEMP_REL.LSP
