 ;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/semantic/phys_rel.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Semantic memory -- physical relations such as PART-OF and ...
;;; DEPENDS-ON: ambr/packages.lsp, ambr/intrface/defagent.lsp, =/kb_util.lsp ;
;;;             ambr/kb/semantic/abstract.lsp, ambr/kb/semantic/spat_rel.lsp
;;; XREFS:      ambr/kb/semantic/physprop.lsp  ; for MADE-OF
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    21-04-98 [3.0]  Used parts of old LTM.LSP.
;;; UPDATED:    ...


       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;     P H Y S I C A L   R E L A T I O N S     ;;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  "AMBR")

;;;; This file defines some physical relations for the semantic memory of AMBR.
;;;; See file AMBR/KB/PHYSPROP.LSP for agents describing physical properties.


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
;; provided by AMBR/KB/SEMANTIC/ABSTRACT.LSP.  Some of the most important of
;; these are:

(require-agents '(relation
                  binary-relation
                  mirror-rel
                  polar-rel
                  symmetrical
                  object
                  *other*
))

;; In addition, this file depends on some spatial concepts and relations
;; provided by AMBR/KB/SEMANTIC/SPAT_REL.LSP.  The spatial aspect of most
;; 'phys/spat' rels is determined by the relative position of two objects.
;; See the section on PHYSICAL/SPATIAL RELATIONS later in this file.

(require-agents '(relative-pos
                  above
))

;; There are a few cross-references with AMBR/KB/SEMANTIC/PHYSPROP.LSP.
;; They involve mostly MADE-OF and MATERIAL.

(reference-agents 'from-KB/PHYSPROP
  '(physprop-rel        ; superclass of MADE-OF, subclass of BIN-PHYS-REL
    physprop-qualifier  ; superclass of MATERIAL
    physprop-attr       ; subclass of UNARY-PHYS-REL
))



;;;;   *****   GENERAL PHYSICAL RELATIONS  *****
;;
;; The detailed lattice below prevents the marker
;; passing mechanism from generating too many
;; 'ACME-like' hypotheses.

(defagent   physical-relation    concept-agent
  :type    (:concept  :relation)
  :subc    relation
  :superc  ((phys/spat-rel  1.0)
            (unary-phys-rel 0.8)
            (bin-phys-rel   1.0)
            (ter-phys-rel   0.5)
            (*other* 1.0) )
  :slot1   "operand(s)"
    :type     :aspect
    :subc     (relation . :slot1)
)
(with-agent  relation
  :superc  (physical-relation 0.5)
)

(defagent   unary-phys-rel    concept-agent
  "Unary physical relation (i.e. attribute)."
  :type     (:concept  :relation)
  :subc     ((physical-relation 1.0)
             (unary-relation    0.1) )
  :superc  ((is-damaged 0.2)
            (physprop-attr 1.0)  ; KB/SEMANTIC/PHYSPROP
            (*other*  2.0) )
  :slot1   "operand"
    :type     :aspect
    :subc     (((physical-relation . :slot1) 1.0)
               ((unary-relation    . :slot1) 0.1) )
)

(defagent   bin-phys-rel    concept-agent
  "Binary physical relation"
  :type     (:concept  :relation)
  :subc     ((physical-relation 1.0)
             (binary-relation   0.1) )
  :superc   ((part-of 1.0)
             (physprop-rel 1.0)  ; KB/SEMANTIC/PHYSPROP
             (*other* 2.0) )
  :slot1   "operand 1"
    :type     :aspect
    :subc     (((physical-relation . :slot1) 1.0)
               ((binary-relation   . :slot1) 0.1) )
  :slot2   "operand 2"
    :type     :aspect
    :subc     (((physical-relation . :slot1) 1.0)
               ((binary-relation   . :slot2) 0.1) )
)

(defagent   ter-phys-rel    concept-agent
  "Ternary physical relation"
  :type     (:concept  :relation)
  :subc     ((physical-relation 1.0)
             (ternary-relation  0.1) )
  :a-link   (*other* 1.0)
  :slot1   "operand 1"
    :type     :aspect
    :subc     (((physical-relation . :slot1) 1.0)
               ((ternary-relation  . :slot1) 0.1) )
  :slot2   "operand 2"
    :type     :aspect
    :subc     (((physical-relation . :slot1) 1.)
               ((ternary-relation  . :slot2) 0.1) )
  :slot3   "operand 3"
    :type     :aspect
    :subc     (((physical-relation . :slot1) 1.0)
               ((ternary-relation  . :slot3) 0.1) )
)


;;;;  *****   PART-OF   *****
;;
;;  The job for PART-OF is often served by
;;  the S-slots in micro-frames.  For example,
;;  the frame TREE may have SLOT2 pointing to
;;  BRANCH.

(defagent   part-of    concept-agent
  :type     (:concept  :relation)
  :subc     bin-phys-rel
;;:instance #<genKB>
  :a-link   (*other* 2.0)
  :slot1   "part"
    :type     :aspect
    :subc     (bin-phys-rel . :slot1)
  :slot2   "whole"
    :type     :aspect
    :subc     (bin-phys-rel . :slot2)
)


;;;;  ****   PHYSICAL/SPATIAL RELATIONS  ****
;;
;; This section defines some relations that can
;; be classified both as physical and spatial.
;; See AMBR/KB/SEMANTIC/SPAT_REL.LSP for 'pure'
;; spatial relations.  (Of course, this division
;; is arbitrary. All agents are in the same LTM.)

(defagent   phys/spat-rel    concept-agent
  :type    (:concept  :relation)
  :subc    ((bin-phys-rel 1.0)
            (relative-pos 0.5) )  ; KB/SEMANTIC/SPAT_REL
  :superc  (supports
            in-touch-with
            (*other* 1.0) )
  :slot1
    :type     :aspect
    :subc     (((bin-phys-rel . :slot1) 1.0)
               ((relative-pos . :slot1) 0.5) )
  :slot2
    :type     :aspect
    :subc     (((bin-phys-rel . :slot2) 1.0)
               ((relative-pos . :slot2) 0.5) )
)
(with-agent  relative-pos   ; in KB/SEMANTIC/SPAT_REL
  :superc  (phys/spat-rel 0.5)
)

(defagent   supports    concept-agent
  :type     (:concept  :relation)
  :subc     phys/spat-rel
  :superc   on
;;:instance #<genKB>
  :slot1  "support"
    :type     :aspect
    :subc     (phys/spat-rel . :slot1)
  :slot2  "supportee"
    :type     :aspect
    :subc     (phys/spat-rel . :slot2)
)

(defagent   in-touch-with    concept-agent
  :type     (:concept  :relation)
  :subc     phys/spat-rel
  :superc   ((attached-to 0.5)
             (in 0.5)
             (on 0.5) )
;;:instance #<genKB>
  :slot1
    :type     :aspect
    :peer     :slot2       ; symmetric
    :subc     (phys/spat-rel . :slot1)
  :slot2
    :type     :aspect
    :peer     :slot1       ; symmetric
    :subc     (phys/spat-rel . :slot2)
)

(defagent  sym-IN-TOUCH-WITH   instance-agent
  "IN-TOUCH-WITH is a symmetrical relation."
  :type     (:instance  :relation)
  :inst-of  symmetrical
  :slot1
    :type     :aspect
    :inst-of  (symmetrical . :slot1)
    :c-coref  in-touch-with
)

(defagent   attached-to   concept-agent
  :type     (:concept  :relation)
  :subc     in-touch-with
;;:instance #<genKB>
  :slot1  "carrier"
    :type     :aspect
    :subc     (in-touch-with . :slot1)
  :slot2  "attache"
    :type     :aspect
    :subc     (in-touch-with . :slot2)
)

(defagent   on     concept-agent
  :type     (:concept  :relation)
  :subc     (in-touch-with
             supports      )
;;:instance #<genKB>
  :a-link   (above   0.5)   ; KB/SEMANTIC/SPAT_REL
  :slot1  "base"
    :type     :aspect
    :subc     ((in-touch-with . :slot1)
               (supports      . :slot1) )
  :slot2  "top"
    :type     :aspect
    :subc     ((in-touch-with . :slot2)
               (supports      . :slot2) )
)

(defagent   in    concept-agent
  :type     (:concept  :relation)
  :subc     in-touch-with
;;:instance #<genKB>
;;:a-link   (container 0.3) -- KB/SEMANTIC/CONTAIN
  :slot1  "insider"
    :type      :aspect
    :subc     (in-touch-with . :slot1)
  :slot2  "container"
    :type      :aspect
    :subc     (in-touch-with . :slot2)
)


;;;;  ****  MADE-OF and periphernalia  ****
;;
;;  The relation MADE-OF belongs to the class
;;  of 'physical property relations' such as
;;  COLOR-OF, TEMPERATURE-OF, etc.
;;  It is defined here because at first the
;;  property 'material' appeared more substantial
;;  than 'color', 'taste', etc.
;;
;;  Future versions of the knowledge base may
;;  define attributional analogs to MADE-OF --
;;  e.g. IS-METALLIC, IS-WOODEN, etc.
;;  See AMBR/KB/SEMANTIC/PHYSPROP.LSP for more.


(defagent   made-of    concept-agent
  :type     (:concept  :relation)
  :subc     physprop-rel    ; KB/SEMANTIC/PHYSPROP.LSP
;;:instance #<genKB>
  :slot1    "thing"
    :type     :aspect
    :subc     (physprop-rel . :slot1)
  :slot2    "material"
    :type     :aspect
    :subc     (physprop-rel . :slot2)
    :c-coref  material      ; range
)

(defagent   material  concept-agent
  :type     (:concept  :object)
  :c-coref  (made-of . :slot2)
  :subc     ((physprop-qualifier 0.7)
             (thing 0.4) )
  :superc   ((material-metal 1.0)
             (material-glass 0.5)
             (material-wood  0.5)
             (material-stone 0.5)
             (material-ice   0.2)
             (material-china 0.2)
             (*other* 1.0) )
)

(defagent   material-metal  concept-agent
  :type     (:concept  :object)
  :subc     material
;;:instance #<genKB>
)

(defagent   material-glass  concept-agent
  :type     (:concept  :object)
  :subc     material
;;:instance #<genKB>
)

(defagent   material-china  concept-agent
  :type     (:concept  :object)
  :subc     material
;;:instance #<genKB>
  :a-link   (material-clay 0.5)
)

(defagent   material-clay   concept-agent
  :type     (:concept  :object)
  :subc     material
;;:instance #<genKB>
;;:a-link   crockery -- KB/SEMANTIC/CONTAIN
)

(defagent   material-wood   concept-agent
  :type     (:concept  :object)
  :subc     material
;;:instance #<genKB>
  :a-link   (material-metal 0.2)
)

(defagent   material-stone    concept-agent
  :type     (:concept  :object)
  :subc     material
;;:instance #<genKB>
;;:c-coref  (stone-md-stone . :slot2) ; FOREST.LSP
)

(defagent   material-ice    concept-agent
  :type     (:concept  :object)
  :subc     material
;;:instance #<genKB>
;;:a-link   ice   -- see KB/SEMANTIC/LIQUID.LSP
)


;;;;  *****  DAMAGE and PROTECTION  *****
;;

(defagent   protects    concept-agent
  :type     (:concept  :relation)
  :subc     bin-phys-rel
;;:instance #<genKB>
  :a-link   (damages 1.0)
  :slot1    "protector"
    :type     :aspect
    :subc     (bin-phys-rel . :slot1)
  :slot2    "protected"
    :type     :aspect
    :subc     (bin-phys-rel . :slot2)
)

(defagent   damages    concept-agent
  :type     (:concept  :relation)
  :subc     bin-phys-rel
  :superc   ((breaks    1.0)
             (burns-out 0.5) )
  :a-link   ((is-damaged 1.0)
             (protects   1.0)
             (*other*    1.0) )
  :slot1    "damaging"
    :type     :aspect
    :subc     (bin-phys-rel . :slot1)
  :slot2    "damaged"
    :type     :aspect
    :subc     (bin-phys-rel . :slot2)
)

(defagent   is-damaged   concept-agent
  :type     (:concept  :relation)
  :subc     unary-phys-rel
  :superc   ((is-broken     1.0)
             (is-burnt-out  0.5)
             (is-dissipated 0.2) )
  :a-link   ((damages 1.0)
             (*other* 1.0) )
  :slot1
    :type     :aspect
    :subc     (unary-phys-rel . :slot1)
)

(defagent   breaks      concept-agent
  :type     (:concept  :relation)
  :subc     damages
;;:instance #<genKB>
  :a-link   ((is-broken 1.0)
             (material-glass 0.2) )
  :slot1    "breaker"
    :type     :aspect
    :subc     (damages . :slot1)
  :slot2    "broken"
    :type     :aspect
    :subc     (damages . :slot2)
)

(defagent   is-broken    concept-agent
  :type     (:concept  :relation)
  :subc     is-damaged
;;:instance #<genKB>
  :a-link   ((breaks  1.0)
             (material-glass 0.2) )
  :slot1
    :type     :aspect
    :subc     (is-damaged . :slot1)
)

(defagent   burns-out   concept-agent
  :type     (:concept  :relation)
  :subc     damages
;;:instance #<genKB>
  :a-link   ((is-burnt-out  1.0)
             (material-wood 0.2)
            ) ; (fire 0.5)  ; KB/FOREST.LSP
  :slot1    "heat source"
    :type     :aspect
    :subc     (damages . :slot1)
  :slot2    "flammable"
    :type     :aspect
    :subc     (damages . :slot2)
)

(defagent   is-burnt-out  concept-agent
  :type     (:concept  :relation)
  :subc     is-damaged
;;:instance #<genKB>
  :a-link   (burns-out 1.0)
  :slot1
    :type     :aspect
    :subc     (is-damaged . :slot1)
)

(defagent   is-dissipated  concept-agent
  :type     (:concept  :relation)
  :subc     is-damaged
;;:instance #<genKB>
  :slot1
    :type     :aspect
    :subc     (is-damaged . :slot1)
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references
  :exemption-id 'from-KB/PHYSPROP )


;;;;;;  ---- Prepare for GENKB ---- ;;;;;;
;;

(GENKB-template
  :herald  "Concepts from KB/SEMANTIC/PHYS_REL.LSP, ver. 3.0.0."
  :templates '(
    (attached-to    (:instance (*other* 10)) )
    (breaks         (:instance (*other* 10)) )
    (burns-out      (:instance (*other* 10)) )
    (in             (:instance (*other* 10)) )
    (in-touch-with  (:instance (*other* 10)) )
    (is-broken      (:instance (*other* 10)) )
    (is-burnt-out   (:instance (*other* 10)) )
    (is-dissipated  (:instance (*other* 10)) )
    (made-of        (:instance (*other*  5)) )  ; see KB/SEMANTIC/CONTAIN.LSP
    (material-china (:instance (*other* 10)) )
    (material-clay  (:instance (*other* 10)) )
    (material-glass (:instance (*other* 10)) )
    (material-metal (:instance (*other* 10)) )
    (material-stone (:instance (*other* 10)) )
    (material-wood  (:instance (*other* 10)) )
    (material-ice   (:instance (*other* 10)) )
    (on             (:instance (*other* 10)) )
    (part-of        (:instance (*other* 10)) )
    (protects       (:instance (*other* 10)) )
    (supports       (:instance (*other* 10)) )
))

#|
;;;;;;;;;;;;   Appendix   ;;;;;;;;;;
;;
;; This file defines the following 29 agents
;; (ordered by name):

 attached-to
 bin-phys-rel
 breaks
 burns-out
 damages
 in
 in-touch-with
 is-broken
 is-burnt-out
 is-damaged
 is-dissipated
 made-of
 material
 material-china
 material-clay
 material-glass
 material-metal
 material-stone
 material-wood
 material-ice
 on
 part-of
 physical-relation
 phys/spat-rel
 protects
 supports
 sym-IN-TOUCH-WITH
 ter-phys-rel
 unary-phys-rel

|#

;;;;;;  End of file  AMBR/KB/SEMANTIC/PHYS_REL.LSP
