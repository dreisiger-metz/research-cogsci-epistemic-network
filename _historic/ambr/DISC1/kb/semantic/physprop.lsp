 ;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/semantic/physprop.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Semantic memory -- physical properties such as COLOR-OF, IS-RED
;;; DEPENDS-ON: ambr/packages.lsp, ambr/intrface/defagent.lsp, =/kb_util.lsp ;
;;;             ambr/kb/abstract.lsp, ambr/kb/phys_rel.lsp
;;; XREFS:      none
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    21-04-98 [3.0.0]  Used parts of old LTM.LSP.
;;; UPDATED:    29-05-98  HOT --> HIGH-TEMP, COLD --> LOW-TEMP, etc.
;;; UPDATED:    22-06-98  Added TASTE-OF and periphernalia.
;;; UPDATED:    ...


       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;     P H Y S I C A L   P R O P E R T I E S     ;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  "AMBR")

;;;; This file defines some relations (and attributes) describing physical
;;;; properties.  See AMBR/KB/PHYS_REL.LSP for other physical relations.

;;; ...
;;;    attributes vs. relations ...
;;;      e.g.  (is-hot X)    vs.  (temperature-of X high-T-X)
;;; ...


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


;; The agents defined in this file depend on the agents provided by several
;; files and in particular AMBR/KB/PHYS_REL.LSP:

(require-agents '(physical-relation
                  unary-phys-rel
                  bin-phys-rel
))


;;;;   *****   GENERAL PHYSICAL PROPERTIES   *****
;;
;; Physical properties (or 'physprops' for short)
;; can be viewed either as relations or as attributes.
;;
;; We plan to experiment with both representations
;; as well as with their combination.  At the time
;; being, however, the relational format dominates.


(defagent   physical-property  concept-agent
  "Common superclass of PHYSPROP-ATTR and ...-REL."
  :type    (:concept  :relation)
  :subc    physical-relation
  :superc  (physprop-attr
            physprop-rel  )
  :slot1   "object"
    :type     :aspect
    :subc     (physical-relation . :slot1)
  :slot2   "property, if any"
    :type     :aspect
    :subc     (physical-relation . :slot1)
)

(defagent   physprop-attr    concept-agent
  "Physical property represented as an attribute."
  :type     (:concept  :relation)
  :subc     ((physical-property 0.6)
             (unary-phys-rel    0.4) )
  :a-link   ((physprop-rel 1.0)
             (*other*      3.0) )
  :slot1   "object"
    :type     :aspect
    :subc     (unary-phys-rel . :slot1)
)
(with-agent  unary-phys-rel
  :superc  physprop-attr
)

(defagent   physprop-rel    concept-agent
  "Physical property represented as a relation."
  :type     (:concept  :relation)
  :subc     ((physical-property 0.6)
             (bin-phys-rel      0.4) )
  :superc   ((temperature-of 0.5)
             (made-of  0.5)  ; KB/SEMANTIC/PHYS_REL.LSP
             (color-of 0.5)
             (shape-of 0.5) )
  :a-link   ((physprop-attr 1.0)
             (*other*  1.0) )
  :slot1   "object"
    :type     :aspect
    :subc     (((physical-property . :slot1) 0.6)
               ((bin-phys-rel      . :slot1) 0.4) )
  :slot2   "property"
    :type     :aspect
    :subc     (((physical-property . :slot2) 0.6)
               ((bin-phys-rel      . :slot2) 0.4) )
    :c-coref  physprop-qualifier   ; range
)
(with-agent  bin-phys-rel
  :superc  (physprop-rel 0.8)
)

(defagent   physprop-qualifier  concept-agent
  :type     (:concept  :object)
  :c-coref  (physprop-rel . :slot2)
;;:subc     nil  ; no superclass in this version
  :superc   ((temper-qualifier 0.3)
             (color            0.3)
             (shape-qualifier  0.3)
             (*other*          1.0) )
)


;;;;  *** TEMPERATURE-OF and periphernalia ***
;;

(defagent   temperature-of    concept-agent
  :type     (:concept  :relation)
  :subc     physprop-rel
;;:instance #<genKB>
  :slot1
    :type     :aspect
    :subc     (physprop-rel . :slot1)
  :slot2
    :type     :aspect
    :subc     (physprop-rel . :slot2)
    :c-coref  temper-qualifier    ; range
)

(defagent   temper-qualifier  concept-agent
  :type     (:concept  :object)
  :c-coref  (temperature-of . :slot2)
  :subc     physprop-qualifier
  :superc   ((high-temp   0.5)
             (medium-temp 0.5)
             (low-temp    0.5) )
  :a-link   (*other* 1.0)
)

(defagent   low-temp    concept-agent
  :type     (:concept  :object)
  :subc     temper-qualifier
;;:instance #<genKB>
  :a-link   ((temperature-of 1.0)
             (high-temp      0.5) )
)

(defagent   medium-temp  concept-agent
  :type     (:concept  :object)
  :subc     temper-qualifier
;;:instance #<genKB>
  :a-link   ((temperature-of 1.0)
             (low-temp       0.2)
             (high-temp      0.2) )
)

(defagent   high-temp  concept-agent
  :type     (:concept  :object)
  :subc     temper-qualifier
;;:instance #<genKB>
  :a-link   ((temperature-of 1.0)
             (low-temp       0.5) )
)


;;;;  ****  COLOR-OF and periphernalia  ****
;;

(defagent   color-of    concept-agent
  :type     (:concept  :relation)
  :subc     physprop-rel
;;:instance #<genKB>
  :slot1
    :type     :aspect
    :subc     (physprop-rel . :slot1)
  :slot2
    :type     :aspect
    :subc     (physprop-rel . :slot2)
    :c-coref  color     ; range
)

(defagent   color  concept-agent
  :type     (:concept  :object)
  :c-coref  (color-of . :slot2)
  :subc     physprop-qualifier
  :superc   ((white 0.2)
             (black 0.2)
             (red   0.2)
             (green 0.2)
             (*other* 1.0) )
)

(defagent   white    concept-agent
  :type     (:concept  :object)
  :subc     color
;;:instance #<genKB>
  :a-link   ((black 0.5)
             (color-of 1.0) )
)

(defagent   black    concept-agent
  :type     (:concept  :object)
  :subc     color
;;:instance #<genKB>
  :a-link   ((white 0.5)
             (color-of 1.0) )
)

(defagent   red    concept-agent
  :type     (:concept  :object)
  :subc     color
;;:instance #<genKB>
  :a-link   ((green 0.2)
             (color-of 1.0) )
)

(defagent   green    concept-agent
  :type     (:concept  :object)
  :subc     color
;;:instance #<genKB>
  :a-link   ((red 0.2)
             (color-of 1.0) )
)


;;;;  ****  SHAPE-OF and periphernalia  ****
;;

(defagent   shape-of    concept-agent
  :type     (:concept  :relation)
  :subc     physprop-rel
;;:instance #<genKB>
  :slot1
    :type     :aspect
    :subc     (physprop-rel . :slot1)
  :slot2
    :type     :aspect
    :subc     (physprop-rel . :slot2)
    :c-coref  shape-qualifier     ; range
)

(defagent   shape-qualifier  concept-agent
  :type     (:concept  :object)
  :c-coref  (shape-of . :slot2)
  :subc     physprop-qualifier
  :superc   ((round-shape     0.5)
             (polygonal-shape 0.5)
             (irregular-shape 0.5)
             (*other* 1.0) )
)

(defagent   irregular-shape    concept-agent
  :type     (:concept  :object)
  :subc     shape-qualifier
;;:instance #<genKB>
  :a-link   (shape-of 1.0)
)

(defagent   round-shape    concept-agent
  :type     (:concept  :object)
  :subc     shape-qualifier
  :superc   circular-shape
;;:instance #<genKB>
  :a-link   (shape-of 1.0)
)

(defagent   polygonal-shape    concept-agent
  :type     (:concept  :object)
  :subc     shape-qualifier
  :superc   (triang-shape
             rectang-shape )
;;:instance #<genKB>
  :a-link   (shape-of 1.0)
)

(defagent   circular-shape    concept-agent
  :type     (:concept  :object)
  :subc     round-shape
;;:instance #<genKB>
  :a-link   (shape-of 1.0)
)

(defagent   triang-shape    concept-agent
  :type     (:concept  :object)
  :subc     polygonal-shape
;;:instance #<genKB>
  :a-link   ((square-shape 0.5)
             (shape-of 1.0) )
)

(defagent   rectang-shape    concept-agent
  :type     (:concept  :object)
  :subc     polygonal-shape
  :superc   square-shape
;;:instance #<genKB>
  :a-link   ((triang-shape 0.5)
             (shape-of 1.0) )
)

(defagent   square-shape    concept-agent
  :type     (:concept  :object)
  :subc     rectang-shape
;;:instance #<genKB>
  :a-link   ((triang-shape 0.5)
             (shape-of 1.0) )
)


;;;;  ****  TASTE-OF and periphernalia  ****
;;

(defagent   taste-of    concept-agent
  :type     (:concept  :relation)
  :subc     physprop-rel
;;:instance #<genKB>
;;:a-link   (food 1.0) ; KB/SEMANTIC/FOOD.LSP
  :slot1
    :type     :aspect
    :subc     (physprop-rel . :slot1)
  :slot2
    :type     :aspect
    :subc     (physprop-rel . :slot2)
    :c-coref  taste         ; range
)

(defagent   taste  concept-agent
  :type     (:concept  :object)
  :c-coref  (taste-of . :slot2)
  :subc     physprop-qualifier
  :superc   ((sweet-taste 0.5)
             (salt-taste  0.5)
             (sour-taste  0.5)
             (*other* 1.0) )
)

(defagent   sweet-taste    concept-agent
  :type     (:concept  :object)
  :subc     taste
;;:instance sugar-taste  ; KB/SEMANTIC/FOOD.LSP
;;          #<genKB>
  :a-link   (taste-of 1.0)
            ;; (sugar 1.0)  ; KB/FOOD.LSP
            ;; (fruit 0.5)  ; KB/FOOD.LSP
            ;; (juice 0.3)  ; KB/LIQUID.LSP
)

(defagent   salt-taste    concept-agent
  :type     (:concept  :object)
  :subc     taste
;;:instance prtype-salt-taste  ; KB/FOOD.LSP
;;          #<genKB>
  :a-link   (taste-of 1.0)
            ;; (salt 1.0)      ; KB/FOOD.LSP

)

(defagent   sour-taste    concept-agent
  :type     (:concept  :object)
  :subc     taste
;;:instance #<genKB>
  :a-link   (taste-of 1.0)
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references )


;;;;;;  ---- Prepare for GENKB ---- ;;;;;;
;;

(GENKB-template
  :herald  "Concepts from KB/SEMANTIC/PHYSPROP.LSP, ver. 3.0.0."
  :templates '(
    (black            (:instance (*other* 10)) )
    (circular-shape   (:instance (*other* 10)) )
    (low-temp         (:instance (*other*  3)) )  ;<--
    (color-of         (:instance (*other* 10)) )
    (green            (:instance (*other* 10)) )
    (high-temp        (:instance (*other*  3)) )  ;<--
    (irregular-shape  (:instance (*other* 10)) )
    (polygonal-shape  (:instance (*other* 10)) )
    (rectang-shape    (:instance (*other* 10)) )
    (red              (:instance (*other* 10)) )
    (round-shape      (:instance (*other* 10)) )
    (salt-taste       (:instance (*other* 10)) )
    (shape-of         (:instance (*other* 10)) )
    (sour-taste       (:instance (*other* 10)) )
    (square-shape     (:instance (*other* 10)) )
    (sweet-taste      (:instance (*other* 10)) )
    (taste-of         (:instance (*other* 10)) )
    (temperature-of   (:instance (*other*  3)) )  ;<--
    (triang-shape     (:instance (*other* 10)) )
    (medium-temp      (:instance (*other* 10)) )
    (white            (:instance (*other* 10)) )
))

#|
;;;;;;;;;;;;   Appendix   ;;;;;;;;;;
;;
;; This file defines the following 29 agents
;; (ordered by name):

black
circular-shape
low-temp
color
color-of
green
high-temp
irregular-shape
physical-property
physprop-attr
physprop-rel
physprop-qualifier
polygonal-shape
rectang-shape
red
round-shape
salt-taste
shape-of
shape-qualifier
square-shape
sweet-taste
taste
taste-of
temperature-of
temper-qualifier
triang-shape
medium-temp
white

|#

;;;;;;  End of file  AMBR/KB/SEMANTIC/PHYSPROP.LSP
