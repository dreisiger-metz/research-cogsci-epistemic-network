;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/semantic/forest.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Semantic memory -- concepts related to forests, fire, etc.
;;; DEPENDS-ON: ambr/packages.lsp, ambr/intrface/defagent.lsp, =/kb_util.lsp,
;;;             ambr/kb/abstract.lsp, =/phys_rel.lsp, =/kitchen.lsp
;;; XREFS:      none
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    27-04-98 [3.0]  Used parts of old LTM.LSP.
;;; UPDATED:    ...


           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;;;;;;   F O R E S T S ,  F I R E ,  E T C .   ;;;;;;
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  "AMBR")

;;;; This file defines concept agents for forest objects, fire, etc.


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


;; The agents defined in this file depend on some concept-agents provided
;; by AMBR/KB/ABSTRACT.LSP, =/PHYS_REL.LSP, and =/KITCHEN.LSP. These include:

(require-agents '(object
                  artifact
                  burns-out
                  material-stone
                  heat-source
                  light-source
                  *other*
))


;;;;   ******  FOREST  OBJECTS  ******
;;

(defagent   fire    concept-agent
  :type     (:concept  :object)
  :subc     ((heat-source  1.0)
             (light-source 0.5) )
;;:instance #<genKB>
  :a-link   ((burns-out 0.5)
             (*other*   2.0) )
  :slot1
    :type   :aspect
    :subc   (((heat-source  . :slot1) 1.0)
             ((light-source . :slot1) 0.5) )
  :slot2
    :type   :relation
    :subc   (((heat-source  . :slot2) 1.0)
             ((light-source . :slot2) 0.5) )
)
(with-agent  burns-out
  :a-link   (fire 0.5)
)

(defagent   stone    concept-agent
  "A piece of stone; not material-stone."
  :type     (:concept  :object)
  :subc     natural-object
  :superc   (pebble
             boulder )
  :a-link   ((material-stone 1.0)
             (*other* 1.0) )
  :slot1
    :type   :aspect
    :subc   (natural-object . :slot1)
  :slot2
    :type   :relation
    :subc   (natural-object . :slot2)
    ;; no c-coref ==> generic slot
  :slot3  "All stones are made of... stone!"
    :type      :relation         ; --vvv--
    :INST-OF   (natural-object . :slot2)
    :c-coref   stone-md-stone    ; concrete prop.
)
(defagent  stone-md-stone  instance-agent
  :type    (:instance  :relation)
  :inst-of made-of
  :c-coref (stone . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (made-of . :slot1)
    :c-coref  stone              ; range
  :slot2
    :type     :aspect
    :SUBC     (made-of . :slot2)
    :c-coref  material-stone     ; range
)
(with-agent  material-stone   ; KB/PHYS_REL.LSP
  :c-coref  (stone-md-stone . :slot2)
)

(defagent   pebble    concept-agent
  :type     (:concept  :object)
  :subc     ((stone        1.0)
             (small-object 0.5) )
;;:instance #<genKB>
  :a-link   ((boulder 0.5)
             (material-stone 0.3) )
  :slot1
    :type   :aspect
    :subc   (((stone        . :slot1) 1.0)
             ((small-object . :slot1) 0.5) )
  :slot2
    :type   :relation
    :subc   (((stone        . :slot2) 1.0)
             ((small-object . :slot2) 0.5) )
)

(defagent   boulder   concept-agent
  :type     (:concept  :object)
  :subc     ((stone      1.0)
             (big-object 0.5) )
;;:instance #<genKB>
  :a-link   ((pebble 0.5)
             (material-stone 0.3) )
  :slot1
    :type   :aspect
    :subc   (((stone      . :slot1) 1.0)
             ((big-object . :slot1) 0.5) )
  :slot2
    :type   :relation
    :subc   (((stone      . :slot2) 1.0)
             ((big-object . :slot2) 0.5) )
)


;;;;   ******  INSTRUMENTS  ******
;;

(defagent   instrument    concept-agent
  :type     (:concept  :object)
  :subc     artifact
  :superc   ((knife  0.5)
             (axe    0.3)
             (shovel 0.3)
             (torch  0.2) )
  :a-link   (*other* 2.0)
  :slot1
    :type   :aspect
    :subc   (artifact . :slot1)
  :slot2
    :type   :relation
    :subc   (artifact . :slot2)
)

(defagent   torch  concept-agent
  :type     (:concept  :object)
  :subc     ((light-source 1.0)
             (instrument   0.1) )
;;:instance #<genKB>
  :a-link   (lamp 0.5)  ; KB/SEMANTIC/KITCHEN
  :slot1
    :type   :aspect
    :subc   (((light-source . :slot1) 1.0)
             ((instrument   . :slot1) 0.1) )
  :slot2
    :type   :relation
    :subc   (((light-source . :slot2) 1.0)
             ((instrument    . :slot2) 0.1) )
)

(defagent   knife    concept-agent
  :type     (:concept  :object)
  :subc     instrument
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   instrument
  :slot2
    :type   :relation
    :subc   instrument
)

(defagent   can-opener    concept-agent
  :type     (:concept  :object)
  :subc     instrument
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   instrument
  :slot2
    :type   :relation
    :subc   instrument
)

(defagent   axe     concept-agent
  :type     (:concept  :object)
  :subc     instrument
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   instrument
  :slot2
    :type   :relation
    :subc   instrument
)

(defagent   shovel   concept-agent
  :type     (:concept  :object)
  :subc     instrument
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   instrument
  :slot2
    :type   :relation
    :subc   instrument
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references )


;;;;;;  ---- Prepare for GENKB ---- ;;;;;;
;;

(GENKB-template
  :herald  "Concepts from KB/SEMANTIC/FOREST.LSP, ver. 3.0.0."
  :templates '(
    (axe         (:instance (*other* 10)) )
    (boulder     (:instance (*other* 10)) )
    (can-opener  (:instance (*other* 10)) )
    (fire        (:instance (*other* 10)) )
    (knife       (:instance (*other* 10)) )
    (pebble      (:instance (*other* 10)) )
    (shovel      (:instance (*other* 10)) )
    (torch       (:instance (*other* 10)) )
 ;;;;;;;;
    (made-of     (:instance (stone-md-stone 1)) )
))

#|
;;;;;;;;;;;;   Appendix   ;;;;;;;;;;
;;
;; This file defines the following 11 agents
;; (ordered by name):

axe
boulder
can-opener
fire
instrument
knife
pebble
shovel
stone
stone-md-stone
torch

|#

;;;;;;  End of file  AMBR/KB/SEMANTIC/FOREST.LSP
