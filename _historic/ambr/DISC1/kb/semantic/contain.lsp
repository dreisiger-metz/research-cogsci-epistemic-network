;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/semantic/contain.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Semantic memory -- containers such as BAG, BOX, and CUP.
;;; DEPENDS-ON: ambr/packages.lsp, ambr/intrface/defagent.lsp, =/kb_util.lsp
;;;             ambr/kb/abstract.lsp, ambr/kb/phys_rel.lsp
;;; XREFS:      ambr/kb/liquid.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    24-04-98 [3.0]  Used parts of old LTM.LSP.
;;; UPDATED:    ...


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;;;;;;;      C O N T A I N E R S        ;;;;;;;;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  "AMBR")

;;;; This file defines concept agents for various kinds of containers used
;;;; in AMBR's semantic memory.


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
;; provided by AMBR/KB/ABSTRACT.LSP and =/PHYS_REL.LSP.  These include:

(require-agents '(object
                  artifact
                  liquid
                  made-of
                  material-metal
                  material-glass
                  material-china
                  in
                  *other*
))

(reference-agents 'from-KB/LIQUID
  '(wine
    tea
    coffee
))


;;;;   ******  GENERAL  CLASSES   ******
;;

(defagent   container    concept-agent
  :type     (:concept  :object)
  :subc     artifact
  :superc   (box/bag
             liquid-holder
             food-holder  )
  :a-link   ((in 0.2)    ; from KB/SEMANTIC/PHYS_REL
             (*other* 1.0) )
  :slot1
    :type   :aspect
    :subc   (artifact . :slot1)
  :slot2
    :type   :relation
    :subc   (artifact . :slot2)
)
(with-agent in  ; from AMBR/KB/SEMANTIC/PHYS_REL.LSP
  :a-link   (container 0.3)
)

(defagent   liquid-holder    concept-agent
  :type     (:concept  :object)
  :subc     container
  :superc   ((bottle  0.7)
             (cup     0.7)
             (glass   0.5)
             (bowl    0.1)  )
  :a-link   ((liquid   1.0)
             (crockery 0.3)
             (*other*  1.0) )
  :slot1
    :type   :aspect
    :subc   (container . :slot1)
  :slot2
    :type   :relation
    :subc   (container . :slot2)
)

(defagent   food-holder    concept-agent
  :type     (:concept  :object)
  :subc     container
  :superc   (plate
            ) ; cook-vessel -- KB/KITCHEN.LSP
  :a-link   ( ; food     -- KB/FOOD.LSP
             (*other*  1.0) )
  :slot1
    :type   :aspect
    :subc   (container . :slot1)
  :slot2
    :type   :relation
    :subc   (container . :slot2)
)

(defagent   crockery    concept-agent
  "Vessels made of clay or porcelain."
  :type     (:concept  :object)
  :subc     container
  :superc   ((pot   1.0)
             (cup   0.5)
             (plate 0.3) )
  :a-link   ((liquid-holder  0.5)
             (material-clay  1.0)
             (material-china 0.5)
             (*other* 1.0)  )
  :slot1
    :type   :aspect
    :subc   (container . :slot1)
  :slot2
    :type   :relation
    :subc   (container . :slot2)
)


;;;;   ******  BOXES AND BAGS   ******
;;
;;  These kinds of containers are under-
;;  represented in the current version of
;;  the knowledge base because it deals
;;  primarily liquids.

(defagent   box/bag    concept-agent
  "The common superclass of BOX and BAG."
  :type     (:concept  :object)
  :subc     container
  :superc   (box
             bag )
  :a-link   (*other* 1.0)
  :slot1
    :type   :aspect
    :subc   (container . :slot1)
  :slot2
    :type   :relation
    :subc   (container . :slot2)
)

(defagent   box    concept-agent
  :type     (:concept  :object)
  :subc     box/bag
;;:instance #<genKB>
  :a-link   (*other* 2.0)  ; subclasses
  :slot1
    :type   :aspect
    :subc   (box/bag . :slot1)
  :slot2
    :type   :relation
    :subc   (box/bag . :slot2)
)

(defagent   bag    concept-agent
  :type     (:concept  :object)
  :subc     box/bag
;;:instance #<genKB>
  :a-link   (*other* 2.0)  ; subclasses
  :slot1
    :type   :aspect
    :subc   (box/bag . :slot1)
  :slot2
    :type   :relation
    :subc   (box/bag . :slot2)
)


;;;;   ******  LIQUID HOLDERS   ******
;;

(defagent   bowl   concept-agent
  :type     (:concept  :object)
  :subc     ((liquid-holder 0.7)
             (food-holder   0.3) )
;;:instance #<genKB>
  :a-link   (crockery 0.5)
  :slot1
    :type   :aspect
    :subc   (((liquid-holder . :slot1) 0.7)
             ((food-holder   . :slot1) 0.3) )
  :slot2
    :type   :relation
    :subc   (((liquid-holder . :slot2) 0.7)
             ((food-holder   . :slot2) 0.3) )
)

(defagent   bottle   concept-agent
  :type     (:concept  :object)
  :subc     liquid-holder
;;:instance #<genKB>
  :a-link   (wine 0.5)
  :slot1
    :type   :aspect
    :subc   (liquid-holder . :slot1)
  :slot2
    :type   :relation
    :subc   (liquid-holder . :slot2)
    ;; no c-coref ==> generic slot
  :slot3
    :type      :relation        ; --vvv--
    :INST-OF   (liquid-holder . :slot2)
    :c-coref   bottle-md-gl   ; concrete prop.
)
(defagent   bottle-md-gl  instance-agent
  :type     (:instance  :relation)
  :inst-of  made-of
  :c-coref  (bottle . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (made-of . :slot1)
    :c-coref  bottle   ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (made-of . :slot2)
    :c-coref  mglass-bottle     ; prototype
)
(defagent  mglass-bottle  instance-agent
  :type    (:PROTOTYPE :instance  :object)
  :inst-of material-glass
  :c-coref (bottle-md-gl . :slot2)
  :a-link  (bottle 0.2)
)

(defagent   glass   concept-agent
  :type     (:concept  :object)
  :subc     ((liquid-holder 0.6)
             (crockery 0.4) )
;;:instance #<genKB>
  :a-link   ((wine 0.8)
             (beer 0.5)
             (cup  0.2) )
  :slot1
    :type   :aspect
    :subc   (((liquid-holder . :slot1) 0.6)
             ((crockery      . :slot1) 0.4) )
  :slot2
    :type   :relation
    :subc   (((liquid-holder . :slot2) 0.6)
             ((crockery      . :slot2) 0.4) )
    ;; no c-coref ==> generic slot
  :slot3
    :type      :relation          ; --vvv--
    :INST-OF   (((liquid-holder . :slot2) 0.6)
                ((crockery      . :slot2) 0.4) )
    :c-coref   glass-md-gl   ; concrete prop.
)
(defagent   glass-md-gl  instance-agent
  :type     (:instance  :relation)
  :inst-of  made-of
  :c-coref  (glass . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (made-of . :slot1)
    :c-coref  glass    ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (made-of . :slot2)
    :c-coref  mglass-glass       ; prototype
)
(defagent   mglass-glass  instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  material-glass
  :c-coref  (glass-md-gl . :slot2)
  :a-link   (glass 0.2)
)

(defagent    cup   concept-agent
  :type      (:concept  :object)
  :subc      ((liquid-holder 0.8)
              (crockery 0.2) )
;;:instance #<genKB>
  :a-link    ((saucer  0.5)
              (tea     0.5)
              (glass   0.1) )
  :slot1
    :type   :aspect
    :subc   (((liquid-holder . :slot1) 0.8)
             ((crockery      . :slot1) 0.2) )
  :slot2
    :type   :relation
    :subc   (((liquid-holder . :slot2) 0.8)
             ((crockery      . :slot2) 0.2) )
    ;; no c-coref ==> generic slot
  :slot3
    :type      :relation          ; --vvv--
    :INST-OF   (((liquid-holder . :slot2) 0.8)
                ((crockery      . :slot2) 0.2) )
    :c-coref   cup-md-china  ; concrete prop.
)
(defagent   cup-md-china  instance-agent
  :type     (:instance  :relation)
  :inst-of  made-of
  :c-coref  (cup . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (made-of . :slot1)
    :c-coref  cup   ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (made-of . :slot2)
    :c-coref  mchina-cup         ; prototype
)
(defagent   mchina-cup  instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  material-china
  :c-coref  (cup-md-china . :slot2)
  :a-link   (cup 0.2)
)

(defagent    saucer   concept-agent
  :type      (:concept  :object)
  :subc      ((crockery    0.8)
              (food-holder 0.2) )
;;:instance #<genKB>
  :a-link    ((cup   1.0)
              (plate 0.5) )
  :slot1
    :type   :aspect
    :subc   (((crockery    . :slot1) 0.8)
             ((food-holder . :slot1) 0.2) )
  :slot2
    :type   :relation
    :subc   (((crockery    . :slot2) 0.8)
             ((food-holder . :slot2) 0.2) )
    ;; no c-coref ==> generic slot
  :slot3
    :type      :relation          ; --vvv--
    :INST-OF   (((crockery    . :slot2) 0.8)
                ((food-holder . :slot2) 0.2) )
    :c-coref   saucer-md-china  ; concrete prop.
)
(defagent   saucer-md-china  instance-agent
  :type     (:instance  :relation)
  :inst-of  made-of
  :c-coref  (saucer . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (made-of . :slot1)
    :c-coref  saucer   ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (made-of . :slot2)
    :c-coref  mchina-saucer     ; prototype
)
(defagent   mchina-saucer  instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  material-china
  :c-coref  (saucer-md-china . :slot2)
  :a-link   (saucer 0.2)
)

(defagent    plate   concept-agent
  "A dish; don't confuse with HOT-PLATE."
  :type     (:concept  :object)
  :subc     ((food-holder 1.0)
             (crockery    0.5) )
;;:instance #<genKB>
;;:a-link   (wash-machine 0.1)  ; KB/KITCHEN
  :slot1
    :type   :aspect
    :subc   (((food-holder . :slot1) 1.0)
             ((crockery    . :slot1) 0.5) )
  :slot2
    :type   :relation
    :subc   (((food-holder . :slot2) 1.0)
             ((crockery    . :slot2) 0.5) )
    ;; no c-coref ==> generic slot
  :slot3
    :type      :relation        ; --vvv--
    :INST-OF   (((food-holder . :slot2) 1.0)
                ((crockery    . :slot2) 0.5) )
    :c-coref   plate-md-china   ; concrete prop.
)
(defagent   plate-md-china  instance-agent
  :type     (:instance  :relation)
  :inst-of  made-of
  :c-coref  (plate . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (made-of . :slot1)
    :c-coref  plate    ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (made-of . :slot2)
    :c-coref  mchina-plate      ; prototype
)
(defagent   mchina-plate  instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  material-china
  :c-coref  (plate-md-china . :slot2)
  :a-link   (plate 0.2)
)

(defagent   pot   concept-agent
  :type     (:concept  :object)
  :subc     ((crockery 1.0)
             (liquid-holder 0.2)
             (food-holder   0.2) )
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (((crockery      . :slot1) 1.0)
             ((liquid-holder . :slot1) 0.2)
             ((food-holder   . :slot1) 0.2) )
  :slot2
    :type   :relation
    :subc   (((crockery      . :slot2) 1.0)
             ((liquid-holder . :slot2) 0.2)
             ((food-holder   . :slot2) 0.2) )
    ;; no c-coref ==> generic slot
  :slot3
    :type      :relation          ; --vvv--
    :INST-OF   (((crockery      . :slot2) 1.0)
                ((liquid-holder . :slot2) 0.2)
                ((food-holder   . :slot2) 0.2) )
    :c-coref   pot-md-clay      ; concrete prop.
)
(defagent   pot-md-clay  instance-agent
  :type     (:instance  :relation)
  :inst-of  made-of
  :c-coref  (pot . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (made-of . :slot1)
    :c-coref  pot   ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (made-of . :slot2)
    :c-coref  mclay-pot         ; prototype
)
(defagent   mclay-pot  instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  material-clay
  :c-coref  (pot-md-clay . :slot2)
  :a-link   (pot 0.2)
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references
  :exemption-id  'from-KB/LIQUID )


;;;;;;  ---- Prepare for GENKB ---- ;;;;;;
;;

(GENKB-template
  :herald  "Concepts from KB/SEMANTIC/CONTAIN.LSP, ver. 3.0.0."
  :templates '(
    (bag             (:instance (*other* 10)) )
    (bottle          (:instance (*other* 10)) )
    (box             (:instance (*other* 10)) )
    (bowl            (:instance (*other* 10)) )
    (cup             (:instance (*other* 10)) )
    (glass           (:instance (*other* 10)) )
    (plate           (:instance (*other* 10)) )
    (pot             (:instance (*other* 10)) )
    (saucer          (:instance (*other* 10)) )
 ;;;;;;;;
    (made-of         (:instance (bottle-md-gl    1)
                                (cup-md-china    1)
                                (glass-md-gl     1)
                                (plate-md-china  1)
                                (pot-md-clay     1)
                                (saucer-md-china 1)) )
    (material-glass  (:instance (mglass-bottle   1)
                                (mglass-glass    1)) )
    (material-china  (:instance (mchina-cup      1)
                                (mchina-plate    1)
                                (mchina-saucer   1)) )
    (material-clay   (:instance (mclay-pot       1)) )
))

#|
;;;;;;;;;;;;   Appendix   ;;;;;;;;;;
;;
;; This file defines the following 26 agents
;; (ordered by name):

bag
bottle
bottle-md-gl
box
box/bag
bowl
container
crockery
cup
cup-md-china
food-holder
glass
glass-md-gl
liquid-holder
mchina-cup
mchina-plate
mchina-saucer
mclay-pot
mglass-bottle
mglass-glass
plate
plate-md-china
pot
pot-md-clay
saucer
saucer-md-china

|#

;;;;;;  End of file  AMBR/KB/SEMANTIC/CONTAIN.LSP
