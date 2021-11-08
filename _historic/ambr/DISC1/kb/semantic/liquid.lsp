;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/semantic/liquid.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Semantic memory -- concepts for liquids and gases.
;;; DEPENDS-ON: ambr/packages.lsp, ambr/intrface/defagent.lsp, =/kb_util.lsp
;;;             ambr/kb/semantic/abstract.lsp, ambr/kb/semantic/physprop.lsp
;;; XREFS:      ambr/kb/semantic/contain.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    24-04-98 [3.0]  Used parts of old LTM.LSP.
;;; UPDATED:    ...


        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;    L I Q U I D S   A N D   G A S E S     ;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  "AMBR")

;;;; This file defines concept agents for some liquids and gases used in AMBR's
;;;; semantic memory.


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

(require-agents '(fluid
                  liquid
                  gas
                  object   ; i.e. solid
                  *other*
))

;; In addition, this file depends on some physical properties provided by
;; AMBR/KB/SEMATIC/PHYSPROP.LSP (and =/PHYS_REL.LSP).  It also has some cross-
;; references with file AMBR/KB/SEMANTIC/CONTAIN.LSP.

(require-agents '(color-of
                  white
                  red
                  low-temp
                  high-temp
                  material-ice   ; from PHYS_REL.LSP
))

(reference-agents 'from-KB/CONTAIN
  '(cup
    glass
    bottle
))


;;;;   ******  GENERAL  CLASSES   ******
;;
;; The most general (and hence abstract) concepts
;; are provided by AMBR/KB/SEMANTIC/ABSTRACT.LSP.
;; These include: [solid] OBJECT and FLUID.  Fluids
;; further subdivide into liquids and gases.
;;   fluid   subc  thing
;;   object  subc  thing
;;   liquid  subc  fluid
;;   gas     subc  fluid
;;
;; This taxonomy is extended below:

(defagent   drinkable-liquid    concept-agent
  :type     (:concept  :object)
  :subc     liquid
  :superc   (water
             beverage )
  :a-link   ((non-drinkable-liquid 1.0)
             (*other* 1.0) )
  :slot1  "Part(s)"
    :type   :aspect
    :subc   (liquid . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (liquid . :slot2)
)
(defagent   non-drinkable-liquid  concept-agent
  :type     (:concept  :object)
  :subc     liquid
  :superc   (oil-based-liquid
            ) ; *other*
  :a-link   ((drinkable-liquid 1.0)
             (*other* 3.0) )
  :slot1  "Part(s)"
    :type   :aspect
    :subc   (liquid . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (liquid . :slot2)
)
(with-agent  liquid     ; see AMBR/KB/SEMANTIC/ABSTRACT
  :superc   (drinkable-liquid
             non-drinkable-liquid )
)

(defagent   oil-based-liquid  concept-agent
  :type     (:concept  :object)
  :subc     non-drinkable-liquid
  :a-link   (*other* 10.0) ; gasoline, diesel, etc.
  :slot1
    :type   :aspect
    :subc   (liquid . :slot1)
  :slot2
    :type   :relation
    :subc   (liquid . :slot2)
)


;;;;  ****   DRINKABLE LIQUIDS   ****
;;

(defagent   water    concept-agent
  :type     (:concept  :object)
  :subc     drinkable-liquid
;;:instance #<genKB>
  :a-link   ((ice   0.3)
             (steam 0.2) )
  :slot1
    :type   :aspect
    :subc   (drinkable-liquid . :slot1)
  :slot2
    :type   :relation
    :subc   (drinkable-liquid . :slot2)
)

(defagent   beverage   concept-agent
  :type     (:concept  :object)
  :subc     drinkable-liquid
  :superc   ((milk        0.3)
             (tea/coffee  0.3)
             (soft-drink  0.5)
             (alcoholic-drink 0.5) )
  :a-link   (*other* 1.0)
  :slot1
    :type   :aspect
    :subc   (drinkable-liquid . :slot1)
  :slot2
    :type   :relation
    :subc   (drinkable-liquid . :slot2)
)

(defagent   milk    concept-agent
  :type     (:concept  :object)
  :subc     (;;dairy-product  ; KB/SEMANTIC/FOOD.LSP
             beverage )
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (beverage . :slot1)
  :slot2
    :type   :relation
    :subc   (beverage . :slot2)
)

(defagent   tea/coffee    concept-agent
  "The common superclass of TEA and COFFEE."
  :type     (:concept  :object)
  :subc     beverage
  :superc   (tea
             coffee )
  :a-link   ((high-temp 0.3)   ; KB/SEMANTIC/PHYSPROP
             (*other*   1.0) )
  :slot1
    :type   :aspect
    :subc   (beverage . :slot1)
  :slot2
    :type   :relation
    :subc   (beverage . :slot2)
)
(defagent   tea    concept-agent
  :type     (:concept  :object)
  :subc     tea/coffee
;;:instance #<genKB>
  :a-link   (cup 0.3) ; KB/SEMANTIC/CONTAIN.LSP
  :slot1
    :type   :aspect
    :subc   (tea/coffee . :slot1)
  :slot2
    :type   :relation
    :subc   (tea/coffee . :slot2)
)
(defagent   coffee    concept-agent
  :type     (:concept  :object)
  :subc     tea/coffee
;;:instance #<genKB>
  :a-link   ((cup  0.3)  ; KB/SEMANTIC/CONTAIN
             (coke 0.2) )
  :slot1
    :type   :aspect
    :subc   (tea/coffee . :slot1)
  :slot2
    :type   :relation
    :subc   (tea/coffee . :slot2)
)


;;;;  ******   SOFT DRINKS   ******
;;

(defagent   soft-drink    concept-agent
  :type     (:concept  :object)
  :subc     beverage
  :superc   ((juice 0.8)
             (coke  0.8) )
  :a-link   ((beer 0.3)
             (hard-drink 0.5)
             (ice-cube   0.1)
             (*other*    1.0) )
  :slot1
    :type   :aspect
    :subc   (beverage . :slot1)
  :slot2
    :type   :relation
    :subc   (beverage . :slot2)
)

(defagent   coke      concept-agent
  :type     (:concept  :object)
  :subc     soft-drink
;;:instance #<genKB>
  :a-link   ((coffee   0.5)
             (glass    0.2) ; KB/SEMANTIC/CONTAIN
             (ice-cube 0.5) )
  :slot1
    :type   :aspect
    :subc   (soft-drink . :slot1)
  :slot2
    :type   :relation
    :subc   (soft-drink . :slot2)
)

(defagent   juice      concept-agent
  :type     (:concept  :object)
  :subc     soft-drink
;;:instance #<genKB>
;;:a-link   fruit  ; see KB/SEMANTIC/FOOD.LSP
  :slot1
    :type   :aspect
    :subc   (soft-drink . :slot1)
  :slot2
    :type   :relation
    :subc   (soft-drink . :slot2)
  :slot3
    :type     :relation
    :INST-OF  (soft-drink . :slot2)
    :c-coref  juice-is-sweet
)
(with-agent  sweet-taste  ; from KB/PHYSPROP
  :a-link   (juice 0.3)
)

(defagent  juice-is-sweet   instance-agent
  :type    (:instance :relation)
  :inst-of taste-of    ; from KB/SEMANTIC/PHYSPROP
  :c-coref (juice . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (taste-of . :slot1)
    :c-coref  juice     ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (taste-of . :slot2)
    :c-coref  swt-jc-taste    ; prototype
)
(defagent   swt-jc-taste   instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  sweet-taste     ; from KB/PHYSPROP
  :c-coref  (juice-is-sweet . :slot2)
)


;;;;  ******   ALCOHOLIC DRINKS   ******
;;

(defagent   alcoholic-drink    concept-agent
  :type     (:concept  :object)
  :subc     beverage
  :superc   ((beer 0.5)
             (wine 0.5)
             (hard-drink 1.0) )
  :a-link   ((soft-drink 0.5)
             (*other*    2.0) )
  :slot1
    :type   :aspect
    :subc   (beverage . :slot1)
  :slot2
    :type   :relation
    :subc   (beverage . :slot2)
)

(defagent   hard-drink    concept-agent
  :type     (:concept  :object)
  :subc     alcoholic-drink
  :a-link   ((soft-drink 0.5)
             (*other*    8.0) ) ; whisky, etc.
  :slot1
    :type   :aspect
    :subc   (alcoholic-drink . :slot1)
  :slot2
    :type   :relation
    :subc   (alcoholic-drink . :slot2)
)

(defagent   beer    concept-agent
  :type     (:concept  :object)
  :subc     alcoholic-drink
;;:instance #<genKB>
  :a-link   ((glass   0.3)  ; KB/SEMANTIC/CONTAIN
             (bottle  0.4) ); KB/SEMANTIC/CONTAIN
  :slot1
    :type   :aspect
    :subc   (alcoholic-drink . :slot1)
  :slot2
    :type   :relation
    :subc   (alcoholic-drink . :slot2)
)

(defagent   wine    concept-agent
  :type     (:concept  :object)
  :subc     alcoholic-drink
  :superc   (red-wine
             white-wine)
;;:instance #<genKB>
  :a-link   ((glass   0.3)  ; from KB/CONTAIN
             (bottle  0.4) ); from KB/CONTAIN
  :slot1
    :type   :aspect
    :subc   (alcoholic-drink . :slot1)
  :slot2
    :type   :relation
    :subc   (alcoholic-drink . :slot2)
)

(defagent   red-wine    concept-agent
  :type     (:concept  :object)
  :subc     wine
;;:instance #<genKB>
  :a-link   (white-wine 0.5)
  :slot1
    :type   :aspect
    :subc   (wine . :slot1)
  :slot2
    :type   :relation
    :subc   (wine . :slot2)
    ;; no c-coref ==> generic slot
  :slot3
    :type     :relation
    :INST-OF  (wine . :slot2)
    :c-coref  rwine-is-red  ; concrete property
)
(defagent  rwine-is-red   instance-agent
  :type    (:instance  :relation)
  :inst-of color-of    ; from KB/SEMANTIC/PHYSPROP
  :c-coref (red-wine . :slot3)
  :a-link  (wwine-is-white 1.0)
  :slot1
    :type     :aspect
    :SUBC     (color-of . :slot1)
    :c-coref  red-wine  ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (color-of . :slot2)
    :c-coref  red-wine-color      ; prototype
)
(defagent   red-wine-color   instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  red
  :c-coref  (rwine-is-red . :slot2)
  :a-link   (white-wine-color 0.5)
)

(defagent   white-wine    concept-agent
  :type     (:concept  :object)
  :subc     wine
;;:instance #<genKB>
  :a-link   (red-wine 0.5)
  :slot1
    :type   :aspect
    :subc   (wine . :slot1)
  :slot2
    :type   :relation
    :subc   (wine . :slot2)
    ;; no c-coref ==> generic slot
  :slot3
    :type     :relation
    :INST-OF  (wine . :slot2)
    :c-coref  wwine-is-white  ; concrete property
)
(defagent  wwine-is-white   instance-agent
  :type    (:instance  :relation)
  :inst-of color-of    ; from KB/PHYSPROP.LSP
  :c-coref (white-wine . :slot3)
  :a-link  (rwine-is-red 1.0)
  :slot1
    :type     :aspect
    :SUBC     (color-of . :slot1)
    :c-coref  white-wine  ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (color-of . :slot2)
    :c-coref  white-wine-color    ; prototype
)
(defagent   white-wine-color   instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  white
  :c-coref  (wwine-is-white . :slot2)
  :a-link   (red-wine-color 0.5)
)


;;;;  ******   G A S E S   ******
;;

(with-agent  gas   ; from KB/ABSTRACT.LSP
  :superc   ((air    1.0)
             (oxygen 0.8)
             (CO2    0.2)
             (steam  0.5)
            ) ; *other*
)

(defagent   air    concept-agent
  :type     (:concept  :object)
  :subc     gas
  :a-link   (*other* 5.0)
  :slot1
    :type   :aspect
    :subc   (gas . :slot1)
  :slot2
    :type   :relation
    :subc   (gas . :slot2)
)

(defagent   oxygen    concept-agent
  :type     (:concept  :object)
  :subc     gas
  :a-link   ((living-thing 0.2)
             ; fire  ; see KB/FOREST.LSP
             (*other* 4.0) )
  :slot1
    :type   :aspect
    :subc   (gas . :slot1)
  :slot2
    :type   :relation
    :subc   (gas . :slot2)
)

(defagent   CO2    concept-agent
  "Carbon dioxide"
  :type     (:concept  :object)
  :subc     gas
  :a-link   ((oxygen  0.5)
             (*other* 4.0) )
  :slot1
    :type   :aspect
    :subc   (gas . :slot1)
  :slot2
    :type   :relation
    :subc   (gas . :slot2)
)

(defagent   steam    concept-agent
  :type     (:concept  :object)
  :subc     gas
  :a-link   ((water 1.0)
             (*other* 3.0) )
  :slot1
    :type   :aspect
    :subc   (gas . :slot1)
  :slot2
    :type   :relation
    :subc   (gas . :slot2)
)


;;;;  *****   ICE AND ICE-CUBE   *****
;;
;;  ICE is defined here as a complement
;;  for WATER and STEAM and as a basis for
;;  ICE-CUBE which is associated with many
;;  soft drinks.

(defagent   ice    concept-agent
  "A block of ice, possibly quite large."
  :type     (:concept  :object)
  :subc     object     ; i.e. solid object
  :superc   ice-cube
  :a-link   ((water  1.0)
             (material-ice 1.0)
             (low-temp 0.5)  ; KB/SEMANTIC/PHYSPROP
             (*other*  1.0) )
  :slot1
    :type   :aspect
    :subc   (object . :slot1)
  :slot2
    :type   :relation
    :subc   (object . :slot2)
)
(with-agent  material-ice  ; KB/SEMANTIC/PHYS_REL
  :a-link    (ice 1.0)
)

(defagent   ice-cube    concept-agent
  :type      (:concept  :object)
  :subc      (ice
              (small-object 0.2) )
;;:instance  #<genKB>
  :a-link    ((soft-drink 0.5)
              (low-temp 0.5)  ; KB/SEMANTIC/PHYSPROP
             ) ; fridge       ; KB/SEMANTIC/KITCHEN
  :slot1
    :type   :aspect
    :subc   ( (ice . :slot1)
             ((small-object . :slot1) 0.2) )
  :slot2
    :type   :relation
    :subc   ( (ice . :slot2)
             ((small-object . :slot2) 0.2) )
)



;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references
  :exemption-id  'from-KB/CONTAIN )


;;;;;;  ---- Prepare for GENKB ---- ;;;;;;
;;

(GENKB-template
  :herald  "Concepts from KB/SEMANTIC/LIQUID.LSP, ver. 3.0.0."
  :templates '(
    (beer             (:instance (*other* 10)) )
    (coke             (:instance (*other* 10)) )
    (coffee           (:instance (*other* 10)) )
    (ice-cube         (:instance (*other* 10)) )
    (juice            (:instance (*other* 10)) )
    (milk             (:instance (*other* 10)) )
    (red-wine         (:instance (*other* 10)) )
    (tea              (:instance (*other* 10)) )
    (water            (:instance (*other* 10)) )
    (white-wine       (:instance (*other* 10)) )
    (wine             (:instance (*other* 10)) )
    ;; no templates for AIR, OXYGEN, STEAM, and CO2
 ;;;;;;;;
    (color-of         (:instance (white-wine-color 1)
                                 (red-wine-color   1)) )
    (red              (:instance (rwine-is-red     1)) )
    (white            (:instance (wwine-is-white   1)) )
    (taste-of         (:instance (juice-is-sweet   1)) )
    (sweet-taste      (:instance (swt-jc-taste     1)) )
))

#|
;;;;;;;;;;;;   Appendix   ;;;;;;;;;;
;;
;; This file defines the following 30 agents
;; (ordered by name):

air
alcoholic-drink
beverage
beer
CO2
coke
coffee
drinkable-liquid
hard-drink
ice
ice-cube
juice
juice-is-sweet
milk
non-drinkable-liquid
oil-based-liquid
oxygen
red-wine
red-wine-color
rwine-is-red
soft-drink
steam
swt-jc-taste
tea/coffee
tea
water
white-wine
white-wine-color
wine
wwine-is-white

|#

;;;;;;  End of file  AMBR/KB/SEMANTIC/LIQUID.LSP
