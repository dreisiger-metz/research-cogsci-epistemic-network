;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/semantic/kitchen.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Semantic memory -- concepts related to kitchen and cooking.
;;; DEPENDS-ON: ambr/packages.lsp, ambr/intrface/defagent.lsp, =/kb_util.lsp,
;;;             ambr/kb/abstract.lsp, =/physprop.lsp, =/contain.lsp, =/food.lsp
;;; XREFS:      none
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    27-04-98 [3.0]  Used parts of old LTM.LSP.
;;; UPDATED:    ...


           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;;;;;   K I T C H E N   A N D   C O O K I N G   ;;;;;
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  "AMBR")

;;;; This file defines concept agents for different kinds of dishes and cooking
;;;; appliancies used in AMBR's semantic memory.


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
;; by AMBR/KB/ABSTRACT.LSP, =/PHYSPROP.LSP, =/CONTAIN.LSP, and =/FOOD.LSP.
;; These include:

(require-agents '(thing
                  object
                  artifact
                  made-of
                  material-metal
                  temperature-of
                  high-temp
                  food
                  food-holder
                  liquid-holder
                  plate
                  *other*
))


;;;;   ******  COOKING VESSELS ******
;;
;;  This section defines some vessels used
;;  for cooking.  See AMBR/KB/CONTAIN.LSP
;;  for general containers, liquid and
;;  food holders, etc.
;;
;;  All cooking vessels (defined here) are
;;  made of metal. This property is implicitly
;;  inherited by all subclasses.

(defagent   cook-vessel   concept-agent
  "A metallic vessel used for cooking."
  :type     (:concept  :object)
  :subc     food-holder       ; KB/CONTAIN.LSP
  :superc   ((teapot 0.5)
             (saucepan 0.5)
             (pan 0.3)
             (baking-dish 0.3) )
  :a-link   ((mmetal-ckves 0.2)
             (*other*      1.0) )
  :slot1
    :type   :aspect
    :subc   (food-holder . :slot1)
  :slot2
    :type   :relation
    :subc   (food-holder . :slot2)
    ;; no c-coref ==> generic slot
  :slot3  "All cooking vessels are made of metal."
    :type      :relation      ; --vvv--
    :INST-OF   (food-holder . :slot2)
    :c-coref   ckves-md-metal   ; concrete prop.
)
(with-agent  food-holder  ; KB/SEMANTIC/CONTAIN.LSP
  :superc   cook-vessel
)
(defagent  ckves-md-metal  instance-agent
  "All cooking vessels are made of metal."
  :type    (:instance  :relation)
  :inst-of made-of
  :c-coref (cook-vessel . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (made-of . :slot1)
    :c-coref  cook-vessel   ; range (i.e. var)
  :slot2
    :type     :aspect
    :inst-of  (made-of . :slot2)
    :c-coref  mmetal-ckves  ; prototype
)
(defagent   mmetal-ckves  instance-agent
  "Prototypical material for cooking vessels."
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  material-metal
  :c-coref  (ckves-md-metal . :slot2)
  :a-link   (cook-vessel 0.2)
)

(defagent   teapot   concept-agent
  :type     (:concept  :object)
  :subc     (cook-vessel
             liquid-holder )
;;:instance #<genKB>
  :a-link  ((ckves-md-metal 0.5)
            (mmetal-ckves   0.2) )
  :slot1
    :type   :aspect
    :subc   ((cook-vessel   . :slot1)
             (liquid-holder . :slot1) )
  :slot2
    :type   :relation
    :subc   ((cook-vessel   . :slot2)
             (liquid-holder . :slot2) )
 ;; slot3 implicitly inherited from COOK-VESSEL
)

(defagent    saucepan   concept-agent
  :type      (:concept  :object)
  :subc     (cook-vessel
             liquid-holder )
;;:instance #<genKB>
  :a-link  ((ckves-md-metal 0.5)
            (mmetal-ckves   0.2) )
  :slot1
    :type   :aspect
    :subc   ((cook-vessel   . :slot1)
             (liquid-holder . :slot1) )
  :slot2
    :type   :relation
    :subc   ((cook-vessel   . :slot2)
             (liquid-holder . :slot2) )
 ;; slot3 implicitly inherited from COOK-VESSEL
)

(defagent    pan   concept-agent
  :type      (:concept  :object)
  :subc      cook-vessel
;;:instance #<genKB>
  :a-link  ((ckves-md-metal 0.5)
            (mmetal-ckves   0.2) )
  :slot1
    :type   :aspect
    :subc   (cook-vessel . :slot1)
  :slot2
    :type   :relation
    :subc   (cook-vessel . :slot2)
 ;; slot3 implicitly inherited from COOK-VESSEL
)

(defagent    baking-dish   concept-agent
  :type      (:concept  :object)
  :subc      cook-vessel
;;:instance #<genKB>
  :a-link  ((ckves-md-metal 0.5)
            (mmetal-ckves   0.2) )
  :slot1
    :type   :aspect
    :subc   (cook-vessel . :slot1)
  :slot2
    :type   :relation
    :subc   (cook-vessel . :slot2)
 ;; slot3 implicitly inherited from COOK-VESSEL
)


;;;;   ******  KITCHEN EQUIPMENT  ******
;;

(defagent   kitch-equipmt   concept-agent
  :type     (:concept  :object)
  :subc     artifact
  :superc   ((hot-plate 1.0)
             (fridge 1.0)
             (oven   0.8)
             (dish-washer 0.3) )
  :a-link   (*other* 1.0)
  :slot1
    :type   :aspect
    :subc   (artifact . :slot1)
  :slot2
    :type   :relation
    :subc   (artifact . :slot2)
)

(defagent   dish-washer      concept-agent
  :type     (:concept  :object)
  :subc     ((kitch-equipmt 0.7)
             (el-appliance  0.3) )  ; see below
;;:instance #<genKB>
  :a-link   ((plate 0.5)
             (cook-vessel 0.5) )
  :slot1
    :type   :aspect
    :subc   (((kitch-equipmt . :slot1) 0.7)
             ((el-appliance  . :slot1) 0.3) )
  :slot2
    :type   :relation
    :subc   (((kitch-equipmt . :slot2) 0.7)
             ((el-appliance  . :slot2) 0.3) )
)

(defagent  fridge     concept-agent
  :type      (:concept  :object)
  :subc      ((kitch-equipmt 1.0)
              (el-appliance  0.2) )
;;:instance  #<genKB>
  :a-link    ((ice-cube 0.2) ; KB/SEMANTIC/LIQUID
              (coke 0.1) )   ; KB/SEMANTIC/LIQUID
  :slot1
    :type   :aspect
    :subc   (((kitch-equipmt . :slot1) 1.0)
             ((el-appliance  . :slot1) 0.2) )
  :slot2
    :type   :relation
    :subc   (((kitch-equipmt . :slot2) 1.0)
             ((el-appliance  . :slot2) 0.2) )
    ;; no c-coref ==> generic slot
  :slot3   "All fridges are cold."
    :type     :relation          ; --vvv--
    :INST-OF  (((kitch-equipmt . :slot2) 1.0)
               ((el-appliance  . :slot2) 0.2) )
    :c-coref  fridge-is-cold   ; concr. property
)
(defagent   fridge-is-cold    instance-agent
  :type     (:instance  :relation)
  :inst-of  temperature-of
  :c-coref  (fridge . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (temperature-of . :slot1)
    :c-coref  fridge   ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (temperature-of . :slot2)
    :c-coref  low-T-fridge      ; prototype
)
(defagent   low-T-fridge   instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  low-temp
  :c-coref  (fridge-is-cold . :slot2)
  :a-link   (fridge 0.2)
)

(with-agent  ice-cube
  :a-link   (fridge 0.2)
)


;;;;   ******  HEAT SOURCES  ******
;;
;; All heat sources are hot and this
;; property is inherited implicitly.

(defagent   heat-source    concept-agent
  :type    (:concept  :object)
  :subc    thing
  :superc  ((hot-plate 1.0)
            (oven      0.5)
            (immersion-heater 0.3)
           ) ; (fire 0.2)  ; see KB/FOREST
  :a-link  ((high-T-htsrc 0.2)
            (*other*      1.0) )
  :slot1  "Part(s)"
    :type   :aspect
    :subc   (thing . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (thing . :slot2)
    ;; no c-coref ==> generic slot
  :slot3   "All heat sources are hot."
    :type     :relation
    :INST-OF  (thing . :slot2)
    :c-coref  htsrc-is-hot
)
(defagent   htsrc-is-hot  instance-agent
  :type     (:instance  :relation)
  :inst-of  temperature-of
  :c-coref  (heat-source . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (temperature-of . :slot1)
    :c-coref  heat-source  ; range (i.e. var)
  :slot2
    :type     :aspect
    :inst-of  (temperature-of . :slot2)
    :c-coref  high-T-htsrc    ; prototype
)
(defagent   high-T-htsrc   instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  high-temp
  :c-coref  (htsrc-is-hot . :slot2)
  :a-link   (heat-source 0.2)
)

(defagent    hot-plate    concept-agent
  "Don't confuse with PLATE (dish)."
  :type     (:concept  :object)
  :subc     ((heat-source   1.0)
             (kitch-equipmt 0.5)
             (el-appliance  0.1) )
;;:instance #<genKB>
  :a-link   ((htsrc-is-hot 0.5)
             (high-T-htsrc 0.3) )
  :slot1
    :type   :aspect
    :subc   (((heat-source   . :slot1) 1.0)
             ((kitch-equipmt . :slot1) 0.5)
             ((el-appliance  . :slot1) 0.1) )
  :slot2
    :type   :relation
    :subc   (((heat-source   . :slot2) 1.0)
             ((kitch-equipmt . :slot2) 0.5)
             ((el-appliance  . :slot2) 0.1) )
  ;; slot 3 implicitly inherited
)

(defagent    oven      concept-agent
  :type     (:concept  :object)
  :subc     ((heat-source   1.0)
             (kitch-equipmt 1.0)
             (el-appliance  0.1) )
;;:instance #<genKB>
  :a-link   ((htsrc-is-hot 0.5)
             (high-T-htsrc 0.3) )
  :slot1
    :type   :aspect
    :subc   (((heat-source   . :slot1) 1.0)
             ((kitch-equipmt . :slot1) 1.0)
             ((el-appliance  . :slot1) 0.1) )
  :slot2
    :type   :relation
    :subc   (((heat-source   . :slot2) 1.0)
             ((kitch-equipmt . :slot2) 1.0)
             ((el-appliance  . :slot2) 0.1) )
  ;; slot 3 implicitly inherited
)

(defagent  immersion-heater    concept-agent
  :type     (:concept  :object)
  :subc     ((heat-source   1.0)
             (el-appliance  0.1) )
;;:instance #<genKB>
  :a-link   ((htsrc-is-hot 0.5)
             (high-T-htsrc 0.3) )
  :slot1
    :type   :aspect
    :subc   (((heat-source   . :slot1) 1.0)
             ((el-appliance  . :slot1) 0.1) )
  :slot2
    :type   :relation
    :subc   (((heat-source   . :slot2) 1.0)
             ((el-appliance  . :slot2) 0.1) )
  ;; slot 3 implicitly inherited
)


;;;;   ******  ELECTRICAL APPLIANCES  ******
;;
;;  This section defines some additional el.
;;  appliances that are not confined to
;;  kitchens alone.

(defagent   el-appliance  concept-agent
  :type     (:concept  :object)
  :subc     artifact
  :superc   ((fridge 0.2)
             (hot-plate 0.4)
             (lamp   0.4)
             (TV-set 0.4)  )
  :a-link   (*other* 2.0)
  :slot1
    :type   :aspect
    :subc   (artifact . :slot1)
  :slot2
    :type   :relation
    :subc   (artifact . :slot2)
)

(defagent   light-source    concept-agent
  :type     (:concept  :object)
  :subc     thing
  :superc   ((lamp  1.0)
            ) ; see KB/FOREST for more
  :a-link   ((heat-source 0.5)
             (*other*     2.0) )
  :slot1
    :type   :aspect
    :subc   (thing . :slot1)
  :slot2
    :type   :relation
    :subc   (thing . :slot2)
)

(defagent   lamp   concept-agent
  :type     (:concept  :object)
  :subc     ((light-source 1.0)
             (el-appliance 0.5) )
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (((light-source . :slot1) 1.0)
             ((el-appliance . :slot1) 0.5) )
  :slot2
    :type   :relation
    :subc   (((light-source . :slot2) 1.0)
             ((el-appliance . :slot2) 0.5) )
)

(defagent   TV-set   concept-agent
  :type     (:concept  :object)
  :subc     el-appliance
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   el-appliance
  :slot2
    :type   :relation
    :subc   el-appliance
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references  )


;;;;;;  ---- Prepare for GENKB ---- ;;;;;;
;;

(GENKB-template
  :herald  "Concepts from KB/SEMANTIC/KITCHEN.LSP, ver. 3.0.0."
  :templates '(
    (baking-dish      (:instance (*other* 10)) )
    (dish-washer      (:instance (*other* 10)) )
    (fridge           (:instance (*other* 10)) )
    (hot-plate        (:instance (*other* 10)) )
    (immersion-heater (:instance (*other* 10)) )
    (lamp             (:instance (*other* 10)) )
    (oven             (:instance (*other* 10)) )
    (pan              (:instance (*other* 10)) )
    (saucepan         (:instance (*other* 10)) )
    (teapot           (:instance (*other* 10)) )
    (TV-set           (:instance (*other* 10)) )
 ;;;;;;;;
    (temperature-of   (:instance (fridge-is-cold 2)
                                 (htsrc-is-hot   2)) )
    (low-temp         (:instance (low-T-fridge   2)) )
    (high-temp        (:instance (high-T-htsrc   2)) )
    (made-of          (:instance (ckves-md-metal 1)) )
    (material-metal   (:instance (mmetal-ckves   1)) )
))

#|
;;;;;;;;;;;;   Appendix   ;;;;;;;;;;
;;
;; This file defines the following 22 agents
;; (ordered by name):

baking-dish
ckves-md-metal
low-T-fridge
cook-vessel
dish-washer
el-appliance
fridge
fridge-is-cold
heat-source
high-T-htsrc
hot-plate
htsrc-is-hot
immersion-heater
kitch-equipment
lamp
light-source
mmetal-ckves
oven
pan
saucepan
teapot
TV-set

|#

;;;;;;  End of file  AMBR/KB/SEMANTIC/KITCHEN.LSP
