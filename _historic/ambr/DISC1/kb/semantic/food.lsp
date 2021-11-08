;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/semantic/food.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Semantic memory -- concepts for bread, vegetables, soups, etc.
;;; DEPENDS-ON: ambr/packages.lsp, ambr/intrface/defagent.lsp, =/kb_util.lsp,
;;;             ambr/kb/abstract.lsp, =/physprop.lsp, =/liquid.lsp,=/contain.lsp
;;; XREFS:      ambr/kb/animals.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    24-04-98 [3.0]  Used parts of old LTM.LSP.
;;; UPDATED:    27-06-98 -- Added SPICE, SALT, SUGAR, SUGAR-IS-SWEET, etc.
;;; UPDATED:    ...


            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;;;;;;;;;       F  O  O  D       ;;;;;;;;;;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  "AMBR")

;;;; This file defines concept agents for different kinds of food used in AMBR's
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


;; The agents defined in this file depend on some concept-agents provided
;; by AMBR/KB/ABSTRACT.LSP, =/LIQUID.LSP, and =/CONTAIN.LSP.  These include:

(require-agents '(object
                  milk
                  food-holder
                  color-of
                  taste-of
                  *other*
))

;; In addition, this file has some cross-references with AMBR/KB/ANIMALS.LSP.

(reference-agents 'from-KB/ANIMALS
  '(plant
    animal
    chicken
    cow
))


;;;;   ******  GENERAL  CLASS  ******
;;

(defagent   food   concept-agent
  :type     (:concept  :object)
  :subc     object
  :superc   ((bread 0.5)
             (fruit/vegetable 0.5)
             (meat  0.5)
             (soup  0.3)
             (spice 0.1)
             (dairy-product 0.3) )
;;:instance #<genKB>
  :a-link   ((food-holder 0.5)
             (taste-of 0.5) )  ; KB/PHYSPROP
  :slot1  "Aspect(s)"
    :type   :aspect
    :subc   (object . :slot1)
  :slot2  "Relation(s)"
    :type   :relation
    :subc   (object . :slot2)
)
(with-agent  food-holder  ; from KB/CONTAIN
  :a-link   (food 1.0)
)
(with-agent  taste-of     ; from KB/PHYSPROP
  :a-link   (food 1.0)
)


;;;;   ***** BREAD, MEAT, AND EGGS  *****
;;
;;  Can be expanded considerably.

(defagent   bread   concept-agent
  :type     (:concept  :object)
  :subc     food
;;:instance #<genKB>
  :a-link   (butter  0.2)
  :slot1
    :type   :aspect
    :subc   (food . :slot1)
  :slot2
    :type   :relation
    :subc   (food . :slot2)
)

(defagent   meat   concept-agent
  :type     (:concept  :object)
  :subc     food
;;:instance #<genKB>
  :a-link   ((animal 1.0)  ; see KB/ANIMALS
             (meat-soup 0.5) )
  :slot1
    :type   :aspect
    :subc   (food . :slot1)
  :slot2
    :type   :relation
    :subc   (food . :slot2)
)

(defagent   egg   concept-agent
  :type     (:concept  :object)
  :subc     food
;;:instance #<genKB>
  :a-link   (chicken 1.0)  ; see KB/ANIMALS
  :slot1
    :type   :aspect
    :subc   (food . :slot1)
  :slot2
    :type   :relation
    :subc   (food . :slot2)
)


;;;;   *****  DAIRY PRODUCTS  *****
;;

(defagent  dairy-product   concept-agent
  :type     (:concept  :object)
  :subc     food
  :superc   ((milk    1.0)
             (cheese  1.0)
             (butter  0.5) )
  :a-link   ((cow     1.0)   ; KB/ANIMALS
             (*other* 2.0) )
  :slot1
    :type   :aspect
    :subc   (food . :slot1)
  :slot2
    :type   :relation
    :subc   (food . :slot2)
)
(with-agent  milk    ; from KB/LIQUID.LSP
  :subc     dairy-product
  :a-link   (cheese 0.5)
  :slot1
    :subc   (dairy-product . :slot1)
  :slot2
    :subc   (dairy-product . :slot2)
)

(defagent  cheese   concept-agent
  :type     (:concept  :object)
  :subc     dairy-product
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (dairy-product . :slot1)
  :slot2
    :type   :relation
    :subc   (dairy-product . :slot2)
)

(defagent  butter   concept-agent
  :type     (:concept  :object)
  :subc     dairy-product
;;:instance #<genKB>
  :a-link   (bread 0.5)
  :slot1
    :type   :aspect
    :subc   (dairy-product . :slot1)
  :slot2
    :type   :relation
    :subc   (dairy-product . :slot2)
)


;;;;  ****  FRUITS AND VEGETABLES  ****
;;

(defagent   fruit/vegetable    concept-agent
  "Common superclass of FRUIT & VEGETABLE."
  :type     (:concept  :object)
  :subc     (food
             plant )  ; from KB/ANIMALS.LSP
  :superc   (fruit
             vegetable )
  :slot1
    :type   :aspect
    :subc   ((food  . :slot1)
             (plant . :slot1) )
  :slot2
    :type   :relation
    :subc   ((food  . :slot2)
             (plant . :slot2) )
)

(defagent  fruit   concept-agent
  :type     (:concept  :object)
  :subc     fruit/vegetable
  :superc   ((apple   1.0)
             (pear    0.8)
             (peach   0.5)
             (orange  0.5) )
  :a-link   ((juice   0.5)   ; KB/LIQUID
             (*other* 1.0) )
  :slot1
    :type   :aspect
    :subc   (fruit/vegetable . :slot1)
  :slot2
    :type   :relation
    :subc   (fruit/vegetable . :slot2)
  :slot3
    :type     :relation
    :INST-OF  (fruit/vegetable . :slot2)
    :c-coref  fruit-is-sweet
)
(with-agent  juice        ; from KB/LIQUID
  :a-link   (fruit 1.0)
)
(with-agent  sweet-taste  ; from KB/PHYSPROP
  :a-link   (fruit 0.5)
)

(defagent  fruit-is-sweet   instance-agent
  :type    (:instance :relation)
  :inst-of taste-of    ; from KB/SEMANTIC/PHYSPROP
  :c-coref (fruit . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (taste-of . :slot1)
    :c-coref  fruit     ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (taste-of . :slot2)
    :c-coref  swt-fr-taste    ; prototype
)
(defagent   swt-fr-taste   instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  sweet-taste     ; from KB/PHYSPROP
  :c-coref  (fruit-is-sweet . :slot2)
)
;; Of course, not all fruits are sweet. Exceptions
;; can be handled by more specific rules shadowing
;; the one here.  E.g. GRAPEFRUIT-IS-SOUR.


(defagent  vegetable   concept-agent
  :type     (:concept  :object)
  :subc     fruit/vegetable
  :superc   ((tomato 1.0)
             (pepper 1.0)
             (potato 0.8)
             (onion  0.5) )
  :a-link   ((vegetable-soup 0.5)
             (*other* 1.0) )
  :slot1
    :type   :aspect
    :subc   (fruit/vegetable . :slot1)
  :slot2
    :type   :relation
    :subc   (fruit/vegetable . :slot2)
)

(defagent  apple   concept-agent
  :type     (:concept  :object)
  :subc     fruit
;;:instance #<genKB>
  :a-link   (red 0.1)
  :slot1
    :type   :aspect
    :subc   (fruit . :slot1)
  :slot2
    :type   :relation
    :subc   (fruit . :slot2)
)

(defagent  pear   concept-agent
  :type     (:concept  :object)
  :subc     fruit
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (fruit . :slot1)
  :slot2
    :type   :relation
    :subc   (fruit . :slot2)
)

(defagent  orange   concept-agent
  :type     (:concept  :object)
  :subc     fruit
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (fruit . :slot1)
  :slot2
    :type   :relation
    :subc   (fruit . :slot2)
)

(defagent  peach   concept-agent
  :type     (:concept  :object)
  :subc     fruit
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (fruit . :slot1)
  :slot2
    :type   :relation
    :subc   (fruit . :slot2)
)

(defagent  pepper   concept-agent
  :type     (:concept  :object)
  :subc     vegetable
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (vegetable . :slot1)
  :slot2
    :type   :relation
    :subc   (vegetable . :slot2)
)

(defagent  potato   concept-agent
  :type     (:concept  :object)
  :subc     vegetable
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (vegetable . :slot1)
  :slot2
    :type   :relation
    :subc   (vegetable . :slot2)
)

(defagent  onion   concept-agent
  :type     (:concept  :object)
  :subc     vegetable
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (vegetable . :slot1)
  :slot2
    :type   :relation
    :subc   (vegetable . :slot2)
)

(defagent  tomato   concept-agent
  :type     (:concept  :object)
  :subc     vegetable
;;:instance #<genKB>
  :a-link   (red 0.3)
  :slot1
    :type   :aspect
    :subc   (vegetable . :slot1)
  :slot2
    :type   :relation
    :subc   (vegetable . :slot2)
  :slot3
    :type      :relation    ; --vvv--
    :INST-OF   (vegetable . :slot2)
    :c-coref   tomato-is-red
)
(defagent   tomato-is-red  instance-agent
  :type     (:instance  :relation)
  :inst-of  color-of
  :c-coref  (tomato . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (color-of . :slot1)
    :c-coref  tomato  ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (color-of . :slot2)
    :c-coref  red-tomato      ; prototype
)
(defagent   red-tomato  instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  red
  :c-coref  (tomato-is-red . :slot2)
  :a-link   (tomato 0.2)
)


;;;;  ****   S O U P S   ****
;;

(defagent   soup   concept-agent
  :type     (:concept  :object)
  :subc     food
  :superc   ((vegetable-soup 0.7)
             (meat-soup    0.7)
             (chicken-soup 0.7) )
  :a-link   ((liquid  1.0)
             (*other* 3.0) )
  :slot1
    :type   :aspect
    :subc   (food . :slot1)
  :slot2
    :type   :relation
    :subc   (food . :slot2)
)

(defagent  vegetable-soup  concept-agent
  :type     (:concept  :object)
  :subc     soup
;;:instance #<genKB>
  :a-link   (vegetable 1.0)
  :slot1
    :type   :aspect
    :subc   (soup . :slot1)
  :slot2
    :type   :relation
    :subc   (soup . :slot2)
)

(defagent  meat-soup  concept-agent
  :type     (:concept  :object)
  :subc     soup
;;:instance #<genKB>
  :a-link   (meat 1.0)
  :slot1
    :type   :aspect
    :subc   (soup . :slot1)
  :slot2
    :type   :relation
    :subc   (soup . :slot2)
)

(defagent  chicken-soup  concept-agent
  :type     (:concept  :object)
  :subc     soup
;;:instance #<genKB>
  :a-link   (chicken 1.0) ; KB/ANIMALS
  :slot1
    :type   :aspect
    :subc   (soup . :slot1)
  :slot2
    :type   :relation
    :subc   (soup . :slot2)
)

;;;;  ****   S P I C E S  ****
;;

(defagent   spice   concept-agent
  :type     (:concept  :object)
  :subc     food
  :superc   ((salt    0.5)
             (sugar   0.5) )
  :a-link   ((taste-of 0.5)   ; KB/PHYSPROP
             (*other*  3.0) )
  :slot1
    :type   :aspect
    :subc   (food . :slot1)
  :slot2
    :type   :relation
    :subc   (food . :slot2)
)

(defagent  salt    concept-agent
  :type     (:concept  :object)
  :subc     spice
;;:instance #<genKB>
  :a-link   ((salt-taste 1.0)   ; KB/PHYSPROP
             (prtype-salt-taste 0.3)
             (sugar 0.2) )
  :slot1
    :type   :aspect
    :subc   (spice . :slot1)
  :slot2
    :type   :relation
    :subc   (spice . :slot2)
  :slot3
    :type     :relation
    :INST-OF  (spice . :slot2)
    :c-coref  salt-is-salty
)

(defagent  salt-is-salty   instance-agent
  :type    (:instance :relation)
  :inst-of taste-of    ; from KB/SEMANTIC/PHYSPROP
  :c-coref (salt . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (taste-of . :slot1)
    :c-coref  salt     ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (taste-of . :slot2)
    :c-coref  prtype-salt-taste
)
(defagent   prtype-salt-taste   instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  salt-taste     ; from KB/PHYSPROP
  :c-coref  (salt-is-salty . :slot2)
)
(with-agent  salt-taste  ; KB/SEMANTIC/PHYSPROP.LSP
  :instance prtype-salt-taste
  :a-link   (salt 1.0)
)

(defagent  sugar    concept-agent
  :type     (:concept  :object)
  :subc     spice
;;:instance #<genKB>
  :a-link   ((sweet-taste 1.0)   ; KB/PHYSPROP
             (sugar-taste 0.3)
             (salt 0.2) )
  :slot1
    :type   :aspect
    :subc   (spice . :slot1)
  :slot2
    :type   :relation
    :subc   (spice . :slot2)
  :slot3
    :type     :relation
    :INST-OF  (spice . :slot2)
    :c-coref  sugar-is-sweet
)

(defagent  sugar-is-sweet   instance-agent
  :type    (:instance :relation)
  :inst-of taste-of    ; from KB/SEMANTIC/PHYSPROP
  :c-coref (sugar . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (taste-of . :slot1)
    :c-coref  sugar     ; range (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (taste-of . :slot2)
    :c-coref  sugar-taste
)
(defagent   sugar-taste   instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  sweet-taste     ; from KB/PHYSPROP
  :c-coref  (sugar-is-sweet . :slot2)
)
(with-agent  sweet-taste  ; KB/SEMANTIC/PHYSPROP.LSP
  :instance sugar-taste
  :a-link   (sugar 1.0)
)



;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references
  :exemption-id  'from-KB/ANIMALS )


;;;;;;  ---- Prepare for GENKB ---- ;;;;;;
;;

(GENKB-template
  :herald  "Concepts from KB/SEMANTIC/FOOD.LSP, ver. 3.0.0."
  :templates '(
    (apple          (:instance (*other* 10)) )
    (bread          (:instance (*other* 10)) )
    (butter         (:instance (*other* 10)) )
    (cheese         (:instance (*other* 10)) )
    (chicken-soup   (:instance (*other* 10)) )
    (egg            (:instance (*other* 10)) )
    (food           (:instance (*other* 10)) )  ; e.g. KB/EPISODIC/B_FDO.LSP
    (meat           (:instance (*other* 10)) )
    (meat-soup      (:instance (*other* 10)) )
    (onion          (:instance (*other* 10)) )
    (orange         (:instance (*other* 10)) )
    (peach          (:instance (*other* 10)) )
    (pear           (:instance (*other* 10)) )
    (pepper         (:instance (*other* 10)) )
    (potato         (:instance (*other* 10)) )
    (salt           (:instance (*other* 10)) )
    (sugar          (:instance (*other* 10)) )
    (tomato         (:instance (*other* 10)) )
    (vegetable-soup (:instance (*other* 10)) )
 ;;;;;;;;
    (color-of       (:instance (tomato-is-red  1)) )
    (red            (:instance (red-tomato     1)) )
    (taste-of       (:instance (fruit-is-sweet 1)
                               (salt-is-salty  1) (sugar-is-sweet 1)) )
    (sweet-taste    (:instance (swt-fr-taste   1)) )
                     ;; SUGAR-TASTE and PRTYPE-SALT-TASTE have permanent links
))

#|
;;;;;;;;;;;;   Appendix   ;;;;;;;;;;
;;
;; This file defines the following 33 agents
;; (ordered by name):

apple
bread
butter
cheese
chicken-soup
dairy-product
egg
food
fruit
fruit-is-sweet
fruit/vegetable
meat
meat-soup
onion
orange
peach
pear
pepper
potato
prtype-salt-taste
red-tomato
salt
salt-is-salty
soup
spice
sugar
sugar-is-sweet
sugar-taste
swt-fr-taste
tomato
tomato-is-red
vegetable
vegetable-soup

|#

;;;;;;  End of file  AMBR/KB/SEMANTIC/FOOD.LSP
