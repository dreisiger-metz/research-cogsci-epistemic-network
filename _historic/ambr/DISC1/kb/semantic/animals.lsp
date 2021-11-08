;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/semantic/animals.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Semantic memory -- concepts for different living things.
;;; DEPENDS-ON: ambr/packages.lsp, ambr/intrface/defagent.lsp, =/kb_util.lsp,
;;;             ambr/kb/abstract.lsp, =/physprop.lsp
;;; XREFS:      ambr/kb/food.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    24-04-98 [3.0]  Used parts of old LTM.LSP.
;;; UPDATED:    ...


            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;;;;;;;;       A N I M A L S       ;;;;;;;;;;
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  "AMBR")

;;;; This file defines concept agents for different kinds of living things
;;;; used in AMBR's semantic memory.  See also AMBR/KB/HUMAN.LSP.


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
;; by AMBR/KB/ABSTRACT.LSP, =/PHYS_REL.LSP and =/PHYSPROP.LSP.  These include:

(require-agents '(living-thing
                  part-of
                  color-of
                  green
                  white
                  *other*
))

;; In addition, this file has some cross-references with AMBR/KB/FOOD.LSP.

(reference-agents 'from-KB/FOOD
  '(fruit/vegetable
    meat
    egg
    dairy-product
))


;;;;  *****  GENERAL  CLASSES  *****
;;
;;  The base class LIVING-THING is defined
;;  in AMBR/KB/ABSTRACT.LSP.  Now it is
;;  time to fill its :SUPERC slot.

(with-agent  living-thing
  :superc   (plant
             animal
            ) ; human  ; see KB/HUMAN.LSP
)

(defagent   plant  concept-agent
  :type     (:concept  :object)
  :subc     living-thing
  :superc   (tree
             flower
             fruit/vegetable )  ; KB/FOOD
  :a-link   (*other* 4.0)
  :slot1
    :type   :aspect
    :subc   (living-thing . :slot1)
  :slot2
    :type   :relation
    :subc   (living-thing . :slot2)
)

(defagent   animal   concept-agent
  :type     (:concept  :object)
  :subc     living-thing
  :superc   (mammal
             fish
             bird  )
  :a-link   (*other* 4.0)
  :slot1
    :type   :aspect
    :subc   (living-thing . :slot1)
  :slot2
    :type   :relation
    :subc   (living-thing . :slot2)
)


;;;;   *****  T R E E S  *****
;;

(defagent   tree   concept-agent
  :type     (:concept  :object)
  :subc     plant
  :superc   ((oak   0.7)
             (pine  0.7)
             (birch 0.4)
             (palm-tree 0.2) )
  :a-link   (*other* 2.0)
  :slot1
    :type   :aspect
    :subc   (plant . :slot1)
  :slot2
    :type   :relation
    :subc   (plant . :slot2)
)

(defagent  oak   concept-agent
  :type     (:concept  :object)
  :subc     tree
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (tree . :slot1)
  :slot2
    :type   :relation
    :subc   (tree . :slot2)
)

(defagent  pine   concept-agent
  :type     (:concept  :object)
  :subc     tree
;;:instance #<genKB>
  :a-link   (green   0.2)
  :slot1
    :type   :aspect
    :subc   (tree . :slot1)
  :slot2
    :type   :relation
    :subc   (tree . :slot2)
)

(defagent  birch   concept-agent
  :type     (:concept  :object)
  :subc     tree
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (tree . :slot1)
  :slot2
    :type   :relation
    :subc   (tree . :slot2)
)

(defagent  palm-tree  concept-agent
  :type     (:concept  :object)
  :subc     tree
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (tree . :slot1)
  :slot2
    :type   :relation
    :subc   (tree . :slot2)
)


;;;;   *****  FLOWERS  *****
;;

(defagent   flower   concept-agent
  :type     (:concept  :object)
  :subc     plant
  :superc   ((rose     0.8)
             (tulip    0.5)
             (snowdrop 0.3) )
  :a-link   (*other* 3.0)
  :slot1
    :type   :aspect
    :subc   (plant . :slot1)
  :slot2
    :type   :relation
    :subc   (plant . :slot2)
)

(defagent  rose   concept-agent
  :type     (:concept  :object)
  :subc     flower
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (flower . :slot1)
  :slot2
    :type   :relation
    :subc   (flower . :slot2)
)

(defagent  tulip   concept-agent
  :type     (:concept  :object)
  :subc     flower
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (flower . :slot1)
  :slot2
    :type   :relation
    :subc   (flower . :slot2)
)

(defagent  snowdrop   concept-agent
  :type     (:concept  :object)
  :subc     flower
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (flower . :slot1)
  :slot2
    :type   :relation
    :subc   (flower . :slot2)
  :slot3
    :type      :relation
    :INST-OF   (flower . :slot2)
    :c-coref   snowdrop-is-white
)
(defagent   snowdrop-is-white instance-agent
  :type     (:instance  :relation)
  :inst-of  color-of
  :c-coref  (snowdrop . :slot3)
  :slot1
    :type     :aspect
    :SUBC     (color-of . :slot1)
    :c-coref  snowdrop  ; rage (i.e. variable)
  :slot2
    :type     :aspect
    :inst-of  (color-of . :slot2)
    :c-coref  white-snowdrop    ; prototype
)
(defagent   white-snowdrop  instance-agent
  :type     (:PROTOTYPE :instance  :object)
  :inst-of  white
  :c-coref  (snowdrop-is-white . :slot2)
  :a-link   (snowdrop 0.2)
)
(with-agent  white  ; from AMBR/KB/PHYSPROP.LSP
  :instance  (white-snowdrop 0.2)
)


;;;;   *****  FISH AND BIRDS  *****
;;

(defagent   fish   concept-agent
  :type     (:concept  :object)
  :subc     animal
  ;; no subclasses defined yet
  :a-link   (*other* 5.0)
  :slot1
    :type   :aspect
    :subc   (animal . :slot1)
  :slot2
    :type   :relation
    :subc   (animal . :slot2)
)

(defagent   bird   concept-agent
  :type     (:concept  :object)
  :subc     animal
  :superc   ((pigeon  0.7)
             (hen     0.7)
             (chicken 0.5) )
  :a-link   ((egg 1.0)   ; KB/SEMANTIC/FOOD
             (*other* 2.0) )
  :slot1
    :type   :aspect
    :subc   (animal . :slot1)
  :slot2
    :type   :relation
    :subc   (animal . :slot2)
)

(defagent  pigeon   concept-agent
  :type     (:concept  :object)
  :subc     bird
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (bird . :slot1)
  :slot2
    :type   :relation
    :subc   (bird . :slot2)
)

(defagent  hen   concept-agent
  :type     (:concept  :object)
  :subc     bird
;;:instance #<genKB>
  :a-link   (chicken 1.0)
  :slot1
    :type   :aspect
    :subc   (bird . :slot1)
  :slot2
    :type   :relation
    :subc   (bird . :slot2)
)

(defagent  chicken   concept-agent
  :type     (:concept  :object)
  :subc     bird
;;:instance #<genKB>
  :a-link   (egg 0.5)  ; KB/SEMANTIC/FOOD
  :slot1
    :type   :aspect
    :subc   (bird . :slot1)
  :slot2
    :type   :relation
    :subc   (bird . :slot2)
)


;;;;   *****   MAMMALS   *****
;;

(defagent   mammal   concept-agent
  :type     (:concept  :object)
  :subc     animal
  :superc   ((dog  0.7)
             (cat  0.7)
             (pig  0.3)
             (cow  0.3)
             (lion 0.5) )
  :a-link   (*other* 2.0)
  :slot1
    :type   :aspect
    :subc   (animal . :slot1)
  :slot2
    :type   :relation
    :subc   (animal . :slot2)
)

(defagent  dog  concept-agent
  :type     (:concept  :object)
  :subc     mammal
;;:instance #<genKB>
  :a-link   (cat 0.5)
  :slot1
    :type   :aspect
    :subc   (mammal . :slot1)
  :slot2
    :type   :relation
    :subc   (mammal . :slot2)
)

(defagent  cat  concept-agent
  :type     (:concept  :object)
  :subc     mammal
;;:instance #<genKB>
  :a-link   (dog 0.5)
  :slot1
    :type   :aspect
    :subc   (mammal . :slot1)
  :slot2
    :type   :relation
    :subc   (mammal . :slot2)
)

(defagent  pig  concept-agent
  :type     (:concept  :object)
  :subc     mammal
;;:instance #<genKB>
  :a-link   (meat 1.0)
  :slot1
    :type   :aspect
    :subc   (mammal . :slot1)
  :slot2
    :type   :relation
    :subc   (mammal . :slot2)
)

(defagent  cow  concept-agent
  :type     (:concept  :object)
  :subc     mammal
;;:instance #<genKB>
  :a-link   (dairy-product 1.0)
  :slot1
    :type   :aspect
    :subc   (mammal . :slot1)
  :slot2
    :type   :relation
    :subc   (mammal . :slot2)
)

(defagent  lion  concept-agent
  :type     (:concept  :object)
  :subc     mammal
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (mammal . :slot1)
  :slot2
    :type   :relation
    :subc   (mammal . :slot2)
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references
  :exemption-id  'from-KB/FOOD )


;;;;;;  ---- Prepare for GENKB ---- ;;;;;;
;;

(GENKB-template
  :herald  "Concepts from KB/SEMANTIC/ANIMALS.LSP, ver. 3.0.0."
  :templates '(
    (birch             (:instance (*other* 10)) )
    (cat               (:instance (*other* 10)) )
    (chicken           (:instance (*other* 10)) )
    (cow               (:instance (*other* 10)) )
    (dog               (:instance (*other* 10)) )
    (hen               (:instance (*other* 10)) )
    (lion              (:instance (*other* 10)) )
    (oak               (:instance (*other* 10)) )
    (palm-tree         (:instance (*other* 10)) )
    (pig               (:instance (*other* 10)) )
    (pigeon            (:instance (*other* 10)) )
    (pine              (:instance (*other* 10)) )
    (rose              (:instance (*other* 10)) )
    (snowdrop          (:instance (*other* 10)) )
    (tulip             (:instance (*other* 10)) )
 ;;;;;;;;
    (color-of  (:instance (snowdrop-is-white 1)) )
))

#|
;;;;;;;;;;;;   Appendix   ;;;;;;;;;;
;;
;; This file defines the following 24 agents
;; (ordered by name):

animal
birch
bird
cat
chicken
cow
dog
fish
flower
hen
lion
mammal
oak
palm-tree
pig
pigeon
pine
plant
rose
snowdrop
snowdrop-is-white
tree
tulip
white-snowdrop

|#

;;;;;;  End of file  AMBR/KB/SEMANTIC/ANIMALS.LSP
