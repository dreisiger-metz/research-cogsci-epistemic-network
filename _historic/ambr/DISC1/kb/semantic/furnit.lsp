;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/semantic/furnit.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Semantic memory -- concepts related to furntiture.
;;; DEPENDS-ON: ambr/packages.lsp, ambr/intrface/defagent.lsp, =/kb_util.lsp,
;;;             ambr/kb/abstract.lsp
;;; XREFS:      none
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    24-06-98 [3.0.0]
;;; UPDATED:    ...


           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;;;;;;;;;       F U R N I T U R E        ;;;;;;;;;
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  "AMBR")

;;;; This file defines concept agents for different kinds of furniture used
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


;; The agents defined in this file depend on the concept ARTIFACT defined
;; in AMBR/KB/ABSTRACT.LSP:

(require-agents '(artifact
                  *other*
))


;;;;   ******  FURNITURE  ******
;;

(defagent   furniture  concept-agent
  :type     (:concept :object)
  :subc     artifact
  :superc   ((sleep-furniture 0.8)
             (sit-furniture   0.8)
             (write-furniture 0.7)
             (store-furniture 0.5) )
  :a-link   (*other* 1.0)
  :slot1
    :type   :aspect
    :subc   (artifact . :slot1)
  :slot2
    :type   :relation
    :subc   (artifact . :slot2)
)

(defagent   sleep-furniture  concept-agent
  :type     (:concept :object)
  :subc     furniture
  :superc   ((bed    1.0)
             (couch  0.5) )
  :a-link   (*other* 2.0)
  :slot1
    :type   :aspect
    :subc   (furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (furniture . :slot2)
)

(defagent   bed       concept-agent
  :type     (:concept :object)
  :subc     sleep-furniture
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (sleep-furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (sleep-furniture . :slot2)
)

(defagent   couch     concept-agent
  :type     (:concept :object)
  :subc     sleep-furniture
;;:instance #<genKB>
  :a-link   (bed 0.5)
  :slot1
    :type   :aspect
    :subc   (sleep-furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (sleep-furniture . :slot2)
)

(defagent   sit-furniture  concept-agent
  :type     (:concept :object)
  :subc     furniture
  :superc   ((chair    1.0)
             (armchair 0.5)
             (sofa     0.5) )
  :a-link   (*other* 2.0)
  :slot1
    :type   :aspect
    :subc   (furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (furniture . :slot2)
)

(defagent   chair     concept-agent
  :type     (:concept :object)
  :subc     sit-furniture
;;:instance #<genKB>
  :a-link   (table 0.5)
  :slot1
    :type   :aspect
    :subc   (sit-furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (sit-furniture . :slot2)
)

(defagent   armchair  concept-agent
  :type     (:concept :object)
  :subc     sit-furniture
;;:instance #<genKB>
  :a-link   (chair 1.0)
  :slot1
    :type   :aspect
    :subc   (sit-furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (sit-furniture . :slot2)
)

(defagent   sofa      concept-agent
  :type     (:concept :object)
  :subc     sit-furniture
;;:instance #<genKB>
  :a-link   ((chair 0.3)
             (bed   0.3) )
  :slot1
    :type   :aspect
    :subc   (sit-furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (sit-furniture . :slot2)
)

(defagent   write-furniture  concept-agent
  :type     (:concept :object)
  :subc     furniture
  :superc   ((table  1.0)
             (desk   0.8) )
  :a-link   (*other* 1.0)
  :slot1
    :type   :aspect
    :subc   (furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (furniture . :slot2)
)

(defagent   table     concept-agent
  :type     (:concept :object)
  :subc     write-furniture
;;:instance #<genKB>
  :a-link   (chair 0.5)
  :slot1
    :type   :aspect
    :subc   (write-furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (write-furniture . :slot2)
)

(defagent   desk      concept-agent
  :type     (:concept :object)
  :subc     write-furniture
;;:instance #<genKB>
  :a-link   (table 0.5)
  :slot1
    :type   :aspect
    :subc   (write-furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (write-furniture . :slot2)
)

(defagent   store-furniture  concept-agent
  :type     (:concept :object)
  :subc     furniture
  :superc   ((shelf    0.5)
             (wardrobe 0.5)
             (cupboard 0.5) )
  :a-link   (*other* 2.0)
  :slot1
    :type   :aspect
    :subc   (furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (furniture . :slot2)
)

(defagent   shelf     concept-agent
  :type     (:concept :object)
  :subc     store-furniture
;;:instance #<genKB>
  :slot1
    :type   :aspect
    :subc   (store-furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (store-furniture . :slot2)
)

(defagent   wardrobe  concept-agent
  :type     (:concept :object)
  :subc     store-furniture
;;:instance #<genKB>
  :a-link   (bed 0.5)
  :slot1
    :type   :aspect
    :subc   (store-furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (store-furniture . :slot2)
)

(defagent   cupboard  concept-agent
  :type     (:concept :object)
  :subc     store-furniture
;;:instance #<genKB>
;; Could add A-LINKs to CUP, PLATE, etc.
  :slot1
    :type   :aspect
    :subc   (store-furniture . :slot1)
  :slot2
    :type   :relation
    :subc   (store-furniture . :slot2)
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references  )


;;;;;;  ---- Prepare for GENKB ---- ;;;;;;
;;

(GENKB-template
  :herald  "Concepts from KB/SEMANTIC/FURNIT.LSP, ver. 3.0.0."
  :templates '(
    (armchair  (:instance (*other* 10)) )
    (bed       (:instance (*other* 10)) )
    (chair     (:instance (*other* 10)) )
    (couch     (:instance (*other* 10)) )
    (cupboard  (:instance (*other* 10)) )
    (desk      (:instance (*other* 10)) )
    (shelf     (:instance (*other* 10)) )
    (sofa      (:instance (*other* 10)) )
    (table     (:instance (*other* 10)) )
    (wardrobe  (:instance (*other* 10)) )
))

#|
;;;;;;;;;;;;   Appendix   ;;;;;;;;;;
;;
;; This file defines the following 15 agents
;; (ordered by name):

 armchair
 bed
 chair
 couch
 cupboard
 desk
 furniture
 shelf
 sit-furniture
 sleep-furniture
 sofa
 store-furniture
 table
 wardrobe
 write-furniture

|#

;;;;;;  End of file  AMBR/KB/SEMANTIC/FURNIT.LSP
