;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/b_FDO.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Base situation FDO -- 'Food on a Dish in an Oven'.
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    31-05-98 [3.0.0]
;;; UPDATED:    18-06-98  Removed IS-GOAL and RESULT propositns. Wght adjustmt.
;;;                       CAUSE consequents are propositions now, not states.
;;;                       T-OF-FDO-F1 and T-OF-FDO-F2 coalesced together.
;;; UPDATED:    ...

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;;;;;;         SITUATION  F D O           ;;;;;;;;
          ;;;;;;;;     Food on a Dish in an Oven      ;;;;;;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Base situation FDO   ;;;;;;;;;;;
;;;;
;;;; There is some food on a dish in an oven.
;;;; The oven is hot.  The dish is made of
;;;; metal and its shape is rectangular.
;;;;
;;;; The goal is to heat the food.
;;;;
;;;; Since the food is on the dish which, in turn,
;;;; is in the oven, the food is in the oven too.
;;;; This causes the food to become hot.
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + BPF -- Butter on a Plate in a Fridge.    ; isomorphic
;;;;  + MTF -- Milk in a Teapot in a Fridge.
;;;;  + ICF -- Ice Cube in a glass in a Fridge.
;;;;  + WTP -- Water in a Teapot on a hot Plate.
;;;;  * ...


;;;;;;;  Situation-agent
;;

(defagent   sit-FDO    instance-agent
  "Food on a Dish in an Oven."
  :type      (:instance :situation)
  :inst-of   (situation 0.1)
  :a-link    ((oven-FDO 1.0)
              (dish-FDO 0.5) )
)


;;;;;;;  Participating objects
;;
;;  food-FDO  :  (inst-of food)
;;  dish-FDO  :  (inst-of baking-dish)
;;  oven-FDO  :  (inst-of oven)
;;

(defagent   food-FDO    instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-FDO 0.2)
  :inst-of    food
  :c-coref    (((T-of-FDO-f . :slot1) 0.50)
               ((on-FDO     . :slot2) 0.25)
               ((in-FDO-fo  . :slot1) 0.1)
               ((goalst-FDO . :slot3) 0.1) )
  :a-link     (initst-FDO-1 0.1)
)

(defagent   dish-FDO    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-FDO 0.5)
  :inst-of    baking-dish
  :c-coref    (((on-FDO       . :slot1) 0.5)
               ((in-FDO-do    . :slot1) 0.5)
               ((shape-of-FDO . :slot1) 0.1) )
  :a-link     (initst-FDO-1 0.1)
  :slot1
    :type       :aspect
    :inst-of    (baking-dish . :slot1)
    :c-coref    (shape-of-FDO 0.1)
    :a-link     (rectang-FDO  0.1)
)

(defagent   oven-FDO    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-FDO 0.5)
  :inst-of    oven
  :c-coref    (((T-of-FDO-o  . :slot1) 0.75)
               ((in-FDO-do   . :slot2) 0.25)
               ((in-FDO-fo   . :slot2) 0.10)
               ((interst-FDO . :slot3) 0.10) )
  :a-link     ((high-T-FDO   0.5)
               (initst-FDO-1 0.1) )
)


;;;;;;;  Initial relations
;;
;;  on-FDO       : (on dish-FDO food-FDO)
;;  in-FDO-do    : (in dish-FDO oven-FDO)
;;  T-of-FDO-o   : (temperature-of oven-FDO high-T-FDO)
;;  shape-of-FDO : (shape-of dish-FDO rectang-FDO)
;;

(defagent   on-FDO    instance-agent
  "(on dish-FDO food-FDO)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-FDO 0.2)
  :inst-of    on
  :c-coref    (((initst-FDO-1 . :slot3) 0.2)
               ((initst-FDO-2 . :slot1) 0.1) )
  :a-link     ((in-FDO-do   0.1)
               (in-FDO-fo   0.1)
               (cause-FDO-i 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    dish-FDO
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    food-FDO
)

(defagent   in-FDO-do    instance-agent
  "(in dish-FDO oven-FDO)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-FDO 0.2)
  :inst-of    in
  :c-coref    (((initst-FDO-1 . :slot4) 0.2)
               ((initst-FDO-2 . :slot2) 0.1) )
  :a-link     ((in-FDO-fo 0.1)
               (on-FDO    0.2) )
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    dish-FDO
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    oven-FDO
)

(defagent   T-of-FDO-o    instance-agent
  "(temperature-of oven-FDO high-T-FDO)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-FDO 0.2)
  :inst-of    temperature-of
  :c-coref    (((initst-FDO-1 . :slot1) 0.3)
               ((interst-FDO  . :slot2) 0.1) )
  :a-link     ((T-of-FDO-f  0.1)
               (cause-FDO-t 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    oven-FDO
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-FDO
)
(defagent  high-T-FDO   instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal :result)
  :situation  (sit-FDO 0.2)
  :inst-of    high-temp
  :c-coref    (((T-of-FDO-o   . :slot2) 0.5)
               ((T-of-FDO-f   . :slot2) 0.5)
               ((initst-FDO-1 . :slot2) 0.1)
               ((goalst-FDO   . :slot2) 0.1) )
  :a-link     (oven-FDO 0.3)
)

(defagent   shape-of-FDO    instance-agent
  "(shape-of dish-FDO rectang-FDO)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-FDO 0.1)
  :c-coref    (dish-FDO . :slot1)
  :inst-of    shape-of
  :slot1
    :type     :aspect
    :inst-of  (shape-of . :slot1)
    :c-coref  dish-FDO
  :slot2
    :type     :aspect
    :inst-of  (shape-of . :slot2)
    :c-coref  rectang-FDO
)
(defagent   rectang-FDO  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-FDO 0.1)
  :inst-of    rectang-shape
  :c-coref    (shape-of-FDO . :slot2)
  :a-link     (dish-FDO 1.0)
)


;;;;;;;  Initial states
;;
;;  initst-FDO-1  -to-reach->  goalst-FDO
;;  initst-FDO-1  -follows->   endst-FDO
;;  initst-FDO-2  --cause-->   in-FDO-fo
;;
;;  initst-FDO-1  :  (init-state T-of-FDO-o high-T-FDO on-FDO in-FDO-do)
;;  initst-FDO-2  :  (init-state on-FDO in-FDO-do)
;;

(defagent   initst-FDO-1  instance-agent
  "initst-FDO-1  -to-reach->  goalst-FDO"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-FDO 0.2)
  :inst-of    init-state
  :c-coref    ((to-reach-FDO . :slot1)
               (follows-FDO  . :slot1) )
  :a-link     ((goalst-FDO   1.0)
               (initst-FDO-2 0.2)
               (food-FDO 0.2)
               (dish-FDO 0.2)
               (oven-FDO 0.5) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-FDO-o
  :slot2
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    high-T-FDO
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-FDO
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-FDO-do
)

(defagent   initst-FDO-2  instance-agent
  "initst-FDO-2  -cause->  in-FDO-fo"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-FDO 0.2)
  :inst-of    init-state
  :c-coref    (cause-FDO-i . :slot1)
  :a-link     ((interst-FDO  1.0)
               (initst-FDO-1 0.3)
               (food-FDO     0.3) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-FDO
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-FDO-do
)


;;;;;;;  Goal state
;;
;;  goalst-FDO  <-to-reach-  initst-FDO-1
;;  goalst-FDO   :  (goal-state T-of-FDO-f high-T-FDO food-FDO)
;;
;;  T-of-FDO-f   :  (temperature-of food-FDO high-T-FDO)
;;  to-reach-FDO :  (to-reach initst-FDO-1 goalst-FDO)
;;

(defagent   T-of-FDO-f    instance-agent
  "(temperature-of food-FDO high-T-FDO)"
  :type       (:instance  :relation)
  :modality   (:GOAL   :intend-true
               :RESULT :true )
  :situation  (sit-FDO 0.2)
  :inst-of    temperature-of
  :c-coref    (((cause-FDO-t . :slot2) 0.2)
               ((goalst-FDO  . :slot1) 0.2)
               ((endst-FDO   . :slot1) 0.2) )
  :a-link     ((T-of-FDO-o 0.1)
               (in-FDO-fo  0.1)
               (oven-FDO   0.1) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    food-FDO
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-FDO
)

(defagent   goalst-FDO    instance-agent
  "goalst-FDO  <-to-reach-  initst-FDO-1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-FDO 0.2)
  :inst-of    goal-state
  :c-coref    (to-reach-FDO . :slot2)
  :a-link     ((initst-FDO-1 0.5)
               (endst-FDO    0.2) )
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-FDO-f
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    high-T-FDO
  :slot3
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    food-FDO
)

(defagent   to-reach-FDO    instance-agent
  "(to-reach initst-FDO-1 goalst-FDO)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-FDO 0.2)
  :inst-of    to-reach
  :a-link     ((follows-FDO 0.5)
               (cause-FDO-i 0.1)
               (cause-FDO-t 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-FDO-1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-FDO
)


;;;;;;;  Intermediary state
;;
;;  interst-FDO  --cause->  T-of-FDO-f
;;  interst-FDO   :  (inter-state in-FDO-fo T-of-FDO-o oven-FDO)
;;
;;  in-FDO-fo     :  (in food-FDO oven-FDO)
;;  cause-FDO-i   :  (cause initst-FDO-2 interst-FDO)
;;

(defagent   in-FDO-fo    instance-agent
  "(in food-FDO oven-FDO)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-FDO 0.2)
  :inst-of    in
  :c-coref    (((cause-FDO-i . :slot2) 0.5)
               ((interst-FDO . :slot1) 0.3) )
  :a-link     ((on-FDO    0.3)
               (in-FDO-do 0.2) )
  :slot1
    :type      :aspect
    :inst-of   (in . :slot1)
    :c-coref   food-FDO
  :slot2
    :type      :aspect
    :inst-of   (in . :slot2)
    :c-coref   oven-FDO
)

(defagent   interst-FDO   instance-agent
  "interst-FDO  -cause->  T-of-FDO-f"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-FDO 0.2)
  :inst-of    inter-state
  :c-coref    (cause-FDO-t . :slot1)
  :a-link     ((endst-FDO    0.6)
               (initst-FDO-2 0.4)
               (cause-FDO-i  0.2) )
  :slot1
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    in-FDO-fo
  :slot2
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    T-of-FDO-o
  :slot3
    :type       :aspect
    :inst-of    (inter-state . :slot1)
    :c-coref    oven-FDO
)

(defagent   cause-FDO-i   instance-agent
  "(cause initst-FDO-2 in-FDO-fo)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-FDO 0.2)
  :inst-of    cause
  :a-link     ((interst-FDO  1.0)
               (cause-FDO-t  0.5)
               (follows-FDO  0.3)
               (to-reach-FDO 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    initst-FDO-2
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    in-FDO-fo
)


;;;;;;;  End state
;;
;;  endst-FDO  <-follows-  initst-FDO-1
;;  endst-FDO     :  (end-state T-of-FDO-f)
;;
;;  follows-FDO   :  (follows initst-FDO-1 endst-FDO)
;;  cause-FDO-t   :  (cause interst-FDO T-of-FDO-f)
;;

(defagent   endst-FDO    instance-agent
  "endst-FDO  <-follows-  initst-FDO-1"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-FDO 0.2)
  :inst-of    end-state
  :c-coref    (follows-FDO . :slot2)
  :a-link     ((cause-FDO-t  0.1)
               (interst-FDO  0.1)
               (goalst-FDO   0.2) )
  :slot1
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    T-of-FDO-f
)

(defagent   follows-FDO   instance-agent
  "(follows initst-FDO-1 endst-FDO)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-FDO 0.2)
  :inst-of    follows
  :a-link     ((to-reach-FDO 0.5)
               (cause-FDO-t  0.2) )
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-FDO-1
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-FDO
)

(defagent   cause-FDO-t    instance-agent
  "(cause interst-FDO T-of-FDO-f)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-FDO 0.2)
  :inst-of    cause
  :a-link     ((endst-FDO    0.5)
               (follows-FDO  0.3)
               (cause-FDO-i  0.2)
               (to-reach-FDO 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    interst-FDO
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    T-of-FDO-f
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-FDO
  "Food on a Dish in an Oven."
  :head      sit-FDO                      ; 21 agents
  :members   (sit-FDO
              food-FDO      dish-FDO      oven-FDO
              on-FDO        in-FDO-do     in-FDO-fo
              T-of-FDO-o    T-of-FDO-f    high-T-FDO
              shape-of-FDO  rectang-FDO
              initst-FDO-1  initst-FDO-2  interst-FDO
              goalst-FDO    endst-FDO
              to-reach-FDO  follows-FDO
              cause-FDO-i   cause-FDO-t
             ))

(GENKB-template
  :herald  "Base sit.FDO -- Food on a Dish in an Oven, ver.3.0.0."
  :templates '(
    (food           (:instance (food-FDO     1))
                    (:a-link   (T-of-FDO-f  0.1)) )
    (baking-dish    (:instance (dish-FDO     3))
                    (:a-link   (rectang-FDO 0.1)) )
    (oven           (:instance (oven-FDO     5))
                    (:a-link   (T-of-FDO-o  0.2)) )
    (temperature-of (:instance (T-of-FDO-f   1) (T-of-FDO-o 2))
                    (:a-link   (high-T-FDO  0.1)) )
    (high-temp      (:instance (high-T-FDO   2))
                    (:a-link   (oven-FDO    0.2)) )
    (on             (:instance (on-FDO       1)) )
    (in             (:instance (in-FDO-do    1)) )
    (shape-of       (:instance (shape-of-FDO 1)) )
    (rectang-shape  (:instance (rectang-FDO  1)) )
))


;;;;;;;;  Propositional representaion
;;
;;  sit-FDO       :  (inst-of sit-FDO situation)
;;
;;  cause-FDO-i   :  (cause initst-FDO-2 in-FDO-fo)
;;  cause-FDO-t   :  (cause interst-FDO  T-of-FDO-f)
;;  dish-FDO      :  (inst-of dish-FDO baking-dish)
;;  endst-FDO     :  (end-state T-of-FDO-f)
;;  follows-FDO   :  (follows initst-FDO-1 endst-FDO)
;;  food-FDO      :  (inst-of food-FDO food)
;;  goalst-FDO    :  (goal-state T-of-FDO-f high-T-FDO food-FDO)
;;  high-T-FDO    :  (inst-of high-T-FDO high-temp)
;;  initst-FDO-1  :  (init-state T-of-FDO-o high-T-FDO on-FDO in-FDO-do)
;;  initst-FDO-2  :  (init-state on-FDO in-FDO-do)
;;  interst-FDO   :  (inter-state in-FDO-fo T-of-FDO-o oven-FDO)
;;  in-FDO-fo     :  (in food-FDO oven-FDO)
;;  in-FDO-do     :  (in dish-FDO oven-FDO)
;;  on-FDO        :  (on dish-FDO food-FDO)
;;  oven-FDO      :  (inst-of oven-FDO oven)
;;  rectang-FDO   :  (inst-of rectang-FDO rectang-shape)
;;  shape-of-FDO  :  (shape-of dish-FDO rectang-FDO)
;;  T-of-FDO-o    :  (temperature-of oven-FDO high-T-FDO)
;;  T-of-FDO-f    :  (temperature-of food-FDO high-T-FDO)
;;  to-reach-FDO  :  (to-reach initst-FDO-1 goalst-FDO)


;;;;;;  End of file  AMBR/KB/EPISODIC/B_FDO.LSP
