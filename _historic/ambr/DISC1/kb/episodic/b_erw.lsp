;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/b_ERW.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Base situation ERW -- 'Egg in Red Water.'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    27-06-98 [3.0.0]
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;        SITUATION  E R W         ;;;;;;;;
         ;;;;;;;;        Egg in Red Water         ;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Base situation ERW   ;;;;;;;;;;;
;;;;
;;;; There is some water in a teapot.  The color of
;;;; the water is red.  The teapot is made of metal.
;;;; There is an egg in the water.
;;;;
;;;; The goal is that the color of the egg is red.
;;;;
;;;; The result is that the color of the egg is red
;;;; because it is in the red water.
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + IHC -- Imm.Heater in a Cup heats water.
;;;;  + WTP -- Water in a Teapot on a hot Plate.
;;;;  + ...
;;;;  * ...


;;;;;;;  Situation-agent
;;

(defagent   sit-ERW    instance-agent
  "Egg in Red Water."
  :type      (:instance :situation)
  :inst-of   (situation 0.1)
  :a-link    ((egg-ERW 0.5)
              (red-ERW 0.5)
              (color-of-ERW-e 1.0) )
)


;;;;;;;  Participating objects
;;
;;  egg-ERW    :  (inst-of egg)
;;  water-ERW  :  (inst-of water)
;;  tpot-ERW   :  (inst-of teapot)
;;

(defagent   egg-ERW    instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-ERW 0.3)
  :inst-of    egg
  :c-coref    (((color-of-ERW-e . :slot1) 0.75)
               ((in-ERW-ew  . :slot1) 0.25)
               ((initst-ERW . :slot3) 0.10)
               ((goalst-ERW . :slot2) 0.10) )
)

(defagent   water-ERW    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-ERW 0.2)
  :inst-of    water
  :c-coref    (((in-ERW-ew . :slot2) 0.5)
               ((in-ERW-wt . :slot1) 0.3)
               ((color-of-ERW-w . :slot1) 1.0) )
  :a-link     (initst-ERW 0.1)
)

(defagent   tpot-ERW    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-ERW 0.2)
  :inst-of    teapot
  :c-coref    (((in-ERW-wt   . :slot2) 0.8)
               ((made-of-ERW . :slot1) 0.2) )
  :a-link     (initst-ERW 0.1)
  :slot1
    :type      :relation
    :inst-of   (teapot . :slot2)
    :c-coref   (made-of-ERW 0.2)
    :a-link    (mmetal-ERW  0.2)
)


;;;;;;;  Initial relations
;;
;;  in-ERW-ew      : (in egg-ERW water-ERW)
;;  in-ERW-wt      : (in water-ERW tpot-ERW)
;;  color-of-ERW-w : (color-of water-ERW red-ERW)
;;  made-of-ERW    : (made-of  tpot-ERW mmetal-ERW)
;;

(defagent   in-ERW-ew    instance-agent
  "(in egg-ERW water-ERW)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ERW 0.2)
  :inst-of    in
  :c-coref    ((initst-ERW . :slot4) 0.2)
  :a-link     (in-ERW-wt 0.1)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    egg-ERW
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    water-ERW
)

(defagent   in-ERW-wt    instance-agent
  "(in water-ERW tpot-ERW)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ERW 0.2)
  :inst-of    in
  :a-link     (in-ERW-ew 0.2)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    water-ERW
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    tpot-ERW
)

(defagent   color-of-ERW-w    instance-agent
  "(color-of water-ERW red-ERW)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ERW 0.2)
  :inst-of    color-of
  :c-coref    ((initst-ERW . :slot1) 0.3)
  :a-link     (color-of-ERW-e 0.2)
  :slot1
    :type       :aspect
    :inst-of    (color-of . :slot1)
    :c-coref    water-ERW
  :slot2
    :type       :aspect
    :inst-of    (color-of . :slot2)
    :c-coref    red-ERW
)
(defagent  red-ERW   instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal :result)
  :situation  (sit-ERW 0.3)
  :inst-of    red
  :c-coref    (((color-of-ERW-e . :slot2) 1.0)
               ((color-of-ERW-w . :slot2) 1.0)
               ((initst-ERW     . :slot2) 0.2) )
)

(defagent   made-of-ERW    instance-agent
  "(made-of tpot-ERW mmetal-ERW)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ERW 0.2)
  :inst-of    made-of
  :c-coref    (tpot-ERW . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    tpot-ERW
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mmetal-ERW
)
(defagent  mmetal-ERW  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-ERW 0.2)
  :inst-of    material-metal
  :c-coref    (made-of-ERW  . :slot2)
  :a-link     (tpot-ERW 1.0)
)


;;;;;;;  Initial state
;;
;;  initst-ERW  -to-reach->  goalst-ERW
;;  initst-ERW  -follows->   endst-ERW
;;
;;  initst-ERW  :  (init-state color-of-ERW-w red-ERW egg-ERW in-ERW-ew)
;;

(defagent   initst-ERW  instance-agent
  "initst-ERW  -to-reach->  goalst-ERW"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-ERW 0.2)
  :inst-of    init-state
  :c-coref    (((to-reach-ERW . :slot1) 0.5)
               ((follows-ERW  . :slot1) 0.5)
               ((cause-ERW    . :slot1) 0.3) )
  :a-link     ((goalst-ERW 0.5)
               (water-ERW  0.2)
               (tpot-ERW   0.2) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    color-of-ERW-w
  :slot2
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    red-ERW
  :slot3
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    egg-ERW
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-ERW-ew
)


;;;;;;;  Goal state
;;
;;  goalst-ERW  <-to-reach-  initst-ERW
;;  goalst-ERW     :  (goal-state color-of-ERW-w egg-ERW)
;;
;;  color-of-ERW-e :  (color-of egg-ERW red-ERW)
;;  to-reach-ERW   :  (to-reach initst-ERW goalst-ERW)
;;

(defagent   color-of-ERW-e    instance-agent
  "(color-of egg-ERW red-ERW)"
  :type       (:instance  :relation)
  :modality   (:GOAL   :intend-true
               :RESULT :true )
  :situation  (sit-ERW 0.5)
  :inst-of    color-of
  :c-coref    (((goalst-ERW . :slot1) 0.2)
               ((endst-ERW  . :slot1) 0.2)
               ((cause-ERW  . :slot2) 0.1) )
  :a-link     (color-of-ERW-w  0.1)
  :slot1
    :type       :aspect
    :inst-of    (color-of . :slot1)
    :c-coref    egg-ERW
  :slot2
    :type       :aspect
    :inst-of    (color-of . :slot2)
    :c-coref    red-ERW
)

(defagent   goalst-ERW    instance-agent
  "goalst-ERW  <-to-reach-  initst-ERW"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-ERW 0.2)
  :inst-of    goal-state
  :c-coref    (to-reach-ERW . :slot2)
  :a-link     ((initst-ERW 1.0)
               (endst-ERW  0.5) )
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    color-of-ERW-e
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    egg-ERW
)

(defagent   to-reach-ERW    instance-agent
  "(to-reach initst-ERW goalst-ERW)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-ERW 0.2)
  :inst-of    to-reach
  :a-link     ((follows-ERW 0.5)
               (cause-ERW   0.2) )
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-ERW
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-ERW
)


;;;;;;;  End state
;;
;;  endst-ERW  <-follows-  initst-ERW
;;  endst-ERW   :  (end-state color-of-ERW-e)
;;
;;  follows-ERW :  (follows initst-ERW endst-ERW)
;;  cause-ERW   :  (cause initst-ERW color-of-ERW-e)
;;

(defagent   endst-ERW    instance-agent
  "endst-ERW  <-follows-  initst-ERW"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-ERW 0.2)
  :inst-of    end-state
  :c-coref    (follows-ERW . :slot2)
  :a-link     ((initst-ERW 0.1)
               (goalst-ERW 0.2) )
  :slot1
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    (color-of-ERW-e 0.5)
)

(defagent   follows-ERW  instance-agent
  "(follows initst-ERW endst-ERW)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-ERW 0.2)
  :inst-of    follows
  :a-link     ((to-reach-ERW 0.5)
               (cause-ERW    0.1) )
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-ERW
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-ERW
)

(defagent   cause-ERW    instance-agent
  "(cause initst-ERW color-of-ERW-e)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-ERW 0.2)
  :inst-of    cause
  :a-link     ((endst-ERW    0.5)
               (follows-ERW  0.3)
               (to-reach-ERW 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    initst-ERW
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    color-of-ERW-e
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-ERW
  "Egg in Red Water."
  :head      sit-ERW                          ; 17 agents
  :members   (sit-ERW
              egg-ERW         water-ERW       tpot-ERW
              in-ERW-ew       in-ERW-wt
              color-of-ERW-w  color-of-ERW-e  red-ERW
              made-of-ERW     mmetal-ERW
              initst-ERW      goalst-ERW      endst-ERW
              to-reach-ERW    follows-ERW     cause-ERW
             ))

(GENKB-template
  :herald  "Base sit.ERW -- Egg in Red Water, ver.3.0.0."
  :templates '(
    (egg            (:instance (egg-ERW         3))
                    (:a-link   (color-of-ERW-e 0.2)) )
    (water          (:instance (water-ERW       1)) )
    (teapot         (:instance (tpot-ERW        1)) )
    (in             (:instance (in-ERW-ew       1) (in-ERW-wt 1)) )
    (color-of       (:instance (color-of-ERW-w  1) (color-of-ERW-e 2))
                    (:a-link   (red-ERW        0.1)) )
    (red            (:instance (red-ERW         1)) )
    (made-of        (:instance (made-of-ERW     1)) )
    (material-metal (:instance (mmetal-ERW      1)) )
))


;;;;;;;;  Propositional representaion
;;
;;  sit-ERW        :  (inst-of sit-ERW situation)
;;
;;  cause-ERW      :  (cause initst-ERW  color-of-ERW-e)
;;  color-of-ERW-e :  (color-of egg-ERW red-ERW)
;;  color-of-ERW-w :  (color-of water-ERW red-ERW)
;;  egg-ERW        :  (inst-of egg-ERW egg)
;;  endst-ERW      :  (end-state color-of-ERW-e)
;;  follows-ERW    :  (follows initst-ERW endst-ERW)
;;  goalst-ERW     :  (goal-state color-of-ERW-e egg-ERW)
;;  in-ERW-ew      :  (in egg-ERW water-ERW)
;;  in-ERW-wt      :  (in water-ERW tpot-ERW)
;;  initst-ERW     :  (init-state color-of-ERW-w red-ERW egg-ERW in-ERW-ew)
;;  made-of-ERW    :  (made-of tpot-ERW mmetal-ERW)
;;  mmetal-ERW     :  (inst-of mmetal-ERW material-metal)
;;  red-ERW        :  (inst-of red-ERW red)
;;  to-reach-ERW   :  (to-reach initst-ERW goalst-ERW)
;;  tpot-ERW       :  (inst-of tpot-ERW teapot)
;;  water-ERW      :  (inst-of water-ERW water)


;;;;;;  End of file  AMBR/KB/EPISODIC/B_ERW.LSP
