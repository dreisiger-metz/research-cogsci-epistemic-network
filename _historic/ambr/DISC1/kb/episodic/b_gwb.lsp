;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/b_GWB.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Base situation GWB -- 'Glass in a Wooden Box.'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    28-06-98 [3.0.0]
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;          SITUATION  G W B          ;;;;;;;;
         ;;;;;;;;       Glass in a Wooden Box        ;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Base situation GWB   ;;;;;;;;;;;
;;;;
;;;; There is a glass which is made of m.glass.
;;;; The glass is in a box.  The box is made of wood.
;;;;
;;;; The goal is that the box protects the glass.
;;;;
;;;; The result is that the box protects the glass.
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + GP  -- Glass on a hot Plate breaks down.
;;;;  + ...
;;;;  * ...


;;;;;;;  Situation-agent
;;

(defagent   sit-GWB    instance-agent
  "Glass in a Wooden Box."
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
  :a-link     ((protects-GWB 1.0)
               (glass-GWB    0.5)
               (box-GWB      0.2) )
)


;;;;;;;  Participating objects
;;
;;  glass-GWB  :  (inst-of glass)
;;  box-GWB    :  (inst-of box)
;;

(defagent   glass-GWB    instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal :result)
  :situation  (sit-GWB 0.3)
  :inst-of    glass
  :c-coref    (((in-GWB . :slot1) 0.5)
               ((made-of-GWB-g . :slot1) 0.5)
               ((protects-GWB  . :slot2) 0.8)
               ((initst-GWB    . :slot1) 0.1) )
  :slot1  
    :type      :relation
    :inst-of   (glass . :slot2)
    :c-coref   (made-of-GWB-g 0.5)
    :a-link    (mglass-GWB    0.5)
)

(defagent   box-GWB    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-GWB 0.3)
  :inst-of    box
  :c-coref    (((in-GWB . :slot2) 0.5)
               ((made-of-GWB-b . :slot1) 0.3)
               ((protects-GWB  . :slot2) 1.0)
               ((initst-GWB    . :slot2) 0.1) )
  :slot1  
    :type      :relation
    :inst-of   (box . :slot2)
    :c-coref   (made-of-GWB-b 0.3)
    :a-link    (mwood-GWB     0.3)
)


;;;;;;;  Initial relations
;;
;;  in-GWB        : (in glass-GWB box-GWB)
;;  made-of-GWB-g : (made-of glass-GWB mglass-GWB)
;;  made-of-GWB-b : (made-of box-GWB mwood-GWB)
;;

(defagent   in-GWB    instance-agent
  "(in glass-GWB box-GWB)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-GWB 0.2)
  :inst-of    in
  :c-coref    (((initst-GWB . :slot3) 0.2)
               ((cause-GWB  . :slot1) 0.3) )
  :a-link     (protects-GWB 0.5)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    glass-GWB
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    box-GWB
)

(defagent   made-of-GWB-g    instance-agent
  "(made-of glass-GWB mglass-GWB)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-GWB 0.2)
  :inst-of    made-of
  :c-coref    (glass-GWB  . :slot1)
  :a-link     ((initst-GWB    0.1)
               (made-of-GWB-b 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    glass-GWB
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mglass-GWB
)
(defagent  mglass-GWB  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-GWB 0.2)
  :inst-of    material-glass
  :c-coref    (made-of-GWB-g . :slot2)
  :a-link     ((glass-GWB 1.0)
               (mwood-GWB 0.1) )
)

(defagent   made-of-GWB-b    instance-agent
  "(made-of box-GWB mwood-GWB)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-GWB 0.2)
  :inst-of    made-of
  :c-coref    (box-GWB . :slot1)
  :a-link     ((initst-GWB    0.1)
               (made-of-GWB-g 0.5) )
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    box-GWB
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mwood-GWB
)
(defagent  mwood-GWB  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-GWB 0.2)
  :inst-of    material-wood
  :c-coref    (made-of-GWB-b . :slot2)
  :a-link     ((box-GWB 1.0)
               (mglass-GWB 0.3) )
)


;;;;;;;  Initial states
;;
;;  initst-GWB  -to-reach->  goalst-GWB
;;  initst-GWB  -follows->   endst-GWB
;;
;;  initst-GWB  :  (init-state glass-GWB box-GWB in-GWB)
;;

(defagent   initst-GWB  instance-agent
  "initst-GWB  -to-reach->  goalst-GWB"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-GWB 0.2)
  :inst-of    init-state
  :c-coref    ((to-reach-GWB . :slot1)
               (follows-GWB  . :slot1) )
  :a-link     ((goalst-GWB 0.5)
               (endst-GWB  0.5) )
  :slot1
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    glass-GWB
  :slot2
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    box-GWB
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-GWB
)


;;;;;;;  Goal state
;;
;;  goalst-GWB  <-to-reach-  initst-GWB
;;  goalst-GWB   :  (goal-state protects-GWB)
;;
;;  protects-GWB :  (protects box-GWB glass-GWB)
;;  to-reach-GWB :  (to-reach initst-GWB goalst-GWB)
;;

(defagent   protects-GWB    instance-agent
  "(protects box-GWB glass-GWB)"
  :type       (:instance  :relation)
  :modality   (:GOAL   :intend-true
               :RESULT :true )
  :situation  (sit-GWB 0.5)
  :inst-of    protects
  :c-coref    (((goalst-GWB . :slot1) 0.2)
               ((endst-GWB  . :slot1) 0.2)
               ((cause-GWB  . :slot2) 0.1) )
  :a-link     (in-GWB 0.5)
  :slot1
    :type       :aspect
    :inst-of    (protects . :slot1)
    :c-coref    box-GWB
  :slot2
    :type       :aspect
    :inst-of    (protects . :slot2)
    :c-coref    glass-GWB
)

(defagent   goalst-GWB    instance-agent
  "goalst-GWB  <-to-reach-  initst-GWB"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-GWB 0.2)
  :inst-of    goal-state
  :c-coref    (to-reach-GWB . :slot2)
  :a-link     ((initst-GWB 0.5)
               (endst-GWB  0.2) )
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    protects-GWB
)

(defagent   to-reach-GWB    instance-agent
  "(to-reach initst-GWB goalst-GWB)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-GWB 0.2)
  :inst-of    to-reach
  :a-link     ((follows-GWB 0.5)
               (cause-GWB   0.1) )
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-GWB
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-GWB
)


;;;;;;;  End state
;;
;;  endst-GWB  <-follows-  initst-GWB
;;  endst-GWB    :  (end-state protects-GWB)
;;
;;  follows-GWB  :  (follows initst-GWB endst-GWB)
;;  cause-GWB    :  (cause in-GWB protects-GWB)
;;

(defagent   endst-GWB    instance-agent
  "endst-GWB  <-follows-  initst-GWB"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-GWB 0.2)
  :inst-of    end-state
  :c-coref    (follows-GWB . :slot2)
  :a-link     ((cause-GWB  0.3)
               (initst-GWB 0.1)
               (goalst-GWB 0.2) )
  :slot1
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    protects-GWB
)

(defagent   follows-GWB   instance-agent
  "(follows initst-GWB endst-GWB)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-GWB 0.2)
  :inst-of    follows
  :a-link     ((to-reach-GWB 0.5)
               (cause-GWB    0.2) )
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-GWB
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-GWB
)

(defagent   cause-GWB    instance-agent
  "(cause in-GWB protects-GWB)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-GWB 0.2)
  :inst-of    cause
  :a-link     ((endst-GWB    0.5)
               (follows-GWB  0.3)
               (to-reach-GWB 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    in-GWB
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    protects-GWB
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-GWB
  "Glass in a Wooden Box."
  :head      sit-GWB                      ; 15 agents
  :members   (sit-GWB
              glass-GWB      box-GWB
              in-GWB         protects-GWB
              made-of-GWB-g  mglass-GWB
              made-of-GWB-b  mwood-GWB
              initst-GWB     goalst-GWB   endst-GWB
              to-reach-GWB   follows-GWB  cause-GWB
             ))

(GENKB-template
  :herald  "Base sit.GWB -- Glass in a Wooden Box, ver.3.0.0."
  :templates '(
    (glass          (:instance (glass-GWB      2))
                    (:a-link   (made-of-GWB-g 0.1)) )
    (box            (:instance (box-GWB        2))
                    (:a-link   (protects-GWB  0.2)) )
    (protects       (:instance (protects-GWB   5)) )
    (in             (:instance (in-GWB         1)) )
    (made-of        (:instance (made-of-GWB-g  2) (made-of-GWB-b 1)) )
    (material-glass (:instance (mglass-GWB     2)) )
    (material-wood  (:instance (mwood-GWB      1)) )
))


;;;;;;;;  Propositional representaion
;;
;;  sit-GWB       :  (inst-of sit-GWB situation)
;;
;;  box-GWB       :  (inst-of box-GWB box)
;;  cause-GWB     :  (cause in-GWB protects-GWB)
;;  endst-GWB     :  (end-state protects-GWB)
;;  follows-GWB   :  (follows initst-GWB endst-GWB)
;;  glass-GWB     :  (inst-of glass-GWB glass)
;;  goalst-GWB    :  (goal-state protects-GWB)
;;  in-GWB        :  (in glass-GWB box-GWB)
;;  initst-GWB    :  (init-state glass-GWB box-GWB in-GWB)
;;  made-of-GWB-b :  (made-of box-GWB mwood-GWB)
;;  made-of-GWB-g :  (made-of glass-GWB mglass-GWB)
;;  mglass-GWB    :  (inst-of mglass-GWB material-glass)
;;  mwood-GWB     :  (inst-of mwood-GWB material-wood)
;;  protects-GWB  :  (protects box-GWB glass-GWB)
;;  to-reach-GWB  :  (to-reach initst-GWB goalst-GWB)


;;;;;;  End of file  AMBR/KB/EPISODIC/B_GWB.LSP
