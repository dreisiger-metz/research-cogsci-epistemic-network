;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/b_WTP.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Base situation WTP -- 'Water in a Teapot on a hot Plate.'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    30-05-98 [3.0.0]  Elaboration of SIT-WTP from old LTM.LSP.
;;; UPDATED:    18-06-98  Removed IS-GOAL and RESULT propositns. Wght adjustmt.
;;;                       CAUSE consequents are propositions now, not states.
;;;                       T-OF-WTP-W1 and T-OF-WTP-W2 coalesced together.
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;         SITUATION  W T P           ;;;;;;;;
         ;;;;;;;;   Water in a Teapot on a Plate     ;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Base situation WTP   ;;;;;;;;;;;
;;;;
;;;; There is some water in a teapot on a hot-plate.
;;;; The plate is hot.  The teapot is made of metal
;;;; and its color is black.
;;;;
;;;; The goal is to heat the water.
;;;;
;;;; The result is that the teapot becomes hot because
;;;; it is on the hot plate.  In turn, this causes the
;;;; water to become hot because it is in the teapot.
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + GP  -- Glass on a hot Plate breaks.
;;;;  + IHC -- Imm.Heater in a Cup heats water.
;;;;  + ...
;;;;  * ...


;;;;;;;  Situation-agent
;;

(defagent   sit-WTP    instance-agent
  "Water in a Teapot on a hot Plate."
  :type      (:instance :situation)
  :inst-of   (situation 0.1)
  :a-link    ((hplate-WTP 0.5)
              (high-T-WTP 0.5)
              (T-of-WTP-w 1.0) )
)


;;;;;;;  Participating objects
;;
;;  water-WTP  :  (inst-of water)
;;  tpot-WTP   :  (inst-of teapot)
;;  hplate-WTP :  (inst-of hot-plate)
;;

(defagent   water-WTP    instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-WTP 0.2)
  :inst-of    water
  :c-coref    (((in-WTP . :slot1) 0.3)
               ((T-of-WTP-w  . :slot1) 1.0)
               ((goalst-WTP  . :slot3) 0.2)
               ((interst-WTP . :slot3) 0.1) )
  :a-link     (initst-WTP-1 0.1)
)

(defagent   tpot-WTP    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-WTP 0.2)
  :inst-of    teapot
  :c-coref    (((in-WTP . :slot2) 0.25)
               ((on-WTP . :slot2) 0.25)
               ((T-of-WTP-t   . :slot1) 0.25)
               ((made-of-WTP  . :slot1) 0.10)
               ((color-of-WTP . :slot1) 0.10)
               ((initst-WTP-2 . :slot3) 0.10) )
  :a-link     (initst-WTP-1 0.1)
  :slot1  
    :type      :relation
    :inst-of   (teapot . :slot2)
    :c-coref   (made-of-WTP 0.2)
    :a-link    (mmetal-WTP  0.2)
  :slot2  
    :type      :relation
    :inst-of   (teapot . :slot2)
    :c-coref   (color-of-WTP 0.1)
    :a-link    (black-WTP    0.1)
)

(defagent   hplate-WTP    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-WTP 0.3)
  :inst-of    hot-plate
  :c-coref    (((T-of-WTP-p . :slot1) 0.75)
               ((on-WTP . :slot1)     0.25) )
  :a-link     ((high-T-WTP   0.5)
               (initst-WTP-1 0.1) )
)


;;;;;;;  Initial relations
;;
;;  in-WTP       : (in water-WTP tpot-WTP)
;;  on-WTP       : (on hplate-WTP tpot-WTP)
;;  T-of-WTP-p   : (temperature-of hplate-WTP high-T-WTP)
;;  made-of-WTP  : (made-of  tpot-WTP mmetal-WTP)
;;  color-of-WTP : (color-of tpot-WTP black-WTP)
;;

(defagent   in-WTP    instance-agent
  "(in water-WTP tpot-WTP)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-WTP 0.2)
  :inst-of    in
  :c-coref    (((initst-WTP-1 . :slot4) 0.2)
               ((interst-WTP  . :slot2) 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    water-WTP
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    tpot-WTP
)

(defagent   on-WTP    instance-agent
  "(on hplate-WTP tpot-WTP)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-WTP 0.2)
  :inst-of    on
  :c-coref    (((initst-WTP-1 . :slot3) 0.2)
               ((initst-WTP-2 . :slot2) 0.1) )
  :a-link     (T-of-WTP-t 0.1)
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    hplate-WTP
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    tpot-WTP
)

(defagent   T-of-WTP-p    instance-agent
  "(temperature-of hplate-WTP high-T-WTP)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-WTP 0.2)
  :inst-of    temperature-of
  :c-coref    (((initst-WTP-1 . :slot1) 0.3)
               ((initst-WTP-2 . :slot1) 0.1) )
  :a-link     ((T-of-WTP-t  0.1)
               (T-of-WTP-w  0.1)
               (cause-WTP-i 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    hplate-WTP
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-WTP
)
(defagent  high-T-WTP   instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal :result)
  :situation  (sit-WTP 0.3)
  :inst-of    high-temp
  :c-coref    (((T-of-WTP-p   . :slot2) 0.5)
               ((T-of-WTP-t   . :slot2) 0.2)
               ((T-of-WTP-w   . :slot2) 0.5)
               ((initst-WTP-1 . :slot2) 0.1)
               ((goalst-WTP   . :slot2) 0.1) )
  :a-link     (hplate-WTP 0.3)
)

(defagent   made-of-WTP    instance-agent
  "(made-of tpot-WTP mmetal-WTP)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-WTP 0.2)
  :inst-of    made-of
  :c-coref    (tpot-WTP . :slot1)
  :a-link     (T-of-WTP-t 0.3)
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    tpot-WTP
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mmetal-WTP
)
(defagent  mmetal-WTP  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-WTP 0.2)
  :inst-of    material-metal
  :c-coref    (made-of-WTP  . :slot2)
  :a-link     (tpot-WTP 1.0)
)

(defagent   color-of-WTP    instance-agent
  "(color-of tpot-WTP black-WTP)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-WTP 0.2)
  :inst-of    color-of
  :c-coref    (tpot-WTP . :slot2)
  :slot1
    :type     :aspect
    :inst-of  (color-of . :slot1)
    :c-coref  tpot-WTP
  :slot2
    :type     :aspect
    :inst-of  (color-of . :slot2)
    :c-coref  black-WTP
)
(defagent   black-WTP  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-WTP 0.2)
  :inst-of    black
  :c-coref    (color-of-WTP . :slot2)
  :a-link     (tpot-WTP 1.0)
)


;;;;;;;  Initial states
;;
;;  initst-WTP-1  -to-reach->  goalst-WTP
;;  initst-WTP-1  -follows->   endst-WTP
;;  initst-WTP-2  --cause-->   interst-WTP
;;
;;  initst-WTP-1  :  (init-state T-of-WTP-p high-T-WTP on-WTP in-WTP)
;;  initst-WTP-2  :  (init-state T-of-WTP-p on-WTP tpot-WTP)
;;

(defagent   initst-WTP-1  instance-agent
  "initst-WTP-1  -to-reach->  goalst-WTP"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-WTP 0.2)
  :inst-of    init-state
  :c-coref    ((to-reach-WTP . :slot1)
               (follows-WTP  . :slot1) )
  :a-link     ((goalst-WTP   1.0)
               (initst-WTP-2 0.2)
               (water-WTP    0.2)
               (tpot-WTP     0.2)
               (hplate-WTP   0.2) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-WTP-p
  :slot2
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    high-T-WTP
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-WTP
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-WTP
)

(defagent   initst-WTP-2  instance-agent
  "initst-WTP-2  --cause-->  interst-WTP"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-WTP 0.2)
  :inst-of    init-state
  :c-coref    (cause-WTP-i . :slot1)
  :a-link     ((interst-WTP  1.0)
               (initst-WTP-1 0.3)
               (hplate-WTP   0.3) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-WTP-p
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-WTP
  :slot3
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    tpot-WTP
)


;;;;;;;  Goal state
;;
;;  goalst-WTP  <-to-reach-  initst-WTP-1
;;  goalst-WTP   :  (goal-state T-of-WTP-w high-T-WTP water-WTP)
;;
;;  T-of-WTP-w   :  (temperature-of water-WTP high-T-WTP)
;;  to-reach-WTP :  (to-reach initst-WTP-1 goalst-WTP)
;;

(defagent   T-of-WTP-w    instance-agent
  "(temperature-of water-WTP high-T-WTP)"
  :type       (:instance  :relation)
  :modality   (:GOAL   :intend-true
               :RESULT :true )
  :situation  (sit-WTP 0.3)
  :inst-of    temperature-of
  :c-coref    (((goalst-WTP  . :slot1) 0.2)
               ((endst-WTP   . :slot2) 0.2)
               ((cause-WTP-e . :slot2) 0.2) )
  :a-link     ((T-of-WTP-p 0.1)
               (T-of-WTP-t 0.1)
               (hplate-WTP 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    water-WTP
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-WTP
)

(defagent   goalst-WTP    instance-agent
  "goalst-WTP  <-to-reach-  initst-WTP-1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-WTP 0.2)
  :inst-of    goal-state
  :c-coref    (to-reach-WTP . :slot2)
  :a-link     ((initst-WTP-1 1.0)
               (endst-WTP    0.5) )
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-WTP-w
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    high-T-WTP
  :slot3
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    water-WTP
)

(defagent   to-reach-WTP    instance-agent
  "(to-reach initst-WTP-1 goalst-WTP)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-WTP 0.2)
  :inst-of    to-reach
  :a-link     ((follows-WTP 0.5)
               (cause-WTP-i 0.1)
               (cause-WTP-e 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-WTP-1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-WTP
)


;;;;;;;  Intermediary state
;;
;;  interst-WTP  <-cause--  initst-WTP-2
;;  interst-WTP  --cause->  endst-WTP
;;  interst-WTP   :  (inter-state T-of-WTP-t in-WTP water-WTP)
;;
;;  T-of-WTP-t    :  (temperature-of tpot-WTP high-T-WTP)
;;  cause-WTP-i   :  (cause initst-WTP-2 T-of-WTP-t)
;;

(defagent   T-of-WTP-t    instance-agent
  "(temperature-of tpot-WTP high-T-WTP)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-WTP 0.2)
  :inst-of    temperature-of
  :c-coref    (((cause-WTP-i . :slot2) 0.5)
               ((interst-WTP . :slot1) 0.2)
               ((endst-WTP   . :slot1) 0.2) )
  :a-link     ((T-of-WTP-p 0.3)
               (T-of-WTP-w 0.3)
               (hplate-WTP 0.1) )
  :slot1
    :type      :aspect
    :inst-of   (temperature-of . :slot1)
    :c-coref   tpot-WTP
  :slot2
    :type      :aspect
    :inst-of   (temperature-of . :slot2)
    :c-coref   high-T-WTP
)

(defagent   interst-WTP    instance-agent
  "interst-WTP  <-cause-  initst-WTP-2"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-WTP 0.2)
  :inst-of    inter-state
  :c-coref    (cause-WTP-e . :slot1)
  :a-link     ((endst-WTP    0.6)
               (initst-WTP-2 0.4) )
  :slot1
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    T-of-WTP-t
  :slot2
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    in-WTP
  :slot3
    :type       :aspect
    :inst-of    (inter-state . :slot1)
    :c-coref    water-WTP
)

(defagent   cause-WTP-i   instance-agent
  "(cause initst-WTP-2 T-of-WTP-t)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-WTP 0.2)
  :inst-of    cause
  :a-link     ((interst-WTP  1.0)
               (cause-WTP-e  0.5)
               (follows-WTP  0.3)
               (to-reach-WTP 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    initst-WTP-2
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    T-of-WTP-t
)


;;;;;;;  End state
;;
;;  endst-WTP  <-follows-  initst-WTP-1
;;  endst-WTP     :  (end-state T-of-WTP-t T-of-WTP-w)
;;
;;  follows-WTP   :  (follows initst-WTP-1 endst-WTP)
;;  cause-WTP-e   :  (cause interst-WTP T-of-WTP-w)
;;

(defagent   endst-WTP    instance-agent
  "endst-WTP  <-follows-  initst-WTP-1"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-WTP 0.2)
  :inst-of    end-state
  :c-coref    (follows-WTP . :slot2)
  :a-link     ((interst-WTP 0.1)
               (goalst-WTP  0.2) )
  :slot1
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    (T-of-WTP-t 0.5)
  :slot2
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    T-of-WTP-w
)

(defagent   follows-WTP  instance-agent
  "(follows initst-WTP-1 endst-WTP)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-WTP 0.2)
  :inst-of    follows
  :a-link     ((to-reach-WTP 0.5)
               (cause-WTP-i  0.2) )
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-WTP-1
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-WTP
)

(defagent   cause-WTP-e    instance-agent
  "(cause interst-WTP T-of-WTP-w)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-WTP 0.2)
  :inst-of    cause
  :a-link     ((endst-WTP    0.5)
               (follows-WTP  0.3)
               (cause-WTP-i  0.2)
               (to-reach-WTP 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    interst-WTP
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    T-of-WTP-w
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-WTP
  "Water in a Teapot on a hot Plate."
  :head      sit-WTP                      ; 23 agents
  :members   (sit-WTP
              water-WTP     tpot-WTP      hplate-WTP
              in-WTP        on-WTP
              T-of-WTP-p    high-T-WTP
              T-of-WTP-t    T-of-WTP-w
              made-of-WTP   mmetal-WTP
              color-of-WTP  black-WTP
              initst-WTP-1  initst-WTP-2  interst-WTP
              goalst-WTP    endst-WTP
              to-reach-WTP  follows-WTP
              cause-WTP-i   cause-WTP-e
             ))

(GENKB-template
  :herald  "Base sit.WTP -- Water in a Teapot on a hot Plate, ver.3.0.0."
  :templates '(
    (water          (:instance (water-WTP    2))
                    (:a-link   (T-of-WTP-w  0.1)) )
    (teapot         (:instance (tpot-WTP     3)) )
    (hot-plate      (:instance (hplate-WTP   5))
                    (:a-link   (T-of-WTP-p  0.2)) )
    (temperature-of (:instance (T-of-WTP-w   3) (T-of-WTP-p 3))
                    (:a-link   (high-T-WTP  0.1)) )
    (high-temp      (:instance (high-T-WTP   5))
                    (:a-link   (hplate-WTP  0.2)) )
    (in             (:instance (in-WTP       1)) )
    (on             (:instance (on-WTP       1)) )
    (made-of        (:instance (made-of-WTP  1)) )
    (material-metal (:instance (mmetal-WTP   1)) )
    (color-of       (:instance (color-of-WTP 1)) )
    (black          (:instance (black-WTP    1)) )
))


;;;;;;;;  Propositional representaion
;;
;;  sit-WTP       :  (inst-of sit-WTP situation)
;;
;;  black-WTP     :  (inst-of black-WTP black)
;;  cause-WTP-i   :  (cause initst-WTP-2 T-of-WTP-t)
;;  cause-WTP-e   :  (cause interst-WTP  T-of-WTP-w)
;;  color-of-WTP  :  (color-of tpot-WTP black-WTP)
;;  endst-WTP     :  (end-state T-of-WTP-t T-of-WTP-w)
;;  follows-WTP   :  (follows initst-WTP-1 endst-WTP)
;;  goalst-WTP    :  (goal-state T-of-WTP-w high-T-WTP water-WTP)
;;  high-T-WTP    :  (inst-of high-T-WTP high-temp)
;;  hplate-WTP    :  (inst-of hplate-WTP hot-plate)
;;  in-WTP        :  (in water-WTP tpot-WTP)
;;  initst-WTP-1  :  (init-state T-of-WTP-p high-T-WTP on-WTP in-WTP)
;;  initst-WTP-2  :  (init-state T-of-WTP-p on-WTP tpot-WTP)
;;  interst-WTP   :  (inter-state T-of-WTP-t in-WTP water-WTP)
;;  made-of-WTP   :  (made-of tpot-WTP mmetal-WTP)
;;  mmetal-WTP    :  (inst-of mmetal-WTP material-metal)
;;  on-WTP        :  (on tpot-WTP hplate-WTP)
;;  T-of-WTP-p    :  (temperature-of hplate-WTP high-T-WTP)
;;  T-of-WTP-t    :  (temperature-of tpot-WTP high-T-WTP)
;;  T-of-WTP-w    :  (temperature-of water-WTP high-T-WTP)
;;  to-reach-WTP  :  (to-reach initst-WTP-1 goalst-WTP)
;;  tpot-WTP      :  (inst-of tpot-WTP teapot)
;;  water-WTP     :  (inst-of water-WTP water)


;;;;;;  End of file  AMBR/KB/EPISODIC/B_WTP.LSP
