;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/b_BF.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Base situation BF -- 'Bowl on a Fire burns out.'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    30-05-98 [3.0.0]  Elaboration of SIT-BF from old LTM.LSP.
;;; UPDATED:    18-06-98  Removed IS-GOAL and RESULT propositns. Wght adjustmt.
;;;                       CAUSE consequents are propositions now, not states.
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;          SITUATION  B F            ;;;;;;;;
         ;;;;;;;;          Bowl on a Fire            ;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Base situation BF   ;;;;;;;;;;;
;;;;
;;;; There is some water in a bowl on a fire.
;;;; The fire is hot.  The bowl is made of wood.
;;;;
;;;; The goal is to heat the water.
;;;;
;;;; The result is that the bowl burns out because
;;;; it is on the fire.  The water dissipates
;;;; because the bowl is burnt out.
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + GP  -- Glass on a hot Plate breaks.   ; isomorphic
;;;;  + IHC -- Imm.Heater in a Cup heats water.
;;;;  + ...
;;;;  * ...


;;;;;;;  Situation-agent
;;

(defagent   sit-BF    instance-agent
  "Wooden Bowl on a Fire burns out."
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
  :a-link     ((fire-BF 1.0)
               (bowl-BF 0.5)
               (is-burnt-BF 0.5) )
)


;;;;;;;  Participating objects
;;
;;  water-BF :  (inst-of water)
;;  bowl-BF  :  (inst-of bowl)
;;  fire-BF  :  (inst-of fire)
;;

(defagent   water-BF    instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal :result)
  :situation  (sit-BF 0.2)
  :inst-of    water
  :c-coref    (((in-BF . :slot1) 0.5)
               ((T-of-BF-w . :slot1) 0.3)
               ((goalst-BF . :slot3) 0.2)
               ((is-dissip-BF . :slot1) 0.5) )
  :a-link     (initst-BF-1 0.1)
)

(defagent   bowl-BF    instance-agent
  :type       (:instance  :object)
  :modality   (:init :result)
  :situation  (sit-BF 0.5)
  :inst-of    bowl
  :c-coref    (((in-BF . :slot2) 0.25)
               ((on-BF . :slot2) 0.25)
               ((is-burnt-BF . :slot1) 0.5)
               ((made-of-BF  . :slot1) 0.5)
               ((interst-BF  . :slot3) 0.1) )
  :a-link     (initst-BF-1 0.1)
  :slot1  
    :type      :relation
    :inst-of   (bowl . :slot2)
    :c-coref   made-of-BF
    :a-link    (mwood-BF 0.5)
)

(defagent   fire-BF    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-BF 0.5)
  :inst-of    fire
  :c-coref    (((T-of-BF-f . :slot1) 0.75)
               ((on-BF . :slot1) 0.25) )
  :a-link     ((high-T-BF   0.5)
               (initst-BF-1 0.1) )
)


;;;;;;;  Initial relations
;;
;;  in-BF      : (in water-BF bowl-BF)
;;  on-BF      : (on bowl-BF fire-BF)
;;  T-of-BF-f  : (temperature-of fire-BF high-T-BF)
;;  made-of-BF : (made-of bowl-BF mwood-BF)
;;

(defagent   in-BF    instance-agent
  "(in water-BF bowl-BF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-BF 0.2)
  :inst-of    in
  :c-coref    (((initst-BF-1 . :slot4) 0.2)
               ((interst-BF  . :slot2) 0.1) )
  :a-link     (is-dissip-BF 0.5)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    water-BF
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    bowl-BF
)

(defagent   on-BF    instance-agent
  "(on fire-BF bowl-BF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-BF 0.2)
  :inst-of    on
  :c-coref    (((initst-BF-1 . :slot3) 0.2)
               ((initst-BF-2 . :slot4) 0.1) )
  :a-link     (is-burnt-BF 0.5)
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    fire-BF
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    bowl-BF
)

(defagent   T-of-BF-f    instance-agent
  "(temperature-of fire-BF high-T-BF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-BF 0.2)
  :inst-of    temperature-of
  :c-coref    (((initst-BF-1 . :slot1) 0.3)
               ((initst-BF-2 . :slot1) 0.1) )
  :a-link     ((is-burnt-BF 0.5)
               (T-of-BF-w   0.2)
               (cause-BF-b  0.1) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    fire-BF
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-BF
)
(defagent  high-T-BF   instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-BF 0.2)
  :inst-of    high-temp
  :c-coref    (((T-of-BF-f   . :slot2) 0.5)
               ((T-of-BF-w   . :slot2) 0.2)
               ((initst-BF-1 . :slot2) 0.1)
               ((goalst-BF   . :slot2) 0.1) )
  :a-link     (fire-BF 0.5)
)

(defagent   made-of-BF    instance-agent
  "(made-of bowl-BF mwood-BF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-BF 0.2)
  :inst-of    made-of
  :c-coref    (initst-BF-2 . :slot2)
  :a-link     ((is-burnt-BF 0.5)
               (cause-BF-b  0.2) )
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    bowl-BF
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mwood-BF
)
(defagent  mwood-BF  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-BF 0.2)
  :inst-of    material-wood
  :c-coref    (((made-of-BF  . :slot2) 0.8)
               ((initst-BF-2 . :slot3) 0.2) )
  :a-link     (bowl-BF 1.0)
)


;;;;;;;  Initial states
;;
;;  initst-BF-1  -to-reach->  goalst-BF
;;  initst-BF-1  -follows->   endst-BF
;;  initst-BF-2  --cause-->   is-burnt-BF
;;
;;  initst-BF-1  :  (init-state T-of-BF-f high-T-BF on-BF in-BF)
;;  initst-BF-2  :  (init-state T-of-BF-f made-of-BF mwood-BF on-BF)
;;

(defagent   initst-BF-1  instance-agent
  "initst-BF-1  -to-reach->  goalst-BF"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-BF 0.2)
  :inst-of    init-state
  :c-coref    ((to-reach-BF . :slot1)
               (follows-BF  . :slot1) )
  :a-link     ((goalst-BF   1.0)
               (initst-BF-2 0.2)
               (water-BF 0.2)
               (bowl-BF  0.2)
               (fire-BF  0.2) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-BF-f
  :slot2
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    high-T-BF
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-BF
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-BF
)

(defagent   initst-BF-2  instance-agent
  "initst-BF-2  -cause->  is-burnt-BF"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-BF 0.2)
  :inst-of    init-state
  :c-coref    (cause-BF-b . :slot1)
  :a-link     ((interst-BF  1.0)
               (initst-BF-1 0.3)
               (bowl-BF 0.3)
               (fire-BF 0.3) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-BF-f
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    made-of-BF
  :slot3
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    mwood-BF
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-BF
)


;;;;;;;  Goal state
;;
;;  goalst-BF  <-to-reach-  initst-BF-1
;;  goalst-BF   :  (goal-state T-of-BF-w high-T-BF water-BF)
;;
;;  T-of-BF-w   :  (temperature-of water-BF high-T-BF)
;;  to-reach-BF :  (to-reach initst-BF-1 goalst-BF)
;;

(defagent   T-of-BF-w    instance-agent
  "(temperature-of water-BF high-T-BF)"
  :type       (:instance  :relation)
  :modality   (:GOAL :intend-true)
  :situation  (sit-BF 0.2)
  :inst-of    temperature-of
  :c-coref    ((goalst-BF . :slot1) 0.5)
  :a-link     ((T-of-BF-f 0.1)
               (fire-BF   0.2) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    water-BF
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-BF
)

(defagent   goalst-BF    instance-agent
  "goalst-BF  <-to-reach-  initst-BF-1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-BF 0.2)
  :inst-of    goal-state
  :c-coref    (to-reach-BF . :slot2)
  :a-link     ((high-T-BF   0.5)
               (initst-BF-1 0.5)
               (endst-BF    0.2) )
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-BF-w
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    high-T-BF
  :slot3
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    water-BF
)

(defagent   to-reach-BF    instance-agent
  "(to-reach initst-BF-1 goalst-BF)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-BF 0.2)
  :inst-of    to-reach
  :a-link     ((follows-BF 0.5)
               (cause-BF-b 0.1)
               (cause-BF-d 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-BF-1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-BF
)


;;;;;;;  Intermediary state
;;
;;  interst-BF  --cause->  is-dissip-BF
;;  interst-BF   :  (inter-state is-burnt-BF in-BF bowl-BF)
;;
;;  is-burnt-BF  :  (is-burnt-out bowl-BF)
;;  cause-BF-b   :  (cause initst-BF-2 is-burnt-BF)
;;

(defagent   is-burnt-BF    instance-agent
  "(is-burnt bowl-BF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-BF 0.5)
  :inst-of    is-burnt-out
  :c-coref    (((cause-BF-b . :slot2) 0.5)
               ((interst-BF . :slot1) 0.2)
               ((endst-BF   . :slot1) 0.2) )
  :a-link     ((is-dissip-BF 0.3)
               (made-of-BF   0.3)
               (mwood-BF     0.3) )
  :slot1
    :type      :aspect
    :inst-of   (is-burnt-out . :slot1)
    :c-coref   bowl-BF
)

(defagent   interst-BF    instance-agent
  "interst-BF  -cause->  is-dissip-BF"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-BF 0.2)
  :inst-of    inter-state
  :c-coref    (cause-BF-d . :slot1)
  :a-link     ((cause-BF-b  0.7)
               (endst-BF    0.5)
               (initst-BF-2 0.3) )
  :slot1
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    is-burnt-BF
  :slot2
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    in-BF
  :slot3
    :type       :aspect
    :inst-of    (inter-state . :slot1)
    :c-coref    bowl-BF
)

(defagent   cause-BF-b   instance-agent
  "(cause initst-BF-2 is-burnt-BF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-BF 0.2)
  :inst-of    cause
  :a-link     ((interst-BF  1.0)
               (cause-BF-d  0.5)
               (follows-BF  0.3)
               (to-reach-BF 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    initst-BF-2
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    is-burnt-BF
)


;;;;;;;  End state
;;
;;  endst-BF  <-follows-  initst-BF-1
;;  endst-BF     :  (end-state is-burnt-BF is-dissip-BF)
;;
;;  is-dissip-BF :  (is-dissipated water-BF)
;;  follows-BF   :  (follows initst-BF-1 endst-BF)
;;  cause-BF-d   :  (cause interst-BF is-dissip-BF)
;;

(defagent   is-dissip-BF    instance-agent
  "(is-dissipated water-BF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-BF 0.3)
  :inst-of    is-dissipated
  :c-coref    (((cause-BF-d . :slot2) 0.5)
               ((endst-BF   . :slot2) 0.2) )
  :a-link     (is-burnt-BF 0.3)
  :slot1
    :type      :aspect
    :inst-of   (is-dissipated . :slot1)
    :c-coref   water-BF
)

(defagent   endst-BF    instance-agent
  "endst-BF  <-follows-  initst-BF-1"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-BF 0.2)
  :inst-of    end-state
  :c-coref    (follows-BF . :slot2)
  :a-link     ((cause-BF-d 0.3)
               (interst-BF 0.1)
               (goalst-BF  0.2) )
  :slot1
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    is-burnt-BF
  :slot2
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    is-dissip-BF
)

(defagent   follows-BF   instance-agent
  "(follows initst-BF-1 endst-BF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-BF 0.2)
  :inst-of    follows
  :a-link     ((to-reach-BF 0.5)
               (cause-BF-b  0.2) )
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-BF-1
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-BF
)

(defagent   cause-BF-d    instance-agent
  "(cause interst-BF is-dissip-BF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-BF 0.2)
  :inst-of    cause
  :a-link     ((endst-BF    0.5)
               (follows-BF  0.3)
               (cause-BF-b  0.2)
               (to-reach-BF 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    interst-BF
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    is-dissip-BF
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-BF
  "Wooden Bowl on a Fire burns out."
  :head      sit-BF                     ; 22 agents
  :members   (sit-BF
              water-BF     bowl-BF      fire-BF
              in-BF        on-BF
              T-of-BF-f    T-of-BF-w    high-T-BF
              made-of-BF   mwood-BF
              is-burnt-BF  is-dissip-BF
              initst-BF-1  initst-BF-2  interst-BF
              goalst-BF    endst-BF
              to-reach-BF  follows-BF
              cause-BF-b   cause-BF-d
             ))

(GENKB-template
  :herald  "Base sit.BF  -- Wooden Bowl on a Fire burns out, ver.3.0.0."
  :templates '(
    (water          (:instance (water-BF     1)) )
    (bowl           (:instance (bowl-BF      3))
                    (:a-link   (is-burnt-BF 0.1)) )
    (fire           (:instance (fire-BF      5))
                    (:a-link   (T-of-BF-f   0.1)
                               (is-burnt-BF 0.1)) )
    (is-burnt-out   (:instance (is-burnt-BF  5)) )
    (is-dissipated  (:instance (is-dissip-BF 2)) )
    (temperature-of (:instance (T-of-BF-w    1) (T-of-BF-f 2)) )
    (high-temp      (:instance (high-T-BF    1))
                    (:a-link   (fire-BF     0.1)) )
    (in             (:instance (in-BF        1)) )
    (on             (:instance (on-BF        1)) )
    (made-of        (:instance (made-of-BF   1)) )
    (material-wood  (:instance (mwood-BF     2)) )
))


;;;;;;;;  Propositional representaion
;;
;;  sit-BF       :  (inst-of sit-BF situation)
;;
;;  bowl-BF      :  (inst-of bowl-BF bowl)
;;  cause-BF-b   :  (cause initst-BF-2 is-burnt-BF)
;;  cause-BF-d   :  (cause interst-BF  is-dissip-BF)
;;  endst-BF     :  (end-state is-burnt-BF is-dissip-BF)
;;  fire-BF      :  (inst-of fire-BF fire)
;;  follows-BF   :  (follows initst-BF-1 endst-BF)
;;  goalst-BF    :  (goal-state T-of-BF-w high-T-BF water-BF)
;;  high-T-BF    :  (inst-of high-T-BF high-temp)
;;  in-BF        :  (in water-BF bowl-BF)
;;  initst-BF-1  :  (init-state T-of-BF-f high-T-BF on-BF in-BF)
;;  initst-BF-2  :  (init-state T-of-BF-f made-of-BF mwood-BF on-BF)
;;  interst-BF   :  (inter-state is-burnt-BF in-BF bowl-BF)
;;  is-burnt-BF  :  (is-burnt-out bowl-BF)
;;  is-dissip-BF :  (is-dissipated water-BF)
;;  made-of-BF   :  (made-of bowl-BF mwood-BF)
;;  mwood-BF     :  (inst-of mwood-BF material-wood)
;;  on-BF        :  (on fire-BF bowl-BF)
;;  T-of-BF-f    :  (temperature-of fire-BF high-T-BF)
;;  T-of-BF-w    :  (temperature-of water-BF high-T-BF)
;;  to-reach-BF  :  (to-reach initst-BF-1 goalst-BF)
;;  water-BF     :  (inst-of water-BF water)


;;;;;;  End of file  AMBR/KB/EPISODIC/B_BF.LSP
