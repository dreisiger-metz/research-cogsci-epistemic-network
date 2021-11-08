;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/b_GP.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Base situation GP -- 'Glass on a hot Plate breaks'.
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    27-05-98 [3.0.0]  Elaboration of SIT-GP from old LTM.LSP.
;;; UPDATED:    18-06-98  Removed IS-GOAL and RESULT propositns. Wght adjustmt.
;;;                       CAUSE consequents are propositions now, not states.
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;          SITUATION  G P            ;;;;;;;;
         ;;;;;;;;         Glass on a Plate           ;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Base situation GP   ;;;;;;;;;;;
;;;;
;;;; There is some water in a glass on a hot-plate.
;;;; The plate is hot.  The glass is made of m.glass.
;;;;
;;;; The goal is to heat the water.
;;;;
;;;; The result is that the glass breaks because it
;;;; is on the hot plate.  The water dissipates
;;;; because the glass is broken.
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + BF  -- Bowl on a Fire burns out.      ; isomorphic
;;;;  + IHC -- Imm.Heater in a Cup with water.
;;;;  + ...
;;;;  * ...


;;;;;;;  Situation-agent
;;

(defagent   sit-GP    instance-agent
  "Glass on a hot Plate breaks."
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
  :a-link     ((is-broken-GP 1.0)
               (is-dissip-GP 0.2)
               (glass-GP  0.5)
               (mglass-GP 0.2) )
)


;;;;;;;  Participating objects
;;
;;  water-GP  :  (inst-of water)
;;  glass-GP  :  (inst-of glass)
;;  hplate-GP :  (inst-of hot-plate)
;;

(defagent   water-GP    instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal :result)
  :situation  (sit-GP 0.2)
  :inst-of    water
  :c-coref    (((in-GP . :slot1) 0.5)
               ((T-of-GP-w . :slot1) 0.3)
               ((goalst-GP . :slot3) 0.2)
               ((is-dissip-GP . :slot1) 0.5) )
  :a-link     (initst-GP-1 0.1)
)

(defagent   glass-GP    instance-agent
  :type       (:instance  :object)
  :modality   (:init :result)
  :situation  (sit-GP 0.3)
  :inst-of    glass
  :c-coref    (((in-GP . :slot2) 0.25)
               ((on-GP . :slot2) 0.25)
               ((is-broken-GP . :slot1) 0.5)
               ((made-of-GP   . :slot1) 0.5)
               ((interst-GP   . :slot3) 0.1) )
  :a-link     (initst-GP-1 0.1)
  :slot1  
    :type      :relation
    :inst-of   (glass . :slot2)
    :c-coref   made-of-GP
    :a-link    (mglass-GP 0.5)
)

(defagent   hplate-GP    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-GP 0.2)
  :inst-of    hot-plate
  :c-coref    (((T-of-GP-p . :slot1) 0.75)
               ((on-GP . :slot1) 0.25) )
  :a-link     ((high-T-GP   0.5)
               (initst-GP-1 0.1) )
)


;;;;;;;  Initial relations
;;
;;  in-GP      : (in water-GP glass-GP)
;;  on-GP      : (on glass-GP hplate-GP)
;;  T-of-GP-p  : (temperature-of hplate-GP high-T-GP)
;;  made-of-GP : (made-of glass-GP mglass-GP)
;;

(defagent   in-GP    instance-agent
  "(in water-GP glass-GP)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-GP 0.2)
  :inst-of    in
  :c-coref    (((initst-GP-1 . :slot4) 0.2)
               ((interst-GP  . :slot2) 0.1) )
  :a-link     (is-dissip-GP 0.5)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    water-GP
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    glass-GP
)

(defagent   on-GP    instance-agent
  "(on hplate-GP glass-GP)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-GP 0.2)
  :inst-of    on
  :c-coref    (((initst-GP-1 . :slot3) 0.2)
               ((initst-GP-2 . :slot4) 0.1) )
  :a-link     (is-broken-GP 0.5)
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    hplate-GP
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    glass-GP
)

(defagent   T-of-GP-p    instance-agent
  "(temperature-of hplate-GP high-T-GP)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-GP 0.2)
  :inst-of    temperature-of
  :c-coref    (((initst-GP-1 . :slot1) 0.3)
               ((initst-GP-2 . :slot1) 0.1) )
  :a-link     ((T-of-GP-w  0.2)
               (cause-GP-b 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    hplate-GP
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-GP
)
(defagent  high-T-GP   instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-GP 0.2)
  :inst-of    high-temp
  :c-coref    (((T-of-GP-p . :slot2) 0.5)
               ((T-of-GP-w . :slot2) 0.5)
               ((initst-GP-1 . :slot2) 0.1)
               ((goalst-GP   . :slot2) 0.1) )
  :a-link     ((hplate-GP 0.5)
               (is-broken-GP 0.2) )
)

(defagent   made-of-GP    instance-agent
  "(made-of glass-GP mglass-GP)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-GP 0.2)
  :inst-of    made-of
  :c-coref    ((glass-GP . :slot1)
               ((initst-GP-2 . :slot2) 0.2) )
  :a-link     ((is-broken-GP 0.5)
               (cause-GP-b   0.2) )
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    glass-GP
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mglass-GP
)
(defagent  mglass-GP  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-GP 0.2)
  :inst-of    material-glass
  :c-coref    (((made-of-GP  . :slot2) 0.8)
               ((initst-GP-2 . :slot3) 0.2) )
  :a-link     (glass-GP 1.0)
)


;;;;;;;  Initial states
;;
;;  initst-GP-1  -to-reach->  goalst-GP
;;  initst-GP-1  -follows->   endst-GP
;;  initst-GP-2  --cause-->   is-broken-GP
;;
;;  initst-GP-1  :  (init-state T-of-GP-p high-T-GP on-GP in-GP)
;;  initst-GP-2  :  (init-state T-of-GP-p made-of-GP mglass-GP on-GP)
;;

(defagent   initst-GP-1  instance-agent
  "initst-GP-1  -to-reach->  goalst-GP"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-GP 0.2)
  :inst-of    init-state
  :c-coref    ((to-reach-GP . :slot1)
               (follows-GP  . :slot1) )
  :a-link     ((goalst-GP   1.0)
               (initst-GP-2 0.2)
               (water-GP  0.2)
               (glass-GP  0.2)
               (hplate-GP 0.2) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-GP-p
  :slot2
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    high-T-GP
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-GP
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-GP
)

(defagent   initst-GP-2  instance-agent
  "initst-GP-2  -cause->  is-broken-GP"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-GP 0.2)
  :inst-of    init-state
  :c-coref    (cause-GP-b . :slot1)
  :a-link     ((interst-GP  1.0)
               (initst-GP-1 0.3)
               (glass-GP  0.3)
               (hplate-GP 0.3) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-GP-p
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    made-of-GP
  :slot3
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    mglass-GP
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-GP
)


;;;;;;;  Goal state
;;
;;  goalst-GP  <-to-reach-  initst-GP-1
;;  goalst-GP   :  (goal-state T-of-GP-w high-T-GP water-GP)
;;
;;  T-of-GP-w   :  (temperature-of water-GP high-T-GP)
;;  to-reach-GP :  (to-reach initst-GP-1 goalst-GP)
;;

(defagent   T-of-GP-w    instance-agent
  "(temperature-of water-GP high-T-GP)"
  :type       (:instance  :relation)
  :modality   (:GOAL :intend-true)
  :situation  (sit-GP 0.2)
  :inst-of    temperature-of
  :c-coref    ((goalst-GP  . :slot1) 0.5)
  :a-link     ((T-of-GP-p 0.1)
               (hplate-GP 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    water-GP
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-GP
)

(defagent   goalst-GP    instance-agent
  "goalst-GP  <-to-reach-  initst-GP-1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-GP 0.2)
  :inst-of    goal-state
  :c-coref    (to-reach-GP . :slot2)
  :a-link     ((high-T-GP   0.5)
               (initst-GP-1 0.5)
               (endst-GP    0.2) )
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-GP-w
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    high-T-GP
  :slot3
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    water-GP
)

(defagent   to-reach-GP    instance-agent
  "(to-reach initst-GP-1 goalst-GP)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-GP 0.2)
  :inst-of    to-reach
  :a-link     ((follows-GP 0.5)
               (cause-GP-b 0.1)
               (cause-GP-d 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-GP-1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-GP
)


;;;;;;;  Intermediary state
;;
;;  interst-GP  --cause->  is-dissip-GP
;;  interst-GP   :  (inter-state is-broken-GP in-GP glass-GP)
;;
;;  is-broken-GP :  (is-broken glass-GP)
;;  cause-GP-b   :  (cause initst-GP-2 is-broken-GP)
;;

(defagent   is-broken-GP    instance-agent
  "(is-broken glass-GP)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-GP 0.5)
  :inst-of    is-broken
  :c-coref    (((cause-GP-b . :slot2) 0.5)
               ((interst-GP . :slot1) 0.2)
               ((endst-GP   . :slot1) 0.2) )
  :a-link     ((is-dissip-GP 0.3)
               (made-of-GP   0.3)
               (mglass-GP    0.3) )
  :slot1
    :type      :aspect
    :inst-of   (is-broken . :slot1)
    :c-coref   glass-GP
)

(defagent   interst-GP    instance-agent
  "interst-GP  -cause->  is-dissip-GP"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-GP 0.2)
  :inst-of    inter-state
  :c-coref    (cause-GP-d . :slot1)
  :a-link     ((cause-GP-b  0.7)
               (endst-GP    0.5)
               (initst-GP-2 0.3) )
  :slot1
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    is-broken-GP
  :slot2
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    in-GP
  :slot3
    :type       :aspect
    :inst-of    (inter-state . :slot1)
    :c-coref    glass-GP
)

(defagent   cause-GP-b   instance-agent
  "(cause initst-GP-2 is-broken-GP)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-GP 0.2)
  :inst-of    cause
  :a-link     ((interst-GP  1.0)
               (cause-GP-d  0.5)
               (follows-GP  0.3)
               (to-reach-GP 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    initst-GP-2
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    is-broken-GP
)


;;;;;;;  End state
;;
;;  endst-GP  <-follows-  initst-GP-1
;;  endst-GP     :  (end-state is-broken-GP is-dissip-GP)
;;
;;  is-dissip-GP :  (is-dissipated water-GP)
;;  follows-GP   :  (follows initst-GP-1 endst-GP)
;;  cause-GP-d   :  (cause interst-GP is-dissip-GP)
;;

(defagent   is-dissip-GP    instance-agent
  "(is-dissipated water-GP)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-GP 0.3)
  :inst-of    is-dissipated
  :c-coref    (((cause-GP-d . :slot2) 0.5)
               ((endst-GP   . :slot2) 0.2) )
  :a-link     (is-broken-GP 0.3)
  :slot1
    :type      :aspect
    :inst-of   (is-dissipated . :slot1)
    :c-coref   water-GP
)

(defagent   endst-GP    instance-agent
  "endst-GP  <-follows-  initst-GP-1"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-GP 0.2)
  :inst-of    end-state
  :c-coref    (follows-GP . :slot2)
  :a-link     ((cause-GP-d 0.3)
               (interst-GP 0.1)
               (goalst-GP  0.2) )
  :slot1
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    is-broken-GP
  :slot2
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    is-dissip-GP
)

(defagent   follows-GP   instance-agent
  "(follows initst-GP-1 endst-GP)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-GP 0.2)
  :inst-of    follows
  :a-link     ((to-reach-GP 0.5)
               (cause-GP-b  0.2) )
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-GP-1
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-GP
)

(defagent   cause-GP-d    instance-agent
  "(cause interst-GP is-dissip-GP)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-GP 0.2)
  :inst-of    cause
  :a-link     ((endst-GP    0.5)
               (follows-GP  0.3)
               (cause-GP-b  0.2)
               (to-reach-GP 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    interst-GP
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    is-dissip-GP
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-GP
  "Glass on a hot Plate breaks."
  :head      sit-GP                     ; 22 agents
  :members   (sit-GP
              water-GP     glass-GP     hplate-GP
              in-GP        on-GP
              T-of-GP-p    T-of-GP-w    high-T-GP
              made-of-GP   mglass-GP
              is-broken-GP is-dissip-GP
              initst-GP-1  initst-GP-2  interst-GP
              goalst-GP    endst-GP
              to-reach-GP  follows-GP
              cause-GP-b   cause-GP-d
             ))

(GENKB-template
  :herald  "Base sit.GP  -- Glass on a hot Plate breaks, ver.3.0.0."
  :templates '(
    (water          (:instance (water-GP     1)) )
    (glass          (:instance (glass-GP     2))
                    (:a-link   (is-broken-GP 0.3)) )
    (hot-plate      (:instance (hplate-GP    1)) )
    (is-broken      (:instance (is-broken-GP 5)) )
    (is-dissipated  (:instance (is-dissip-GP 2)) )
    (temperature-of (:instance (T-of-GP-w    1) (T-of-GP-p 1)) )
    (high-temp      (:instance (high-T-GP    1)) )
    (in             (:instance (in-GP        1)) )
    (on             (:instance (on-GP        1)) )
    (made-of        (:instance (made-of-GP   2)) )
    (material-glass (:instance (mglass-GP    3)) )
))


;;;;;;;;  Propositional representaion
;;
;;  sit-GP       :  (inst-of sit-GP situation)
;;
;;  cause-GP-b   :  (cause initst-GP-2 is-broken-GP)
;;  cause-GP-d   :  (cause interst-GP  is-dissip-GP)
;;  endst-GP     :  (end-state is-broken-GP is-dissip-GP)
;;  follows-GP   :  (follows initst-GP-1 endst-GP)
;;  glass-GP     :  (inst-of glass-GP glass)
;;  goalst-GP    :  (goal-state T-of-GP-w high-T-GP water-GP)
;;  high-T-GP    :  (inst-of high-T-GP high-temp)
;;  hplate-GP    :  (inst-of hplate-GP hot-plate)
;;  in-GP        :  (in water-GP glass-GP)
;;  initst-GP-1  :  (init-state T-of-GP-p high-T-GP on-GP in-GP)
;;  initst-GP-2  :  (init-state T-of-GP-p made-of-GP mglass-GP on-GP)
;;  interst-GP   :  (inter-state is-broken-GP in-GP glass-GP)
;;  is-broken-GP :  (is-broken glass-GP)
;;  is-dissip-GP :  (is-dissipated water-GP)
;;  made-of-GP   :  (made-of glass-GP mglass-GP)
;;  mglass-GP    :  (inst-of mglass-GP material-glass)
;;  on-GP        :  (on hplate-GP glass-GP)
;;  T-of-GP-p    :  (temperature-of hplate-GP high-T-GP)
;;  T-of-GP-w    :  (temperature-of water-GP high-T-GP)
;;  to-reach-GP  :  (to-reach initst-GP-1 goalst-GP)
;;  water-GP     :  (inst-of water-GP water)


;;;;;;  End of file  AMBR/KB/EPISODIC/B_GP.LSP
