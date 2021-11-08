;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/b_MTF.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Base situation MTF -- 'Milk in a Teapot in a Frdige.'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    30-05-98 [3.0.0]  Elaboration of SIT-MTF from old LTM.LSP.
;;; UPDATED:    18-06-98  Removed IS-GOAL and RESULT propositns. Wght adjustmt.
;;;                       CAUSE consequents are propositions now, not states.
;;;                       T-OF-MTF-M1 and T-OF-MTF-M2 coalesced together.
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;         SITUATION  M T F           ;;;;;;;;
         ;;;;;;;;   Milk in a Teapot in a Fridge     ;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Base situation MTF   ;;;;;;;;;;;
;;;;
;;;; There is some milk in a teapot on a fridge.
;;;; The fridge is cold.  The color of the teapot
;;;; is green.
;;;;
;;;; The goal is to cool the milk.
;;;;
;;;; Since the milk is in the teapot which, in turn,
;;;; is in the fridge, the milk is in the fridge too.
;;;; This causes the milk to become cold.
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + ICF -- Ice Cube in a glass in a Fridge.  ; isomorphic
;;;;  + BPF -- Butter in a Plate in a Fridge.    ; isomorphic
;;;;  + FDO -- Food in a Dish in an Oven.        ; isomorphic
;;;;  + ...
;;;;  * ...


;;;;;;;  Situation-agent
;;

(defagent   sit-MTF    instance-agent
  "Milk in a Teapot in a Fridge."
  :type      (:instance :situation)
  :inst-of   (situation 0.1)
  :a-link    ((fridge-MTF 0.5)
              (low-T-MTF  0.5)
              (T-of-MTF-m 1.0) )
)


;;;;;;;  Participating objects
;;
;;  milk-MTF   :  (inst-of milk)
;;  tpot-MTF   :  (inst-of teapot)
;;  fridge-MTF :  (inst-of fridge)
;;

(defagent   milk-MTF    instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-MTF 0.2)
  :inst-of    milk
  :c-coref    (((T-of-MTF-m . :slot1) 1.0)
               ((in-MTF-mt  . :slot1) 0.2)
               ((in-MTF-mf  . :slot1) 0.1)
               ((goalst-MTF . :slot3) 0.1) )
  :a-link     (initst-MTF-1 0.1)
)

(defagent   tpot-MTF    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-MTF 0.2)
  :inst-of    teapot
  :c-coref    (((in-MTF-mt . :slot2) 0.5)
               ((in-MTF-tf . :slot1) 0.5)
               ((color-of-MTF . :slot1) 0.1) )
  :a-link     (initst-MTF-1 0.1)
)

(defagent   fridge-MTF    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-MTF 0.5)
  :inst-of    fridge
  :c-coref    (((T-of-MTF-f  . :slot1) 0.75)
               ((in-MTF-tf   . :slot2) 0.25)
               ((in-MTF-mf   . :slot2) 0.10)
               ((interst-MTF . :slot3) 0.10) )
  :a-link     ((low-T-MTF    0.5)
               (initst-MTF-1 0.1) )
)


;;;;;;;  Initial relations
;;
;;  in-MTF-mt    : (in milk-MTF tpot-MTF)
;;  in-MTF-tf    : (in tpot-MTF fridge-MTF)
;;  T-of-MTF-f   : (temperature-of fridge-MTF low-T-MTF)
;;  color-of-MTF : (color-of tpot-MTF green-MTF)
;;

(defagent   in-MTF-mt    instance-agent
  "(in milk-MTF tpot-MTF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-MTF 0.2)
  :inst-of    in
  :c-coref    (((initst-MTF-1 . :slot3) 0.2)
               ((initst-MTF-2 . :slot1) 0.1) )
  :a-link     ((in-MTF-tf   0.1)
               (in-MTF-mf   0.1)
               (cause-MTF-i 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    milk-MTF
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    tpot-MTF
)

(defagent   in-MTF-tf    instance-agent
  "(in tpot-MTF fridge-MTF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-MTF 0.2)
  :inst-of    in
  :c-coref    (((initst-MTF-1 . :slot4) 0.2)
               ((initst-MTF-2 . :slot2) 0.1) )
  :a-link     ((in-MTF-mf 0.1)
               (in-MTF-mt 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    tpot-MTF
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    fridge-MTF
)

(defagent   T-of-MTF-f    instance-agent
  "(temperature-of fridge-MTF low-T-MTF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-MTF 0.3)
  :inst-of    temperature-of
  :c-coref    (((initst-MTF-1 . :slot1) 0.3)
               ((interst-MTF  . :slot2) 0.1) )
  :a-link     ((T-of-MTF-m  0.2)
               (cause-MTF-t 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    fridge-MTF
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    low-T-MTF
)
(defagent  low-T-MTF   instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal :result)
  :situation  (sit-MTF 0.3)
  :inst-of    low-temp
  :c-coref    (((T-of-MTF-f   . :slot2) 0.5)
               ((T-of-MTF-m   . :slot2) 0.5)
               ((initst-MTF-1 . :slot2) 0.1)
               ((goalst-MTF   . :slot2) 0.1) )
  :a-link     (fridge-MTF 0.5)
)

(defagent   color-of-MTF    instance-agent
  "(color-of tpot-MTF green-MTF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-MTF 0.1)
  :inst-of    color-of
  :slot1
    :type     :aspect
    :inst-of  (color-of . :slot1)
    :c-coref  tpot-MTF
  :slot2
    :type     :aspect
    :inst-of  (color-of . :slot2)
    :c-coref  green-MTF
)
(defagent   green-MTF  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-MTF 0.1)
  :inst-of    green
  :c-coref    (color-of-MTF . :slot2)
  :a-link     (tpot-MTF 1.0)
)


;;;;;;;  Initial states
;;
;;  initst-MTF-1  -to-reach->  goalst-MTF
;;  initst-MTF-1  -follows->   endst-MTF
;;  initst-MTF-2  --cause-->   in-MTF-mf
;;
;;  initst-MTF-1  :  (init-state T-of-MTF-f low-T-MTF in-MTF-mt in-MTF-tf)
;;  initst-MTF-2  :  (init-state in-MTF-mt in-MTF-tf)
;;

(defagent   initst-MTF-1  instance-agent
  "initst-MTF-1  -to-reach->  goalst-MTF"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-MTF 0.2)
  :inst-of    init-state
  :c-coref    ((to-reach-MTF . :slot1)
               (follows-MTF  . :slot1) )
  :a-link     ((goalst-MTF   1.0)
               (initst-MTF-2 0.2)
               (milk-MTF     0.2)
               (tpot-MTF     0.2)
               (fridge-MTF   0.2) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-MTF-f
  :slot2
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    low-T-MTF
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-MTF-mt
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-MTF-tf
)

(defagent   initst-MTF-2  instance-agent
  "initst-MTF-2  -cause->  in-MTF-mf"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-MTF 0.2)
  :inst-of    init-state
  :c-coref    (cause-MTF-i . :slot1)
  :a-link     ((interst-MTF  1.0)
               (initst-MTF-1 0.3)
               (milk-MTF     0.3) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-MTF-mt
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-MTF-tf
)


;;;;;;;  Goal state
;;
;;  goalst-MTF  <-to-reach-  initst-MTF-1
;;  goalst-MTF   :  (goal-state T-of-MTF-m low-T-MTF milk-MTF)
;;
;;  T-of-MTF-m   :  (temperature-of milk-MTF low-T-MTF)
;;  to-reach-MTF :  (to-reach initst-MTF-1 goalst-MTF)
;;

(defagent   T-of-MTF-m    instance-agent
  "(temperature-of milk-MTF low-T-MTF)"
  :type       (:instance  :relation)
  :modality   (:GOAL   :intend-true
               :RESULT :true )
  :situation  (sit-MTF 0.3)
  :inst-of    temperature-of
  :c-coref    (((cause-MTF-t . :slot2) 0.2)
               ((goalst-MTF  . :slot1) 0.2)
               ((endst-MTF   . :slot1) 0.2) )
  :a-link     ((T-of-MTF-f 0.2)
               (in-MTF-mf  0.1)
               (fridge-MTF 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    milk-MTF
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    low-T-MTF
)

(defagent   goalst-MTF    instance-agent
  "goalst-MTF  <-to-reach-  initst-MTF-1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-MTF 0.2)
  :inst-of    goal-state
  :c-coref    (to-reach-MTF . :slot2)
  :a-link     ((initst-MTF-1 0.5)
               (endst-MTF    0.2) )
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-MTF-m
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    low-T-MTF
  :slot3
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    milk-MTF
)

(defagent   to-reach-MTF  instance-agent
  "(to-reach initst-MTF-1 goalst-MTF)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-MTF 0.2)
  :inst-of    to-reach
  :a-link     ((follows-MTF 0.5)
               (cause-MTF-i 0.1)
               (cause-MTF-t 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-MTF-1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-MTF
)


;;;;;;;  Intermediary state
;;
;;  interst-MTF  --cause->  T-of-MTF-m
;;  interst-MTF   :  (inter-state in-MTF-mf T-of-MTF-f fridge-MTF)
;;
;;  in-MTF-mf     :  (in milk-MTF fridge-MTF)
;;  cause-MTF-i   :  (cause initst-MTF-2 in-MTF-mf)
;;

(defagent   in-MTF-mf    instance-agent
  "(in milk-MTF fridge-MTF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-MTF 0.2)
  :inst-of    in
  :c-coref    (((cause-MTF-i . :slot2) 0.5)
               ((interst-MTF . :slot1) 0.3) )
  :a-link     ((in-MTF-mt 0.3)
               (in-MTF-tf 0.2) )
  :slot1
    :type      :aspect
    :inst-of   (in . :slot1)
    :c-coref   milk-MTF
  :slot2
    :type      :aspect
    :inst-of   (in . :slot2)
    :c-coref   fridge-MTF
)

(defagent   interst-MTF   instance-agent
  "interst-MTF  -cause->  T-of-MTF-m"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-MTF 0.2)
  :inst-of    inter-state
  :c-coref    (cause-MTF-t . :slot1)
  :a-link     ((endst-MTF    0.6)
               (initst-MTF-2 0.4)
               (cause-MTF-i  0.2) )
  :slot1
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    in-MTF-mf
  :slot2
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    T-of-MTF-f
  :slot3
    :type       :aspect
    :inst-of    (inter-state . :slot1)
    :c-coref    fridge-MTF
)

(defagent   cause-MTF-i   instance-agent
  "(cause initst-MTF-2 in-MTF-mf)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-MTF 0.2)
  :inst-of    cause
  :a-link     ((interst-MTF  1.0)
               (cause-MTF-t  0.5)
               (follows-MTF  0.3)
               (to-reach-MTF 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    initst-MTF-2
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    in-MTF-mf
)


;;;;;;;  End state
;;
;;  endst-MTF  <-follows-  initst-MTF-1
;;  endst-MTF     :  (end-state T-of-MTF-m)
;;
;;  follows-MTF   :  (follows initst-MTF-1 endst-MTF)
;;  cause-MTF-t   :  (cause interst-MTF T-of-MTF-m)
;;

(defagent   endst-MTF    instance-agent
  "endst-MTF  <-follows-  initst-MTF-1"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-MTF 0.2)
  :inst-of    end-state
  :c-coref    (follows-MTF . :slot2)
  :a-link     ((interst-MTF 0.1)
               (goalst-MTF  0.2)
               (cause-MTF-t 0.1) )
  :slot1
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    T-of-MTF-m
)

(defagent   follows-MTF    instance-agent
  "(follows initst-MTF-1 endst-MTF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-MTF 0.2)
  :inst-of    follows
  :a-link     ((to-reach-MTF 0.5)
               (cause-MTF-t  0.2) )
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-MTF-1
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-MTF
)

(defagent   cause-MTF-t    instance-agent
  "(cause interst-MTF T-of-MTF-m)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-MTF 0.2)
  :inst-of    cause
  :a-link     ((endst-MTF    0.5)
               (follows-MTF  0.3)
               (cause-MTF-i  0.2)
               (to-reach-MTF 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    interst-MTF
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    T-of-MTF-m
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-MTF
  "Milk in a Teapot in a Fridge."
  :head      sit-MTF                      ; 21 agents
  :members   (sit-MTF
              milk-MTF      tpot-MTF      fridge-MTF
              in-MTF-mt     in-MTF-tf     in-MTF-mf
              T-of-MTF-f    T-of-MTF-m    low-T-MTF
              color-of-MTF  green-MTF
              initst-MTF-1  initst-MTF-2  interst-MTF
              goalst-MTF    endst-MTF
              to-reach-MTF  follows-MTF
              cause-MTF-i   cause-MTF-t
             ))

(GENKB-template
  :herald  "Base sit.MTF -- Milk in a Teapot in a Fridge, ver.3.0.0."
  :templates '(
    (milk           (:instance (milk-MTF     2))
                    (:a-link   (T-of-MTF-m  0.1)) )
    (teapot         (:instance (tpot-MTF     1)) )
    (fridge         (:instance (fridge-MTF   5))
                    (:a-link   (T-of-MTF-f  0.2)) )
    (temperature-of (:instance (T-of-MTF-m   3) (T-of-MTF-f 3))
                    (:a-link   (low-T-MTF   0.1)) )
    (low-temp       (:instance (low-T-MTF    3))
                    (:a-link   (fridge-MTF  0.2)) )
    (in             (:instance (in-MTF-mt    1) (in-MTF-tf  1)) )
    (color-of       (:instance (color-of-MTF 1)) )
    (green          (:instance (green-MTF    1)) )
))


;;;;;;;;  Propositional representaion
;;
;;  sit-MTF       :  (inst-of sit-MTF situation)
;;
;;  cause-MTF-i   :  (cause initst-MTF-2 in-MTF-mf)
;;  cause-MTF-t   :  (cause interst-MTF  T-of-MTF-m)
;;  color-of-MTF  :  (color-of tpot-MTF green-MTF)
;;  endst-MTF     :  (end-state T-of-MTF-m)
;;  follows-MTF   :  (follows initst-MTF-1 endst-MTF)
;;  fridge-MTF    :  (inst-of fridge-MTF fridge)
;;  goalst-MTF    :  (goal-state T-of-MTF-m low-T-MTF milk-MTF)
;;  green-MTF     :  (inst-of green-MTF green)
;;  initst-MTF-1  :  (init-state T-of-MTF-f low-T-MTF in-MTF-mt in-MTF-tf)
;;  initst-MTF-2  :  (init-state in-MTF-mt in-MTF-tf)
;;  interst-MTF   :  (inter-state in-MTF-mf T-of-MTF-f fridge-MTF)
;;  in-MTF-mf     :  (in milk-MTF fridge-MTF)
;;  in-MTF-mt     :  (in milk-MTF tpot-MTF)
;;  in-MTF-tf     :  (in tpot-MTF fridge-MTF)
;;  low-T-MTF     :  (inst-of low-T-MTF low-temp)
;;  milk-MTF      :  (inst-of milk-MTF milk)
;;  T-of-MTF-f    :  (temperature-of fridge-MTF low-T-MTF)
;;  T-of-MTF-m    :  (temperature-of milk-MTF low-T-MTF)
;;  to-reach-MTF  :  (to-reach initst-MTF-1 goalst-MTF)
;;  tpot-MTF      :  (inst-of tpot-MTF teapot)


;;;;;;  End of file  AMBR/KB/EPISODIC/B_MTF.LSP
