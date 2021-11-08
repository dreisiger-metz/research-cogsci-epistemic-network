;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/b_BPF.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Base situation BPF -- 'Butter on a Plate in a Fridge.'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    31-05-98 [3.0.0]  Elaboration of SIT-BPF from old LTM.LSP.
;;; UPDATED:    18-06-98  Removed IS-GOAL and RESULT propositns. Wght adjustmt.
;;;                       CAUSE consequents are propositions now, not states.
;;;                       T-OF-BPF-B1 and T-OF-BPF-B2 coalesced together.
;;; UPDATED:    ...

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;;;;;;         SITUATION  B P F           ;;;;;;;;
          ;;;;;;;;   Butter on a Plate in a Fridge    ;;;;;;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Base situation BPF   ;;;;;;;;;;;
;;;;
;;;; There is some butter on a plate in a fridge.
;;;; The fridge is cold.  The plate is made of
;;;; china and its shape is circular.
;;;;
;;;; The goal is to cool the butter.
;;;;
;;;; Since the butter is on the plate which, in turn,
;;;; is in the fridge, the butter is in the fridge too.
;;;; This causes the butter to become cold.
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + MTF -- Milk in a Teapot in a Fridge.
;;;;  + ICF -- Ice Cube in a glass in a Fridge.
;;;;  + FDO -- Food in a Dish in an Oven.        ; isomorphic
;;;;  + SFF -- Salt in Food on a plate in a Fridge.
;;;;  + ...
;;;;  * ...


;;;;;;;  Situation-agent
;;

(defagent   sit-BPF    instance-agent
  "Butter on a Plate in a Fridge."
  :type      (:instance :situation)
  :inst-of   (situation 0.1)
  :a-link    ((butter-BPF 1.0)
              (plate-BPF  0.5) )
)


;;;;;;;  Participating objects
;;
;;  butter-BPF :  (inst-of butter)
;;  plate-BPF    :  (inst-of plate)
;;  fridge-BPF   :  (inst-of fridge)
;;

(defagent   butter-BPF    instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-BPF 0.5)
  :inst-of    butter
  :c-coref    (((T-of-BPF-b . :slot1) 0.5)
               ((on-BPF     . :slot2) 0.3)
               ((in-BPF-bf  . :slot1) 0.1)
               ((goalst-BPF . :slot3) 0.1) )
  :a-link     (initst-BPF-1 0.1)
)

(defagent   plate-BPF    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-BPF 0.5)
  :inst-of    plate
  :c-coref    (((on-BPF       . :slot1) 0.5)
               ((in-BPF-pf    . :slot1) 0.5)
               ((made-of-BPF  . :slot1) 0.2)
               ((shape-of-BPF . :slot1) 0.1) )
  :a-link     (initst-BPF-1 0.1)
  :slot1
    :type       :aspect
    :inst-of    (plate . :slot1)
    :c-coref    (made-of-BPF 0.2)
    :a-link     (mchina-BPF  0.2)
  :slot2
    :type       :aspect
    :inst-of    (plate . :slot1)
    :c-coref    (shape-of-BPF 0.1)
    :a-link     (circular-BPF 0.1)
)

(defagent   fridge-BPF    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-BPF 0.2)
  :inst-of    fridge
  :c-coref    (((T-of-BPF-f  . :slot1) 0.75)
               ((in-BPF-pf   . :slot2) 0.25)
               ((in-BPF-bf   . :slot2) 0.10)
               ((interst-BPF . :slot3) 0.10) )
  :a-link     ((low-T-BPF    0.5)
               (initst-BPF-1 0.1) )
)


;;;;;;;  Initial relations
;;
;;  on-BPF       : (on plate-BPF butter-BPF)
;;  in-BPF-pf    : (in plate-BPF fridge-BPF)
;;  T-of-BPF-f   : (temperature-of fridge-BPF low-T-BPF)
;;  made-of-BPF  : (made-of plate-BPF mchina-BPF)
;;  shape-of-BPF : (shape-of plate-BPF circular-BPF)
;;

(defagent   on-BPF    instance-agent
  "(in plate-BPF butter-BPF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-BPF 0.2)
  :inst-of    on
  :c-coref    (((initst-BPF-1 . :slot3) 0.2)
               ((initst-BPF-2 . :slot1) 0.1) )
  :a-link     ((in-BPF-pf   0.1)
               (in-BPF-bf   0.1)
               (cause-BPF-i 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    plate-BPF
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    butter-BPF
)

(defagent   in-BPF-pf    instance-agent
  "(in plate-BPF fridge-BPF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-BPF 0.2)
  :inst-of    in
  :c-coref    (((initst-BPF-1 . :slot4) 0.2)
               ((initst-BPF-2 . :slot2) 0.1) )
  :a-link     ((in-BPF-bf 0.2)
               (on-BPF    0.2) )
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    plate-BPF
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    fridge-BPF
)

(defagent   T-of-BPF-f    instance-agent
  "(temperature-of fridge-BPF low-T-BPF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-BPF 0.2)
  :inst-of    temperature-of
  :c-coref    (((initst-BPF-1 . :slot1) 0.2)
               ((interst-BPF  . :slot2) 0.1) )
  :a-link     ((T-of-BPF-b  0.2)
               (cause-BPF-t 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    fridge-BPF
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    low-T-BPF
)
(defagent  low-T-BPF   instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal :result)
  :situation  (sit-BPF 0.2)
  :inst-of    low-temp
  :c-coref    (((T-of-BPF-f   . :slot2) 0.5)
               ((T-of-BPF-b   . :slot2) 0.5)
               ((initst-BPF-1 . :slot2) 0.1)
               ((goalst-BPF   . :slot2) 0.1) )
  :a-link     (fridge-BPF 0.5)
)

(defagent   made-of-BPF    instance-agent
  "(made-of plate-BPF mchina-BPF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-BPF 0.1)
  :c-coref    (plate-BPF . :slot1)
  :inst-of    made-of
  :slot1
    :type     :aspect
    :inst-of  (made-of . :slot1)
    :c-coref  plate-BPF
  :slot2
    :type     :aspect
    :inst-of  (made-of . :slot2)
    :c-coref  mchina-BPF
)
(defagent   mchina-BPF  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-BPF 0.1)
  :inst-of    material-china
  :c-coref    (made-of-BPF . :slot2)
  :a-link     (plate-BPF 1.0)
)

(defagent   shape-of-BPF    instance-agent
  "(shape-of plate-BPF circular-BPF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-BPF 0.1)
  :c-coref    (plate-BPF . :slot2)
  :inst-of    shape-of
  :slot1
    :type     :aspect
    :inst-of  (shape-of . :slot1)
    :c-coref  plate-BPF
  :slot2
    :type     :aspect
    :inst-of  (shape-of . :slot2)
    :c-coref  circular-BPF
)
(defagent   circular-BPF  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-BPF 0.1)
  :inst-of    circular-shape
  :c-coref    (shape-of-BPF . :slot2)
  :a-link     (plate-BPF 1.0)
)


;;;;;;;  Initial states
;;
;;  initst-BPF-1  -to-reach->  goalst-BPF
;;  initst-BPF-1  -follows->   endst-BPF
;;  initst-BPF-2  --cause-->   in-BPF-bf
;;
;;  initst-BPF-1  :  (init-state T-of-BPF-f low-T-BPF on-BPF in-BPF-pf)
;;  initst-BPF-2  :  (init-state on-BPF in-BPF-pf)
;;

(defagent   initst-BPF-1  instance-agent
  "initst-BPF-1  -to-reach->  goalst-BPF"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-BPF 0.2)
  :inst-of    init-state
  :c-coref    (((to-reach-BPF . :slot1) 0.5)
               ((follows-BPF  . :slot1) 0.5) )
  :a-link     ((goalst-BPF   0.5)
               (initst-BPF-2 0.2)
               (butter-BPF   0.2)
               (plate-BPF    0.2)
               (fridge-BPF   0.2) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-BPF-f
  :slot2
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    low-T-BPF
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-BPF
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-BPF-pf
)

(defagent   initst-BPF-2  instance-agent
  "initst-BPF-2  -cause->  in-BPF-bf"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-BPF 0.2)
  :inst-of    init-state
  :c-coref    (cause-BPF-i . :slot1)
  :a-link     ((interst-BPF  1.0)
               (initst-BPF-1 0.3)
               (butter-BPF   0.3) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-BPF
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-BPF-pf
)


;;;;;;;  Goal state
;;
;;  goalst-BPF  <-to-reach-  initst-BPF-1
;;  goalst-BPF   :  (goal-state T-of-BPF-b low-T-BPF butter-BPF)
;;
;;  T-of-BPF-b   :  (temperature-of butter-BPF low-T-BPF)
;;  to-reach-BPF :  (to-reach initst-BPF-1 goalst-BPF)
;;

(defagent   T-of-BPF-b    instance-agent
  "(temperature-of butter-BPF low-T-BPF)"
  :type       (:instance  :relation)
  :modality   (:GOAL   :intend-true
               :RESULT :true )
  :situation  (sit-BPF 0.2)
  :inst-of    temperature-of
  :c-coref    (((cause-BPF-t . :slot2) 0.2)
               ((goalst-BPF  . :slot1) 0.2)
               ((endst-BPF   . :slot1) 0.2) )
  :a-link     ((T-of-BPF-f 0.2)
               (in-BPF-bf  0.1)
               (fridge-BPF 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    butter-BPF
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    low-T-BPF
)

(defagent   goalst-BPF    instance-agent
  "goalst-BPF  <-to-reach-  initst-BPF-1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-BPF 0.2)
  :inst-of    goal-state
  :c-coref    (to-reach-BPF . :slot2)
  :a-link     ((initst-BPF-1 0.5)
               (endst-BPF    0.2) )
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-BPF-b
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    low-T-BPF
  :slot3
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    butter-BPF
)

(defagent   to-reach-BPF    instance-agent
  "(to-reach initst-BPF-1 goalst-BPF)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-BPF 0.2)
  :inst-of    to-reach
  :a-link     ((follows-BPF 0.5)
               (cause-BPF-i 0.1)
               (cause-BPF-t 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-BPF-1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-BPF
)


;;;;;;;  Intermediary state
;;
;;  interst-BPF  <-cause--  initst-BPF-2
;;  interst-BPF  --cause->  T-of-BPF-b
;;  interst-BPF   :  (inter-state in-BPF-bf T-of-BPF-f fridge-BPF)
;;
;;  in-BPF-bf     :  (in butter-BPF fridge-BPF)
;;  cause-BPF-i   :  (cause initst-BPF-2 in-BPF-bf)
;;

(defagent   in-BPF-bf    instance-agent
  "(in butter-BPF fridge-BPF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-BPF 0.2)
  :inst-of    in
  :c-coref    (((cause-BPF-i . :slot2) 0.5)
               ((interst-BPF . :slot1) 0.3) )
  :a-link     ((on-BPF    0.3)
               (in-BPF-pf 0.2) )
  :slot1
    :type      :aspect
    :inst-of   (in . :slot1)
    :c-coref   butter-BPF
  :slot2
    :type      :aspect
    :inst-of   (in . :slot2)
    :c-coref   fridge-BPF
)

(defagent   interst-BPF   instance-agent
  "interst-BPF  -cause->  T-of-BPF-b"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-BPF 0.2)
  :inst-of    inter-state
  :c-coref    (cause-BPF-t . :slot1)
  :a-link     ((cause-BPF-i  0.2)
               (endst-BPF    0.6)
               (initst-BPF-2 0.4) )
  :slot1
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    in-BPF-bf
  :slot2
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    T-of-BPF-f
  :slot3
    :type       :aspect
    :inst-of    (inter-state . :slot1)
    :c-coref    fridge-BPF
)

(defagent   cause-BPF-i   instance-agent
  "(cause initst-BPF-2 in-BPF-bf)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-BPF 0.2)
  :inst-of    cause
  :a-link     ((interst-BPF  0.8)
               (cause-BPF-t  0.5)
               (follows-BPF  0.3)
               (to-reach-BPF 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    initst-BPF-2
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    in-BPF-bf
)


;;;;;;;  End state
;;
;;  endst-BPF  <-follows-  initst-BPF-1
;;  endst-BPF     :  (end-state T-of-BPF-b)
;;
;;  follows-BPF   :  (follows initst-BPF-1 endst-BPF)
;;  cause-BPF-t   :  (cause interst-BPF T-of-BPF-b)
;;

(defagent   endst-BPF    instance-agent
  "endst-BPF  <-follows-  initst-BPF-1"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-BPF 0.2)
  :inst-of    end-state
  :c-coref    (follows-BPF . :slot2)
  :a-link     ((cause-BPF-t 0.1)
               (interst-BPF 0.1)
               (goalst-BPF  0.2) )
  :slot1
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    T-of-BPF-b
)

(defagent   follows-BPF   instance-agent
  "(follows initst-BPF-1 endst-BPF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-BPF 0.2)
  :inst-of    follows
  :a-link     ((to-reach-BPF 0.5)
               (cause-BPF-t  0.2) )
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-BPF-1
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-BPF
)

(defagent   cause-BPF-t    instance-agent
  "(cause interst-BPF T-of-BPF-b)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-BPF 0.2)
  :inst-of    cause
  :a-link     ((endst-BPF    0.5)
               (follows-BPF  0.3)
               (cause-BPF-i  0.2)
               (to-reach-BPF 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    interst-BPF
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    T-of-BPF-b
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-BPF
  "Butter on a Plate in a Fridge."
  :head      sit-BPF                      ; 23 agents
  :members   (sit-BPF
              butter-BPF    plate-BPF     fridge-BPF
              on-BPF        in-BPF-pf     in-BPF-bf
              T-of-BPF-f    T-of-BPF-b    low-T-BPF
              made-of-BPF   mchina-BPF
              shape-of-BPF  circular-BPF
              initst-BPF-1  initst-BPF-2  interst-BPF
              goalst-BPF    endst-BPF
              to-reach-BPF  follows-BPF
              cause-BPF-i   cause-BPF-t
             ))

(GENKB-template
  :herald  "Base sit.BPF -- Butter on a Plate in a Fridge, ver.3.0.0."
  :templates '(
    (butter         (:instance (butter-BPF   2)) )
    (plate          (:instance (plate-BPF    1)) )
    (fridge         (:instance (fridge-BPF   1))
                    (:a-link   (T-of-BPF-f  0.1)) )
    (temperature-of (:instance (T-of-BPF-b   1) (T-of-BPF-f 1)) )
    (low-temp       (:instance (low-T-BPF    1)) )
    (in             (:instance (in-BPF-pf    1)) )
    (on             (:instance (on-BPF       1)) )
    (made-of        (:instance (made-of-BPF  1)) )
    (material-china (:instance (mchina-BPF   1)) )
    (shape-of       (:instance (shape-of-BPF 1)) )
    (circular-shape (:instance (circular-BPF 1)) )
))


;;;;;;;;  Propositional representaion
;;
;;  sit-BPF       :  (inst-of sit-BPF situation)
;;
;;  butter-BPF    :  (inst-of butter-BPF butter)
;;  cause-BPF-i   :  (cause initst-BPF-2 in-BPF-bf)
;;  cause-BPF-t   :  (cause interst-BPF  T-of-BPF-b)
;;  circular-BPF  :  (inst-of circular-BPF circular-shape)
;;  endst-BPF     :  (end-state T-of-BPF-b)
;;  follows-BPF   :  (follows initst-BPF-1 endst-BPF)
;;  fridge-BPF    :  (inst-of fridge-BPF fridge)
;;  goalst-BPF    :  (goal-state T-of-BPF-b low-T-BPF butter-BPF)
;;  initst-BPF-1  :  (init-state T-of-BPF-f low-T-BPF on-BPF in-BPF-pf)
;;  initst-BPF-2  :  (init-state on-BPF in-BPF-pf)
;;  interst-BPF   :  (inter-state in-BPF-bf T-of-BPF-f fridge-BPF)
;;  in-BPF-bf     :  (in butter-BPF fridge-BPF)
;;  in-BPF-pf     :  (in plate-BPF fridge-BPF)
;;  low-T-BPF     :  (inst-of low-T-BPF low-temp)
;;  made-of-BPF   :  (made-of plate-BPF mchina-BPF)
;;  mchina-BPF    :  (inst-of mchina-BPF material-china)
;;  on-BPF        :  (on plate-BPF butter-BPF)
;;  plate-BPF     :  (inst-of plate-BPF plate)
;;  shape-of-BPF  :  (shape-of plate-BPF circular-BPF)
;;  T-of-BPF-f    :  (temperature-of fridge-BPF low-T-BPF)
;;  T-of-BPF-b    :  (temperature-of butter-BPF low-T-BPF)   ; goal
;;  to-reach-BPF  :  (to-reach initst-BPF-1 goalst-BPF)


;;;;;;  End of file  AMBR/KB/EPISODIC/B_BPF.LSP
