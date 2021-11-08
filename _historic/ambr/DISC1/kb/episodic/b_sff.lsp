;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/b_SFF.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Base situation SFF -- 'Sugar in Tea in a Cup.'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    27-06-98 [3.0.0]
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;            SITUATION  S F F               ;;;;;;
         ;;;;;;    Salt in Food on a plate in a Fridge    ;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Base situation SFF   ;;;;;;;;;;;
;;;;
;;;; There is some food on a plate.  There is some
;;;; salt in the food.  The salt is salty.  The plate
;;;; is in a fridge.  The fridge is cold.
;;;;
;;;; The goal is that the food is cold.
;;;;
;;;; The result is that the food is both cold and
;;;; salty.  It is cold because of the fridge and
;;;; salty because of the salt in it.
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + STC -- Sugar in Tea in a Cup.
;;;;  + BPF -- Butter on a Plate in a Fridge.
;;;;  + ...
;;;;  * ...


;;;;;;;  Situation-agent
;;

(defagent   sit-SFF    instance-agent
  "Salt in Food on a plate in a Fridge."
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
  :a-link     ((salt-SFF   0.5)
               (food-SFF   0.3)
               (fridge-SFF 0.3) )
)


;;;;;;;  Participating objects
;;
;;  salt-SFF   :  (inst-of salt)
;;  food-SFF   :  (inst-of food)
;;  plate-SFF  :  (inst-of plate)
;;  fridge-SFF :  (inst-of fridge)
;;

(defagent   salt-SFF    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-SFF 0.3)
  :inst-of    salt
  :c-coref    (((taste-of-SFF-s . :slot1) 1.0)
               ((initst-SFF-1 . :slot2) 0.2)
               ((in-SFF-sf . :slot1) 0.2) )
  :a-link     (salty-SFF 0.5)
)

(defagent   food-SFF    instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-SFF 0.3)
  :inst-of    food
  :c-coref    (((T-of-SFF-fd    . :slot1) 0.4)
               ((taste-of-SFF-f . :slot1) 0.4)
               ((in-SFF-sf      . :slot2) 0.3)
               ((on-SFF         . :slot2) 0.3)
               ((in-SFF-ff      . :slot1) 0.1)
               ((initst-SFF-1   . :slot1) 0.1)
               ((goalst-SFF     . :slot3) 0.1) )
)

(defagent   plate-SFF    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-SFF 0.2)
  :inst-of    plate
  :c-coref    (((on-SFF    . :slot1) 0.5)
               ((in-SFF-pf . :slot1) 0.3) )
  :a-link     ((initst-SFF-1 0.1)
               (initst-SFF-2 0.2) )
)

(defagent   fridge-SFF    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-SFF 0.3)
  :inst-of    fridge
  :c-coref    (((T-of-SFF-fr . :slot1) 0.75)
               ((in-SFF-pf   . :slot2) 0.25)
               ((in-SFF-ff   . :slot2) 0.10)
               ((interst-SFF . :slot3) 0.10) )
  :a-link     ((low-T-SFF    0.5)
               (initst-SFF-2 0.1) )
)


;;;;;;;  Initial relations
;;
;;  in-SFF-sf      : (in salt-SFF food-SFF)
;;  on-SFF         : (on plate-SFF food-SFF)
;;  in-SFF-pf      : (in plate-SFF fridge-SFF)
;;  T-of-SFF-fr    : (temperature-of fridge-SFF low-T-SFF)
;;  taste-of-SFF-s : (taste-of salt-SFF salty-SFF)
;;

(defagent   in-SFF-sf    instance-agent
  "(in salt-SFF food-SFF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-SFF 0.2)
  :inst-of    in
  :c-coref    (((initst-SFF-1 . :slot4) 0.2)
               ((initst-SFF-3 . :slot1) 0.1) )
  :a-link     ((taste-of-SFF-f 0.3)
               (on-SFF 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    salt-SFF
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    food-SFF
)

(defagent   on-SFF  instance-agent
  "(on plate-SFF food-SFF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-SFF 0.2)
  :inst-of    on
  :c-coref    ((initst-SFF-2 . :slot1) 0.2)
  :a-link     ((in-SFF-pf 0.1)
               (in-SFF-ff 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    plate-SFF
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    food-SFF
)

(defagent   in-SFF-pf    instance-agent
  "(in plate-SFF fridge-SFF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-SFF 0.2)
  :inst-of    in
  :c-coref    ((initst-SFF-2 . :slot2) 0.2)
  :a-link     ((initst-SFF-1 0.1)
               (on-SFF       0.1)
               (in-SFF-ff    0.1) )
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    plate-SFF
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    fridge-SFF
)

(defagent   T-of-SFF-fr    instance-agent
  "(temperature-of fridge-SFF low-T-SFF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-SFF 0.2)
  :inst-of    temperature-of
  :c-coref    (((initst-SFF-1 . :slot5) 0.2)
               ((interst-SFF  . :slot2) 0.1) )
  :a-link     ((T-of-SFF-fd   0.2)
               (cause-SFF-tmp 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    fridge-SFF
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    low-T-SFF
)
(defagent  low-T-SFF   instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal :result)
  :situation  (sit-SFF 0.2)
  :inst-of    low-temp
  :c-coref    (((T-of-SFF-fr . :slot2) 0.5)
               ((T-of-SFF-fd . :slot2) 0.5)
               ((goalst-SFF   . :slot2) 0.1) )
  :a-link     ((fridge-SFF 0.5)
               (initst-SFF-1 0.1)
               (interst-SFF  0.1) )
)

(defagent   taste-of-SFF-s    instance-agent
  "(taste-of salt-SFF salty-SFF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-SFF 0.5)
  :inst-of    taste-of
  :c-coref    ((initst-SFF-3 . :slot2) 0.2)
  :a-link     ((taste-of-SFF-f 0.2)
               (cause-SFF-tst  0.2) )
  :slot1
    :type       :aspect
    :inst-of    (taste-of . :slot1)
    :c-coref    salt-SFF
  :slot2
    :type       :aspect
    :inst-of    (taste-of . :slot2)
    :c-coref    salty-SFF
)
(defagent  salty-SFF   instance-agent
  :type       (:instance  :object)
  :modality   (:init :result)
  :situation  (sit-SFF 0.2)
  :inst-of    salt-taste
  :c-coref    (((taste-of-SFF-s . :slot2) 0.7)
               ((taste-of-SFF-f . :slot2) 0.5)
               ((initst-SFF-3   . :slot3) 0.1) )
  :a-link     ((salt-SFF  0.7)
               (endst-SFF 0.1) )
)


;;;;;;;  Initial states
;;
;;  initst-SFF-1  -to-reach->  goalst-SFF
;;  initst-SFF-1  -follows->   endst-SFF
;;  initst-SFF-2  --cause-->   in-SFF-ff
;;  initst-SFF-3  --cause-->   taste-of-SFF-f
;;
;;  initst-SFF-1  :  (init-state in-SFF-ff T-of-SFF-fr fridge
;;                                           in-SFF-sf T-of-SFF-fr)
;;  initst-SFF-2  :  (init-state on-SFF in-SFF-pf)
;;  initst-SFF-3  :  (init-state in-SFF-sf taste-of-SFF-s salty-SFF)
;;

(defagent   initst-SFF-1  instance-agent
  "initst-SFF-1  -to-reach->  goalst-SFF"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-SFF 0.2)
  :inst-of    init-state
  :c-coref    (((to-reach-SFF . :slot1) 0.3)
               ((follows-SFF  . :slot1) 0.4) )
  :a-link     ((taste-of-SFF-s 0.2)
               (goalst-SFF     0.2)
               (endst-SFF      0.3)
               (initst-SFF-2   0.1)
               (initst-SFF-3   0.1) )
  :slot1
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    food-SFF
  :slot2
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    salt-SFF
  :slot3
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    fridge-SFF
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-SFF-sf
  :slot5
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-SFF-fr
)

(defagent   initst-SFF-2  instance-agent
  "initst-SFF-2  -cause->  in-SFF-ff"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-SFF 0.2)
  :inst-of    init-state
  :c-coref    (cause-SFF-i . :slot1)
  :a-link     ((plate-SFF    0.2)
               (initst-SFF-1 0.1)
               (interst-SFF  0.1) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-SFF
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-SFF-pf
)

(defagent   initst-SFF-3  instance-agent
  "initst-SFF-3  -cause->  taste-of-SFF-f"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-SFF 0.2)
  :inst-of    init-state
  :c-coref    (cause-SFF-tst . :slot1)
  :a-link     ((food-SFF     0.2)
               (initst-SFF-1 0.1)
               (endst-SFF    0.1) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-SFF-sf
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    taste-of-SFF-s
  :slot3
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    salty-SFF
)


;;;;;;;  Goal state
;;
;;  goalst-SFF  <-to-reach-  initst-SFF-1
;;  goalst-SFF     :  (goal-state T-of-SFF-fd low-T-SFF food-SFF)
;;
;;  T-of-SFF-fd    :  (temperature-of food-SFF low-T-SFF)
;;  to-reach-SFF   :  (to-reach initst-SFF-1 goalst-SFF)
;;

(defagent   T-of-SFF-fd    instance-agent
  "(temperature-of food-SFF low-T-SFF)"
  :type       (:instance  :relation)
  :modality   (:GOAL   :intend-true
               :RESULT :true )
  :situation  (sit-SFF 0.2)
  :inst-of    temperature-of
  :c-coref    (((cause-SFF-tmp . :slot2) 0.2)
               ((goalst-SFF    . :slot1) 0.1)
               ((endst-SFF     . :slot1) 0.2) )
  :a-link     ((T-of-SFF-fr 0.2)
               (fridge-SFF  0.2)
               (in-SFF-ff   0.1) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    food-SFF
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    low-T-SFF
)

(defagent   goalst-SFF    instance-agent
  "goalst-SFF  <-to-reach-  initst-SFF-1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-SFF 0.2)
  :inst-of    goal-state
  :c-coref    (to-reach-SFF . :slot2)
  :a-link     ((initst-SFF-1 0.3)
               (endst-SFF    0.2) )
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-SFF-fd
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    low-T-SFF
  :slot3
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    food-SFF
)

(defagent   to-reach-SFF    instance-agent
  "(to-reach initst-SFF-1 goalst-SFF)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-SFF 0.2)
  :inst-of    to-reach
  :a-link     ((follows-SFF   0.5)
               (cause-SFF-tmp 0.2)
               (cause-SFF-tst 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-SFF-1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-SFF
)


;;;;;;;  Intermediary state
;;
;;  interst-SFF  <-cause--  initst-SFF-1-2
;;  interst-SFF  --cause->  T-of-SFF-fd
;;  interst-SFF   :  (inter-state in-SFF-ff T-of-SFF-fr fridge-SFF)
;;
;;  in-SFF-ff     :  (in food-SFF fridge-SFF)
;;  cause-SFF-i   :  (cause initst-SFF-2 in-SFF-ff)
;;

(defagent   in-SFF-ff    instance-agent
  "(in food-SFF fridge-SFF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-SFF 0.2)
  :inst-of    in
  :c-coref    (((cause-SFF-i . :slot2) 0.5)
               ((interst-SFF . :slot1) 0.3) )
  :a-link     ((on-SFF    0.3)
               (in-SFF-pf 0.2) )
  :slot1
    :type      :aspect
    :inst-of   (in . :slot1)
    :c-coref   food-SFF
  :slot2
    :type      :aspect
    :inst-of   (in . :slot2)
    :c-coref   fridge-SFF
)

(defagent   interst-SFF   instance-agent
  "interst-SFF  -cause->  T-of-SFF-fd"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-SFF 0.2)
  :inst-of    inter-state
  :c-coref    (cause-SFF-tmp . :slot1)
  :a-link     ((initst-SFF-2 0.3)
               (endst-SFF    0.3)
               (cause-SFF-i  0.2) )
  :slot1
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    in-SFF-ff
  :slot2
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    T-of-SFF-fr
  :slot3
    :type       :aspect
    :inst-of    (inter-state . :slot1)
    :c-coref    fridge-SFF
)

(defagent   cause-SFF-i   instance-agent
  "(cause initst-SFF-2 in-SFF-ff)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-SFF 0.2)
  :inst-of    cause
  :a-link     ((interst-SFF   0.8)
               (cause-SFF-tmp 0.5)
               (cause-SFF-tst 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    initst-SFF-2
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    in-SFF-ff
)


;;;;;;;  End state
;;
;;  endst-SFF  <-follows-  initst-SFF-1
;;  endst-SFF      :  (end-state T-of-SFF-fd taste-of-SFF-f)
;;
;;  taste-of-SFF-f :  (taste-of food-SFF salty-SFF)
;;  follows-SFF    :  (follows initst-SFF-1 endst-SFF)
;;  cause-SFF-tmp  :  (cause interst-SFF T-of-SFF-fd)
;;  cause-SFF-tst  :  (cause initst-SFF-3 taste-of-SFF-f)
;;

(defagent   taste-of-SFF-f    instance-agent
  "(taste-of food-SFF salty-SFF)"
  :type       (:instance  :relation)
  :modality   (:RESULT :true )
  :situation  (sit-SFF 0.2)
  :inst-of    taste-of
  :c-coref    (((cause-SFF-tst . :slot2) 0.2)
               ((endst-SFF     . :slot2) 0.2) )
  :a-link     ((taste-of-SFF-s 0.5)
               (salt-SFF 0.3) )
  :slot1
    :type       :aspect
    :inst-of    (taste-of . :slot1)
    :c-coref    food-SFF
  :slot2
    :type       :aspect
    :inst-of    (taste-of . :slot2)
    :c-coref    salty-SFF
)

(defagent   endst-SFF    instance-agent
  "endst-SFF  <-follows-  initst-SFF-1"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-SFF 0.2)
  :inst-of    end-state
  :c-coref    (follows-SFF . :slot2)
  :a-link     ((goalst-SFF    0.3)
               (interst-SFF   0.1)
               (initst-SFF-3  0.1)
               (cause-SFF-tmp 0.1)
               (cause-SFF-tst 0.1) )
  :slot1
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    T-of-SFF-fd
  :slot2
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    taste-of-SFF-f
)

(defagent   follows-SFF  instance-agent
  "(follows initst-SFF-1 endst-SFF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-SFF 0.2)
  :inst-of    follows
  :a-link     ((to-reach-SFF  0.3)
               (cause-SFF-tmp 0.2)
               (cause-SFF-tst 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-SFF-1
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-SFF
)

(defagent   cause-SFF-tmp    instance-agent
  "(cause interst-SFF T-of-SFF-fd)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-SFF 0.2)
  :inst-of    cause
  :a-link     ((endst-SFF     0.5)
               (goalst-SFF    0.4)
               (follows-SFF   0.2)
               (cause-SFF-tst 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    interst-SFF
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    T-of-SFF-fd
)

(defagent   cause-SFF-tst    instance-agent
  "(cause initst-SFF-3 taste-of-SFF-f)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-SFF 0.2)
  :inst-of    cause
  :a-link     ((salt-SFF  0.5)
               (endst-SFF 0.5)
               (follows-SFF   0.1)
               (cause-SFF-tmp 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    initst-SFF-3
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    taste-of-SFF-f
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-SFF
  "Salt in Food on a plate in a Fridge."
  :head      sit-SFF                          ; 26 agents
  :members   (sit-SFF
              salt-SFF        food-SFF        fridge-SFF
              plate-SFF       on-SFF
              in-SFF-sf       in-SFF-pf       in-SFF-ff
              T-of-SFF-fr     T-of-SFF-fd     low-T-SFF
              taste-of-SFF-s  taste-of-SFF-f  salty-SFF
              initst-SFF-1    initst-SFF-2
              initst-SFF-3    interst-SFF
              goalst-SFF      endst-SFF
              to-reach-SFF    follows-SFF
              cause-SFF-i     cause-SFF-tmp   cause-SFF-tst
             ))

(GENKB-template
  :herald  "Base sit.SFF -- Salt in Food on a plate in a Fridge, ver.3.0.0."
  :templates '(
    (food           (:instance (food-SFF        1)) )
    (plate          (:instance (plate-SFF       1)) )
    (fridge         (:instance (fridge-SFF      1)) )
    (salt           (:instance (salt-SFF        3))
                    (:a-link   (taste-of-SFF-s 0.1)) )
    (taste-of       (:instance (taste-of-SFF-f  1) (taste-of-SFF-s 2))
                    (:a-link   (salty-SFF      0.1)) )
    (salt-taste     (:instance (salty-SFF       2))
                    (:a-link   (salt-SFF       0.1)) )
    (temperature-of (:instance (T-of-SFF-fd     1) (T-of-SFF-fr 1)) )
    (low-temp       (:instance (low-T-SFF       1)) )
    (in             (:instance (in-SFF-sf       1) (in-SFF-pf   1)) )
    (on             (:instance (on-SFF          1)) )
))


;;;;;;;;  Propositional representaion
;;
;;  sit-SFF        :  (inst-of sit-SFF situation)
;;
;;  cause-SFF-i    :  (cause initst-SFF-2 in-SFF-ff)
;;  cause-SFF-tmp  :  (cause interst-SFF T-of-SFF-fd)
;;  cause-SFF-tst  :  (cause initst-SFF-3 taste-of-SFF-f)
;;  endst-SFF      :  (end-state T-of-SFF-fd taste-of-SFF-f)
;;  follows-SFF    :  (follows initst-SFF-1 endst-SFF)
;;  food-SFF       :  (inst-of food-SFF food)
;;  fridge-SFF     :  (inst-of fridge-SFF fridge)
;;  goalst-SFF     :  (goal-state T-of-SFF-fd low-T-SFF food-SFF)
;;  in-SFF-ff      :  (in food-SFF fridge-SFF)
;;  in-SFF-pf      :  (in plate-SFF fridge-SFF)
;;  in-SFF-sf      :  (in salt-SFF food-SFF)
;;  initst-SFF-1   :  (init-state in-SFF-ff T-of-SFF-fr fridge
;;                                            in-SFF-sf T-of-SFF-fr)
;;  initst-SFF-2   :  (init-state on-SFF in-SFF-pf)
;;  initst-SFF-3   :  (init-state in-SFF-sf taste-of-SFF-s salty-SFF)
;;  interst-SFF    :  (inter-state in-SFF-ff T-of-SFF-fr fridge-SFF)
;;  low-T-SFF      :  (inst-of low-T-SFF low-temp)
;;  on-SFF         :  (on plate-SFF food-SFF)
;;  plate-SFF      :  (inst-of plate-SFF plate)
;;  salt-SFF       :  (inst-of salt-SFF salt)
;;  salty-SFF      :  (inst-of salty-SFF salt-taste)
;;  taste-of-SFF-s :  (taste-of salt-SFF salty-SFF)
;;  taste-of-SFF-f :  (taste-of food-SFF salty-SFF)
;;  T-of-SFF-fd    :  (temperature-of food-SFF low-T-SFF)
;;  T-of-SFF-fr    :  (temperature-of fridge-SFF low-T-SFF)
;;  to-reach-SFF   :  (to-reach initst-SFF-1 goalst-SFF)


;;;;;;  End of file  AMBR/KB/EPISODIC/B_SFF.LSP
