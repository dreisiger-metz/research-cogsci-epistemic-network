;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/b_STC.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Base situation STC -- 'Sugar in Tea in a Cup.'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    27-06-98 [3.0.0]
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;          SITUATION  S T C           ;;;;;;;;
         ;;;;;;;;       Sugar in Tea in a Cup.        ;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Base situation STC   ;;;;;;;;;;;
;;;;
;;;; There is some tea in a cup.  There is some sugar
;;;; in the tea. The sugar is sweet.  The cup is on a
;;;; saucer.
;;;;
;;;; The goal is that the tea is sweet.
;;;;
;;;; The result is that the tea becomes sweet because
;;;; of the sugar in it.
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + SFF -- Salt in Food on a plate in a Fridge.
;;;;  + IHC -- Imm.Heater in a Cup with water.
;;;;  + ...
;;;;  * ...


;;;;;;;  Situation-agent
;;

(defagent   sit-STC    instance-agent
  "Sugar in Tea in a Cup."
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
  :a-link     ((sugar-STC 1.0)
               (sweet-STC 0.2)
               (taste-of-STC-t 0.2) )
)


;;;;;;;  Participating objects
;;
;;  sugar-STC   :  (inst-of sugar)
;;  tea-STC     :  (inst-of tea)
;;  cup-STC     :  (inst-of cup)
;;  saucer-STC  :  (inst-of saucer)
;;

(defagent   sugar-STC    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-STC 0.5)
  :inst-of    sugar
  :c-coref    (((taste-of-STC-s . :slot1) 1.0)
               ((initst-STC . :slot4) 0.2)
               ((in-STC-st  . :slot1) 0.2) )
  :a-link     (sweet-STC 0.5)
)

(defagent   tea-STC    instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-STC 0.2)
  :inst-of    tea
  :c-coref    (((taste-of-STC-t . :slot1) 0.5)
               ((in-STC-st  . :slot2) 0.3)
               ((in-STC-tc  . :slot1) 0.2)
               ((goalst-STC . :slot3) 0.1) )
  :a-link     ((sugar-STC  0.5)
               (initst-STC 0.1) )
)

(defagent   cup-STC    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-STC 0.2)
  :inst-of    cup
  :c-coref    (((in-STC-tc   . :slot2) 0.5)
               ((on-STC      . :slot2) 0.3) )
  :a-link     ((initst-STC  0.1)
               (saucer-STC  0.2) )
)

(defagent   saucer-STC    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-STC 0.2)
  :inst-of    saucer
  :c-coref    (on-STC . :slot1)
  :a-link     (cup-STC 0.5)
)


;;;;;;;  Initial relations
;;
;;  in-STC-st      : (in sugar-STC tea-STC)
;;  in-STC-tc      : (in tea-STC cup-STC)
;;  on-STC         : (on saucer-STC cup-STC)
;;  taste-of-STC-s : (taste-of sugar-STC sweet-STC)
;;

(defagent   in-STC-st    instance-agent
  "(in sugar-STC tea-STC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-STC 0.2)
  :inst-of    in
  :c-coref    ((initst-STC . :slot3) 0.2)
  :a-link     ((taste-of-STC-t 0.3)
               (in-STC-tc      0.2) )
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    sugar-STC
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    tea-STC
)

(defagent   in-STC-tc  instance-agent
  "(in tea-STC cup-STC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-STC 0.2)
  :inst-of    in
  :a-link     ((in-STC-st 0.3)
               (on-STC    0.1) )
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    tea-STC
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    cup-STC
)

(defagent   on-STC    instance-agent
  "(on saucer-STC cup-STC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-STC 0.2)
  :inst-of    on
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    saucer-STC
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    cup-STC
)

(defagent   taste-of-STC-s    instance-agent
  "(taste-of sugar-STC sweet-STC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-STC 0.2)
  :inst-of    taste-of
  :c-coref    ((initst-STC . :slot1) 0.2)
  :a-link     ((taste-of-STC-t 0.2)
               (cause-STC      0.2) )
  :slot1
    :type       :aspect
    :inst-of    (taste-of . :slot1)
    :c-coref    sugar-STC
  :slot2
    :type       :aspect
    :inst-of    (taste-of . :slot2)
    :c-coref    sweet-STC
)
(defagent  sweet-STC   instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal :result)
  :situation  (sit-STC 0.3)
  :inst-of    sweet-taste
  :c-coref    (((taste-of-STC-s . :slot2) 0.7)
               ((taste-of-STC-t . :slot2) 0.5)
               ((initst-STC . :slot2) 0.1)
               ((goalst-STC . :slot2) 0.1) )
  :a-link     (sugar-STC 0.7)
)


;;;;;;;  Initial state
;;
;;  initst-STC  -to-reach->  goalst-STC
;;  initst-STC  -follows->   endst-STC
;;  initst-STC  :  (init-state taste-of-STC-s sweet-STC in-STC-st sugar-STC)
;;

(defagent   initst-STC  instance-agent
  "initst-STC  -to-reach->  goalst-STC"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-STC 0.2)
  :inst-of    init-state
  :c-coref    (((to-reach-STC . :slot1) 0.4)
               ((follows-STC  . :slot1) 0.3)
               ((cause-STC    . :slot1) 0.2) )
  :a-link     ((goalst-STC 0.3)
               (endst-STC  0.3)
               (tea-STC    0.2)
               (cup-STC    0.2)
               (saucer-STC 0.1) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    taste-of-STC-s
  :slot2
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    sweet-STC
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-STC-st
  :slot4
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    sugar-STC
)


;;;;;;;  Goal state
;;
;;  goalst-STC  <-to-reach-  initst-STC
;;  goalst-STC     :  (goal-state taste-of-STC-t sweet-STC tea-STC)
;;
;;  taste-of-STC-t :  (taste-of tea-STC sweet-STC)
;;  to-reach-STC   :  (to-reach initst-STC goalst-STC)
;;

(defagent   taste-of-STC-t    instance-agent
  "(taste-of tea-STC sweet-STC)"
  :type       (:instance  :relation)
  :modality   (:GOAL   :intend-true
               :RESULT :true )
  :situation  (sit-STC 0.3)
  :inst-of    taste-of
  :c-coref    (((cause-STC  . :slot2) 0.2)
               ((goalst-STC . :slot1) 0.2)
               ((endst-STC  . :slot1) 0.2) )
  :a-link     ((taste-of-STC-s 0.5)
               (sugar-STC 0.3) )
  :slot1
    :type       :aspect
    :inst-of    (taste-of . :slot1)
    :c-coref    tea-STC
  :slot2
    :type       :aspect
    :inst-of    (taste-of . :slot2)
    :c-coref    sweet-STC
)

(defagent   goalst-STC    instance-agent
  "goalst-STC  <-to-reach-  initst-STC"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-STC 0.2)
  :inst-of    goal-state
  :c-coref    (to-reach-STC . :slot2)
  :a-link     ((initst-STC 0.5)
               (endst-STC  0.2) )
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    taste-of-STC-t
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    sweet-STC
  :slot3
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    tea-STC
)

(defagent   to-reach-STC    instance-agent
  "(to-reach initst-STC goalst-STC)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-STC 0.2)
  :inst-of    to-reach
  :a-link     ((follows-STC 0.3)
               (cause-STC   0.2) )
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-STC
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-STC
)


;;;;;;;  End state
;;
;;  endst-STC  <-follows-  initst-STC
;;  endst-STC   :  (end-state taste-of-STC-t)
;;
;;  follows-STC :  (follows initst-STC endst-STC)
;;  cause-STC   :  (cause initst-STC taste-of-STC-t)
;;

(defagent   endst-STC    instance-agent
  "endst-STC  <-follows-  initst-STC"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-STC 0.2)
  :inst-of    end-state
  :c-coref    (follows-STC . :slot2)
  :a-link     ((initst-STC 0.5)
               (goalst-STC 0.2) )
  :slot1
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    taste-of-STC-t
)

(defagent   follows-STC  instance-agent
  "(follows initst-STC endst-STC)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-STC 0.2)
  :inst-of    follows
  :a-link     ((to-reach-STC 0.3)
               (cause-STC    0.2) )
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-STC
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-STC
)

(defagent   cause-STC    instance-agent
  "(cause initst-STC taste-of-STC-t)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-STC 0.2)
  :inst-of    cause
  :a-link     ((endst-STC    0.5)
               (follows-STC  0.3)
               (to-reach-STC 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    initst-STC
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    taste-of-STC-t
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-STC
  "Sugar in Tea in a Cup."
  :head      sit-STC                          ; 17 agents
  :members   (sit-STC
              tea-STC         sugar-STC
              cup-STC         saucer-STC
              in-STC-st       in-STC-tc       on-STC
              taste-of-STC-s  taste-of-STC-t  sweet-STC
              initst-STC      goalst-STC      endst-STC
              to-reach-STC    follows-STC     cause-STC
             ))

(GENKB-template
  :herald  "Base sit.STC -- Sugar in Tea in a Cup, ver.3.0.0."
  :templates '(
    (tea          (:instance (tea-STC         1))
                  (:a-link   (taste-of-STC-t 0.1)) )
    (cup          (:instance (cup-STC         1)) )
    (saucer       (:instance (saucer-STC      1)) )
    (sugar        (:instance (sugar-STC       5))
                  (:a-link   (taste-of-STC-s 0.2)) )
    (taste-of     (:instance (taste-of-STC-t  1) (taste-of-STC-s 3))
                  (:a-link   (sweet-STC      0.1)) )
    (sweet-taste  (:instance (sweet-STC       5))
                  (:a-link   (sugar-STC      0.1)) )
    (in           (:instance (in-STC-st       1) (in-STC-tc 1)) )
    (on           (:instance (on-STC          1)) )
))


;;;;;;;;  Propositional representaion
;;
;;  sit-STC        :  (inst-of sit-STC situation)
;;
;;  cause-STC      :  (cause initst-STC taste-of-STC-t)
;;  cup-STC        :  (inst-of cup-STC cup)
;;  endst-STC      :  (end-state taste-of-STC-t)
;;  follows-STC    :  (follows initst-STC endst-STC)
;;  goalst-STC     :  (goal-state taste-of-STC-t sweet-STC tea-STC)
;;  in-STC-st      :  (in sugar-STC tea-STC)
;;  in-STC-tc      :  (in tea-STC cup-STC)
;;  initst-STC     :  (init-state taste-of-STC-s sweet-STC in-STC-st sugar-STC)
;;  on-STC         :  (on saucer-STC cup-STC)
;;  saucer-STC     :  (inst-of saucer-STC saucer)
;;  sugar-STC      :  (inst-of sugar-STC sugar)
;;  sweet-STC      :  (inst-of sweet-STC sweet-taste)
;;  taste-of-STC-s :  (taste-of sugar-STC sweet-STC)
;;  taste-of-STC-t :  (taste-of tea-STC sweet-STC)
;;  tea-STC        :  (inst-of tea-STC tea)
;;  to-reach-STC   :  (to-reach initst-STC goalst-STC)


;;;;;;  End of file  AMBR/KB/EPISODIC/B_STC.LSP
