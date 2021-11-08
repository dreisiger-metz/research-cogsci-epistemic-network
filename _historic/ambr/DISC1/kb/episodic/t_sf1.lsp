;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/t_SF1.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Target sit.SF1 -- 'How to Heat Milk in a teapot?'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   ambr/kb/episodic/t_SF2.lsp
;;; CREATED:    28-06-98 [3.0.0]
;;; UPDATED:    ...

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;             SITUATION  S F 1             ;;;;;;;
        ;;;;;;;       How to Salt Food on a plate?       ;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Target situation SF1   ;;;;;;;;;;;
;;;;
;;;; There is some food on a plate. The plate is
;;;; made of china.
;;;;
;;;; How to achieve the goal that the food is salty?
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + SFF -- Salt in Food on a plate in a Fridge.
;;;;  + STC -- Sugar in Tea in a Cup.
;;;;  * SF2 -- Salt in Food on a plate. What will happen?


;;;;;;;  Situation-agent
;;

(defagent   sit-SF1    temp-instance-agent
  "How to Salt Food on a plate?"
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
)


;;;;;;;  Participating objects
;;
;;  food-SF1  :  (inst-of food)
;;  plate-SF1  :  (inst-of plate)
;;

(defagent   food-SF1    temp-instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-SF1 0.1)
  :inst-of    food
  :c-coref    ((on-SF1 . :slot2)
               (taste-of-SF1 . :slot1)
               ((initst-SF1 . :slot1) 0.2) )
)

(defagent   plate-SF1    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-SF1 0.1)
  :inst-of    plate
  :c-coref    (((on-SF1 . :slot1) 1.0)
               ((made-of-SF1 . :slot1) 1.0)
               ((initst-SF1  . :slot2) 0.2) )
  :a-link     (mchina-SF1 0.5)
)


;;;;;;;  Initial relations
;;
;;  on-SF1      : (on plate-SF1 food-SF1)
;;  made-of-SF1 : (made-of plate-SF1 mchina-SF1)
;;

(defagent   on-SF1    temp-instance-agent
  "(on plate-SF1 food-SF1)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-SF1 0.1)
  :inst-of    on
  :c-coref    (initst-SF1 . :slot3)
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    plate-SF1
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    food-SF1
)

(defagent   made-of-SF1    instance-agent
  "(made-of plate-SF1 mchina-SF1)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-SF1 0.1)
  :inst-of    made-of
  :c-coref    ((initst-SF1 . :slot4) 0.1)
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    plate-SF1
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mchina-SF1
)
(defagent  mchina-SF1  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-SF1 0.2)
  :inst-of    material-china
  :c-coref    (made-of-SF1 . :slot2)
)


;;;;;;;  Initial state
;;
;;  initst-SF1  -to-reach->  goalst-SF1
;;  initst-SF1  :  (init-state food-SF1 plate-SF1 on-SF1 made-of-SF1)
;;

(defagent   initst-SF1  temp-instance-agent
  "initst-SF1  -to-reach->  goalst-SF1"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-SF1 0.1)
  :inst-of    init-state
  :c-coref    (to-reach-SF1 . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    food-SF1
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    plate-SF1
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-SF1
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    made-of-SF1
)


;;;;;;;  Goal state
;;
;;  goalst-SF1  <-to-reach-  initst-SF1
;;  goalst-SF1   :  (goal-state taste-of-SF1 salty-SF1)
;;
;;  taste-of-SF1 :  (taste-of food-SF1 salty-SF1)
;;  to-reach-SF1 :  (to-reach initst-SF1 goalst-SF1)
;;

(defagent   taste-of-SF1    temp-instance-agent
  "(taste-of food-SF1 salty-SF1)"
  :type       (:instance  :relation)
  :modality   (:goal :intend-true)
  :situation  (sit-SF1 0.1)
  :inst-of    taste-of
  :c-coref    (goalst-SF1 . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (taste-of . :slot1)
    :c-coref    food-SF1
  :slot2
    :type       :aspect
    :inst-of    (taste-of . :slot2)
    :c-coref    salty-SF1
)
(defagent  salty-SF1   temp-instance-agent
  :type       (:instance  :object)
  :modality   :goal
  :situation  (sit-SF1 0.1)
  :inst-of    salt-taste
  :c-coref    (((taste-of-SF1 . :slot2) 1.0)
               ((goalst-SF1   . :slot2) 0.2) )
)

(defagent   goalst-SF1   temp-instance-agent
  "goalst-SF1  <-to-reach-  initst-SF1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-SF1 0.1)
  :inst-of    goal-state
  :c-coref    (to-reach-SF1 . :slot2)
  :a-link     (initst-SF1 1.0)
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    taste-of-SF1
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    salty-SF1
)

(defagent   to-reach-SF1  temp-instance-agent
  "(to-reach initst-SF1 goalst-SF1)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-SF1 0.1)
  :inst-of    to-reach
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-SF1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-SF1
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-SF1
  "How to Salt Food on a plate?"
  :head      sit-SF1                   ; 11 agents
  :members   (sit-SF1
              food-SF1     plate-SF1
              on-SF1
              taste-of-SF1 salty-SF1
              made-of-SF1  mchina-SF1
              initst-SF1   goalst-SF1  to-reach-SF1
             ))


(defun  attach-target (&optional (verbose-p T))
  "Attach SIT-SF1 to goal and input lists and prepare the model to run."
;;;; Add to *goal* and *input*, see AMBR/GOALINPT.LSP.
  (add-T-driver-tag #$sit-SF1)
  (add-to-goal/input #$taste-of-SF1  1.00 :goal )
  (add-to-goal/input #$salty-SF1     1.00 :goal )
  (add-to-goal/input #$food-SF1      1.00 :input )
  (add-to-goal/input #$plate-SF1     1.00 :input )
  (add-to-goal/input #$on-SF1        1.00 :input )
  (add-to-goal/input #$made-of-SF1   1.00 :input )
  (add-to-goal/input #$mchina-SF1    1.00 :input )
;;;; Put global parameters in order, see AMBR/TOPLEVEL.LSP and AMBR/DEFS.LSP
  (enforce-AMBR-parameters)
  (make-node-constructors *number-of-node-constructors*
                          :mode :at-least)
;;;;;;;;;;;;;
;;;; For the interface only: arrange for REPORT, RETRIEVAL-INDEX, VERBOSE, etc.
  (setq *target* (find-coalition 'sit-SF1))    ; see AMBR/INTRFACE/DEFS.LSP
  (setq *total-RI-coalitions*                  ; see DUAL/INTRFACE/COALITN.LSP
        (remove *target* *all-coalitions*))
;;;; Notify user
  (when verbose-p
    (format t "~%~%;;; Target is ~S -- ~A"
              *target*  (coalition-comment *target*))
    (format t "~%;;; *TIME* = ~,2F, ~D NCs, ~D agents on *goal*, ~D on *input*."
           *time* (length *node-constructors*)
           (WM-size *goal*) (WM-size *input*)) )
) ; end of ATTACH-TARGET



;;;;;;;;  Propositional representaion
;;
;;  sit-SF1       :  (inst-of sit-SF1 situation)
;;
;;  food-SF1      :  (inst-of food-SF1 food)
;;  goalst-SF1    :  (goal-state taste-of-SF1 salty-SF1)
;;  initst-SF1    :  (init-state food-SF1 plate-SF1 on-SF1 made-of-SF1)
;;  made-of-SF1   :  (made-of plate-SF1 mchina-SF1)
;;  mchina-SF1    :  (inst-of mchina-SF1 material-china)
;;  on-SF1        :  (on plate-SF1 food-SF1)
;;  plate-SF1     :  (inst-of plate-SF1 plate)
;;  salty-SF1     :  (inst-of salty-SF1 salt-taste)
;;  taste-of-SF1  :  (taste-of food-SF1 salty-SF1)
;;  to-reach-SF1  :  (to-reach initst-SF1 goalst-SF1)


;;;;;;  End of file  AMBR/KB/EPISODIC/T_SF1.LSP
