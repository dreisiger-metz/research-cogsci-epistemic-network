;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/t_CM1.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Target sit.CM1 -- 'How to Cool Milk in a teapot?'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   ambr/kb/episodic/t_HM1.lsp, =/t_CM2.lsp
;;; CREATED:    21-06-98 [3.0.0]  Elaboration of SIT-U1 from old LTM.LSP.
;;; UPDATED:    ...

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;             SITUATION  C M 1              ;;;;;;;
        ;;;;;;;       How to Cool Milk in a teapot?       ;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Target situation CM1   ;;;;;;;;;;;
;;;;
;;;; There is some milk in a teapot.  The teapot
;;;; is made of metal.
;;;;
;;;; How to achieve the goal that the milk is cold?
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + WTP -- Water in a Teapot on a hot Plate.
;;;;  + MTF -- Milk in a Teapot in a Fridge.
;;;;  * CM2 -- Milk in a tpot in a fridge. What is the goal?
;;;;  * HM1 -- How to Heat Milk in a teapot?
;;;;  * HM2 -- Milk in a teapot on a plate. What will happen?
;;;;
;;;; Situation CM1 is identical to sit. HM1 except that LOW-TEMP<-->HIGH-TEMP.


;;;;;;;  Situation-agent
;;

(defagent   sit-CM1    temp-instance-agent
  "How to Cool Milk in a teapot?"
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
)


;;;;;;;  Participating objects
;;
;;  milk-CM1  :  (inst-of milk)
;;  tpot-CM1  :  (inst-of teapot)
;;

(defagent   milk-CM1    temp-instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-CM1 0.1)
  :inst-of    milk
  :c-coref    ((in-CM1 . :slot1)
               (T-of-CM1 . :slot1)
               ((initst-CM1 . :slot1) 0.2) )
)

(defagent   tpot-CM1    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-CM1 0.1)
  :inst-of    teapot
  :c-coref    (((in-CM1 . :slot2) 1.0)
               ((made-of-CM1 . :slot1) 0.5)
               ((initst-CM1  . :slot2) 0.2) )
)


;;;;;;;  Initial relations
;;
;;  in-CM1      : (in milk-CM1 tpot-CM1)
;;  made-of-CM1 : (made-of tpot-CM1 mmetal-CM1)
;;

(defagent   in-CM1    temp-instance-agent
  "(in milk-CM1 tpot-CM1)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-CM1 0.1)
  :inst-of    in
  :c-coref    (initst-CM1 . :slot3)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    milk-CM1
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    tpot-CM1
)

(defagent   made-of-CM1    instance-agent
  "(made-of tpot-CM1 mmetal-CM1)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-CM1 0.1)
  :inst-of    made-of
  :c-coref    ((initst-CM1 . :slot4) 0.1)
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    tpot-CM1
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mmetal-CM1
)
(defagent  mmetal-CM1  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-CM1 0.2)
  :inst-of    material-metal
  :c-coref    (made-of-CM1 . :slot2)
)


;;;;;;;  Initial state
;;
;;  initst-CM1  -to-reach->  goalst-CM1
;;  initst-CM1  :  (init-state milk-CM1 tpot-CM1 in-CM1 made-of-CM1)
;;

(defagent   initst-CM1  temp-instance-agent
  "initst-CM1  -to-reach->  goalst-CM1"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-CM1 0.1)
  :inst-of    init-state
  :c-coref    (to-reach-CM1 . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    milk-CM1
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    tpot-CM1
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-CM1
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    made-of-CM1
)


;;;;;;;  Goal state
;;
;;  goalst-CM1  <-to-reach-  initst-CM1
;;  goalst-CM1   :  (goal-state T-of-CM1 low-T-CM1)
;;
;;  T-of-CM1     :  (temperature-of milk-CM1 low-T-CM1)
;;  to-reach-CM1 :  (to-reach initst-CM1 goalst-CM1)
;;

(defagent   T-of-CM1    temp-instance-agent
  "(temperature-of milk-CM1 low-T-CM1)"
  :type       (:instance  :relation)
  :modality   (:goal :intend-true)
  :situation  (sit-CM1 0.1)
  :inst-of    temperature-of
  :c-coref    (goalst-CM1 . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    milk-CM1
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    low-T-CM1
)
(defagent  low-T-CM1   temp-instance-agent
  :type       (:instance  :object)
  :modality   :goal
  :situation  (sit-CM1 0.1)
  :inst-of    low-temp
  :c-coref    (((T-of-CM1   . :slot2) 1.0)
               ((goalst-CM1 . :slot2) 0.2) )
)

(defagent   goalst-CM1   temp-instance-agent
  "goalst-CM1  <-to-reach-  initst-CM1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-CM1 0.1)
  :inst-of    goal-state
  :c-coref    (to-reach-CM1 . :slot2)
  :a-link     (initst-CM1 1.0)
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-CM1
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    low-T-CM1
)

(defagent   to-reach-CM1  temp-instance-agent
  "(to-reach initst-CM1 goalst-CM1)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-CM1 0.1)
  :inst-of    to-reach
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-CM1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-CM1
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-CM1
  "How to Cool Milk in a teapot?"
  :head      sit-CM1                   ; 11 agents
  :members   (sit-CM1
              milk-CM1     tpot-CM1
              in-CM1
              T-of-CM1     low-T-CM1
              made-of-CM1  mmetal-CM1
              initst-CM1   goalst-CM1  to-reach-CM1
             ))


(defun  attach-target (&optional (verbose-p T))
  "Attach SIT-CM1 to goal and input lists and prepare the model to run."
;;;; Add to *goal* and *input*, see AMBR/GOALINPT.LSP.
  (add-T-driver-tag #$sit-CM1)
  (add-to-goal/input #$T-of-CM1     1.00 :goal )
  (add-to-goal/input #$low-T-CM1    1.00 :goal )
  (add-to-goal/input #$milk-CM1     1.00 :input )
  (add-to-goal/input #$tpot-CM1     1.00 :input )
  (add-to-goal/input #$in-CM1       1.00 :input )
  (add-to-goal/input #$made-of-CM1  0.50 :input )
  (add-to-goal/input #$mmetal-CM1   0.25 :input )
;;;; Put global parameters in order, see AMBR/TOPLEVEL.LSP and AMBR/DEFS.LSP
  (enforce-AMBR-parameters)
  (make-node-constructors *number-of-node-constructors*
                          :mode :at-least)
;;;;;;;;;;;;;
;;;; For the interface only: arrange for REPORT, RETRIEVAL-INDEX, VERBOSE, etc.
  (setq *target* (find-coalition 'sit-CM1))    ; see AMBR/INTRFACE/DEFS.LSP
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
;;  sit-CM1       :  (inst-of sit-CM1 situation)
;;
;;  goalst-CM1    :  (goal-state T-of-CM1 low-T-CM1)
;;  in-CM1        :  (in milk-CM1 tpot-CM1)
;;  initst-CM1    :  (init-state milk-CM1 tpot-CM1 in-CM1 made-of-CM1)
;;  low-T-CM1     :  (inst-of low-T-CM1 low-temp)
;;  made-of-CM1   :  (made-of tpot-CM1 mmetal-CM1)
;;  milk-CM1      :  (inst-of milk-CM1 milk)
;;  mmetal-CM1    :  (inst-of mmetal-CM1 material-metal)
;;  T-of-CM1      :  (temperature-of milk-CM1 low-T-CM1)
;;  to-reach-CM1  :  (to-reach initst-CM1 goalst-CM1)
;;  tpot-CM1      :  (inst-of tpot-CM1 teapot)


;;;;;;  End of file  AMBR/KB/EPISODIC/T_CM1.LSP
