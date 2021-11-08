;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/t_HM1.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Target sit.HM1 -- 'How to Heat Milk in a teapot?'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   ambr/kb/episodic/t_CM1.lsp, =/HM2.lsp
;;; CREATED:    28-06-98 [3.0.0]  Elaboration of SIT-Y1 from old LTM.LSP.
;;; UPDATED:    ...

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;             SITUATION  H M 1              ;;;;;;;
        ;;;;;;;       How to Heat Milk in a teapot?       ;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Target situation HM1   ;;;;;;;;;;;
;;;;
;;;; There is some milk in a teapot.  The teapot
;;;; is made of metal.
;;;;
;;;; How to achieve the goal that the milk is hot?
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + WTP -- Water in a Teapot on a hot Plate.
;;;;  + MTF -- Milk in a Teapot in a Fridge.
;;;;  * HM2 -- Milk in a teapot on a plate. What will happen?
;;;;  * CM1 -- How to cool milk in a teapot?
;;;;  * CM2 -- Milk in a tpot in a fridge. What is the goal?
;;;;
;;;; Situation HM1 is identical to sit. CM1 except that HIGH-TEMP<-->LOW-TEMP.


;;;;;;;  Situation-agent
;;

(defagent   sit-HM1    temp-instance-agent
  "How to Heat Milk in a teapot?"
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
)


;;;;;;;  Participating objects
;;
;;  milk-HM1  :  (inst-of milk)
;;  tpot-HM1  :  (inst-of teapot)
;;

(defagent   milk-HM1    temp-instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-HM1 0.1)
  :inst-of    milk
  :c-coref    ((in-HM1 . :slot1)
               (T-of-HM1 . :slot1)
               ((initst-HM1 . :slot1) 0.2) )
)

(defagent   tpot-HM1    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-HM1 0.1)
  :inst-of    teapot
  :c-coref    (((in-HM1 . :slot2) 1.0)
               ((made-of-HM1 . :slot1) 0.5)
               ((initst-HM1  . :slot2) 0.2) )
)


;;;;;;;  Initial relations
;;
;;  in-HM1      : (in milk-HM1 tpot-HM1)
;;  made-of-HM1 : (made-of tpot-HM1 mmetal-HM1)
;;

(defagent   in-HM1    temp-instance-agent
  "(in milk-HM1 tpot-HM1)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-HM1 0.1)
  :inst-of    in
  :c-coref    (initst-HM1 . :slot3)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    milk-HM1
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    tpot-HM1
)

(defagent   made-of-HM1    instance-agent
  "(made-of tpot-HM1 mmetal-HM1)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-HM1 0.1)
  :inst-of    made-of
  :c-coref    ((initst-HM1 . :slot4) 0.1)
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    tpot-HM1
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mmetal-HM1
)
(defagent  mmetal-HM1  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-HM1 0.2)
  :inst-of    material-metal
  :c-coref    (made-of-HM1 . :slot2)
)


;;;;;;;  Initial state
;;
;;  initst-HM1  -to-reach->  goalst-HM1
;;  initst-HM1  :  (init-state milk-HM1 tpot-HM1 in-HM1 made-of-HM1)
;;

(defagent   initst-HM1  temp-instance-agent
  "initst-HM1  -to-reach->  goalst-HM1"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-HM1 0.1)
  :inst-of    init-state
  :c-coref    (to-reach-HM1 . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    milk-HM1
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    tpot-HM1
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-HM1
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    made-of-HM1
)


;;;;;;;  Goal state
;;
;;  goalst-HM1  <-to-reach-  initst-HM1
;;  goalst-HM1   :  (goal-state T-of-HM1 high-T-HM1)
;;
;;  T-of-HM1     :  (temperature-of milk-HM1 high-T-HM1)
;;  to-reach-HM1 :  (to-reach initst-HM1 goalst-HM1)
;;

(defagent   T-of-HM1    temp-instance-agent
  "(temperature-of milk-HM1 high-T-HM1)"
  :type       (:instance  :relation)
  :modality   (:goal :intend-true)
  :situation  (sit-HM1 0.1)
  :inst-of    temperature-of
  :c-coref    (goalst-HM1 . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    milk-HM1
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-HM1
)
(defagent  high-T-HM1   temp-instance-agent
  :type       (:instance  :object)
  :modality   :goal
  :situation  (sit-HM1 0.1)
  :inst-of    high-temp
  :c-coref    (((T-of-HM1   . :slot2) 1.0)
               ((goalst-HM1 . :slot2) 0.2) )
)

(defagent   goalst-HM1   temp-instance-agent
  "goalst-HM1  <-to-reach-  initst-HM1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-HM1 0.1)
  :inst-of    goal-state
  :c-coref    (to-reach-HM1 . :slot2)
  :a-link     (initst-HM1 1.0)
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-HM1
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    high-T-HM1
)

(defagent   to-reach-HM1  temp-instance-agent
  "(to-reach initst-HM1 goalst-HM1)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-HM1 0.1)
  :inst-of    to-reach
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-HM1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-HM1
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-HM1
  "How to Heat Milk in a teapot?"
  :head      sit-HM1                   ; 11 agents
  :members   (sit-HM1
              milk-HM1     tpot-HM1
              in-HM1
              T-of-HM1     high-T-HM1
              made-of-HM1  mmetal-HM1
              initst-HM1   goalst-HM1  to-reach-HM1
             ))


(defun  attach-target (&optional (verbose-p T))
  "Attach SIT-HM1 to goal and input lists and prepare the model to run."
;;;; Add to *goal* and *input*, see AMBR/GOALINPT.LSP.
  (add-T-driver-tag #$sit-HM1)
  (add-to-goal/input #$T-of-HM1     1.00 :goal )
  (add-to-goal/input #$high-T-HM1   1.00 :goal )
  (add-to-goal/input #$milk-HM1     1.00 :input )
  (add-to-goal/input #$tpot-HM1     1.00 :input )
  (add-to-goal/input #$in-HM1       1.00 :input )
  (add-to-goal/input #$made-of-HM1  0.50 :input )
  (add-to-goal/input #$mmetal-HM1   0.25 :input )
;;;; Put global parameters in order, see AMBR/TOPLEVEL.LSP and AMBR/DEFS.LSP
  (enforce-AMBR-parameters)
  (make-node-constructors *number-of-node-constructors*
                          :mode :at-least)
;;;;;;;;;;;;;
;;;; For the interface only: arrange for REPORT, RETRIEVAL-INDEX, VERBOSE, etc.
  (setq *target* (find-coalition 'sit-HM1))    ; see AMBR/INTRFACE/DEFS.LSP
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
;;  sit-HM1       :  (inst-of sit-HM1 situation)
;;
;;  goalst-HM1    :  (goal-state T-of-HM1 high-T-HM1)
;;  in-HM1        :  (in milk-HM1 tpot-HM1)
;;  initst-HM1    :  (init-state milk-HM1 tpot-HM1 in-HM1 made-of-HM1)
;;  high-T-HM1    :  (inst-of high-T-HM1 high-temp)
;;  made-of-HM1   :  (made-of tpot-HM1 mmetal-HM1)
;;  mmetal-HM1    :  (inst-of mmetal-HM1 material-metal)
;;  milk-HM1      :  (inst-of milk-HM1 milk)
;;  T-of-HM1      :  (temperature-of milk-HM1 high-T-HM1)
;;  to-reach-HM1  :  (to-reach initst-HM1 goalst-HM1)
;;  tpot-HM1      :  (inst-of tpot-HM1 teapot)


;;;;;;  End of file  AMBR/KB/EPISODIC/T_HM1.LSP
