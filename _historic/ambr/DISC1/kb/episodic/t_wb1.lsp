;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/t_WB1.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Target sit.WB1 -- 'How to heat Water in a wooden Bowl?'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    28-06-98 [3.0.0]  Elaboration of SIT-X from old LTM.LSP.
;;; UPDATED:    ...

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;             SITUATION  W B 1              ;;;;;;;
        ;;;;;;;    How to heat Water in a wooden Bowl?    ;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Target situation WB1   ;;;;;;;;;;;
;;;;
;;;; There is some water in a bowl. The bowl is
;;;; made of wood.
;;;;
;;;; How to achieve the goal that the water is hot?
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + WTP -- Water in a Teapot on a hot Plate.
;;;;  + IHC -- Imm.Heater in a Cup with water.
;;;;  + BF  -- Bowl on a Fire burns out.
;;;;  * WG1 -- How to heat Water in a Glass?
;;;;  * HM1 -- How to Heat Milk in a teapot?
;;;;  * HM2 -- Milk in a teapot on a plate. What will happen?


;;;;;;;  Situation-agent
;;

(defagent   sit-WB1    temp-instance-agent
  "How to heat Water in a wooden Bowl?"
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
)


;;;;;;;  Participating objects
;;
;;  water-WB1  :  (inst-of water)
;;  bowl-WB1  :  (inst-of bowl)
;;

(defagent   water-WB1    temp-instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-WB1 0.1)
  :inst-of    water
  :c-coref    ((in-WB1 . :slot1)
               (T-of-WB1 . :slot1)
               ((initst-WB1 . :slot1) 0.2) )
)

(defagent   bowl-WB1    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-WB1 0.1)
  :inst-of    bowl
  :c-coref    (((in-WB1 . :slot2) 1.0)
               ((made-of-WB1 . :slot1) 1.0)
               ((initst-WB1  . :slot2) 0.2) )
)


;;;;;;;  Initial relations
;;
;;  in-WB1      : (in water-WB1 bowl-WB1)
;;  made-of-WB1 : (made-of bowl-WB1 mwood-WB1)
;;

(defagent   in-WB1    temp-instance-agent
  "(in water-WB1 bowl-WB1)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-WB1 0.1)
  :inst-of    in
  :c-coref    (initst-WB1 . :slot3)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    water-WB1
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    bowl-WB1
)

(defagent   made-of-WB1    instance-agent
  "(made-of bowl-WB1 mwood-WB1)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-WB1 0.1)
  :inst-of    made-of
  :c-coref    ((initst-WB1 . :slot4) 0.1)
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    bowl-WB1
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mwood-WB1
)
(defagent  mwood-WB1  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-WB1 0.2)
  :inst-of    material-wood
  :c-coref    (made-of-WB1 . :slot2)
)


;;;;;;;  Initial state
;;
;;  initst-WB1  -to-reach->  goalst-WB1
;;  initst-WB1  :  (init-state water-WB1 bowl-WB1 in-WB1 made-of-WB1)
;;

(defagent   initst-WB1  temp-instance-agent
  "initst-WB1  -to-reach->  goalst-WB1"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-WB1 0.1)
  :inst-of    init-state
  :c-coref    (to-reach-WB1 . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    water-WB1
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    bowl-WB1
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-WB1
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    made-of-WB1
)


;;;;;;;  Goal state
;;
;;  goalst-WB1  <-to-reach-  initst-WB1
;;  goalst-WB1   :  (goal-state T-of-WB1 high-T-WB1)
;;
;;  T-of-WB1     :  (temperature-of water-WB1 high-T-WB1)
;;  to-reach-WB1 :  (to-reach initst-WB1 goalst-WB1)
;;

(defagent   T-of-WB1    temp-instance-agent
  "(temperature-of water-WB1 high-T-WB1)"
  :type       (:instance  :relation)
  :modality   (:goal :intend-true)
  :situation  (sit-WB1 0.1)
  :inst-of    temperature-of
  :c-coref    (goalst-WB1 . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    water-WB1
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-WB1
)
(defagent  high-T-WB1   temp-instance-agent
  :type       (:instance  :object)
  :modality   :goal
  :situation  (sit-WB1 0.1)
  :inst-of    high-temp
  :c-coref    (((T-of-WB1   . :slot2) 1.0)
               ((goalst-WB1 . :slot2) 0.2) )
)

(defagent   goalst-WB1   temp-instance-agent
  "goalst-WB1  <-to-reach-  initst-WB1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-WB1 0.1)
  :inst-of    goal-state
  :c-coref    (to-reach-WB1 . :slot2)
  :a-link     (initst-WB1 1.0)
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-WB1
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    high-T-WB1
)

(defagent   to-reach-WB1  temp-instance-agent
  "(to-reach initst-WB1 goalst-WB1)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-WB1 0.1)
  :inst-of    to-reach
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-WB1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-WB1
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-WB1
  "How to heat Water in a wooden Bowl?"
  :head      sit-WB1                   ; 11 agents
  :members   (sit-WB1
              water-WB1    bowl-WB1
              in-WB1
              T-of-WB1     high-T-WB1
              made-of-WB1  mwood-WB1
              initst-WB1   goalst-WB1  to-reach-WB1
             ))


(defun  attach-target (&optional (verbose-p T))
  "Attach SIT-WB1 to goal and input lists and prepare the model to run."
;;;; Add to *goal* and *input*, see AMBR/GOALINPT.LSP.
  (add-T-driver-tag #$sit-WB1)
  (add-to-goal/input #$T-of-WB1     1.00 :goal )
  (add-to-goal/input #$high-T-WB1   1.00 :goal )
  (add-to-goal/input #$water-WB1    1.00 :input )
  (add-to-goal/input #$bowl-WB1     1.00 :input )
  (add-to-goal/input #$in-WB1       1.00 :input )
  (add-to-goal/input #$made-of-WB1  0.50 :input )
  (add-to-goal/input #$mwood-WB1    1.00 :input )
;;;; Put global parameters in order, see AMBR/TOPLEVEL.LSP and AMBR/DEFS.LSP
  (enforce-AMBR-parameters)
  (make-node-constructors *number-of-node-constructors*
                          :mode :at-least)
;;;;;;;;;;;;;
;;;; For the interface only: arrange for REPORT, RETRIEVAL-INDEX, VERBOSE, etc.
  (setq *target* (find-coalition 'sit-WB1))    ; see AMBR/INTRFACE/DEFS.LSP
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
;;  sit-WB1       :  (inst-of sit-WB1 situation)
;;
;;  bowl-WB1      :  (inst-of bowl-WB1 bowl)
;;  goalst-WB1    :  (goal-state T-of-WB1 high-T-WB1)
;;  in-WB1        :  (in water-WB1 bowl-WB1)
;;  initst-WB1    :  (init-state water-WB1 bowl-WB1 in-WB1 made-of-WB1)
;;  high-T-WB1    :  (inst-of high-T-WB1 high-temp)
;;  made-of-WB1   :  (made-of bowl-WB1 mwood-WB1)
;;  mwood-WB1     :  (inst-of mwood-WB1 material-wood)
;;  T-of-WB1      :  (temperature-of water-WB1 high-T-WB1)
;;  to-reach-WB1  :  (to-reach initst-WB1 goalst-WB1)
;;  water-WB1     :  (inst-of water-WB1 water)


;;;;;;  End of file  AMBR/KB/EPISODIC/T_WB1.LSP
