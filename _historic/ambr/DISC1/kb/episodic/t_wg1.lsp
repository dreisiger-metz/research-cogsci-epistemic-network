;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/t_WG1.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Target sit.WG1 -- 'How to heat Water in a wooden Bowl?'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    28-06-98 [3.0.0]  Elaboration of SIT-Z from old LTM.LSP.
;;; UPDATED:    ...

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;             SITUATION  W G 1              ;;;;;;;
        ;;;;;;;      How to heat Water in a Glass?        ;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Target situation WG1   ;;;;;;;;;;;
;;;;
;;;; There is some water in a glass. The glass is
;;;; made of material-glass and its color is white.
;;;;
;;;; How to achieve the goal that the water is hot?
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + WTP -- Water in a Teapot on a hot Plate.
;;;;  + IHC -- Imm.Heater in a Cup with water.
;;;;  + GP  -- Glass on a hot Plate breaks.
;;;;  * WB1 -- How to heat Water in a wooden Bowl?
;;;;  * HM1 -- How to Heat Milk in a teapot?
;;;;  * HM2 -- Milk in a teapot on a plate. What will happen?


;;;;;;;  Situation-agent
;;

(defagent   sit-WG1    temp-instance-agent
  "How to heat Water in a Glass?"
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
)


;;;;;;;  Participating objects
;;
;;  water-WG1  :  (inst-of water)
;;  glass-WG1  :  (inst-of glass)
;;

(defagent   water-WG1    temp-instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-WG1 0.1)
  :inst-of    water
  :c-coref    ((in-WG1 . :slot1)
               (T-of-WG1 . :slot1)
               ((initst-WG1 . :slot1) 0.2) )
)

(defagent   glass-WG1    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-WG1 0.1)
  :inst-of    glass
  :c-coref    (((in-WG1 . :slot2) 1.0)
               ((made-of-WG1  . :slot1) 1.0)
               ((color-of-WG1 . :slot1) 0.5)
               ((initst-WG1   . :slot2) 0.2) )
)


;;;;;;;  Initial relations
;;
;;  in-WG1       : (in water-WG1 glass-WG1)
;;  made-of-WG1  : (made-of glass-WG1 mglass-WG1)
;;  color-of-WG1 : (color-of glass-WG1 white-WG1)
;;

(defagent   in-WG1    temp-instance-agent
  "(in water-WG1 glass-WG1)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-WG1 0.1)
  :inst-of    in
  :c-coref    (initst-WG1 . :slot3)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    water-WG1
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    glass-WG1
)

(defagent   made-of-WG1    instance-agent
  "(made-of glass-WG1 mglass-WG1)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-WG1 0.1)
  :inst-of    made-of
  :c-coref    ((initst-WG1 . :slot4) 0.1)
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    glass-WG1
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mglass-WG1
)
(defagent  mglass-WG1  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-WG1 0.2)
  :inst-of    material-glass
  :c-coref    (made-of-WG1 . :slot2)
)

(defagent   color-of-WG1    instance-agent
  "(color-of glass-WG1 white-WG1)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-WG1 0.1)
  :inst-of    color-of
  :slot1
    :type       :aspect
    :inst-of    (color-of . :slot1)
    :c-coref    glass-WG1
  :slot2
    :type       :aspect
    :inst-of    (color-of . :slot2)
    :c-coref    white-WG1
)
(defagent  white-WG1  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-WG1 0.2)
  :inst-of    white
  :c-coref    (color-of-WG1 . :slot2)
)


;;;;;;;  Initial state
;;
;;  initst-WG1  -to-reach->  goalst-WG1
;;  initst-WG1  :  (init-state water-WG1 glass-WG1 in-WG1 made-of-WG1)
;;

(defagent   initst-WG1  temp-instance-agent
  "initst-WG1  -to-reach->  goalst-WG1"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-WG1 0.1)
  :inst-of    init-state
  :c-coref    (to-reach-WG1 . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    water-WG1   ; inst-of water
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    glass-WG1   ; inst-of glass
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-WG1   ; in water-WG1 glass-WG1
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    made-of-WG1   ; made-of glass-WG1 mglass-WG1
)


;;;;;;;  Goal state
;;
;;  goalst-WG1  <-to-reach-  initst-WG1
;;  goalst-WG1   :  (goal-state T-of-WG1 high-T-WG1)
;;
;;  T-of-WG1     :  (temperature-of water-WG1 high-T-WG1)
;;  to-reach-WG1 :  (to-reach initst-WG1 goalst-WG1)
;;

(defagent   T-of-WG1    temp-instance-agent
  "(temperature-of water-WG1 high-T-WG1)"
  :type       (:instance  :relation)
  :modality   (:goal :intend-true)
  :situation  (sit-WG1 0.1)
  :inst-of    temperature-of
  :c-coref    (goalst-WG1 . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    water-WG1
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-WG1
)
(defagent  high-T-WG1   temp-instance-agent
  :type       (:instance  :object)
  :modality   :goal
  :situation  (sit-WG1 0.1)
  :inst-of    high-temp
  :c-coref    (((T-of-WG1   . :slot2) 1.0)
               ((goalst-WG1 . :slot2) 0.2) )
)

(defagent   goalst-WG1   temp-instance-agent
  "goalst-WG1  <-to-reach-  initst-WG1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-WG1 0.1)
  :inst-of    goal-state
  :c-coref    (to-reach-WG1 . :slot2)
  :a-link     (initst-WG1 1.0)
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-WG1
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    high-T-WG1
)

(defagent   to-reach-WG1  temp-instance-agent
  "(to-reach initst-WG1 goalst-WG1)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-WG1 0.1)
  :inst-of    to-reach
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-WG1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-WG1
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-WG1
  "How to heat Water in a Glass?"
  :head      sit-WG1                   ; 13 agents
  :members   (sit-WG1
              water-WG1    glass-WG1
              in-WG1
              T-of-WG1     high-T-WG1
              made-of-WG1  mglass-WG1
              color-of-WG1 white-WG1
              initst-WG1   goalst-WG1  to-reach-WG1
             ))


(defun  attach-target (&optional (verbose-p T))
  "Attach SIT-WG1 to goal and input lists and prepare the model to run."
;;;; Add to *goal* and *input*, see AMBR/GOALINPT.LSP.
  (add-T-driver-tag #$sit-WG1)
  (add-to-goal/input #$T-of-WG1     1.00 :goal )
  (add-to-goal/input #$high-T-WG1   1.00 :goal )
  (add-to-goal/input #$water-WG1    1.00 :input )
  (add-to-goal/input #$glass-WG1    1.00 :input )
  (add-to-goal/input #$in-WG1       1.00 :input )
  (add-to-goal/input #$made-of-WG1  0.50 :input )
  (add-to-goal/input #$mglass-WG1   1.00 :input )
  (add-to-goal/input #$color-of-WG1 0.50 :input )
  (add-to-goal/input #$white-WG1    0.25 :input )
;;;; Put global parameters in order, see AMBR/TOPLEVEL.LSP and AMBR/DEFS.LSP
  (enforce-AMBR-parameters)
  (make-node-constructors *number-of-node-constructors*
                          :mode :at-least)
;;;;;;;;;;;;;
;;;; For the interface only: arrange for REPORT, RETRIEVAL-INDEX, VERBOSE, etc.
  (setq *target* (find-coalition 'sit-WG1))    ; see AMBR/INTRFACE/DEFS.LSP
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
;;  sit-WG1       :  (inst-of sit-WG1 situation)
;;
;;  color-of-WG1  :  (color-of glass-WG1 white-WG1)
;;  glass-WG1     :  (inst-of glass-WG1 glass)
;;  goalst-WG1    :  (goal-state T-of-WG1 high-T-WG1)
;;  in-WG1        :  (in water-WG1 glass-WG1)
;;  initst-WG1    :  (init-state water-WG1 glass-WG1 in-WG1 made-of-WG1)
;;  high-T-WG1    :  (inst-of high-T-WG1 high-temp)
;;  made-of-WG1   :  (made-of glass-WG1 mglass-WG1)
;;  mglass-WG1    :  (inst-of mglass-WG1 material-glass)
;;  T-of-WG1      :  (temperature-of water-WG1 high-T-WG1)
;;  to-reach-WG1  :  (to-reach initst-WG1 goalst-WG1)
;;  water-WG1     :  (inst-of water-WG1 water)
;;  white-WG1     :  (inst-of white-WG1 white)


;;;;;;  End of file  AMBR/KB/EPISODIC/T_WG1.LSP
