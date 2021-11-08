;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/t_CM2.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Target sit.CM2 -- 'Milk in a teapot on a plate. What'll happen?'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   ambr/kb/episodic/t_CM1.lsp, =/t_HM2.lsp
;;; CREATED:    28-06-98 [3.0.0]  Modification of SIT-U2 from old LTM.LSP.
;;; UPDATED:    ...

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;                 SITUATION  C M 2                   ;;;;
        ;;;;   Milk in a teapot in a fridge. What is the goal?  ;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Target situation CM2   ;;;;;;;;;;;
;;;;
;;;; There is some milk in a teapot.  The teapot
;;;; is in a fridge.  The fridge is cold.  The color
;;;; of the teapot is black.
;;;;
;;;; What is the goal of this arrangement?
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + WTP -- Water in a Teapot on a hot Plate.
;;;;  + MTF -- Milk in a Teapot in a Fridge.
;;;;  * CM1 -- How to cool milk in a teapot?
;;;;  * HM1 -- How to heat milk in a teapot?
;;;;  * HM2 -- Milk in a tpot on a plate. What will happen?


;;;;;;;  Situation-agent
;;

(defagent   sit-CM2    temp-instance-agent
  "Milk in a teapot in a fridge. What is the goal?"
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
)


;;;;;;;  Participating objects
;;
;;  milk-CM2   :  (inst-of milk)
;;  tpot-CM2   :  (inst-of teapot)
;;  fridge-CM2 :  (inst-of fridge)
;;

(defagent   milk-CM2    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-CM2 0.1)
  :inst-of    milk
  :c-coref    (in-CM2-mt . :slot1)
  :a-link     (initst-CM2 0.25)
)

(defagent   tpot-CM2    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-CM2 0.1)
  :inst-of    teapot
  :c-coref    ((in-CM2-mt . :slot2)
               (in-CM2-tf . :slot1)
               ((color-of-CM2 . :slot1) 0.3) )
  :a-link     ((initst-CM2 0.2)
               (black-CM2  0.3) )
)

(defagent   fridge-CM2    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-CM2 0.1)
  :inst-of    fridge
  :c-coref    (((T-of-CM2   . :slot1) 1.00)
               ((initst-CM2 . :slot1) 0.50)
               ((in-CM2-tf  . :slot2) 0.25) )
  :a-link     (low-T-CM2 1.0)
)


;;;;;;;  Initial relations
;;
;;  in-CM2-mt    : (in milk-CM2 tpot-CM2)
;;  in-CM2-tf    : (in tpot-CM2 fridge-CM2)
;;  T-of-CM2     : (temperature-of fridge-CM2 low-T-CM2)
;;  color-of-CM2 : (color-of tpot-CM2 black-CM2)
;;

(defagent   in-CM2-mt    temp-instance-agent
  "(in milk-CM2 tpot-CM2)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-CM2 0.1)
  :inst-of    in
  :c-coref    (initst-CM2 . :slot3)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    milk-CM2
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    tpot-CM2
)

(defagent   in-CM2-tf    temp-instance-agent
  "(in tpot-CM2 fridge-CM2)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-CM2 0.1)
  :inst-of    in
  :c-coref    (initst-CM2 . :slot2)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    tpot-CM2
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    fridge-CM2
)

(defagent   T-of-CM2    temp-instance-agent
  "(temperature-of fridge-CM2 low-T-CM2)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-CM2 0.1)
  :inst-of    temperature-of
  :c-coref    (initst-CM2 . :slot4)
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    fridge-CM2
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    low-T-CM2
)
(defagent  low-T-CM2   temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-CM2 0.1)
  :inst-of    low-temp
  :c-coref    (T-of-CM2 . :slot2)
  :a-link     (fridge-CM2 1.0)
)

(defagent   color-of-CM2    instance-agent
  "(color-of tpot-CM2 black-CM2)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-CM2 0.1)
  :inst-of    color-of
  :slot1
    :type       :aspect
    :inst-of    (color-of . :slot1)
    :c-coref    tpot-CM2
  :slot2
    :type       :aspect
    :inst-of    (color-of . :slot2)
    :c-coref    black-CM2
)
(defagent  black-CM2  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-CM2 0.2)
  :inst-of    black
  :c-coref    (color-of-CM2 . :slot2)
)


;;;;;;;  Initial state
;;
;;  initst-CM2  -to-reach->  goalst-CM2
;;  initst-CM2  :  (init-state fridge-CM2 in-CM2-tf in-CM2-mt T-of-CM2)
;;

(defagent   initst-CM2  temp-instance-agent
  "initst-CM2  -to-reach->  goalst-CM2"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-CM2 0.1)
  :inst-of    init-state
  :c-coref    (to-reach-CM2 . :slot1)
  :a-link     ((goalst-CM2 0.5)
               (low-T-CM2  0.5)
               (milk-CM2   0.2)
               (tpot-CM2   0.2) )
  :slot1
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    fridge-CM2
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-CM2-tf
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-CM2-mt
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-CM2
)


;;;;;;;  Goal state
;;
;;  goalst-CM2  <-to-reach-  initst-CM2
;;  goalst-CM2   :  (goal-state ???)
;;
;;  to-reach-CM2 :  (to-reach initst-CM2 goalst-CM2)
;;

(defagent   goalst-CM2   temp-instance-agent
  "(goal-state ???)"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-CM2 0.1)
  :inst-of    goal-state
  :c-coref    (to-reach-CM2 . :slot2)
  :a-link     (initst-CM2 1.0)
  ;;slot1 must be filled by the model
)

(defagent   to-reach-CM2  temp-instance-agent
  "(to-reach initst-CM2 goalst-CM2)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-CM2 0.1)
  :inst-of    to-reach
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-CM2
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-CM2
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-CM2
  "Milk in a teapot in a fridge. What is the goal?"
  :head      sit-CM2                   ; 13 agents
  :members   (sit-CM2
              milk-CM2     tpot-CM2    fridge-CM2
              in-CM2-mt    in-CM2-tf
              T-of-CM2     low-T-CM2
              color-of-CM2 black-CM2
              initst-CM2   goalst-CM2  to-reach-CM2
             ))


(defun  attach-target (&optional (verbose-p T))
  "Attach SIT-CM2 to goal and input lists and prepare the model to run."
;;;; Add to *goal* and *input*, see AMBR/GOALINPT.LSP.
  (add-T-driver-tag #$sit-CM2)
  (add-to-goal/input #$goalst-CM2    1.00 :goal )
  (add-to-goal/input #$to-reach-CM2  1.00 :goal )
  (add-to-goal/input #$milk-CM2      1.00 :input )
  (add-to-goal/input #$tpot-CM2      1.00 :input )
  (add-to-goal/input #$fridge-CM2    1.00 :input )
  (add-to-goal/input #$T-of-CM2      0.50 :input )
  (add-to-goal/input #$low-T-CM2     0.50 :input )
  (add-to-goal/input #$in-CM2-tf     0.50 :input )
  (add-to-goal/input #$in-CM2-mt     0.50 :input )
  (add-to-goal/input #$color-of-CM2  0.25 :input )
  (add-to-goal/input #$black-CM2     0.25 :input )
;;;; Put global parameters in order, see AMBR/TOPLEVEL.LSP and AMBR/DEFS.LSP
  (enforce-AMBR-parameters)
  (make-node-constructors *number-of-node-constructors*
                          :mode :at-least)
;;;;;;;;;;;;;
;;;; For the interface only: arrange for REPORT, RETRIEVAL-INDEX, VERBOSE, etc.
  (setq *target* (find-coalition 'sit-CM2))    ; see AMBR/INTRFACE/DEFS.LSP
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
;;  sit-CM2       :  (inst-of sit-CM2 situation)
;;
;;  black-CM2     :  (inst-of black-CM2 black)
;;  color-of-CM2  :  (color-of tpot-CM2 black-CM2)
;;  goalst-CM2    :  (goal-state ???)
;;  to-reach-CM2  :  (to-reach initst-CM2 goalst-CM2)
;;  fridge-CM2    :  (inst-of fridge-CM2 fridge)
;;  in-CM2-mt     :  (in milk-CM2 tpot-CM2)
;;  in-CM2-tf     :  (in tpot-CM2 fridge-CM2)
;;  initst-CM2    :  (init-state fridge-CM2 in-CM2-tf in-CM2-mt T-of-CM2)
;;  low-T-CM2     :  (inst-of low-T-CM2 low-temp)
;;  milk-CM2      :  (inst-of milk-CM2 milk)
;;  T-of-CM2      :  (temperature-of fridge-CM2 low-T-CM2)
;;  tpot-CM2      :  (inst-of tpot-CM2 teapot)


;;;;;;  End of file  AMBR/KB/EPISODIC/T_CM2.LSP
