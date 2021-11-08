;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/t_HM2.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Target sit.HM2 -- 'Milk in a teapot on a plate. What'll happen?'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   ambr/kb/episodic/t_HM1.lsp, =/t_CM2.lsp
;;; CREATED:    21-06-98 [3.0.0]  Elaboration of SIT-Y2 from old LTM.LSP.
;;; UPDATED:    ...

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;                 SITUATION  H M 2                   ;;;;
        ;;;;   Milk in a teapot on a plate. What will happen?   ;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Target situation HM2   ;;;;;;;;;;;
;;;;
;;;; There is some milk in a teapot.  The teapot
;;;; is on a plate.  The plate is hot.
;;;;
;;;; The goal, if any, is not represented explicitly.
;;;;
;;;; What will be the result of this state of
;;;; affairs?
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + WTP -- Water in a Teapot on a hot Plate.
;;;;  + MTF -- Milk in a Teapot in a Fridge.
;;;;  * HM1 -- How to heat milk in a teapot?
;;;;  * CM1 -- How to cool milk in a teapot?
;;;;  * CM2 -- Milk in a tpot in a fridge. What is the goal?


;;;;;;;  Situation-agent
;;

(defagent   sit-HM2    temp-instance-agent
  "Milk in a teapot on a plate. What will happen?"
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
)


;;;;;;;  Participating objects
;;
;;  milk-HM2   :  (inst-of milk)
;;  tpot-HM2   :  (inst-of teapot)
;;  hplate-HM2 :  (inst-of hot-plate)
;;

(defagent   hplate-HM2    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-HM2 0.1)
  :inst-of    hot-plate
  :c-coref    (((T-of-HM2   . :slot1) 1.00)
               ((initst-HM2 . :slot1) 0.50)
               ((on-HM2     . :slot1) 0.25) )
  :a-link     (high-T-HM2 1.0)
)

(defagent   milk-HM2    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-HM2 0.1)
  :inst-of    milk
  :c-coref    (in-HM2 . :slot1)
  :a-link     (initst-HM2 0.25)
)

(defagent   tpot-HM2    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-HM2 0.1)
  :inst-of    teapot
  :c-coref    ((in-HM2 . :slot2)
               (on-HM2 . :slot2) )
  :a-link     (initst-HM2 0.3)
)


;;;;;;;  Initial relations
;;
;;  in-HM2   : (in milk-HM2   tpot-HM2)
;;  on-HM2   : (on hplate-HM2 tpot-HM2)
;;  T-of-HM2 : (temperature-of hplate-HM2 high-T-HM2)
;;

(defagent   on-HM2    temp-instance-agent
  "(on hplate-HM2 tpot-HM2)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-HM2 0.1)
  :inst-of    on
  :c-coref    (initst-HM2 . :slot2)
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    hplate-HM2
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    tpot-HM2
)

(defagent   in-HM2    temp-instance-agent
  "(in milk-HM2 tpot-HM2)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-HM2 0.1)
  :inst-of    in
  :c-coref    (initst-HM2 . :slot3)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    milk-HM2
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    tpot-HM2
)

(defagent   T-of-HM2    temp-instance-agent
  "(temperature-of hplate-HM2 high-T-HM2)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-HM2 0.1)
  :inst-of    temperature-of
  :c-coref    (initst-HM2 . :slot4)
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    hplate-HM2
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-HM2
)
(defagent  high-T-HM2   temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-HM2 0.1)
  :inst-of    high-temp
  :c-coref    (T-of-HM2 . :slot2)
  :a-link     (hplate-HM2 1.0)
)


;;;;;;;  Initial state
;;
;;  initst-HM2  -follows->  endst-HM2
;;  initst-HM2  :  (init-state hplate-HM2 on-HM2 in-HM2 T-of-HM2)
;;

(defagent   initst-HM2  temp-instance-agent
  "initst-HM2  -follows->  endst-HM2"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-HM2 0.1)
  :inst-of    init-state
  :c-coref    (follows-HM2 . :slot1)
  :a-link     ((endst-HM2  0.5)
               (high-T-HM2 0.5)
               (milk-HM2   0.2)
               (tpot-HM2   0.2) )
  :slot1
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    hplate-HM2
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-HM2
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-HM2
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-HM2
)


;;;;;;;  End state
;;
;;  endst-HM2  <-follows-  initst-HM2
;;  endst-HM2   :  (end-state ???)
;;
;;  follows-HM2 :  (follows initst-HM2 endst-HM2)
;;

(defagent   endst-HM2   temp-instance-agent
  "(end-state ???)"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-HM2 0.1)
  :inst-of    end-state
  :c-coref    (follows-HM2 . :slot2)
  :a-link     (initst-HM2 1.0)
  ;;slot1 must be filled by the model
)

(defagent   follows-HM2  temp-instance-agent
  "(follows initst-HM2 endst-HM2)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-HM2 0.1)
  :inst-of    follows
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-HM2
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-HM2
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-HM2
  "Milk in a teapot on a plate. What will happen?"
  :head      sit-HM2                   ; 11 agents
  :members   (sit-HM2
              milk-HM2     tpot-HM2    hplate-HM2
              on-HM2       in-HM2
              T-of-HM2     high-T-HM2
              initst-HM2   endst-HM2   follows-HM2
             ))


(defun  attach-target (&optional (verbose-p T))
  "Attach SIT-HM2 to goal and input lists and prepare the model to run."
;;;; Add to *goal* and *input*, see AMBR/GOALINPT.LSP.
  (add-T-driver-tag #$sit-HM2)
  (add-to-goal/input #$endst-HM2    1.00 :goal )
  (add-to-goal/input #$follows-HM2  1.00 :goal )
  (add-to-goal/input #$milk-HM2     1.00 :input )
  (add-to-goal/input #$tpot-HM2     1.00 :input )
  (add-to-goal/input #$hplate-HM2   1.00 :input )
  (add-to-goal/input #$T-of-HM2     1.00 :input )
  (add-to-goal/input #$high-T-HM2   1.00 :input )
  (add-to-goal/input #$on-HM2       0.50 :input )
  (add-to-goal/input #$in-HM2       0.50 :input )
;;;; Put global parameters in order, see AMBR/TOPLEVEL.LSP and AMBR/DEFS.LSP
  (enforce-AMBR-parameters)
  (make-node-constructors *number-of-node-constructors*
                          :mode :at-least)
;;;;;;;;;;;;;
;;;; For the interface only: arrange for REPORT, RETRIEVAL-INDEX, VERBOSE, etc.
  (setq *target* (find-coalition 'sit-HM2))    ; see AMBR/INTRFACE/DEFS.LSP
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
;;  sit-HM2       :  (inst-of sit-HM2 situation)
;;
;;  endst-HM2     :  (end-state ???)
;;  follows-HM2   :  (follows initst-HM2 endst-HM2)
;;  high-T-HM2    :  (inst-of high-T-HM2 high-temp)
;;  hplate-HM2    :  (inst-of hplate-HM2 hot-plate)
;;  in-HM2        :  (in milk-HM2 tpot-HM2)
;;  initst-HM2    :  (init-state hplate-HM2 on-HM2 in-HM2 T-of-HM2)
;;  milk-HM2      :  (inst-of milk-HM2 milk)
;;  on-HM2        :  (on hplate-HM2 tpot-HM2)
;;  T-of-HM2      :  (temperature-of hplate-HM2 high-T-HM2)
;;  tpot-HM2      :  (inst-of tpot-HM2 teapot)


;;;;;;  End of file  AMBR/KB/EPISODIC/T_HM2.LSP
