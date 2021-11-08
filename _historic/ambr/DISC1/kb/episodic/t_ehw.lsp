;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/t_EHW.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Target situation EHW -- 'Egg in Hot Water. What will happen?'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    28-06-98 [3.0.0]
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;              SITUATION  I C C                   ;;;;
         ;;;;  Egg in Hot Water in a teapot. What'll happen?  ;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Target situation EHW   ;;;;;;;;;;;
;;;;
;;;; There is an egg in some water.  The water
;;;; is in a teapot. The teapot is made of metal.
;;;; The water is hot. The color of the egg is white.
;;;;
;;;; The goal, if any, is not represented explicitly.
;;;;
;;;; What will be the result of this state of
;;;; affairs?
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + ERW -- Egg in Red Water.
;;;;  + IHC -- Imm.Heater in a Cup with water.


;;;;;;;  Situation-agent
;;

(defagent   sit-EHW    temp-instance-agent
  "Egg in Hot Water in a tpot. What'll happen?"
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
)


;;;;;;;  Participating objects
;;
;;  egg-EHW    :  (inst-of egg)
;;  water-EHW  :  (inst-of water)
;;  tpot-EHW   :  (inst-of teapot)
;;

(defagent   egg-EHW    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-EHW 0.1)
  :inst-of    egg
  :c-coref    ((initst-EHW   . :slot1)
               (in-EHW-ew    . :slot1)
               (color-of-EHW . :slot1) )
  :a-link     (white-EHW 0.5)
)

(defagent   water-EHW    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-EHW 0.1)
  :inst-of    water
  :c-coref    ((T-of-EHW  . :slot1)
               (in-EHW-ew . :slot2)
               (in-EHW-wt . :slot1) )
  :a-link     (high-T-EHW 0.5)
)

(defagent   tpot-EHW    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-EHW 0.1)
  :inst-of    teapot
  :c-coref    ((in-EHW-wt   . :slot2)
               (made-of-EHW . :slot1) )
  :a-link     (initst-EHW 0.5)
  :slot1  
    :type      :relation
    :inst-of   (teapot . :slot1)
    :c-coref   made-of-EHW
    :a-link    (mmetal-EHW  0.5)
)


;;;;;;;  Initial relations
;;
;;  in-EHW-ew    : (in egg-EHW water-EHW)
;;  in-EHW-wt    : (in water-EHW tpot-EHW)
;;  T-of-EHW     : (temperature-of water-EHW high-T-EHW)
;;  made-of-EHW  : (made-of tpot-EHW mmetal-EHW)
;;  color-of-EHW : (color-of egg-EHW white-EHW)
;;

(defagent   in-EHW-ew    temp-instance-agent
  "(in egg-EHW water-EHW)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-EHW 0.1)
  :inst-of    in
  :c-coref    (initst-EHW . :slot2)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    egg-EHW
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    water-EHW
)

(defagent   in-EHW-wt    temp-instance-agent
  "(in water-EHW tpot-EHW)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-EHW 0.1)
  :inst-of    in
  :c-coref    (initst-EHW . :slot3)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    water-EHW
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    tpot-EHW
)

(defagent   T-of-EHW    temp-instance-agent
  "(temperature-of water-EHW high-T-EHW)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-EHW 0.1)
  :inst-of    temperature-of
  :c-coref    (initst-EHW . :slot4)
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    water-EHW
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-EHW
)
(defagent  high-T-EHW   temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-EHW 0.1)
  :inst-of    high-temp
  :c-coref    (T-of-EHW . :slot2)
  :a-link     (water-EHW 1.0)
)

(defagent   made-of-EHW    temp-instance-agent
  "(made-of tpot-EHW mmetal-EHW)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-EHW 0.1)
  :inst-of    made-of
  :c-coref    (tpot-EHW . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    tpot-EHW
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mmetal-EHW
)
(defagent  mmetal-EHW  temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-EHW 0.1)
  :inst-of    material-metal
  :c-coref    (made-of-EHW  . :slot2)
  :a-link     (tpot-EHW 1.0)
)

(defagent   color-of-EHW    temp-instance-agent
  "(color-of egg-EHW white-EHW)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-EHW 0.1)
  :inst-of    color-of
  :slot1
    :type       :aspect
    :inst-of    (color-of . :slot1)
    :c-coref    egg-EHW
  :slot2
    :type       :aspect
    :inst-of    (color-of . :slot2)
    :c-coref    white-EHW
)
(defagent  white-EHW  temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-EHW 0.1)
  :inst-of    white
  :c-coref    (color-of-EHW  . :slot2)
  :a-link     (egg-EHW 1.0)
)


;;;;;;;  Initial state
;;
;;  initst-EHW  -follows->  endst-EHW
;;  initst-EHW  :  (init-state egg-EHW in-EHW-ew in-EHW-wt T-of-EHW)
;;

(defagent   initst-EHW  temp-instance-agent
  "initst-EHW  -follows->  endst-EHW"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-EHW 0.1)
  :inst-of    init-state
  :c-coref    (follows-EHW . :slot1)
  :a-link     (endst-EHW 0.5)
  :slot1
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    egg-EHW
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-EHW-ew
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-EHW-wt
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-EHW
)


;;;;;;;  End state
;;
;;  endst-EHW  <-follows-  initst-EHW
;;  endst-EHW   :  (end-state ???)
;;
;;  follows-EHW :  (follows initst-EHW endst-EHW)
;;

(defagent   endst-EHW   temp-instance-agent
  "(end-state ???)"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-EHW 0.1)
  :inst-of    end-state
  :c-coref    (follows-EHW . :slot2)
  :a-link     (initst-EHW 1.0)
  ;;slot1 must be filled by the model
)

(defagent   follows-EHW  temp-instance-agent
  "(follows initst-EHW endst-EHW)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-EHW 0.1)
  :inst-of    follows
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-EHW
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-EHW
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-EHW
  "Egg in Hot Water in a tpot. What'll happen?"
  :head      sit-EHW                     ; 15 agents
  :members   (sit-EHW
              egg-EHW       water-EHW    tpot-EHW
              in-EHW-ew     in-EHW-wt
              T-of-EHW      high-T-EHW
              made-of-EHW   mmetal-EHW
              color-of-EHW  white-EHW
              initst-EHW    endst-EHW    follows-EHW
             ))


(defun  attach-target (&optional (verbose-p T))
  "Attach SIT-EHW to goal and input lists and prepare the model to run."
;;;; Add to *goal* and *input*, see AMBR/GOALINPT.LSP.
  (add-T-driver-tag #$sit-EHW)
  (add-to-goal/input #$endst-EHW    1.00 :goal )
  (add-to-goal/input #$follows-EHW  1.00 :goal )
  (add-to-goal/input #$egg-EHW      1.00 :input )
  (add-to-goal/input #$water-EHW    1.00 :input )
  (add-to-goal/input #$tpot-EHW     1.00 :input )
  (add-to-goal/input #$T-of-EHW     1.00 :input )
  (add-to-goal/input #$high-T-EHW   1.00 :input )
  (add-to-goal/input #$in-EHW-ew    0.50 :input )
  (add-to-goal/input #$in-EHW-wt    0.50 :input )
  (add-to-goal/input #$made-of-EHW  0.25 :input )
  (add-to-goal/input #$mmetal-EHW   0.25 :input )
  (add-to-goal/input #$color-of-EHW 0.25 :input )
  (add-to-goal/input #$white-EHW    0.25 :input )
;;;; Put global parameters in order, see AMBR/TOPLEVE.LSP and AMBR/DEFS.LSP
  (enforce-AMBR-parameters)
  (make-node-constructors *number-of-node-constructors*
                          :mode :at-least)
;;;;;;;;;;;;;
;;;; For the interface only: arrange for REPORT, RETRIEVAL-INDEX, VERBOSE, etc.
  (setq *target* (find-coalition 'sit-EHW))    ; see AMBR/INTRFACE/DEFS.LSP
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
;;  sit-EHW       :  (inst-of sit-EHW situation)
;;
;;  color-of-EHW  :  (color-of egg-EHW white-EHW)
;;  egg-EHW       :  (inst-of egg-EHW egg)
;;  endst-EHW     :  (end-state ???)
;;  follows-EHW   :  (follows initst-EHW endst-EHW)
;;  in-EHW-ew     :  (in egg-EHW water-EHW)
;;  in-EHW-wt     :  (in water-EHW tpot-EHW)
;;  initst-EHW    :  (init-state egg-EHW in-EHW-ew in-EHW-wt T-of-EHW)
;;  high-T-EHW    :  (inst-of high-T-EHW high-temp)
;;  made-of-EHW   :  (made-of tpot-EHW mmetal-EHW)
;;  mmetal-EHW    :  (inst-of mmetal-EHW material-metal)
;;  T-of-EHW      :  (temperature-of water-EHW high-T-EHW)
;;  tpot-EHW      :  (inst-of tpot-EHW teapot)
;;  water-EHW     :  (inst-of water-EHW water)
;;  white-EHW     :  (inst-of white-EHW white)


;;;;;;  End of file  AMBR/KB/EPISODIC/T_EHW.LSP
