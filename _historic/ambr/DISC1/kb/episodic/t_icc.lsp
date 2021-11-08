;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/t_ICC.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Target situation ICC -- 'Ice Cube in Coke. What will happen?'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   ambr/kb/episodic/t_CCG.lsp
;;; CREATED:    31-05-98 [3.0.0]  Elaboration of SIT-ICC from old LTM.LSP.
;;; UPDATED:    18-06-98  Removed RESULT proposition.  Weight adjustment.
;;;                       Added ON-ICC and TABLE-ICC.
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;              SITUATION  I C C               ;;;;
         ;;;;     Ice Cube in Coke. What will happen?     ;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Target situation ICC   ;;;;;;;;;;;
;;;;
;;;; There is some coke in a glass.  There is an
;;;; ice cube in the coke. The ice cube is cold.
;;;; The glass is on a table.  The glass is made
;;;; of material-glass.
;;;;
;;;; The goal, if any, is not represented explicitly.
;;;;
;;;; What will be the result of this state of
;;;; affairs?
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + IHC -- Imm.Heater in a Cup with water.
;;;;  + ICF -- Ice Cube in a glass in a Fridge.
;;;;  * CCG -- How to Cool Coke in a Glass?


;;;;;;;  Situation-agent
;;

(defagent   sit-ICC    temp-instance-agent
  "Ice Cube in Coke. What will happen?"
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
)


;;;;;;;  Participating objects
;;
;;  ice-cube-ICC :  (inst-of ice-cube)
;;  coke-ICC     :  (inst-of coke)
;;  glass-ICC    :  (inst-of glass)
;;  table-ICC    :  (inst-of table)
;;

(defagent   ice-cube-ICC    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-ICC 0.2)
  :inst-of    ice-cube
  :c-coref    (((T-of-ICC   . :slot1) 1.00)
               ((initst-ICC . :slot1) 0.50)
               ((in-ICC-ic  . :slot1) 0.25) )
  :a-link     (low-T-ICC 1.0)
)

(defagent   coke-ICC    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-ICC 0.2)
  :inst-of    coke
  :c-coref    (((in-ICC-ic . :slot2) 0.5)
               ((in-ICC-cg . :slot1) 0.5) )
  :a-link     (initst-ICC 0.25)
)

(defagent   glass-ICC    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-ICC 0.2)
  :inst-of    glass
  :c-coref    (((in-ICC-cg   . :slot2) 1.0)
               ((on-ICC      . :slot2) 0.5)
               ((made-of-ICC . :slot1) 0.3) )
  :a-link     ((initst-ICC 0.3)
               (table-ICC  0.2) )
  :slot1  
    :type      :relation
    :inst-of   (glass . :slot1)
    :c-coref   (made-of-ICC 0.3)
    :a-link    (mglass-ICC  0.3)
)

(defagent   table-ICC    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-ICC 0.2)
  :inst-of    table
  :c-coref    (on-ICC . :slot2)
  :a-link     ((initst-ICC 0.1)
               (glass-ICC  0.3) )
)


;;;;;;;  Initial relations
;;
;;  in-ICC-ic   : (in ice-cube-ICC coke-ICC)
;;  in-ICC-cg   : (in coke-ICC glass-ICC)
;;  on-ICC      : (on table-ICC glass-ICC)
;;  T-of-ICC    : (temperature-of ice-cube-ICC low-T-ICC)
;;  made-of-ICC : (made-of glass-ICC mglass-ICC)
;;

(defagent   in-ICC-ic    temp-instance-agent
  "(in ice-cube-ICC coke-ICC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ICC 0.2)
  :inst-of    in
  :c-coref    (initst-ICC . :slot2)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    ice-cube-ICC
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    coke-ICC
)

(defagent   in-ICC-cg    temp-instance-agent
  "(in coke-ICC glass-ICC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ICC 0.2)
  :inst-of    in
  :c-coref    (initst-ICC . :slot3)
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    coke-ICC
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    glass-ICC
)

(defagent   on-ICC    temp-instance-agent
  "(on table-ICC glass-ICC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ICC 0.2)
  :inst-of    on
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    table-ICC
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    glass-ICC
)

(defagent   T-of-ICC    temp-instance-agent
  "(temperature-of ice-cube-ICC low-T-ICC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ICC 0.2)
  :inst-of    temperature-of
  :c-coref    (initst-ICC . :slot4)
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    ice-cube-ICC
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    low-T-ICC
)
(defagent  low-T-ICC   temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-ICC 0.2)
  :inst-of    low-temp
  :c-coref    (T-of-ICC . :slot2)
  :a-link     (ice-cube-ICC 1.0)
)

(defagent   made-of-ICC    temp-instance-agent
  "(made-of glass-ICC mglass-ICC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ICC 0.2)
  :inst-of    made-of
  :c-coref    (glass-ICC . :slot1)
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    glass-ICC
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mglass-ICC
)
(defagent  mglass-ICC  temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-ICC 0.2)
  :inst-of    material-glass
  :c-coref    (made-of-ICC  . :slot2)
  :a-link     (glass-ICC 1.0)
)


;;;;;;;  Initial state
;;
;;  initst-ICC  -follows->  endst-ICC
;;  initst-ICC  :  (init-state ice-cube-ICC in-ICC-ic in-ICC-cg T-of-ICC)
;;

(defagent   initst-ICC  temp-instance-agent
  "initst-ICC  -follows->  endst-ICC"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-ICC 0.2)
  :inst-of    init-state
  :c-coref    (follows-ICC . :slot1)
  :a-link     ((endst-ICC 0.5)
               (low-T-ICC 0.5)
               (coke-ICC  0.2)
               (glass-ICC 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    ice-cube-ICC
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-ICC-ic
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-ICC-cg
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-ICC
)


;;;;;;;  End state
;;
;;  endst-ICC  <-follows-  initst-ICC
;;  endst-ICC   :  (end-state ???)
;;
;;  follows-ICC :  (follows initst-ICC endst-ICC)
;;

(defagent   endst-ICC   temp-instance-agent
  "(end-state ???)"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-ICC 0.2)
  :inst-of    end-state
  :c-coref    (follows-ICC . :slot2)
  :a-link     (initst-ICC 1.0)
  ;;slot1 must be filled by the model
)

(defagent   follows-ICC  temp-instance-agent
  "(follows initst-ICC endst-ICC)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-ICC 0.2)
  :inst-of    follows
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-ICC
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-ICC
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-ICC
  "Ice Cube in Coke. What will happen?"
  :head      sit-ICC                     ; 15 agents
  :members   (sit-ICC
              ice-cube-ICC  coke-ICC
              glass-ICC     table-ICC
              in-ICC-ic     in-ICC-cg    on-ICC
              T-of-ICC      low-T-ICC
              made-of-ICC   mglass-ICC
              initst-ICC    endst-ICC
              follows-ICC
             ))


(defun  attach-target (&optional (verbose-p T))
  "Attach SIT-ICC to goal and input lists and prepare the model to run."
;;;; Add to *goal* and *input*, see AMBR/GOALINPT.LSP.
  (add-T-driver-tag #$sit-ICC)
  (add-to-goal/input #$endst-ICC    1.00 :goal )
  (add-to-goal/input #$follows-ICC  1.00 :goal )
  (add-to-goal/input #$coke-ICC     1.00 :input )
  (add-to-goal/input #$glass-ICC    1.00 :input )
  (add-to-goal/input #$ice-cube-ICC 1.00 :input )
  (add-to-goal/input #$T-of-ICC     1.00 :input )
  (add-to-goal/input #$low-T-ICC    1.00 :input )
  (add-to-goal/input #$in-ICC-ic    0.50 :input )
  (add-to-goal/input #$in-ICC-cg    0.50 :input )
  (add-to-goal/input #$on-ICC       0.50 :input )
  (add-to-goal/input #$made-of-ICC  0.25 :input )
  (add-to-goal/input #$mglass-ICC   0.25 :input )
;;;; Put global parameters in order, see AMBR/TOPLEVE.LSP and AMBR/DEFS.LSP
  (enforce-AMBR-parameters)
  (make-node-constructors *number-of-node-constructors*
                          :mode :at-least)
;;;;;;;;;;;;;
;;;; For the interface only: arrange for REPORT, RETRIEVAL-INDEX, VERBOSE, etc.
  (setq *target* (find-coalition 'sit-ICC))    ; see AMBR/INTRFACE/DEFS.LSP
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
;;  sit-ICC       :  (inst-of sit-ICC situation)
;;
;;  follows-ICC   :  (follows initst-ICC endst-ICC)
;;  coke-ICC      :  (inst-of coke-ICC coke)
;;  endst-ICC     :  (end-state ???)
;;  glass-ICC     :  (inst-of glass-ICC glass)
;;  ice-cube-ICC  :  (inst-of ice-cube-ICC ice-cube)
;;  in-ICC-ic     :  (in ice-cube-ICC coke-ICC)
;;  in-ICC-cg     :  (in coke-ICC glass-ICC)
;;  initst-ICC    :  (init-state ice-cube-ICC in-ICC-ic in-ICC-cg T-of-ICC)
;;  low-T-ICC     :  (inst-of low-T-ICC low-temp)
;;  made-of-ICC   :  (made-of glass-ICC mglass-ICC)
;;  mglass-ICC    :  (inst-of mglass-ICC material-glass)
;;  on-ICC        :  (on table-ICC glass-ICC)
;;  table-ICC     :  (inst-of table-ICC table)
;;  T-of-ICC      :  (temperature-of ice-cube-ICC low-T-ICC)


;;;;;;  End of file  AMBR/KB/EPISODIC/T_ICC.LSP
