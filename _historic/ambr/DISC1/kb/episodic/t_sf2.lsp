;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/t_SF2.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Target sit.SF2 -- 'Salt in Food on a plate. What will happen?'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   ambr/kb/episodic/t_SF1.lsp
;;; CREATED:    21-06-98 [3.0.0]
;;; UPDATED:    ...

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;                 SITUATION  H M 2                   ;;;;
        ;;;;     Salt in Food on a plate. What will happen?     ;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Target situation SF2   ;;;;;;;;;;;
;;;;
;;;; There is some food on a plate.  There is
;;;; some salt in the food.
;;;;
;;;; The goal, if any, is not represented explicitly.
;;;;
;;;; What will be the result of this state of
;;;; affairs?
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + SFF -- Salt in Food on a plate in a Fridge.
;;;;  + STC -- Sugar in Tea in a Cup.
;;;;  * SF1 -- How to Salt Food on a plate?


;;;;;;;  Situation-agent
;;

(defagent   sit-SF2    temp-instance-agent
  "Salt in Food on a plate. What will happen?"
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
)


;;;;;;;  Participating objects
;;
;;  salt-SF2   :  (inst-of salt)
;;  food-SF2   :  (inst-of food)
;;  plate-SF2  :  (inst-of plate)
;;

(defagent   salt-SF2    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-SF2 0.1)
  :inst-of    salt
  :c-coref    ((in-SF2     . :slot1)
               (initst-SF2 . :slot1) )
)

(defagent   food-SF2    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-SF2 0.1)
  :inst-of    food
  :c-coref    ((on-SF2 . :slot2)
               (in-SF2 . :slot2)
               (initst-SF2 . :slot2) )
)

(defagent   plate-SF2    temp-instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-SF2 0.1)
  :inst-of    plate
  :c-coref    ((on-SF2 . :slot2)
               (initst-SF2 . :slot3) )
)


;;;;;;;  Initial relations
;;
;;  in-SF2   : (in salt-SF2 food-SF2)
;;  on-SF2   : (on plate-SF2 food-SF2)
;;

(defagent   in-SF2    temp-instance-agent
  "(in salt-SF2 food-SF2)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-SF2 0.1)
  :inst-of    in
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    salt-SF2
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    food-SF2
)

(defagent   on-SF2    temp-instance-agent
  "(on plate-SF2 food-SF2)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-SF2 0.1)
  :inst-of    on
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    plate-SF2
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    food-SF2
)


;;;;;;;  Initial state
;;
;;  initst-SF2  -follows->  endst-SF2
;;  initst-SF2  :  (init-state salt-SF2 food-SF2 plate-SF2)
;;

(defagent   initst-SF2  temp-instance-agent
  "initst-SF2  -follows->  endst-SF2"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-SF2 0.1)
  :inst-of    init-state
  :c-coref    (follows-SF2 . :slot1)
  :a-link     (endst-SF2 0.5)
  :slot1
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    salt-SF2
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    food-SF2
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    plate-SF2
)


;;;;;;;  End state
;;
;;  endst-SF2  <-follows-  initst-SF2
;;  endst-SF2   :  (end-state ???)
;;
;;  follows-SF2 :  (follows initst-SF2 endst-SF2)
;;

(defagent   endst-SF2   temp-instance-agent
  "(end-state ???)"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-SF2 0.1)
  :inst-of    end-state
  :c-coref    (follows-SF2 . :slot2)
  :a-link     (initst-SF2 1.0)
  ;;slot1 must be filled by the model
)

(defagent   follows-SF2  temp-instance-agent
  "(follows initst-SF2 endst-SF2)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-SF2 0.1)
  :inst-of    follows
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-SF2
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-SF2
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-SF2
  "Salt in Food on a plate. What will happen?"
  :head      sit-SF2                   ; 9 agents
  :members   (sit-SF2
              salt-SF2     food-SF2    plate-SF2
              in-SF2       on-SF2
              initst-SF2   endst-SF2   follows-SF2
             ))


(defun  attach-target (&optional (verbose-p T))
  "Attach SIT-SF2 to goal and input lists and prepare the model to run."
;;;; Add to *goal* and *input*, see AMBR/GOALINPT.LSP.
  (add-T-driver-tag #$sit-SF2)
  (add-to-goal/input #$endst-SF2    1.00 :goal )
  (add-to-goal/input #$follows-SF2  1.00 :goal )
  (add-to-goal/input #$food-SF2     1.00 :input )
  (add-to-goal/input #$plate-SF2    1.00 :input )
  (add-to-goal/input #$salt-SF2     1.00 :input )
  (add-to-goal/input #$in-SF2       0.50 :input )
  (add-to-goal/input #$on-SF2       0.50 :input )
;;;; Put global parameters in order, see AMBR/TOPLEVEL.LSP and AMBR/DEFS.LSP
  (enforce-AMBR-parameters)
  (make-node-constructors *number-of-node-constructors*
                          :mode :at-least)
;;;;;;;;;;;;;
;;;; For the interface only: arrange for REPORT, RETRIEVAL-INDEX, VERBOSE, etc.
  (setq *target* (find-coalition 'sit-SF2))    ; see AMBR/INTRFACE/DEFS.LSP
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
;;  sit-SF2       :  (inst-of sit-SF2 situation)
;;
;;  endst-SF2     :  (end-state ???)
;;  follows-SF2   :  (follows initst-SF2 endst-SF2)
;;  food-SF2      :  (inst-of food-SF2 food)
;;  in-SF2        :  (in salt-SF2 food-SF2)
;;  initst-SF2    :  (init-state salt-SF2 food-SF2 plate-SF2)
;;  on-SF2        :  (on plate-SF2 food-SF2)
;;  plate-SF2     :  (inst-of plate-SF2 plate)
;;  salt-SF2      :  (inst-of salt-SF2 salt)


;;;;;;  End of file  AMBR/KB/EPISODIC/T_SF2.LSP
