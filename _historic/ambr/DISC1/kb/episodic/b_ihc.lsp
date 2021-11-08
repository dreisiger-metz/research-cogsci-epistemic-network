;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/b_IHC.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Base situation IHC -- 'Immersion Heater in a Cup with water.'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    30-05-98 [3.0.0]  Elaboration of SIT-IHC from old LTM.LSP.
;;; UPDATED:    18-06-98  Removed IS-GOAL and RESULT propositns. Wght adjustmt.
;;;                       CAUSE consequents are propositions now, not states.
;;;                       T-OF-IHC-W1 and T-OF-IHC-W2 coalesced together.
;;;                       Added ON-IHC and SAUCER-IHC.
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;          SITUATION  I H C           ;;;;;;;;
         ;;;;;;;;   Imm.Heater in a Cup with water    ;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Base situation IHC   ;;;;;;;;;;;
;;;;
;;;; There is some water in a cup.  There is an
;;;; immersion heater in the water. The imm.heater
;;;; is hot.  The cup is on a saucer.  The cup is
;;;; made of china.
;;;;
;;;; The goal is to heat the water.
;;;;
;;;; The result is that the water becomes hot
;;;; because the immersion heater is in it.
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + WTP -- Water in a Teapot on a Plate.
;;;;  + GP  -- Glass on a hot Plate breaks.
;;;;  + ...
;;;;  * ICC -- Ice Cube in Coke. What will happen?
;;;;  * ...


;;;;;;;  Situation-agent
;;

(defagent   sit-IHC    instance-agent
  "Imm.Heater in a Cup with water."
  :type       (:instance :situation)
  :inst-of    (situation 0.1)
  :a-link     ((imm-htr-IHC 1.0)
               (high-T-IHC  0.2)
               (T-of-IHC-w  0.2) )
)


;;;;;;;  Participating objects
;;
;;  imm-htr-IHC :  (inst-of immersion-heater)
;;  water-IHC   :  (inst-of water)
;;  cup-IHC     :  (inst-of cup)
;;  saucer-IHC  :  (inst-of saucer)
;;

(defagent   imm-htr-IHC    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-IHC 0.5)
  :inst-of    immersion-heater
  :c-coref    (((T-of-IHC-ih . :slot1) 1.0)
               ((initst-IHC  . :slot4) 0.3)
               ((in-IHC-iw   . :slot1) 0.2) )
  :a-link     (high-T-IHC 0.5)
)

(defagent   water-IHC    instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-IHC 0.2)
  :inst-of    water
  :c-coref    (((T-of-IHC-w . :slot1) 0.3)
               ((in-IHC-iw  . :slot2) 0.3)
               ((in-IHC-wc  . :slot1) 0.2)
               ((goalst-IHC . :slot3) 0.1) )
  :a-link     ((imm-htr-IHC 0.5)
               (initst-IHC  0.1) )
)

(defagent   cup-IHC    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-IHC 0.2)
  :inst-of    cup
  :c-coref    (((in-IHC-wc   . :slot2) 0.5)
               ((on-IHC      . :slot2) 0.5)
               ((made-of-IHC . :slot1) 0.3) )
  :a-link     ((initst-IHC  0.1)
               (saucer-IHC  0.2)
               (mchina-IHC  0.3) )
)

(defagent   saucer-IHC    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-IHC 0.2)
  :inst-of    saucer
  :c-coref    (on-IHC . :slot1)
  :a-link     (cup-IHC 0.5)
)


;;;;;;;  Initial relations
;;
;;  in-IHC-iw   : (in imm-htr-IHC water-IHC)
;;  in-IHC-wc   : (in water-IHC cup-IHC)
;;  on-IHC      : (on saucer-IHC cup-IHC)
;;  T-of-IHC-ih : (temperature-of imm-htr-IHC high-T-IHC)
;;  made-of-IHC : (made-of cup-IHC mchina-IHC)
;;

(defagent   in-IHC-iw    instance-agent
  "(in imm-htr-IHC water-IHC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-IHC 0.2)
  :inst-of    in
  :c-coref    ((initst-IHC . :slot3) 0.3)
  :a-link     ((T-of-IHC-w 0.3)
               (in-IHC-wc  0.4) )
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    imm-htr-IHC
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    water-IHC
)

(defagent   in-IHC-wc  instance-agent
  "(in water-IHC cup-IHC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-IHC 0.2)
  :inst-of    in
  :a-link     ((in-IHC-iw 0.3)
               (on-IHC    0.1) )
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    water-IHC
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    cup-IHC
)

(defagent   on-IHC    instance-agent
  "(on saucer-IHC cup-IHC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-IHC 0.2)
  :inst-of    on
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    saucer-IHC
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    cup-IHC
)

(defagent   T-of-IHC-ih    instance-agent
  "(temperature-of imm-htr-IHC high-T-IHC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-IHC 0.2)
  :inst-of    temperature-of
  :c-coref    ((initst-IHC . :slot1) 0.2)
  :a-link     ((T-of-IHC-w 0.2)
               (cause-IHC  0.2) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    imm-htr-IHC
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-IHC
)
(defagent  high-T-IHC   instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal :result)
  :situation  (sit-IHC 0.3)
  :inst-of    high-temp
  :c-coref    (((T-of-IHC-ih . :slot2) 0.7)
               ((T-of-IHC-w  . :slot2) 0.5)
               ((initst-IHC  . :slot2) 0.1)
               ((goalst-IHC  . :slot2) 0.1) )
  :a-link     (imm-htr-IHC 0.7)
)

(defagent   made-of-IHC    instance-agent
  "(made-of cup-IHC mchina-IHC)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-IHC 0.1)
  :inst-of    made-of
  :slot1
    :type       :aspect
    :inst-of    (made-of . :slot1)
    :c-coref    cup-IHC
  :slot2
    :type       :aspect
    :inst-of    (made-of . :slot2)
    :c-coref    mchina-IHC
)
(defagent  mchina-IHC  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-IHC 0.1)
  :inst-of    material-china
  :c-coref    (made-of-IHC . :slot2)
  :a-link     (cup-IHC 1.0)
)


;;;;;;;  Initial state
;;
;;  initst-IHC  -to-reach->  goalst-IHC
;;  initst-IHC  -follows->   endst-IHC
;;  initst-IHC  :  (init-state T-of-IHC-ih high-T-IHC in-IHC-iw imm-htr-IHC)
;;

(defagent   initst-IHC  instance-agent
  "initst-IHC  -to-reach->  goalst-IHC"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-IHC 0.2)
  :inst-of    init-state
  :c-coref    (((to-reach-IHC . :slot1) 0.5)
               ((follows-IHC  . :slot1) 0.3)
               ((cause-IHC    . :slot1) 0.3) )
  :a-link     ((goalst-IHC  0.3)
               (endst-IHC   0.3)
               (water-IHC   0.2)
               (cup-IHC     0.2)
               (saucer-IHC  0.1) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-IHC-ih
  :slot2
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    high-T-IHC
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-IHC-iw
  :slot4
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    imm-htr-IHC
)


;;;;;;;  Goal state
;;
;;  goalst-IHC  <-to-reach-  initst-IHC
;;  goalst-IHC   :  (goal-state T-of-IHC-w high-T-IHC water-IHC)
;;
;;  T-of-IHC-w   :  (temperature-of water-IHC high-T-IHC)
;;  to-reach-IHC :  (to-reach initst-IHC goalst-IHC)
;;

(defagent   T-of-IHC-w    instance-agent
  "(temperature-of water-IHC high-T-IHC)"
  :type       (:instance  :relation)
  :modality   (:GOAL   :intend-true
               :RESULT :true )
  :situation  (sit-IHC 0.3)
  :inst-of    temperature-of
  :c-coref    (((cause-IHC  . :slot2) 0.2)
               ((goalst-IHC . :slot1) 0.2)
               ((endst-IHC  . :slot1) 0.2) )
  :a-link     ((T-of-IHC-ih 0.5)
               (imm-htr-IHC 0.3) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    water-IHC
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    high-T-IHC
)

(defagent   goalst-IHC    instance-agent
  "goalst-IHC  <-to-reach-  initst-IHC"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-IHC 0.2)
  :inst-of    goal-state
  :c-coref    (to-reach-IHC . :slot2)
  :a-link     ((initst-IHC 0.5)
               (endst-IHC  0.2) )
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-IHC-w
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    high-T-IHC
  :slot3
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    water-IHC
)

(defagent   to-reach-IHC    instance-agent
  "(to-reach initst-IHC goalst-IHC)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-IHC 0.2)
  :inst-of    to-reach
  :a-link     ((follows-IHC 0.5)
               (cause-IHC   0.3) )
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-IHC
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-IHC
)


;;;;;;;  End state
;;
;;  endst-IHC  <-follows-  initst-IHC
;;  endst-IHC   :  (end-state T-of-IHC-w)
;;
;;  follows-IHC :  (follows initst-IHC endst-IHC)
;;  cause-IHC   :  (cause initst-IHC T-of-IHC-w)
;;

(defagent   endst-IHC    instance-agent
  "endst-IHC  <-follows-  initst-IHC"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-IHC 0.2)
  :inst-of    end-state
  :c-coref    (follows-IHC . :slot2)
  :a-link     ((initst-IHC 0.5)
               (goalst-IHC 0.2) )
  :slot1
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    T-of-IHC-w
)

(defagent   follows-IHC  instance-agent
  "(follows initst-IHC endst-IHC)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-IHC 0.2)
  :inst-of    follows
  :a-link     ((to-reach-IHC 0.5)
               (cause-IHC    0.2) )
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-IHC
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-IHC
)

(defagent   cause-IHC    instance-agent
  "(cause initst-IHC T-of-IHC-w)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-IHC 0.2)
  :inst-of    cause
  :a-link     ((endst-IHC    0.5)
               (follows-IHC  0.3)
               (to-reach-IHC 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    initst-IHC
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    T-of-IHC-w
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-IHC
  "Imm.Heater in a Cup with water."
  :head      sit-IHC                        ; 19 agents
  :members   (sit-IHC
              water-IHC      imm-htr-IHC
              cup-IHC        saucer-IHC
              in-IHC-iw      in-IHC-wc      on-IHC
              T-of-IHC-ih    T-of-IHC-w     high-T-IHC
              made-of-IHC    mchina-IHC
              initst-IHC     goalst-IHC     endst-IHC
              to-reach-IHC   follows-IHC    cause-IHC
             ))

(GENKB-template
  :herald  "Base sit.IHC -- Imm.Heater in a Cup with water, ver.3.0.0."
  :templates '(
    (water            (:instance (water-IHC    1))
                      (:a-link   (T-of-IHC-w   0.1)) )
    (cup              (:instance (cup-IHC       1)) )
    (saucer           (:instance (saucer-IHC    1)) )
    (immersion-heater (:instance (imm-htr-IHC   5))
                      (:a-link   (T-of-IHC-ih  0.2)) )
    (temperature-of   (:instance (T-of-IHC-w    1) (T-of-IHC-ih   2))
                      (:a-link   (high-T-IHC   0.1)) )
    (high-temp        (:instance (high-T-IHC    2))
                      (:a-link   (imm-htr-IHC  0.1)) )
    (in               (:instance (in-IHC-iw     1) (in-IHC-wc     1)) )
    (on               (:instance (on-IHC        1)) )
    (made-of          (:instance (made-of-IHC   1)) )
    (material-china   (:instance (mchina-IHC    1)) )
))


;;;;;;;;  Propositional representaion
;;
;;  sit-IHC       :  (inst-of sit-IHC situation)
;;
;;  cause-IHC     :  (cause initst-IHC T-of-IHC-w)
;;  cup-IHC       :  (inst-of cup-IHC cup)
;;  endst-IHC     :  (end-state T-of-IHC-w)
;;  follows-IHC   :  (follows initst-IHC endst-IHC)
;;  goalst-IHC    :  (goal-state T-of-IHC-w high-T-IHC water-IHC)
;;  high-T-IHC    :  (inst-of high-T-IHC high-temp)
;;  imm-htr-IHC   :  (inst-of imm-htr-IHC immersion-heater)
;;  in-IHC-iw     :  (in imm-htr-IHC water-IHC)
;;  in-IHC-wc     :  (in water-IHC cup-IHC)
;;  initst-IHC    :  (init-state T-of-IHC-ih high-T-IHC in-IHC-iw imm-htr-IHC)
;;  made-of-IHC   :  (made-of cup-IHC mchina-IHC)
;;  mchina-IHC    :  (inst-of mchina-IHC material-china)
;;  on-IHC        :  (on saucer-IHC cup-IHC)
;;  saucer-IHC    :  (inst-of saucer-IHC saucer)
;;  T-of-IHC-ih   :  (temperature-of imm-htr-IHC high-T-IHC)
;;  T-of-IHC-w    :  (temperature-of water-IHC high-T-IHC)
;;  to-reach-IHC  :  (to-reach initst-IHC goalst-IHC)
;;  water-IHC     :  (inst-of water-IHC water)


;;;;;;  End of file  AMBR/KB/EPISODIC/B_IHC.LSP
