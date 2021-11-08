;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/episodic/b_ICF.lsp
;;; VERSION:    3.0.0   ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Base situation ICF -- 'Ice Cube in a glass in a Fridge.'
;;; DEPENDS-ON: AMBR, AMBR/kb/semantic/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; VARIANTS:   none
;;; CREATED:    31-05-98 [3.0.0]  Elaboration of SIT-ICF from old LTM.LSP.
;;; UPDATED:    18-06-98  Removed IS-GOAL and RESULT propositns. Wght adjustmt.
;;;                       CAUSE consequents are propositions now, not states.
;;;                       T-OF-ICF-I1 and T-OF-ICF-I2 coalesced together.
;;;                       IN-ICF-IG changed into ON-ICF-IG (i.e. IN --> ON).
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;          SITUATION  I C F           ;;;;;;;;
         ;;;;;;;;   Ice Cube in a glass in a Fridge   ;;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;;;;;;;   Base situation ICF   ;;;;;;;;;;;
;;;;
;;;; There is an ice cube ON a glass in a fridge.
;;;; The fridge is cold.  The glass is made of
;;;; material-glass.
;;;;
;;;; The goal is that the temperature of the ice
;;;; cube becomes (or stays?) low.
;;;;
;;;; Since the ice cube is on the glass which, in turn,
;;;; is in the fridge, the cube is in the fridge too.
;;;; This causes the ice cube to become (or stay) cold.
;;;;
;;;;;;;;;
;;;; Related situations:
;;;;  + MTF -- Milk in a Teapot in a Fridge.   ; isomorphic
;;;;  + BPF -- Butter in a Plate in a Fridge.  ; isomorphic
;;;;  + FDO -- Food in a Dish in an Oven.      ; isomorphic
;;;;  + ...
;;;;  * ...


;;;;;;;  Situation-agent
;;

(defagent   sit-ICF    instance-agent
  "Ice Cube in a glass in a Fridge."
  :type      (:instance :situation)
  :inst-of   (situation 0.1)
  :a-link    (ice-cube-ICF 1.0)
)


;;;;;;;  Participating objects
;;
;;  ice-cube-ICF :  (inst-of ice-cube)
;;  glass-ICF    :  (inst-of glass)
;;  fridge-ICF   :  (inst-of fridge)
;;

(defagent   ice-cube-ICF    instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal)
  :situation  (sit-ICF 0.5)
  :inst-of    ice-cube
  :c-coref    (((T-of-ICF-i . :slot1) 0.5)
               ((on-ICF-ig  . :slot2) 0.2)
               ((in-ICF-if  . :slot1) 0.1)
               ((goalst-ICF . :slot3) 0.1) )
  :a-link     (initst-ICF-1 0.2)
)

(defagent   glass-ICF    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-ICF 0.2)
  :inst-of    glass
  :c-coref    (((on-ICF-ig   . :slot1) 0.5)
               ((in-ICF-gf   . :slot1) 0.5)
               ((made-of-ICF . :slot1) 0.1) )
  :a-link     (initst-ICF-1 0.1)
  :slot1
    :type       :aspect
    :inst-of    (glass . :slot1)
    :c-coref    (made-of-ICF 0.1)
    :a-link     (mglass-ICF  0.1)
)

(defagent   fridge-ICF    instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-ICF 0.2)
  :inst-of    fridge
  :c-coref    (((T-of-ICF-f  . :slot1) 0.75)
               ((in-ICF-gf   . :slot2) 0.25)
               ((in-ICF-if   . :slot2) 0.10)
               ((interst-ICF . :slot3) 0.10) )
  :a-link     ((low-T-ICF    0.5)
               (initst-ICF-1 0.1) )
)


;;;;;;;  Initial relations
;;
;;  on-ICF-ig    : (on glass-ICF ice-cube-ICF)
;;  in-ICF-gf    : (in glass-ICF fridge-ICF)
;;  T-of-ICF-f   : (temperature-of fridge-ICF low-T-ICF)
;;  made-of-ICF  : (made-of glass-ICF mglass-ICF)
;;

(defagent   on-ICF-ig    instance-agent
  "(on glass-ICF ice-cube-ICF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ICF 0.2)
  :inst-of    on
  :c-coref    (((initst-ICF-1 . :slot3) 0.2)
               ((initst-ICF-2 . :slot1) 0.1) )
  :a-link     ((in-ICF-gf   0.2)
               (in-ICF-if   0.1)
               (cause-ICF-i 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (on . :slot1)
    :c-coref    glass-ICF
  :slot2
    :type       :aspect
    :inst-of    (on . :slot2)
    :c-coref    ice-cube-ICF
)

(defagent   in-ICF-gf    instance-agent
  "(in glass-ICF fridge-ICF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ICF 0.2)
  :inst-of    in
  :c-coref    (((initst-ICF-1 . :slot4) 0.2)
               ((initst-ICF-2 . :slot2) 0.1) )
  :a-link     ((in-ICF-if 0.1)
               (on-ICF-ig 0.3) )
  :slot1
    :type       :aspect
    :inst-of    (in . :slot1)
    :c-coref    glass-ICF
  :slot2
    :type       :aspect
    :inst-of    (in . :slot2)
    :c-coref    fridge-ICF
)

(defagent   T-of-ICF-f    instance-agent
  "(temperature-of fridge-ICF low-T-ICF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ICF 0.3)
  :inst-of    temperature-of
  :c-coref    (((initst-ICF-1 . :slot1) 0.2)
               ((interst-ICF  . :slot2) 0.1) )
  :a-link     ((T-of-ICF-i  0.2)
               (cause-ICF-t 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    fridge-ICF
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    low-T-ICF
)
(defagent  low-T-ICF   instance-agent
  :type       (:instance  :object)
  :modality   (:init :goal :result)
  :situation  (sit-ICF 0.3)
  :inst-of    low-temp
  :c-coref    (((T-of-ICF-f   . :slot2) 0.5)
               ((T-of-ICF-i   . :slot2) 0.5)
               ((initst-ICF-1 . :slot2) 0.1)
               ((goalst-ICF   . :slot2) 0.1) )
  :a-link     (fridge-ICF 0.5)
)

(defagent   made-of-ICF    instance-agent
  "(made-of glass-ICF mglass-ICF)"
  :type       (:instance  :relation)
  :modality   :init
  :situation  (sit-ICF 0.1)
  :c-coref    (glass-ICF . :slot1)
  :inst-of    made-of
  :slot1
    :type     :aspect
    :inst-of  (made-of . :slot1)
    :c-coref  glass-ICF
  :slot2
    :type     :aspect
    :inst-of  (made-of . :slot2)
    :c-coref  mglass-ICF
)
(defagent   mglass-ICF  instance-agent
  :type       (:instance  :object)
  :modality   :init
  :situation  (sit-ICF 0.1)
  :inst-of    material-glass
  :c-coref    (made-of-ICF . :slot2)
  :a-link     (glass-ICF 1.0)
)


;;;;;;;  Initial states
;;
;;  initst-ICF-1  -to-reach->  goalst-ICF
;;  initst-ICF-1  -follows->   endst-ICF
;;  initst-ICF-2  --cause-->   in-ICF-if
;;
;;  initst-ICF-1  :  (init-state T-of-ICF-f low-T-ICF on-ICF-ig in-ICF-gf)
;;  initst-ICF-2  :  (init-state on-ICF-ig in-ICF-gf)
;;

(defagent   initst-ICF-1  instance-agent
  "initst-ICF-1  -to-reach->  goalst-ICF"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-ICF 0.2)
  :inst-of    init-state
  :c-coref    ((to-reach-ICF . :slot1)
               (follows-ICF  . :slot1) )
  :a-link     ((goalst-ICF   1.0)
               (initst-ICF-2 0.2)
               (ice-cube-ICF 0.2)
               (glass-ICF    0.2)
               (fridge-ICF   0.2) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    T-of-ICF-f
  :slot2
    :type       :aspect
    :inst-of    (init-state . :slot1)
    :c-coref    low-T-ICF
  :slot3
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-ICF-ig
  :slot4
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-ICF-gf
)

(defagent   initst-ICF-2  instance-agent
  "initst-ICF-2  -cause->  in-ICF-if"
  :type       (:instance :situation)
  :modality   :init
  :situation  (sit-ICF 0.2)
  :inst-of    init-state
  :c-coref    (cause-ICF-i . :slot1)
  :a-link     ((interst-ICF  1.0)
               (initst-ICF-1 0.3)
               (ice-cube-ICF 0.3) )
  :slot1
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    on-ICF-ig
  :slot2
    :type       :relation
    :inst-of    (init-state . :slot2)
    :c-coref    in-ICF-gf
)


;;;;;;;  Goal state
;;
;;  goalst-ICF  <-to-reach-  initst-ICF-1
;;  goalst-ICF   :  (goal-state T-of-ICF-i low-T-ICF ice-cube-ICF)
;;
;;  T-of-ICF-i   :  (temperature-of ice-cube-ICF low-T-ICF)
;;  to-reach-ICF :  (to-reach initst-ICF-1 goalst-ICF)
;;

(defagent   T-of-ICF-i    instance-agent
  "(temperature-of ice-cube-ICF low-T-ICF)"
  :type       (:instance  :relation)
  :modality   (:GOAL   :intend-true
               :RESULT :true )
  :situation  (sit-ICF 0.3)
  :inst-of    temperature-of
  :c-coref    (((cause-ICF-t . :slot2) 0.2)
               ((goalst-ICF  . :slot1) 0.2)
               ((endst-ICF   . :slot1) 0.2) )
  :a-link     ((T-of-ICF-f 0.2)
               (in-ICF-if  0.1)
               (fridge-ICF 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (temperature-of . :slot1)
    :c-coref    ice-cube-ICF
  :slot2
    :type       :aspect
    :inst-of    (temperature-of . :slot2)
    :c-coref    low-T-ICF
)

(defagent   goalst-ICF    instance-agent
  "goalst-ICF  <-to-reach-  initst-ICF-1"
  :type       (:instance :situation)
  :modality   :goal
  :situation  (sit-ICF 0.2)
  :inst-of    goal-state
  :c-coref    (to-reach-ICF . :slot2)
  :a-link     ((initst-ICF-1 0.5)
               (endst-ICF    0.2) )
  :slot1
    :type       :relation
    :inst-of    (goal-state . :slot2)
    :c-coref    T-of-ICF-i
  :slot2
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    low-T-ICF
  :slot3
    :type       :aspect
    :inst-of    (goal-state . :slot1)
    :c-coref    ice-cube-ICF
)

(defagent   to-reach-ICF    instance-agent
  "(to-reach initst-ICF-1 goalst-ICF)"
  :type       (:instance  :relation)
  :modality   :goal
  :situation  (sit-ICF 0.2)
  :inst-of    to-reach
  :a-link     ((follows-ICF 0.5)
               (cause-ICF-i 0.1)
               (cause-ICF-t 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (to-reach . :slot1)
    :c-coref    initst-ICF-1
  :slot2
    :type       :aspect
    :inst-of    (to-reach . :slot2)
    :c-coref    goalst-ICF
)


;;;;;;;  Intermediary state
;;
;;  interst-ICF  -cause->  I-of-ICF-i
;;  interst-ICF   :  (inter-state in-ICF-if T-of-ICF-f fridge-ICF)
;;
;;  in-ICF-if     :  (in ice-cube-ICF fridge-ICF)
;;  cause-ICF-i   :  (cause initst-ICF-2 in-ICF-if)
;;

(defagent   in-ICF-if    instance-agent
  "(in ice-cube-ICF fridge-ICF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-ICF 0.2)
  :inst-of    in
  :c-coref    (((cause-ICF-i . :slot2) 0.5)
               ((interst-ICF  .:slot1) 0.3) )
  :a-link     ((on-ICF-ig 0.3)
               (in-ICF-gf 0.2) )
  :slot1
    :type      :aspect
    :inst-of   (in . :slot1)
    :c-coref   ice-cube-ICF
  :slot2
    :type      :aspect
    :inst-of   (in . :slot2)
    :c-coref   fridge-ICF
)

(defagent   interst-ICF   instance-agent
  "interst-ICF  -cause->  I-of-ICF-i"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-ICF 0.2)
  :inst-of    inter-state
  :c-coref    (cause-ICF-t . :slot1)
  :a-link     ((endst-ICF    0.6)
               (initst-ICF-2 0.4)
               (cause-ICF-i  0.2) )
  :slot1
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    in-ICF-if
  :slot2
    :type       :relation
    :inst-of    (inter-state . :slot2)
    :c-coref    T-of-ICF-f
  :slot3
    :type       :aspect
    :inst-of    (inter-state . :slot1)
    :c-coref    fridge-ICF
)

(defagent   cause-ICF-i   instance-agent
  "(cause initst-ICF-2 in-ICF-if)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-ICF 0.2)
  :inst-of    cause
  :a-link     ((interst-ICF  1.0)
               (cause-ICF-t  0.5)
               (follows-ICF  0.3)
               (to-reach-ICF 0.2) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    initst-ICF-2
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    in-ICF-if
)


;;;;;;;  End state
;;
;;  endst-ICF  <-follows-  initst-ICF-1
;;  endst-ICF     :  (end-state T-of-ICF-i)
;;
;;  follows-ICF   :  (follows initst-ICF-1 endst-ICF)
;;  cause-ICF-t   :  (cause interst-ICF T-of-ICF-i)
;;

(defagent   endst-ICF    instance-agent
  "endst-ICF  <-follows-  initst-ICF-1"
  :type       (:instance :situation)
  :modality   :result
  :situation  (sit-ICF 0.2)
  :inst-of    end-state
  :c-coref    (follows-ICF . :slot2)
  :a-link     ((interst-ICF 0.1)
               (goalst-ICF  0.2)
               (cause-ICF-t 0.1) )
  :slot1
    :type       :relation
    :inst-of    (end-state . :slot2)
    :c-coref    T-of-ICF-i
)

(defagent   follows-ICF   instance-agent
  "(follows initst-ICF-1 endst-ICF)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-ICF 0.2)
  :inst-of    follows
  :a-link     ((to-reach-ICF 0.5)
               (cause-ICF-t  0.2) )
  :slot1
    :type       :aspect
    :inst-of    (follows . :slot1)
    :c-coref    initst-ICF-1
  :slot2
    :type       :aspect
    :inst-of    (follows . :slot2)
    :c-coref    endst-ICF
)

(defagent   cause-ICF-t    instance-agent
  "(cause interst-ICF T-of-ICF-i)"
  :type       (:instance  :relation)
  :modality   :result
  :situation  (sit-ICF 0.2)
  :inst-of    cause
  :a-link     ((endst-ICF    0.5)
               (follows-ICF  0.3)
               (cause-ICF-i  0.2)
               (to-reach-ICF 0.1) )
  :slot1
    :type       :aspect
    :inst-of    (cause . :slot1)
    :c-coref    interst-ICF
  :slot2
    :type       :aspect
    :inst-of    (cause . :slot2)
    :c-coref    T-of-ICF-i
)


;;;;;;  ---- Sanity check ---- ;;;;;;
;;
(check-for-unresolved-references)


;;;;;;;;;;;;  ---  Appendix  ---  ;;;;;;;;;;
;;

(defcoalition  sit-ICF
  "Ice Cube in a glass in a Fridge."
  :head      sit-ICF                      ; 21 agents
  :members   (sit-ICF
              ice-cube-ICF  glass-ICF     fridge-ICF
              on-ICF-ig     in-ICF-gf     in-ICF-if
              T-of-ICF-f    T-of-ICF-i    low-T-ICF
              made-of-ICF   mglass-ICF
              initst-ICF-1  initst-ICF-2  interst-ICF
              goalst-ICF    endst-ICF
              to-reach-ICF  follows-ICF
              cause-ICF-i   cause-ICF-t
             ))

(GENKB-template
  :herald  "Base sit.ICF -- Ice Cube in a glass in a Fridge, ver.3.0.0."
  :templates '(
    (ice-cube       (:instance (ice-cube-ICF 5))
                    (:a-link   (T-of-ICF-i  0.3)) )
    (glass          (:instance (glass-ICF    1)) )
    (fridge         (:instance (fridge-ICF   1))
                    (:a-link   (T-of-ICF-f  0.1)) )
    (temperature-of (:instance (T-of-ICF-i   1) (T-of-ICF-f 1)) )
    (low-temp       (:instance (low-T-ICF    2))
                    (:a-link   (fridge-ICF  0.1)) )
    (on             (:instance (on-ICF-ig    1)) )
    (in             (:instance (in-ICF-gf    1)) )
    (made-of        (:instance (made-of-ICF  1)) )
    (material-glass (:instance (mglass-ICF   1)) )
))


;;;;;;;;  Propositional representaion
;;
;;  sit-ICF       :  (inst-of sit-ICF situation)
;;
;;  cause-ICF-i   :  (cause initst-ICF-2 in-ICF-if)
;;  cause-ICF-t   :  (cause interst-ICF  T-of-ICF-i)
;;  endst-ICF     :  (end-state T-of-ICF-i)
;;  follows-ICF   :  (follows initst-ICF-1 endst-ICF)
;;  fridge-ICF    :  (inst-of fridge-ICF fridge)
;;  glass-ICF     :  (inst-of glass-ICF glass)
;;  goalst-ICF    :  (goal-state T-of-ICF-i low-T-ICF ice-cube-ICF)
;;  ice-cube-ICF  :  (inst-of ice-cube-ICF ice-cube)
;;  initst-ICF-1  :  (init-state T-of-ICF-f low-T-ICF on-ICF-ig in-ICF-gf)
;;  initst-ICF-2  :  (init-state on-ICF-ig in-ICF-gf)
;;  interst-ICF   :  (inter-state in-ICF-if T-of-ICF-f fridge-ICF)
;;  in-ICF-if     :  (in ice-cube-ICF fridge-ICF)
;;  on-ICF-ig     :  (on ice-cube-ICF glass-ICF)
;;  in-ICF-gf     :  (in glass-ICF fridge-ICF)
;;  low-T-ICF     :  (inst-of low-T-ICF low-temp)
;;  made-of-ICF   :  (made-of glass-ICF mglass-ICF)
;;  mglass-ICF    :  (inst-of mglass-ICF material-glass)
;;  T-of-ICF-f    :  (temperature-of fridge-ICF low-T-ICF)
;;  T-of-ICF-i    :  (temperature-of ice-cube-ICF low-T-ICF)
;;  to-reach-ICF  :  (to-reach initst-ICF-1 goalst-ICF)


;;;;;;  End of file  AMBR/KB/EPISODIC/B_ICF.LSP
