;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       ambr/defs.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Global variables used by the AMBR model.
;;; DEPENDS-ON: DUAL; AMBR/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    08-05-97 [2.0]
;;; UPDATED:    21-12-97 [2.1]
;;; UPDATED:    04-04-98 [2.2.1]
;;; UPDATED:    10-05-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...
;;;;;;
;;; TO DO:      Document everything


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;    GLOBAL  DEFINITIONS  FOR   A M B R 2    ;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl:in-package  "AMBR-CORE")

;;;; SYMBOLS: *AMBR-max-act*, *AMBR-threshold*,
;;;;          *AMBR-excit-rate*, *AMBR-decay-rate*,
;;;;          *hypoth-max-act*, *hypoth-zero-act*, *hypoth-min-act*,
;;;;          *hypoth-excit-rate*, *hypoth-inhib-rate*, *hypoth-decay-rate*,
;;;;          *hypoth-output-factor* ;
;;;;
;;;;          *element->hypoth-weight*,
;;;;          *hypoth->element-weight*, *hypoth->situation-weight*,
;;;;          *contradict-hypoth-weight*, *hypoth->MP-mentor-weight*,
;;;;          *hypoth->SC1-mentor-weight*, *hypoth->SC2-mentor-weight*,
;;;;          *MP-mentor-homog-weight*,  *MP-mentor-heterog-weight*,
;;;;          *SC1-mentor-homog-weight*, *SC1-mentor-heterog-weight*,
;;;;          *SC2-mentor-homog-weight*, *SC2-mentor-heterog-weight*,
;;;;
;;;;          *element->winner-hypoth-weight*, *mapped-T-LINK-weight*,
;;;;          *element->loser-hypoth-weight*,
;;;;
;;;;          *Skolem-instance-weight*, *Skolem-proposition-weight*,
;;;;          *Skolem-INST-OF-weight*,  *Skolem-C-COREF-weight*,
;;;;          *Skolem-SITUATION-weight* ;
;;;;
;;;;          *marker-emission-flag*,
;;;;          *weak-SC-flag*,
;;;;          *skolemization-flag*,
;;;;          *loser-hypothesis-elimination-flag*
;;;;
;;;;          *number-of-state-SC-trials*, *state-SC-wait-period* ;
;;;;
;;;;          *positive-rating-factor*, *negative-rating-factor*,
;;;;          *rating-time-period*,
;;;;          *winner-rating*,  *lethal-rating*,  *skolem-rating*,
;;;;          *initial-rating*, *ballotage-rating*,
;;;;          *critical-winner-level*, *critical-loser-level*,
;;;;          *loser-protection-quota* ;
;;;;
;;;;          *number-of-node-constructors*,
;;;;          *default-input-capacity*, *default-goal-capacity*,
;;;;          *default-AMBR-efficiency*, *hypothesis-agent-efficiency*,
;;;;          *AMBR-symb/conn-ratio*,
;;;;          AMBR-time-slice, AMBR-symb-subcycles

;;;;;;;;;
;;;;  ***  See the function ENFORCE-AMBR-PARAMETERS in AMBR/TOPLEVEL.LSP  ***
;;;;;;;;;


;;;;  *****  Time-related parameters  ******
;;
;;  DUAL specification postulates that time in the architecture is continuous.
;;  For the purposes of the computer implementation, the time is discretized.
;;  The main time-related parameters are *TIME-SLICE* and *SYMB-SUBCYCLES*
;;  defined in DUAL/DEFS.LSP and used in DUAL/ARCHIT/SPREAD, =/AGENDA.LSP, etc.
;;
;;  Ideally, these parameters are infinitesimally small.  In practice, they
;;  should be small enough. (That is, making them even smaller should not change
;;  significantly the behavior of the model.)  Of course, the smaller they are,
;;  the slower the program iterates.
;;
;;  Experiments with AMBR showed that *TIME-SLICE* should be as small as 0.01
;;  in the beginning of the run and could be as large as 0.2 in the end.
;;  (The dynamics of the model is greater during the early stages of the run.)
;;  The inefficient (though simple) solution to keep *TIME-SLICE* constant
;;  throughout the run is avoided.  Instead, *TIME-SLICE* and *SYMB-SUBCYCLES*
;;  vary depending on the particular moment in time.
;;
;;  The functions below define fixed schedules for these parameters. Think of
;;  these functions as parameters themselves.
;;
;;  These parameters are put into effect by the function ENFORCE-TIME-PARAMETERS
;;  defined in AMBR/TOPLEVEL.LSP.  See also ENFORCE-AMBR-PARAMETERS.


(declaim (notinline AMBR-time-slice AMBR-symb-subcycles))

(defun  AMBR-time-slice ()
  "Determine the *TIME-SLICE* for the moment. Think of that fun as parameter."
  (declare (values float))
  (cond ((< *time*   4.999)  0.02 )      ;  2    ; 0.01
        ((< *time*  19.999)  0.05 )      ;  5    ; 0.01
     ;; ((< *time*  49.999)  0.1  )      ; 10    ; 0.01
        ((< *time*  99.999)  0.1  )      ;  5    ; 0.02
        (t                   0.2  ) ))   ; 10    ; 0.02

(defun  AMBR-symb-subcycles ()
"Determine the *SYMB-SUBCYCLES* for the moment. Think of that fun as parameter."
  (declare (values integer))
  (cond ((< *time*   4.999)   2 )        ; 0.02  ; 0.01
        ((< *time*  19.999)   5 )        ; 0.05  ; 0.01
        ((< *time*  49.999)  10 )        ; 0.1   ; 0.01
        ((< *time*  99.999)   5 )        ; 0.1   ; 0.02
        (t                   10 ) ))     ; 0.2   ; 0.02



;;;;  ***** Parameters related to the connectionist machinery ******
;;
;;  (Note: Some parameters are defined as constants for efficiency reasons.)
;;
;;  Put into effect by ENFORCE-AMBR-PARAMETERS defined in AMBR/TOPLEVEL.LSP.

(defparameter  *AMBR-max-act*  5.0
  "Maximal activation level -- the 'ceiling' of DUAL-ACT-FUN." )

(defparameter  *AMBR-threshold*  0.09
  "Minimal activation needed for participation in working memory." )

(defparameter  *AMBR-excit-rate*  0.5
  "Excitation parameter used in CONCEPT- and INSTANCE-AGENTs." )

(defparameter  *AMBR-decay-rate*  1.85   ; 1.8
  "Decay parameter used in CONCEPT- and INSTANCE-AGENTs." )

(defconstant  *default-input-capacity*  30.0
  "Used to initialize the WM-ACT-SOURCE of *INPUT* working memory." )

(defconstant  *default-goal-capacity*  20.0
  "Used to initialize the WM-ACT-SOURCE of *GOAL* working memory." )

(declaim (type float *AMBR-max-act*    *AMBR-threshold*    ; assist
                     *AMBR-excit-rate* *AMBR-decay-rate*   ; compiler
                     *default-goal-capacity*               ; optimization
                     *default-input-capacity* ))



;;;;  ***** Parameters related to the constraint satisfaction net *****
;;
;;  See AMBR/HYPOTH.LSP and in particular HYPOTH-ACT-FUN, ENCODE-HYPOTH-
;;  ACTIVATION, and DECODE-HYPOTH-ACTIVATION.
;;  (Note: Some parameters are defined as constants for efficiency reasons.)
;;

;;;; Related to HYPOTH-ACT-FUN
(defparameter  *hypoth-max-act*  2.0    ; in the coded-activation scale
  "Maximal act. level for hypotheses -- the 'ceiling' of HYPOTH-ACT-FUN." )

(defparameter  *hypoth-zero-act*  0.8
  "Neutral act. level for hypotheses -- the 'zero' of HYPOTH-ACT-FUN." )

(defparameter  *hypoth-min-act*  -0.8   ; in the coded-activation scale
  "Minimal act. level for hypotheses -- the 'floor' of HYPOTH-ACT-FUN." )

(defparameter  *hypoth-excit-rate*  0.2
  "Excitation parameter used in the positive branch of HYPOTH-ACT-FUN." )

(defparameter  *hypoth-inhib-rate*  0.3
  "Inhibition parameter used in the negative branch HYPOTH-ACT-FUN." )

(defparameter  *hypoth-decay-rate*  0.1
  "Decay parameter used in HYPOTHESIS-AGENTs." )

(defparameter  *hypoth-output-factor* 1.0
  "Coefficient in HYPOTH-OUTPUT-FUN." )

(declaim (type single-float *hypoth-max-act* *hypoth-zero-act* *hypoth-min-act*
                            *hypoth-excit-rate* *hypoth-inhib-rate*
                            *hypoth-decay-rate* *hypoth-output-factor* ))

;;;; Link weights

(defparameter *element->hypoth-weight*  0.1
  "Weight of the link from an entity-agent to a hypothesis-agent." )

(defparameter *hypoth->element-weight*  0.3  ; 0.2
  "Weight of the links from a hypothesis-agent to its elements." )

(defparameter *hypoth->situation-weight*  0.1  ; 0.05
  "Weight of the link from a hypothesis-agent to its driver situation." )

(defparameter *hypoth->MP-mentor-weight*   0.4  ; 0.3
  "Weight of the link from a hypoth to its MP-mentor (which is a concept)." )

(defparameter *hypoth->SC1-mentor-weight*  0.3
  "Weight of the link from a hypoth to its SC1-mentor (which is a hypoth)." )

(defparameter *hypoth->SC2-mentor-weight*  0.2
  "Weight of the link from a hypoth to its SC2-mentor (which is a hypoth)." )


(defparameter *contradict-hypoth-weight*  -0.3
  "Weight of the link between contradictory hypotheses. (1-1 constraint.)" )


(defparameter *MP-mentor-homog-weight*  0.1
  "Weight of the link from a concept-agent to a 'homogeneous' hypothesis." )

(defparameter *MP-mentor-heterog-weight*  0.05   ; not used in current version
  "Weight of the link from a concept-agent to a 'heterogeneous' hypothesis." )

(defparameter *SC1-mentor-homog-weight*  0.3
  "Weight of the link from a hypoth to its SC1-generated 'homogeneous' hyps." )

(defparameter *SC1-mentor-heterog-weight*  0.2   ; not used in current version
  "Weight of the link from a hypoth to its SC1-generated 'heterogeneous' hyps.")

(defparameter *SC2-mentor-homog-weight*  0.2
  "Weight of the link from a hypoth to its SC2-generated 'homogeneous' hyps." )

(defparameter *SC2-mentor-heterog-weight*  0.1   ; not used in current version
  "Weight of the link from a hypoth to its SC2-generated 'heterogeneous' hyps.")

;; See HOMOGENEOUS-STATES-P in AMBR/KREPRES.LSP for HOMOG-... vs. HETEROG-...


(defparameter *element->winner-hypoth-weight*  0.5     ; see AMBR/PROMOTN.LSP
  "Weight of the link from an entity-agent to a winner-hypothesis agent." )

(defparameter *element->loser-hypoth-weight*  0.05     ; see AMBR/PROMOTN.LSP
  "Weight of the link from an entity-agent to a loser hypothesis agent." )

(defparameter *mapped-T-LINK-weight*  1.0              ; see AMBR/PROMOTN.LSP
  "Weight of the T-LINKs between mapped elements." )


;;;;;;;;; Backward compatibility note
;; Previous versions (<= 2.2.2) used other link parameters.  Some of them
;; are abandoned now.  The table below gives the correspondence b/n abandoned
;; and current parameters.  The old version also made no difference b/n homo-
;; and heterogeneous mentor weights.
;;
;; *entity->hypoth-weight*  0.1 -- *element->hypoth-weight*, *MP-mentor-...
;; *hypoth->entity-weight*  0.2 -- *hypoth->element-weight*, *hypoth->MP-mentor.
;; *coherent-hypoth-weight* 0.3 -- *SC1-mentor-xxx-weight*, *hypoth->SC1-mentor.
;;;;;;;;;


;;;;  *****  Flags disabling certain mechanisms  *****
;;
(defvar  *marker-emission-flag*  T
  "Flag that controls whether intance agents emit markers when entering WM." )

(defvar  *weak-SC-flag* t      ; see AMBR/WEAK_SC.LSP
  "Flag controling the structure-correspondence mechanism for state hypoth." )

;; See also *skolemization-flag* and *loser-hypothesis-elimination-flag* below.


;;;;  *** Parameters related to 'weak' structure-correspondence ***
;;
;;  See files AMBR/WEAK_SC.LSP for details.

(defparameter  *number-of-state-SC-trials*  3   ; give up after 4 failures
  "How many failures can the state-SC mechanism tolerate?" )

(defparameter  *state-SC-wait-period*  10.0
  "Time interval between two consecutive state-SC trials." )


;;;;  *****  Parameters related to the skolemization mechanism  *****
;;
;;  See files AMBR/SKOLEM.TXT and =.LSP for details on the skolemization mech.

(defvar  *skolemization-flag*  t
"Flag that controls whether secretaries send Skolem incentives to hypotheses.")

(defparameter *Skolem-instance-weight*  1.0
  "Weight of the link from a concept- or prototype-agent to a Skolem instance.")

(defparameter *Skolem-proposition-weight*  1.0
  "Weight of the link from a general proposition to a Skolem proposition.")

(defparameter *Skolem-INST-OF-weight*  1.0
  "Weight of the INST-OF link of Skolem instances." )

(defparameter *Skolem-C-COREF-weight*  1.0
  "Weight of the C-COREF link of Skolem instances." )

(defparameter *Skolem-SITUATION-weight*  0.2
  "Weight of the SITUATION link of Skolem instances." )



;;;;  *****  Parameters related to the rating mechanism  *****
;;
;;  See file AMBR/RATING.LSP for details on the rating mechanism.

(defvar  *loser-hypothesis-elimination-flag*  t
  "Is the rating mechanism allowed to eliminate 'loser' hypotheses?" )
  ;; *skolemization-flag* is also relevant to the rating mechanism.


(defparameter  *rating-time-period*  5.0
  "Interval between two successive 'rating surveys'." )

(defparameter  *winner-rating*   1.0     ; see *critical-winner-level* below
  "The rating that a hypothesis must achieve to be considered for promotion." )

(defparameter  *lethal-rating*   0.0     ; see *critical-loser-level* below
  "The rating at which the hypothesis is considered for elimination." )

(defparameter  *skolem-rating*   0.6
 "The rating that a general hyp must achieve to be considered for skolemizatn.")

(defparameter  *initial-rating*  0.4
  "The level at which new hypotheses enter the rating competition." )

(defparameter *ballotage-rating* 0.7
  "The starting level for repeated rating competitions." )


(defparameter *critical-winner-level*       ; see also *winner-rating* above
              (* 1.25 *hypoth-zero-act*)    ; somewhat above the neutral level
  "Minimal activation level for considering a hypothesis for promotion." )

(defparameter *critical-loser-level*        ; see also *lethal-rating* above
              (/ (+ *AMBR-threshold* *hypoth-zero-act*)
                 2.0)                       ; halfway b/n floor and neutral
  "Critical activation level for eliminating a loser hypothesis." )

(defparameter *loser-protection-quota*  2
  "How many loser hypotheses are retained after the winner is promoted." )


(defparameter  *positive-rating-factor*
               (/ (- *winner-rating*  *initial-rating* )
                  (- *hypoth-max-act* *hypoth-zero-act*)
                  5 )   ; under best conditions, promote a winner for approx. 5
                        ; rating periods; under normal conds -- for appx. 20-50
  "Controls the transition from *initial-rating* to *winner-rating*." )

(defparameter  *negative-rating-factor*
               (/ *positive-rating-factor*
                  -1.5 )     ; kill losers more slowly than promote winners
  "Controls the transition from *initial-rating* to *lethal-rating*." )



;;;;  *****  Parameters related to symbolic processing  *****
;;
;;  (Note: Some parameters are defined as constants for efficiency reasons.)
;;

(defconstant  *number-of-node-constructors*  10
  "The number of node constructors in AMBR." )   ; see AMBR/TOPLEVEL.LSP


(defconstant  *default-AMBR-efficiency* 0.5
  "The efficiency coefficient of ordinary AMBR agents." )

(defconstant  *hypothesis-agent-efficiency* 0.75
  "The efficiency coefficient of hypothesis-agents in AMBR." )

(defparameter  *AMBR-symb/conn-ratio*  1.0
  "The ratio of symbolic and connectionist speed of processing." )

(declaim (type single-float *default-AMBR-efficiency*       ; help the compiler
                            *hypothesis-agent-efficiency*
                            *AMBR-symb/conn-ratio*  ))


;;;;;;;  End of file AMBR/DEFS.LSP
