;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/proclaim.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Proclamations of architecture parameters.
;;; DEPENDS-ON: DUAL; ambr/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    09-04-97 [2.0]
;;; UPDATED:    21-12-97 [2.1.0]
;;; UPDATED:    04-02-98 [2.1.1]
;;; UPDATED:    12-02-98 [2.2.0]
;;; UPDATED:    08-03-98 [2.2.1]
;;; UPDATED:    01-05-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;     P A R A M E T E R   P R O C L A M A T I O N S     ;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package  "AMBR-CORE")

;;;; This file proclaims a number of parameters that control the operation
;;;; of the AMBR model.  These proclamations complement (and possibly shadow)
;;;; the proclamations from DUAL/LABELS.LSP and DUAL/CONSUM.LSP.
;;;; See DUAL/PROCLAIM.LSP for documentation of the proclamation facilities.
;;;; See DUAL/ARCHIT/BRACKETS.LSP for details on consumption proclamations.


;;;;  ********   PROCLAMATIONS OF LABELS, LINKS, AND TAGS  **********
;;
;;  See DUAL/LABELS.LSP

;;;;;;  Adding more labels to the basic repertoire (see DUAL/LABELS.LSP)
;;
(proclaim-label :corr      :G-slot :reference )
(proclaim-label :hypoth    :G-slot :reference )
(proclaim-label :situation :G-slot :reference )
(proclaim-label :peer      :G-slot :tag       )
(proclaim-label :modality  :G-slot :tag       )


;;;;;;  Defining new link types (see DUAL/LABELS.LSP)
;;
(proclaim-link :situation :default-weight nil )   ; defaulting forbidden
(proclaim-link :corr      :default-weight nil )   ; for (permanent) corr-agents
(proclaim-link :hypoth    :default-weight nil     ; for hypoth-corr-agents
                          :temporary-p t )


;;;;;;  Defining new tags (see DUAL/LABELS.LSP)
;;
(proclaim-tag :prototype  '((:G-slot :type)) )
(proclaim-tag :corr       '((:G-slot :type)) )
(proclaim-tag :hypoth     '((:G-slot :type)) )
(proclaim-tag :embryo     '((:G-slot :type)) )
(proclaim-tag :mature     '((:G-slot :type)) )
(proclaim-tag :winner     '((:G-slot :type)) )
(proclaim-tag :loser      '((:G-slot :type)) )

(proclaim-tag :situation  '((:G-slot :type)) )
(proclaim-tag :T-driver   '((:G-slot :type)) )   ; Target sit acting as driver
(proclaim-tag :B-driver   '((:G-slot :type)) )   ; Base sit acting as driver
(proclaim-tag :input      '((:G-slot :type)) )
(proclaim-tag :goal       '((:G-slot :type)
                            (:G-slot :modality)) )    ; member of a goal-state
(proclaim-tag :result     '((:G-slot :modality)) )    ; member of an end-state
(proclaim-tag :init       '((:G-slot :modality)) )    ; member of an init-state
(proclaim-tag :conjecture '((:G-slot :modality)) )
(proclaim-tag :inference  '((:G-slot :modality)) )
(proclaim-tag :true       '((:G-slot :modality)) )
(proclaim-tag :false      '((:G-slot :modality)) )
(proclaim-tag :intend-true  '((:G-slot :modality)) )
(proclaim-tag :intend-false '((:G-slot :modality)) )

(proclaim-tag :slot1      '((:facet  :peer)) )
(proclaim-tag :slot2      '((:facet  :peer)) )
(proclaim-tag :slot3      '((:facet  :peer)) )
(proclaim-tag :slot4      '((:facet  :peer)) )
(proclaim-tag :slot5      '((:facet  :peer)) )


;;;;  ***************   CONSUMPTION  PROCLAMATIONS  ********************
;;
;; ***************************************************************************
;; ***************************************************************************
;;  WARNING!!!  When DUAL-FLEXIBLE quality is off run-time changes  WARNING!!!
;;  WARNING!!!  in the 'consumption table' do not take effect.      WARNING!!!
;;  WARNING!!!  See DUAL/START_ME.LSP for more details.             WARNING!!!
;;  WARNING!!!                                                      WARNING!!!
;;  WARNING!!!  In particular, when DUAL-FLEXIBLE is off, the       WARNING!!!
;;  WARNING!!!  consumptions that are actually in effect are those  WARNING!!!
;;  WARNING!!!  proclaimed in DUAL/CONSUM.LSP, not here.            WARNING!!!
;;  WARNING!!!  If you use DUAL-FLEXIBLE off, make sure that _all_  WARNING!!!
;;  WARNING!!!  consumptions proclaimed in DUAL/CONSUM.LSP have the WARNING!!!
;;  WARNING!!!  same values as the ones defined here.               WARNING!!!
;;  WARNING!!!                                                      WARNING!!!
;;  WARNING!!!  Still,it was possible to achieve identical behavior WARNING!!!
;;  WARNING!!!  of the model in the two modes of DUAL-FLEXIBLE.     WARNING!!!
;; ***************************************************************************
;; ***************************************************************************

;;;;; For generic types of operations; used throughout.
;;

(defparameter *default-AMBR-READ-consumption*  0.02  ;  0.025 ; 0.03
   "The amount of energy needed for a transaction of type 'read'." )

(defparameter *default-AMBR-SEND-consumption*  0.1   ;  0.125 ; 0.15
   "The amount of energy needed for a transaction of type 'send'." )

(defparameter *default-AMBR-MODIFY-consumption*  0.3   ;  0.4 ; 0.5
   "The amount of energy needed for modifying the local micro-frame." )

(defparameter *default-AMBR-INTERNAL-consumption* 0.005  ; 0.01
   "The amount of energy needed for internal operations in the buffer." )

(defparameter *negligible-AMBR-consumption* 0.001 )

(defparameter *default-AMBR-make-agent-consumption*  2.0  ;  2.25 ; 2.0
   "The amount of energy needed for constructing a AMBR agent." )



;;;;;; ************  PROCLAMATIONS FROM DUAL/CONSUM.LSP  ************  ;;;;;;;
;;;;
;;;; These proclamations are repeated here in order to inforce AMBR settings
;;;; when they are inconsistent with default DUAL settings.
;;;;
;;;; WARNING!!!  This works only when DUAL-FLEXIBLE quality is on.  WARNING!!!
;;;; WARNING!!!  See DUAL/START_ME.LSP for the compil. qualities.   WARNING!!!

;;; For functions from DUAL/ARCHIT/CONNECT.LSP

(proclaim-consumption 'agent-visible-p  *negligible-AMBR-consumption* )

(proclaim-consumption 'agent-activation *default-AMBR-READ-consumption* )
(proclaim-consumption 'act              *default-AMBR-READ-consumption* )


;;; For functions from DUAL/ARCHIT/FILLER.LSP

(proclaim-consumption 'fholder-filler  *default-AMBR-INTERNAL-consumption* )
(proclaim-consumption 'set-filler      *default-AMBR-MODIFY-consumption* )

(proclaim-consumption 'add-filler-elt    *default-AMBR-MODIFY-consumption* )
(proclaim-consumption 'remove-filler-elt *default-AMBR-MODIFY-consumption* )


;;; For functions from DUAL/ARCHIT/MCRFRAME.LSP

(proclaim-consumption 'locate-mfr-component *default-AMBR-INTERNAL-consumption*)

(proclaim-consumption 'add-G-slot   *default-AMBR-MODIFY-consumption* )
(proclaim-consumption 'add-S-slot   *default-AMBR-MODIFY-consumption* )
(proclaim-consumption 'add-facet    *default-AMBR-MODIFY-consumption* )
(proclaim-consumption 'remove-slot  *default-AMBR-MODIFY-consumption* )
(proclaim-consumption 'remove-facet *default-AMBR-MODIFY-consumption* )


;;; For functions from DUAL/ARCHIT/LINKS.LSP

(proclaim-consumption 'get-filler       *default-AMBR-INTERNAL-consumption* )
(proclaim-consumption 'get-filler-refs! *default-AMBR-INTERNAL-consumption*)

(proclaim-consumption 'add-link     *default-AMBR-MODIFY-consumption* )
(proclaim-consumption 'remove-link  *default-AMBR-MODIFY-consumption* )


;;; For functions from DUAL/ARCHIT/SYMPROC2.LSP

(proclaim-consumption 'receive-symbol *default-AMBR-SEND-consumption* )
  ;; The agent that calls RECEIVE-SYMBOL acts as a sender in the transaction.

(proclaim-consumption 'trigger-symbolic-processor
                      (/ *default-AMBR-SEND-consumption* 5.0) )
  ;; The agent that calls TRIGGER-SYMBOLIC-PROCESSOR acts as a sender
  ;; in the transaction. The transaction itself is inexpensive, though.

(proclaim-consumption 'symbolic-microcycle :explicit-S-PROGN )
(proclaim-consumption 'handle-symbol       :explicit-S-PROGN )

(proclaim-consumption 'handle-next-input *default-AMBR-INTERNAL-consumption* )
  ;; This is the time needed to pop the input zone.
  ;; HANDLE-SYMBOL takes care of itself.


;;; For functions from DUAL/ARCHIT/SYMBOLIC.LSP

(proclaim-consumption 'send-write-request *default-AMBR-SEND-consumption* )
  ;; The sender consumes only as much as for a transaction of type 'send'.
  ;; The real work (and its consumption) will be carried out by the receiver.


;;; For functions from DUAL/ARCHIT/NC_AGENT.LSP

(proclaim-consumption 'recruit-node-constructor
                      *default-AMBR-read-consumption* )

(proclaim-consumption 'make-DUAL-agent *default-AMBR-make-agent-consumption* )

(proclaim-consumption 'dual-core::construct-new-agent 
          (+ *default-AMBR-INTERNAL-consumption*    ; for phase 0
             (get-consumption 'agent-activation)    ; for phase 1
             (get-consumption 'make-DUAL-agent)     ; for phase 2a
             (get-consumption 'add-link)) )         ; for phase 2c

(proclaim-consumption 'send-NC-request *default-AMBR-INTERNAL-consumption* )
;; The NC-request is not really sent. It is only stored in the NCR-queue and
;; will be sent (with due consumption) when there is a free node-constructor.


;;; For functions from DUAL/ARCHIT/MP_AGENT.LSP
;; See the section for AMBR/MARKER.LSP later in this file.

;;; No consumptions for DUAL/ARCHIT/TIME and =/METRONOM.LSP

;;;;
;;;;;; End of the general DUAL proclamations


;;;;;; ************  PROCLAMATIONS FOR AMBR FUNCTIONS  ************  ;;;;;;;
;;;;

;;;;; For functions from AMBR/AMBR_AG.LSP

(proclaim-consumption 'agent-buffer *default-AMBR-INTERNAL-consumption* )
  ;; The buffer is accessible only locally.


;;;;; For functions from AMBR/KREPRES.LSP

(proclaim-consumption 'agent-type      *default-AMBR-READ-consumption* )
(proclaim-consumption 'agent-modality  *default-AMBR-READ-consumption* )
(proclaim-consumption 'agent-situation *default-AMBR-READ-consumption* )

(proclaim-consumption 'homogeneous-states-p
                      (* 2.0 (get-consumption 'agent-modality)) )

(proclaim-consumption 'affiliate-agent (get-consumption 'add-link) )

(proclaim-consumption 'instance->concept *default-AMBR-INTERNAL-consumption* )

(proclaim-consumption 'get-slot-superordinates
                                *default-AMBR-INTERNAL-consumption* )

(proclaim-consumption 'proposition-args *default-AMBR-READ-consumption* )
(proclaim-consumption 'argument-props   *default-AMBR-READ-consumption* )

(proclaim-consumption 'range-slot-p *default-AMBR-INTERNAL-consumption* )

(proclaim-consumption 'general-proposition-p *default-AMBR-READ-consumption* )


;;;;; For functions from AMBR/FIZZLE.LSP

(proclaim-consumption 'fizzle-agent 0.0 )     ; It doesn't matter anyway.

(proclaim-consumption 'send-fizzle-message
                      (+ *default-AMBR-INTERNAL-consumption*   ; for making it
                         (get-consumption 'receive-symbol) ))  ; for sending it

(proclaim-consumption 'fizzle-message-sender
                      *default-AMBR-INTERNAL-consumption* )


;;;;; For functions from AMBR/CORRESP.LSP

(proclaim-consumption 'corresp-elt     *default-AMBR-READ-consumption* )
(proclaim-consumption 'corresp-justif  *default-AMBR-READ-consumption* )
(proclaim-consumption 'corresp-driver  *default-AMBR-READ-consumption* )

(proclaim-consumption 'corresp-type
                      (* 2.0 (get-consumption 'agent-type)) )

(proclaim-consumption 'add-corresp-justif *default-AMBR-MODIFY-consumption* )
(proclaim-consumption 'add-corresp-driver *default-AMBR-MODIFY-consumption* )

(proclaim-consumption 'trivial-corresp-p *default-AMBR-INTERNAL-consumption* )
(proclaim-consumption 'equivalent-corresp-p
                      (* 2.0 (get-consumption 'corresp-elt)) )


;;;;; For functions from AMBR/GOALINPT.LSP

(proclaim-consumption 'add-to-goal/input
                      *default-AMBR-MODIFY-consumption* )
(proclaim-consumption 'remove-from-goal/input
                      *default-AMBR-MODIFY-consumption* )

(proclaim-consumption 'T-driver-p *default-AMBR-READ-consumption* )
(proclaim-consumption 'B-driver-p *default-AMBR-READ-consumption* )

(proclaim-consumption 'add-T-DRIVER-tag (get-consumption 'add-filler-elt) )
(proclaim-consumption 'add-B-DRIVER-tag (get-consumption 'add-filler-elt) )


;;;;; For functions from AMBR/MARKER.LSP
;;
;; The first two groups of proclam's below relate to DUAL/ARCHIT/MP_AGENT.LSP.
;; The remaining proclamations relate to AMBR/MARKER.LSP proper.

(proclaim-consumption 'make-marker   *default-AMBR-INTERNAL-consumption* )
(proclaim-consumption 'copy-marker   *default-AMBR-INTERNAL-consumption* )
(proclaim-consumption 'store-marker  *default-AMBR-INTERNAL-consumption* )
(proclaim-consumption 'delete-marker *default-AMBR-INTERNAL-consumption* )

(proclaim-consumption 'preprocess-new-marker      :explicit-S-PROGN )
(proclaim-consumption 'go-through-old-markers     :explicit-S-PROGN )
(proclaim-consumption 'report-marker-intersection :explicit-S-PROGN )
(proclaim-consumption 'mark-MP-neighbors          :explicit-S-PROGN )
(proclaim-consumption 'send-T-marker              :explicit-S-PROGN )

(proclaim-consumption 'send-B-marker
                      (+ (get-consumption 'make-marker)        ; for making it
                         (get-consumption 'receive-symbol) ))  ; for sending it


(proclaim-consumption 'agent-markers *default-AMBR-READ-consumption* )

(proclaim-consumption 'marker-origin-visible-p
                      (get-consumption 'agent-visible-p) )

(proclaim-consumption 'complementary-markers-p
                      (+ *default-AMBR-INTERNAL-consumption*  ; for AND,EQL,etc.
                         (get-consumption 'T-driver-p)) )     ; for T-DRIVER-P

(proclaim-consumption 'equivalent-markers-p
                      *default-AMBR-INTERNAL-consumption* )

(proclaim-consumption 'make-MP-X-report
                      (* 2.0 *default-AMBR-INTERNAL-consumption*) )

(proclaim-consumption 'formulate-MP-NCR
                      (+ (get-consumption 'homogeneous-states-p)
                         (* 5.0 *default-AMBR-INTERNAL-consumption*)) )


;;;;; For functions from AMBR/SECRETAR.LSP
;;
;; There are a few explicit S-EVALs in the file too.

(proclaim-consumption 'send-HR-request
                      (+ *default-AMBR-INTERNAL-consumption*   ; for making it
                         (get-consumption 'receive-symbol) ))  ; for sending it

(proclaim-consumption 'send-secretary-answer
                      (+ *default-AMBR-INTERNAL-consumption*   ; for making it
                         (get-consumption 'receive-symbol) ))  ; for sending it

(proclaim-consumption 'HR-hypothesis    *default-AMBR-INTERNAL-consumption* )
(proclaim-consumption 'HR-slot          *default-AMBR-INTERNAL-consumption* )
(proclaim-consumption 'secretary-answer *default-AMBR-INTERNAL-consumption* )

(proclaim-consumption 'agent-hypotheses *default-AMBR-READ-consumption* )
(proclaim-consumption 'agent-correspondences *default-AMBR-READ-consumption* )
(proclaim-consumption 'other-element    (get-consumption 'corresp-elt)  )

(proclaim-consumption 'register-hypothesis   *default-AMBR-MODIFY-consumption* )
(proclaim-consumption 'unregister-hypothesis *default-AMBR-MODIFY-consumption* )

(proclaim-consumption 'identify-relevant-hyps
                      *default-AMBR-INTERNAL-consumption* )
  ;; Caching is used to minimize consumption -- see comment in AMBR/SECRETAR.LSP
(proclaim-consumption 'hypoth-set-winner *default-AMBR-INTERNAL-consumption*)
  ;; Caching is used to minimize consumption.
(proclaim-consumption 'hypoth-set-leader *default-AMBR-READ-consumption*)
  ;; Connectionist aspect can quickly locate the most active agent.

(proclaim-consumption 'find-any-hypothesis :explicit-S-PROGN )
(proclaim-consumption 'find-all-hypotheses :explicit-S-PROGN )

(proclaim-consumption 'driver-elt-mapping
                      (* 3.0 *default-AMBR-READ-consumption*) )  ; approximation

(proclaim-consumption 'check-for-duplicates :explicit-S-PROGN )
(proclaim-consumption 'analyze-duplicates   :explicit-S-PROGN )


;;; For functions from AMBR/HYPOTH.LSP

(proclaim-consumption 'ensure-hypothesis-elements
                      (* 2 (get-consumption 'agent-visible-p)))

(proclaim-consumption 'analyze-secretary-answers :explicit-S-PROGN )
(proclaim-consumption 'resign-hypothesis         :explicit-S-PROGN )
(proclaim-consumption 'resign-or-fizzle          :explicit-S-PROGN )
(proclaim-consumption 'establish-hypothesis      :explicit-S-PROGN )
(proclaim-consumption 'transfer-heritage         :explicit-S-PROGN )

(proclaim-consumption 'choose-favorite
                      (* 2 *default-AMBR-READ-consumption*) )

(proclaim-consumption 'redirect-mentor-support
                      (+ (get-consumption 'get-filler)
                         *default-AMBR-INTERNAL-consumption*
                         (get-consumption 'send-write-request)) )

(proclaim-consumption 'embryo->mature-hypothesis
                      *default-AMBR-MODIFY-consumption* )


;;; For functions from AMBR/STR_CORR.LSP

(proclaim-consumption 'structure-correspondence :explicit-S-PROGN )
(proclaim-consumption 'top-down-SC  :explicit-S-PROGN )
(proclaim-consumption 'bottom-up-SC :explicit-S-PROGN )

(proclaim-consumption 'SC1-mentor-weight
                      (* 2.0 *default-AMBR-READ-consumption*) )
(proclaim-consumption 'formulate-SC-NCR
                      (* 5.0 *default-AMBR-INTERNAL-consumption*) )

(proclaim-consumption 'look-for-cached-MP-X
                      *default-AMBR-INTERNAL-consumption* )


;;; For functions from AMBR/WEAK_SC.LSP

(proclaim-consumption 'weak-structure-correspondence :explicit-S-PROGN )
(proclaim-consumption 'top-down-SC-2  :explicit-S-PROGN )
(proclaim-consumption 'bottom-up-SC-2 :explicit-S-PROGN )

(proclaim-consumption 'make-SC-memo *default-AMBR-INTERNAL-consumption* )
(proclaim-consumption 'send-SC-memo-to-self
                      *default-AMBR-INTERNAL-consumption* )

(proclaim-consumption 'top-down-C-COREFs  *default-AMBR-READ-consumption* )
(proclaim-consumption 'bottom-up-C-COREFs *default-AMBR-READ-consumption* )

(proclaim-consumption 'postpone-SC-memo  *default-AMBR-INTERNAL-consumption* )

(proclaim-consumption 'create-SC-2-links :explicit-S-PROGN )


;;; For functions from AMBR/SKOLEM1.LSP

(proclaim-consumption 'general-corresp-p
                      (* 2.0 (get-consumption 'general-proposition-p)) )
(proclaim-consumption 'prototype-corresp-p
                      (* 2.0 (get-consumption 'agent-type)) )

(proclaim-consumption 'remote-general-hypothesis-p
                      (get-consumption 'general-corresp-p) )
   ;; This is an approximation. Most hyps do not satisfy GENERAL-CORRESP-P and
   ;; hence checking for markers is not necessary.

(proclaim-consumption 'need-skolemization-p
                      (* 2.0 *default-AMBR-READ-consumption*) )

(proclaim-consumption 'has-relevant-markers-p :explicit-S-PROGN )

(proclaim-consumption 'argument->concept  *default-AMBR-INTERNAL-consumption* )

(proclaim-consumption 'send-Skolem-incentive
                      (+ *default-AMBR-INTERNAL-consumption*   ; for making it
                         (get-consumption 'receive-symbol) ))  ; for sending it

(proclaim-consumption 'make-Skolem-message-1
                      *default-AMBR-INTERNAL-consumption* )
(proclaim-consumption 'make-Skolem-message-2
                      *default-AMBR-INTERNAL-consumption* )

(proclaim-consumption 'find-Skolem-table *default-AMBR-INTERNAL-consumption* )
(proclaim-consumption 'find-or-make-Skolem-table
                                         *default-AMBR-INTERNAL-consumption* )


;;; For functions from AMBR/SKOLEM2.LSP

(proclaim-consumption 'skolemize-hypothesis       :explicit-S-PROGN )
(proclaim-consumption 'ensure-instance-arguments  :explicit-S-PROGN )
(proclaim-consumption 'ensure-Skolem-propositions :explicit-S-PROGN )

(proclaim-consumption 'look-for-non-Skolem-propositions :explicit-S-PROGN )
(proclaim-consumption 'strengthen-non-Skolem-proposition
                      (get-consumption 'send-write-request) )

(proclaim-consumption 'finish-skolemization *negligible-AMBR-consumption* )

(proclaim-consumption 'formulate-Skolem-inst-NCR
                      (* 3.0 *default-AMBR-INTERNAL-consumption*))
(proclaim-consumption 'formulate-Skolem-prop-NCR
                      (* 5.0 *default-AMBR-INTERNAL-consumption*))


;;; For functions from AMBR/RATING.LSP
;;
;;  The rating mechanism consumes little energy because it is assumed that
;;  large parts of it are done by the connectionist aspect without engaging
;;  the symbolic processor.

(proclaim-consumption 'authorized-p
                      (get-consumption 'T-driver-p) )

(proclaim-consumption 'initiate-rating-survey
                      *default-AMBR-INTERNAL-consumption* )

(proclaim-consumption 'no-longer-authorized *negligible-AMBR-consumption* )
(proclaim-consumption 'finish-rating        *negligible-AMBR-consumption* )

(proclaim-consumption 'rating-survey :explicit-S-PROGN )

(proclaim-consumption 'identify-rating-hyps
                      (get-consumption 'identify-relevant-hyps) )

(proclaim-consumption 'update-rating-table
                      *default-AMBR-INTERNAL-consumption* )
  ;; The consumption is low because the task is highly automated and
  ;; is carried out in large part by the connectionist machinery.

(proclaim-consumption 'check-for-lethal-ratings
                      *negligible-AMBR-consumption* )  ; + calls to ELIMINATE...
(proclaim-consumption 'eliminate-loser-hypothesis :explicit-S-PROGN )

(proclaim-consumption 'check-for-Skolem-ratings :explicit-S-PROGN )

(proclaim-consumption 'check-for-winner-ratings :explicit-S-PROGN )
(proclaim-consumption 'check-situation-winner   :explicit-S-PROGN )

(proclaim-consumption 'ballotage-rating       :explicit-S-PROGN )
(proclaim-consumption 'skolemize-on-ballotage :explicit-S-PROGN )



;;; For functions from AMBR/PROMOTN.LSP

(proclaim-consumption 'send-promotion-incentive
                      (+ *default-AMBR-INTERNAL-consumption*   ; for making it
                         (get-consumption 'receive-symbol) ))  ; for sending it

(proclaim-consumption 'send-metamorphosis-notification
                      (+ *default-AMBR-INTERNAL-consumption*   ; for making it
                         (get-consumption 'receive-symbol) ))  ; for sending it

(proclaim-consumption 'promotion-incentive-secretary
                                 *default-AMBR-INTERNAL-consumption* )
(proclaim-consumption 'metamorphosis-notification-imago
                                 *default-AMBR-INTERNAL-consumption* )

(proclaim-consumption 'mature->winner-hypothesis
                      (+ (get-consumption 'ensure-hypothesis-elements)
                         *default-AMBR-MODIFY-consumption* ))

(proclaim-consumption 'strengthen-winner-link
                      (get-consumption 'add-link) )    ; modify-link-weight
(proclaim-consumption 'add-mapped-T-LINK
                      (get-consumption 'add-link) )    ; modify-link-weight

(proclaim-consumption 'eliminate-losers-after-promotion  :explicit-S-PROGN )
(proclaim-consumption 'eliminate-matures-after-promotion :explicit-S-PROGN )
(proclaim-consumption 'salvage-losers-after-promotion    :explicit-S-PROGN )

(proclaim-consumption 'add-LOSER-tag (get-consumption 'add-filler-elt) )


;;; For functions from AMBR/PROMPROP.LSP

(proclaim-consumption 'send-promotion-propagation
                      (+ *default-AMBR-INTERNAL-consumption*   ; for making it
                         (get-consumption 'receive-symbol) ))  ; for sending it

;;;;
;;;;;; End of the AMBR-specific proclamations


;;;;;;;  End of file AMBR/PROCLAIM.LSP
