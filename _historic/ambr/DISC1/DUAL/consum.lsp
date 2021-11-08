;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/consum.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Consumption of various symbolic operations
;;; DEPENDS-ON: DUAL/packages.lsp, DUAL/start_me.lsp, DUAL/proclaim.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    26-01-98 [1.1.1]  DUAL/GENERAL.LSP   -->  DUAL/PROCLAIM.LSP
;;;                               DUAL/PROCLAIM.LSP  -->  DUAL/CONSUM.LSP
;;;                                                       DUAL/WEIGHTS.LSP
;;; UPDATED:    04-05-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...
;;;;;;;;;;
;;; TO DO:      Design and implement proclamations for SETF methods. For inst:
;;;               (proclaim-consumption '(setf get-filler) ... )


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;     C O N S U M P T I O N   P R O C L A M A T I O N S     ;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl:in-package  "DUAL-CORE")

;;;; This file proclaims the consumption of some symbolic operations.
;;;;
;;;; The specification of DUAL postulates that symbolic processing consumes
;;;; energy. The fun. PROCLAIM-CONSUMPTION, defined in DUAL/PROCLAIM.LSP
;;;; (see also DUAL/ARCHIT/BRACKETS.LSP), is the tool for proclaiming the
;;;; consumption of a given operation.
;;;;
;;;; Operations are implemented by [generic] functions, so their consumptions
;;;; are kept in a 'consumption table' indexed by the name of the operation
;;;; (i.e. the symbol that names the function).
;;;;
;;;;***************************************************************************
;;;; WARNING!!!  When DUAL-FLEXIBLE quality is, off run-time changes WARNING!!!
;;;; WARNING!!!  of the 'consumption table' do not take effect.      WARNING!!!
;;;; WARNING!!!  See DUAL/START_ME.LSP for more details.             WARNING!!!
;;;;***************************************************************************


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: *default-READ-consumption*, *default-SEND-consumption*,
;;          *negligible-consumption*


;;  ******************************************************
;;  Proclamations                *************************
;;  ******************************************************

;;; **** For generic types of operations; used throughout.

(defparameter *default-READ-consumption*  0.02
   "The amount of energy needed for a transaction of type 'read'." )

(defparameter *default-SEND-consumption*  0.1
   "The amount of energy needed for a transaction of type 'send'." )

(defparameter *default-MODIFY-consumption*  0.3
   "The amount of energy needed for modifying the local micro-frame." )

(defparameter *default-INTERNAL-consumption* 0.005
   "The amount of energy needed for internal operations in the buffer." )

(defparameter *negligible-consumption* 0.001 )


;;; **** For functions from DUAL/ARCHIT/CONNECT.LSP

(proclaim-consumption 'agent-visible-p *negligible-consumption* )

(proclaim-consumption 'agent-activation *default-READ-consumption* )
(proclaim-consumption 'act              *default-READ-consumption* )


;;; **** For functions from DUAL/ARCHIT/FILLER.LSP

(proclaim-consumption 'fholder-filler  *default-INTERNAL-consumption* )
(proclaim-consumption 'set-filler      *default-MODIFY-consumption* )

(proclaim-consumption 'add-filler-elt    *default-MODIFY-consumption* )
(proclaim-consumption 'remove-filler-elt *default-MODIFY-consumption* )


;;; **** For functions from DUAL/ARCHIT/MCRFRAME.LSP

(proclaim-consumption 'locate-mfr-component *default-INTERNAL-consumption* )

(proclaim-consumption 'add-G-slot   *default-MODIFY-consumption* )
(proclaim-consumption 'add-S-slot   *default-MODIFY-consumption* )
(proclaim-consumption 'add-facet    *default-MODIFY-consumption* )
(proclaim-consumption 'remove-slot  *default-MODIFY-consumption* )
(proclaim-consumption 'remove-facet *default-MODIFY-consumption* )


;;; **** For functions from DUAL/ARCHIT/LINKS.LSP

(proclaim-consumption 'get-filler       *default-INTERNAL-consumption* )
(proclaim-consumption 'get-filler-refs! *default-INTERNAL-consumption*)

(proclaim-consumption 'add-link     *default-MODIFY-consumption* )
(proclaim-consumption 'remove-link  *default-MODIFY-consumption* )


;;; **** For functions from DUAL/ARCHIT/AGENDA.LSP

(proclaim-consumption 'busy-wait 0.0)
  ;; BUSY-WAIT is special!!  It manipulates directly the energetic balance.
  ;; No other functions from AGENDA.LSP should occur in user programs.


;;; **** For functions from DUAL/ARCHIT/SYMPROC2.LSP

(proclaim-consumption 'receive-symbol *default-SEND-consumption* )
  ;; The agent that calls RECEIVE-SYMBOL acts as a sender in the transaction.

(proclaim-consumption 'trigger-symbolic-processor
                      (/ *default-SEND-consumption* 5.0) )
  ;; The agent that calls TRIGGER-SYMBOLIC-PROCESSOR acts as a sender
  ;; in the transaction. The transaction itself is inexpensive, though.

(proclaim-consumption 'symbolic-microcycle :explicit-S-PROGN )
(proclaim-consumption 'handle-symbol       :explicit-S-PROGN )

(proclaim-consumption 'handle-next-input *default-INTERNAL-consumption* )
  ;; This is the time needed to pop the input zone.
  ;; HANDLE-SYMBOL takes care of itself.


;;; **** For functions from DUAL/ARCHIT/SYMBOLIC.LSP

(proclaim-consumption 'send-write-request *default-SEND-consumption* )
  ;; The sender consumes only as much as for a transaction of type 'send'.
  ;; The real work (and its consumption) will be carried out by the receiver.


;;; **** For functions from DUAL/ARCHIT/NC_AGENT.LSP

(proclaim-consumption 'recruit-node-constructor  *default-READ-consumption* )

(proclaim-consumption 'make-DUAL-agent 2.0 )

(proclaim-consumption 'construct-new-agent   ; see NC_AGENT.LSP for details
          (+ *default-INTERNAL-consumption*        ; for phase 0
             (get-consumption 'agent-activation)   ; for phase 1
             (get-consumption 'make-DUAL-agent)    ; for phase 2a
             (get-consumption 'add-link)) )        ; for phase 2c

(proclaim-consumption 'send-NC-request *default-INTERNAL-consumption* )
;; The NC-request is not really sent. It is only stored in the NCR-queue and
;; will be sent (with due consumption) when there is a free node-constructor.


;;; **** For functions from DUAL/ARCHIT/MP_AGENT.LSP

(proclaim-consumption 'make-marker   *default-INTERNAL-consumption* )
(proclaim-consumption 'copy-marker   *default-INTERNAL-consumption* )
(proclaim-consumption 'store-marker  *default-INTERNAL-consumption* )
(proclaim-consumption 'delete-marker *default-INTERNAL-consumption* )

(proclaim-consumption 'marker-origin-visible-p
                      (get-consumption 'agent-visible-p) )

(proclaim-consumption 'equivalent-markers-p
                      *default-INTERNAL-consumption* )

(proclaim-consumption 'complementary-markers-p
                      (+ *default-INTERNAL-consumption*
                         *default-READ-consumption*) )

(proclaim-consumption 'emit-marker :explicit-S-PROGN )

(proclaim-consumption 'preprocess-new-marker      :explicit-S-PROGN )
(proclaim-consumption 'go-through-old-markers     :explicit-S-PROGN )
(proclaim-consumption 'report-marker-intersection :explicit-S-PROGN )
(proclaim-consumption 'mark-MP-neighbors          :explicit-S-PROGN )


;;;;;;;  End of file DUAL/CONSUM.LSP
