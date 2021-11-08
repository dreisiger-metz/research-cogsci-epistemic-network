;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-CORE -*-

;;; FILE:       DUAL/toplevel.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    The top-level function of the architecture
;;; DEPENDS-ON: packages.lsp; archit/spread.lsp, archit/agenda.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    10-06-97 [1.0]
;;; UPDATED:    11-11-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2]  The 'official release'
;;; UPDATED:    ...


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;;     T O P - L E V E L   F U N C T I O N S    ;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; This file defines the basic DUAL function -- MAIN-DUAL-CYCLE.
;;;;
;;;; Running a DUAL-based model is typically done by the following sequence:
;;;;   (clear-everything-in-DUAL)
;;;;   <load model definition files>
;;;;   (do () (<terminating condition>)
;;;;     (main-DUAL-cycle) )


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: main-DUAL-cycle, clear-everything-in-DUAL
;;

;; MAIN-DUAL-CYCLE ()  -->  no values
;;
;; The top-level function of the architecture.  Performs one 'cycle', namely:
;;   (clock-tick)                ; forward the internal clock
;;   (spread)                    ; connectionist aspect
;;   (run-all-processes)         ; symbolic aspect
;;

;; CLEAR-EVERYTHING-IN-DUAL ()  -->  T
;;
;; A function that clears everything in the architecture. Always returns T.
;; WARNING: This irreversibly destroys all agents and all related structures.
;;
;; More concretely, CLEAR-EVERYTHING-IN-DUAL triggers the following actions:
;;  -- remove all agents from any working memories
;;  -- terminate all symbolic processes
;;  -- set *NODE-CONSTRUCTORS* to NIL   (see DUAL/ARCHIT/NC_AGENT.LSP)
;; *** destroy all agents! (via a call to REMOVE-ALL-AGENTS)
;;  -- clear *DEAD-NAME-REGISTRY*    (see DUAL/ARCHIT/BASIC.LSP)
;;  -- set DUALI:*ALL-COALITIONS* to NIL  (see DUAL/INTRFACE/COALITN.LSP)
;;  -- set *TIME* to 0.0
;;
;; After a call to CLEAR-EVERYTHING-IN-DUAL it is always true that:
;;   (find-agent whatever)  -->  nil

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

(defun main-DUAL-cycle ()
  "Call SPREAD and RUN-ALL-PROCESSES in turn."
  ;; It is easy when everything is implemented already :-)
  (clock-tick)                   ; forward the internal clock
  (spread)                       ; connectionist aspect
  (run-all-processes)            ; symbolic aspect
  (values) )


(defun clear-everything-in-DUAL ()
  "Empty all working memories, the agenda, and finally remove all agents."
  (remove-all-wm *WM*)
  (mapc #'remove-all-wm *special-WMs*)
  (do-all-processes (proc)
    (remove-from-agenda proc) )
  (setq *node-constructors* nil)
  (remove-all-agents)
  (setq *dead-name-registry* nil)
  (setq DUAL-interface:*all-coalitions* nil)
  (setq *time* 0.0)
  T )


;;;;;;;  End of file DUAL/TOPLEVEL.LSP
