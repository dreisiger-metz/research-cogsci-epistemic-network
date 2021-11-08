;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/toplevel.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Establish the top-level objects of the AMBR model
;;; DEPENDS-ON: DUAL, ambr/packages.lsp, ambr/defs.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    08-05-97
;;; UPDATED:    14-02-98 -- Move *GOAL* and *INPUT* to AMBR/GOALINPT.LSP.
;;; UPDATED:    18-05-98 [2.2.2]  Add AMBR and ENFORCE-xxx-PARAMETERS.
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;;    T O P - L E V E L    F U N C T I O N S    ;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR-CORE")

;;;; This file builds the AMBR model -- it sets variables, etc.
;;;; It also provides the main function of the model -- AMBR.
;;;; ...


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: AMBR,
;;          enforce-AMBR-parameters, enforce-time-parameters,
;;

;; ...

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Setting global architectural parameters
;;;

(defun  enforce-time-parameters ()
  "Set *TIME-SLICE* and *SYMB-SUBCYCLES* to their scheduled values."
  (setq *time-slice*
        (AMBR-time-slice))               ; depends implicitly on *TIME*
  (setq *symb-subcycles*
        (AMBR-symb-subcycles))           ; depends implicitly on *TIME*
  (prepare-for-optimized-DUAL-act-fun)   ; see DUAL/ARCHIT/ACT_FUN.LSP
) ; ENFORCE-TIME-PARAMETERS

(defun  enforce-AMBR-parameters ()
  "Set DUAL global parameters according to AMBR parameters."
  (setq *default-agent-efficiency* *default-AMBR-efficiency*)
  (setq *symb/conn-ratio* *AMBR-symb/conn-ratio* )
  (setq *max-act*         *AMBR-max-act*)
  (setq *decay-rate*      *AMBR-decay-rate*)
  (setq *excit-rate*      *AMBR-excit-rate*)
  (setq *threshold*       *AMBR-threshold*)
  (setq *WM-threshold*    *AMBR-threshold*)
  (enforce-time-parameters)                 ; set *TIME-SLICE*, etc.
  ;; (prepare-for-optimized-DUAL-act-fun)   ; called by ENFORCE-TIME-PARAMETERS
) ; ENFORCE-AMBR-PARAMETERS



;;;;;;  Setting up the node-constructor list  (see DUAL/ARCHIT/NC_AGENT.LSP)

(make-node-constructors *number-of-node-constructors*
                        :package (find-package "AMBR")
                        :mode :new )


;;;;;;  Main function

(defun AMBR (delta-T)
  "Run the simulation for DELTA-T units of simulated time."
  (declare (type number delta-T)
           (values float))
  (let ((final-time (- (+ *time* delta-T)
                       0.0001)))            ; tolerance for float '>'
    (do ()                                  ; while loop
        ((> *time* final-time) *time*)
      (enforce-time-parameters)             ; set *TIME-SLICE*, etc.
      (main-DUAL-cycle)                     ; see DUAL/TOPLEVEL.LSP
      ;; *TIME* is incremented within MAIN-DUAL-CYCLE
    )))


;;;;;;;  End of file AMBR/TOPLEVEL.LSP
