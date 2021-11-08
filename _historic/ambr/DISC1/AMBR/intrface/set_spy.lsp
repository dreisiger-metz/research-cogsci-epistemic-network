;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-interface -*-

;;; FILE:       AMBR/intrface/set_spy.lsp
;;; VERSION:    2.2.2  ; see AMBR/VERSION.LSP
;;; PURPOSE:    Setting spy (verbose and counter) methods.
;;; DEPENDS-ON: DUAL, DUAL/intrface/set_spy.lsp; AMBR/*.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    25-03-98 [2.2.1] (Modification of old AMBR/INTRFACE/VERBOSE.LSP)
;;; UPDATED:    25-05-98 [2.2.2]
;;; UPDATED:    14-08-98 The 'official release'
;;; UPDATED:    ...


       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;;;;      INDIVIDUAL  SPY  METHODS      ;;;;;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package  "AMBR-INTERFACE")

;;;; This file establishes verbose- and count-spy methods for some AMBR
;;;; functions.  See DUAL/INTRFACE/VERBOSE, =/COUNTER, and =/SET_SPY.LSP.
;;;;
;;;; All spy templates are set to NIL and all spy methods are deactivated by
;;;; default.  Two goals are achieved in this way:
;;;;   -- There is no overhead when running the model.
;;;;   -- At the same time it is easy to switch verbosing and/or counting on
;;;;       by simply calling VERBOSE or COUNT.


;; The global variable below is needed to prevent error messages when re-loading
;; this file.  It is :CERROR now and is changed to :NEW at the end of the file.
;;
;; (Note the variable DUAL-interface::*default-priority-for-INSTALL-SPY-TOOL*
;;  in file DUAL/INTRFACE/SET_SPY.LSP.)

(defvar AMBR-interface::*default-priority-for-INSTALL-SPY-TOOL*
        :cerror
  "Used in DUAL/INTRFACE/SET_SPY.LSP to avoid errors when re-loading the file.")
  ;; Note that it is DEFVAR, not DEFPARAMETER.



;;;;;;; *****   V E R B O S E   M E T H O D S   *****
;;;;
;;;;  See DUAL/INTRFACE/VERBOSE.LSP and DUAL/INTRFACE/SET_SPY.LSP.

;;;;;;;  For AMBR/KREPRES.LSP

;; AFFILIATE-AGENT
(let ((new-spy (make-verbose-tool 'affiliate-agent)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   T  ; <---
    :closure #'(lambda (new-member situation weight)
      (declare (ignore weight))
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  new-member situation)
        (print-verbose-header)
        (format *verbose-stream* "~S affiliates to ~S."
                new-member situation)) ))
) ; verbose AFFILIATE-AGENT :before


;;;;;;;  For AMBR/FIZZLE.LSP

;; FIZZLE-AGENT
(let ((new-spy (make-verbose-tool 'fizzle-agent)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (agent)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  agent)
        (print-verbose-header)
        (format *verbose-stream* "~S fizzles."
                agent)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose FIZZLE-AGENT :before


;;;;;;;  For AMBR/HYPOTH.LSP

;; RESIGN-HYPOTHESIS
(let ((new-spy (make-verbose-tool 'resign-hypothesis)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (hypothesis in-favor-of)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  hypothesis in-favor-of)
        (print-verbose-header)
        (format *verbose-stream* "~S resigns in favor of ~S."
                hypothesis in-favor-of)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose RESIGN-HYPOTHESIS :before

;; ESTABLISH-HYPOTHESIS
(let ((new-spy (make-verbose-tool 'establish-hypothesis)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (hypothesis competitors)
      (declare (ignore competitors))
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  hypothesis)
        (print-verbose-header)
        (format *verbose-stream* "establishing hypothesis ~S."
                hypothesis)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose ESTABLISH-HYPOTHESIS :before


;;;;;;;  For AMBR/STR_CORR.LSP

;; BOTTOM-UP-SC
(let ((new-spy (make-verbose-tool 'bottom-up-SC)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (hypothesis)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  hypothesis)
        (print-verbose-header)
        (format *verbose-stream* "~S begins bottom-up SC."
                hypothesis)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose BOTTOM-UP-SC :before

;; TOP-DOWN-SC
(let ((new-spy (make-verbose-tool 'top-down-SC)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (hypothesis)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  hypothesis)
        (print-verbose-header)
        (format *verbose-stream* "~S begins top-down SC."
                hypothesis)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose TOP-DOWN-SC :before


;;;;;;;  For AMBR/WEAK_SC.LSP

;; BOTTOM-UP-SC-2
(let ((new-spy (make-verbose-tool 'bottom-up-SC-2)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (hypothesis)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  hypothesis)
        (print-verbose-header)
        (format *verbose-stream* "~S begins 'weak' bottom-up SC."
                hypothesis)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose BOTTOM-UP-SC-2 :before

;; TOP-DOWN-SC-2
(let ((new-spy (make-verbose-tool 'top-down-SC-2)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (hypothesis)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  hypothesis)
        (print-verbose-header)
        (format *verbose-stream* "~S begins 'weak' top-down SC."
                hypothesis)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose TOP-DOWN-SC-2 :before


;;;;;;;  For AMBR/SKOLEM2.LSP

;; SKOLEMIZE-HYPOTHESIS  is no longer generic.  ENSURE-INSTANCE-ARGUMENTS
;; is used instead.  It is more convenient because it is the etry point to
;; the 'real' skolemization.
(let ((new-spy (make-verbose-tool 'skolemize
                                  :gfun-name 'ensure-instance-arguments)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   T  ; <---
    :closure #'(lambda (hypothesis Skolem-table)
      (declare (ignore Skolem-table))
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  hypothesis)
        (print-verbose-header)
        (format *verbose-stream* "~S begins skolemization."
                hypothesis)) ))
) ; verbose SKOLEMIZE :before


;;;;;  Re-activate the spy tool for RECEIVE-SYMBOL (see DUAL/INTRFACE/SET_SPY).
;;
(verbose 'receive-symbol '(skolem-symbolic-structure promotion-incentive))


;;;;;;; *****   C O U N T E R   M E T H O D S   *****
;;;;
;;;;  See DUAL/INTRFACE/COUNTER.LSP and DUAL/INTRFACE/SET_SPY.LSP.

;; Nothing yet


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Set the default priority variable to :NEW to avoid errors when re-loading.

(setq AMBR-interface::*default-priority-for-INSTALL-SPY-TOOL* :NEW )


;;;;;;; End of file AMBR/INTRFACE/SET_SPY.LSP
