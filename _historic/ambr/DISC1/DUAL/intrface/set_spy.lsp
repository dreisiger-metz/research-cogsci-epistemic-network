;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/set_spy.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Setting individual spy methods.
;;; DEPENDS-ON: DUAL/intrface/gfun_spy, =/spy_tool, =/verbose, and =/counter.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    24-03-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;;;;      INDIVIDUAL  SPY  METHODS      ;;;;;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package  "DUAL-INTERFACE")

;;;; This file establishes verbose- and count-spy methods for some functions.
;;;; See DUAL/INTRFACE/VERBOSE.LSP and DUAL/INTRFACE/COUNTER.LSP.
;;;;
;;;; All spy templates are set to NIL and all spy methods are deactivated by
;;;; default.  Two goals are achieved in this way:
;;;;   -- There is no overhead when running the model.
;;;;   -- At the same time it is easy to switch verbosing and/or counting on
;;;;       by simply calling VERBOSE or COUNT.


;; The global variable below is needed to prevent error messages when re-loading
;; this file.  It is :CERROR now and is changed to :NEW at the end of the file.

(defvar *default-priority-for-INSTALL-SPY-TOOL*  :cerror
  "Used in DUAL/INTRFACE/SET_SPY.LSP to avoid errors when re-loading the file.")
  ;; Note that it is DEFVAR, not DEFPARAMETER.



;;;;;;; *****   V E R B O S E   M E T H O D S   *****
;;;;
;;;;  See DUAL/INTRFACE/VERBOSE.LSP

;;;;;;;  For DUAL/ARCHIT/BASIC.LSP and DUAL/ARCHIT/DUAL_AG.LSP

;; MAKE-DUAL-AGENT is not a generic function, its subordinate is used.
(let ((new-spy (make-verbose-tool 'make-agent
                                  :gfun-name 'establish-agent-integrity)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (agent)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  agent)
        (print-verbose-header)
        (format *verbose-stream*  "creating a new agent: ~S" agent)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose MAKE-AGENT :before


;; REMOVE-AGENT
(let ((new-spy (make-verbose-tool 'remove-agent)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (agent)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  agent)
        (print-verbose-header)
        (format *verbose-stream*  "removing ~S" agent)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose REMOVE-AGENT :before


;;;;;;;  For DUAL/ARCHIT/LINKS.LSP

;; ADD-LINK
(let ((new-spy (make-verbose-tool 'add-link)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (agent link-label symref weight
                        &key facet-label &allow-other-keys)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  agent link-label symref facet-label)
        (print-verbose-header)
        (format *verbose-stream*
                "adding a ~S~@[.~S~] link from ~S to ~S with weight ~,3F"
                link-label facet-label agent symref weight)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose ADD-LINK :before

;; REMOVE-LINK
(let ((new-spy (make-verbose-tool 'remove-link)))
  (install-spy-tool new-spy :after
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (removed-p        ; return value of primary method
                        agent slot-label reference
                        &key facet-label &allow-other-keys)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  agent slot-label reference facet-label)
        (print-verbose-header)
        (if removed-p
            (format *verbose-stream* "removing a ~S~@[.~S~] link from ~S to ~S"
                    slot-label facet-label agent reference)
            (format *verbose-stream*
            "there isn't any ~S~@[.~S~] link from ~S to ~S. So nothing removed."
                    slot-label facet-label agent reference))) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose REMOVE-LINK :after


;;;;;;;  For DUAL/ARCHIT/SYMPROCx.LSP

;; RECEIVE-SYMBOL
(let ((new-spy (make-verbose-tool 'receive-symbol)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (agent symbol)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  agent symbol)
        (print-verbose-header)
        (cond ((agent-visible-p agent)
                  (format *verbose-stream* "~S received in ~A."
                          symbol (agent-name agent)) )
              ((dead-agent-p agent)
                  (format *verbose-stream* "~S sent to ~A but it is dead."
                          symbol (agent-name agent)) )
              (t  ; invisible but not dead ==> permanent but out of WM
                  (format *verbose-stream* "~S sent to ~A but it is invisible."
                          symbol (agent-name agent)) ))) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose RECEIVE-SYMBOL :before

;; HANDLE-SYMBOL
(let ((new-spy (make-verbose-tool 'handle-symbol)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (agent symbol)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  agent symbol)
        (print-verbose-header)
        (format *verbose-stream* "~A begins working on ~S."
                (agent-name agent) symbol)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose HANDLE-SYMBOL :before


;;;;;;;  For DUAL/ARCHIT/WORK_MEM.LSP

;; ADD-TO-WM
(let ((new-spy (make-verbose-tool 'add-to-WM)))
  (install-spy-tool new-spy :after
    :comment "Verbose spy for ADD-TO-WM."
    :priority :new
    :closure #'(lambda (added-p agent WM &optional weight)
      (declare (ignore weight))
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  agent)
        (print-verbose-header)
        (if added-p     ; the primary value of ADD-TO-WM
            (format *verbose-stream* "adding ~S to ~:[WM~;:*~A~]."
                    agent (WM-comment WM))
            (format *verbose-stream* "not adding ~S to ~:[WM~;:*~A~] because ~
                                      it is ~:[now in ~S~;already there~*~]."
                    agent (WM-comment WM)
                    (eq WM (now-in-WM agent)) (now-in-WM agent)) )))
  )) ; verbose ADD-TO-WM :after

;; REMOVE-FROM-WM
(let ((new-spy (make-verbose-tool 'remove-from-WM)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (agent &optional reset-p)
      (declare (ignore reset-p))
      (let ((WM (now-in-WM agent)))
        (unless (null WM)   ; not in any WM
          (when (spy-template-match-p (spy-tool-template new-spy)
                                      agent WM)
            (print-verbose-header)
            (format *verbose-stream* "removing ~S from ~:[WM~;~:*~A~]."
                    agent (WM-comment WM))))) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose REMOVE-FROM-WM :before


;;;;;;;  For DUAL/ARCHIT/SPREAD.LSP

;; FOCUS-CHANGED
(let ((new-spy (make-verbose-tool 'focus-changed)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (new-focus old-focus &optional WM)
      (declare (ignore WM))
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  new-focus old-focus)
        (print-verbose-header)
        (format *verbose-stream* "~S is now in focus; old focus was ~S."
                new-focus old-focus)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose FOCUS-CHANGED :before


;;;;;;;  For DUAL/ARCHIT/METRONOM.LSP

;; RUN-METRONOME
(let ((new-spy (make-verbose-tool 'run-metronome)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (metronome)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  metronome)
        (print-verbose-header)
        (format *verbose-stream*  "~S emits a pulse." metronome)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose RUN-METRONOME :before


;;;;;;;  For DUAL/ARCHIT/TEMP_AG.LSP

;; KILL-AGENT is a shorthand for KILL-TEMP-AGENT
(let ((new-spy (make-verbose-tool 'kill-agent
                                  :gfun-name 'kill-temp-agent)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (agent)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  agent)
        (print-verbose-header)
        (format *verbose-stream*  "killing temporary ~S" agent)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose KILL-AGENT :before


;;;;;;;  For DUAL/ARCHIT/MP_AGENT.LSP

;; REPORT-MARKER-INTERSECTION
(let ((new-spy (make-verbose-tool 'report-marker-intersection)))
  (install-spy-tool new-spy :before
    :priority   *default-priority-for-INSTALL-SPY-TOOL*
    :template   nil
    :closure #'(lambda (intersection marker1 marker2)
      (when (spy-template-match-p (spy-tool-template new-spy)
                                  intersection marker1 marker2)
        (print-verbose-header)
        (format *verbose-stream* "~S and ~S intersected at ~S."
                marker1 marker2 intersection)) ))

  (deactivate-spy-tool new-spy)      ; Don't cause overhead, just be prepared.
) ; verbose REPORT-MARKER-INTERSECTION :before



;;;;;;; *****   C O U N T E R   M E T H O D S   *****
;;;;
;;;;  See DUAL/INTRFACE/COUNTER.LSP

;; Nothing yet


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Set the default priority variable to :NEW to avoid errors when re-loading.

(setq *default-priority-for-INSTALL-SPY-TOOL* :NEW )

;;;;;;; End of file DUAL/INTRFACE/SET_SPY.LSP
