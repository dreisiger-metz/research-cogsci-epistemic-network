;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/defs.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Definitions of types, constants, and global variables.
;;; DEPENDS-ON: DUAL/start_me.lsp, DUAL/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    19-01-97 [1.0]
;;; UPDATED:    15-09-97 [1.1.0]
;;; UPDATED:    03-02-98 [1.1.1]
;;; UPDATED:    18-05-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;                                            ;;;;;;;;;;
   ;;;;;;;;;;     G L O B A L   D E F I N I T I O N S    ;;;;;;;;;;
   ;;;;;;;;;;                                            ;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl:in-package  "DUAL-CORE")

;; SYMBOLS:  *min-act*, *max-act*,
;;           *time-slice*,
;;           *excit-rate*, *decay-rate*,
;;           *threshold*, *WM-threshold* ;
;;
;;           *default-agent-efficiency*, *special-agent-efficiency*,
;;           *symb/conn-ratio*, *symb-subcycles* ;
;;
;;           *Anderson-excit-rate*,  *Anderson-decay-rate* ;
;;           *Rum&McCl-excit-rate*,  *Rum&McCl-decay-rate* ;
;;           *Grossberg-excit-rate*, *Grossberg-inhib-rate*,
;;                                   *Grossberg-decay-rate* ;
;;           *Kokinov-threshold*, *Kokinov-decay-rate* ;
;;
;;           *expected-number-of-agents*, *expected-WM-size*,
;;           *expected-number-of-processes*
;;


;;;;  ***** Parameters related to the connectionist machinery ******
;;
;;  USED-IN:  DUAL/archit/act_fun.lsp, DUAL/archit/work_mem.lsp
;;

(defparameter  *min-act*  -1.0
  "The minimal value that the activation level may take." )
  ;; DUAL-ACT-FUN actually has an min-act value of 0.0.  This parameter is
  ;; used only by the 'non-canonical' functions such as Rum&McCl-ACT-FUN.

(defparameter  *max-act*  5.0
  "The maximal value that the activation level may take." )
  ;; WARNING!!!  See PREPARE-FOR-DUAL-ACT-FUN in DUAL/ARCHIT/ACT_FUN.LSP

(defparameter  *time-slice*  0.1
  "Amount of time for one discretization 'cycle'."  )
  ;; WARNING!!!  See PREPARE-FOR-DUAL-ACT-FUN in DUAL/ARCHIT/ACT_FUN.LSP
  ;; Also used in ACTIVATE-PROCESS (divided by *SYMB-SUBCYCLES*) in =/AGENDA.LSP

(defparameter  *excit-rate*  0.5
  "Excitation parameter used in DUAL-ACT-FUN.")
  ;; WARNING!!!  See PREPARE-FOR-DUAL-ACT-FUN in DUAL/ARCHIT/ACT_FUN.LSP

(defparameter  *decay-rate*  2.0
  "Decay parameter used in DUAL-ACT-FUN.")
  ;; WARNING!!!  See PREPARE-FOR-DUAL-ACT-FUN in DUAL/ARCHIT/ACT_FUN.LSP

(defparameter  *threshold*  0.1
  "Threshold parameter used in DUAL-ACT-FUN and THRESHOLD." )
  ;; *THRESHOLD* should always be >= 0.
  ;; WARNING!!!  See PREPARE-FOR-DUAL-ACT-FUN in DUAL/ARCHIT/ACT_FUN.LSP

(defparameter  *WM-threshold*  0.1
  "Minimal activation needed for participation in working memory." )
  ;; Should be coordinated with activation-function thresholds (see below).

(declaim (type single-float *min-act*  *max-act*  *time-slice*   ; help the
                            *excit-rate*  *decay-rate*           ; compiler
                            *threshold*  *WM-threshold* ))       ; to optimize


;;;;  ***** Parameters related to alternative activation functions *****
;;
;;  USED-IN:  DUAL/archit/act_fun.lsp
;;
;;  The parameters below are used in the definitions of alternative activation
;;  functions (see DUAL/ARCHIT/ACT_FUN.LSP).  These alternative activation
;;  functions are not used anywhere in the current DUAL implementation.
;;  They are provided here as a library only.

(defparameter  *Kokinov-threshold* 0.1
  "Threshold parameter used in Kokinov-ACT-FUN." )
(defparameter  *Kokinov-decay-rate*  0.1
  "Decay parameter used in Kokinov-ACT-FUN." )

(defparameter  *Rum&McCl-excit-rate*  0.5
  "Excitation parameter used in Rum&McCl-xxx-TAIL-ACT-FUN.")
(defparameter  *Rum&McCl-decay-rate*  0.1
  "Decay parameter used in Rum&McCl-xxx-TAIL-ACT-FUN." )

(defparameter  *Grossberg-excit-rate*  0.5
  "Excitation parameter used in Grossberg-ACT-FUN.")
(defparameter  *Grossberg-inhib-rate*  1.0
  "Inhibition parameter used in Grossberg-ACT-FUN.")
(defparameter  *Grossberg-decay-rate*  0.1
  "Decay parameter used in Grossberg-ACT-FUN." )

(defparameter  *Anderson-excit-rate*  0.8
  "Excit. parameter used in Anderson-ACT-FUN.")
(defparameter  *Anderson-decay-rate*  1.0
  "Decay parameter used in Anderson-ACT-FUN." )



;;;;  ***** Parameters related to variable-speed symbolic processing *****
;;
;;  USED-IN:  DUAL/archit/agenda.lsp, DUAL/archit/symproc.lsp
;;
;;  See also DUAL/CONSUM.LSP for the consumption of various operations.

(defparameter  *default-agent-efficiency*  0.5
  "The default value of the efficiency coefficient of symbolic processors." )
  ;; *DEFAULT-AGENT-EFFICIENCY* should always be between 0.0 and 1.0.

(defparameter  *special-agent-efficiency*  0.8
  "The default value of the eff.coef of special agents (e.g NC-agents)." )
  ;; *SPECIAL-AGENT-EFFICIENCY* should always be between 0.0 and 1.0.

(defparameter  *symb/conn-ratio*  1.0
  "The ratio of symbolic and connectionist speed of processing." )

(defparameter  *symb-subcycles*  5
  "Number of symbolic subcycles within one *time-slice*." )


(declaim (type float *symb/conn-ratio*             ; help the compiler
                     *default-agent-efficiency*
                     *special-agent-efficiency* ))


;;;;  ***** Implementation-oriented constants *****
;;
;; These constants are merely efficiency hints to the storage allocator,
;; so that implementations using hash tables or arrays will not have to
;; incrementally expand the data structure too often.
;; All data structures used in the program are 'adjustable', so there
;; will be no problem if the actual number of XXX exceeds at some point
;; the *expected-number-of-XXX* suggested below.

(defconstant  *expected-number-of-agents*  2000    ;; see ARCHIT/BASIC.LSP
  "Approximate number of all agents expected in the system.")

(defconstant  *expected-WM-size*  500              ;; see ARCHIT/WORK_MEM.LSP
  "Approximate maximal number of simultaneously active agents.")

(defconstant  *expected-number-of-processes*  200  ;; see ARCHIT/AGENDA.LSP
  "Approximate maximal number of simultaneously running processes." )


;;;;;;;  End of file DUAL/DEFS.LSP
