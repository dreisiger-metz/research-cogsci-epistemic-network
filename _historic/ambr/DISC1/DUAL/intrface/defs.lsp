;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/defs.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Definitions of interface-controlling variables.
;;; DEPENDS-ON: DUAL/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    17-05-97
;;; UPDATED:    12-03-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;     I N T E R F A C E   D E F I N I T I O N S    ;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package  "DUAL-INTERFACE")


;; SYMBOLS:  DUAL-interface-object ;
;;           *SWM-depth*, *SWM-in-to-columns-p*,
;;
;;           *print-float-directive*


;;;;;;;;;;;;  Variable for INTRFACE/PRNFLOAT.LSP

(defvar *print-float-directive*   "~,3F"
  "Default format directive for printing floating-point numbers." )



;;;;;;;;;;;;  Variables for INTRFACE/SPREAD.LSP  ;;;;;;;;;

(defvar *SWM-depth*  5
  "The number of lines used by SWM to show working-memory contents." )

(defvar *SWM-in-two-columns-p*  T
  "Whether SWM show working memories in two (T) or one (NIL) column." )



;;;;;;;  Base class definition for interface-related classes   ;;;;;;;;

(defclass DUAL-interface-object (DUAL-object)
  ()                    ; No slots; this class is a hook for methods.
  (:documentation "The common superclass of all DUAL-interface classes." ))
  ;; See also DUAL-OBJECT in DUAL/GENERAL.LSP.


;;;;;;;  End of file DUAL/INTRFACE/DEFS.LSP
