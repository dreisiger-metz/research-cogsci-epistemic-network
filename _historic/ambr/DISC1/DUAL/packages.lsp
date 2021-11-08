;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Common-LISP-user -*-

;;; FILE:       DUAL/packages.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Definition of the packages used in DUAL
;;; DEPENDS-ON: DUAL/nonport1.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    21-01-97 [1.0]
;;; UPDATED:    14-12-97 [1.1.0]
;;; UPDATED:    25-01-98 [1.1.1]
;;; UPDATED:    12-03-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                   ;;;;
;;;;    D  U  A  L     C O G N I T I V E   A R C H I T E C T U R E     ;;;;
;;;;                                                                   ;;;;
;;;;          Packages used in the implementation, ver. 1.1.2          ;;;;
;;;;                                                                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  See DUAL/START_ME.LSP for an introduction to DUAL implementation.
;;;  See DUAL/DEFSYS.LSP for a description of the modules used in the system.
;;;  See DUAL/VERSION.LSP for version-related information.

;;;  OVERVIEW OF DUAL PACKAGES
;;;  ~~~~~~~~~~~~~~~~~~~~~~~~~
;;;  The classes, functions, variables, and other Lisp objects that
;;;  implement the DUAL cognitive architecture are bound to symbols
;;;  owned by the following packages:
;;;    -- DUAL-core      -- the theoretically motivated stuff
;;;    -- DUAL-interface -- interface and various utilities
;;;
;;;  Typically, the programmer puts a (use-package "DUAL-CORE")
;;;  at the start of model-defining files. This ensures that the model
;;;  being defined adheres strictly to the conceptual description of
;;;  the architecture.
;;;  To debug the model and to experiment with it, one needs additional
;;;  utilities. They are provided in the DUAL-INTERFACE package.
;;;
;;;  Thus, a typical session at the console usually begins with:
;;;    (use-package '("DUALC" "DUALI"))


;;;  DUAL-CORE PACKAGE
;;;  ~~~~~~~~~~~~~~~~~
;;;  This package is the name space for the implementation of the DUAL
;;;  cognitive architecture as set forth by 'DUAL Report #1'. In other
;;;  words, the theoretically motivated portions of the program are
;;;  bound to symbols owned by this package. The set of all external
;;;  symbols of the DUAL-core package comprise the external protocol
;;;  of the implementation of the DUAL architecture.
;;;  Definitions of DUAL-based models should use only the (external)
;;;  symbols from this package. This ensures that the model adheres to
;;;  the conceptual description of the architecture.
;;;
;;;  (Note that "DUAL-CORE" is in fact a nickname of the package named "DUALC".
;;;   This is for brevity of the printed output during debugging:
;;;     DUALC::FOOBAR  is shorter than  DUAL-CORE::FOOBAR
;;;     DUALI::FOOBAR  is shorter than  DUAL-INTERFACE::FOOBAR  )


(cl:defpackage  "DUALC"
  (:nicknames "DUAL-CORE")
  (:use "COMMON-LISP")
  (:size 1500)
  #+(and :PCL (NOT :CLOS))  (:use "PCL")
  #+:CMU  (:import-from "EXTENSIONS" "REQUIRED-ARGUMENT")
  (:export   ; from DUAL/NONPORT2.LSP
     "REQUIRED-ARGUMENT"
     #+:CMU "DESCRIBE1"
  )
  (:export  ; from DUAL/DEFS.LSP
     "*MIN-ACT*"
     "*MAX-ACT*"
     "*TIME-SLICE*"       ; see *SYMB-SUBCYCLE* below

     "*WM-THRESHOLD*"
     "*THRESHOLD*"   "*KOKINOV-THRESHOLD*"
     "*EXCIT-RATE*"  "*RUM&MCCL-EXCIT-RATE*"
                     "*ANDERSON-EXCIT-RATE*"
                     "*GROSSBERG-EXCIT-RATE*"  "*GROSSBERG-INHIB-RATE*"
     "*DECAY-RATE*"  "*RUM&MCCL-DECAY-RATE*"
                     "*ANDERSON-DECAY-RATE*"
                     "*GROSSBERG-DECAY-RATE*"
                     "*KOKINOV-DECAY-RATE*"

     "*DEFAULT-AGENT-EFFICIENCY*"
     "*SPECIAL-AGENT-EFFICIENCY*"
     "*SYMB/CONN-RATIO*"
     "*SYMB-SUBCYCLES*"

     "*EXPECTED-NUMBER-OF-AGENTS*"
     "*EXPECTED-WM-SIZE*"
     "*EXPECTED-NUMBER-OF-PROCESSES*"
  )
  (:export  ; from DUAL/GENERAL.LSP
     "DUAL-DESCRIBE"
     "DUAL-OBJECT"
     "TEMPORARY-DUAL-OBJECT"
     "BOOLEAN"
     "UNION1"
  )
  (:export  ; from DUAL/PROCLAIM.LSP
     "PROCLAIM-CONSUMPTION"  ; see DUAL/ARCHIT/BRACKETS and DUAL/CONSUM.LSP
     "GET-CONSUMPTION"

     "PROCLAIM-LABEL"        ; see DUAL/ARCHIT/FILLER and =/SLOTS.LSP
     "LABEL-TYPE"
     "G-SLOT-LABEL-P"
     "S-SLOT-LABEL-P"
     "SLOT-LABEL-P"

     "PROCLAIM-LINK"         ; see DUAL/ARCHIT/LINKS.LSP
     "LINK-DESCRIPTOR"
     "LINK-LABEL-P"
     "TEMP-LINK-LABEL-P"
     "MP-LABEL-P"
     "LINK-DEFAULT-WEIGHT"   ; see "DEFAULT-WEIGHT" in DUAL/INTRFACE/READFILR.L

     "PROCLAIM-TAG"          ; see DUAL/ARCHIT/FILLER.LSP and
     "TAG-TYPE"                                ; DUAL/INTRFACE/DEFAGENT.LSP
     "TAG-P"

     "DUAL-KEYWORD-P"
  )
  (:export  ; from DUAL/CONSUM.LSP
     "*DEFAULT-READ-CONSUMPTION*"
     "*DEFAULT-SEND-CONSUMPTION*"
     "*DEFAULT-MODIFY-CONSUMPTION*"
     "*DEFAULT-INTERNAL-CONSUMPTION*"
     "*NEGLIGIBLE-CONSUMPTION*"
  )
  ;; no symbols are exported from DUAL/LABELS.LSP
  (:export  ; from DUAL/ARCHIT/BASIC.LSP
     "BASE-AGENT"
     "MAKE-BASE-AGENT"
     "AGENTP"
     "AGENT-NAME"
     "AGENT-COMMENT"
     "FIND-AGENT"
   ;; #\#$ macro character
     "REMOVE-AGENT"
     "DO-ALL-AGENTS"
     "REMOVE-ALL-AGENTS"
     "GENNAME"
     "*DEAD-NAME-REGISTRY*"
     "AGENT-DESCRIPTOR-STRING"

     "AGENT-FLAG"
     "*FLAG1*"
     "*FLAG2*"
     "*FLAG3*"
     "*FLAG4*"
  )
  (:export  ; from DUAL/ARCHIT/SYMREF.LSP
     "MAKE-SYMREF"
     "SYMREF-AGENT"
     "SYMREF-SLOT"
     "SIMPLE-SYMREF-P"
     "EXTENDED-SYMREF-P"
     "SYMREF-P"
     "SIMPLIFY-SYMREF"
     "SYMREF-MATCH"
     "SYMREF-EQUAL"
     "SYMREF->STRING"
  )
  (:export  ; from DUAL/ARCHIT/CONREF.LSP
     "MAKE-CONREF"
     "COPY-CONREF"
     "CONREF-REFERENCE"
     "CONREF-WEIGHT"
     "CONREF-P"
     "CONREF-MATCH"
     "CONREF-EQUAL"
     "ADJOIN-CONREF"
     "NORMALIZE-WEIGHTS"
  )
  (:export  ; from DUAL/ARCHIT/ACT_FUN.LSP
     "DUAL-ACT-FUN"
     "UNOPTIMIZED-DUAL-ACT-FUN"
     "PREPARE-FOR-OPTIMIZED-DUAL-ACT-FUN"

     "CLIP-0-1"   "CLIP-1-1"     "CLIP-MIN-MAX"
     "THRESHOLD"  "THRESHOLD-0"  "THRESHOLD-THETA"

     "ANDERSON-ACT-FUN"
     "RUM&MCCL-ONE-TAIL-ACT-FUN"
     "RUM&MCCL-TWO-TAIL-ACT-FUN"
     "GROSSBERG-ACT-FUN"
     "KOKINOV-ACT-FUN"
  )
  (:export  ; from DUAL/ARCHIT/CONNECT.LSP
     "CONNECTIONIST-AGENT"
     "MAKE-CONN-AGENT"
     "AGENT-ACTIVATION"  "ACT"
     "AGENT-VISIBLE-P"
     "AGENT-OUTPUT"
     "AGENT-NEIGHBORS"
     "ADD-NEIGHBORS"
     "REMOVE-NEIGHBORS"
     "NORMALIZE-NEIGHBORS"
     "PREPARE-TO-RECEIVE-ACTIVATION"
     "RECEIVE-ACTIVATION"
     "ACTIVATE-NEIGHBORS"
     "AGENT-NEW-ACTIVATION"
     "UPDATE-ACTIVATION"
  )
  (:export  ; from DUAL/ARCHIT/FILLER.LSP
     "MICROFRAME-COMPONENT"
     "FILLER-HOLDER"

     "FILLER-TYPE"
     "FHOLDER-FILLER-TYPE"
     "FHOLDER-FILLER"
     "SET-FILLER"
     "ADD-FILLER-ELT"
     "REMOVE-FILLER-ELT"
  )
  (:export  ; from DUAL/ARCHIT/SLOTS.LSP
     "BASE-SLOT"
     "SLOT-BUNDLE"
     "LINK"
     "TEMPORARY-LINK"
     "G-SLOT"
     "S-SLOT"
     "FACET"
     "TEMPORARY-G-SLOT"
     "TEMPORARY-FACET"

     "MAKE-G-SLOT"
     "MAKE-S-SLOT"
     "MAKE-FACET"
     "SLOT-LABEL"
     "S-SLOT-FACETS"
     "S-SLOT-COMMENT"

     "SLOT-OWNER"
     "NOTIFY-OWNER"    ; actually defined in DUAL/ARCHIT/FILLER.LSP
  )
  (:export  ; from DUAL/ARCHIT/MCRFRAME.LSP
     "MICRO-FRAME"
     "MAKE-MICRO-FRAME"
     "AGENT-G-SLOTS"
     "AGENT-S-SLOTS"
     "ADD-G-SLOT"
     "ADD-S-SLOT"
     "ADD-FACET"
     "REMOVE-SLOT"
     "REMOVE-FACET"
     "LOCATE-MFR-COMPONENT"
     "ESTABLISH-MICROFRAME-INTEGRITY"
     "DO-ALL-REFERENCES"
  )
  (:export  ; from DUAL/ARCHIT/LINKS.LSP
     "ADD-LINK"
     "REMOVE-LINK"
     "REMOVE-ALL-TEMPORARY-LINKS"
     "TEMP-LINK-P"
     "GET-FILLER"
     "GET-FILLER-REFS!"
  )
  (:export  ; from DUAL/ARCHIT/SYMPROC1.LSP
     "SYMBOLIC-PROCESSOR"
     "MAKE-SYMB-PROCESSOR"
     "AGENT-EFFICIENCY"
     "AGENT-INPUT-ZONE"
     "CLEAR-VOLATILE-MEMORY"
     "SYMBOLIC-STRUCTURE"
     "ACTIVE-PROCESSOR-P"
  )
  (:export  ; from DUAL/ARCHIT/AGENDA.LSP
     "SUSPENDED-PROCESS"
     "MAKE-PROCESS"
     "PROCESS-HOST"
     "PROCESS-ENERGY"
     "PROCESS-STACK"
     "SUSPENDED-PROCESS-P"
     "PUSH-TO-STACK"
     "BURY-AT-BOTTOM-OF-STACK"
     "SUPPLY-ENERGY"
     "CONSUME-ENERGY-WITH-LOSSES"
     "BUSY-WAIT"
     "ACTIVATE-PROCESS"
     "RUN-PROCESS"
     "*AGENDA*"
     "FIND-PROCESS"
     "FIND-OR-MAKE-PROCESS"
     "REMOVE-FROM-AGENDA"
     "DO-ALL-PROCESSES"
     "RUN-ALL-PROCESSES"
     "RUN1-ALL-PROCESSES"
     "NUMBER-OF-PROCESSES"
  )
  (:export  ; from DUAL/ARCHIT/SUSPEND.LSP
     "S-EVAL"
     "*IGNORE-SUSPENSIONS*"
     "*SUSPENSION-PRIMITIVES*"
     "SUSPEND-FORM-P"
     "SIMPLE-FORM-P"
     "S-PROGN-RETURN-TYPE"
  )
  (:export  ; from DUAL/ARCHIT/BRACKETS.LSP
  ;; #\[ and #\]  macro characters
  ;; "GET-CONSUMPTION"        ; see DUAL/PROCLAIM.LSP
  ;; "PROCLAIM-CONSUMPTION"   ; see DUAL/PROCLAIM.LSP
     "NUMERALIZE-CONSUMPTION-SPECIFIER"
                 ; :EXPLICIT-S-PROGN
  )
  (:export  ; from DUAL/ARCHIT/MAILBOX.LSP
     "S-PROGN"
     "S-VALUES"              ; :EMPTY-MBOX
     "S-VALUES!"
     "SUSPENDED-VALUE-BIND"
     "MAILBOX"
  )
  (:export  ; from DUAL/ARCHIT/SPROGN.LSP
     "S-PROGN-AUX"     ; S-PROGN is defined in DUAL/ARCHIT/MAILBOX.LSP
     "*S-PROGN-RECOGNIZED-FUNCTORS*"
  )
  (:export  ; from DUAL/ARCHIT/SYMPROC2.LSP
     "TRIGGER-SYMBOLIC-PROCESSOR"
     "SYMBOLIC-MICROCYCLE"
     "RECEIVE-SYMBOL"        ; :SYMBOL-INPUT
     "HANDLE-SYMBOL"
     "HANDLE-NEXT-INPUT"
  )
  (:export  ; from DUAL/ARCHIT/SYMBOLIC.LSP
     "SYMBOLIC-AGENT"
     "MAKE-SYMB-AGENT"
     "WRITE-REQUEST"
     "SEND-WRITE-REQUEST"
  )
  (:export  ; from DUAL/ARCHIT/HYBRID.LSP
     "HYBRID-AGENT"
     "MAKE-HYBRID-AGENT"
     "SLOTS->LINKS"
  )
  (:export  ; from DUAL/ARCHIT/WORK_MEM.LSP
     "*WM*"
     "*SPECIAL-WMS*"
     "WORKING-MEMORY"
     "PRIMARY-WM"
     "SPECIAL-WM"
     "MAKE-PRIMARY-WM"
     "MAKE-SPECIAL-WM"
     "PRIMARY-WM-P"
     "SPECIAL-WM-P"
     "WM-P"
     "WM-SIZE"
     "WM-COMMENT"
     "WM-CONTENTS"
     "WM-FOCUS"
     "WM-ACT-SOURCE"
     "NOW-IN-WM"
     "ADD-TO-WM"      ; :ENTER-WM
     "REMOVE-FROM-WM"
     "REMOVE-ALL-WM"
     "WM-CONREFS"
     "NORMALIZE-WM-WEIGHTS"
     "DENORMALIZE-WM-WEIGHTS"
     "DO-ALL-WM"
     "DO*-ALL-WM"
     "ACTIVATE-ALL-SPECIAL-WM"
     "SORT-WM"
     "UPDATE-WM-FOCUS"
  )
  (:export  ; from DUAL/ARCHIT/DUAL_AG.LSP
     "DUAL-AGENT"
     "DUAL-AGENT-P"
     "VALID-DUAL-AGENT-TYPE-P"
     "MAKE-DUAL-AGENT"
     "INITIALIZE-AGENT"
     "ESTABLISH-AGENT-INTEGRITY"   ; :JUST-CREATED
     "SPECIAL-DUAL-AGENT"
     "SPECIAL-AGENT-P"
  )
  (:export  ; from DUAL/ARCHIT/SPREAD.LSP
     "SPREAD"
     "UPDATE-FOCUS"
     "FOCUS-CHANGED"
     "*FOCUS*"
     "*PREVIOUS-FOCUS*"
  )
  (:export  ; from DUAL/ARCHIT/TIME.LSP
     "*TIME*"
     "CLOCK-TICK"

     "TIME-RELATED-DUAL-OBJECT"
     "TIME-RELATED-SYMBOLIC-STRUCTURE"
     "ALARM-MESSAGE"
     "SEND-ALARM-MESSAGE"

     "ALARM-CLOCK"
     "MAKE-ALARM-CLOCK"
     "ALARM-CLOCK-NAME"
     "ALARM-CLOCK-COMMENT"
     "ALARM-CLOCK-TIME-REMAINING"
     "ALARM-CLOCK-OWNER"
     "ALARM-CLOCK-MESSAGE"
     "SET-ALARM-CLOCK"
     "DEACTIVATE-ALARM-CLOCK"
     "NUMBER-OF-ACTIVE-ALARM-CLOCKS"
     "FIND-ALARM-CLOCK"
     "FIND-OR-MAKE-ALARM-CLOCK"
     "TICK-ALARM-CLOCK"
     "TICK-ALL-ALARM-CLOCKS"
     "RUN-ALARM-CLOCK"
  )
  (:export  ; from DUAL/ARCHIT/METRONOM.LSP
     "METRONOME"
     "MAKE-METRONOME"
     "FIND-METRONOME"
     "REMOVE-METRONOME"
     "*METRONOMES*"
     "METRONOME-NAME"
     "METRONOME-COMMENT"
     "METRONOME-EVENT"
     "METRONOME-SUBSCRIBERS"
     "METRONOME-PERIOD"
     "METRONOME-TIME-REMAINING"
     "TICK-METRONOME"
     "TICK-ALL-METRONOMES"
     "RUN-METRONOME"
     "DO-ALL-SUBSCRIBERS"
  )
  (:export  ; from DUAL/ARCHIT/TEMP_AG.LSP
     "TEMP-DUAL-AGENT"
     "TEMP-AGENT-P"
     "DEAD-TEMP-AGENT"
     "DEAD-AGENT-P"
     "TEMP-AGENT-DYING-CLASS"
     "KILL-TEMP-AGENT"
     "CUT-LINKS-TO-AGENT"
  )
  (:export  ; from DUAL/ARCHIT/NC_AGENT.LSP
     "NODE-CONSTRUCTOR"
     "*NODE-CONSTRUCTORS*"
     "MAKE-NODE-CONSTRUCTORS"
     "FREE-P"
     "RECRUIT-NODE-CONSTRUCTOR"

     ;; HANDLE-SYMBOL, :JUST-CREATED  are defined elsewhere

     "NODE-CONSTRUCTION-REQUEST"
     "MAKE-NC-REQUEST"
     "NCR-MENTOR"
     "NCR-AGENT-TYPE"
     "NCR-GENNAME"
     "NCR-SLOTS"
     "NCR-MAIL"

     "NCR-SENDING-AGENT"
     "AGENT-NCR-QUEUE"
     "SEND-NC-REQUEST"
  )
  (:export  ; from DUAL/ARCHIT/MP_AGENT.LSP
     "MP-DUAL-AGENT"
     "AGENT-MARKERS"
     "AGENT-MP-NEIGHBORS"
     "EMIT-MARKER"
     "STORE-MARKER"
     "DELETE-MARKER"

     "PREPROCESS-NEW-MARKER"
     "GO-THROUGH-OLD-MARKERS"
     "REPORT-MARKER-INTERSECTION"
     "MARK-MP-NEIGHBORS"

     "MARKER"
     "MARKERP"
     "MARKER-TYPE"
     "VALID-MARKER-TYPE-P"
     "MAKE-MARKER"
     "COPY-MARKER"
     "MARKER-COLOR"
     "MARKER-ORIGIN"
     "MARKER-PATH"
     "COMPLEMENTARY-MARKERS-P"
     "EQUIVALENT-MARKERS-P"
     "MARKER-ORIGIN-VISIBLE-P"
  )
  (:export  ; from DUAL/TOPLEVEL.LSP
     "MAIN-DUAL-CYCLE"
     "CLEAR-EVERYTHING-IN-DUAL"
  )
) ; defpackage DUAL-CORE



;;;  DUAL-INTERFACE PACKAGE
;;;  ~~~~~~~~~~~~~~~~~~~~~~~
;;;  The DUAL-core package defines the core of the DUAL cognitive
;;;  architecture. In order to build, debug, and experiment with
;;;  DUAL-based models, one needs additional utilities like a (graphical)
;;;  inspector, batch processor, visualization tools, and the like.
;;;
;;;  The DUAL-INTERFACE package is the name space for these features.
;;;  Strictly DUAL-based models should not depend on this package for
;;;  their theoretically motivated programs. This package, however, is
;;;  very useful for developing, testing and using DUAL-based models.


(cl:defpackage  "DUALI"
  (:nicknames "DUAL-INTERFACE")
  (:use "DUAL-CORE" "COMMON-LISP")
  (:size 500)
  #+:CMU  (:use "EXTENSIONS")
  #+(and :PCL (not :CLOS))  (:use "PCL")
  (:export  ; from DUAL/UTILS.LSP
     "CAUSES-AN-ERROR-P"
     "MAKE-OBSOLETE"
     "PROMPT-AND-READ"
     "=~="
     "SQUARED-DIFFERENCE"
     "SET-EQUAL"
     "FIND-ASYMPTOTE"
  )
  (:export  ; from DUAL/INTRFACE/DEFS.LSP
     "DUAL-INTERFACE-OBJECT"
     "*SWM-DEPTH*"
     "*SWM-IN-TWO-COLUMNS-P*"
     "*PRINT-FLOAT-DIRECTIVE*"
  )
  (:export  ; from DUAL/INTRFACE/CONNECT.LSP
     "SUPPORTED-BY"
     "FAN-IN"
     "FAN-OUT"
     "FLOW-IN"
     "FLOW-OUT"
  )
  (:export  ; from DUAL/INTRFACE/SPREAD.LSP
     "SPR"
     "SWM"          ; see "SWM-DEPTH" in DUAL/INTRFACE/DEFS.LSP
     "SNAPSHOT-WM"
     "WM-TOTAL-ACT"
     "DEACTIVATE-ALL"
  )
  (:export  ; from DUAL/INTRFACE/AGENDA.LSP
     "RA"
     "PROC"
     "*STEP-WHAT*"
     "ST"  "ST!"
  )
  (:export  ; from DUAL/INTRFACE/TIME.LSP
     "SHOW-CLOCKS"
     "SHOW-METRONOMES"
  )
  (:export  ; from DUAL/INTRFACE/DUMMY_AG.LSP
     "FIND-OR-MAKE-AGENT"
     "MAKE-DUAL-AGENT-1"
     "CHECK-FOR-DUMMIES"
     "DUMMY-AGENT-P"
  )
  (:export  ; from DUAL/INTRFACE/READFILR.LSP
     "READ-FILLER"
     "DEFAULT-WEIGHT"   ; see "LINK-DEFAULT-WEIGHT" in DUAL/PROCLAIM.LSP
  )
  (:export  ; from DUAL/INTRFACE/DEFAGENT.LSP
     "DEFAGENT"
     "PARSE-DEFAGENT-BODY"

     "AGENT->DEFAGENT"
     "PPRINT-DEFAGENT"
     "PPRINT-DEFAGENT-SLOTS"
     "PPRINT-MICRO-FRAME"     "MFR"
  )
  (:export  ; from DUAL/INTRFACE/WITH_AG.LSP
     "WITH-AGENT"
     "WITH-AGENT-AUX"
     "PPRINT-WITH-AGENT"
  )
  (:export  ; from DUAL/INTRFACE/KB_UTIL.LSP
     "REQUIRE-AGENTS"
     "REFERENCE-AGENTS"
     "CHECK-FOR-UNRESOLVED-REFERENCES"
     "CHECK-FOR-MISSING-SLOTS"
     "CHECK-FOR-SELF-REFS"
     "CHECK-FOR-OLD-AGENTS"

     "LIST-ALL-AGENTS"
     "XREF"
     "XREF-TO-FILE"
  )
  (:export  ; from DUAL/INTRFACE/COALITN.LSP
     "COALITION"
     "COALITION-P"
     "COALITION-NAME"
     "COALITION-COMMENT"
     "COALITION-MEMBERS"
     "COALITION-HEAD"
     "COALITION-USER1"
     "COALITION-USER2"
     "COALITION-USER3"

     "MAKE-COALITION"        "DEFCOALITION"
     "*ALL-COALITIONS*"
     "FIND-COALITION"        "COA"

     "COUNT-ACTIVE-MEMBERS"  "CA"
     "RETRIEVAL-INDEX"
     "TOTAL-RETRIEVAL-INDEX"
     "*TOTAL-RI-COALITIONS*"
     "LUCE-RETRIEVAL-INDEX"
     "RI"
  )
  (:export  ; from DUAL/INTRFACE/GFUN_SPY.LSP
     "BEGIN-SPYING"
     "STOP-SPYING"
     "ACTIVATE-SPIES"
     "DEACTIVATE-SPIES"
     "ADD-SPY"
     "REMOVE-SPY"
     "SPY-STATUS"
     "GENERAL-SPY-STATUS"
     "REPORT-SPY-STATUS"
  )
  (:export  ; from DUAL/INTRFACE/SPY_TOOL.LSP
     "SPY-TOOL"
     "MAKE-SPY-TOOL"
     "INSTALL-SPY-TOOL"
     "UNINSTALL-SPY-TOOL"
     "SPY-TOOL-NAME"
     "SPY-TOOL-IDENTIFIER"

     "SPY-TOOL-ACTIVE-P"
     "ACTIVATE-SPY-TOOL"
     "DEACTIVATE-SPY-TOOL"

     "SPY-TOOL-TEMPLATE"
     "PREPARE-SPY-TEMPLATE"
     "COMBINE-SPY-TEMPLATES"
     "CHANGE-SPY-TEMPLATE"
     "SPY-TEMPLATE-MATCH-P"
  )
  (:export  ; from DUAL/INTRFACE/VERBOSE.LSP
     "VERBOSE-TOOL"
     "MAKE-VERBOSE-TOOL"
     "FIND-VERBOSE-TOOL"
     "VERBOSE"
     "VERBOSE-STATUS"
     "*VERBOSE-STREAM*"
     "PRINT-VERBOSE-HEADER"
  )
  (:export  ; from DUAL/INTRFACE/COUNTER.LSP
     ;; Nothing yet
  )
  (:export  ; from DUAL/INTRFACE/SET_SPY.LSP
     "MAKE-AGENT"     ; shorthand for VERBOSE
     "KILL-AGENT"     ; shorthand for VERBOSE
  )
  (:export  ; from DUAL/VERSION.LSP
     "*DUAL-VERSION*"
  )
) ; defpackage DUAL-INTERFACE


;;;;;;;  End of file DUAL/PACKAGES.LSP
