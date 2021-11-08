;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User -*-

;;; FILE:       AMBR/packages.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Definition of the AMBR packages
;;; DEPENDS-ON: dual/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    09-05-97 [2.0]
;;; UPDATED:    21-12-97 [2.1]
;;; UPDATED:    17-02-98 [2.2.0]
;;; UPDATED:    03-03-98 [2.2.1]
;;; UPDATED:    07-05-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                ;;;;
;;;;        ASSOCIATIVE  MEMORY-BASED  REASONING  ver. 2.2.2        ;;;;
;;;;                                                                ;;;;
;;;;            Packages used in the implementation                 ;;;;
;;;;                                                                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  See DUAL/PACKAGES.LSP for a description of DUAL packages.

;;;  OVERVIEW OF AMBR PACKAGES
;;;  ~~~~~~~~~~~~~~~~~~~~~~~~~
;;;  ...
;;;    -- AMBR-core       -- the theoretically motivated stuff
;;;    -- AMBR-interface  -- interface and various utilities
;;;    -- AMBR            -- intended to be the current package while working
;;;                          and experimenting with the model
;;;  ...


;;;  AMBR-CORE PACKAGE
;;;  ~~~~~~~~~~~~~~~~~
;;;  This package is the name space for the implementation of the theoretically
;;;  motivated aspects of the model.
;;;  ...

(cl:unless (cl:find-package "DUAL-CORE")
  (cl:error "DUAL-CORE package should be loaded prior to AMBR-CORE package."))

(cl:defpackage  "AMBRC" (:nicknames "AMBR-CORE") 
                        (:use "DUAL-CORE" "COMMON-LISP")
                        (:size 1000)
                        #+(and :CMU :PCL (not :CLOS)) (:use "PCL")
  (:export  ; from AMBR/DEFS.LSP
     "AMBR-TIME-SLICE"
     "AMBR-SYMB-SUBCYCLES"

     "*AMBR-MAX-ACT*"
     "*AMBR-THRESHOLD*"
     "*AMBR-EXCIT-RATE*"
     "*AMBR-DECAY-RATE*"
     "*HYPOTH-MAX-ACT*"
     "*HYPOTH-ZERO-ACT*"
     "*HYPOTH-MIN-ACT*"
     "*HYPOTH-EXCIT-RATE*"
     "*HYPOTH-INHIB-RATE*"
     "*HYPOTH-DECAY-RATE*"
     "*HYPOTH-OUTPUT-FACTOR*"

     "*ELEMENT->HYPOTH-WEIGHT*"
     "*HYPOTH->ELEMENT-WEIGHT*"
     "*HYPOTH->SITUATION-WEIGHT*"
     "*HYPOTH->MP-MENTOR-WEIGHT*"
     "*HYPOTH->SC1-MENTOR-WEIGHT*"
     "*HYPOTH->SC2-MENTOR-WEIGHT*"
     "*CONTRADICT-HYPOTH-WEIGHT*"
     "*MP-MENTOR-HOMOG-WEIGHT*"
     "*MP-MENTOR-HETEROG-WEIGHT*"
     "*SC1-MENTOR-HOMOG-WEIGHT*"
     "*SC1-MENTOR-HETEROG-WEIGHT*"
     "*SC2-MENTOR-HOMOG-WEIGHT*"
     "*SC2-MENTOR-HETEROG-WEIGHT*"

     "*ELEMENT->WINNER-HYPOTH-WEIGHT*"
     "*MAPPED-T-LINK-WEIGHT*"
     "*ELEMENT->LOSER-HYPOTH-WEIGHT*"

     "*SKOLEM-INSTANCE-WEIGHT*"
     "*SKOLEM-PROPOSITION-WEIGHT*"
     "*SKOLEM-INST-OF-WEIGHT*"
     "*SKOLEM-C-COREF-WEIGHT*"
     "*SKOLEM-SITUATION-WEIGHT*"

     "*DEFAULT-INPUT-CAPACITY*"
     "*DEFAULT-GOAL-CAPACITY*"
     "*DEFAULT-AMBR-EFFICIENCY*"
     "*HYPOTHESIS-AGENT-EFFICIENCY*"
     "*DEFAULT-AMBR-SYMB/CONN-RATIO*"
     "*NUMBER-OF-NODE-CONSTRUCTORS*"

     "*MARKER-EMISSION-FLAG*"
     "*WEAK-SC-FLAG*"
     "*SKOLEMIZATION-FLAG*"
     "*LOSER-HYPOTHESIS-ELIMINATION-FLAG*"

     "*NUMBER-OF-STATE-SC-TRIALS*"
     "*STATE-SC-WAIT-PERIOD*"

     "*POSITIVE-RATING-FACTOR*"
     "*NEGATIVE-RATING-FACTOR*"
     "*RATING-TIME-PERIOD*"
     "*WINNER-RATING*"
     "*LETHAL-RATING*"
     "*SKOLEM-RATING*"
     "*INITIAL-RATING*"
     "*BALLOTAGE-RATING*"
     "*CRITICAL-WINNER-LEVEL*"
     "*CRITICAL-LOSER-LEVEL*"
     "*LOSER-PROTECTION-QUOTA*"
  )
  (:export  ; from AMBR/PROCLAIM.LSP
     "*DEFAULT-AMBR-READ-CONSUMPTION*"
     "*DEFAULT-AMBR-SEND-CONSUMPTION*"
     "*DEFAULT-AMBR-MODIFY-CONSUMPTION*"
     "*DEFAULT-AMBR-INTERNAL-CONSUMPTION*"
     "*NEGLIGIBLE-AMBR-CONSUMPTION*"
     "*DEFAULT-AMBR-MAKE-AGENT-CONSUMPTION*"
     ;; AMBR/PROCLAIM.LSP uses many symbols from the keyword package.
  )
  (:export  ; from AMBR/AMBR_AG.LSP
     "AMBR-AGENT"
     "AMBR-AGENT-P"
     "AGENT-BUFFER"
     "ENSURE-TYPE-SLOT"

     "TEMP-AMBR-AGENT"       ; TEMP-AGENT-P is defined in DUAL/ARCHIT/TEMP_AG
     "DEAD-AMBR-AGENT"       ; DEAD-AGENT-P is defined in DUAL/ARCHIT/TEMP_AG
     "SPECIAL-AMBR-AGENT"    ; SPECIAL-AGENT-P is defined in DUAL/ARCHIT/DUAL_AG
     "SECRETARY-AGENT"
     "NCR-SENDING-AMBR-AGENT"
     "MP-AMBR-AGENT"
     "CONCEPT-AGENT"
     "INSTANCE-AGENT"        ; ENTITY-AGENT was eliminated in ver. 2.2.0.
     "TEMP-INSTANCE-AGENT"
     "DEAD-INSTANCE-AGENT"

     "CORR-AGENT"
     "TEMP-CORR-AGENT"
     "DEAD-CORR-AGENT"
     "HYPOTH-AGENT"
     "EMBRYO-HYPOTH-AGENT"
     "MATURE-HYPOTH-AGENT"
     "WINNER-HYPOTH-AGENT"
     "DEAD-HYPOTH-AGENT"
  )
  (:export  ; from AMBR/KREPRES.LSP
     "AGENT-TYPE"
     "AGENT-MODALITY"
     "HOMOGENEOUS-STATES-P"
     "AGENT-SITUATION"
     "AFFILIATE-AGENT"
     "INSTANCE->CONCEPT"
     "GET-SLOT-SUPERORDINATES"
     "PROPOSITION-ARGS"
     "EQUAL-ARGS"
     "ARGUMENT-PROPS"
     "RANGE-SLOT-P"
     "GENERAL-PROPOSITION-P"
  )
  (:export  ; from AMBR/FIZZLE.LSP
     "FIZZLE-AGENT"
     "FIZZLE-MESSAGE"
     "SEND-FIZZLE-MESSAGE"
     "FIZZLE-MESSAGE-SENDER"
  )
  (:export  ; from AMBR/CORRESP.LSP
     "CORRESP-ELT"         "*DRIVER-ELT-LABEL*"
                           "*RECIPIENT-ELT-LABEL*"
     "CORRESP-JUSTIF"      "*JUSTIFICATION-LABEL*"
     "CORRESP-DRIVER"      "*DRIVER-SIT-LABEL*"
     "ADD-CORRESP-JUSTIF"
     "ADD-CORRESP-DRIVER"
     "CORRESP-TYPE"
     "EQUIVALENT-CORRESP-P"
     "TRIVIAL-CORRESP-P"
     "DOUBLE-TRIVIAL-WEIGHTS"
  )
  (:export  ; from AMBR/GOALINPT.LSP
     "*GOAL*"
     "*INPUT*"
     "ADD-TO-GOAL/INPUT"
     "REMOVE-FROM-GOAL/INPUT"

     "T-DRIVER-P"
     "B-DRIVER-P"
     "ADD-T-DRIVER-TAG"
     "ADD-B-DRIVER-TAG"
  )
  (:export  ; from AMBR/MARKER.LSP
     "MP-SYMBOLIC-STRUCTURE"
     "AMBR-MARKER"
     "T-MARKER"
     "B-MARKER"
     "AGENT-MARKER-COLOR"
     "SEND-T-MARKER"
     "SEND-B-MARKER"

     "MARKER-INTERSECTION-REPORT"
     "MP-X-REPORT-P"
     "MAKE-MP-X-REPORT"
     "MPX-INTERSECTION"
     "MPX-DRIVER-PATH"
     "MPX-RECIPIENT-PATH"
     "TRACK-SLOT-IN-PATH"
  )
  (:export  ; from AMBR/SECRETAR.LSP
     "AGENT-HYPOTHESES"
     "AGENT-CORRESPONDENCES"
     "OTHER-ELEMENT"
     "REGISTER-HYPOTHESIS"
     "UNREGISTER-HYPOTHESIS"
     "IDENTIFY-RELEVANT-HYPS"
     "FIND-ALL-HYPOTHESES"
     "FIND-ANY-HYPOTHESIS"
     "HYPOTH-SET-LEADER"
     "HYPOTH-SET-WINNER"
     "DRIVER-ELT-MAPPING"

     "SECRETARY-SYMBOLIC-STRUCTURE"
     "HYPOTHESIS-REGISTRATION-REQUEST"
     "SEND-HR-REQUEST"
     "SECRETARY-ANSWER"
     "SECRETARY-ANSWER-P"
     "SEND-SECRETARY-ANSWER"
  )
  (:export  ; from AMBR/CSNET.LSP
     "ENCODE-HYPOTH-ACTIVATION"
     "DECODE-HYPOTH-ACTIVATION"
     "GROSSBERG-HYPOTH-ACT-FUN"
     "RUM&MCCL-HYPOTH-ACT-FUN"
     "HYPOTH-OUTPUT-FUN"
  )
  (:export  ; from AMBR/HYPOTH.LSP
     "ENSURE-HYPOTHESIS-ELEMENTS"
     "ANALYZE-SECRETARY-ANSWERS"
     "RESIGN-HYPOTHESIS"
     "RESIGN-OR-FIZZLE"
     "TRANSFER-HERITAGE"
     "ESTABLISH-HYPOTHESIS"
  )
  (:export  ; from AMBR/STR_CORR.LSP
     "STRUCTURE-CORRESPONDENCE"
     "TOP-DOWN-SC"
     "BOTTOM-UP-SC"

     "CARTESIAN-PRODUCT"
  )
  (:export  ; from AMBR/WEAK_SC.LSP
     "WEAK-STRUCTURE-CORRESPONDENCE"
     "TOP-DOWN-SC-2"
     "BOTTOM-UP-SC-2"

     "SC-MEMO"
     "MAKE-SC-MEMO"
     "SC-MEMO-DRIVER-ELT"
     "SC-MEMO-RECIPIENT-ELT"
     "SC-MEMO-DRIVER-SIT"
     "SC-MEMO-M2H-WEIGHT"
     "SC-MEMO-H2M-WEIGHT"
     "SC-MEMO-TRIALS"
  )
  (:export  ; from AMBR/SKOLEM1.LSP
     "GENERAL-CORRESP-P"
     "PROTOTYPE-CORRESP-P"
     "REMOTE-GENERAL-HYPOTHESIS-P"
     "NEED-SKOLEMIZATION-P"
     "HAS-RELEVANT-MARKERS-P"

     "SKOLEM-SYMBOLIC-STRUCTURE"
     "SKOLEM-INCENTIVE"
     "SEND-SKOLEM-INCENTIVE"
     "SKOLEM-INCENTIVE-SECRETARY"

     "SKOLEM-MESSAGE"
     "SKOLEM-MESSAGE-SENDER"
     "SKOLEM-MESSAGE-SITUATION"
     "SKOLEM-MESSAGE-CONCEPT"
     "SKOLEM-MESSAGE-1"
     "SKOLEM-MESSAGE-2"
     "MAKE-SKOLEM-MESSAGE-1"
     "MAKE-SKOLEM-MESSAGE-2"

     "SKOLEM-TABLE"
     "SKOLEM-TABLE-P"
     "MAKE-SKOLEM-TABLE"
     "FIND-SKOLEM-TABLE"
     "SKOLEM-TABLE-HYP"
     "SKOLEM-TABLE-PROP"
     "SKOLEM-TABLE-SIT"
     "SKOLEM-TABLE-SLOTS"
     "SKOLEM-TABLE-OLD-PROPS"
     "SKOLEM-TABLE-NEW-PROPS"

     "MAKE-SKOLEM-SLOT-DESCRIPTOR"
     "SSD-LABEL"
     "SSD-CONCEPT"
     "SSD-PROTOTYPE"
     "SSD-INSTANCES"

     "MAKE-SKOLEM-PROP-DESCRIPTOR"
     "SPD-TARGET"
     "SPD-SLOTS"
  )
  (:export  ; from AMBR/SKOLEM2.LSP
     "SKOLEMIZE-HYPOTHESIS"
     "ENSURE-INSTANCE-ARGUMENTS"
     "ENSURE-SKOLEM-PROPOSITIONS"
     "FINISH-SKOLEMIZATION"
  )
  (:export  ; from AMBR/RATING.LSP
     "RATING-TABLE"
     "RATING-TABLE-P"
     "MAKE-RATING-TABLE"
     "RATING-TABLE-SECRETARY"
     "SECRETARY-RATING-TABLE"
     "INDIVIDUAL-RATING"
     "MAKE-INDIVIDUAL-RATING"
     "IR-HYPOTHESIS"
     "IR-RATING"
     "IR-OTHER"

     "AUTHORIZED-P"
     "INITIATE-RATING-SURVEY"
     "NO-LONGER-AUTHORIZED"
     "FINISH-RATING"
     "RATING-SURVEY"
     "UPDATE-RATING-TABLE"

     "CHECK-FOR-LETHAL-RATINGS"
     "ELIMINATE-LOSER-HYPOTHESIS"
     "CHECK-FOR-SKOLEM-RATINGS"
     "CHECK-FOR-WINNER-RATINGS"
     "CHECK-SITUATION-WINNER"
     "BALLOTAGE-RATING"
  )
  (:export  ; from AMBR/PROMOTN.LSP
     "PROMOTION-SYMBOLIC-STRUCTURE"
     "PROMOTION-INCENTIVE"
     "SEND-PROMOTION-INCENTIVE"
     "PROMOTION-INCENTIVE-SECRETARY"
     "METAMORPHOSIS-NOTIFICATION"
     "SEND-METAMORPHOSIS-NOTIFICATION"
     "METAMORPHOSIS-NOTIFICATION-IMAGO"
  )
  (:export  ; from AMBR/PROMPROP.LSP
     "PROMOTION-PROPAGATION"
     "SEND-PROMOTION-PROPAGATION"
     "PROMPROP-DRIVER-SIT"
     "PROMPROP-DRIVER-INST"
     "PROMPROP-INSTANCE-HYPOTH"
     "PROMPROP-CONCEPT-HYPOTH"
     ;; ...
  )
  (:export  ; from AMBR/TOPLEVEL.LSP
     "AMBR"

     "ENFORCE-AMBR-PARAMETERS"
     "ENFORCE-TIME-PARAMETERS"
  )
  ;; other exports ...
) ; defpackage AMBR-CORE



;;;  AMBR-INTERFACE PACKAGE
;;;  ~~~~~~~~~~~~~~~~~~~~~~~
;;;  This package is the name space for the implementation of ...
;;;  ...


(cl:unless (cl:find-package "DUAL-INTERFACE")
  (cl:error 
  "DUAL-INTERFACE package should be loaded prior to AMBR-INTERFACE package."))

(cl:defpackage  "AMBRI" (:nicknames "AMBR-INTERFACE") 
                        (:use "DUAL-CORE" "DUAL-INTERFACE" "AMBR-CORE")
                        (:use "COMMON-LISP")
                        (:size 500) 
                        #+(and :CMU :PCL (not :CLOS)) (:use "PCL")
  (:export  ; from AMBR/INTRFACE/DEFS.LSP
     "*TARGET*"
     "TARGET-P"

     "*MAX-MAP-INDEX*"
     "*MIN-MAP-INDEX*"
     "*MAPPING-INDEX-MODE*"

     "*A-LINK-GENKB-WEIGHT*"
     "*INSTANCE-LINK-GENKB-PATTERN*"
     "*GENKB-PARAMETER-DESCRIPTIONS*"
     "*EXPECTED-NUMBER-OF-GENKB-TEMPLATES*"
  )
  ;; No exports from AMBR/INTRFACE/KREPRES.LSP yet.
  (:export  ; from AMBR/INTRFACE/MAPPING.LSP
     "MAPPING"
     "MAKE-MAPPING"
     "MAPPING-COALITION"
     "MAPPING-ROLE"
     "MAPPING-OTHER"
     "MAPPING-TIME"
     "MAPPING-TABLE"
     "MAPPING-CM"
     "MAPPING-MI1"
     "MAPPING-MI2"

     "COALITION-MAPPING"
     "MAKE-COALITION-MAPPING"
     "FILL-IN-MAPPING"

     "COUNT-MAPPED-MEMBERS"    "CM"
     "MAPPING-INDEX"           "MI"
     "RAW->INDEX-ACTIVATION"
  )
  (:export  ; from AMBR/INTRFACE/GENKB.LSP
     "GENKB-TEMPLATE"
     "FIND-GENKB-TEMPLATE"
     "REGISTER-GENKB-TEMPLATE"
     "COMBINE-GENKB-DESCRIPTORS"
     "REMOVE-ALL-GENKB-TEMPLATES"
     "DO-ALL-GENKB-TEMPLATES"
     "*GENKB-HERALDS*"

     "GENERATE-KB"
     "GENERATE-KB-IMMEDIATELY"
     "PRINT-GENKB-HERALD"
     "PROCESS-GENKB-TEMPLATE"
     "GENKB->LINKS"

     "SAMPLE"
     "BIASED-COIN-P"
  )
  (:export  ; from AMBR/INTRFACE/REPORT.LSP
     "G"
     "REPORT"
     "REPORT-HEADER"

     "MT"
     "RT"
     "SKT"

     "HYPS"
     "SHOW-HYPS"
     "DO-ALL-HYPS"

     "SHOW-COA"
  )
  (:export  ; from AMBR/INTRFACE/SET_SPY.LSP
     "SKOLEMIZE"   ; shorthand used by VERBOSE
  )
  (:export  ; from AMBR/VERSION.LSP
     "*AMBR-VERSION*"
  )
) ; defpackage AMBR-INTERFACE



;;;  AMBR PACKAGE
;;;  ~~~~~~~~~~~~~
;;;  This package is the name space for the model itself, i.e. its agents, etc.
;;;  ...

(cl:defpackage  "AMBR"  (:use "DUAL-CORE" "DUAL-INTERFACE") 
                        (:use "AMBR-CORE" "AMBR-INTERFACE")
                        (:use "COMMON-LISP")
                        (:size 2000)   ; expected number of agents
                        #+(and :CMU :PCL (not :CLOS)) (:use "PCL")
                        #+:MK-DEFSYSTEM (:use "MAKE")
                        #+:CMU (:use "EXTENSIONS")
                        #+:ACLPC (:use "ALLEGRO")
) ; defpackage AMBR


;;;;;;;  End of file AMBR/PACKAGES.LSP
