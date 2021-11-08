;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User -*-

;;; FILE:       DUAL/start_me.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Entry point to all DUAL programs. Sets compilation policies.
;;; DEPENDS-ON: NIL
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; UPDATED:    11-11-97 [1.1]
;;; UPDATED:    18-04-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;                                                                   ;;;;
  ;;;;    D  U  A  L     C O G N I T I V E   A R C H I T E C T U R E     ;;;;
  ;;;;                                                                   ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;  See file DUAL/README.LSP for general information, references, and
;;;  installation instructions.  See also DUAL/DEFSYS and =/VERSION.LSP.


;;;  COMPILATION POLICIES
;;;  ~~~~~~~~~~~~~~~~~~~~
;;;  The behavior of the program can be modified to some extent via the
;;;  following features:
;;;    * DUAL-DEBUG     When 'on', some effort is taken to maintain human
;;;                     readability of DUAL's internal data structures (and,
;;;                     in particular, closure stacks). These consumes extra
;;;                     space (and hence garbage collection time).
;;;                     Also, when DUAL-DEBUG is 'on' the program tends to
;;;                     generate more output.  (In other words, DUAL-DEBUG
;;;                     subsumes the old DUAL-VERBOSE.)
;;;    * DUAL-FLEXIBLE  When 'on', all parameters of the architecture can be
;;;                     modified on-line and the changes will take effect
;;;                     immediately.  When 'off', some of the flexibility
;;;                     is traded for efficiency.  In particular, the
;;;                     consumption of all symbolic operations is computed
;;;                     at compile time and is wired into the symbolic
;;;                     routines.  (See CONSUM.LSP and ARCHIT/BRACKETS.LSP.)
;;;                     WARNING!!!
;;;                     WARNING!!! When this feature is off, you should make
;;;                     WARNING!!! sure that _all_ consumption proclamations
;;;                     WARNING!!! of one and the same operation have the same
;;;                     WARNING!!! value.  (You may have overlapping proclam's
;;;                     WARNING!!! in several files, e.g. DUAL/CONSUM.LSP and
;;;                     WARNING!!! MY_MODEL/CONSUM.LSP, but when you switch
;;;                     WARNING!!! DUAL-FLEXIBLE off, you should modify the
;;;                     WARNING!!! proclamations in DUAL/CONSUM.LSP according
;;;                     WARNING!!! to your needs.
;;;                     WARNING!!! It was possible to achieve identical behavior
;;;                     WARNING!!! of the AMBR model with FLEXIBLE on and off.
;;;                     WARNING!!!
;;;                     Note also that timing experiments showed that turning
;;;                     DUAL-FLEXIBLE off led to negligible (about 1%) speedup.
;;;
;;;
;;;  Previous versions also supported DUAL-VERBOSE compilation policy.  It,
;;;  however, proved not very useful and is abolished in the current version.
;;;
;;;  Additional control is of course available through the standard OPTIMIZE
;;;  declarations (see p.231 in CLtL2).
;;;  DUAL-specific compilation policies are implemented at READ level via #+
;;;  and #-. In other words, to switch a feature on one must push it to the
;;;  *FEATURES* list; to switch it off, delete it from the list.
;;;
;;;*** YOU MUST RECOMPILE WHENEVER YOU HAVE CHANGED ANY COMPILATION POLICY  ***
;;;
;;;
;;;  SUGGESTED CONFIGURATIONS
;;;  ~~~~~~~~~~~~~~~~~~~~~~~~
;;;  To debug, extend and analyse the program, use the following configuration:
;;;     (pushnew :DUAL-debug    *features*)
;;;     (pushnew :DUAL-flexible *features*)
;;;   ; (declaim (optimize (speed 0) (safety 3) (debug 3)) )
;;;
;;;  While tuning a debugged model, use:
;;;     (setq *features* (delete :DUAL-debug *features*))
;;;     (pushnew :DUAL-flexible *features*)
;;;   ; (declaim (optimize (speed 1) (safety 1) (debug 0)) )
;;;
;;;  While performing voluminous experiments with the tuned model, use:
;;;     (setq *features* (delete :DUAL-debug    *features*))
;;;     (setq *features* (delete :DUAL-flexible *features*))
;;;   ; (declaim (optimize (speed 3) (safety 0) (debug 0)) )
;;;

;;;  (With respect to the forms  (DECLAIM (OPTIMIZE ...)) , note that Common
;;;   Lisp specification (p. 223 in CLtL2) states that "[I]t is unspecified
;;;   whether or not the compile-time side effects of a DECLAIM persist after
;;;   the file has been compiled.  Check whether your OPTIMIZE declarations
;;;   are in force outside the file DUAL/START_ME.LSP.  When they do, however,
;;;   it is difficult to get rid of them, e.g., when compiling a DUAL-based
;;;   model.  Therefore, I recommend that you control the compiler through
;;;   its implementation-dependent interface rather than through declarations.)


;;; Current configuration
(cl:eval-when (compile load eval)
;  (cl:setq cl:*features* (cl:delete :DUAL-debug  cl:*features*))
  (cl:pushnew :DUAL-debug    cl:*features*)

;  (cl:setq cl:*features* (cl:delete :DUAL-flexible  cl:*features*)) ; WARNING!
  (cl:pushnew :DUAL-flexible cl:*features*)

; (cl:declaim (optimize (speed 2) (safety 0) #+:CMU (debug 2)) )
)

;;;;  End of file DUAL/START_ME.LSP
