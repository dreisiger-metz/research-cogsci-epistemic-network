;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User -*-

;;; FILE:       AMBR/defsys.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Definition of the AMBR system (in Mark Kantrowitz' format).
;;; DEPENDS-ON: DUAL
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    29-05-97
;;; UPDATED:    21-12-97 [2.1]
;;; UPDATED:    11-02-98 [2.2.0]
;;;
;;; UPDATED:    13-03-98 [2.2.1]
;;; UPDATED:    01-05-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...



    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;                                                          ;;;;
    ;;;;     ASSOCIATIVE  MEMORY-BASED  REASONING  (A M B R)      ;;;;
    ;;;;                                                          ;;;;
    ;;;;     Description of the files and their dependencies      ;;;;
    ;;;;                                                          ;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;  This file defines AMBR implementation as an interrelated system of files.
;;;  It uses Mark Kantrowitz' DEFSYSTEM -- a tool for organizing systems into
;;;  hierarchical layers of modules, with matching directory structure (an
;;;  analog to MAKE in C).  DEFSYSTEM can be downloaded from:
;;;    ftp.cs.cmu.edu/afs/cs.cmu.edu/user/mkant/Defsystem/defsystem.{text,lisp}
;;;
;;;  I thank Mark Kantrowitz for designing and implementing this convenient
;;;  tool and for placing it into the public domain.
;;;
;;;  To use DEFSYSTEM:
;;;    1. Load the file containing the implementation of DEFSYSTEM (or better,
;;;       arrange it to be loaded automatically whenever Lisp is started).
;;;    2. Load this file (AMBR/DEFSYS.LSP).  It will provide the definition
;;;       of a system named "ambr". (You need to do this only once per session.)
;;;    3. Use the function OPERATE-ON-SYSTEM (or OOS for short) to do things
;;;       to your system.  In particular:
;;;         -- (OOS "ambr" :LOAD)     will load AMBR
;;;         -- (OOS "ambr" :COMPILE)  will compile-and-load it.
;;;
;;;  (Note: You may arrange a 'central registry' with system definition files.
;;;         If you do this, step 2. becomes unnecessary -- a system's definition
;;;         is loaded automagically whenever you operate on it.  See Mark
;;;         Kantrowitz' documentation for details. )
;;;
;;;  If you do not have DEFSYSTEM installed, you will need to compile and/or
;;;  load files manually in the order of their definition below.  See the
;;;  file DUAL/DEFSYS.LSP for an alternative idea.


;;;  AMBR is a cognitive model built on top of the architecture DUAL.
;;;  See the file AMBR/README.TXT for more information and references.
;;;
;;;  See the files in the DUAL directory for more details on the implementation
;;;  of the architecture.  The file DUAL/START_ME.LSP is a good place to start.
;;;
;;;  The "ambr" system depends on "dual" system (described in DUAL/DEFSYS.LSP).
;;;  OPERATE-ON-SYSTEM requires "dual" as a preparation for "ambr".
;;;  Thus, you may simply use  (oos "ambr" :load)  and it would 'automagically'
;;;  load "dual" too.

;;;  The 'knowledge base' of AMBR models is organized in a separate system
;;;  that depends on "ambr" -- see file AMBR/KB/DEFSYS.LSP.



;;;  MODULES of AMBR's implementation
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;  :system "ambr"
;;;    :module  "basic"           ; main files of AMBR implementation
;;;       :file "packages"        ; define packages used in AMBR
;;;       :file "defs"            ; global variables and constants
;;;       :file "proclaim"        ; proclaim consumptions, default weights, etc.
;;;       :file "ambr_ag"         ; define main classes of AMBR agents
;;;       :file "krepres"         ; functions related to the knowledge repres'n
;;;       :file "fizzle"          ; voluntary destruction of a temporary agent
;;;       :file "corresp"         ; correspondence-agents
;;;       :file "goalinpt"        ; goal and input lists
;;;       :file "marker"          ; marker passing mechanism
;;;       :file "secretar"        ; secretaries
;;;       :file "hypoth"          ; hypothesis-agents -- embryos, mature, etc.
;;;       :file "csnet"           ; constraint satisfaction network
;;;       :file "str_corr"        ; structure-correspondence mechanism
;;;       :file "weak_sc"         ; str.corr creating only links, not hyps
;;;       :file "skolem1"         ; skolemiztion mechanism, part 1  ; see AMBR/
;;;       :file "skolem2"         ; skolemiztion mechanism, part 2  ; SKOLEM.TXT
;;;       :file "rating"          ; rating mechanism
;;;       :file "promotn"         ; promotion mechanism
;;;       :file "promprop"        ; promotion propagation mechanism
;;;       :file "toplevel"        ; macro-level structures of the model
;;;    :module  "interface"       ; utilities for running the model
;;;       :file "defs"            ; global variables used in the interface
;;;       :file "krepres"         ; coalitions, meso-frames, etc.
;;;       :file "mapping"         ; mapping and retrieval indices
;;;       :file "genkb"           ; generating KB variants via GENKB-templates
;;;       :file "set_spy"         ; verbose and counting spy tools
;;;       :file "report"          ; print summaries about WM, indices, etc.

(make:defsystem "ambr"
  :device           #.(car '(#+:ACLPC       "D:"
                             #|otherwise|#  nil   ))
  :source-pathname  #.(car '(#+:ACLPC       "\\dual\\ambr\\"
                             #+:CMU         "/home/alex/dual/programs/ambr/"
                             #|otherwise|#  "dual;ambr"   ))
  :source-extension #.(car '(#+:ACLPC       "lsp"
                             #+:CMU         "lisp"
                             #|otherwise|#  "lisp"   ))
  :binary-pathname  #.(car '(#+:ACLPC       "\\dual\\fsl\\ambr\\"
                             #+:CMU   "/home/alex/dual/programs/.compiled/ambr/"
                             #|otherwise|#  "dual;fasl;ambr"   ))
  :binary-extension #.(car '(#+:ACLPC       "fsl"
                             #+:CMU         "sparcf"
                             #|otherwise|#  "fasl"   ))
  :depends-on  ("dual")
  :components
  ((:module  "basic"
      :source-pathname ""                ; AMBR's directory
      :binary-pathname ""                ; .../compiled/
      :depends-on  nil
      :components  ((:file  "packages")
                    (:file  "defs"
                       :depends-on ("packages"))
                    (:file  "proclaim"
                       :depends-on ("packages"))
                    (:file  "ambr_ag"
                       :depends-on ("defs" "proclaim"))
                    (:file  "krepres"
                       :depends-on ("ambr_ag"))
                    (:file  "fizzle"
                       :depends-on ("ambr_ag"))
                    (:file  "corresp"
                       :depends-on ("ambr_ag" "krepres"))
                    (:file  "goalinpt"
                       :depends-on ("ambr_ag" "krepres"))
                    (:file  "marker"
                       :depends-on ("corresp" "goalinpt"))
                    (:file  "secretar"
                       :depends-on ("ambr_ag" "corresp" "fizzle"))
                    (:file  "hypoth"
                       :depends-on ("corresp" "marker" "secretar" "fizzle"))
                    (:file  "csnet"
                       :depends-on ("defs" "ambr_ag"))
                    (:file  "weak_sc"
                       :depends-on ("secretar" "hypoth"))
                    (:file  "str_corr"
                       :depends-on ("corresp" "hypoth" "weak_sc"))
                    (:file  "skolem1"
                       :depends-on ("krepres" "corresp" "marker"))
                    (:file  "skolem2"
                       :depends-on ("skolem1" "hypoth"))
                    (:file  "rating"
                       :depends-on ("secretar" "hypoth" "skolem1"))
                    (:file  "promotn"
                       :depends-on ("rating"))
                    (:file  "promprop"
                       :depends-on ("promotn" "marker"))
                    (:file  "toplevel"
                       :depends-on ("defs" "ambr_ag"))
   ))
   (:module  "interface"
      :source-pathname "intrface"
      :binary-pathname "intrface"
      :depends-on  ("basic")
      :components  ((:file  "defs")
                    (:file  "krepres"
                       :depends-on ("defs"))
                    (:file  "mapping"
                       :depends-on ("defs"))
                    (:file  "genkb"
                       :depends-on ("defs"))
                    (:file  "report"
                       :depends-on ("mapping"))
                    (:file  "set_spy"
                       :depends-on nil)
   ))
   ;; module "experiment"
   (:file  "version"
      :depends-on ("basic" "interface"))
))  ; end of AMBR system


;;;;;---------
;;
;;  See the end of the file DUAL/DEFSYSTSM.LSP for a method to load
;;  files without recourse to Mark Kantrowitz' DEFSYSTEM.
;;  Generally, the files can be loaded in the order they are listed above.
;;


;;;;  End of file AMBR/DEFSYS.LSP
