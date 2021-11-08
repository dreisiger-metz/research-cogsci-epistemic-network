;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User -*-

;;; FILE:       DUAL/defsys.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Definition of the DUAL system (in Mark Kantrowitz' format).
;;; DEPENDS-ON: NIL  (except DEFSYSTEM.LSP itself, of course)
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    21-01-97 [1.0]
;;; UPDATED:    13-09-97 [1.1.0]
;;; UPDATED:    25-01-98 [1.1.1]
;;; UPDATED:    12-03-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                   ;;;;
;;;;    D  U  A  L     C O G N I T I V E   A R C H I T E C T U R E     ;;;;
;;;;                                                                   ;;;;
;;;;          Description of the files and their dependencies          ;;;;
;;;;                                                                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;  See DUAL/README.TXT for an introduction to DUAL implementation.
;;;  See DUAL/VERSION.LSP to see the current version number.

;;;  This file defines DUAL implementation as an interrelated system of files.
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
;;;    2. Load this file (DUAL/DEFSYS.LSP).  It will provide the definition
;;;       of a system named "dual". (You need to do this only once per session.)
;;;    3. Use the function OPERATE-ON-SYSTEM (or OOS for short) to do things
;;;       to your system.  In particular:
;;;         -- (OOS "dual" :LOAD)     will load DUAL
;;;         -- (OOS "dual" :COMPILE)  will compile-and-load it.
;;;
;;;  (Note: You may arrange a 'central registry' with system definition files.
;;;         If you do this, step 2. becomes unnecessary -- a system's definition
;;;         is loaded automagically whenever you operate on it.  See Mark
;;;         Kantrowitz' documentation for details. )
;;;
;;;  You may use DUAL even if you do not have DEFSYSTEM installed.  See the
;;;  addendum at the end of this file.


;;;  MODULES of DUAL's implementation
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;  :system "dual"
;;;    :module  "basic"                  ; defvar's, macros, etc.
;;;    :module  "architecture"           ; the architecture itself
;;;       :module  "basic-archit"        ; basic housekeeping functions
;;;       :module  "connectionist"       ; microlevel, connectionist aspect
;;;       :module  "symbolic"            ; microlevel, symbolic aspect
;;;          :module  "symb-decl"        ; micro-frame
;;;          :module  "symb-proc"        ; suspendable symbolic processing
;;;       :module  "macrolevel"          ; LTM, WM, visual array, etc.
;;;       :module  "agents"              ; special kinds of agents
;;;    :module  "interface"              ; TTY interface, portable
;;;    :module  "final"                  ; putting everything together

;;; // :module  "graphic-interface"      ; GUI, not necessarily portable



(make:defsystem "dual"
  :device           #.(car '(#+:ACLPC       "D:"
                             #|otherwise|#  nil   ))
  :source-pathname  #.(car '(#+:ACLPC       "\\dual\\dual\\"
                             #+:CMU         "/home/alex/dual/programs/dual/"
                             #|otherwise|#  "dual;dual"   ))
  :source-extension #.(car '(#+:ACLPC       "lsp"
                             #+:CMU         "lisp"
                             #|otherwise|#  "lisp"   ))
  :binary-pathname  #.(car '(#+:ACLPC       "\\dual\\fsl\\dual\\"
                             #+:CMU   "/home/alex/dual/programs/.compiled/dual/"
                             #|otherwise|#  "dual;fasl;dual"   ))
  :binary-extension #.(car '(#+:ACLPC       "fsl"
                             #+:CMU         "sparcf"
                             #|otherwise|#  "fasl"   ))
  :depends-on nil
  :components
  ((:module  "basic"
      :source-pathname ""                ; DUAL's directory
      :binary-pathname ""                ; .../fasl/...
      :depends-on  nil
      :components  ((:file  "start_me")
                    (:file  "nonport1")
                    (:file  "packages"
                       :depends-on ("nonport1"))
                    (:file  "nonport2"
                       :depends-on ("packages"))
                    (:file  "defs"
                       :depends-on ("start_me" "packages" "nonport2"))
                    (:file  "general"
                       :depends-on ("packages" "nonport2"))
                    (:file  "utils"
                       :depends-on ("general"))
                    (:file  "proclaim"
                       :depends-on ("defs"))
                    (:file  "consum"
                       :depends-on ("proclaim"))
                    (:file  "labels"
                       :depends-on ("proclaim"))
   ))
   (:module  "architecture"
      :source-pathname "archit"
      :binary-pathname "archit"
      :depends-on  ("basic")
      :components
      ((:module  "basic-archit"
          :source-pathname ""            ; ARCHIT directory
          :binary-pathname ""
          :components  ((:file "basic")
                        (:file "symref"
                           :depends-on ("basic"))
       ))
       (:module  "connectionist"
          :source-pathname ""            ; ARCHIT directory
          :binary-pathname ""
          :depends-on  ("basic-archit")
          :components  ((:file "act_fun")
                        (:file "conref")
                        (:file "connect"
                           :depends-on ("act_fun" "conref"))
       ))
       (:module  "symbolic"
          :source-pathname ""            ; ARCHIT directory
          :binary-pathname ""
          :depends-on  ("basic-archit"
                        "connectionist")
          :components
          ((:module  "symb-decl"         ; submodule for micro-frames
              :source-pathname ""            ; ARCHIT directory
              :binary-pathname ""
              :depends-on  nil
              :components  ((:file "filler")
                            (:file "slots"
                               :depends-on ("filler"))
                            (:file "mcrframe"
                               :depends-on ("filler" "slots"))
                            (:file "links"
                               :depends-on ("filler" "slots" "mcrframe"))
           ))
           (:module  "symb-proc"         ; submodule for symbolic processors
              :source-pathname ""            ; ARCHIT directory
              :binary-pathname ""
              :depends-on  nil
              :components  ((:file "symproc1")
                            (:file "agenda"
                               :depends-on ("symproc1"))
                            (:file "suspend")
                            (:file "brackets"
                               :depends-on ("suspend"))
                            (:file "mailbox"
                               :depends-on ("suspend" "brackets"))
                            (:file "sprogn"
                               :depends-on ("agenda" "mailbox"))
                            (:file "symproc2"
                               :depends-on ("agenda" "sprogn"))
           ))
           (:file "symbolic"
                  :depends-on ("symb-decl" "symb-proc"))
       ))
       (:module  "macrolevel"
          :source-pathname ""            ; ARCHIT directory
          :binary-pathname ""
          :depends-on  ("connectionist" "symbolic")
          :components  ((:file "hybrid")
                        (:file "work_mem")
                        (:file "dual_ag"
                           :depends-on ("hybrid" "work_mem"))
                        (:file "spread"
                           :depends-on ("work_mem" "dual_ag"))
                        (:file "time"
                           :depends-on ("dual_ag"))
                        (:file "metronom"
                           :depends-on ("time" "work_mem"))
       ))
       (:module  "agents"
          :source-pathname ""
          :binary-pathname ""
          :depends-on ("connectionist" "symbolic" "macrolevel")
          :components ((:file "temp_ag")     ; temporary agents
                       (:file "nc_agent"     ; node constructors
                          :depends-on ("temp_ag"))
                       (:file "mp_agent"     ; marker passers
                          :depends-on ("nc_agent"))
       ))
   )) ; end of module "architecture"
   (:module  "interface"
      :source-pathname "intrface"
      :binary-pathname "intrface"
      :depends-on  ("basic" "architecture")
      :components  ((:file  "defs")                ; not the same as ./DEFS
                    #-:CMU (:file  "prnfloat"      ; CMU printer is not in CLOS
                            :depends-on ("defs"))
                    (:file  "connect")
                    (:file  "spread"
                       :depends-on ("defs"))
                    (:file  "agenda"
                       :depends-on ("defs"))
                    (:file  "time")
                    (:file  "dummy_ag"
                       :depends-on ("defs"))
                    (:file  "readfilr"
                       :depends-on ("dummy_ag"))
                    (:file  "defagent"
                       :depends-on ("readfilr"))
                    (:file  "with_ag"
                       :depends-on ("defagent"))
                    (:file  "kb_util"
                       :depends-on ("dummy_ag" "connect"))
                    (:file  "coalitn"
                       :depends-on ("defs"))
                    (:file  "gfun_spy"
                       :depends-on nil)
                    (:file  "spy_tool"
                       :depends-on ("gfun_spy" "coalitn"))
                    (:file  "verbose"
                       :depends-on ("spy_tool"))
         ;;           (:file  "counter"
         ;;              :depends-on ("spy_tool"))
                   (:file  "set_spy"
                       :depends-on ("verbose"))
   ))
   ;; module "graphic-interface"
   (:module  "final"
      :source-pathname ""                ; DUAL's directory
      :binary-pathname ""                ; .../fasl/...
      :depends-on  ("architecture" "interface")
      :components  ((:file "toplevel")
                    (:file "version" )
   ))
  ))  ; end of DUAL system


;;;  ******************************************************************
;;;
;;;  A D D E N D U M
;;;  ~~~~~~~~~~~~~~~
;;;  If you do not have DEFSYSTEM installed, you may still use DUAL by
;;;  loading files in the following order:
;;;

#|
(cl:in-package "COMMON-LISP-USER")

(defvar  *DUAL-source-path*  "C:\\DUAL\\"     ; modify according to local needs
  "Path to the sources of DUAL architecture" )

(flet ((load-component (component-name)
         (load (concatenate 'string *DUAL-source-path*
                                    component-name
                                    ".lsp" )
               :verbose nil) ))
  (load-component "start_me" )
  (load-component "nonport1" )
  (load-component "packages" )
  (load-component "nonport2" )
  (load-component "utils" )
  (load-component "defs" )
  (load-component "general" )
  (load-component "proclaim" )
  (load-component "consum" )
  (load-component "labels" )
  (load-component "archit\\basic" )           ; modify directory separator
  (load-component "archit\\symref" )          ;   according to local needs
  (load-component "archit\\conref" )
  (load-component "archit\\act_fun" )
  (load-component "archit\\connect" )
  (load-component "archit\\filler" )
  (load-component "archit\\slots" )
  (load-component "archit\\mcrframe" )
  (load-component "archit\\links" )
  (load-component "archit\\symproc1" )
  (load-component "archit\\agenda" )
  (load-component "archit\\suspend" )
  (load-component "archit\\brackets" )
  (load-component "archit\\mailbox" )
  (load-component "archit\\sprogn" )
  (load-component "archit\\symproc2" )
  (load-component "archit\\symbolic" )
  (load-component "archit\\hybrid" )
  (load-component "archit\\work_mem" )
  (load-component "archit\\dual_ag" )
  (load-component "archit\\spread" )
  (load-component "archit\\time" )
  (load-component "archit\\metronom" )
  (load-component "archit\\temp_ag" )
  (load-component "archit\\nc_agent" )
  (load-component "archit\\mp_agent" )

  (load-component "intrface\\defs" )
  (load-component "intrface\\prnfloat" )
  (load-component "intrface\\connect" )
  (load-component "intrface\\agenda" )
  (load-component "intrface\\spread" )
  (load-component "intrface\\time" )
  (load-component "intrface\\dummy_ag" )
  (load-component "intrface\\readfilr" )
  (load-component "intrface\\defagent" )
  (load-component "intrface\\with_ag" )
  (load-component "intrface\\kb_util" )
  (load-component "intrface\\coalitn" )
  (load-component "intrface\\gfun_spy" )
  (load-component "intrface\\spy_tool" )
  (load-component "intrface\\verbose" )
;  (load-component "intrface\\counter" )
  (load-component "intrface\\set_spy" )

  (load-component "toplevel" )
  (load-component "version" )
)  ; end of FLET
|#

;;;;  End of file DUAL/DEFSYS.LSP
