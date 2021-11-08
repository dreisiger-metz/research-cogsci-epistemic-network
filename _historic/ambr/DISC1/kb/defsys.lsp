;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User -*-

;;; FILE:       AMBR/KB/defsys.lsp
;;; VERSION:    3.0.0    ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Definition of the knowledge base for AMBR model(s).
;;; DEPENDS-ON: DUAL, AMBR
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    21-12-97 [1.1]
;;; UPDATED:    14-04-98 [3.0]  Split into several files using INTRFACE/KB_UTIL.
;;; UPDATED:    14-08-98 [3.0.0] The 'official release'
;;; UPDATED:    ...
;;;;;;;;;
;;; TO DO:      Include AMBR/KB/SIT_CODE.LSP in the system definition.


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;                                                        ;;;;
    ;;;;         A M B R   K N O W L E D G E   B A S E          ;;;;
    ;;;;                                                        ;;;;
    ;;;;               Description of the files                 ;;;;
    ;;;;                                                        ;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;  This file defines the dependencies between the pieces of the AMBR
;;;  knowledge base.  It defines a system named 'ltm' that depends on the
;;;  system 'ambr' (see AMBR/DEFSYS.LSP).  You may use  (oos "ltm" :load)  to
;;;  'automagically' load everything -- the programs and the knowledge base.

;;;  The long-term memory consists of two main sections: SEMANTIC and EPISODIC.
;;;  This partitioning is conventional.  In reality, there isn't any boundary
;;;  between the two memories -- there is simply a unified pool of agents.
;;;  There are many links between the agents, including links between agents
;;;  ascribed to different memories.
;;;
;;;  The semantic memory consists mainly (although not exclusively) of agents
;;;  of :type :concept.  There are also some 'free standing' instance agents.
;;;
;;;  The episodic memory consists exclusively of agents of :type :instance.
;;;  Most of these agents are 'affiliated' to situations (see AMBR/CORRESP.LSP
;;;  and KB/ABSTRACT.LSP).  Situations are represented by coalitions of agents.
;;;  Usually, there is a 'situation agent' serving as a 'head' of each situatn.
;;;  Most situations belong to the long-term memory and are represented by
;;;  permanent agents.  There are also 'target' situations which are temporary.



;;;  MODULES of the knowledge base, ver 3.0.0
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;;  :system "semantic"     ; the semantic part of the long-term memory
;;;    :file  "abstract"    ; most abstract concepts: THING, OBJECT, etc.
;;;    :file  "temp_rel"    ; temporal relations: FOLLOWS, BEFORE, etc.
;;;    :file  "spat_rel"    ; spatial relations:  LEFT-OF, LONG, etc.
;;;    :file  "phys_rel"    ; physical relations:  PART-OF, PROTECTS,etc.
;;;    :file  "physprop"    ; physical properties: COLOR-OF, IS-RED, etc.
;;;    :file  "liquid"      ; liquids and gases: WATER, RED-WINE, AIR, etc.
;;;    :file  "contain"     ; containers: BAG, BOX, CUP, BOTTLE, etc.
;;;    :file  "food"        ; food: BREAD, FRUIT, VEGETABLE, etc.
;;;    :file  "animals"     ; living beings: ANIMAL, BIRD, COW, etc.
;;;    :file  "kitchen"     ; kitchen and cooking: SAUCEPAN, FRIDGE, etc.
;;;    :file  "forest"      ; forest objects: FIRE, STONE, KNIFE, etc.
;;;    :file  "furnit"      ; furniture: TABLE, CHAIR, BED, etc.
;;;    :file  "..."         ; et cetera ad stuporem ...
;;;
;;;  :system  "base-sit"    ; permanent episodes and situations in LTM
;;;  ;; See also AMBR/KB/EPISODIC/SIT_LIST.TXT.
;;;    :file  "b_WTP"       ; base sit WTP -- Water in a Teapot on a hot Plate.
;;;    :file  "b_BF"        ; base sit BF  -- Bowl on a Fire burns down.
;;;    :file  "b_GP"        ; base sit GP  -- Glass on a hot Plate breaks.
;;;    :file  "b_IHC"       ; base sit IHC -- Imm.Heater in a Cup with tea.
;;;    :file  "b_MTF"       ; base sit MTF -- Milk in a Teapot in a Fridge.
;;;    :file  "b_ICF"       ; base sit ICF -- Ice Cube in a glass in a Fridge.
;;;    :file  "b_BPF"       ; base sit BPF -- Butter on a Plate in a Fridge.
;;;    :file  "b_FDO"       ; base sit FDO -- Food on a Dish in an Oven.
;;;    :file  "b_STC"       ; base sit STC -- Sugar in Tea in a Cup.
;;;    :file  "b_SFF"       ; base sit SFF -- Salt in Food on a plate in a Frge.
;;;    :file  "b_ERW"       ; base sit ERW -- Egg in Red Water.
;;;    :file  "b_GWB"       ; base sit GWB -- Glass in a Wooden Box.
;;;    :file  "..."         ; ...


(make:defsystem "semantic"
  :device           #.(car '(#+:ACLPC       "D:"
                             #|otherwise|#  nil   ))
  :source-pathname  #.(car '(#+:ACLPC       "\\dual\\ambr\\kb\\semantic\\"
                             #+:CMU "/home/alex/dual/programs/ambr/kb/semantic/"
                             #|otherwise|#  "dual;ambr;kb;semantic"   ))
  :source-extension #.(car '(#+:ACLPC       "lsp"
                             #+:CMU         "lisp"
                             #|otherwise|#  "lisp"   ))
  :binary-pathname  #.(car '(#+:ACLPC       "\\dual\\fsl\\ambr\\kb\\semantic\\"
                             #+:CMU
                         "/home/alex/dual/programs/.compiled/ambr/kb/semantic/"
                             #|otherwise|#  "dual;fasl;ambr;kb;semantic"   ))
  :binary-extension #.(car '(#+:ACLPC       "fsl"
                             #+:CMU         "sparcf"
                             #|otherwise|#  "fasl"   ))
  :depends-on  ("ambr")
  :components ((:file  "abstract")
               (:file  "temp_rel"
                  :depends-on  ("abstract"))
               (:file  "spat_rel"
                  :depends-on  ("abstract"))
               (:file  "phys_rel"
                  :depends-on  ("spat_rel"))
               (:file  "physprop"
                  :depends-on  ("phys_rel"))
               (:file  "liquid"
                  :depends-on  ("abstract" "physprop"))
               (:file  "contain"
                  :depends-on  ("abstract" "phys_rel"))
               (:file  "food"
                  :depends-on  ("liquid" "contain"))
               (:file  "animals"
                  :depends-on  ("abstract" "physprop"))
               (:file  "kitchen"
                  :depends-on  ("physprop" "contain" "food"))
               (:file  "forest"
                  :depends-on  ("phys_rel" "kitchen"))
               (:file  "furnit"
                  :depends-on  ("abstract"))
               ;; ...
  ))  ; end of SEMANTIC system


(make:defsystem "base-sit"
  :device           #.(car '(#+:ACLPC       "D:"
                             #|otherwise|#  nil   ))
  :source-pathname  #.(car '(#+:ACLPC       "\\dual\\ambr\\kb\\episodic\\"
                             #+:CMU "/home/alex/dual/programs/ambr/kb/episodic/"
                             #|otherwise|#  "dual;ambr;kb;episodic"   ))
  :source-extension #.(car '(#+:ACLPC       "lsp"
                             #+:CMU         "lisp"
                             #|otherwise|#  "lisp"   ))
  :binary-pathname  #.(car '(#+:ACLPC       "\\dual\\fsl\\ambr\\kb\\episodic\\"
                             #+:CMU
                         "/home/alex/dual/programs/.compiled/ambr/kb/episodic/"
                             #|otherwise|#  "dual;fasl;ambr;kb;episodic"   ))
  :binary-extension #.(car '(#+:ACLPC       "fsl"
                             #+:CMU         "sparcf"
                             #|otherwise|#  "fasl"   ))
  :depends-on  ("semantic")
  :components ((:file  "b_wtp")    ; WTP -- Water in a Teapot on a hot Plate.
               (:file  "b_bf" )    ; BF  -- Bowl on a Fire burns down.
               (:file  "b_gp" )    ; GP  -- Glass on a hot Plate breaks.
               (:file  "b_ihc")    ; IHC -- Imm.Heater in a Cup with tea.
               (:file  "b_mtf")    ; MTF -- Milk in a Teapot in a Fridge.
               (:file  "b_icf")    ; ICF -- Ice Cube in a glass in a Fridge.
               (:file  "b_bpf")    ; BPF -- Butter on a Plate in a Fridge.
               (:file  "b_fdo")    ; FDO -- Food on a Dish in an Oven.
               (:file  "b_stc")    ; STC -- Sugar in Tea in a Cup.
               (:file  "b_sff")    ; SFF -- Salt in Food on a plate in a Fridge.
               (:file  "b_erw")    ; ERW -- Egg in Red Water.
               (:file  "b_gwb")    ; GWB -- Glass in a Wooden Box.
               ;; ...
  ))  ; end of BASE-SIT system


(make:defsystem "ltm"
  :device           #.(car '(#+:ACLPC       "D:"
                             #|otherwise|#  nil   ))
  :source-pathname  #.(car '(#+:ACLPC       "\\dual\\ambr\\kb\\"
                             #+:CMU "/home/alex/dual/programs/ambr/kb/"
                             #|otherwise|#  "dual;ambr;kb"   ))
  :source-extension #.(car '(#+:ACLPC       "lsp"
                             #+:CMU         "lisp"
                             #|otherwise|#  "lisp"   ))
  :binary-pathname  #.(car '(#+:ACLPC       "\\dual\\fsl\\ambr\\kb\\"
                             #+:CMU
                         "/home/alex/dual/programs/.compiled/ambr/kb/"
                             #|otherwise|#  "dual;fasl;ambr;kb"   ))
  :binary-extension #.(car '(#+:ACLPC       "fsl"
                             #+:CMU         "sparcf"
                             #|otherwise|#  "fasl"   ))
  :depends-on  ("semantic"   ; semantic portion of LTM
                "base-sit" ) ; episodic portion of LTM
  :components ((:file  "version")
  ))  ; end of LTM system

;; May define additional systems with various target problems.


;;;;  End of file AMBR/KB/DEFSYS.LSP
