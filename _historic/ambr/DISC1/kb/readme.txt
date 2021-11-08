;;; -*- Mode: ASCII Text -*-

;;; FILE:       AMBR/KB/readme.txt
;;; VERSION:    2.2.2    ; see AMBR/KB/VERSION.LSP
;;; PURPOSE:    Getting started with AMBR's implementation.
;;; DEPENDS-ON: NIL
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    14-08-98 [2.2.2]  The 'official release'
;;; UPDATED:    ...


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;                                                                  ;;;;
  ;;;;   I N S T A L L I N G   A M B R   K N O W L E D G E   B A S E    ;;;;
  ;;;;                                                                  ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



INSTALLATION
~~~~~~~~~~~~~
 Installation of AMBR's knowledge base amounts to four broad steps:
   1. You should install the architecture DUAL and the model AMBR first.
      See files DUAL/README.TXT and AMBR/README.TXT for instructions.
   2. You should unpack the distributive archive and create the directory
      structure.  The breakup of files into directories is explained below.
   3. You should arrange that the source files are (compiled and) loaded in
      appropriate order.
   4. You should generate at least one KB variant (see AMBR/INTRFACE/GENKB.LSP)
      or install the variants contained in the AMBR_EXP archive and load one
      of them. (KB variants must be loaded one at a time.)

 After you install the permanent memory (LTM), you will probably want to load
 a target problem.  Targets are stored in separate files and are loaded
 manually one at a time (i.e. there is no DEFSYSTEM definition for them).
 Look for file names of the form T_*.LSP in the directory AMBR/KB/EPISODIC/.
 (Base analogs are stored in files whose names begin with 'B_' in the same dir.)

 After loading a target problem, you must 'attach' it using the function
 ATTACH-TARGET (no arguments).  When the target is attached, the model is
 ready to run -- try the functions G and MT and watch the fun.
 (See file AMBR/INTRFACE/REPORT.LSP for other useful interface functions.)


 DIRECTORY STRUCTURE  (step 2)
 ------------------------------
 AMBR's knowledge base is defined in more than 40 files grouped in several
 directories.  The directory structure is important, as file names alone are
 not always enough to identify the files.

 I recommend the following directory structure:
   DUAL_ROOT
     +-------- DUAL                ; DUAL architecture, see DUAL/README.TXT
     |          +-- ...
     |
     +-------- AMBR                ; AMBR model, see AMBR/README.TXT
     |          +-- ...
     |          +-- KB             ; AMBR's knowledge base
     |               +-- SEMANTIC  ; definitions of various concepts
     |               +-- EPISODIC  ; definitions of various episodes
     |               +-- GENKB     ; KB variants (see AMBR/INTRFACE/GENKB.LSP)
     |
     +-------- AMBR_EXP            ; AMBR experiments
     |          +-- ...
     |
     +-------- FASL                ; an identical directory tree keeping the
                +-- DUAL           ;  compiled (FASL) versions of LISP sources
                |    +-- ...
                |
                +-- AMBR
                |    +-- ...
                |    +-- KB
                |         +-SEMANTIC
                |         +-EPISODIC
                +-- ...



 COMPILING AND LOADING THE SYSTEM  (step 3)
 -------------------------------------------
 The file AMBR/KB/DEFSYS.LSP defines the dependencies between the various files
 that make up AMBR's knowledge base. It uses Mark Kantrowitz' DEFSYSTEM -- a
 tool for organizing systems into hierarchical layers of modules, with matching
 directory structure (an analog to MAKE in C).  The LTM system depends on AMBR
 system, which in turn depends on DUAL. system.  See file DUAL/README.TXT for
 instruction regarding DEFSYSTEM.

 In a nutshell, after you compile and load DUAL (as explained in DUAL/README)
 the following commands are useful:
   -- (OOS "semantic" :LOAD)     will load the files from AMBR/KB/SEMANTIC
   -- (OOS "semantic" :COMPILE)  will compile-and-load them
   -- (OOS "base-sit" :LOAD)     will load the base episodes (EPISODIC/B_*.LSP)
   -- (OOS "base-sit" :COMPILE)  will compile-and-load them
   -- (OOS "ltm" :LOAD)          will load "semantic" followed by "base-sit"
   -- (OOS "ltm" :COMPILE)       will compile-and-load them

 If all files are loaded OK, you should see a message like this:
    ;;
    ;; Knowledge base version 3.0.0 of 24 July 1998.
    ;; (c) Boicho Kokinov and Alexander Petrov
    ;;     Cognitive Science Department
    ;;     New Bulgarian University
    ;;     {kokinov,apetrov}@cogs.nbu.acad.bg
    ;;

 (Tip for Allegro CL users: A convenient way to load things automatically at )
 (  the beginning of the session is to place the respective FSL files in the )
 (  directory ALLEGRO/UPDATE. This is the directory for patches and Allegro  )
 (  loads whatever FSL file it sees there, in alphabetical order(?).         )
 (  I use the following arrangement:  I compile Mark Kantrowitz' DEFSYSTEM   )
 (  to produce a FSL file and save it as ALLEGRO/UPDATE/DEFSYS.FSL.  Then,   )
 (  I compile the DUAL system definition file (i.e.DUAL/DEFSYS.LSP) and save )
 (  the FSL file under the name ALLEGRO/UPDATE/MK1_DUAL.FSL. The definitions )
 (  of AMBR and KB are compiled to produce ALLEGRO/UPDATE/MK2_AMBR.FSL and   )
 (  =/MK3_KB.FSL, respectively.  Under this arrangement, whenever Allegro is )
 (  started all system definitions are loaded in the appropriate order.      )
 (  A single command -- (oos "ltm" :load) -- then loads DUAL, AMBR, and KB.  )



WHERE TO GO NEXT
~~~~~~~~~~~~~~~~~
 If you want to replicate some of the experiments documented in the Ph.D.
 Thesis of Alexander Petrov, follow the instructions in AMBR_EXP/README.TXT.

 See file AMBR/KB/DEFSYS.LSP for a description of the files comprising the
 knowledge base of AMBR.
 See files AMBR/KB/SEMANTIC/FILELIST.TXT and AMBR/KB/EPISODIC/FILELIST.TXT
 for descriptions of individual KB files in each subdirectory.
 See file AMBR/KB/VERSION.LSP for version information.
 See file AMBR/INTRFACE/GENKB.LSP for the function GENERATE-KB that generates
 KB variants and stores them in the AMBR/KB/GENKB directory (set the
 parameter *GENKB-PREFIX* in AMBR/KB/SIT_CODE.LSP accordingly).
 See file AMBR/INTRFACE/REPORT.LSP for functions that are most frequently
 used while experimenting with the model.

 See file DUAL/README.TXT to get started with the DUAL architecture.
 See file AMBR/README.TXT to get started with the AMBR model.


;;;;  End of file AMBR/KB/README.TXT
