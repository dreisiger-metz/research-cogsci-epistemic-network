;;; -*- Mode: ASCII Text -*-

;;; FILE:       AMBR/readme.txt
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Getting started with AMBR's implementation.
;;; DEPENDS-ON: NIL
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    14-08-98 [2.2.2]  The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;                                                               ;;;;
   ;;;;      G E T T I N G   S T A R T E D   W I T H   A M B R        ;;;;
   ;;;;                                                               ;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


GENERAL INFORMATION AND REFERENCES
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 This program is a portable implementation of the cognitive model AMBR
 (Associative Memory-Based Reasoning).  AMBR is an dynamic emergent integrated
 model of analogy-making based on the cognitive architecture DUAL.

 See file DUAL/README.TXT for general information and references about DUAL.

---------
 The program files in this folder implement the current version of the model --
 AMBR3.  This third version of AMBR is described in the following document:

 Petrov, A. (1998) "A Dynamic Emergent Model of Analogy-Making Based on
                   Decentralized Representations".  Ph.D. Thesis.
                   New Bulgarian University, Cognitive Science Department

 Previous versions were:
   AMBR2 -- described in:  Petrov, A. (1997)
                           "Extensions of DUAL and AMBR". M.Sc. Thesis.
                           New Bulgarian University, Cognitive Science Dept.

   AMBR1 -- described in:  Kokinov, B. (1994)
                           "A hybrid model of reasoning by analogy".
                           In K.Holyoak & J.Barnden (Eds.),
                              Advances in connectionist and neural computation
                              theory. Vol.2: Analogical connections (pp.247-318)
                              Norwood, NJ: Ablex


 These documents, as well as any additional information, may be obtained from:
     Alexander Petrov:  apetrov@cogs.nbu.acad.bg
     Boicho Kokinov:    kokinov@cogs.nbu.acad.bg

 Anonymous ftp access is forthcoming. Check the site  cogs.nbu.acad.bg/pub/


PACKAGES USED IN THE IMPLEMENTATION
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 There are three packages (name spaces) used in this program.
   -- AMBR-core -- this package exports all 'conceptual' symbols
         and only those symbols. In other words, this package refers
         to the conceptual description of the model as specified in
         the Ph.D. thesis cited above. Implementational details are
         hidden in the package.

   -- AMBR-interface -- supports various tools that are not essential
         to the model as a psychological theory but are needed for
         debugging, interface, off-line batch processing, and the like.

   -- AMBR -- this package is intended to be the 'current package' while
         doing simulation experiments with the model. It uses all other
         packages (DUAL-CORE, DUAL-INTERFACE, AMBR-CORE, and AMBR-INTERFACE).
         The names of the agents (AMBR's main protagonists) are symbols
         from this package.


 See file AMBR/PACKAGES.LSP for more details (and a list of all function
 names, variables, classes, etc.).


MODULES OF THE IMPLEMENTATION
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 AMBR is a complex program (about 4,000 lines of code) and is organized
 into a layered system of files.  See file AMBR/DEFSYS.LSP for details.


INSTALLATION
~~~~~~~~~~~~~
 Installation of AMBR amounts to four broad steps:
   1. AMBR is based on the cognitive architecture DUAL. You should install
      the implementation of the architecture first. See file DUAL/README.TXT
      for instructions on installing DUAL.
   2. You should unpack the distributive archive and create the directory
      structure.  The breakup of files into directories is explained below.
   3. You should arrange that the source files are (compiled and) loaded in
      appropriate order.  There are two ways for doing this: with and without
      DEFSYSTEM.  The former is explained below.  See DUAL/README.TXT and
      AMBR/DEFSYS.LSP for hints for the latter.
   4. In itself, AMBR's implementation defines a taxonomy of agent types and
      a set of methods for various generic functions.  You cannot do much with
      these unless you have concrete agents coding some concrete knowledge.
      You should install the knowledge base (according to the instructions
      given in KB/README.TXT) in order to test AMBR and experiment with it.


 DIRECTORY STRUCTURE  (step 2)
 ------------------------------
 The implementation of AMBR consists of more than 30 files grouped in several
 directories.  The directory structure is important, as file names alone are
 not enough to identify the files (e.g. there are several files named DEFS.LSP).

 I recommend the following directory structure:
   DUAL_ROOT
     +-------- DUAL                ; DUAL architecture, see DUAL/README.LSP
     |          +-- ...
     |
     +-------- AMBR                ; 'core' files of the AMBR model
     |          +-- INTRFACE       ; interface facilities
     |          +-- EXPERMT        ; facilities for running sim. experiments
     |          +-- KB             ; knowledge base of the model, see KB/README
     |               +-- ...
     |
     +-------- AMBR_EXP            ; AMBR experiments
     |          +-- ...
     |
     +-------- FASL                ; an identical directory tree keeping the
                +-- DUAL           ;  compiled (FASL) versions of LISP sources
                |    +-- ...
                |
                +-- AMBR
                |    +-- INTRFACE
                |    +-- EXPERMT
                |    +-- KB
                |         +-...
                +-- ...



 COMPILING AND LOADING THE SYSTEM  (step 3)
 -------------------------------------------
 The file AMBR/DEFSYS.LSP defines the dependencies between the various files
 that make up AMBR. It uses Mark Kantrowitz' DEFSYSTEM -- a tool for organizing
 systems into hierarchical layers of modules, with matching directory structure
 (an analog to MAKE in C).  The AMBR system depends on DUAL system.  See file
 DUAL/README.TXT for instruction regarding DEFSYSTEM.

 In a nutshell, after you compile and load DUAL (as explained in DUAL/README)
 the following commands are useful:
   -- (OOS "ambr" :LOAD)     will load AMBR
   -- (OOS "ambr" :COMPILE)  will compile-and-load it.

 If all files are loaded OK, you should see a message like this:
    ;;
    ;; Associative Memory-Based Reasoning (AMBR).
    ;; Version 2.2.2 of 24 July 1998.
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
 If you want to install the knowledge base and experiment with the model,
 follow the instructions from KB/README.TXT and AMBR_EXP/README.TXT.

 See file AMBR/DEFSYS.LSP for a description of the files comprising the
 implementation of AMBR.
 See file AMBR/PACKAGES.LSP for a full list of all classes, constants,
 variables, and functions advertized in the external protocol.
 See file AMBR/VERSION.LSP for version information.
 See file AMBR/INTRFACE/REPORT.LSP for functions that are most frequently
 used while experimenting with the model.

 See file DUAL/README.TXT to get started with the DUAL architecture.
 See file KB/README.TXT to get started with the knowledge base.
 See file AMBR_EXP/README.TXT for details on the simulation experiments
   performed so far.

 See files AMBR/DEFS, and =/PROCLAIM.LSP for the parameters governing the
 model. (Note that the settings should be coordinated with DUAL/DEFS, =/LABELS,
 and =/CONSUM.LSP.)


;;;;  End of file AMBR/README.TXT
