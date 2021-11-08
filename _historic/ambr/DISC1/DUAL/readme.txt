;;; -*- Mode: ASCII Text -*-

;;; FILE:       DUAL/readme.txt
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Getting started with DUAL's implementation.
;;; DEPENDS-ON: NIL
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    24-07-98 [1.1.2]  The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;                                                               ;;;;
   ;;;;      G E T T I N G   S T A R T E D   W I T H   D U A L        ;;;;
   ;;;;                                                               ;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


GENERAL INFORMATION AND REFERENCES
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 This program is a portable implementation of the DUAL cognitive
 architecture. It is a shell for building DUAL-based models and probing
 their behavior through simulation experiments.

 The cognitive architecture DUAL is proposed by Boicho Kokinov. Some of the
 publications describing DUAL are the following:
   Kokinov, B. (1994a) "A hybrid model of reasoning by analogy".
      In K.Holyoak & J.Barnden (Eds.), Advances in connectionist and neural
         computation theory. Vol.2: Analogical connections (pp.247-318).
         Norwood, NJ: Ablex
   Kokinov, B. (1994b) "The DUAL cognitive architecture: A hybrid multi-agent
      approach".  Proceedings of the eleventh ECAI (pp.203-207).
   Kokinov, B. (1994c) "The context-sensitive cognitive architecture DUAL".
      Proceedings of the sixth annual conference of the cognitive science soc.

---------
 The implementation here adheres strictly to the conceptual description of
 the architecture as defined in the following document:

 Petrov, A. (1997) "Extensions of DUAL and AMBR". M.Sc. Thesis.
                   New Bulgarian University, Cognitive Science Department

 This M.Sc. thesis will be referred here as 'DUAL Report #1' or 'DR#1'.

 There is also a Ph.D. thesis that is a continuation of DR#1. This later
 work, however, is not devoted to the architecture (but to the model AMBR
 built on top of it).  Here is the reference:

 Petrov, A. (1998) "A Dynamic Emergent Model of Analogy-Making Based on
                   Decentralized Representations".  Ph.D. Thesis.
                   New Bulgarian University, Cognitive Science Department

 The theses, as well as any additional information, may be obtained from:
     Alexander Petrov:  apetrov@cogs.nbu.acad.bg
     Boicho Kokinov:    kokinov@cogs.nbu.acad.bg

 Anonymous ftp access is forthcoming. Check the site  cogs.nbu.acad.bg/pub/


PACKAGES USED IN THE IMPLEMENTATION
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 There are two packages (name spaces) used in this program.
   -- DUAL-core -- this package exports all 'conceptual' symbols
         and only those symbols. In other words, this package refers
         to the conceptual description of the architecture as specified
         in "DUAL Report #1". Implementational details are hidden in
         the package. Strictly DUAL-based programs should depend only on
         the external symbols of this package.

   -- DUAL-interface -- supports various tools that are not essential
         to the architecture as a psychological theory but are needed
         for debugging, interface, off-line batch processing, and the like.

 See file DUAL/PACKAGES.LSP for more details (and a list of all function
 names, variables, classes, etc.).


MODULES OF THE IMPLEMENTATION
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 DUAL is a complex program (about 6,000 lines of code) and is organized
 into a layered system of files.  See file DUAL/DEFSYS.LSP for details.

STRUCTURE OF INDIVIDUAL SOURCE FILES
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 Each source file constitutes a relatively self-contained module of the
 program.  It provides some programming objects (functions, classes, etc.)
 for use by other modules.  Each file is divided in two top-level sections:
   -- External protocol: documents all 'external symbols' of the file
   -- Implementation   : contains the actual Lisp code

 Future versions of the program should try to alter the external protocol
 as little as possible.  The implementation section, however, is free to
 change for any reason, as long as it adheres to the external protocol.
 (See Sonya Keene's book on CLOS for an extensive treatment of the
  utility of protocols.)


PORTABILITY
~~~~~~~~~~~
 Many efforts have been taken to ensure maximum portability of DUAL's
 implementation.  All Lisp sources adhere to the ANSI Common Lisp standard.
 The files DUAL/NONPORT?.LSP fix some discrepancies between the standard
 and the Lisp implementations that have been used.

 The program heavily depends on CLOS but does not depend on the condition
 system, the new LOOP macro, or the pretty printer. File-related operations
 are kept to a minimum and are isolated in separate files. Deprecated
 Lisp constructs have been avoided (with the exception of the old-fashioned
 keywords of EVAL-WHEN).
 The overall design of the system defines a portable and fully functional core
 that implements all architectural features. There is a primitive (but portable)
 interface that depends only on *STANDARD-INPUT* and *STANDARD-OUTPUT*.
 There are no graphical or window-related modules at the time being. Such
 modules may be added in the future and they will probably not be portable.

 Up to now, the program has been run on two platforms -- Allegro Common Lisp
 (Windows, ver. 3.0.2 standard) and CMU Common Lisp (Unix, ver. 17f).
 See files DUAL/NONPORT1 and =/NONPORT2.LSP for more details.


INSTALLATION
~~~~~~~~~~~~~
 Installation of DUAL amounts to three broad steps:
   1. You should unpack the distributive archive and create the directory
      structure.  The breakup of files into directories is explained below.
   2. You should arrange that the source files are (compiled and) loaded in
      appropriate order.  There are two ways for doing this: with and without
      DEFSYSTEM.  Both ways are explained below.
   3. In itself, DUAL is a library of classes and functions. You cannot do
      much with it unless you install a model on top of it.  At the time
      being, there is only one such model -- AMBR.  You should install AMBR
      (according to the instructions given in AMBR/README.TXT) in order to
      test DUAL and experiment with it.


 DIRECTORY STRUCTURE  (step 1)
 ------------------------------
 The implementation of DUAL consists of more than 50 files grouped in several
 directories.  The directory structure is important, as file names alone are
 not enough to identify the files (e.g. there are several files named DEFS.LSP).

 I recommend the following directory structure:
   DUAL_ROOT
     +-------- DUAL                ; stage-setting files for DUAL
     |          +-- ARCHIT         ; implementation of the architectural 'core'
     |          +-- INTRFACE       ; interface facilities
     |
     +-------- AMBR                ; AMBR model, see AMBR/README.LSP
     |          +-- ...
     |
     +-------- AMBR_EXP            ; AMBR experiments
     |          +-- ...
     |
     +-------- FASL                ; an identical directory tree keeping the
                +-- DUAL           ;  compiled (FASL) versions of LISP sources
                |    +-- ARCHIT
                |    +-- INTRFACE
                |
                +-- AMBR
                |    +-- ...
                +-- ...



 COMPILING AND LOADING THE SYSTEM  (step 2)
 -------------------------------------------
 The file DUAL/DEFSYS.LSP defines the dependencies between the various files
 that make up DUAL. It uses Mark Kantrowitz' DEFSYSTEM -- a tool for organizing
 systems into hierarchical layers of modules, with matching directory structure
 (an analog to MAKE in C).  DEFSYSTEM can be downloaded from:
    ftp.cs.cmu.edu/afs/cs.cmu.edu/user/mkant/Defsystem/defsystem.{text,lisp}

 To use DEFSYSTEM, you need to perform the following steps:
   A. Load the file containing the implementation of DEFSYSTEM (or better,
      arrange it to be loaded automatically whenever Lisp is started).
   B. Edit the file DUAL/DEFSYS.LSP and re-define all path names to match
      your local operation system and your directory structure.
   C. Load the file DUAL/DEFSYS.LSP.  It will provide the definition
      of a system named "dual". (You need to do this only once per session.
      You may even load this file from your INIT.LSP file.)
   D. Use the function OPERATE-ON-SYSTEM (or OOS for short) to do things
      to your system.  In particular:
        -- (OOS "dual" :LOAD)     will load DUAL
        -- (OOS "dual" :COMPILE)  will compile-and-load it.

 If all files are loaded OK, you should see a message like this:
    ;;
    ;; DUAL cognitive architecture, ver. 1.1.2. of 24 July, 1998.
    ;; (c) Boicho Kokinov and Alexander Petrov
    ;;     Cognitive Science Department
    ;;     New Bulgarian University
    ;;     {kokinov,apetrov}@cogs.nbu.acad.bg
    ;;

 The DEFSYSTEM tool is included in the archive. (I thank Mark Kantrowitz for
 designing and implementing this convenient tool and for placing it into the
 public domain.)  If, however, you do not want to use it, the source files
 can simply be loaded in the order listed at the end of DUAL/DEFSYS.LSP.

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
 If you want to install the AMBR model and experiment with it, follow the
 instructions from AMBR/README.TXT.

 See file DUAL/DEFSYS.LSP for a description of the files comprising the
 implementation of DUAL.
 See file DUAL/PACKAGES.LSP for a full list of all classes, constants,
 variables, and functions advertized in the external protocol.
 See file DUAL/VERSION.LSP for version information.
 See file DUAL/TO_DO.TXT for suggestions for improvements of the program.
 See file DUAL/START_ME.LSP for details on the so-called compilation policies.

 See files DUAL/DEFS, =/LABELS, and =/CONSUM.LSP for the parameters governing
 the architecture. (Note that the settings should be coordinated with AMBR/DEFS
 and AMBR/PROCLAIM.LSP.)

 See file DUAL/ARCHIT/SUSPEND.TXT for an introduction to the variable-speed
 computation postulated by DUAL and for its current implementation using
 closures and stacks.

 See file DUAL/ARCHIT/DUAL_AG.LSP for documentation of the main entity in
 the architecture -- the so-called DUAL-AGENT -- and for its current implem.
 using a lattice of classes.

 See the various files in the DUAL/INTRFACE/ directory for the functions used
 for running DUAL and inspecting the agents, coalitions, etc.  The files DUAL/
 INTRFACE/SPREAD, =/AGENDA, and =/DEFAGENT.LSP are probably the most useful.
 Note also the #\? shorthand for DESCRIBE implemented in DUAL/GENERAL.LSP
 and the #$ shorthand for FIND-AGENT implemented in DUAL/ARCHIT/BASIC.LSP.


;;;;  End of file DUAL/README.TXT
