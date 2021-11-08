;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/version.lsp
;;; VERSION:    1.1.2
;;; PURPOSE:    Current DUAL version and log of previous versions.
;;; DEPENDS-ON: NIL
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:       21-01-98 [1.1.0]
;;; LAST UPDATED:  24-07-98 [1.1.2]  The 'official release'

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;                                                       ;;;;
      ;;;;      CURRENT VERSION OF DUAL'S IMPLEMENTATION         ;;;;
      ;;;;                                                       ;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package  "DUAL-INTERFACE")

(defconstant  *DUAL-version*  "1.1.2 of 24 July 1998"
  "Current version of DUAL's implementation" )

(eval-when (load)
  (format t "~%~%;; DUAL cognitive architecture, ver. ~A." *DUAL-version*)
  (format t   "~%;; (c) Boicho Kokinov and Alexander Petrov ~
               ~%;;     Cognitive Science Department ~
               ~%;;     New Bulgarian University ~
               ~%;;     {kokinov,apetrov}@cogs.nbu.acad.bg~%" )
) ; eval-when


#|   ; The rest of the file contains comments only

TIME STAMP OF THE CURRENT VERSION
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Version 1.1.2 reflects the state of the program on July 1, 1998.
This version was used for the simulation experiments reported in the Ph.D.
Thesis of Alexander Petrov. It dovetails to ver. 2.2.2. of the AMBR model.


RECENT MINOR MODIFICATIONS
~~~~~~~~~~~~~~~~~~~~~~~~~~~
21-01-98 [ver. 1.1.0]  -- Creating the file DUAL/VERSIONS.LSP (this file).
                       Version 1.1.0 has been used for the so-called 'ice-cube
                       experiment' with the AMBR model.  See AMBR/*.LSP and
                       AMBR_EXP/ICE_CUBE/*.DOC for details.
08-02-98 [ver. 1.1.1]  -- Re-organizing suspendable computations and in
                       particular adding S-VALUES and SUSPENDED-VALUE-BIND.
                       Creating files DUAL/ARCHIT/SUSPEND.LSP and =/MAILBOX.LSP
                       Moving S-LISP documentation to DUAL/ARCHIT/SUSPEND.TXT.
                       Support for suspended binding-forms in LET and LET*.
                       Re-writing the local marker-passing mechanism and
                       partitioning it into smaller (generic) functions that
                       can be customized by individual models.
                       Adding PROCLAIM-LABEL, PROCLAIM-LINK, and PROCLAIM-TAG.
                       Re-writing DEFAGENT to support tags and numbers.
                       DUAL/INTRFACE/READREF.LSP changed to =/READFILR.LSP.
01-07-98 [ver. 1.1.2]  -- Ph.D. Thesis of Alexander Petrov: 'official release'
                       This version was used for the simulation experiments
                       reported in the Ph.D. Thesis of Alexander Petrov.
                       It dovetails with ver. 2.2.2. of the AMBR model.
                       This version improves the interface by defining an
                       explicit class COALITION.  A number of utilities for
                       defining modular knowledge bases are provided -- see
                       DUAL/INTRFACE/WITH_AG, and =/KB_UTIL.LSP.  DEFAGENT is
                       now more general.  The interface is improved also by
                       'spy tools' -- see DUAL/INTRFACE/GFUN_SPY and =/SPY_TOOL.
                       Spy tools are a generic way to define 'verbose tools',
                       'count tools', etc. -- see DUAL/INTRFACE/VERBOSE,
                       =/COUNTER, & =/SET_SPY.LSP.  A rudimentary pretty-printer
                       for micro-frames and generators for DEFAGENT and WITH-
                       AGENT forms were implemented, see DUAL/INTRFACE/DEFAGENT
                       and =/WITH_AG.
                       More importantly, the conceptual specification of DUAL
                       is extended. DUAL agents now have a (limited) sense of
                       time.  There are alarm clocks (documented and implemented
                       in DUAL/ARCHIT/TIME.LSP) and metronomes (=/METRONOM.LSP).
                       Fixed a bug in the algorithm for node constructor
                       recruitment (see DUAL/ARCHIT/NC_AGENT.LSP).  The new
                       symbolic microcycle of node constructors ensures that
                       the most active 'client' is served first.
                       Time discretization was improved by introduction of
                       'symbolic subcycles' within one *TIME-SLICE* -- see
                       DUAL/ARCHIT/AGENDA.LSP.  Precompiled ACTIVATE-NEIGHBORS
                       methods were abolished.  (Timing and profiling showed
                       that there are of no use -- they save time during SPREAD
                       but waste time during SLOTS->LINKS.)  The program was
                       ported back to CMU CL. Now it is tested under Allegro CL
                       for Windows (ver. 3.0.2 standard) and CMU CL (ver. 17f).

                       Archives: OLD/DUAL/09JUN98.ARJ and =/20JUN98.ARJ.
                       See also OLD/AMBR/20JUN98.ARJ and OLD/AMBR/KB/20JUN98.ARJ


PREVIOUS MAJOR VERSIONS
~~~~~~~~~~~~~~~~~~~~~~~~
The implementation of DUAL consists of many files and each of them has been
changed many times.  The log of these 'minor modifications' has been thrown
away.  Only the major landmarks are given here:

Version 1.0   -- The implementation of the DUAL architecture at the time of
-------------    M.Sc. thesis of Alexander Petrov (July 1997).  This version
(July, 1997)     of the program has been used with the AMBR model, ver. 2.0,
                 and for the simulation experiments reported in the thesis.

Version 1.1   -- Improved version of DUAL's implementation intended for
-------------    distribution outside the Department of Cognitive Science at
(January, 1998)  the New Bulgarian University.  This is the so-called 'official
                 release'.  This version has been used with the AMBR model,
                 ver. 2.1, and for the so-called 'ice-cube experiment'.
                 Main improvements with respect to ver. 1.0 are:
                  -- slot fillers are stored in an optimized way
                  -- precompiled ACTIVATE-NEIGHBORS methods
                  -- S-PROGN is now compiler, not interpreter
                  -- DO-ALL-xxx are now macros, not functions

----------------------------------------------------------------------------

See file DUAL/TO_DO.TXT for suggestions for further improvement of the program.

|#

;;;;  End of file DUAL/VERSION.LSP
