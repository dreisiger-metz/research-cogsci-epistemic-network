;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-interface -*-

;;; FILE:       AMBR/version.lsp
;;; VERSION:    2.2.2
;;; PURPOSE:    Current AMBR version and log of previous versions.
;;; DEPENDS-ON: AMBR/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:       21-01-98 [2.1.0]
;;; LAST UPDATED:  14-08-98 [2.2.2]  The 'official release'

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;                                                       ;;;;
      ;;;;      CURRENT VERSION OF AMBR'S IMPLEMENTATION         ;;;;
      ;;;;                                                       ;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package  "AMBR-INTERFACE")

(defconstant  *AMBR-version*  "2.2.2 of 24 July 1998"
  "Current version of AMBR's implementation" )

(eval-when (load)
  (format t "~%~%;; Associative Memory-Based Reasoning (AMBR)." )
  (format t   "~%;; Version ~A." *AMBR-version* )
  (format t   "~%;; (c) Boicho Kokinov and Alexander Petrov ~
               ~%;;     Cognitive Science Department ~
               ~%;;     New Bulgarian University ~
               ~%;;     {kokinov,apetrov}@cogs.nbu.acad.bg~%" )
) ; eval-when


#|   ; The rest of the file contains comments only

TIME STAMP OF THE CURRENT VERSION
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Version 2.2.2 reflects the state of the program on July 24, 1998 -- the Ph.D.
Thesis of Alexander Petrov.  This version is characterized by the following
improvements over ver. 2.2.1:
  -- ...
  -- ... (listed below)

The current version of the model AMBR works with version 1.1.2 of the
cognitive architecture DUAL (check file DUAL/VERSION.LSP).


RECENT MINOR MODIFICATIONS
~~~~~~~~~~~~~~~~~~~~~~~~~~~
...

25-05-98 [ver. 2.2.2]  -- Ph.D. Thesis of Alexander Petrov.  Imprvmt of 2.2.1:
  -- The skolemization mechanism has been designed and implemented.
      See AMBR/SKOLEM.TXT, =/SKOLEM1 and =/SKOLEM2.LSP for details.
  -- The 'weak' structure-correspondence mechanism has been designed and
      implemented.  See AMBR/WEAK_SC.LSP for details.
  -- Mentors (MP and SC) now distinguish b/n 'homogeneous' and 'heterogeneous'
      hypotheses and choose mentor-link weights accordingly. See AMBR/DEFS.LSP
      for the elaborated set of weight parameters.
  -- The promotion mechanism now handles 'mapped T-LINKs' and :LOSER tags.
  -- Elaborate control of time discretization.  See ENFORCE-xxx-PARAMETERS in
      AMBR/TOPLEVEL.LSP.
  -- TRACK-SLOT-IN-PATH now handles both INST-OF and SUBC facets. See the new
      function GET-SLOT-SUPERORDINATES in AMBR/MARKER.LSP.
  -- More precise usage of INST-OF and SUBC slot superordinates in KB files.
  -- AMBR/UTILS.LSP extended and renamed to =/KREPRES.LSP. New AMBR/SITUATN.TXT
  -- Thorough revision of the knowledge base.  It has been partitioned into
      numerous small files using the utilities in DUAL/INTRFACE/KB_UTIL.LSP.
      See file AMBR/KB/VERSION.LSP for more details.
  -- Many new concepts added to the semantic memory -- see KB/SEMANTIC/*.LSP.
  -- The episodes (both base and target) in LTM were elaborated by detailing
      the causal structure and separating intentions from results.
      See AMBR/KB/EPISODIC/*.LSP and AMBR/KB/VERSION.LSP for more details.
  AMBR ver. 2.2.2 works with ver. 1.1.2. of DUAL (check file DUAL/VERSION.LSP).
  Archives: OLD/AMBR/09JUN98.ARJ, OLD/AMBR/20JUN98.ARJ;
            OLD/AMBR/KB/25MAY98.ARJ, OLD/AMBR/KB/09JUN98,
   See also OLD/DUAL/09JUN98.ARJ, OLD/DUAL/20JUN98.ARJ.


03-04-98 [ver. 2.2.1]  -- Improvement of ver. 2.2.0:
  -- The 'promotion mechanism' for WINNER hypotheses -- see AMBR/PROMOTN.LSP.
  -- The 'rating mechanism' for choosing whom to promote -- see AMBR/RATING.LSP.
  -- Consumption parameters have been tuned so that the 'ice cube experiment'
      yields the expected result again. (It failed in ver. 2.2.0.)
  -- Permanent (and temporary) correspondence agents are kept in a new slot
      labeled :CORR as opposed to the :HYPOTH slots for the hypotheses.
  -- COALITN.LSP moved (after substantial modification) from AMBR/INTRFACE/
      to DUAL/INTRFACE/  .  Making use of the new COALITION class.
  -- File AMBR/INTRFACE/VERBOSE.LSP modified substantially and renamed to
      AMBR/INTRFACE/SET_SPY.LSP.  Now using spy tools (see DUAL/INTRFACE/SPY*.)
  -- HYPOTHESIS-BUFFER subsumed by AGENT-BUFFER (now a slot of AMBR-AGENTs).
  -- FIZZLE-HYPOTHESIS has been generalized to FIZZLE-AGENT and moved to a
      separate file AMBR/FIZZLE.LSP.  Fizzle messages.
  AMBR ver. 2.2.1 works with ver. 1.1.2. of DUAL (check file DUAL/VERSION.LSP).
  Archives: OLD/AMBR/18APR98.ARJ.  See also OLD/DUAL/18APR98.ARJ

17-02-98 [ver. 2.2.0]  -- Improvement of ver. 2.1:
  -- An abstraction barrier around 'situation agents' -- see AMBR/SITUATN.LSP.
  -- Marker colors are now situation-agents (and not simply integers).
      This allows for several mappings running in parallel.
  -- Instance-agents are not MP-agents any more; they only know to emit markers.
      (They did not receive or handle markers in the previous versions anyway.)
  -- Eliminate the class ENTITY-AGENT (because instance-agents are not MP-ags).
  -- Introduce the notions of 'driver situation' and 'recipient situation'.
  -- Correspondence (and hence hypothesis) agents now have a 'driver slot'.
  -- Correspondences are asymmetric, e.g. WATER<==>MILK /= MILK<==>WATER.
  -- HYPOTHESIS-P, MATURE-HYPOTHESIS-P, etc. have been subsumed by AGENT-TYPE.
  AMBR ver. 2.2.0 works with ver. 1.1.1. of DUAL (check file DUAL/VERSION.LSP).
  Archives: OLD/AMBR/28FEB98.ARJ and OLD/AMBR/24FEB98.ARJ.



PREVIOUS MAJOR VERSIONS
~~~~~~~~~~~~~~~~~~~~~~~~
The implementation of AMBR consists of many files and each of them has been
changed many times.  The log of these 'minor modifications' has been thrown
away.  Only the major landmarks are given here:

Version 1.0   -- The implementation of the AMBR model and the DUAL architecture
-------------    written by Boicho Kokinov prior to his 1994 publication
(before 1994)    titled "A Hybrid Model of Reasoning by Analogy".
                 This program ran on Golden Common LISP under MS-DOS.
                 No part of this old code is used in current programs.

Version 2.0   -- The implementation of the AMBR model at the time of the
-------------    M.Sc. thesis of Alexander Petrov (July 1997).  This version
(July, 1997)     of the program depends on ver. 1.0 of DUAL's implementation
                 and has been used for the simulation experiments reported in
                 the thesis.  See the archive OLD/AMBR/10JUL97.ARJ.

Version 2.1   -- Improved version of AMBR's implementation intended to account
-------------    for the improvements in the implementation of the architecture
(January, 1998)  and more precisely the transition from DUAL ver. 1.0 to DUAL
                 ver. 1.1 (the 'official release').  No conceptual changes
                 have been introduced to the model's mechanisms.  Only the
                 knowledge base was enlarged.  Theoretically, AMBR 2.1 running
                 with the knowledge base of AMBR 2.0 should produce identical
                 results.  Version 2.1 (with the enlarged KB) has been used
                 in the so-called 'ice-cube experiment'.
                 See AMBR_EXP/ICE/ICE_CUBE.DOC for a report of this experiment.
                 Archives: OLD/AMBR/11JAN98.ARJ, OLD/AMBR/05FEB98.ARJ, and
                           OLD/DUAL/05FEB98.ARJ.

Version 2.2   -- ...
-------------
(February, 1998)
|#

;;;;  End of file AMBR/VERSION.LSP
