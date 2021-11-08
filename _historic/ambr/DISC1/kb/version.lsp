;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/KB/version.lsp
;;; VERSION:    3.0.0
;;; PURPOSE:    Current version of AMBR's knowledge base and log of prev. vers.
;;; DEPENDS-ON: AMBR/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:       28-04-98 [3.0.0]
;;; LAST UPDATED:  14-08-98 [3.0.0] The 'official release'

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;                                                       ;;;;
      ;;;;      CURRENT VERSION OF AMBR'S KNOWLEDGE BASE         ;;;;
      ;;;;                                                       ;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package  "AMBR")

(defconstant  *KB-version*  "3.0.0 of 24 July 1998"
  "Current version of AMBR's knowledge base" )

(eval-when (load)
  (format t "~%~%;; Knowledge base version ~A." *KB-version* )
  (format t   "~%;; (c) Boicho Kokinov and Alexander Petrov ~
               ~%;;     Cognitive Science Department ~
               ~%;;     New Bulgarian University ~
               ~%;;     {kokinov,apetrov}@cogs.nbu.acad.bg~%" )
) ; eval-when


#|   ; The rest of the file contains comments only

TIME STAMP OF THE CURRENT VERSION
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Version 3.0.0 reflects the state of the knowledge base on July 24, 1998.
This version is characterized by the following improvements over the
knowledge base used in the 'ice-cube experiment' (see AMBR_EXP/ICE/ICE_LTM.LSP):
  -- It has been partitioned into numerous small files using the utilities
      in DUAL/INTRFACE/KB_UTIL.LSP.  In particular, 'semantic' and 'episodic'
      memories are now separated in different sets of files (and even in
      different directories -- KB/SEMANTIC/ and KB/EPISODIC/).
  -- Many new concepts added to the semantic memory -- see KB/SEMANTIC/*.LSP.
      The semantic memory now consists of about 300 agents.
      (The semantic portion of AMBR_EXP/ICE/ICE_LTM.LSP had 127 agents.)
  -- More precise usage of INST-OF and SUBC slot superordinates to distinguish
      between 'value' and 'range' interpretations of C-COREF facets.
  -- Many new concepts added to the episodic memory -- see KB/EPISODIC/*.LSP.
      The permanent episodic memory now consists of about 250 agents.
      (The episodic portion of AMBR_EXP/ICE/ICE_LTM.LSP had 117 agents.)
  -- The representations of the base situations have been elaborated.  Now
      they have richer causal structure which (hopefully) will aid transfer.
  -- Modular GENKB templates are provided in each file (see AMBR/INTRFACE/GENKB)


The current version of the knowledge base works with version 2.2.2. of the
AMBR model (check file AMBR/VERSION.LSP).


RECENT MINOR MODIFICATIONS
~~~~~~~~~~~~~~~~~~~~~~~~~~~
...

28-04-98 [ver. 3.0.0]  -- New major version:
  -- ...


PREVIOUS MAJOR VERSIONS
~~~~~~~~~~~~~~~~~~~~~~~~
The knowledge base used for simulation experiments with the AMBR model has
been changed many times.  The log of these modifications has been thrown away.
Only the major landmarks are given here:

Version 1.0   -- The implementation of the AMBR model and the DUAL architecture
-------------    written by Boicho Kokinov prior to his 1994 publication
(before 1994)    titled "A Hybrid Model of Reasoning by Analogy".
                 This program ran on Golden Common LISP under MS-DOS.
                 Its knowledge base used the same knowledge representation
                 scheme that is used today (i.e. micro-frames, slots, and
                 facets) but the LISP representation was very different.
                 The label 'is-a' was used instead of the current 'subc'.
                 The names of the agents were void symbols (G100, G101, etc).

Version 2.0   -- The knowledge base used in the M.Sc. thesis of Alexander
-------------    Petrov (July 1997).  This version was derived by version 1.0
(July, 1997)     by translating old agent definitions to DEFAGENT clauses,
                 using meaningful agent names (e.g. WATER instead of G205),
                 and adding many new concepts and episodes.  This knowledge
                 base was used for the simulation experiments reported in
                 the thesis.  See the archive OLD/AMBR/10JUL97.ARJ.

Version 2.1   -- The knowledge base used in the so-called 'ice-cube experiment'.
-------------    See AMBR_EXP/ICE/*.* and OLD/AMBR_EXP/ICE/*.* for details.
(January, 1998)  This version contains 127 semantic agents and 130 episodic
                 agents grouped in 8+1 situations.  The whole KB is kept
                 in a single source file -- AMBR_EXP/ICE/ICE_LTM.LSP.
|#

;;;;  End of file AMBR/KB/VERSION.LSP
