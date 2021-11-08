;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/kb_util.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Utilities for defining and analysing knowledge bases (KBs).
;;; DEPENDS-ON: archit/dual_ag.lsp, intrface/connect.lsp, intrface/dummy_ag.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov
;;; CREATED:    04-04-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;;;;;    KNOWLEDGE  BASE  UTILITIES     ;;;;;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-INTERFACE")

;;;; This file defines some functions that are useful for defining knowledge
;;;; bases spread in several files and for analyzing the cross-references
;;;; between agents.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: require-agents,
;;          reference-agents, check-for-unresolved-references,
;;          check-for-missing-slots, check-for-old-agents, check-for-self-refs
;;
;;          list-all-agents, xref, xref-to-file

;; REQUIRE-AGENTS  (name-list &key warn-p)  -->  list-of-undefined
;;
;;   A function that checks that all agents listed in NAME-LIST are defined.
;;   This function usually is placed at the top of KB-definition files that
;;   depend on other files.
;;
;;   NAME-LIST should be a list of symbols.
;;   WARN-P is a flag controling whether the function issues warnings on
;;    undefined agents or not.  When not supplied, it defaults to T.
;;
;;   Each name in NAME-LIST is checked for the following two criteria:
;;    1. There is an agent with this name (FIND-AGENT returns non-NIL)  and
;;    2. This agent is not a dummy-agent.
;;        (Dummy agents are referenced by some other agent but are not defined
;;         via DEFAGENT or MAKE-DUAL-AGENT -- see DUAL/INTRFACE/DUMMY_AG.LSP. )
;;   The function returns the list of all names that failed to satisfy either
;;   of these two criteria.  When WARN-P is non-NIL (the default), it also
;;   issues a warning.  Note that REQUIRE-AGENTS makes no attempt to load
;;   any agent definitions.
;;
;;   Example:
;;     (require-agents '(water teapot in))
;;     (defagent   water-1
;;       :inst-of  water  )      ; WATER is guaranteed to exist
;;     (defagent   teapot-1
;;       :inst-of  teapot )      ; TEAPOT is guaranteed to exist
;;
;;   Note the difference between REQUIRE-AGENTS and REFERENCE-AGENTS below.
;;     REQUIRE-AGENTS warns when the listed agents are not defined.
;;     REFERENCE-AGENTS prevents CHECK-FOR-UNRESOLVED-AGENTS from warning
;;       even when the listed agents are not defined.


;; REFERENCE-AGENTS (exemption-id name-list)  -->  full-list
;;
;;   A function that is used to proclaim that the agents listed in NAME-LIST
;;   will be referenced but need not be defined in KB file.  The 'exemption set'
;;   proclaimed in this way is identified by the symbol EXEMPTION-ID and
;;   may be passed to the function CHECK-FOR-UNRESOLVED-REFERENCES (see below).
;;
;;   EXEMPTION-ID must be a symbol.
;;   NAME-LIST must be a list of symbols (agent names).
;;   The function returns the full exemption set identified by EXEMPTION-ID.
;;
;;   REFERENCE-AGENTS is cumulative -- the exemption set produced by repeated
;;   calls (with the same set identifier) is the union of all NAME-LISTS.
;;
;;   Example: Consider the following two files defining parts of a common KB:
;;                                        |
;;     ;; File KB/TEMP_REL.LSP            |  ;; File KB/SPAT_REL.LSP
;;     ;; Defines temporal relations.     |  ;; Defines spatial relations.
;;                                        |
;;     (reference-agents 'from-SPAT_REL   |  (reference-agents 'from-TEMP_REL
;;       '(short long))                   |    '(brief prolonged))
;;     ....                               |  ...
;;     (defagent brief  concept-agent     |  (defagent short   concept-agent
;;       :subc    temporal-relation       |    :subc    spatial-relaton
;;       ...                              |    ...
;;       :a-link  (short 0.5)             |    :a-link  (brief 0.5)
;;     )                                  |  )
;;     ....                               |  ...
;;     (check-for-unresolved-references   |  (check-for-unresolved-references
;;       :exemption-id 'from-SPAT_REL )   |    :exemption-id 'from-TEMP_REL )
;;                                        |
;;     ;; End of file KB/TEMP_REL.LSP     |  ;; End of file KB/SPAT_REL.LSP
;;                                        |
;;   With this organization, each of the two files may be compiled and loaded
;;   without warnings.  The two files may be loaded in arbitrary order even
;;   though they are mutually dependent: BRIEF references SHORT and vice versa.
;;   (The forward-reference mechanism implemented in DUAL/INTRFACE/DUMMY_AG.LSP
;;    and =/DEFAGENT.LSP is vital for breaking these cyclic references.)
;;   Each of the files can still use CHECK-FOR-UNRESOLVED-REFERENCES to check
;;   for typographical errors (e.g. SPATIAL-RELATON instead of SPATIAL-RELATION)
;;

;; CHECK-FOR-UNRESOLVED-REFERENCES (&key warn-p  check-slots-p check-self-refs-p
;;                                      exemption-set exemption-id)  -->
;;                                                       -->  unresolved-list
;;
;;   A function that checks for agents that are referenced but are not defined.
;;   It catches many of the typographical errors that are inevitable when
;;   modifying large KB files (e.g referencing IMM-HTR instead of IMM-HEATER).
;;
;;   WARN-P, CHECK-SLOTS-P, and CHECK-SELF-REFS-P are flags that control whether
;;     the function calls WARN (if needed), CHECK-FOR-MISSING-SLOTS, and CHECK-
;;     FOR-SELF-REFS.  When not supplied, all these flags default to T.
;;   EXEMPTION-SET should be a list of symbols.  These symbols are interpreted
;;     as names of agents that may be referenced without being defined.
;;     When not supplied, EXEMPTION-SET defaults to the empty list.
;;   EXEMPTION-ID should be a symbol or a list of symbols.  Usually it is a
;;     symbol that has been used as a first argument to REFERENCE-AGENTS (see
;;     above).  If EXEMPTION-ID is a list, their respective sets are combined.
;;     When not supplied, EXEMPTION-ID defaults to the empty list.
;;   The function returns a list of 'dummy agents' -- see INTRFACE/DUMMY_AG.LSP.
;;
;;   CHECK-FOR-UNRESOLVED-REFERENCES works according to the following algorithm:
;;     1. EXEMPTION-SET and the set identified by EXEMPTION-ID are united to
;;          produce the so-called COMBINED-EXEMPTION-SET.
;;     2. The function CHECK-FOR-DUMMIES (see DUAL/INTRFACE/DUMMY_AG.LSP) is
;;          called to produce the list of all dummy agents.
;;     3. The agents whose names are listed in the COMBINED-EXEMPTION-SET are
;;          excluded from the list generated at step 2.  Call the resulting
;;          set BAD-DUMMIES.
;;     4. If BAD-DUMMIES is not empty and WARN-P is non-NIL, issue a warning.
;;     5. If CHECK-SLOTS-P is not NIL, call CHECK-FOR-MISSING-SLOTS.
;;     6. If CHECK-SELF-REFS-P is not NIL, call CHECK-FOR-SELF-REFS.
;;     7. Return BAD-DUMMIES (a list of agents, not a list of names).
;;

;; CHECK-FOR-MISSING-SLOTS (&key warn-p exemption-set exemption-id)  -->
;;                                                       -->  unresolved-list
;;
;;   A function that checks for slots that are referenced but are missing
;;   from their respective agents.
;;
;;   WARN-P, EXEMPTION-SET, and EXEMPTION-ID have the same interpretation and
;;     default values as in CHECK-FOR-UNRESOLVED-REFERENCES.
;;   The function returns a list of symbolic references (see ARCHIT/SYMREF.LSP).

;; CHECK-FOR-SELF-REFS  (&key warn-p exemption-set exemption-id)  -->
;;                                                       -->  self-ref-list
;;
;;   A function that checks for agents that have references (i.e. links)
;;   pointing back to themselves.
;;
;;   WARN-P, EXEMPTION-SET, and EXEMPTION-ID have the same interpretation and
;;     default values as in CHECK-FOR-UNRESOLVED-REFERENCES.
;;   The function returns the list of all self-referencing agents found.

;; CHECK-FOR-OLD-AGENTS  (&key exemption-set cerror-p)  -->  OK-flag
;;
;;   A function that is usually placed at the beginning of the first KB file
;;   and that ensures that there are no remnants from old runs.
;;
;;   EXEMPTION-SET should be a list of agents.  When not supplied, it defaults
;;     to the list of node-constructors stored in the global variable
;;     *NODE-CONSTRUCTORS*.  (See DUAL/ARCHIT/NC_AGENT.LSP.)
;;   CERROR-P is a flag controling whether a continuable error is signaled
;;     upon necessity. When not supplied, it defaults to T.
;;   The function returns T if there were no old agents (possibly with the
;;   exception of those listed in EXEMPTION-SET).  If there is some agent in
;;   the system that is not listed in EXEMPTION-SET, and CERROR-P is non-NIL,
;;   a continuable error is signaled.  If the user continues from the error,
;;   the function returns NIL without attempting to remove the old agents.
;;   If there are old agents and CERROR-P is NIL, the fun simply returns NIL.


;; LIST-ALL-AGENTS ()  -->  list-of-agents
;;
;;   A function that collects all agents registered in the system, sorts them
;;   by agent-name, and returns them as a list.
;;
;;   The agents collected are those accessible through DO-ALL-AGENTS.


;; XREF (agent &optional verbose stream)  -->  (values)
;;
;;   A function that prints out information about the links coming in or out
;;   of a given agent. (The name is a shorthand for 'cross-reference'.)
;;
;;   AGENT should be a connectionist agent.  If it isn't, an error is signaled.
;;   VERBOSE is a flag that controls the amount of output.  Defaults to T.
;;   STREAM should be an output stream.  Defaults to *STANDARD-OUTPUT*.
;;   The function returns no values.
;;
;;   The exact format of the output is implementation-dependent.  When VERBOSE
;;   is set to NIL, only one line of output is generated.  This mode is useful
;;   for the function XREF-TO-FILE.
;;

;; XREF-TO-FILE  (file-name &optional agent-list verbose)  -->  agent-list
;;
;;   A function that prints cross-reference information to a file.  This
;;   function opens a file with the given name, applies the function XREF to
;;   all agents in AGENT-LIST in order, and closes the file.
;;
;;   FILE-NAME should be acceptable by WITH-OPEN-FILE or OPEN (see CLtL1/2).
;;   AGENT-LIST should be either a list of agents or the symbol T.  When not
;;     supplied, it defaults to T.  When AGENT-LIST is T, the list of all
;;     agents (as produced by LIST-ALL-AGENTS) is used.
;;   VERBOSE is a flag that controls the amount of ouput produced by XREF.
;;     When not supplied, it defaults to NIL (note that the default VERBOSE 
;;     value for XREF is T).
;;   The function returns the list of agents that have been printed.
;;   When AGENT-LIST has been supplied and is a list, the same list is returned.
;;   When AGENT-LIST has not been supplied (or was T), the list of all agents
;;     as produced by LIST-ALL-AGENTS is returned.

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

;;;;;;; *******  UTILITIES FOR KB DEFINITION  ********
;;

(defun  require-agents (name-list &key (warn-p T))
  "Check that all agents listed in NAME-LIST exist."
  (let ((list-of-undefined nil))
    (dolist (name name-list)
      (let ((agent (find-agent name)))
        (when (or (null agent)
                  (dummy-agent-p agent))     ; see DUAL/INTRFACE/DUMMY_AG.LSP
          (push name list-of-undefined) )))
    (setq list-of-undefined
          (nreverse list-of-undefined))
    (when (and list-of-undefined             ; when there are undefined
               warn-p)                       ; and the flag is set
      (warn "REQUIRE-AGENTS: The following agents are not defined: ~S."
            list-of-undefined))
    list-of-undefined))


;; In this implementation, the exemption set is stored on the property list
;; of the exemption identifier.  The indicator is 'duali::exemption-set.
;;
(defun  reference-agents  (exemption-id name-list)
  "Proclaim that some agents will be referenced here but defined elsewhere."
  (declare (type symbol exemption-id)
           (type list name-list) )
  (let ((old-exemption-set (get exemption-id 'exemption-set)))
    (setf (get exemption-id 'exemption-set)
          (union1 old-exemption-set name-list)) ))   ; see DUAL/GENERAL.LSP

(defun combined-exemption-set (set identifier)
  (declare (type list set)
           (type (or symbol list) identifier)
           (values list) )
  (let ((total-set set))
    (etypecase identifier
      (list  (dolist (id identifier)
               (setq total-set (union1 total-set
                                       (get id 'exemption-set))) ))
      (symbol  (setq total-set (union1 total-set
                                       (get identifier 'exemption-set))) ))
    total-set ))


(defun check-for-unresolved-references (&key (warn-p T)
                                             (check-slots-p T)
                                             (check-self-refs-p T)
                                             exemption-set  exemption-id )
  "Check for agents that are referenced but are not defined."
  (declare (type list exemption-set)
           (type (or symbol list) exemption-id) )
  (let* ((comb-exempt-set (combined-exemption-set exemption-set exemption-id))
         (all-dummies (check-for-dummies))     ; see DUAL/INTRFACE/DUMMY_AG.LSP
         (bad-dummies (remove-if #'(lambda (dummy)
                                     (member (agent-name dummy)
                                             comb-exempt-set))
                                 all-dummies))
         (bad-names (mapcar #'agent-name bad-dummies)) )
    (when (and warn-p bad-dummies)
      (warn "The following agents are referenced but are not defined: ~S."
            bad-names))
    (when check-slots-p
      (check-for-missing-slots :warn-p warn-p
             :exemption-set (union1 comb-exempt-set bad-names)) )
    (when check-self-refs-p
      (check-for-self-refs :warn-p warn-p
             :exemption-set (union1 comb-exempt-set bad-names)) )
    bad-dummies ))


(defun check-for-missing-slots (&key (warn-p T) exemption-set  exemption-id )
  "Check for slots that are referenced but are not owned by the resp. agents."
  (declare (type list exemption-set)
           (type symbol exemption-id) )
  (let ((comb-exempt-set (combined-exemption-set exemption-set exemption-id))
        (bad-symrefs nil) )
    (do-all-agents (agent)
      (unless (member (agent-name agent) comb-exempt-set)
        (do-all-references (conref agent)
          (let ((symref (conref-reference conref)))
            (when (and (not (locate-mfr-component symref))
                       (not (member (agent-name (symref-agent symref))
                                    comb-exempt-set)) )
              (pushnew symref bad-symrefs :test #'symref-equal))))))
    (when (and warn-p bad-symrefs)
      (warn "The following slots are referenced but are not defined: ~{ ~A~}"
            (mapcar #'symref->string bad-symrefs) ))
    bad-symrefs ))


(defun check-for-self-refs  (&key (warn-p T) exemption-set  exemption-id )
  "Ensure that no agent points to itself."
  (declare (type list exemption-set)
           (type symbol exemption-id)
           (values list) )   ; of self-referencing agents, if any.
  (let ((comb-exempt-set (combined-exemption-set exemption-set exemption-id))
        (self-refs nil) )
    (do-all-agents (agent)
      (unless (member (agent-name agent) comb-exempt-set)
        (do-all-references (conref agent)
          (when (symref-match agent (conref-reference conref))
            (pushnew agent self-refs)))))
    (when (and warn-p self-refs)
      (warn "The following agent(s) refer to themselves: ~S."
            self-refs))
    self-refs ))


(defun check-for-old-agents  (&key (exemption-set *node-constructors*)
                                   (cerror-p T) )
  "Ensure that the system is free from remnants of old knowledge bases."
  (declare (type list exemption-set)     ; list of (special) agents
           (values boolean) )
  (let ((old-agents '())
        (counter 0)
        (*print-length* 5) )
    (do-all-agents (agent)
      (unless (member agent exemption-set)
        (push agent old-agents)
        (incf counter)
        (when (> counter 10)
          (return)) ))       ; terminate DO-ALL-AGENTS
    (cond ((null old-agents)  T )          ; OK
          ((null cerror-p)   NIL)          ; not OK, but be quiet
          (t (cerror "Continue anyway."        
               "There are old agents that may interfere with the new KB: ~S"
               old-agents)
             NIL)) ))



;;;;;;; *******  UTILITIES FOR KB ANALYSIS  ********
;;

(defun list-all-agents (&aux result)
  "Collects a list of all agents in the system, sorted by name."
  (declare (values list))
  (do-all-agents (ag)
    (push ag result))
  (sort result #'string< :key #'agent-name) )


(defun massage-conrefs (conref-list)
  ;; Discard slots (from extended symrefs) and order by weights.
  (sort (mapcar #'(lambda (conref) 
                    (list (agent-name (conref-reference conref))
                          (conref-weight conref)))
                conref-list)
        #'>=
        :key #'second))

(defun xref (agent &optional (verbose t) (stream *standard-output*))
  "Prints information about the links coming in and out of AGENT."
  (declare (type connectionist-agent agent)
           (type stream stream) )
  (let ((fan-in (massage-conrefs (fan-in  agent)))
        (fan-out  (massage-conrefs (fan-out agent))) )
    (if verbose
        (progn
          (format stream "~%~20S -------------------~%"  agent)
          (format stream "  fan-out = ~3D, sum-out = ~6,3F ------~%"
                          (length fan-out) (reduce #'+ fan-out :key #'second))
          (dolist (pair fan-out)
            (format stream "   ~6,3F  ~S~%" (second pair) (first pair)))
          (format stream "  fan-in  = ~3D, sum-in  = ~6,3F ------~%"
                           (length fan-in) (reduce #'+ fan-in :key #'second))
          (dolist (pair fan-in)
            (format stream "   ~6,3F  ~S~%" (second pair) (first pair)))
        ) ; verbose
        (format stream "~&~25@S ~3D  ~3D   ~6,3F ~%"
                (agent-name agent)
                (length fan-out)
                (length fan-in)
                (reduce #'+ fan-in :key #'second)) ))
  (values))


(defun xref-to-file (file-name &optional (agent-list t) (verbose NIL))
  "Prints cross-reference information to a file."
  (when (eq agent-list t)
     (setq agent-list (list-all-agents)))   ; collect all
  (with-open-file (stream file-name :direction :output)
    (dolist (agent agent-list)
      (xref agent verbose stream) ))
  agent-list )


;;;;;;;  End of file DUAL/INTRFACE/KB_UTIL.LSP
