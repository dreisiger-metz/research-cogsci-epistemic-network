;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/work_mem.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Working memory management
;;; DEPENDS-ON: defs.lsp, general.lsp, archit/conref.lsp, archit/hybrid.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    21-04-97 [1.0]
;;; UPDATED:    19-12-97 [1.1]
;;; UPDATED:    18-04-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...
;;;;;;;
;;; TO DO:      In ACTIVATE-ALL-SPECIAL-WM, do several small steps instead
;;;             of one big one (i.e. bind *TIME-SLICE*) when necessary.
;;; TO DO:      Add a flag that prevents repeated invocations of NORMALIZE-
;;;             WM-WEIGHTS (see the warning at its documentation below).
;;;             The flag may be a separate slot in special WMs which is
;;;             cleared by DENORMALIZE-... and set by NORMALIZE-WM-WEIGHTS.


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;         W O R K I N G    M E M O R I E S       ;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concepts defined in this file is WORKING MEMORY (WM).
;;;;
;;;; According to DUAL's specification (section 3.4.1. in DR#1), the working
;;;; memory of the system consists of all active agents and only those agents.
;;;; (Note that this number includes all temporary agents 'alive' at the time.)
;;;; It also requires that the activation level of an agent should be above
;;;; certain prespecified threshold (*WM-THRESHOLD* in this program) in order
;;;; to enter the working memory.
;;;; Whenever the activation level of some agent exceeds the threshold, the
;;;; agent is automatically included into the working memory.
;;;; In reverse, whenever the activation level drops below the threshold,
;;;; the agent is automatically excluded from the working memory. Exclusion
;;;; from WM has certain consequences: the contents of the volatile memory
;;;; is lost (see DUAL/ARCHIT/SYMPROC.LSP and =/HYBRID.LSP), temporary links
;;;; (and agents) are destroyed, etc.
;;;;
;;;; DUAL's specification does not clarify whether the working memory is a
;;;; uniform set of agents or is divided into several disjoint subsets.
;;;; This implementation supports the latter (and more general) option.
;;;; There is one PRIMARY-WORKING-MEMORY (kept in the global variable *WM*)
;;;; and an arbitrary number of SPECIAL-WORKING-MEMORIES (the list of which
;;;; is kept in *SPECIAL-WMs*).
;;;;
;;;; Special WMs have activation sources of their own. Agents are included to
;;;; some special WM explicitly and stay there until are explicitly removed.
;;;; The primary WM does not have an activation source of its own. Agents are
;;;; included and removed from it automatically depending on their activation
;;;; levels. The function SPREAD (defined in DUAL/ARCHIT/SPREAD.LSP) takes care
;;;; of this.
;;;;
;;;; Each working memory has a focus which is by definition the agent with
;;;; max. activation. See DUAL/ARCHIT/SPREAD.LSP for focus-related facilities.
;;;;
;;;; This file also introduces the concept WM-AGENT. This is an agent that
;;;; may participate in working memories. All DUAL agents are also WM-agents.
;;;;

;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: *WM*, *special-WMs*,
;;          working-memory, primary-WM, special-WM,
;;          make-primary-WM, make-special-WM,
;;          primary-WM-p, special-WM-p, WM-p,
;;          WM-size, WM-comment, WM-contents, WM-focus, WM-act-source,
;;          now-in-WM, add-to-WM, :enter-WM
;;          remove-from-WM, remove-all-WM,
;;          do-all-WM, do*-all-WM,
;;          WM-conrefs, activate-all-special-WM,
;;          normalize-WM-weights, denormalize-WM-weights,
;;          sort-WM, update-WM-focus
;;
;; Note also *expected-WM-size* defined in DUAL/DEFS.LSP.
;;

;; *WM*
;; *SPECIAL-WMs*
;;
;;   Global variables that store the primary working memory and a list of
;;   special working memories, respectively.
;;   *SPECIAL-WM* is an addendum to *WM*. They are two disjoint sets whose
;;   union comprise the 'conceptual' working memory as specified in DR#1.
;;
;;   At the beginning of each DUAL session, these variables are initialized
;;   as if by the following:
;;      (defvar  *WM*           (make-primary-WM *expected-WM-size*) )
;;      (defvar  *special-WMs*  '() )
;;
;;   That is, there is a predefined primary WM of length *EXPECTED-WM-SIZE*
;;   (a constant defined in DUAL/ARCHIT/DEFS.LSP).
;;   The list of special WMs is empty.
;;   Newly constructed specaial WMs must be explicitly added to this list.
;;   For example:
;;      (push (make-special-WM  10               ; expected size
;;                              1.0              ; activation source capacity
;;                     :comment "Agents on the goal list.")
;;            *special-WMs*)
;;
;;   It is not recommended to alter the contents of these variables through
;;   any means other than the appropriate housekeeping functions:
;;     ADD-TO-WM, REMOVE-FROM-WM, REMOVE-ALL-WM;
;;   If only the aforementioned functions are used, it is guaranteed that all
;;   WMs (considered as sets) are disjoint and that each of them contains no
;;   duplicates.
;;
;;   See also the function SPREAD from DUAL/ARCHIT/SPREAD.LSP.


;; MAKE-PRIMARY-WM  (expected-size &key comment)  -->  new-primary-WM
;;
;;   A function that constructs a primary working memory.
;;   EXPECTED-SIZE should be a positive integer.
;;   :COMMENT should be a string (or NIL). When not supplied,it defaults to NIL.
;;   MAKE-PRIMARY-WM returns the new primary memory.
;;
;;   EXPECTED-SIZE specifies approximately the maximal number of agents
;;   expected to be in the working memory. This is purely an efficiency hint
;;   to the storage allocator, so that implementations using vectors as part
;;   of the WM data structure (the usual technique) will not have to incre-
;;   mentally expand the vectors as new agents are added to it.
;;
;;   After a successful call to MAKE-PRIMARY-WM, the following conditions hold:
;;     (WM-comment  new-primary-WM)  -->  comment  or  NIL
;;     (WM-size     new-primary-WM)  -->  0
;;     (WM-contents new-primary-WM)  -->  empty sequence       ; () or #()
;;     (WM-focus    new-primary-WM)  -->  :FOCUS-UNDEFINED

;; MAKE-SPECIAL-WM  (expected-size act-source &key comment)  --> new-special-WM
;;
;;   A function that constructs a special working memory.
;;   EXPECTED-SIZE should be a positive integer.
;;   ACT-SOURCE should be a number that may be coerced to type SINGLE-FLOAT.
;;   :COMMENT should be a string (or NIL). When not supplied,it defaults to NIL.
;;   MAKE-SPECIAL-WM returns the new special memory.
;;
;;   EXPECTED-SIZE has the same interpretation as in MAKE-PRIMARY-MEMORY.
;;   ACT-SOURCE defines the capacity of new memory's activation source. This
;;   value is used by the function ACTIVATE-ALL-SPECIAL-WM (see below) and
;;   may be changed at any time by (SETF WM-ACT-SOURCE).
;;
;;   After a successful call to MAKE-SPECIAL-WM, the following conditions hold:
;;     (WM-act-source new-special-WM)  -->  act-source
;;     (WM-comment    new-special-WM)  -->  comment  or  NIL
;;     (WM-size       new-special-WM)  -->  0
;;     (WM-contents   new-special-WM)  -->  empty sequence       ; () or #()
;;     (WM-focus      new-special-WM)  -->  :FOCUS-UNDEFINED
;;

;; PRIMARY-WM-P  (thing)  -->  T or NIL
;; SPECIAL-WM-P  (thing)  -->  T or NIL
;;         WM-P  (thing)  -->  T or NIL
;;
;;   Predicates that check whether something is a working memory of the
;;   appropriate type.
;;
;;   The types PRIMARY-WM and SPECIAL-WM are disjoint subclasses of WM.
;;   Note, however, that they do not necessarily form a partition of the latter
;;   because additional (sub)types of working memories may be defined in the
;;   future.
;;
;;   It is always true that:
;;      (primary-WM-p (make-primary-WM ...))   -->  T
;;      (primary-WM-p (make-special-WM ...))   -->  NIL
;;      (special-WM-p (make-special-WM ...))   -->  T
;;      (special-WM-p (make-primary-WM ...))   -->  NIL
;;      (if (primary-WM-p x) (WM-p x) t)       -->  T
;;      (if (special-WM-p x) (WM-p x) t)       -->  T
;;      (and (primary-WM-p x) (special-WM x))  -->  NIL
;;

;; WM-SIZE (working-memory)  -->  WM-size
;;
;;   A generic function that returns the number of agents that currently
;;   participate in WORKING-MEMORY.
;;
;;   WORKING-MEMORY should be a working memory (i.e. should satisfy WM-P).
;;   If it isn't, an error is signaled.
;;
;;   The form   (WM-size WM)  is equivalent to the form
;;     (length (WM-contents WM))  except that it is potentially more efficient.
;;   It also provides an additional layer of data abstraction.
;;
;;   It is always true that:
;;      (WM-size (make-primary-memory whatever-expected-size))  -->  0
;;      (WM-size (make-special-memory ...))                     -->  0
;;      (progn (remove-all-WM WM) (WM-size WM))                 -->  0

;; WM-COMMENT (working-memory)  -->  comment
;; (SETF WM-COMMENT)
;;
;;   A generic function that accesses the comment of WORKING-MEMORY.
;;   Returns a string or NIL. May be used with SETF.
;;   Signals an error if WORKING-MEMORY is not a working memory.

;; WM-CONTENTS (working-memory)  -->  sequence
;;
;;   A generic function that reads the contents of a working memory.
;;   WORKING-MEMORY should be a working memory; if not, an error is signalled.
;;   WM-CONTENTS returns a sequence (a vector or a list) of agents.
;;   This sequence may sensibly be fed to FIND, EVERY, COPY-SEQ or other
;;   _non_destructive_ sequence function.
;;
;;   Programmers are very strongly discouraged to perform any destructive
;;   operations on the sequence returned by WM-CONTENTS. If they do it, the
;;   results are unpredictable, particularly with special WMs.
;;   Use the appropriate housekeeping functions instead:
;;   ADD-TO-WM, REMOVE-FROM-WM, SORT-WM, DO-ALL-WM, ACTIVATE-ALL-SPECIAL-WM,etc.

;; WM-FOCUS (working-memory)  -->  WM-agent  or  :FOCUS-UNDEFINED
;;
;;   A generic function that accesses the focus of WORKING-MEMORY.
;;   WORKING-MEMORY should be a working memory; if not, an error is signaled.
;;
;;   It is always true that:
;;      (WM-focus (make-primary-WM ...))         -->  :FOCUS-UNDEFINED
;;      (WM-focus (make-special-WM ...))         -->  :FOCUS-UNDEFINED
;;      (progn (remove-all-WM WM) (WM-focus WM)) -->  :FOCUS-UNDEFINED
;;
;;   The only way to change the focus is through the function UPDATE-WM-FOCUS.

;; WM-ACT-SOURCE (special-WM)  -->  act-source-capacity
;; (SETF WM-ACT-SOURCE)
;;
;;   A generic function that accesses the activation source of SPECIAL-WM.
;;   May be used with SETF to set the capacity of this source.
;;   The function returns a single-float.
;;
;;   SPECIAL-WM should be a special working memory, i.e. it should satisfy
;;     SPECIAL-WM-P. If it isn't, an error is signaled.
;;   The new value set by (SETF WM-ACT-SOURCE) should be a positive single-float
;;
;;   The value of WM-ACT-SOURCE is used by ACTIVATE-ALL-SPECIAL-WM (see below).


;;  NOW-IN-WM (agent)  -->  working-memory  or  NIL
;;
;;   A generic function that returns the working memory to which AGENT belongs
;;   at the moment or NIL if it does not belong to any.
;;
;;   AGENT should be a WM-agent; if it isn't, an error is signaled.
;;
;;   If all working memory operations are performed through the functions
;;   defined in this file (i.e. no destructive functions have ever been applied
;;   to WM-CONTENTS of any working memory), then the following holds:
;;      (eq WM (now-in-WM agent))   iff   (find agent (WM-contents WM))
;;      (progn (remove-from-WM agent) (now-in-WM agent))  -->  NIL
;;

;; ADD-TO-WM (agent WM &optional weight)  -->  T or NIL
;;
;;   A generic function that adds an agent to WM, provided it is not already
;;   a member of some working memory (as determined by NOW-IN-WM).
;;   ADD-TO-WM is the _only_ standardized means of performing that operation.
;;
;;   AGENT should be a WM-agent; if it isn't, an error is signaled.
;;   WM should be a working memory; if it isn't, an error is signaled.
;;   When supplied, WEIGHT should be a number that can be coerced to type
;;   single-float. If it cannot, COERCE will signal an error.
;;   When not supplied, WEIGHT defaults to 1.0.
;;   The function returns T or NIL depending on whether it has actually
;;   changed the contents of WM.
;;
;;   WEIGHT matters only when WM is a special WM (i.e. satisfies SPECIAL-WM-P).
;;   When WM is a primary WM, WEIGHT is ignored.
;;
;;   More concretely, ADD-TO-WM works according to the following algorithm:
;;     1. If  (NOW-IN-WM AGENT)  returns non-NIL, then return NIL and stop.
;;     2. Otherwise, add AGENT to WM-CONTENTS of WM and...
;;     3. If WM is a primary working memory, go to step 5.
;;     4. If WM is a special working memory, then arrange that AGENT will
;;        receive activation proportional to WEIGHT on each call to the
;;        function ACTIVATE-ALL-SPECIAL-WM. Having done that, normalize all
;;        weights in WM by a call to NORMALIZE-WM-WEIGHTS (see below).
;;        (Thus, after each call to ADD-TO-WM the sum of the absoulute weight
;;         values is equal to NORMALIZE-WM-WEIGHTS' default SUM, that is, 1.0.)
;;     5. Trigger AGENT's symbolic processor by sending it a :ENTER-WM message.
;;        (See TRIGGER-SYMBOLIC-PROCESSOR in DUAL/ARCHIT/SYMPROC2.LSP.)
;;     6. Finally, return T.
;;
;;   Note that when AGENT is a member of WM, (NOW-IN-WM AGENT) will be non-NIL
;;   and hence ADD-TO-WM will return NIL without doing anything. In other
;;   words, it is not possible to add the same agent to the same WM twice.
;;   Note also that when AGENT is a member of some other WM, (NOW-IN-WM AGENT)
;;   will be non-NIL (again) and hence ADD-TO-WM will return NIL (again) without
;;   doing anything. In other words, it is not possible for an agent to parti-
;;   cipate in two different working memories at the same time. Thus, all
;;   working memories are kept disjoint.
;;
;;   Programmers may define additional methods (preferably :AFTER methods) to
;;   ADD-TO-WM in order to customize its behavior. They may also define
;;   methods for TRIGGER-SYMBOLIC-PROCESSOR specializing on (EQL :ENTER-WM).
;;
;;   It is always true that:
;;      (progn (add-to-WM agent) (now-in-WM agent))   -->  non-NIL
;;      (if (now-in-WM agent) (add-to-WM-agent) nil)  -->  NIL
;;      (if (now-in-WM agent) t (add-to-WM-agent))    -->  T
;;      (let ((old-size (WM-size WM)))
;;        (if (add-to-WM agent WM)
;;            (= (WM-size WM) (+ 1 old-size))
;;            (= (WM-size WM) old-size) ))            -->  T
;;
;;   See also REMOVE-FROM-WM.

;; TRIGGER-SYMBOLIC-PROCESSOR (agent :ENTER-WM)  -->  unspecified
;;
;;   A primary method for the generic function TRIGGER-SYMBOLIC-PROCESSOR
;;   (defined in DUAL/ARCHIT/SYMPROC2.LSP) with the following method signature:
;;     trigger-symbolic-processor ((agent WM-agent) (event (eql :enter-WM)))
;;
;;   This method returns NIL without doing anything. Other programming
;;   modules may shadow this method by defining primary methods for more
;;   specific kinds of agents.  For instance, marker-passing agents (see DUAL/
;;   ARCHIT/MP_AGENT.LSP) sometimes emit a marker on entering the WM.


;; REMOVE-FROM-WM (agent &optional reset-p)  -->  T or NIL
;;
;;   A generic function that removes AGENT from the working memory in which
;;   it participates (if any).
;;   Returns T if the agent did participate in some WM and has been removed
;;   or NIL if  (NOW-IN-WM AGENT)  was NIL anyway.
;;   After AGENT has been removed, (NOW-IN-WM AGENT) will always return NIL.
;;   AGENT should be a WM-agent; if it isn't, an error is signaled.
;;   RESET-P should be T or NIL. It defaults to T.
;;
;;   RESET-P controls whether the agent should be 'reset' when leaving the WM.
;;   When RESET-P is NIL, no changes are done to AGENT (apart from canceling
;;     its memory membership, of course).
;;   When RESET-P is T (the default), the activation level and the connection-
;;     ist input zone of AGENT are set to 00, as if by evaluating the forms:
;;       (setf (agent-activation agent) 0.0)   ; see DUAL/ARCHIT/CONNECT.LSP
;;       (prepare-to-receive-activation)
;;     These forms are evaluated even when (NOW-IN-WM AGENT) is NIL.
;;
;;   If AGENT participated in a special WM, after his removal from there the
;;   weights of the remaining elements of that special WM are renormalized by a
;;   call to NORMALIZE-WM-WEIGHTS (see below).
;;
;;   Other modules of the program are encouraged to provide :BEFORE methods
;;   for REMOVE-FROM-WM.  The aim of these methods is to provide additional
;;   cleanup of the agent.  However, they should reset it _only_ when RESET-P
;;   is bound to T.  In particular, CLEAR-VOLATILE-MEMORY is in order
;;   (see DUAL/ARCHIT/SYMPROC.LSP and DUAL/ARCHIT/DUAL_AG.LSP).
;;   Temporary agents (see DUAL/ARCHIT/TEMP_AG.LSP) die when leaving the WM.
;;
;;   It is always true that:
;;      (progn (remove-from-WM agent) (now-in-WM agent))         -->  NIL
;;      (progn (remove-from-WM agent) (agent-activation agent))  -->  0.0
;;      (let ((act (agent-activation agent)))
;;        (remove-from-WM agent NIL)                             -->  T
;;        (= act (agent-activation agent)) )
;;

;; REMOVE-ALL-WM (working-memory &optional reset-p)  -->  NIL
;;
;;   A function that removes all agents from WORKING-MEMORY.
;;
;;   WORKING-MEMORY should be a working memory; if not, an error is signaled.
;;   RESET-P should be T or NIL. It defaults to T.
;;   REMOVE-ALL-WM always returns NIL and is used for its side effect only.
;;
;;   The actual removal is done by calling REMOVE-FROM-WM and hence performing
;;   any cleanup side effects associated with it. The parameter RESET-P has
;;   the same interpretation and default value as in REMOVE-FROM-WM.
;;
;;   The order of passing agents to REMOVE-FROM-WM is implementation dependent.
;;   REMOVE-ALL-WM is equivalent to the form:  (do*-all-WM (agent WM nil)
;;                                               (remove-from-WM agent reset-p))
;;   except that is potentially more efficient.
;;
;;   It is always true that:
;;      (progn (remove-all-WM WM) (WM-size  WM))  -->  0
;;      (progn (remove-all-WM WM) (WM-focus WM))  -->  :FOCUS-UNDEFINED
;;

;; DO-ALL-WM  (var WM-form [resultform]) {form}*  -->  dependent-on-RESULTFORM
;;
;;   A macro that iterates through all members of a working memory and executes
;;   a user-supplied piece of code over each member.  Similar to DOLIST.
;;   Left-to-right order. Side effects restricted in the body. See DO*-ALL-WM.
;;
;;   VAR should be a symbol. It is not evaluated and serves as a variable name.
;;   WM-FORM is a LISP form that produces a working memory. If not -- error.
;;   RESULTFORM is an arbitrary LISP form. When not supplied it defaults to NIL.
;;
;;   First DO-ALL-WM evaluates WM-FORM to produce a working memory.  It then
;;   executes the forms in the body once for each agent participating in that
;;   working memory, with the variable VAR bound to the agent.  The memory is
;;   traversed in left-to-right order as if by DOLIST.  Finally, RESULTFORM (a
;;   single form, not an implicit PROGN) is evaluated, and the result is the
;;   value of the DO-ALL-WM form.  When the RESULTFORM is evaluated, the control
;;   variable VAR is still bound and has the value NIL. If RESULTFORM is omitted,
;;   the result is NIL.
;;
;;   An explicit RETURN statement may be used to terminate the loop and return
;;   a specified value.  Declarations may appear at the front of the body. Tags
;;   and GO's are not allowed (i.e. DO-ALL-WM is not an implicit TAGBODY).
;;
;;   The user-supplied body of DO-ALL-WM may _not_ involve any side effects
;;   that destructively modify the contents of the working memory being
;;   traversed. (See section 7.9 in CLtL2.)  If particular, calls to ADD-TO-WM
;;   or REMOVE-FROM-WM may lead to unpredictable results.  It is save to
;;   perform side effects on WM's members themselves, as long as their
;;   membership in the working memory is not changed.  It is also safe to
;;   modify any working memories different from the one being traversed.
;;
;;   The aforementioned restrictions are somehow relaxed in the body of a
;;   companion macro -- DO*-ALL-WM -- at the expense of the neat linear
;;   traversal order (see below).
;;
;;   Examples:
;;     (do-all-WM (agent WM)                    -->  returns NIL after preparing
;;       (prepare-to-receive-activation agent))        all agents in WM
;;     (do-all-WM (agent WM)                    -->  returns NIL after sending
;;       (when (on-my-radio-freq-p agent)              messages to all agents in
;;         (send-radio-msg agent)))                    WM on that radion freq.
;;     (let ((count 0))                -->  equivalent to  (WM-size WM)
;;       (do-all-WM (agent WM count)
;;         (declare (ignore agent))
;;         (incf count) ))
;;     (do-all-WM (agent WM)           -->  returns a wanted agent or NIL
;;       (when (wanted-p agent)
;;         (return agent) ))
;;
;;   See also DO-ALL-AGENTS in DUAL/ARCHIT/BASIC.LSP,
;;     DO-ALL-REFERENCES in DUAL/ARCHIT/MCRFRAME.LSP, and
;;     DO-ALL-PROCESSES in DUAL/ARCHIT/AGENDA.LSP.
;;

;; DO*-ALL-WM (var WM-form [resultform]) {form}*  -->  dependent-on-RESULTFORM
;;
;;   A macro that is similar to DO-ALL-WM but allows certain side effects
;;   in the body.  Traversal order is implementation-dependent.
;;
;;   DO*-ALL-WM is identical to DO-ALL-WM except the following differences:
;;    -- The user-supplied forms in the body may call REMOVE-FROM-WM to remove
;;       the agent _currently_ being processed. (For example, REMOVE-ALL-WM
;;       does exactly this.)  If the body tries to remove some _other_ member
;;       of WM, the results are unpredictable. Compare with DO-ALL-AGENTS
;;       from DUAL/ARCHIT/BASIC.LSP and DO-ALL-PROCESSES from =/AGENDA.LSP.
;;    -- The user-supplied forms in the body may call (ADD-TO-WM some-agent WM)
;;       to add new agents to WM.  It is implementation-dependent whether the
;;       body will be executed on the newly-added agent during the current
;;       execution of DO*-ALL-WM. It is guaranteed, however, that the newly-
;;       added agent will be heeded during next invocations of DO*-ALL-WM.
;;    -- The exact order in which DO*-ALL-WM visits the elements of the WM
;;       is implementation-dependent.  It is guaranteed that all agents that
;;       participated in the WM _at_the_beginning_ of the iterations will be
;;       visited exactly once.  DO*-ALL-WM may also invoke the forms in the
;;       body to some new agents that have been added to WM along the way,
;;       although this is implementation-dependent.
;;    -- DO*-ALL-WM is less efficient than DO-ALL-WM.
;;
;;   The moral is that you should use DO-ALL-WM whenever possible and recourse
;;   to DO*-ALL-WM only when you need to add/remove agents within the cycle.
;;
;;   Examples:
;;     (do*-all-WM (agent WM)              ; equivalent to  (remove-all-WM WM)
;;       (remove-from-WM agent))
;;     (do*-all-WM (agent WM)              ; part of the function SPREAD
;;       (when (< (agent-activation agent) *WM-threshold*)
;;         (remove-from-WM agent)))


;; SORT-WM (working-memory)  -->  sorted-WM-contents
;;
;;   A generic function that sorts the members of WORKING-MEMORY according
;;   to their activation level, the most active agent coming first.
;;
;;   WORKING-MEMORY should be a working memory; if not, an error is signaled.
;;   SORT-WM returns the sorted WM contents (see WM-CONTENTS above). It is
;;   a sequence (i.e. a vector or a list) and may sensibly be fed to functions
;;   like FIND, EVERY, or COPY-SEQ.
;;
;;   After a call to SORT-WM, the contents of WM remains sorted until agents
;;   are added to or removed from WM, or until their activation levels change.
;;   In particular, a call to DO-ALL-WM immediately after a call to SORT-WM
;;   will visit the agents according to the order established by SORT-WM.
;;
;;   The actual sorting is done as if by the form
;;      (sort (WM-contents WM) #'> :key #'agent-activation)    except that all
;;   informtn associated with any agent within WM is moved in parallel with it.
;;   Programmers are strongly recommended _not_ to issue this request them-
;;   themselves, however, particularly on special WMs. (That may shuffle
;;   the weights associated with the agents.)
;;
;;   It is always true that:
;;      (eq (sort-WM WM) (WM-contents WM))  -->  T
;;

;; UPDATE-WM-FOCUS (working-memory)  -->  new-focus
;;
;;   A generic function that updates the focus of WORKING-MEMORY.
;;
;;   WORKING-MEMORY should be a working memory; if not, an error is signaled.
;;   The function returns the new focus. It is also stored in WORKIN-MEMORY's
;;     WM-FOCUS field.
;;
;;   NEW-FOCUS is the most active agent in WORKING-MEMORY or the keyword
;;     :FOCUS-UNDEFINED when WM-CONTENTS is empty (i.e. there are no agents).
;;
;;   When there are several agents with equal (and maximal) activation level,
;;     it is implementation-dependent which one of them will be returned.
;;     (By the way, such collisions should happen with infinitesimal probability
;;      because activation levels are continuous variables.)
;;

;;;;;;;;;  FUNCTIONS  DEALING  WITH  SPECIAL  WORKING  MEMORIES   ;;;;;;;;;;;
;;

;; WM-CONREFS (special-WM &optional raw-weight-p)  -->  conref-list
;;
;;   A generic function that returns the weights associated with each member
;;   of SPECIAL-WM in the form of a list of connectionist references or
;;   'conrefs' (see DUAL/ARCHIT/CONREF.LSP).
;;
;;   SPECIAL-WM should be a special working memory, that is, it should satisfy
;;   SPECIAL-WM-P. If it isn't, an error is signaled.
;;   RAW-WEIGHT-P should be either T or NIL. It defaults to NIL.
;;   The function returns a list of conrefs analogous to the list returned
;;   by AGENT-NEIGHBORS (see DUAL/ARCHIT/CONNECT.LSP and =/CONREF.LSP).
;;
;;   The length of the list returned by WM-CONREFS is always equal to the
;;   size of SPECIAL-WM (as determined by WM-SIZE).
;;   The order of the conrefs reflects the order of the agents in WM-CONTENTS.
;;   In particular, if SPECIAL-WM have been sorted (see SORT-WM above), this
;;   order will be preserved in the conref list returned by WM-CONREFS.
;;
;;   RAW-WEIGHTS-P controls which weights will be used. If it is T, the original
;;   ('raw') weights will be used, that is,the weights given to ADD-TO-WM on the
;;   addition of each individual agent. If RAW-WEIGHTS-P is NIL (the default),
;;   the resulting conref list contains the 'effective' weights, that is, the
;;   weights that are actually used by ACTIVATE-ALL-SPECIAL-WM (see below).
;;   If the weights in SPECIAL-WM have been denormalized (see DENORMALIZE-WM-
;;   WEIGHTS below), 'raw' weights and 'effective' weights are identical, so
;;   WM-CONREFS always returns the same conref list regardless of the value of
;;   RAW-WEIGHTS-P.
;;
;;   Example:
;;     (remove-all-agents special-WM)         -->  NIL
;;     (add-to-WM #$ag1 special-WM 0.9)       -->  T
;;     (add-to-WM #$ag2 special-WM 0.6)       -->  T
;;     (normalize-WM-weights special-WM 1.0)  -->  T
;;  ;; and now
;;     (WM-conrefs special-WM nil)  -->  ((#$ag1 0.6) (#$ag2 0.4))   ; normalzd
;;     (WM-conrefs special-WM t)    -->  ((#$ag1 0.9) (#$ag2 0.6))   ; raw
;;

;; ACTIVATE-ALL-SPECIAL-WM (special-WM)  -->  NIL
;;
;;   A generic function that activates all agents in SPECIAL-WM taking
;;   activation from WM-ACT-SOURCE and proportional to the (usually normalized)
;;   weight of each agent.
;;
;;   SPECIAL-WM should be a special working memory; if not, an error is signld.
;;   The function always return NIL and is useful for the side effect.
;;
;;   Agents are activated as if by the following form, except that a direct
;;   call to ACTIVATE-ALL-SPECIAL-WM is usually much more efficient:
;;     (mapc #'(lambda (conref)
;;               (receive-activation (conref-reference conref)
;;                                   (* (WM-act-source special-WM)
;;                                      (conref-weight conref))))
;;           (WM-conrefs special-WM nil) )
;;
;;   That is, agents are visited in the order they appear in WM-CONTENTS
;;   and a weighted portion of WM-ACT-SOURCE is sent to each of them via
;;   a call to RECEIVE-ACTIVATION (see DUAL/ARCHIT/CONNECT.LSP).
;;
;;   It is useful if all agents have been prepared to receive activation prior
;;   to the call to ACTIVATE-ALL-SPECIAL-WM. Use the form
;;     (do-all-WM special-WM #'prepare-to-receive-activation) for that purpose.
;;   ACTIVATE-ALL-SPECIAL-WM, however, does not do this itself.
;;
;;   See also the function SPREAD defined in DUAL/ARCHIT/SPREAD.LSP.
;;

;; NORMALIZE-WM-WEIGHTS (special-WM &optional sum)  -->  T or NIL
;;
;;   A generic function that normalizes the weights associated with the
;;   agents in SPECIAL-WM. Normalization is done in a way, similar to that
;;   of the function NORMALIZE-WEIGHTS defined in DUAL/ARCHIT/CONREF.LSP.
;;   The weights are rescaled, so that the sum of their absoulte values
;;   equals SUM.
;;
;;   SUM should be a positive single-float; it defaults to 1.0 .
;;   SPECIAL-WM should be a special working memory; if not, an error is signld.
;;
;;   When there are no weights (and no agents) or all weights are zero, they
;;   cannot be multiplied so that their sum becomes non-zero. In such cases,
;;   NORMALIZE-WM-WEIGHTS returns NIL without doing anything.
;;   If there is at least one weight that is different from zero, all weights
;;   are normalized and NORMALIZE-WM-WEIGHTS returns T.
;;   Thus, it is always true that, if NORMALIZE-WEIGHTS returns T, the sum
;;   of absolute values of the weights has been made equal to SUM;
;;   if NORMALIZE-WEIGHTS returns NIL, the sum is zero.
;;
;;   Each call to ADD-TO-WM and REMOVE-FROM-WM contains an implicit call to
;;   DENORMALIZE-WM-WEIGHTS followed by an implicit call to NORMALIZE-WM-WEIGHTS
;;
;; !! WARNING: Evaluating NORMALIZE-WM-WEIGHTS more than once in succession
;; !!          without intervening call to DENORMALIZE-WM-WEIGHTS may result
;; !!          in irreversible loss of information. In particular, the original
;; !!          weight values (the 'raw weights') may be lost. In all cases,
;; !!          however, the proportions among the weights are preserved.
;; !!          (See the example below.)
;; !!
;; !!          The same warning applies to situations in which NORMALIZE-WM-
;; !!          WEIGHTS is called after a call to ADD-TO-WM or REMOVE-FROM-WM
;; !!          without intervening call to DENORMALIZE-WM-WEIGHTS.
;;

;; DENORMALIZE-WM-WEIGHTS (special-WM)  -->  sum-weight
;;
;;   A generic function that reverses the effect of NORMALIZE-WM-WEIGHTS.
;;
;;   Returns the sum of the absolute values of the weights that are currently
;;   in effect (i.e. that will be used by ACTIVATE-ALL-SPECIAL-WM).
;;
;;   Each call to ADD-TO-WM and REMOVE-FROM-WM contains an implicit call to
;;   DENORMALIZE-WM-WEIGHTS followed by an implicit call to NORMALIZE-WM-WEIGHTS
;;
;;   DENORMALIZE-WM-WEIGHTS may not succeed in restoring the original ('raw')
;;   weights if there have been two successive calls to NORMALIZE-WM-WEIGHTS.
;;   In contrast, repeated denormalization is safe.
;;
;;   Example:
;;     (remove-all-agents special-WM)         -->  NIL
;;     (add-to-WM #$ag1 special-WM 0.9)       -->  T
;;     (add-to-WM #$ag2 special-WM -0.6)      -->  T
;;   ; and now
;;     (WM-conrefs special-WM)   -->  ((#$ag1 0.6) (#$ag2 -0.4))   ; sum = 1.0
;;     (denormalize-WM-weights special-WM 1.0)  -->  1.5
;;     (WM-conrefs special-WM)   -->  ((#$ag1 0.9) (#$ag2 -0.6))   ; sum = 1.5
;;     (normalize-WM-weights special-WM 0.5)    -->  T
;;     (WM-conrefs special-WM)   -->  ((#$ag1 0.3) (#$ag2 -0.2))   ; sum = 0.5
;;     (WM-conrefs special-WM t) -->  ((#$ag1 0.9) (#$ag2 -0.6))   ; sum = 1.5
;;   ; WARNING: don't do this:
;;     (normalize-WM-weights special-WM 1.0)  -->  T   ; repeated normalization
;;     (WM-conrefs special-WM nil) -->  ((#$ag1 0.6) (#$ag2 -0.4))  ; sum = 1.0
;;     (WM-conrefs special-WM t)   -->  ((#$ag1 0.3) (#$ag2 -0.2))  ; sum = 0.5
;;                ;; the original raw weight sum (1.5) has been overwritten ^^^.
;;
;;   Second example -- zero weight:
;;     (remove-all-agents special-WM)    -->  NIL
;;     (add-to-WM #$ag1 special-WM 0.0)  -->  T
;;   ; and now
;;     (normalize-WM-weights special-WM)    -->  NIL       ; 0.0 cannot be 1.0
;;     (denormalize-WM-weights special-WM)  -->  0.0
;;



;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric WM-size (working-memory)
  (:documentation "The number of agents currently in WORKING-MEMORY." ))

(defgeneric add-to-WM (agent WM &optional weight)
  (:documentation
   "Adding AGENT to WM provided it isn't already in (some) working memory."))
   ;; Returns T if the agent has been added or NIL otherwise.

(defgeneric remove-from-WM (agent &optional reset-p)
  (:documentation
   "Removes AGENT from the working memory to which it belongs (if any)." ))

(defgeneric remove-all-WM (working-memory &optional reset-p)
  (:documentation "Removes all agents from WORKING-MEMORY." ))

(defgeneric WM-conrefs (special-WM &optional raw-weight-p)
  (:documentation "Returns a list of connectionist references (conref's)." ))

(defgeneric normalize-WM-weights (special-WM &optional sum)
  (:documentation "Sum_i (abs weight_i)  ==  SUM." ))

(defgeneric denormalize-WM-weights (special-WM)
  (:documentation "Restores original (denormalized) weights." ))

(defgeneric  activate-all-special-WM (special-WM)
  (:documentation "Sends to each agent a weighted amount from WM-ACT-SOURCE." ))

(defgeneric  sort-WM (working-memory)
  (:documentation "Sorts all agents in decreasing order of their activation." ))

(defgeneric  update-WM-focus (working-memory)
  (:documentation
    "Updates the FOCUS slot of a WM, finding its most active agent." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************



;;;;;;  *****    W O R K I N G   M E M O R I E S    *****
;;;
;;;  This portion of the file defines the class WORKING-MEMORY and its
;;;  associated subclasses, constructors, printing methods, and variables.
;;;

;;;;;;   Class definitions  (and accessor methods)

(eval-when (compile load eval)
  (defclass working-memory (DUAL-object)
    ((comment     :accessor    WM-comment
                  :type        (or string null)    ; NIL stands for 'no comment'
                  :initarg     :comment
                  :initform    nil     )
     (contents    :reader      WM-contents
                  :type        (vector (or null base-agent))
                  :initarg     :contents
                  :initform    (required-argument)  )
     (focus       :accessor    WM-focus            ; see DUAL/ARCHIT/SPREAD.LSP
                  :type        (or base-agent
                                   (member :focus-undefined))
                  :initform    :focus-undefined  )
    )
   (:documentation "Working memory: a set of active agents. A base class." ))


  (defclass  primary-WM (working-memory)
    ()  ; no additional slots
    (:documentation "Primary (as opposed to special) working memory." ))


  (defclass  special-WM (working-memory)
    ((weights     :reader      WM-weights              ; for internal use only
                  :type        (vector single-float)
                  :initarg     :weights
                  :initform    (required-argument)
                  :documentation  "Parallel to WM-CONTENTS." )
     (norm-factor :accessor    WM-norm-factor          ; for internal use only
                  :type        single-float
                  :initform    0.0
                  :documentation  "Normalization factor for WM-WEIGHTS." )
     (act-source  :accessor    WM-act-source           ; for the external prot.
                  :type        single-float
                  :initarg     :act-source
                  :initform    (required-argument)
                  :documentation  "Capacity of special-WM's activation source." )
    )
   (:documentation "Working memory whose members receive special support." ))
) ; eval-when

;;;;  Constructors

(defun make-primary-WM  (expected-size &key (comment nil) )
  "Constructor for primary working memories."
  (declare (values primary-WM)
           (type (integer 0 *) expected-size)
           (type (or null string) comment) )
  (let ((contents (make-array expected-size
                              :element-type '(or null base-agent)
                              :fill-pointer 0
                              :adjustable t
                              :initial-element nil) ))
    (make-instance 'primary-WM :comment comment
                   :contents contents)
    ;; WM-FOCUS initialized automatically to :FOCUS-UNDEFINED.
  ))


(defun make-special-WM  (expected-size act-source &key (comment nil) )
  "Constructor for special working memories."
  (declare (values special-WM)
           (type (integer 0 *) expected-size)
           (type number act-source)
           (type (or null string) comment) )
  (let ((contents (make-array expected-size
                              :element-type '(or null base-agent)
                              :fill-pointer 0
                              :adjustable t
                              :initial-element nil) )
        (weights  (make-array expected-size
                              :element-type 'single-float
                              :fill-pointer 0
                              :adjustable t
                              :initial-element 0.0) ) )
    (make-instance 'special-WM :comment comment
                               :contents contents
                               :weights weights
                               :act-source (coerce act-source 'single-float) )
    ;; WM-FOCUS and WM-NORM-FACTOR initialized automatically.
  ))


;;;;;;;  Global variable definitions

(defvar  *WM*  (make-primary-WM *expected-WM-size* )
  "The primary working memory of the system; a set of active DUAL agents."  )

(defvar  *special-WMs*  '()
  "A list of all special working memories in the system." )


;;;;;;  Printing methods

(defmethod  print-object ((WM working-memory) stream)
  (format stream "#<~A~@[ ~A~]>"
          (type-of WM)
          (if (slot-boundp WM 'comment)
                (WM-comment WM)
                nil)) )

(defmethod DUAL-describe  ((WM working-memory)
                            &optional (stream *standard-output*))
  (format stream "~&~S is a ~A.~%"
          WM  (type-of WM) )
  (format stream "~&  Its comment string is: ~A~%"
                 (if (and (slot-boundp WM 'comment)
                          (WM-comment WM))          ; supplied?
                     (WM-comment WM)
                     "(no comment)"))
  (format stream "~&  Its currently contains ~D agent~:P.~%"
                 (WM-size WM) )
  (format stream "~&  Its focus is: ~S~%"
                 (if (slot-boundp WM 'focus)
                     (WM-focus WM)
                     "(unbound)"))
  (format stream "~&  Its contents is: ~S~%"
                 (if (slot-boundp WM 'contents)
                     (WM-contents WM)
                     "(unbound)"))
  (values))

(defmethod DUAL-describe :after  ((WM special-WM)
                                   &optional (stream *standard-output*))
  (format stream "~&  Its weight vector is: ~S~%"
          (if (slot-boundp WM 'weights)
              (WM-weights WM)
              "(unbound)"))
  (format stream 
      "~&  Its normalization factor is: ~:[(unbound slot)~;~:*~6,3F~]~%"
          (if (slot-boundp WM 'norm-factor)
              (WM-norm-factor WM)
              nil))
  (format stream
      "~&  Its activation source capacity is: ~:[(unbound slot)~;~:*~6,3F~]~%"
          (if (slot-boundp WM 'act-source)
              (WM-act-source WM)
              nil))
  (values))


;;;;  Type predicates

(declaim (inline primary-WM-p special-WM-p WM-p))

(defun primary-WM-p (thing)
  "Predicate that is T on primary working memories and NIL otherwise."
  (if (typep thing 'primary-WM) t nil))

(defun special-WM-p (thing)
  "Predicate that is T on special working memories and NIL otherwise."
  (if (typep thing 'special-WM) t nil))

(defun WM-p (thing)
  "Predicate that is T on working memories and NIL otherwise."
  (if (typep thing 'working-memory) t nil))


;;;;  Type-checking methods for the accessors

(defmethod  WM-comment ((x t))
  (error "WM-COMMENT: ~S is not a working memory." x))

(defmethod  (setf WM-comment) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF WM-COMMENT): ~S is not a working memory." x))

(defmethod  WM-contents ((x t))
  (error "WM-CONTENTS: ~S is not a working memory." x))

(defmethod  (setf WM-contents) (new-value (x t))
  (declare (ignore new-value 
                   #-:PCL x ))
  (error "WM-CONTENTS cannot be used with SETF. ~
          Use ADD-TO-WM and REMOVE-FROM-WM instead."))

(defmethod  WM-focus ((x t))
  (error "WM-FOCUS: ~S is not a working memory." x))

(defmethod  (setf WM-focus) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF WM-FOCUS): ~S is not a working memory." x))

(defmethod  WM-weights ((x t))
  (error "DUAL-CORE::WM-WEIGHTS: ~S is not a special working memory." x))

(defmethod  (setf WM-weights) (new-value (x t))
  (declare (ignore new-value 
                   #-:PCL x ))
  (error "DUAL-CORE::WM-CONTENTS cannot be used with SETF. ~
          Use ADD-TO-WM and REMOVE-FROM-WM instead."))

(defmethod  WM-norm-factor ((x t))
  (error "DUAL-CORE::WM-NORM-FACTOR: ~S is not a special working memory." x))

(defmethod  (setf WM-norm-factor) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF DUAL-CORE::WM-NORM-FACTOR): ~
          ~S is not a special working memory." x ))

(defmethod  WM-act-source ((x t))
  (error "DUAL-CORE::WM-ACT-SOURCE: ~S is not a special working memory." x))

(defmethod  (setf WM-act-source) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF DUAL-CORE::WM-ACT-SOURCE): ~
          ~S is not a special working memory." x ))



;;;;;;  *****    W O R K I N G - M E M O R Y   A G E N T S    *****
;;;
;;;  This portion of the file defines the class WM-AGENT and its associates.
;;;  WM-agent are agents suitable for membership in a working memory.
;;;

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass WM-agent (connectionist-agent)
    ((work-mem    :reader      now-in-WM             ; for the external protocol
                  :accessor    agent-WM              ; for internal use only
                  :type        (or (member nil
                                           '*WM-candidates*) ; see ARCHIT/SPREAD
                                   working-memory)
                  :initform    nil
                  :documentation  "Agent's current membership." )
    )
   (:documentation "Agent that can participate in a working memory. Mix-in." ))
)

;;;;  Constructor

;; No constructor is provided for WM-AGENT because this is a mix-in class
;; that is not intended to stand alone.
;; Use MAKE-DUAL-AGENT, defined in DUAL/ARCHIT/DUAL_AG.LSP.
;; See also MAKE-CONN-AGENT, defined in DUAL/ARCHIT/CONNECT.LSP.


;;;;;;  Printing methods

(defmethod  agent-descriptor-string ((agent WM-agent))
  (if (eq (type-of agent) 'WM-agent)
      "a working-memory agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod DUAL-describe :after ((ag WM-agent)
                                  &optional (stream *standard-output*))
  (format stream "~&  It  currently is a member of: ~S~%"
          (if (slot-boundp ag 'work-mem)
              (agent-WM ag)
              "(unbound)") )
  (values))


;;;;  Type-checking methods for the accessors

(defmethod  agent-WM ((x t))
  (error "DUAL-CORE::AGENT-WM: ~S is not a WM-agent." x))

(defmethod  (setf agent-WM) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF DUAL-CORE::AGENT-WM): ~S is not a WM-agent." x))

(defmethod  now-in-WM ((x t))
  (error "NOW-IN-WM: ~S is not a WM-agent." x))



;;;;;;  *****    WORKING-MEMORY   M A N A G E M E N T     *****
;;;
;;;  This portion of the file defines the tools for WM management --
;;;  adding and removing WM-agents, iteration over working memories, etc.
;;;

(defmethod WM-size ((WM working-memory))
  (fill-pointer (WM-contents WM)) )

(defmethod WM-size ((x t))
  (error "WM-SIZE: ~S is not a working memory." x ))


;;;; Adding to a working memory

(defmethod  add-to-WM  ((ag WM-agent)
                        (WM working-memory)
                        &optional (weight 1.0) )
  (declare (ignore weight))  
  (cond ((now-in-WM ag)  nil)
        (t  (vector-push-extend ag (WM-contents WM))
            (setf (agent-WM ag) WM)    ; register new membership
            (trigger-symbolic-processor ag :enter-WM)
            T)))

(defmethod  add-to-WM ((ag WM-agent)
                       (WM special-WM)
                       &optional (weight 1.0) )
  (cond ((now-in-WM ag)
            nil)
        (t  (vector-push-extend ag (WM-contents WM))
            (setf (agent-WM ag) WM)    ; register new membership
           ;; Add the weight
            (denormalize-WM-weights WM)
            (vector-push-extend (coerce weight 'single-float)
                                (WM-weights WM))
            (normalize-WM-weights WM)
            (unless (= (fill-pointer (WM-contents WM))       ; sanity check
                       (fill-pointer (WM-weights WM)))
              (error "ADD-TO-WM: ~S corrupted. ~
                     (WM-CONTENTS and WM-WEIGHTS have different lengths.)"
                     WM ))
            (trigger-symbolic-processor ag :enter-WM)
            T)))

(defmethod add-to-WM ((x t) (y t) &optional weight)
  (declare (ignore weight))
  (error "ADD-TO-WM: ~S is not a WM-agent or ~S is not a working memory."
         x y) )

(defmethod trigger-symbolic-processor ((agent WM-agent)
                                       (event (eql :enter-WM)))
  (declare (ignore agent event))
  nil )    ; to be shadowed by more specific methods


;;;; Removing from a working memory

(defgeneric  remove-from-WM-aux (agent WM)
  (:documentation
   "Removes AGENT from WM. Returns T on actual removals or NIL otherwise." ))

(defmethod  remove-from-WM-aux ((agent WM-agent) (nl null))
  ;; This method is useful when (AGENT-WM AGENT) is NIL.
  nil )

(defmethod  remove-from-WM-aux ((agent WM-agent) (WM primary-WM))
  (let ((vector (WM-contents WM)))
    (if (zerop (fill-pointer vector))
        NIL                                   ; pathological case: empty vector
        (let ((pos (position agent vector)))
          (cond ((null pos) NIL)              ; not there
                (t  (setf (aref vector pos) (vector-pop vector))   ; last -> gap
                    (setf (aref vector (fill-pointer vector)) nil) ; garb. coll.
                    (setf (agent-WM agent) nil)
                    T))) )))

(defmethod  remove-from-WM-aux ((agent WM-agent) (WM special-WM))
  (let ((vector  (WM-contents WM))
        (weights (WM-weights WM)) )
    (flet ((remove-weight (pos)
             (denormalize-WM-weights WM)
             (setf (aref weights pos) (vector-pop weights))  ; move LAST in gap
             (normalize-WM-weights WM)
             (unless (= (fill-pointer (WM-contents WM))        ; sanity check
                        (fill-pointer (WM-weights WM)))
               (error "REMOVE-FROM-WM-AUX: ~S corrupted. ~
                      (WM-CONTENTS and WM-WEIGHTS have different lengths.)"
                      WM)) ))
      (if (zerop (fill-pointer vector))
          NIL                               ; pathological case: empty vector
          (let ((pos (position agent vector)))
            (cond ((null pos) NIL)
                  (t  (setf (aref vector pos) (vector-pop vector))  ;last -> gap
                      (setf (aref vector (fill-pointer vector)) nil) ; garb.col.
                      (setf (agent-WM agent) nil)
                      (remove-weight pos)
                      T))) ))))

(defmethod remove-from-WM-aux ((x t) (y t))
  (error
"DUALC::REMOVE-FROM-WM-AUX: ~S is not a WM-agent or ~S is not a working memory."
    x y) )


(defmethod remove-from-WM ((agent WM-agent)
                           &optional (reset-p T) )
  (prog1
     (remove-from-WM-aux agent (agent-WM agent))
     (when reset-p
       (setf (agent-activation agent) 0.0)
       (prepare-to-receive-activation agent) )))

(defmethod remove-from-WM  ((x t) &optional (reset-p T) )
  (declare (ignore reset-p))
  (if x
    (error "REMOVE-FROM-WM:  ~S is not a WM-agent." x)
    (error "REMOVE-FROM-WM applied to NIL (perhaps #$missing-agent).") ))


(defmethod remove-all-WM ((WM working-memory)
                          &optional (reset-p T) )
  (setf (WM-focus WM) :focus-undefined)
  (do*-all-WM-aux WM #'(lambda (agent)
    (remove-from-WM agent reset-p))) )    ; The macro is avoided because it's
                                          ; defined later and the compiler errs

(defmethod remove-all-WM ((x t) &optional (reset-p T))
  (declare (ignore reset-p))
  (error "REMOVE-ALL-WM: ~S is not a working memory." x) )



;;;;  WEIGHTS in special working memories.

(defmethod WM-conrefs ((WM special-WM)
                       &optional (raw-weight-p nil) )
  (declare (values list))
  (let ((agents (WM-contents WM))
        (weights (WM-weights WM))
        (norm-factor (WM-norm-factor WM))
        (result '()) )
    (if raw-weight-p
        (dotimes (k (fill-pointer agents) (nreverse result))
          (push (make-conref (aref agents k)
                             (* (aref weights k) norm-factor) )
                result))
        (dotimes (k (fill-pointer agents) (nreverse result))
          (push (make-conref (aref agents k)
                             (aref weights k))
                result)) )))

(defmethod WM-conrefs ((x t) &optional raw-weight-p )
  (declare (ignore raw-weight-p))
  (error "WM-CONREFS: ~S is not a _special_ working memory." x ))


(defmethod normalize-WM-weights ((WM special-WM) &optional (sum 1.0))
  (declare (values boolean))
  (let* ((w-vector (WM-weights WM))
         (w-size (fill-pointer w-vector))
         (w-sum 0.0) )
    (dotimes (k w-size)
      (incf w-sum (abs (aref w-vector k))) )
    (cond ((< w-sum 0.0001)     ; W-VECTOR is empty or contains only 0s.
              (setf (WM-norm-factor WM) 0.0)
              nil)
          (t  (let ((coef (/ sum w-sum)))
                (dotimes (k w-size)
                   (setf (aref w-vector k)
                         (* (aref w-vector k) coef)) ))
              (setf (WM-norm-factor WM) w-sum)
              t) )))

(defmethod normalize-WM-weights ((x t) &optional sum )
  (declare (ignore sum))
  (error "NORMALIZE-WM-WEIGHTS: ~S is not a _special_ working memory." x ))


(defmethod denormalize-WM-weights ((WM special-WM))
  (declare (values single-float))
  (let ((w-vector (WM-weights WM))
        (norm-factor (WM-norm-factor WM))
        (sum 0.0) )
    (dotimes (k (fill-pointer w-vector))
      (incf sum
            (abs (setf (aref w-vector k)
                       (* (aref w-vector k) norm-factor)))) )
    (setf (WM-norm-factor WM) 1.0)         ; foresee repeated denormalization
    sum ))

(defmethod denormalize-WM-weights ((x t))
  (error "DENORMALIZE-WM-WEIGHTS: ~S is not a _special_ working memory." x ))


;;;;;;  Iterators over a working memory

(defmacro do-all-WM  (header &body body)
"Iterates over all agents participating in a working memory. Similar to DOLIST."
  (unless (and (listp header)
               (<= 2 (length header) 3)
               (symbolp (first header)) )    ; var-name
    (error "DO-ALL-WM: Malformed header in ~S."
           (list* 'do-all-WM header body) ))
  (let ((var-name (first header))
        (WM-form (second header))
        (result-form (third  header)) )  ; possibly NIL for two-element headers
    `(block nil
       (dualc::do-all-WM-aux ,WM-form
                             #'(lambda (,var-name) ,@body))
       ,(DOLIST-finalization var-name result-form)) ))   ; see ARCHIT/BASIC.LSP

(defmacro do*-all-WM  (header &body body)
  "Iterates over all agents participating in a working memory.
  Certain side effects allowed in BODY."
  (unless (and (listp header)
               (<= 2 (length header) 3)
               (symbolp (first header)) )    ; var-name
    (error "DO*-ALL-WM: Malformed header in ~S."
           (list* 'do*-all-WM header body) ))
  (let ((var-name (first header))
        (WM-form (second header))
        (result-form (third  header)) )  ; possibly NIL for two-element headers
    `(block nil
       (dualc::do*-all-WM-aux ,WM-form
                              #'(lambda (,var-name) ,@body))
       ,(DOLIST-finalization var-name result-form)) ))   ; see ARCHIT/BASIC.LSP

#+:ACLPC
(eval-when (load)      ; provide more readable messages for the status line
  (when allegro:*save-lambda-lists*
    (setf (get 'do-all-WM 'allegro:lambda-list)
          '((var WM-form &optional result) &rest body))
    (setf (get 'do*-all-WM 'allegro:lambda-list)
          '((var WM-form &optional result) &rest body)) ))


(defun  do-all-WM-aux (WM fun)
  (declare (type (function (base-agent) t) fun)
           (values null) )
  #+:DUAL-DEBUG
    (unless (WM-p WM)
      (error "DUAL-CORE::DO-ALL-WM-AUX: ~S is not a working memory."
             WM ))
  (let ((vector (WM-contents WM)))
    (dotimes (k (fill-pointer vector) nil)
      (funcall fun (aref vector k)) )))

(defun  do*-all-WM-aux (WM fun)
  (declare (type (function (base-agent) t) fun)
           (values null) )
  #+:DUAL-DEBUG
    (unless (WM-p WM)
      (error "DUAL-CORE::DO*-ALL-WM-AUX: ~S is not a working memory."
             WM ))
  (let ((vector (WM-contents WM))
        (k 0) )   ; index in VECTOR
    (flet ((do-one-call (curr-pos curr-agent)
             (funcall fun curr-agent)           ; FUN may call REMOVE-FROM-WM
             (eq curr-agent (aref vector curr-pos)) ))
      (loop
         ;; VARIANT:   d := (fill-ptr - k) decreases with 1 on each iteration
         ;; INVARIANT: When d > 0, A[k] contains an agent to be fetched to FUN
         ;;            and all A[0] ... A[k-1] have been FUN-ed already.
         (when (>= k (fill-pointer vector))
            (return nil))
         (if (do-one-call k (aref vector k))
             (incf k)      ; go to next position
             )             ; intercepting removal -- stay on same pos
       ))))

;;;;;;

(defmethod  activate-all-special-WM ((WM special-WM))
  (let ((agents (WM-contents WM))
        (weights (WM-weights WM))
        (act-source (WM-act-source WM)) )
    (dotimes (k (fill-pointer agents) nil)
      (receive-activation (aref agents k)
                          (* (aref weights k) act-source)) )))

(defmethod activate-all-special-WM ((x t))
  (error "ACTIVATE-ALL-SPECIAL-WM: ~S is not a special working memory." x ))


;;;;;;  Two additional utilities

(defmethod sort-WM ((WM primary-WM))
  (sort (WM-contents WM) #'> :key #'agent-activation) )

(defmethod sort-WM ((WM special-WM))
  ;; combine into conrefs  -->  sort conref-list  -->  split conrefs back
  (let ((conref-list (WM-conrefs WM))
        (agents (WM-contents WM))
        (weights (WM-weights WM)) )
    (setq conref-list (sort conref-list #'>
                       :key #'(lambda (conref)
                                (agent-activation (conref-reference conref))) ))
    (setf (fill-pointer agents)  0)
    (setf (fill-pointer weights) 0)
    (dolist  (conref conref-list)
      (vector-push (conref-reference conref) agents)
      (vector-push (conref-weight conref)    weights) ))
  (WM-contents WM) )

(defmethod sort-WM ((x t))
  (error "SORT-WM: ~S is not a working memory." x ))


(defmethod  update-WM-focus ((WM working-memory))
  (declare (values (or WM-agent
                       (member :focus-undefined))) )
  (let ((old-focus (WM-focus WM))
        (new-focus :focus-undefined)
        (focal-act *min-act*) )
    ;; To avoid setting NEW-FOCUS and FOCAL-ACT to mediocre candidates,
    ;; use the new activation of OLD-FOCUS (if applicable) as a barrier.
       (when (and (not (eq old-focus :focus-undefined))
                  (eq (agent-WM old-focus) WM) )        ; still in our team?
         (setq focal-act (agent-activation old-focus))
         (setq new-focus old-focus) )
    ;; Main cycle
    (do-all-WM (agent WM)
      (when (> (agent-activation agent) focal-act)
        (setq focal-act (agent-activation agent))
        (setq new-focus agent) ))
    ;; Update the slot and return the new focus.
    (setf (WM-focus WM) new-focus) ))

(defmethod update-WM-focus ((x t))
  (error "UPDATE-WM-FOCUS: ~S is not a working memory." x ))


;;;;;;;  End of file DUAL/ARCHIT/WORK_MEM.LSP
