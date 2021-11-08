;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/secretar.lsp
;;; VERSION:    2.2.2  ; see AMBR/VERSION.LSP
;;; PURPOSE:    Secretaries, secretary answers, hypothesis registration, etc.
;;; DEPENDS-ON: DUAL; ambr/ambr_ag.lsp, ambr/fizzle.lsp, ambr/corresp.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    11-05-97
;;; UPDATED:    20-06-97 -- Thorough revision.
;;; UPDATED:    18-02-98 -- Thorough revision of the hypothesis registration
;;;                          protocol. Better treatment of 'trivial hypotheses'.
;;;                          Driver situations are taken into account.
;;; UPDATED:    10-03-98 [2.2.1]
;;; UPDATED:    20-05-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...
;;;;;;;;
;;; TO DO: Account for the :CORR slot during hypoth registration.
;;; TO DO: Decide what to do with the temporary corr-agents on the (permanent)
;;;        :CORR slot.  In general, it is a bad idea to have a permanent link
;;;        from a permanent to a temporary agent, esp. when leaving the WM.


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;          S E C R E T A R I E S            ;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "AMBR-CORE")

;;;; The key concept defined in this file is SECRETARY-AGENT.
;;;; ... mix-in class ...
;;;;


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: agent-hypotheses, register-hypothesis, unregister-hypothesis,
;;          agent-correspondences, other-element;
;;
;;          identify-relevant-hyps,
;;          hypoth-set-winner, hypoth-set-leader,
;;          find-all-hypotheses, find-any-hypothesis,
;;          driver-elt-mapping ;
;;
;;          secretary-symbolic-structure,
;;          hypothesis-registration-request, send-HR-request,
;;          secretary-answer, send-secretary-answer, secretary-answer-p
;;
;; Hypothesis registration depends on the parameter *ELEMENT->HYPOTH-WEIGHT*
;; defined in AMBR/DEFS.LSP.
;;


;; ...

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric agent-hypotheses (secretary &optional symref-only-p)
  (:documentation "Returns the hypothesis list maintained by SECRETARY." ))
  ;; cf. AGENT-SITUATION defined in AMBR/KREPRES.LSP.

(defgeneric agent-correspondences (secretary &optional symref-only-p)
  (:documentation "Returns the correspondence list maintained by SECRETARY." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;  ***********  HYPOTHESIS-REGISTRATION REQUESTS  ************
;;;
;;;  This portion of the file defines the class HYPOTHESIS-REGISTRATION-REQUEST.
;;;  Hypothesis registration requests (or HR-requests for short) are symbolic
;;;  structures (see DUAL/ARCHIT/SYMPROC1.LSP) exchanged between the agents
;;;  during the construction of a constraint-satisfaction network.
;;;

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass  secretary-symbolic-structure (symbolic-structure)
    ()
    (:documentation
      "Symbolic structure sent to or emitted by secretaries. Base class." ))

  (defclass hypothesis-registration-request (secretary-symbolic-structure)
    ((hypoth   :reader      HR-hypothesis      ; for internal use
               :type        corr-agent
               :initarg     :hypothesis
               :initform    (required-argument)  )
     (slot     :reader      HR-slot            ; for internal use
               :type        symbol             ; S-slot-label
               :initarg     :slot
               :initform    (required-argument)  )
    )
   (:documentation
     "Symbolic structure requesting to put HYPOTHESIS on a secretary's list." ))
) ; eval-when


;;;;  Constructor

(defun send-HR-request (secretary hypothesis slot-label)
  "HYPOTHESIS sends a hypothesis-registration request to SECRETARY."
  (declare (type corr-agent hypothesis)
           (type secretary-agent secretary)
           (type symbol slot-label)
           (values hypothesis-registration-request) )
  #+:DUAL-DEBUG
  (unless (member slot-label
                  (list *driver-elt-label* *recipient-elt-label* :trivial))
    (error "SEND-HR-REQUEST: Invalid slot label ~S."  slot-label ))
  (receive-symbol secretary (make-instance 'hypothesis-registration-request
                                           :hypothesis hypothesis
                                           :slot       slot-label )))


;;;;;;  Printing methods

(defmethod  print-object ((HR-request hypothesis-registration-request) stream)
  (if (and (slot-boundp HR-request 'hypoth)
           (slot-boundp HR-request 'slot)
           (typep (HR-hypothesis HR-request) 'corr-agent) )
      (format stream "#<HR ~A>" (agent-name (HR-hypothesis HR-request)))
      (format stream "#<malformed HR-request>") ))

(defmethod DUAL-describe ((HR-request hypothesis-registration-request)
                          &optional (stream *standard-output*) )
  (format stream "~&~S is a ~A.~%"
          HR-request (type-of HR-request) )
  (format stream "~&  Its hypothesis is: ~S~%"
          (if (slot-boundp HR-request 'hypoth)
              (HR-hypothesis HR-request)
              "(unbound)"))
  (format stream "~&  Its slot is: ~S~%"
          (if (slot-boundp HR-request 'slot)
              (HR-slot HR-request)
              "(unbound)"))
  (values))


;;;;  Type-checking methods for the accessors

(defmethod  HR-hypothesis ((x t))
  (error "HR-HYPOTHESIS: ~S is not a hypothesis-registration request." x ))

(defmethod  HR-slot ((x t))
  (error "HR-SLOT: ~S is not a hypothesis-registration request." x ))



;;;;;;  ********  S E C R E T A R Y   A N S W E R S  *********
;;;
;;;  This portion of the file defines the class SECRETARY-ANSWER.
;;;  Secretary answers are symbolic structures that result after a secretary
;;;  has handled a hypothesis registration request.
;;;

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass secretary-answer (secretary-symbolic-structure)
    ((datum    :reader      secretary-answer
               :type        (or list corr-agent)
               :initarg     :datum
               :initform    (required-argument)  )
    )
   (:documentation "Secretary's answer to a hypothesis-registration request." ))
) ; eval-when


;;;;  Constructor

(defun send-secretary-answer (receiver answer)
  "Send secretary ANSWER to the candidate-hypothesis RECEIVER."
  (declare (type hypoth-agent receiver)
           (type (or list corr-agent) answer)
           (values secretary-answer) )
  (receive-symbol receiver (make-instance 'secretary-answer :datum answer)))


;;;;;;  Printing methods

(defmethod  print-object ((answer secretary-answer) stream)
  (if (not (slot-boundp answer 'datum))
      (format stream "#<malformed SA>")
      (typecase (secretary-answer answer)
        (null                 (format stream "#<SA+ nil>"))
        (list                 (format stream "#<SA+ list>"))
        (embryo-hypoth-agent  (format stream "#<SA- embryo>"))
        (mature-hypoth-agent  (format stream "#<SA- mature>"))
        (winner-hypoth-agent  (format stream "#<SA- winner>"))
        (dead-corr-agent      (format stream "#<SA- dead>"))
        (corr-agent           (format stream "#<SA- corresp>"))
        (t                    (format stream "#<malformed SA>")) )))

(defmethod DUAL-describe ((answer secretary-answer)
                          &optional (stream *standard-output*) )
  (format stream "~&~S is a ~A.~%"
          answer (type-of answer) )
  (format stream "~&  The answer is: ~S~%"
          (if (slot-boundp answer 'datum)
              (secretary-answer answer)
              "(unbound slot)"))
  (values))


;;;;  Type-checking methods for the accessors

(defmethod  secretary-answer ((x t))
  (error "SECRETARY-ANSWER: ~S is not a secretary answer." x ))


;;;;  Type predicate

(declaim (inline secretary-answer-p))
(defun secretary-answer-p (thing)
  (and (typep thing 'secretary-answer) T))



;;;;;;  **********    S E C R E T A R Y   A G E N T S    ************
;;;
;;;  This portion of the file defines the class SECRETARY-AGENT.
;;;  Secretary agents are agents who know how to answer secretary-information
;;;  questions. To that end, they keep 'secretary lists' (see below) and
;;;  maintain them by handling hypothesis registration requests.
;;;
;;;  Note that SECRETARY-AGENT is a mix-in class.  In other words, secretary
;;;  agents are not an independent type of agents.  Rather, they are functional
;;;  parts of other agents.  INSTANCE-AGENTs and CONCEPT-AGENTs (see AMBR/
;;;  AMBR_AG.LSP) inherit from SECRETARY-AGENTs.

;;;;;;   Class definition
;;;;;;   Type predicates
;;;;;;   Printing methods

;; These are defined in AMBR/AMBR_AG.LSP together with all other classes.
;;
;; The relevant classes are:
;;   SECRETARY-AGENT, CORR-AGENT, TEMP-CORR-AGENT, DEAD-CORR-AGENT ;
;;   HYPOTH-AGENT, EMBRYO-HYPOTH-AGENT, MATURE-HYPOTH-AGENT, WINNER-HYPOTH-AGENT



;;;;;;  ************    S E C R E T A R Y   L I S T S    ************
;;;
;;;  This portion of the file defines the notion of a secretary list and
;;;  provides functions for handling such lists.
;;;
;;;  There are two kinds of secretary lists -- HYPOTHESIS-LIST and
;;;  CORRESPONDENCE-LIST.  The former stores information about the hypothesis
;;;  agents that have been constructed recently.  The latter stores information
;;;  about non-hypothetical correspondences (temporary or permanent).
;;;
;;;  The hypothesis list is kept in a G-slot labeled :HYPOTH ; the corresp.
;;;  list -- in a G-slot labeled :CORR.  Stated differently, there are links
;;;  from the secretary agent to its associated hypothesis and correspondence
;;;  agents (see *ELEMENT->HYPOTH-WEIGHT* in AMBR/DEFS.LSP).
;;;  Links labeled :HYPOTH are temporary while those labeled :CORR -- permanent.
;;;
;;;  Both kinds of secretary lists have similar structure -- they are lists
;;;  of connectionist references (conrefs). If we ignore the connectionist
;;;  aspect for the moment, these lists consist of extended symbolic references
;;;  (or 'symrefs' -- see DUAL/ARCHIT/SYMREF.LSP).
;;;  The reference field of each symref is a correspondence agent.  The slot
;;;  field of each symref denotes the slot that keeps the 'other element' of
;;;  the correspondence (as opposed to 'this element' which is the secretary
;;;  agent itself).
;;;  For example, the correspondence  WATER<-->MILK  will be registered as
;;;   (make-symref #$water<-->milk :slot2)  at the secretary of WATER and as
;;;   (make-symref #$water<-->milk :slot1)  at the secretary of MILK.
;;;
;;;  The only exception to this rule is for 'trivial correspondences' in which
;;;  the two elements coincide. Such correspondences are kept on the secretary
;;;  list as simple symrefs (i.e. 'bare' agents without slot label at all).
;;;  For example, the trivial correspondence  WATER<-->WATER  will be registered
;;;  at the secretary of WATER as  (make-symref #$water<-->water) .

;; The generic function AGENT-HYPOTHESES retrieves the hypothesis list.
;; The generic function AGENT-CORRESPONDENCES retrieves the correspondence list.
;; The function OTHER-ELEMENT retrieves the 'other elt' of one such record.

(defmethod agent-hypotheses ((agent secretary-agent)
                             &optional (symref-only-p T) )
  (declare (values list))            ; of extended symbolic references
  (if symref-only-p
      (get-filler-refs! agent :hypoth)
      (get-filler       agent :hypoth) ))

(defmethod agent-correspondences ((agent secretary-agent)
                                  &optional (symref-only-p T) )
  (declare (values list))                ; of extended symbolic references
  (if symref-only-p
      (get-filler-refs! agent :corr)     ; see DUAL/ARCHIT/LINKS.LSP
      (get-filler       agent :corr) ))

(defmethod agent-hypotheses ((x t) &optional (symref-only-p T))
  (declare (ignore symref-only-p))
  (error "AGENT-HYPOTHESES: ~S is not a secretary agent." x ))

(defmethod agent-correspondences ((x t) &optional (symref-only-p T))
  (declare (ignore symref-only-p))
  (error "AGENT-CORRESPONDENCES: ~S is not a secretary agent." x ))


(defun other-element (registered-correspondence)
  "Retrieves the 'other' element of a correspondence registered at a secretary."
  (cond ((extended-symref-p registered-correspondence)
            (corresp-elt (symref-agent registered-correspondence)
                         (symref-slot registered-correspondence)) )
        ((simple-symref-p registered-correspondence)    ; trivial corresp ?
            #+:DUAL-DEBUG (assert (trivial-corresp-p registered-correspondence))
            (corresp-elt (symref-agent registered-correspondence)
                         *driver-elt-label*) )   ; same as *RECIPIENT-ELT-...
        (t  (error "OTHER-ELEMENT: ~S is not a symbolic reference."
                   registered-correspondence)) ))

(defun parallel-secretary-symrefs-p (symref1 symref2)
  "Do these registered-correspondences involve the same 'other' S-label?"
  (declare (values boolean))
  (or (simple-symref-p symref1)      ; correspondence 1 is trivial  or
      (simple-symref-p symref2)      ; correspondence 2 is trivial  or
      (eq (symref-slot symref1)      ; both are non-trivial and
          (symref-slot symref2)) ))  ; have the same S-labels


;;;;;  Writers for the hypothesis list
;;
;; The weight of each :HYPOTH link is *ELEMENT->HYPOTH-WEIGHT* (AMBR/DEFS.LSP).
;; Each new hypothesis is appended to the end of the list. This :fifo order is
;;  is used by functions like FIND-ANY-HYPOTHESIS that take the first hypoth
;;  from the list that meets some criterion.  Thus they take the oldest hypoth,
;;  which usually is the best (i.e. mature vs. embryo).
;;
;; Trivial correspondences receive special treatment. They are stored as
;;  simple symrefs (see OTHER-ELEMENT). The weight is doubled because the
;;  secretary-agent occurs twice in the hypothesis.
;;
;; Note that these are 'low-level' functions.  Use SEND-HR-REQUEST from outside
;; the secretary.
;; See also STRENGTHEN-WINNER-LINK and WEAKEN-LOSER-LINK in AMBR/PROMOTN.LSP.

(defun register-hypothesis (secretary hypothesis slot)
  "Register HYPOTHESIS into SECRETARY's list."
  (declare (type secretary-agent secretary)
           (type corr-agent hypothesis)
           (type symbol slot) )
  #+:DUAL-DEBUG
  (when (member hypothesis (agent-hypotheses secretary) :test #'symref-match)
    (cerror "Override the old registration and continue."
            "Attempt to register ~S twice at the secretary of ~S."
            hypothesis secretary ))
  (if (eq slot :trivial)                     ; register a trivial hypothesis ?
      (add-link secretary :hypoth (make-symref hypothesis)       ; without slot
                                  (* 2.0 *element->hypoth-weight*)
                                  :priority :new  :order :fifo)
      (add-link secretary :hypoth (make-symref hypothesis slot)  ; with slot
                                  *element->hypoth-weight*
                                  :priority :new  :order :fifo) ))

(defun unregister-hypothesis (secretary hypothesis)
  "Removes HYPOTHESIS from SECRETARY's list."
  (declare (values boolean))
  (remove-link secretary :hypoth hypothesis) )



;;;;;;  *******  RELEVANT HYPOTHESES, LEADERS, AND WINNERS  *******
;;;
;;; The information kept in secretary lists (and in particular hypothesis lists)
;;; is used by various AMBR mechanisms.  It is also used when registering new
;;; hypotheses (see below).  This section provides a small 'library' of
;;; routines for sifting through these lists.
;;;
;;; Note the following terminology:
;;; RELEVANT HYPOTHESES with respect to some (driver) situation are the hypoth
;;;  agents having the same situation as CORRESP-DRIVER (see AMBR/CORRESP.LSP).
;;;  In fact, the total set of hypotheses in a secretary list is partitioned
;;;  into non-overlapping subsets depending on the respective driver situations.
;;;  (This allows for parallel mapping processes with different drivers.)
;;; CURRENT LEADER (or simply 'leader') in a set of hypothesis agents is the
;;;  non-embryo hypothesis with the highest activation level.
;;; WINNER (or 'final winner') hypothesis is a hypothesis agent which has the
;;;  tag :WINNER in its :TYPE slot.  Winner hypotheses come via the promotion
;;;  mechanism (see AMBR/PROMOTN.LSP).  A hypothesis is promoted if it has
;;;  been a leader many times -- see the rating mechanism in AMBR/RATING.LSP.
;;;  The mechanisms are designed in such a way that each set of relevant hyps
;;;  may contain at most one winner.

;; IDENTIFY-RELEVANT-HYPS takes a secretary list (i.e. a list of symrefs) and
;; a situation-agent and returns the list of 'relevant hypotheses' with respect
;; to this situation.  The output is a freshly consed list of symrefs which is
;; a subset of the original list, in the same order.
;; Dead hypotheses (who has fizzled in the interim) are not considered relevant.
;; Compare with IDENTIFY-RATING-HYPS in AMBR/RATING.LSP.
;;
;; (Note: This function is usually called immediately after  AGENT-HYPOTHESES.)
;; (  The partitioning of the total secretary list into 'relevant' subsets may)
;; (  be cached somehow and hence the list need not be traversed each time.   )
;; (  This optimization is not actually implemented here but the consumption  )
;; (  of IDENTIFY-RELEVANT-HYPS is proclaimed (see AMBR/PROCLAIM.LSP)         )
;; (  quite low on this basis.                                                )
;;
(defun identify-relevant-hyps (all-hyps driver-situation)
  (declare (type list all-hyps)    ; list of symrefs
           (values list) )         ; freshly consed list of symrefs
  #+:DUAL-DEBUG
  (unless (agentp driver-situation)
    (error "IDENTIFY-RELEVANT-HYPS: ~S is not a driver situation."
           driver-situation ))
  (let ((result nil))
    (dolist (symref all-hyps)
      (when (and (not (dead-agent-p (symref-agent symref)))
                 (eq driver-situation (corresp-driver (symref-agent symref))))
        (push symref result) ))
    (nreverse result) ))  ; preserve order


;; FIND-ANY-HYPOTHESIS and FIND-ALL-HYPOTHESES are very general (and very
;; time-consuming) tools for finding a hypothesis on the basis of its two
;; elements.  They answer the question:
;;   "Are there any hypotheses relating AGENT-X to AGENT-Y and who are they?"
;;
;; Using these functions, any agent could find the hypothesis if it knows the
;; names of the following three agents:
;;   + SECRETARY  -- the element whose secretary list will be checked
;;   + OTHER-ELT  -- the 'other' element (see OTHER-ELTEMENT above)
;;   + DRIVER-SIT -- the driver situation of the hypothesis.
;; The actual computations are carried out by the symbolic processor of the
;; caller of FIND-xxx-HYPOTHESIS -- the so-called HOST.  Note that the host may
;; be (and usually is) different from the SECRETARY.
;; FIND-HYPOTHESES returns S-VALUES and hence must be used with SUSPENDED-
;; VALUES-BIND (see DUAL/ARCHIT/MAILBOX.LSP for details on S-VALUES).
;;
;; FIND-ANY-HYPOTHESIS returns either NIL or a hypothesis-agent.
;; FIND-ALL-HYPOTHESES returns the list of all matching hyps from SECRETARY's
;;   list.  It is more time-consuming than FIND-ANY-HYPOTHESIS because it
;;   always goes through the whole list while FIND-ANY-HYPOTHESIS returns as
;;   soon as it finds the first hypothesis that match. No effort is made to
;;   separate embryo from mature hypotheses, etc.

(eval-when (compile load eval)
  (proclaim-consumption 'recurse-until-found :explicit-S-PROGN) )

(defun  find-any-hypothesis (host secretary other-elt driver-sit)
  "Is there any hypothesis relating SECRETARY to OTHER-ELT? If yes, give first."
  (declare (type AMBR-agent host)
           (type secretary-agent secretary other-elt)
           (type instance-agent driver-sit)          ; see AMBR/SITUATN.TXT
           (values mailbox) )   ; (s-values hypothesis-agent-or-NIL)
  (s-progn host
    (let* ((all-hyps [agent-hypotheses secretary])   ; remotely read secretary
           (relevant-hyps [identify-relevant-hyps all-hyps driver-sit])
           (found nil) )
      (labels ((recurse-until-found (symref-list)
                 (cond ((null symref-list) nil)                  ; not found
                       ((eq other-elt [other-element (first symref-list)])
                          (setq found (first symref-list)))      ; export value
                       (t [recurse-until-found (rest symref-list)]) )))  ; loop
        [recurse-until-found relevant-hyps] )
      (if found
          (s-values (symref-agent found))   ; return hypothesis-agent
          (s-values NIL)) )))

(defun  find-all-hypotheses (host secretary other-elt driver-sit)
  "The list of all hypotheses relating SECRETARY to OTHER-ELT."
  (declare (type AMBR-agent host)
           (type secretary-agent secretary other-elt)
           (type instance-agent driver-sit)          ; see AMBR/SITUATN.TXT
           (values mailbox) )   ; (s-values list-of-hypothesis-agents)
  (s-progn host
    (let* ((all-hyps [agent-hypotheses secretary])   ; remotely read secretary
           (relevant-hyps [identify-relevant-hyps all-hyps driver-sit])
           (result nil) )
      (dolist (symref relevant-hyps)
        (when (eq other-elt [other-element symref])
          (push (symref-agent symref) result)))
      (s-values (nreverse result)) )))


;; HYPOTH-SET-WINNER takes a secretary list (i.e. a list of symrefs) and
;; looks for agents having the tag :WINNER in their TYPE slot.  If exactly
;; one such hypothesis is found, its enclosing symref is returned.  If none is
;; found, NIL is returned.  If two or more a found, HYPOTH-SET-WINNER returns
;; the leftmost after signalling a continuable error.
;; The input list is usually produced by IDENTIFY-RELEVANT-HYPS.  The consumptn
;; of HYPOTH-SET-WINNER is proclaimed low for similar reasons (caching).
;;
(defun hypoth-set-winner (relevant-hypotheses)
  "Is there a winner-hypotheses among this set of hypotheses?"
  (let ((winners (remove-if #'(lambda (symref)
                               (not (agent-type (symref-agent symref) :winner)))
                            relevant-hypotheses)))
    (case (length winners)
      (0  nil)
      (1  (first winners))
      (t  (cerror "Use first winner and continue."
                  "Too many winners in a hypothesis set: ~S"
                  winners)
          (first winners)) )))


;; HYPOTH-SET-LEADER takes a secretary list (i.e. a list of symrefs) and
;; returns the symref enclosing the most active non-embryo hypothesis, if any.
;; Usually, HYPOTH-SET-LEADER is called when HYPOTH-SET-WINNER has returned NIL.
;;
;; The consumption of HYPOTH-SET-LEADER is proclaimed low because it is assumed
;; that the connectionist aspect can quickly locate the most active agent.
;;
(defun hypoth-set-leader (relevant-hypotheses)
  "The most active non-embryo hypothesis."
  (let ((leader nil)
        (max-act -9999.999))
    (dolist (symref relevant-hypotheses)
      (let ((hyp (symref-agent symref)))
        (when (and (agent-type hyp nil :embryo)         ; non-embryo ?
                   (> (agent-activation hyp) max-act))  ; most active ?
          (setq leader symref)
          (setq max-act (agent-activation hyp)) )))
    leader ))    ; symref or NIL


;; DRIVER-ELT-MAPPING takes a driver element and returns two values -- its
;; corresponding recipient element and the hypothesis involving this element.
;; This function is the culmination of all mapping mechanisms (secretaties,
;; constraint satisfaction, rating, promotion, etc.)
;;
;; The 'driver element' is an instance-agent that belongs to a driver situation.
;; Its 'corresponding recipient element' is the OTHER-ELEMENT (see above) of
;; the most promising hypothesis registered at the secretary of the driver elt.
;; (Recall that each instance-agent has a built-in secretary-agent.)
;;
;; This function is not used in this file.  It is defined here to make it
;; accessible to other files -- AMBR/RATING, =/SKOLEM.LSP, etc.
;;
;; DRIVER-ELT-MAPPING works according to the following algorithm:
;;   1. Retrieve the situation to which DRIVER-ELT is affiliated.
;;      (See the function AGENT-SITUATION defined in AMBR/KREPRES.LSP.)
;;      Let DRIVER-SIT is the situation-agent retrieved in this way.
;;   2. Retrieve the secretary list of DRIVER-ELT (see AGENT-HYPOTHESES above).
;;   3. Identify the set of relevant hypotheses with respect to DRIVER-SIT.
;;   4. Identify the most promising hypothesis among the relevant set:
;;    4a. If the set contains a winner hyp, it is the 'most promising' one.
;;    4b. If :WINNER-ONLY-P is NIL and the the set contains at least one mature
;;        hyp, the 'most promising' is the current leader (see HYPOTH-LEADER
;;        above).  The default value of :WINNER-ONLY-P is NIL.
;;    4c. Otherwise there isn't any promising hypotheses and DRIVER-ELT-MAPPING
;;        returns (values NIL NIL).  (Embyo hypotheses do not count; mature
;;        hyps count only when :WINNER-ONLY-P is NIL.)
;;   5. Return two values -- the OTHER-ELEMENT of the most promising hypoth and
;;        the most promising hypothesis itself.

(defun driver-elt-mapping  (driver-elt &key winner-only-p)
  "What element, if any, corresponds to DRIVER-ELT ?"
  (declare (type instance-agent driver-elt)
           (values (or null AMBR-agent) (or null corr-agent)) )
  #+:DUAL-DEBUG (assert (not (null (agent-situation driver-elt))))
  (let* ((driver-sit (agent-situation driver-elt))                 ; step 1.
         #+:DUAL-DEBUG (foo (when (null driver-sit)
                              (error "DRIVER-ELT-MAPPING: ~S is not affiliated."
                                driver-elt)))
         (all-hyps   (agent-hypotheses driver-elt))                ; step 2.
         (relev-hyps (identify-relevant-hyps all-hyps driver-sit)) ; step 3.
         (most-promising-hyp (most-promising-hyp relev-hyps        ; step 4abc.
                                                 winner-only-p)) )
    (if (null most-promising-hyp)
        (values nil nil)                                           ; step 4c.
        (values (other-element most-promising-hyp)                 ; step 5.
                (symref-agent  most-promising-hyp)) )))

(defun  most-promising-hyp  (hypoth-set winner-only-p)
  (declare (values (or null AMBR-agent)))
  (let ((winner-hyp (hypoth-set-winner hypoth-set)))               ; step 4a.
    (cond (winner-hyp     winner-hyp)                              ; step 4a.
          (winner-only-p  nil)                                     ; step 4bc.
          (t              (hypoth-set-leader hypoth-set)) )))      ; step 4bc.



;;;;;;  *******  H Y P O T H E S I S   R E G I S T R A T I O N  *******
;;;
;;; When a secretary receives a hypothesis-registration request, it compares
;;; the candidate-hypothesis with the old hypotheses registered so far and:
;;;   A. Decides what answer to send back to the candidate (if any).
;;;   B. Decides whether to register the new candidate.
;;;

;; Hypothesis registration is triggered by a receipt of a HR-request and
;; involves a complex chain of checks. It begins as follows:
;;  1. Retrieve the CANDIDATE hypoth from the hypothesis-registration request.
;;  2. Check whether CANDIDATE is visible. Abort all if invisible (i.e. dead).
;;  3. Retrieve the CORRESP-DRIVER situation from CANDIDATE (see AMBR/CORRESP).
;;  4. Retrieve the AGENT-HYPOTHESES list from the secretary.
;;  5. Identify the list of relevant hyp's via IDENTIFY-RELEVANT-HYPS.
;;  6. Check whether this list contains a 'winner hypothesis' (see AMBR/
;;     PROMOTN.LSP) by calling the function HYPOTH-SET-WINNER. If there is
;;     such winner hypoth, send an answer 'winner' to CANDIDATE and stop.
;;     (Note that the new candidate is not registered in this case.)
;;  5. Otherwise, proceed with CHECK-FOR-DUPLICATES (see below).

(defmethod handle-symbol ((secretary secretary-agent)
                          (HR-request hypothesis-registration-request) )
  (declare (values S-PROGN-return-type))
  (s-progn secretary
    (let ((candidate [HR-hypothesis HR-request]))                  ; step 1.
     (when [agent-visible-p candidate]                             ; step 2.
        (let ((driver-sit [corresp-driver candidate])              ; step 3.
              (all-hyps   (agent-hypotheses secretary)) )          ; step 4.
          (s-eval (* 2.0 *default-AMBR-INTERNAL-consumption*) nil) ; for 4+6.
          (let* ((relev-hyps [identify-relevant-hyps
                                       all-hyps  driver-sit])      ; step 5.
                 (winner (hypoth-set-winner relev-hyps)) )         ; step 6.
            (if winner
                [send-secretary-answer candidate 
                                       (symref-agent winner)]      ; step 6.
                [check-for-duplicates secretary                    ; step 7.
                                      HR-request
                                      relev-hyps] )))))))
        ;; The consumption for steps 4, 5, and 6. is INTERNAL-consum because:
        ;; AGENT-HYPOTHESES accesses a local slot.
        ;; IDENTIFY-RELEVANT-HYPS may have cached in its local memory the
        ;;   driver-situation of each hypoth at registration time (cf.step 3).
        ;; HYPOTH-SET-WINNER may have cached winner hyps at 'promotion time'.


;; Checking for duplicates involves collecting a list and then acting accordgly:
;;  1. Collect a list of 'duplicates' by calling COLLECT-DUPLICATES (see below).
;;  1a. Remove any hypotheses that have died in the interim.
;;  2. Check whether CANDIDATE is still visible. Abandon everything if not.
;;  3. If DUPLICATES is empty then send an answer "list" to CANDIDATE and
;;     _then_ register it.  (The list must be freshly consed as it is used
;;     destructively by CANDIDATE.)
;;  4. Otherwise, _first_ register it and then proceed with ANALYZE-DUPLICATES.
;;
;; (Remark: The special ordering of hypothesis registration at steps 3. and 4.)
;; (  above is designed to minimize the probability of 'ambiguous secretary   )
;; (  answers'.  Such ambiguity can occur when two identical hypotheses send  )
;; (  HR-requests to the respective two secretaries at approximately the same )
;; (  time. With such ties it may happen that a hypothesis receives an answer )
;; (  'establish' from one secretary and 'resign' from the other.  The embryo )
;; (  hypothesis agents are equipped with procedural knowledge for breaking   )
;; (  the ties on the basis of the activation levels of the two pretenders.   )
;; (  See ANALYZE-AMBIGUOUS-ANSWERS in AMBR/HYPOTH.LSP.                       )
;; (Remark: Note that the CANDIDATE is registered in both cases (3. and 4.).  )
;; (  This is to ensure that all hypotheses that eventually establish are     )
;; (  properly registered.  The price to pay is that it is necessary to deal  )
;; (  with whole lists of duplicates instead of individual 'curr. favorites'. )

(defun check-for-duplicates (secretary HR-request relev-hypotheses)
  (declare (type secretary-agent secretary)
           (type hypothesis-registration-request HR-request)
           (type list relev-hypotheses) )   ; 'secretary' list of symrefs
 (s-progn secretary
  (let* ((candidate  (HR-hypothesis HR-request))
         (slot       [HR-slot HR-request])
         (duplicates nil) )   ; to be set below
    (s-eval (consumption-of-COLLECT-DUPLICATES relev-hypotheses) nil)  ; step 1.
    (setq relev-hypotheses (delete-if #'dead-agent-p                   ; step 1a
                                      relev-hypotheses
                                      :key #'symref-agent))
    (setq duplicates (collect-duplicates candidate slot
                                         relev-hypotheses))            ; step 1.
    (cond ((not (agent-visible-p candidate)) nil)                      ; step 2.
          ((endp duplicates)
                 [send-secretary-answer candidate
                       (mapcar #'symref-agent relev-hypotheses)]       ; step 3.
                       ; ^^^^^ freshly consed list, see ESTABLISH-HYPOTHESIS
                 [register-hypothesis secretary candidate slot])       ; step 3.
          (t     [register-hypothesis secretary candidate slot]        ; step 4.
                 [analyze-duplicates secretary duplicates candidate])  ; step 4.
    ))))


;; 'Duplicate' correspondences are those that involve the same elements in
;; the same order. One possible test for this is EQUIVALENT-CORRESP-P defined
;; in AMBR/CORRESP.LSP. This test, however, is expensive because it involves
;; reading both elements of all correspondences each time. Given that
;; CORRESP-ELT is relatively expensive operation (see AMBR/PROCLAIM.LSP),
;; this would make the whole hypothesis registration too expensive. To avoid
;; this, secretaries in AMBR (assisted by corr-agents themselves) use two
;; optimization techniques:
;;  + They track only the OTHER-ELEMENT of each correspondence by keeping its
;;     slot label. ('This element' is always the same -- the secretary itself.)
;;  + The elements are read from the remote corr-agent only once and are then
;;     cached in the local buffer of the secretary-agent.
;; Hence, the total consumption of COLLECT-DUPLICATES is given by the formula:
;;
;; Correspondences such as GLASS<==>CUP and CUP<==>GLASS are not duplicates.
;; (The check for PARRALLEL-SECRETARY-SYMREFS-P guards against that.)

(defun consumption-of-COLLECT-DUPLICATES (relev-hypotheses)
  (+ (get-consumption 'other-element t)         ; remotely read new-corresp (A)
     (* (length relev-hypotheses)               ; for each old hypothesis   (B)
        *default-AMBR-INTERNAL-consumption*)) ) ; use cached data           (C)

(defun collect-duplicates (new-corresp new-slot relev-hypotheses)
  (declare (type corr-agent new-corresp)
           (type symbol new-slot)          ; driver/recipient label or :TRIVIAL
           (type list relev-hypotheses)    ; 'secretary' list of symrefs
           (values list) )                 ; freshly consed list of agents
  (let* ((S-label (if (eq new-slot :trivial) *driver-elt-label* new-slot))
         (new-other-elt (corresp-elt new-corresp           ; consumption (A)
                                     S-label))
         (result nil) )
    (dolist (old-symref relev-hypotheses)                  ; consumption (B)
      (when (parallel-secretary-symrefs-p old-symref
                                          (make-symref new-corresp S-label))
        (let ((old-other-elt (other-element old-symref)))  ; consumption (C)
              ;; Not really cached in the implementn, only consumption adjusted.
          (when (eq new-other-elt old-other-elt)
            (push (symref-agent old-symref) result) ))))
    (nreverse result) ))



;; The goal of ANALYZE-DUPLICATES is to determine what answer to send to
;; NEW-HYPOTHESIS. At this point it is clear that the answer will be negative:
;; the new candidate will be advised to resign in favor of some older hypoth.
;; This older favorite is chosen according to the following algorithm:
;;  1. Terminate everything if CANDIDATE is invisible (i.e. dead).
;;  2. Discard all hypotheses from the DUPLICATE list that have fizzled in
;;     the interim.
;;  3. If all have fizzled, return some of them (the first one for concreteness)
;;      Note that in this case the secretary answer is a dead hypothesis.
;;  4. If the DUPLICATE list contains at least one mature hypothesis then
;;     discard any embryo hypotheses and consider only the mature ones.
;;  5. Pick up the most active member of this (possibly reduced) list.
;;  6. Send it to NEW-HYPOTHESIS as the secretary answer.

(eval-when (compile load eval)
  (proclaim-consumption 'most-active-duplicate :explicit-S-PROGN) )

(defun analyze-duplicates (secretary duplicates candidate)
  "Send a secretary answer to CANDIDATE informing it about a duplicate hypoth."
  (declare (type secretary-agent secretary)
           (type corr-agent candidate)
           (type cons duplicates) )   ; freshly consed list of corr-agents
  (when (agent-visible-p candidate)                                  ; step 1.
    (let ((dup1 (first duplicates))                 ; just in case for step 3.
          (filtered (filter-duplicates duplicates candidate)) )      ; step 2+4.
      (s-progn secretary
        (case (length filtered)
          (0  [send-secretary-answer candidate dup1])                ; step 3.
          (1  [send-secretary-answer candidate (first filtered)])    ; step 5+6.
          (t  (suspended-value-bind (most-active-duplicate)          ; step 5.
                                    [most-active-duplicate secretary filtered]
                [send-secretary-answer candidate                     ; step 6.
                                       most-active-duplicate])) )))))

(defun filter-duplicates (original-duplicates candidate)
  ;; Implements steps 2 and 4 of the algorithm for ANALYZE-DUPLICATES above.
  (declare (type cons original-duplicates)
           (type corr-agent candidate)
           (values list) )
  (let ((duplicates (delete-if #'dead-agent-p original-duplicates)))  ; step 2.
    #+:DUAL-DEBUG                                                     ; debug
    (unless (every #'(lambda (dup) (equivalent-corresp-p dup candidate))
                   duplicates)
      (cerror "Consider only the equivalent ones and continue."
              "AMBR-CORE::FILTER-DUPLICATES: ~S are not all equivalent to ~S."
              duplicates candidate)
      (setq duplicates                                                ; debug
            (delete-if #'(lambda (d) (not (equivalent-corresp-p d candidate)))
                       duplicates)) )
    (if (some #'(lambda (ag) (agent-type ag :mature))                 ; step 4.
              duplicates)
        (delete-if #'(lambda (ag) (agent-type ag :embryo))            ; step 4.
                   duplicates)
        duplicates) ))                                                ; step 4.

(defun most-active-duplicate (host duplicates)
  (declare (values mailbox))     ; (s-values corr-agent)
  (let ((result (first duplicates))        ; possibly dead
        (curr-max -1000.0) )
    (s-progn host
       (dolist (next (delete-if #'dead-agent-p duplicates))
         (when (> [agent-activation next] curr-max)
           (setq result next)
           (setq curr-max (agent-activation next)) ))
       (s-values result) )))

;;;
;;;;;;;; End of the hypothesis-registration protocol.



;;;;;  When a secretary dies, all associated hypotheses die with it.
;;
;; Compare with temporary agents (DUAL/ARCHIT/TEMP_AG.LSP).
;; Recall that :HYPOTH is a temporary link.

(defmethod remove-from-WM :before ((agent secretary-agent)
                                   &optional (reset-p T) )
  (when reset-p
    (let ((hypoth-list (agent-hypotheses agent)))
      (remove-slot agent :HYPOTH  :notify-p nil)    ; avoid piecemeal removal
      (dolist (symref hypoth-list)
        (fizzle-agent (symref-agent symref))) )
    (dolist (symb-struc (agent-input-zone agent))
      (when (typep symb-struc 'hypothesis-registration-request)
        (fizzle-agent (HR-hypothesis symb-struc)))) ))

;; TO DO: Decide what to do with the temporary corr-agents on the (permanent)
;;        :CORR slot.  In general, it is a bad idea to have a permanent link
;;        from a permanent to a temporary agent, esp. when leaving the WM.

;;;;;;;  End of file AMBR/SECRETAR.LSP
