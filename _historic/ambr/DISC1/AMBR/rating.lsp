;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/rating.lsp
;;; VERSION:    2.2.2  ; see AMBR/VERSION.LSP
;;; PURPOSE:    Rating hypotheses over time and deciding who is the winner.
;;; DEPENDS-ON: DUAL; ambr/secretar.lsp, ambr/hypoth.lsp, ambr/skolem.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    03-04-98 [2.2.1]
;;; UPDATED:    10-05-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...
;;;;;;;;
;;; TO DO:      Consider moving the bulky comments to a separate text file.


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;       R A T I N G   M E C H A N I S M       ;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "AMBR-CORE")

;;;; This file implements the so-called RATING MECHANISM -- a mechanism for
;;;; selecting 'winner hypotheses', eliminating 'loser hypotheses', and
;;;; triggering skolemization of 'general hypotheses'.
;;;; See AMBR/PROMOTN.LSP, =/FIZZLE.LSP, and =/SKOLEM.LSP, respectively.
;;;;
;;;; The main idea of the rating mechanism is the following:
;;;; Some of the secretaries are 'authorized' to decide which of the many
;;;; competing hypotheses on their secretary list is likely to represent the
;;;; most promising correspondence.  To identify the most promising candidate,
;;;; authorized secretaries inspect hypotheses' activation levels at regular
;;;; intervals (see *RATING-TIME-PERIOD* in AMBR/DEFS.LSP).  Each such poll
;;;; is called a 'rating survey'.  The outcome of each survey is summarized in
;;;; a RATING TABLE which is stored in the local buffer of the secretary agent.
;;;; The next survey builds upon the ratings of the previous one in a manner
;;;; similar to the competitive learning algorithms in neural networks -- the
;;;; rating of the current LEADER is boosted at the expense of the remaining
;;;; hypotheses.
;;;;
;;;; After a sequence of surveys, the individual rating of one particular
;;;; (hopefully 'most promising') hypothesis gets high while the ratings of
;;;; its rivals go down.  When an individual rating gets high enough (see
;;;; parameter *WINNER-RATING* in AMBR/DEFS.LSP) the respective hypothesis
;;;; is considered for promotion.  Conversely, when a rating gets too low
;;;; (see *LETHAL-RATING*) the respective hypothesis is considered for
;;;; elimination.
;;;;
;;;; Note the following terminology: a [current] LEADER is the hypothesis agent
;;;; with highest activation level during a particular survey; a [final] WINNER
;;;; is a hypothesis agent that has been a leader many times in succession and
;;;; has been 'promoted' via the 'promotion mechanism' (see AMBR/PROMOTN.LSP).
;;;; (Note that even a 'final' winner may not be winner forever.)
;;;;
;;;; The rating mechanism is based on local information only -- the activation
;;;; levels of the hypotheses accessible from a single secretary.  Therefore
;;;; it can sometimes favor hypotheses that are inconsistent with the global
;;;; mapping as computed by the constraint satisfaction network (AMBR/CSNET.LSP)
;;;; To avoid such premature promotions, when a hypothesis' rating reaches the
;;;; *winner-rating* level this does not automatically lead to promotion.
;;;; Rather, the hypothesis is only _considered_ for promotion. This considera-
;;;; tion takes into account some limited global information -- it consults the
;;;; secretary of the relevant situation agent.  This is done by the function
;;;; CHECK-SITUATION-WINNER below. (See file AMBR/SITUATN.TXT for documentation
;;;; of situation agents and affiliation of instances to situations.)
;;;;
;;;; A second purpose of the rating mechanism is to trigger the skolemization
;;;; procedure for general hypotheses provided they are strong enough.
;;;; (See AMBR/SKOLEM.TXT and =/SKOLEM.LSP for details on skolemization.)
;;;;
;;;; The third purpose of the rating mechanism is to detect and eliminate
;;;; obviously bad hypotheses.  Just as the rating of the leader goes up,
;;;; the ratings of the losers go down.  When the rating of some hypothesis
;;;; falls below a threshold, the hypothesis is considered for elimination.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: rating-table, make-rating-table, rating-table-p,
;;          rating-table-secretary,   ; RATING-TABLE-TABLE --> RATING-TABLE
;;          secretary-rating-table,
;;          individual-rating, make-individual-rating,
;;          IR-hypothesis, IR-rating, IR-other ;
;;
;;          authorized-p, initiate-rating-survey,
;;          no-longer-authorized, finish-rating,
;;          rating-survey, update-rating-table ;
;;
;;          check-for-lethal-ratings, eliminate-loser-hypothesis,
;;          check-for-winner-ratings, check-situation-winner,
;;          check-for-Skolem-ratings, ballotage-rating
;;
;; The functions in this file depend on the following parameters from DEFS.LSP:
;;  *rating-time-period*, *positive-rating-factor*, *negative-rating-factor*,
;;  *initial-rating*, *winner-rating*, *lethal-rating*, *ballotage-rating*,
;;  *skolem-rating*, *critical-winner-level*, *critical-loser-level*, and
;;  *contradict-hypoth-weight*.
;;
;; CHECK-FOR-WINNER-RATINGS also refers to the function SEND-PROMOTION-INCENTIVE
;; defined in AMBR/PROMOTN.LSP.


;; ...

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric individual-rating  (hypothesis rating-holder)
  (:documentation "Retrieve the rating for HYPOTHESIS." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;  ********   R A T I N G   T A B L E S   ********
;;;
;;;  The purpose of rating tables is to keep track of individual hypothesis'
;;;  ratings.  Individual ratings are stored in an a-list and the list itself
;;;  is wrapped into a symbolic structure of class RATING-TABLE.
;;;  This wrapping (as opposed to bare a-list) is useful for two reasons:
;;;   + rating tables are conveniently stored in the buffer of the corresponding
;;;       secretary agent and may easily be distinguished from other structures
;;;       there (e.g. markers) ;
;;;   + the generic function HANDLE-SYMBOL can specialize on RATING-TABLEs.
;;;
;;;  The SECRETARY field of rating tables is not needed for the computation.
;;;  It is provided for identification and error-signaling purposes only.
;;;

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass  rating-table  (secretary-symbolic-structure)
    ((secretary   :reader     rating-table-secretary
                  :type       secretary-agent
                  :initarg    :secretary
                  :initform   (required-argument) )
     (table       :accessor   rating-table
                  :type       list            ; a-list of 'individual ratings'
                  :initform   nil   )
    )
   (:documentation
     "Table maintained by authorized secretaries for selecting winner hyps." ))
) ; eval-when


;;;;  Constructor

(defun  make-rating-table (secretary)
  (declare (type secretary-agent secretary)
           (values rating-table) )
  (make-instance 'rating-table
                 :secretary secretary ))


;;;;;;  Printing methods

(defmethod  print-object ((rating-table rating-table) stream)
  (if (and (slot-boundp rating-table 'secretary)
           (agentp (rating-table-secretary rating-table)) )
      (format stream "#<RT ~A>"
                     (agent-name (rating-table-secretary rating-table)))
      (format stream "#<malformed rating-table>") ))

(defmethod DUAL-describe ((rating-table rating-table)
                          &optional (stream *standard-output*) )
  (format stream "~&~S is a ~A.~%"
          rating-table (type-of rating-table) )
  (format stream "~&  Its secretary is: ~S~%"
          (if (slot-boundp rating-table 'secretary)
              (rating-table-secretary rating-table)
              "(unbound, which is anomalous)"))
  (format stream "~&  Its table is: ~S~%"
          (if (slot-boundp rating-table 'table)
              (rating-table rating-table)
              "(unbound)"))
  (values))


;;;;  Type predicate

(declaim (inline rating-table-p))
(defun rating-table-p (thing)
  (and (typep thing 'rating-table) T))


;;;;  Type-checking methods for the accessors

(defmethod  rating-table-secretary ((x t))
  (error "RATING-TABLE-SECRETARY: ~S is not a rating table." x ))

(defmethod  rating-table ((x t))
  (error "RATING-TABLE: ~S is not a rating table." x ))

(defmethod  (setf rating-table) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF RATING-TABLE): ~S is not a rating table." x ))


;;;;;;;  Rating tables are stored in the buffer of secretary agents
;;
;;  Note that RATING-TABLE-SECRETARY takes a rtg table and returns a secretary
;;  while SECRETARY-RATING-TABLE takes a secretary agent and returns a table.

(defun  secretary-rating-table  (secretary)
  "Retrieve the rating table stored in a secretary's buffer, if any."
  (declare (type secretary-agent secretary)
           (values (or null rating-table)) )
  (find-if #'rating-table-p
           (agent-buffer secretary)) )

(defun  find-or-make-rating-table (secretary)
  "Ensures that SECRETARY's buffer contains a rating table."
  (declare (values rating-table))
  (let ((rating-table (secretary-rating-table secretary)))
    (when (null rating-table)                       ; no table found in buffer
      (setq rating-table                            ; make an empty table
            (make-rating-table secretary))
      (push rating-table (agent-buffer secretary)) )
    rating-table ))



;;;;;;  ********   I N D I V I D U A L   R A T I N G S   ********
;;;
;;; The rating table (if we ignore the SECRETARY field) is a list of individual
;;; ratings.  The INDIVIDUAL-RATINGs (or IRs for short) are data structures
;;; keeping the following fields:
;;;   + the hypothesis being rated     ; (type hypothesis-agent)
;;;   + the rating value               ; (type float)
;;;   + a list of other (optional) pieces of information, e.g. :SKOLEM tags.
;;;
;;; In the current implementation, individual ratings are represented as
;;; three-element lists.

(defun  make-individual-rating (hypothesis rating &optional other-fields)
  "Constructs an entry suitable for storing in a rating table."
  ;; See also FIND-OR-MAKE-INDIVIDUAL-RATING below.
  (declare (type corr-agent hypothesis)
           (type number rating)
           (type list other-fields) )
  (list hypothesis (coerce rating 'float) other-fields) )

(declaim (inline IR-hypothesis IR-rating IR-other))
(defun  IR-hypothesis  (individual-rating)
  "Reader for the HYPOTHESIS field of individual ratings."
  (first individual-rating) )
(defun  IR-rating  (individual-rating)         ; Compare with INDIVIDUAL-RATING
  "Accessor for the RATING field of individual ratings."
  (second individual-rating) )
(defun  IR-other  (individual-rating)
  "Accessor for the OTHER field of individual ratings."
  (third individual-rating) )

;; Make IR-RATING and IR-OTHER usable with SETF
(defsetf IR-rating (individual-rating)
         (new-hypothesis)
  `(setf (second ,individual-rating) ,new-hypothesis) )
(defsetf IR-other (individual-rating)
         (new-hypothesis)
  `(setf (third ,individual-rating) ,new-hypothesis) )


;; Generic access to the most valuable piece of information -- the rating value.
;;
(defmethod  individual-rating ((hyp hypoth-agent) (rating-table rating-table) )
  ;; Unwrap the a-list keeping the actual table and recurse.
  (individual-rating hyp (rating-table rating-table)) )

(defmethod  individual-rating ((hyp hypoth-agent) (secretary secretary-agent) )
  ;; Retrieve the rating table from the secretary's buffer and recurse.
  (let ((rating-table (secretary-rating-table secretary)))
    (if (null rating-table)
        (cerror "Return NIL and continue."
                "~S has no rating table in its buffer and hence cannot rate ~S."
                secretary hyp)
        (individual-rating hyp (rating-table rating-table)) )))

(defmethod  individual-rating ((hyp hypoth-agent) (list-of-IRs list) )
  ;; The real work.  If not found, use *INITIAL-RATING* as default.
  (declare (values float))
  (let ((IR (find hyp list-of-IRs :key #'IR-hypothesis)))
    (if (null IR)
        *initial-rating*     ; see special case 2 in UPDATE-RATING-TABLE below
        (IR-rating IR) )))

(defmethod  individual-rating ((x t) (y t))
  (error "INDIVIDUAL-RATING: ~S is not a hypoth or ~S is not a rating holder."
    x y ))



;;;;;;  ********   A U T H O R I Z E D   S E C R E T A R I E S   *******
;;;
;;;  Not all secretaries are authorized to promote hypotheses.
;;;  To have this authority, an agent must meet the following criteria:
;;;   1. to be an instance (and not concept) agent  and
;;;   2. to belong to a 'driver' coalition -- see T-DRIVER-P in AMBR/GOALINPT.LSP.
;;;
;;;  The second criterion is checked by inspecting the TYPE slot of the
;;;  situation-agent to which the instance-agent is affiliated (see file
;;;  AMBR/SITUATN.TXT for detailed documentation on affiliations).
;;;  The situation agent is retrieved by AGENT-SITUATION (see AMBR/KREPRES.LSP)
;;;  and is then passed through T-DRIVER-P.

(defun authorized-p (secretary)
  "Is this secretary authorized to rate and promote hypotheses?"
  (declare (type secretary-agent secretary)
           (values boolean))
  (if (not (agent-type secretary :instance))
      nil                                   ; concept-agent ==> not authorized
      (let ((situation-agent (agent-situation secretary)))
        (if (null situation-agent)
            nil                                 ; free-standing instance
            (T-driver-p situation-agent) ))))   ; see AMBR/GOALINPT.LSP



;;;;;;  ********   INITIATING THE RATING MECHANISM   ********
;;;
;;;  The rating mechanism works in successive 'rating surveys' separated by
;;;  relatively long periods of inactivity.  Alarm clocks (see DUAL/ARCHIT/
;;;  TIME.LSP) are used to span these periods of inactivity and to measure
;;;  the intervals between the rating surveys.  These intervals are roughly
;;;  equal (possibly with some delay) to *RATING-TIME-PERIOD* from AMBR/DEFS.LSP
;;;
;;;  Each rating survey begins by a receipt of a clock message.  The clock
;;;  message contains a rating-table that keeps the individual ratings
;;;  accumulated so far.  The same rating table is stored in the local buffer
;;;  of the secretary agent.  (The table contained in the clock message, being
;;;  a distinct type of symbolic structure, is used only as a specifier for
;;;  the method for HANDLE-SYMBOL.)
;;;
;;;  Each rating survey prepares the ground for the next by re-setting the
;;;  alarm clock.  The first ring in this chain is established as a side effect
;;;  of the hypothesis registration at the secretary.  The receipt of the first
;;;  HR-request (see AMBR/SECRETAR and =/HYPOTH.LSP) sets the first alarm clock.
;;;  Subsequent HR-requests do not set duplicate clocks because they detect
;;;  the presence of a rating table in the buffer.
;;;  (Consider the possibility that these subsequent requests increment a bit
;;;    the time period of the clock.  In this way, intensive registration of
;;;    new hypotheses would slow down the rating mechanism and thus give more
;;;    chances to the new candidates to become leaders and be promoted. )

(defmethod  handle-symbol :after ((secretary instance-agent)
                                  (HR-request hypothesis-registration-request) )
  (when (and (null (secretary-rating-table secretary))  ; no table in the buffer
             (authorized-p secretary) )
    (initiate-rating-survey secretary) ))


;; INITIATE-RATING-SURVEY sets an alarm clock on the basis of a rating table
;; stored in the local buffer.  If no such table exists, it creates an empty
;; one, stores it in the buffer for future use, and passes it to the clock.
;;
;; The alarm clocks serving the rating mechanism are identified by the
;; symbol 'RATING and are owned by the respective secretary agent.

(defun  initiate-rating-survey (authorized-secretary)
  "Set an alarm clock driving the rating mechanism."
  (declare (type instance-agent authorized-secretary)
           (values alarm-clock) )
  (set-alarm-clock (find-or-make-alarm-clock 'rating authorized-secretary )
                   *rating-time-period*
                   (find-or-make-rating-table authorized-secretary)) )



;;;;;;  ********   R A T I N G   S U R V E Y S   ********
;;;
;;;  Each rating survey begins by a receipt of a clock message.  Not all clock
;;;  messages are necessarily related to the rating mechanism, however,  To
;;;  mark those that are, they contain a symbolic structure of type RATING-TABLE
;;;  in them.  The general method for handling clock messages opens them,
;;;  retrieves the symbolic structure carried by them, and pushes it to the
;;;  input zone (see DUAL/ARCHIT/TIME.LSP).  Thus, we may assume here that
;;;  rating surveys are triggered when HANDLE-SYMBOL encounters a rating table
;;;  (read from the input zone).

(defmethod  handle-symbol ((secretary instance-agent)
                           (rating-table rating-table) )
  (declare (ignore rating-table))    ; the actual table is taken from buffer
  (s-progn secretary
     (if [authorized-p secretary]    ; still authorized?
         [rating-survey secretary]
         [no-longer-authorized secretary] )))


;; It may happen that a situation is a driver situation for some time and
;; then is removed from the goal or input lists (see AMBR/GOALINPT.LSP).
;; In this case, the authorization of respective secretaries will be no longer
;; valid.  When this happens, the function NO-LONGER-AUTHORIZED is invoked.
;; In the current version [2.2.1] it simply discards the rating table from
;; the buffer.  Future versions may alter this behavior.
;; (Compare with the function FINISH-RATING below.)

(defun  no-longer-authorized (secretary)
  "Stops the rating mechanism because SECRETARY is no longer authorized."
  (setf (agent-buffer secretary)
        (remove-if #'rating-table-p (agent-buffer secretary)) )
  nil )


;; RATING-SURVEY does the bulk of the work of the rating mechanism.
;; The entry point to this function is the method for HANDLE-SYMBOL above.
;; Therefore, it assumes that in the beginning the secretary is authorized.
;; (It checks AUTHORIZED-P in the end because things may have changed.)
;;
;; Another assumption is that the list of 'relevant hypotheses' (see step 1.
;; below) does not contain any winner hypotheses.  This assumption is warranted
;; because winner hypotheses are created via the promotion mechanism (see
;; AMBR/PROMOTN.LSP) which in turn is triggered by the rating mechanism
;; discussed here.  The rating mechanism stops when the promotion mechanism
;; begins.  Therefore, the rating mechanism shouldn't encounter winner hyps.
;;
;; Each rating survey can be partitioned into the following five broad steps:
;;   1. Identify the set of hypotheses relevant for rating.
;;    1a. If the set is empty go to step 5a, otherwise proceed with step 2.
;;   2. Update the rating table stored in the secretary's buffer.
;;   3a. Consider the hypoth whose rating has fallen below the lethal threshold.
;;   3b. Consider the 'general hypotheses' (see AMBR/SKOLEM.TXT) whose rating
;;        has exceeded the skolemization threshold.
;;   4. If the leader has gained enough rating, consider it for promotion.
;;   5. Prepare for the next rating survey by resetting the alarm clock ...
;;    5a. ...provided the secretary is still authorized and there was no promtn.
;;    5b. If the secretary is no longer authorzd,proceed with NO-LONGER-AUTHORZ.
;;    5c. If the leader has been promoted to winner, proceed with FINISH-RATING.
;; Most of these steps have substeps and are implemented by separate functions.
;; The function RATING-SURVEY binds them all together.

(defun  rating-survey (secretary)
  "Boost the rating of the leading hypothesis at the expense of its rivals."
  (declare (values S-PROGN-return-type))
  (s-progn secretary
    (let ((relev-hyps  [identify-RATING-hyps secretary]))             ; step 1.
      (if (null relev-hyps)                                           ; step 1a.
          [initiate-rating-survey secretary]                          ; step 5a.
          (let ((new-ratings [update-rating-table secretary           ; step 2.
                                                  relev-hyps]))
            [check-for-lethal-ratings secretary new-ratings]          ; step 3a.
            [check-for-Skolem-ratings secretary new-ratings]          ; step 3b.
            (suspended-value-bind (winner)
                    [check-for-winner-ratings secretary new-ratings]  ; step 4.
              (if winner
                  [finish-rating secretary winner]                    ; step 5c.
                  (if (authorized-p secretary)    ; no consumption
                      [initiate-rating-survey secretary]              ; step 5a.
                      [no-longer-authorized   secretary])))) ))))     ; step 5b.


;; IDENTIFY-RATING-HYPS is similar to IDENTIFY-RELEVANT-HYPS defined in AMBR/
;; SECRETAR.LSP.  It collects the set (represented as a list) of the hypotheses
;; that are relevant to the rating mechanism.  A hypothesis is relevant if it
;; meets the following criteria:
;;   Cr1. its driver situation is the same as the situation of the secretary,
;;   Cr2. it is a mature hypoth (see below), and
;;   Cr3. it is not dead.
;;
;; Checking the driver situation (Cr1) is necessary because one and the same
;; instance agent (and hence its secretary) can play two roles simultaneously.
;; An instance may participate in one mapping as a driver element and in
;; another mapping as a recipient elt.  This happens when there are two driver
;; situations (e.g. one on the goal and the other on the input list).  The
;; secretary of the instance is authorized to rate only the hypotheses in which
;; the instance plays the driver role.  (See AMBR/CORRESP.LSP for details on
;; driver and recipient situations. See also x-DRIVER-P in AMBR/GOALINPT.LSP.)
;; Usually the instance agent plays only one role (either driver or recipient)
;; and thus criterion Cr1 is trivial.  Nevertheless, it must be checked to
;; support the exotic cases.  The consumption of this function, however,
;; is low because some optimized caching strategies may be used -- see the
;; documentation for IDENTIFY-RELEVANT-HYPS in AMBR/SECRETAR.LSP.
;;
;; Checking the :TYPE slot (Cr2) is also necessary in order to weed out
;; embryos (and to signal errors on winners).  More concretely:
;;  Cr2-a. :EMBRYO hypotheses are not considered relevant,
;;  Cr2-b. :MATURE hyps are considered relevant, and
;;  Cr2-c. :WINNER hyps should not be encountered by the rating mechanism --
;;           see the second assumption of RATING-SURVEY above.
;;
;; IDENTIFY-RATING-HYPS works according to the following algorithm:
;;  1. Retrieve the list of all hypotheses by calling AGENT-HYPOTHESES.
;;  2. Retrieve the situation-agent to which the current instance-agent is
;;      affiliated. It is assumed that the situation-agent satisfies T-DRIVER-P.
;;      That assumption is warranted because the instance-agent has just
;;      satisfied AUTHORIZED-P (according to the first assumption of RATING-
;;      SURVEY above).
;;  3. Filter this list to satisfy criteria Cr1, Cr2, and Cr3 above.

(defun identify-rating-hyps (secretary)
  "Identify the hypotheses that are relevant to the rating mechanism."
  (declare (type instance-agent secretary)
           (values list) )                 ; freshly consed list of hypotheses
  (let* ((all-hyps (agent-hypotheses secretary))               ; step 1.
         (my-situation (agent-situation secretary))            ; step 2.
         (result nil) )
    (dolist (symref all-hyps)                                  ; step 3.
      (let ((hyp (symref-agent symref)))
        (cond ((dead-agent-p hyp)       nil)                   ; Cr3.
              ((not (eq my-situation (corresp-driver hyp)))
                                        nil)                   ; Cr1.
              ((agent-type hyp :embryo) nil)                   ; Cr2-a.
              ((agent-type hyp :mature) (push hyp result))     ; Cr2-b.
              ((agent-type hyp :winner)                        ; Cr2-c.
                 (cerror "Treat it as mature hypothesis and continue."
                    "The rating mechanism encountered a winner hypothesis ~S."
                    hyp )
                 (push hyp result) )      ; treat as mature
              (t (cerror "Ignore it and continue."
                    "Hypothesis ~S is neither embryo, nor mature, nor winner."
                    hyp)) )))
    (nreverse result) ))


;; FINISH-RATING is invoked when a hypothesis is approved for promotion.
;; It will soon become a winner-hypothesis and, therefore, need not be rated
;; any longer.  The current version [2.2.1] of FINISH-RATING does nothing.
;; Future versions may alter this behavior.  Compare with NO-LONGER-AUTHORIZED.
;; Note that the rating table is not removed from the buffer.  This is to
;; prevent calling INITIATE-RATING-SURVEY upon the receipt of hypothesis-registr
;; requests.  (See HANDLE-SYMBOL (HR-REQUEST) earlier in this file.)

(defun  finish-rating (secretary winner-hyp)
  "Stops the rating mechanism because a winner-hypothesis has been promoted."
  (declare (ignore secretary winner-hyp))
  nil )    ; may change in future versions



;;;;;;  *******   UPDATING THE RATING TABLE   ********
;;;
;;;  Recall that the rating table is a list of individual ratings.  They
;;;  are modified according to a competitive learning algorithm -- on each
;;;  rating survey the current leader is identified and its rating is
;;;  incremented at the expense of the ratings of all other hypotheses.
;;;  The LEADER is the hypothesis with highest activation level.
;;;
;;;  The magnitude of the change in rating levels depends on the activation
;;;  levels of the respective hypothesis agents.  More precisely, it depends
;;;  on the 'potential difference' between two activation levels.
;;;  Let RTG(H) and ACT(H) denote the rating and activation levels of the
;;;  hypothesis H, respectively.  Consider the following formulas:
;;;   (1)  leader := the hypothesis with highest activation level ACT(H)
;;;   (2)  second := the hypothesis with second-highest ACT(H)
;;;
;;;   (3)  level1 := max { act(leader), *hyp-zero-act* }   ; see AMBR/CSNET.LSP
;;;   (4)  level2 := max { act(second), *hyp-zero-act* }
;;;
;;;   (5)  delta1 := *positive-rating-factor* * (level1 - level2)
;;;   (6)  deltaH := *negative-rating-factor* * (level1 - act(H))  for each H
;;;          ;; Note that deltaH is negative because *neg-rtg-factor* is negtv.
;;;
;;;   (7)  rtg(leader) := old-rtg(leader) + delta1    ; LEADER gains strength
;;;   (8)  rtg(H) := max {0, old-rtg(H) + deltaH}     ; all other H lose strgth
;;;
;;;  The parameters xxx-RATING-FACTOR* and *HYPOTH-ZERO-ACT* are defined in
;;;  AMBR/DEFS.LSP.  See AMBR/CSNET.LSP for details on *HYPOTH-ZERO-ACT*.
;;;
;;;  The actual computations coded below are somewhat more complicated because
;;;  they must handle some special cases:
;;;   case1. When there is only one hypothesis (and hence there is no SECOND
;;;            as defined by eq.(2)), set LEVEL2 to *HYPOTH-ZERO-ACT*.  (3)
;;;   case2. When some hypothesis H is new (and hence does not have old rating),
;;;            use parameter *INITIAL-RATING* as OLD-RTG(H) in eq. (9).
;;;            (This is done by the generic function INDIVIDUAL-RATING above.)
;;;            In addition, check whether the new hypothesis need skolemization
;;;            (via NEED-SKOLEMIZATION-P defined in AMBR/SKOLEM.LSP) and mark
;;             the newly constructed indiv. rating with the tag :NEED-SKOLEMIZ.
;;;            (This is done by the function FIND-OR-MAKE-INDIVIDUAL-RATING.)
;;;   case3. When the old rating table contains an entry for a hypothesis that
;;;            is not in the new list (e.g. because it has died in the interim),
;;;            remove this entry from the rating table.
;;;  As a consequence from cases 2 and 3, the new rating table contains entries
;;;  for all hypotheses from the current set and only those hypotheses. In
;;;  particular, the length of the updated table is equal to that of RELEV-HYPS.
;;;
;;;  Note also that the updated rating table is sorted acrdg. to hyp act.levels.
;;;  The leader is at the front.  This is used by CHECK-FOR-xxx-RATINGS below.

(defun  update-rating-table  (secretary relev-hyps)
  "Update the individual ratings according to current activation levels."
  (declare (type instance-agent secretary)
           (type list relev-hyps)    ; produced by IDENTIFY-RATING-HYPS
           (values list) )           ; of new ratings, sorted
  (let* ((rating-table (secretary-rating-table secretary))
         (old-ratings  (rating-table rating-table))
         (sorted-hyps  (sort relev-hyps #'> :key #'agent-activation))
         (leader (first sorted-hyps))                                  ; (1)
         (level1 (max *hypoth-zero-act* (agent-activation leader)))    ; (3)
         (level2 (if (< (length sorted-hyps) 2)
                     *hypoth-zero-act*                                 ; case1.
                     (max *hypoth-zero-act*                            ; (4)
                          (agent-activation (second sorted-hyps))) ))  ; (2)
         (delta1 (* *positive-rating-factor*                           ; (5)
                    (- level1 level2)))
         (new-ratings nil) )
    (push (find-or-make-individual-rating leader                       ; (7)
                (+ delta1
                   (individual-rating leader old-ratings))
                old-ratings)
          new-ratings)
    (dolist (h (rest sorted-hyps))   ; all but LEADER
      (let* ((deltaH (* *negative-rating-factor*                       ; (6)
                        (- level1 (agent-activation h))))
             (old+deltaH (+ (individual-rating h old-ratings)          ; (8)
                            deltaH)) )
        (push (find-or-make-individual-rating h                        ; (8)
                    (max 0.0 old+deltaH)
                    old-ratings)
              new-ratings) ))
    (setf (rating-table rating-table)      ; store and return new ratings
          (nreverse new-ratings)) ))       ; keep LEADER at front


(defun  find-or-make-individual-rating (hypothesis new-rating old-IRs)
   "Make a new individual-rating structure or update the existing one."
   ;; Implements case2 of the list above.
  (declare (type corr-agent hypothesis)
           (type number new-rating)
           (type list old-IRs)
           (values list) )    ; individual-rating
  (let ((old-IR (find hypothesis old-IRs :key #'IR-hypothesis)))
    (cond (old-IR   ; Reuse old IR structure if such exists.
              (setf (IR-rating old-IR)              ; reuse old structure
                         (coerce new-rating 'float))
                   old-IR)
          ;; New HYPOTHESIS -- handle tags related to skolemization
          ((need-skolemization-p hypothesis)
              (make-individual-rating hypothesis new-rating '(:NEED-SKOLEMIZ)))
          (t  (make-individual-rating hypothesis new-rating NIL)) )))



;;;;;;  *******  LOSER-HYPOTHESIS ELIMINATION  ********
;;;
;;; The rating mechanism was designed to select winner hypotheses.  Another
;;; purpose that it now serves, however, and no less important one, is to
;;; eliminate 'obvious loser' hypotheses.
;;;
;;; This aspect of the rating mechanism may be turned off by setting
;;; *LOSER-HYPOTHESIS-ELIMINATION-FLAG* to NIL.  The flag defaults to T.
;;; (See AMBR/DEFS.LSP.  Compare with *MARKER-EMISSION-FLAG*.)
;;;
;;; The individual ratings for all non-leader hypotheses decrease with each
;;; rating survey according to formulas (6) and (8) of UPDATE-RATING-TABLE
;;; above.  Moreover, the speed of the decrease is related to the activation
;;; level of the loser hypothesis.
;;;
;;; Whenever the individual rating of a hypothesis drops below *LETHAL-RATING*
;;; (parameter in AMBR/DEFS.LSP), it is considered for elimination.  To avoid
;;; unwarranted or premature eliminations, an additional check is made -- the
;;; hypothesis is eliminated only if its activation level is below *CRITICAL-
;;; LOSER-LEVEL* defined in AMBR/DEFS.LSP).  If the activation level is above,
;;; the hypothesis survives but its bad rating remains.  The loser will be in
;;; danger at the next rating survey too.
;;;
;;; To sum up, a hypothesis is eliminated if:
;;;  0. *LOSER-HYPOTHESIS-ELIMINATION-FLAG* is non-NIL,
;;;  1. its rating is below *LETHAL-RATING*,  and
;;;  2. its activation level is below *CRITICAL-LOSER-LEVEL*
;;;
;;; (See also ELIMINATE-LOSERS-AFTER-PROMOTION in AMBR/PROMOTN.LSP.)

(defun check-for-lethal-ratings (secretary ratings &aux (count 0))
  "Check whether some hypotheses should be eliminated."
  (declare (type instance-agent secretary)
           (type list ratings)    ; NB: list of IRs, not a rating-table
           (values integer) )     ; number of eliminations
  (when *loser-hypothesis-elimination-flag*                           ; crit 0.
    (dolist (indiv-rtg (rest ratings))    ; skip leader hypothesis
      (when (<= (IR-rating indiv-rtg) *lethal-rating*)                ; crit 1.
        (let ((loser (IR-hypothesis indiv-rtg)))
          (when (< (agent-activation loser) *critical-loser-level*)   ; crit 2.
            (incf count)
            (eliminate-loser-hypothesis secretary loser) )))))
            ;; Note that ELIMINATE-... is suspendable and therefore only pushes
            ;; closures on a stack. This reverses order and the hypothesis that
            ;; will actually be eliminated first is the last one listed in
            ;; RATINGS.  The order imposed by UPDATE-RATING-TABLE entails that
            ;; the losers will receive fizzle messages in proper order.
  count )


;; ELIMINATE-LOSER-HYPOTHESIS works according to the following algorithm:
;;  1. Remove the loser from the secretary list. (This may be important during
;;      hypothesis registration of duplicate embryos. See AMBR/SECRETAR.LSP).
;;  2. Send a fizzle message to the loser (see AMBR/FIZZLE.LSP).

(defun  eliminate-loser-hypothesis (secretary loser)
  "Remove LOSER from the secretary list and send it a fizzle message."
  (declare (type instance-agent secretary)
           (type hypoth-agent loser)
           (values S-PROGN-return-type) )
  (s-progn secretary
     (unregister-hypothesis secretary loser)    ; no consumption
     [send-fizzle-message   secretary loser] ))



;;;;;;  *******  TRIGGERING THE SKOLEMIZATION MECHANISM  ********
;;;
;;;  The skolemization mechanism (see AMBR/SKOLEM.TXT and =.LSP) converts
;;;  'general hypotheses' into 'specific hypotheses'.  The skolemization
;;;  process, however, is complex and time-consuming and, therefore, should
;;;  be triggered only when truly warranted.  Many 'general propositions' may
;;;  be retrieved from the semantic memory but only a few of them really merit
;;;  skolemization.  The rating mechanism can weed out the rest.
;;;
;;;  The skolemization mechanism is triggered when the authorized secretary
;;;  sends a SKOLEM-INCENTIVE to the hypothesis -- see AMBR/SKOLEM.TXT and =.LSP
;;;  Such incentive is sent when the following criteria are met:
;;;    Cr0. *SKOLEMIZATION-FLAG* is non-NIL.
;;;    Cr1. The hypothesis satisfies the predicate NEED-SKOLEMIZATION-P.
;;;           To avoid doing this expensive check during each rating survey,
;;;           the rating table keeps a tag :NEED-SKOLEMIZ for such hypotheses.
;;;           (See function FIND-OR-MAKE-INDIVIDUAL-RATING above.)
;;;    Cr2. The rating of the hypothesis exceeds *SKOLEM-RATING*.
;;;    Cr3. Skolem incentives have not been sent to this hypothesis before.
;;;           The tag :SKOLEMIZ in the rating table marks the hypotheses that
;;;           have already been sent such incentives.
;;;
;;; Skolemization incentives may also be sent during 'big ballotages' -- see
;;; SKOLEMIZE-ON-BALLOTAGE later in this file.  In fact, most of the incentives
;;; are sent in this way because general hypotheses rarely win the constraint
;;; satisfaction competition by themselves.
;;;
;;; The emission of Skolem incentives (and thus the skolemization mechanism
;;; as a whole) may be disabled by setting *SKOLEMIZATION-FLAG* to NIL.
;;; The flag defaults to T (see AMBR/DEFS.LSP).
;;; Compare with *MARKER-EMISSION-FLAG*.

(defun check-for-Skolem-ratings (secretary ratings)
  "Send Skolem-incentives to promising general hypotheses."
  (declare (type instance-agent secretary)
           (type list ratings)    ; NB: list of IRs, not a rating-table
           (values S-PROGN-return-type) )
  (when *skolemization-flag*                                   ; Cr0.
    (let ((incentive-receivers nil))
      (dolist (IR ratings)
        (when (and (member :need-skolemiz (IR-other IR))       ; Cr1.
                   (> (IR-rating IR) *Skolem-rating*)          ; Cr2.
                   (not (member :skolemiz (IR-other IR))) )    ; Cr3.
          ;; Arrange that this hypothesis receives a Skolem incentive
          (push (IR-hypothesis IR) incentive-receivers)
          ;; Replace the tag :NEED-SKOLEMIZ with :SKOLEMIZ     ; Cr3.
          (setf (IR-other IR)
                (adjoin :skolemiz
                        (remove :need-skolemiz (IR-other IR)))) ))
      (unless (null incentive-receivers)   ; usually it is null
        (s-progn secretary
           (dolist (hypothesis (nreverse incentive-receivers))  ; active 1st
            [send-Skolem-incentive hypothesis secretary]) ))
      )))



;;;;;;  *******  H Y P O T H E S I S   P R O M O T I O N   *********
;;;
;;;  The mission of the rating mechanism is to promote a winner.  This should
;;;  be done carefully, however, to avoid premature promotions.
;;;
;;;  UPDATE-RATING-TABLE (see above) has identified the current LEADER and
;;;  has placed it at the front of the RATINGS list.  The job of CHECK-FOR-
;;;  WINNER-RATINGS is to check whether the current leader is worthy of becoming
;;;  the final winner.
;;;  To be promoted, the leader must meet the following three criteria:
;;;   Cr1. its individual rating should be at least *WINNER-RATING*,
;;;   Cr2. its curr. activation level should be at least *CRITICAL-WINNER-LEVEL*
;;;   Cr3. the correspondence represented by the LEADER hypothesis about
;;;         instance agents should be consistent with the leading (or winner)
;;;         hypothesis about situation agents. See CHECK-SITUATION-WINNER below.
;;;
;;;  The actual promotion is done by sending a 'promotion incentive' message
;;;  to the winner.  See AMBR/PROMOTN.LSP for more details on promotion
;;;  incentives and the promotion mechanism in general.
;;;  (Note that the function SEND-PROMOTION-INCENTIVE is used before being
;;;   defined.  Do not be troubled if the compiler issues a warning.)
;;;
;;;  A special situation arises when the leader passes the first criterion
;;;  above but fails some of the other two.  Stated differently, when a hypoth.
;;;  has high rating but is rejected on some other grounds.  In this case
;;;  we have a BALLOTAGE -- it is judged that the rating procedure must be
;;;  repeated.  There are two kinds of ballotages:
;;;   + 'small' ballotage -- when the leader fails Criterion 2 above.
;;;       In this case the secretary simly has to give more time to the
;;;       constraint satisfaction mechanism (see AMBR/CSNET.LSP). To that end,
;;;       the individual rating of the leader is set to *BALLOTAGE-RATING*.
;;;       No other actions are taken.
;;;       (*BALLOTAGE-RATING* is a parameter in AMBR/DEFS.LSP which is lower
;;;        than *WINNER-RATING* but is higher than *INITIAL-RATING*.)
;;;   + 'big' ballotage -- when the leader passes Criterion 2 but fails Crit 3.
;;;       This case is treated in a separate section below.
;;;
;;;  CHECK-FOR-WINNER-RATINGS returns (via S-VALUE) a value indicating whether
;;;  a winner has been promoted.  This value is catched by RATING-SURVEY
;;;  (via SUSPENDED-VALUE-BIND -- see DUAL/ARCHIT/MAILBOX.LSP) and is checked
;;;  whether to continue with INITIATE-RATING-SURVEY or FINISH-RATING.

(defun check-for-winner-ratings (secretary ratings)
  "If the current leader is the final winner, promote it."
  (declare (type instance-agent secretary)
           (type list ratings)          ; sorted list of IRs (individual ratngs)
           (values mailbox) )           ; (s-values NIL)  or  (s-values winner)
  (let* ((leader-IR  (first ratings))   ; RATINGS is guaranteed to be non-empty
         (leader     (IR-hypothesis leader-IR))
         (leader-rtg (IR-rating     leader-IR))
         (leader-act (agent-activation leader)) )   ; no consumption
    (s-progn secretary
       (cond ((< leader-rtg *winner-rating*)  (s-values NIL))           ; Cr1.
             ((< leader-act *critical-winner-level*)                    ; Cr2.
                   (setf (IR-rating leader-IR) *ballotage-rating*)  ; 'small'
                   (s-values NIL))                                  ; ballotage
             (t  ;; We need to check at the situation level.
                 (suspended-value-bind
                            (promote-p ballotage-sit win-sit-hypoth)
                            [check-situation-winner secretary leader]   ; Cr3.
                   (cond (promote-p                         ; promotion
                            [send-promotion-incentive leader secretary]
                            (s-values T))
                         ((null ballotage-sit)              ; 'small' ballotage
                            (setf (IR-rating leader-IR) *ballotage-rating*)
                            (s-values NIL))
                         (t                                 ; 'big' ballotage
                            [ballotage-rating secretary  ratings
                                       ballotage-sit  win-sit-hypoth]
                            (s-values NIL))))) ))))


;; CHECK-SITUATION-WINNER is the final guard against inconsistent promotions.
;; It returns three values (via S-VALUES) -- PROMOTE-P, BALLOTAGE-SIT, and
;; WIN-SIT-HYPOTH.  These values are used by CHECK-FOR-WINNER-RATINGS (via
;; SUSPENDED-VALUE-BIND) to determine whether to promote or to ballotage.
;; When PROMOTE-P is T, BALLOTAGE-SIT and WIN-SIT-HYPOTH are always NIL.
;; When BALLOTAGE-SIT is NIL, WIN-SIT-HYPOTH is always NIL too.
;; When PROMOTE-P is NIL, BALLOTAGE-SIT is either NIL or the situation-agent
;;   that currently leads (or even wins) the competition for the driver sit.
;;   When BALLOTAGE-SIT is NIL, it is too early for promotions because the
;;     driver situation-agent has not been mapped yet.  Do a 'small' ballotage.
;;   When BALLOTAGE-SIT is not NIL, it is passed to BALLOTAGE-RATING to do
;;     a 'big' ballotage.  The 'most promising' hypothesis from the secretary
;;     list of the driver situation is also passed as WIN-SIT-HYPOTH.
;;     (See the two return values of DRIVER-ELT-MAPPING in AMBR/SECRETAR.LSP.)
;;
;; Consider the following example (taken from the 'ice cube experiment -- see
;; AMBR_EXP/ICE).  There is a target situation involving an ice cube in a glass
;; of coke.  There are two base situations: (i) SIT-A involves an immersion
;; heater in a cup of water, (ii) SIT-B involves an ice cube in a refridgerator.
;; Structurally, SIT-A maps better to the target with the ice-cube going to
;; imm-heater, cold to hot, etc.  Initially, however, the model doesn't 'know'
;; this.  It starts by building the semantically grounded hypothesis that
;; the ice-cube in the target maps to the ice-cube in SIT-B.  This latter
;; hypothesis eventually loses the competition because of the holistic effect
;; of the constraint satisfaction network.  For quite a long time, however,
;; the hypothesis ICE-CUBE-T<-->ICE-CUBE-B is the 'leader' in the rating.
;;
;; The rating routines discussed so far work with local information only.
;; The secretary considers only the hypotheses registered at its own list.
;; Based on such information, it is impossible to tell that, in the example
;; above, the hypothesis ICE-CUBE-T<-->ICE-CUBE-B is 'bad' while the hypoth
;; GLASS-T<-->GLASS-A is 'good', especially during the early stages of mapping.
;; Both hypotheses maintain high activation levels and are much stronger than
;; any of their rivals.  Therefore, their ratings reach *WINNER-RATING* quite
;; rapidly and the hypotheses will be considered for promotion.  The problem
;; is how to decide that it is good to promote GLASS-T<-->GLASS-A but it is
;; bad to promote ICE-CUBE-T<-->ICE-CUBE-B.
;;
;; One straightforward solution is simply to wait long enough for the constraint
;; satisfaction net to settle (see CSNET.LSP). This can be achieved quite easily
;; by lifting the *WINNER-RATING* level.  This approach, however, entails that
;; hypothesis promotions (and hence the transfer mechanism that they trigger)
;; will happen _only_after_ the mapping process has completed.  This imposes
;; too much seriality in the model -- first map and only then transfer.  This
;; seriality is antithetical to the spirit of AMBR.  It would be much better
;; if the transfer mechanism interleaves with mapping.  Therefore, *WINNER-
;; RATING* should not be inflated.
;;
;; The solution adopted here is to bring to bear some non-local information
;; as a safeguard against the anomalies like the one illustrated above.
;; (By the way, such anomalies are quite rare.)  The last check that is made
;; before promoting a hypothesis is whether it is consistent with the leading
;; correspondence between the respective situation agents (see AMBR/CORRESP.LSP)
;; In the example above, this would mean that ICE-CUBE-T<-->ICE-CUBE-B would
;; be promoted only if the hypothesis SITUATION-T<-->SITUATION-B leads the
;; competition at the level of situation-agents.  The check is made by
;; inspecting the secretary list of SITUATION-T.
;; (Compare with the bottom-up structure correspondence in AMBR/STR_CORR.LSP.)
;;
;; An additional complication arises with the so-called free-standing instances.
;; They are instance agents that are not affiliated to any situation (see AMBR/
;; SITUATN.TXT).  For such instances, there isn't any correspondence of the
;; form  SITUATION-T<-->???  which can be checked.  In the current version of
;; AMBR [2.2.1] these free-standing instances are never promoted by the rating
;; mechanism.  (Theoretically, they can be promoted by somebody else.)  This
;; decision may need reconsideration in the future.  Its main rationale is
;; that the free-standing elements may block the unmapped target elements and
;; thus undermine the transfer mechanism.

;; All this is concretized in the following algorithm for CHECK-SITUATION-WINNER
;;  1. Retrieve the 'recipient element' of the correspdce represented by HYPOTH.
;;       (The 'driver elt' is always equal to SECRETARY because HYPOTH is a
;;        'relevant hypothesis' -- see IDENTIFY-RATING-HYPS above.)
;;  2. Retrieve the driver situation of HYPOTH.  Call it DRIVER-SIT.
;;       (The 'driver elt' (i.e. SECRETARY) is affiliated to the same sit.)
;;  3. Identify the situation mapped to DRIVER-SIT by calling DRIVER-ELT-MAPPING
;;       (see AMBR/SECRETAR.LSP).  Let it be called MAP-SIT.  Identify the
;;       'most promising' hypothesis on the secretary list of DRIVER-SIT by
;;       capturing the second return value of DRIVER-ELT-MAPPING.  Let it be
;;       called WIN-SIT-HYPOTH.
;;  4. If MAP-SIT is NIL, which means that the driver situation-agent has not
;;       yet registered any non-embryo hypothesis on its secretary list, it is
;;       too early to make any commitments (i.e. promotions).
;;       Return  (s-values NIL NIL NIL).                    ; 'small' ballotage
;;  5. If the recipient element from step 1. is a concept agent, return
;;       (s-values NIL MAP-SIT WIN-SIT-HYPOTH) and stop.    ; 'big' ballotage
;;  6. Otherwise the recipient elt should be an instance agent.  Retrieve the
;;       situation to which it is affiliated.  Call it RECIPIENT-SIT.
;;  7. Compare RECIPIENT-SIT and MAP-SIT for equality.
;;   7a. If they are equal, return  (s-values T NIL NIL).   ; promotion
;;   7b. Otherwise  (s-values NIL MAP-SIT WIN-SIT-HYPOTH)   ; 'big' ballotage

(defun  check-situation-winner (secretary hypoth)
  "Is instance-level HYPOTH consistent with the winner at the situation level?"
  (declare (values mailbox))    ; (s-values promote-p ballotage-situation)
  (let ((recipient-elt (corresp-elt hypoth *recipient-elt-label*))   ; step 1.
        (driver-sit    (corresp-driver hypoth)) )                    ; step 2.
    (multiple-value-bind (map-sit win-sit-hypoth)                    ; step 3.
                    (driver-elt-mapping driver-sit :winner-only-p nil)
      (s-progn secretary
        (s-eval (get-consumption 'driver-elt-mapping T) nil)         ; step 3.
        (cond ((null map-sit)                                        ; step 4.
                 (s-values nil nil nil))
              ([agent-type recipient-elt :concept]                   ; step 5.
                 (s-values nil map-sit win-sit-hypoth))
              ((eq map-sit                                           ; step 7.
                   [agent-situation recipient-elt])                  ; step 6.
                 (s-values T nil nil))                               ; step 7a.
              (t (s-values nil map-sit win-sit-hypoth)) )))))        ; step 7b.



;;;;;;  ******   B A L L O T A G E   R A T I N G S   ******
;;;
;;;  As stated above, there is a 'ballotage' when the leader hypothesis in
;;;  a secretary list has achieved *WINNER-RATING* (Criterion 1) but fails
;;;  to achieve some of the other two criteria for promotion.
;;;
;;;  The function CHECK-FOR-WINNER-RATINGS handles the so-called 'small
;;;  ballotage'.  This section deals with the 'big ballotage'.
;;;
;;;  We have a 'big ballotage' when the leader is acceptable by all local
;;;  criteria but is inconsistent with the situation-level mapping.  Stated
;;;  differently, the driver situation is mapped to some other situation
;;;  (referred to as BALLOTAGE-SIT below) which is different from the
;;;  affiliation of the local leader.  The function CHECK-SITUATION-WINNER
;;;  is responsible for detecting the need for ballotage and for identifying
;;;  the ballotage situation and the 'most promising' hypothesis on the
;;;  secretary list of the driver sit (see DRIVER-ELT-MAPPING in SECRETAR.LSP).
;;;
;;;  The aim of the steps implemented below is to foster alternative hypotheses.
;;;  Hopefully, these alternative hypotheses will supplant the current leader.
;;;  Moreover, the hope is that the new leader will belong to BALLOTAGE-SIT
;;;  and thus will fit into the global mapping.
;;;
;;;  The search for alternative hypotheses is done in two orthogonal directions:
;;;    1. The rating of the current leader is decreased while the rating of
;;;       the second-best hypothesis is increased.  Moreover, an inhibitory
;;;       link is established from the 'most promising' hypothesis and the
;;;       current leader.  If the constraint satisf. mechanism later swaps the
;;;       activation levels of the two competing hypotheses, the correction of
;;;       the individual ratings will speed up the promotion of the alternative.
;;;       (Note that on 'big' ballotages the rating of the leader is set to
;;;        *INITIAL-RATING* while on 'small' baltg -- to *BALLOTAGE-RATING*.)
;;;    2. The skolemization mechanism is invoked by calling the function
;;;       SKOLEMIZE-ON-BALLOTAGE (defined in AMBR/SKOLEM.LSP).  This function
;;;       tries to create new hypotheses involving instances of BALLOTAGE-SIT.
;;;       The list of individual ratings is used to produce a list of
;;;       'skolemizable' hypotheses ordered by activation levels.  The tags
;;;       stored in the rating table (cf. CHECK-FOR-SKOLEM-RATINGS above)
;;;       help distinguish the skolemizable hyps.
;;;
;;;  When *SKOLEMIZATION-FLAG* is NIL, the second direction is blocked.

(defun  ballotage-rating (secretary ratings ballotage-sit win-sit-hypoth)
  "Try to foster alternative hypoth's because the current leader is inappropr."
  (declare (type secretary-agent secretary)
           (type list ratings)
           (type instance-agent ballotage-sit)   ; situation-agent actually
           (type corr-agent win-sit-hypoth)
           (values S-PROGN-return-type) )
  (let ((leader-IR (first  ratings))
        (second-IR (second ratings))
        (skolemizable-hyps (skolemizable-hypotheses ratings)) )
    (s-progn secretary
       [send-write-request win-sit-hypoth                ; create inhibitory
                           'add-link :t-link             ; link
                           (IR-hypothesis leader-IR)
                           *contradict-hypoth-weight*]
       (setf (IR-rating leader-IR) *INITIAL-rating*)     ; decrease leader
       (unless (null second-IR)
         (let ((new-second-rating
                    (if (member (IR-hypothesis second-IR) skolemizable-hyps)
                        (min *SKOLEM-rating* *ballotage-rating*)
                        *BALLOTAGE-rating*)))
           (setf (IR-rating second-IR)                   ; increase second
                 (max (IR-rating second-IR) new-second-rating))))
       (when *skolemization-flag*
         [skolemize-on-ballotage secretary ballotage-sit
                                           skolemizable-hyps
                                           ratings]) )))

(defun  skolemizable-hypotheses (ratings)
  (declare (type list ratings)  ; of individual ratings
           (values list) )      ; of 'skolemizable' hypotheses, freshly consed
  (let ((result nil))
    (dolist (IR ratings)
      (when (intersection (IR-other IR) '(:need-skolemiz :skolemiz))
        (push (IR-hypothesis IR) result) ))
    (nreverse result) ))

(defun  skolemize-on-ballotage (secretary ballotage-sit hypoth-list ratings)
  "Send a skolem incentive to the first hyp that has relevant markers, if any."
  (declare (type instance-agent secretary ballotage-sit)
           (type list hypoth-list)         ; of general hyps, ordered by activ.
           (type list ratings)             ; of individual ratings
           (values S-PROGN-return-type) )
  (unless (endp hypoth-list)               ; recursion base
    (let ((curr-hypoth (first hypoth-list)))
      (s-progn secretary
        (suspended-value-bind (has-relevant-markers-p)
                   [has-relevant-markers-p curr-hypoth    ; see AMBR/SKOLEM.LSP
                                           ballotage-sit  ; relevance criterion
                                           secretary]     ; host processor
          (if has-relevant-markers-p       ; merits skolemization?
              (let ((IR (find curr-hypoth ratings :key #'IR-hypothesis)))
                (setf (IR-other IR)        ; keep IR tags up to date
                      (adjoin :skolemiz (remove :need-skolemiz (IR-other IR))))
                [send-Skolem-incentive curr-hypoth secretary])      ; and stop
              [skolemize-on-ballotage secretary ballotage-sit
                                                (rest hypoth-list)  ; loop
                                                ratings]))) )))


;;;;;;;  End of file AMBR/RATING.LSP
