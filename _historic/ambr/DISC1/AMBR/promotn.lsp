;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/promotn.lsp
;;; VERSION:    2.2.2  ; see AMBR/VERSION.LSP
;;; PURPOSE:    Promoting hypotheses into non-hypothetical corresp-agents.
;;; DEPENDS-ON: DUAL; ambr/secretar.lsp, ambr/hypoth.lsp, ambr/rating.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    03-03-98 [2.2.1]
;;; UPDATED:    14-08-98 [2.2.2] The 'official release'
;;; UPDATED:    ...
;;;;;;;;
;;; TO DO:      Propagate promotions to concept-agents, see AMBR/PROMPROP.LSP


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;      P R O M O T I O N   M E C H A N I S M      ;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "AMBR-CORE")

;;;; The key concept defined in this file is PROMOTION of a hypothesis agent
;;;; into a WINNER hypothesis agent.
;;;;
;;;; ...


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: promotion-symbolic-structure,
;;          promotion-incentive, send-promotion-incentive,
;;          promotion-incentive-secretary,
;;          metamorphosis-notification, send-metamorphosis-notification,
;;          metamorphosis-notification-imago
;;
;; The functions dealing with loser elimination use the following parameters
;; from AMBR/DEFS.LSP:  *loser-hypothesis-elimination-flag*, *loser-protection-
;;  quota*, *element->winner-hypoth-weight*, *element->loser-hypoth-weight*,
;;  *hypoth-zero-act*, and *initial-rating*.


;; ...

;;;;;; Generic function(s) pertaining to the external protocol

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;  ******  SYMBOLIC STRUCTURES RELATED TO PROMOTION  ********
;;;
;;;  This portion of the file defines several classes of symbolic structures
;;;  (see DUAL/ARCHIT/SYMPROC1.LSP) exchanged between the agents as part of
;;;  the promotion protocol.
;;;

;;;;;;   Class definitions  (and accessor methods)

(eval-when (compile load eval)
  (defclass  promotion-symbolic-structure (secretary-symbolic-structure)
    ()
    (:documentation
      "Symbolic structure related to the promotion mechanism. Base class." ))

  (defclass  promotion-incentive  (promotion-symbolic-structure)
    ((secretary   :reader     promotion-incentive-secretary
                  :type       secretary-agent
                  :initarg    :secretary
                  :initform   (required-argument) )
    )
   (:documentation
     "Symbolic structure sent by a secretary to its 'winning hypothesis'." ))

  (defclass  metamorphosis-notification  (promotion-symbolic-structure)
    ((imago    :reader     metamorphosis-notification-imago
               :type       AMBR-agent
               :initarg    :imago
               :initform   (required-argument) )
    )
   (:documentation
     "Symbolic structure marking the end of a successful metamorphosis." ))
) ; eval-when


;;;;  Constructors

(defun send-promotion-incentive (hypothesis secretary)
  "SECRETARY sends a promotion incentive to HYPOTHESIS."
  (declare (type corr-agent hypothesis)
           (type secretary-agent secretary)
           (values promotion-incentive) )
  (receive-symbol hypothesis (make-instance 'promotion-incentive
                                            :secretary secretary )))

(defun send-metamorphosis-notification (receiver imago)
  "IMAGO sends a metamorphosis notification to RECEIVER."
  (declare (type AMBR-agent receiver imago)
           (values metamorphosis-notification) )
  (receive-symbol receiver (make-instance 'metamorphosis-notification
                                          :imago imago )))


;;;;;;  Printing methods

(defmethod  print-object ((promote promotion-incentive) stream)
  (if (and (slot-boundp promote 'secretary)
           (agentp (promotion-incentive-secretary promote)) )
      (format stream "#<PR ~A>"
                     (agent-name (promotion-incentive-secretary promote)))
      (format stream "#<malformed promotion-incentive>") ))

(defmethod  print-object ((mmorph metamorphosis-notification) stream)
  (if (and (slot-boundp mmorph 'imago)
           (agentp (metamorphosis-notification-imago mmorph)) )
      (format stream "#<MM ~A>"
                     (agent-name (metamorphosis-notification-imago mmorph)))
      (format stream "#<malformed metamorphosis-notification>") ))


(defmethod DUAL-describe ((promote promotion-incentive)
                          &optional (stream *standard-output*) )
  (format stream "~&~S is a ~A.~%"
          promote (type-of promote) )
  (format stream "~&  Its secretary is: ~S~%"
          (if (slot-boundp promote 'secretary)
              (promotion-incentive-secretary promote)
              "(unbound)"))
  (values))

(defmethod DUAL-describe ((mmorph metamorphosis-notification)
                          &optional (stream *standard-output*) )
  (format stream "~&~S is a ~A.~%"
          mmorph (type-of mmorph) )
  (format stream "~&  Its imago is: ~S~%"
          (if (slot-boundp mmorph 'imago)
              (metamorphosis-notification-imago mmorph)
              "(unbound)"))
  (values))


;;;;  Type-checking methods for the accessors

(defmethod  promotion-incentive-secretary ((x t))
  (error "PROMOTION-INCENTIVE-SECRETARY: ~S is not a promotion incentive." x ))

(defmethod  metamorphosis-notification-imago ((x t))
  (error "METAMORPHOSIS-NOTIFICATION-IMAGO: ~S is not a metamorphosis notif."
         x ))



;;;;;;  *******  H Y P O T H E S I S   P R O M O T I O N   *********
;;;
;;; Only a mature hypothesis-agent can be promoted.  The promotion process
;;; begins when the hypothesis receives a PROMOTION-INCENTIVE sent by the
;;; respective 'authorized secretary' (see AMBR/RATING.LSP).  The promoted
;;; hypothesis transforms itself into a winner-hypoth-agent and notifies
;;; its secretaries about this by sending them metamorphosis-notifications.


;; When a mature hypothesis-agent receives a promotion incentive, it does
;; two things:
;;   1. it transforms itself into a winner-hypoth-agent   and
;;   2. it sends metamorphosis notifications to the two secretaries.
;;      (Exception: trivial hypotheses send only one notification.)
;;  [3. To do: propagate the promotion to concept-agents...]
;;
;; In this version of the model, the data field of the promotion-incentive
;; (i.e. the secretary that has sent it) is ignored.  Only the type of
;; symbolic structure itself is used.
;;
;; Compare with the hypothesis-registration requests sent by embryo hypotheses
;; at the first stage of the hypothesis life cycle (see AMBR/HYPOTH.LSP).

(defmethod handle-symbol ((host mature-hypoth-agent)
                          (promote promotion-incentive) )
  (declare (values S-PROGN-return-type))
  (let ((driver-elt    (corresp-elt host *driver-elt-label*))
        (recipient-elt (corresp-elt host *recipient-elt-label*)) )
   (s-progn host
     [mature->winner-hypothesis host]                            ; step 1.
     (if [trivial-corresp-p host]
         [send-metamorphosis-notification driver-elt host]       ; step 2.
         (progn                                                  ; step 2.
           [send-metamorphosis-notification driver-elt host]
           [send-metamorphosis-notification recipient-elt host])) )))

#+:DUAL-DEBUG
(defmethod handle-symbol ((host winner-hypoth-agent)
                          (promote promotion-incentive) )
  (warn "Ignoring ~S because ~S is already winner at time ~,2F."
        promote host *time* ))

(defun mature->winner-hypothesis (mature)
  "Transform a mature hypothesis into a winner hypothesis."
  (declare (type hypoth-agent mature)    ; mature or dead
           (values hypoth-agent) )       ; winner or dead
  (ensure-hypothesis-elements mature)
  (unless (dead-agent-p mature)
    (let ((TYPE-slot (locate-mfr-component mature :type)))
      (remove-filler-elt TYPE-slot :embryo  nil)  ; avoid notifying many times
      (remove-filler-elt TYPE-slot :mature  nil)
      (add-filler-elt    TYPE-slot :winner  :notify-p T) )
    (change-class mature 'winner-hypoth-agent) )
   mature)

;; Compare with EMBRYO->MATURE-HYPOTHESIS defined in AMBR/HYPOTH.LSP


;; Winner hypotheses are not NCR-sending agents. Mature hypotheses are,
;; however, and that is why the suspended stack of the just promoted
;; agent contains a closure that calls the generic function AGENT-NCR-QUEUE.
;; This call is a remnant of an old (elaborate) symbolic micro-cycle.
;; The dummy method below prevents error messages (see DUAL/ARCHIT/NC_AGENT.LSP).
;; (The NCR queue is empty anyway because the hypothesis should have generated
;;  its 'children' (see AMBR/STR_CORR.LSP) long before promotion.)

(defmethod agent-ncr-queue ((promoted winner-hypoth-agent))
   nil )


;; In the current version of AMBR, winner hypotheses are not expected to fizzle.
;; See AMBR/FIZZLE.LSP for documentation on fizzle messages.

#+:DUAL-DEBUG
(defmethod handle-symbol :before ((winner winner-hypoth-agent)
                                  (fizzle fizzle-message) )
  (warn "Winner hypothesis ~S fizzles due to a fizzle message from ~S."
        winner (fizzle-message-sender fizzle)) )


#|  ;; Dead code
(defmethod  receive-activation :around  ((ag winner-hypoth-agent)
                                         (amount float))
  (declare (type single-float amount))
  (if (minusp amount)
      amount                      ; do not call the primary method
      (call-next-method) ))       ; call the primary method
|#


;;;;;;  *******  LOSER-HYPOTHESIS ELIMINATION  ********
;;;
;;; When a hypothesis from a secretary list (see AMBR/SECRETAR.LSP) is
;;; promoted, its sends a PROMOTION-NOTIFICATION to its secretaries.
;;; When a secretary receives such notification, it broadly does two things:
;;;   (i) increase the weight of the link to the winner  and
;;;  (ii) eliminate (most of) the loser hypoths registered at the secretary.
;;;        (Controlled by *LOSER-HYPOTHESIS-ELIMINATION-FLAG*.)

;; The concrete algorithm for handling a metamorphosis-notification is:
;;  1. Retrieve the WINNER hypoth from the metamorphosis-notification.
;;      Check that it is registered at SECRETARY (as it should).
;;  2. Check whether WINNER is visible. Abort all if invisible (i.e. dead).
;;  3. Change the weight of the winner's link to *ELEMENT->WINNER-HYPOTH-WEIGHT*.
;;      (The old weight was *ELEMENT->HYPOTH-WEIGHT*, which is typically much
;;      smaller -- see REGISTER-HYPOTHESIS in AMBR/SECRETAR.LSP)
;;  4. Create  T-LINKs between the two mapped secretary-agents.
;;  5. Proceed with ELIMINATE-LOSERS-AFTER-PROMOTION.

(defmethod handle-symbol ((secretary secretary-agent)
                          (mmorph metamorphosis-notification) )
  (declare (values S-PROGN-return-type))
  (let* ((winner-hypoth (metamorphosis-notification-imago mmorph))    ; step 1.
         (winner-symref (find winner-hypoth (agent-hypotheses secretary)
                                            :test #'symref-match)) )
    (s-progn secretary
      (cond ((not [agent-visible-p winner-hypoth]) nil)               ; step 2.
            ((null winner-symref)                                     ; step 1.
                (cerror "Ignore the notification and continue."
                  "~S received a metamorph.notif. from unregistered hypoth ~S."
                        secretary winner-hypoth) )
            (t  [strengthen-winner-link secretary winner-symref]      ; step 3.
                [add-mapped-T-LINK secretary                          ; step 4.
                                   (other-element winner-symref)]
                [eliminate-losers-after-promotion secretary           ; step 5.
                                                  winner-hypoth]) ))))


(defun  strengthen-winner-link (secretary winner-symref)
  "*element->hypoth-weight*  -->  *element->winner-hypoth-weight*"
  (add-link secretary :hypoth winner-symref
                              *element->winner-hypoth-weight*
                              :priority :new ))       ; MODIFY-LINK-WEIGHT

(defun  add-mapped-T-LINK (host-secretary other-secretary)
  "Make a T-LINK b/n two entities that have been put into correspondence."
  (declare (type secretary-agent host-secretary other-secretary))
  (add-link host-secretary :t-link other-secretary           
                                   *mapped-T-LINK-weight*
                                   :priority #'max) )


;; ELIMINATE-LOSERS-AFTER-PROMOTION works according to the following algorithm.
;;  0. If *LOSER-HYPOTHESIS-ELIMINATION-FLAG* is non-NIL, stop and do not
;;       eliminate anything.
;;  1. Retrieve the CORRESP-DRIVER situation from WINNER (see AMBR/CORRESP.LSP).
;;  2. Retrieve the AGENT-HYPOTHESES list from the secretary.
;;  3. Identify the list of relevant hyp's via IDENTIFY-RELEVANT-HYPS.
;;      (The steps up to here are identical with the initial steps of handling
;;       a hypothesis registration request -- see AMBR/SECRETAR.LSP.)
;;  4. Go through this list and consider the tags in the :TYPE slots:
;;   4a. Do not touch any :WINNER hypotheses (normally there should be exactly
;;         one such hypoth -- the newly promoted winner).
;;   4b. Eliminate any :EMBRYO hypotheses by passing them to ELIMINATE-LOSER-
;;         HYPOTHESIS (defined in AMBR/RATING.LSP).
;;   4c. Collect all :MATURE hypotheses in a list.
;;  5. Pass the list collected at step 4c. to ELIMINATE-MATURES-AFTER-PROMOTION.

(defun  eliminate-losers-after-promotion (secretary winner)
  (declare (type secretary-agent secretary)
           (type hypoth-agent winner)
           (values (or null S-PROGN-return-type)) )
  (when *loser-hypothesis-elimination-flag*                          ; step 0.
   (s-progn secretary
     (let* ((driver-sit [corresp-driver winner])                     ; step 1.
            (all-hyps   (agent-hypotheses secretary))                ; step 2.
            (relev-hyps [identify-relevant-hyps all-hyps             ; step 3.
                                                driver-sit])
            (mature-hyps nil) )
       (dolist (symref relev-hyps)
         (let ((hyp (symref-agent symref)))
           (cond ((agent-type hyp :winner)  nil)                     ; step 4a.
                 ((agent-type hyp :embryo)       ; no consumption
                     [eliminate-loser-hypothesis secretary hyp])     ; step 4b.
                 ((agent-type hyp :mature)
                     (push hyp mature-hyps))                         ; step 4c.
                 (t  (cerror "Ignore it and continue."
                      "Hypothesis ~S is neither embryo, nor mature, nor winner."
                             hyp)) )))
       [eliminate-matures-after-promotion secretary mature-hyps] )))) ; step 5.


;; The rating mechanism (AMBR/RATING.LSP) is not absolutely reliable.
;; Therefore, it is desirable to have some 'way back' -- some means to undo
;; the effects of promoting the wrong hypothesis. The most dangerous and
;; irreversible such effect is the elimination of the alternative hypotheses.
;; Therefore, some of the losers are rescued -- they are not killed, only the
;; links to them are weakened (see *ELEMENT->LOSER-HYPOTH-WEIGHT* in DEFS.LSP).
;; This is called 'salvation'.
;;
;; On the other hand, loser hypotheses clutter the system and worsen the
;; energetic balance (because *HYPOTH-ZERO-ACT* is above zero).  Therefore,
;; only a limited number of losers should be saved.  More concretely, a loser
;; hypothesis is rescued if it meets at least one of the following criteria:
;;  Cr1. Its current activation level is above *HYPOTH-ZERO-ACT*.
;;  Cr2. Its individual rating is above *INITIAL-RATING* (see AMBR/RATING.LSP).
;;        (This criterion relates only to 'authorized' secretaries. Unauthorzd
;;         secretaries do not maintaing rating tables -- see AMBR/RATING.LSP.)
;;  Cr3. A small number (e.g. 2) of alternative hypotheses are maintained even
;;        in the absence of any justification. There is a *LOSER-PROTECTION-
;;        QUOTA* (defined in AMBR/DEFS.LSP) that gives the minimal number of
;;        survivors after a promotion.  If too few hypotheses are rescued on
;;        the basis of Cr1 or Cr2 above, the quota is filled up by the next
;;        most active mature hypotheses.

(defun  eliminate-matures-after-promotion (secretary mature-hyps)
  (declare (type secretary-agent secretary)
           (type list mature-hyps)         ; freshly-consed list of mature hyps
           (values S-PROGN-return-type) )
  (s-progn secretary
    (let ((sorted (s-eval (* (length mature-hyps)                        ; Cr3.
                             (get-consumption 'agent-activation))
                          (sort mature-hyps #'> :key #'agent-activation)))
          (rating-table (secretary-rating-table secretary))
          (saved-hyps nil) )
      (dotimes (k *loser-protection-quota*)                              ; Cr3.
        (unless (endp sorted)
          (push (pop sorted) saved-hyps)))
      (dolist (hyp (nreverse sorted))        ; least active losers killed first
        (cond ((> (agent-activation hyp) *hypoth-zero-act*)
                  (push hyp saved-hyps) )                                ; Cr1.
              ((> (individual-rating hyp rating-table) *initial-rating*)
                  ;; Strict '>' avoids errors due to null RATING-TABLE.
                  (push hyp saved-hyps) )                                ; Cr2.
              (t  [eliminate-loser-hypothesis secretary hyp])))
      [salvage-losers-after-promotion secretary (nreverse saved-hyps)] )))


(defun  salvage-losers-after-promotion (secretary saved-hyps)
  (declare (type secretary-agent secretary)
           (type list saved-hyps)         ; freshly-consed list of mature hyps
           (values S-PROGN-return-type) )
  (s-progn secretary
    (dolist (loser-hypoth saved-hyps)
      (let ((loser-symref (find loser-hypoth (agent-hypotheses secretary)
                                             :test #'symref-match)))
        (unless (null loser-symref)                   ; just died by itself?
          [send-write-request loser-hypoth 'add-LOSER-tag]
          [add-link secretary :hypoth loser-symref
                              *element->loser-hypoth-weight*
                              :priority :new]))) ))   ; MODIFY-LINK-WEIGHT

(defun  add-LOSER-tag (loser-hypothesis)
  (declare (type hypoth-agent loser-hypothesis))
  (pushnew :loser (get-filler loser-hypothesis :type)))


;;;;;;;  End of file AMBR/PROMOTN.LSP
