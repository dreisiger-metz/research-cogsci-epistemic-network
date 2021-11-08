;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/hypoth.lsp
;;; VERSION:    2.2.2  ; see AMBR/VERSION.LSP
;;; PURPOSE:    Hypothesis agents and their life cycle.
;;; DEPENDS-ON: DUAL; ambr/fizzle.lsp, ambr/corresp.lsp, ambr/secretar.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    10-06-97
;;; UPDATED:    10-03-98 [2.2.1]
;;; UPDATED:    09-05-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...
;;;;;;;;
;;; TO DO:      Periodically check whether secretary is still visible while
;;;             waiting for secretary answer(s) using an alarm clock.
;;; TO DO:      Handle corresp (and not hypoth) agents in secretary answers.


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;     H Y P O T H E S I S   A G E N T S     ;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR-CORE")

;;;; The key concept defined in this file is HYPOTH-AGENT -- a special kind
;;;; of correspondence agent (see AMBR/CORRESP.LSP) that takes part in a
;;;; constraint satisfaction network (see AMBR/CSNET.LSP).
;;;;
;;;; This file deals with the early stages of hypothesis' life -- sending
;;;; 'hypothesis registration requests' to secretaries and analyzing 'secretary
;;;; answers'.  Depending on the answer, an embryo hypothesis either 'resigns'
;;;; or 'establishes' as a mature hypothesis.


;;; The buffer (see AGENT-BUFFER in AMBR/AMBR_AG.LSP) is used to keep the
;;; first secretary-answer while waiting for the second one.
;;; It also stores (temporarily) marker-intersection reports (MP-X reports,
;;; see AMBR/MARKER.LSP and =/STR_CORR.LSP).
;;; See also AMBR/CSNET.LSP for methods for INHIB-INPUT-ZONE.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: ensure-hypothesis-elements, analyze-secretary-answers,
;;          resign-hypothesis, transfer-heritage, resign-or-fizzle,
;;          establish-hypothesis,
;;
;; The functions in this file depend on the following parameters from DEFS.LSP:
;;   *hypoth-agent-efficiency*, *contradict-hypoth-weight*
;;

;; ...

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric analyze-secretary-answers (embryo sinfo1 sinfo2)
  (:documentation "Decide whether to resign or to establish HYPOTHESIS." ))

(defgeneric resign-hypothesis (hypothesis favorite)
  (:documentation "Destroy HYPOTHESIS to clear the way for FAVORITE." ))

(defgeneric establish-hypothesis (hypothesis competitors)
  (:documentation "Inhibit COMPETITORS and trigger structure-correspondence." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definitions
;;;;;;   Type predicates
;;;;;;   Printing methods

;; These are defined in AMBR/AMBR_AG.LSP together with all other classes.
;;
;; The relevant classes and type predicates are:
;;   SECRETARY-AGENT, CORR-AGENT, TEMP-CORR-AGENT, DEAD-CORR-AGENT ;
;;   HYPOTH-AGENT, DEAD-HYPOTH-AGENT,
;;   EMBRYO-HYPOTH-AGENT, MATURE-HYPOTH-AGENT, WINNER-HYPOTH-AGENT ;
;;   AGENT-TYPE, DEAD-AGENT-P


;;;;;;  Hypothesis-agents, being specialized, have greater efficiency.

(defmethod agent-efficiency ((hypothesis hypoth-agent))
  (declare (ignore hypothesis))
  *hypothesis-agent-efficiency* )            ; cf. DUAL/ARCHIT/SYMPROC1.LSP



;;;;;;  ******  I N Q U I R I N G   S E C R E T A R I E S   *****
;;;
;;;  The consistency of the constraint-satisfaction network (CSN) is
;;;  maintained with the assistance of the secretary agents outside the CSN.
;;;  See AMBR/SECRETAR.LSP for details on the secretary agents.
;;;
;;;  In this section of the program we will use the term 'secretary' to
;;;  denote that portion of an agent (concept or instance) that 'knows'
;;;  the list of competing hypotheses involving the agent in question.
;;;  For instance, suppose that there are two hypotheses: #$water<-->milk  and
;;;    #$water<-->tea.  Then, the :HYPOTH slot of the agent  #$water carries
;;;    (indirect) pointers to #$tea and #$milk, thus capturing the fact that
;;;    the system is considering two possible (and competing) analogs for
;;;    the concept 'water'.
;;;
;;;  The secretary mechanism serves two purposes:
;;;    -- Establishing excitatory links from conceptual agents to hypotheses.
;;;    -- Establishing inhibitory links between competing hypotheses, thus
;;;         implementing the one-to-one restriction (or pressure).
;;;
;;;  Each hypothesis agent begins its life as an 'embryo'. During this first
;;;  and critical phase of its life, the embryo analyzes the secretary
;;;  information stored in its two 'elements' (see CORRESP-ELT in AMBR/CORRESP).
;;;  By doing this, it seeks answers to the following questions:
;;;    -- Am I a duplicate of an existing hypothesis?   ; "To be or not to be?"
;;;    -- Who are my rivals?             ; "If I be, whom should I fight with?"
;;;
;;;  If the answer to the first question is 'yes', the embryo hypothesis agent
;;;  resigns in favor of the mature one by 'fizzling out' (see below).
;;;  If the answer to the first (Hamletian) question is 'no', then the new
;;;  hypothesis has a _raison_d'etre_. It does not fizzle ; on the contrary,
;;;  it establishes itself, thus becoming a 'mature' hypothesis (see below).
;;;
;;;  (The transition embryo->mature is the first metamorphosis of the life
;;;   cycle of a hypothesis-agent.  The second transition is from mature to
;;;   'winner' hypothesis -- see AMBR/PROMOTN.LSP and AMBR/RATING.LSP)
;;;

;; The first period of the 'life cycle' of a hypothesis agent begins with
;; the receipt of a :JUST-CREATED event. During this period, the embryo
;; gathers information needed to face the Hamletian question.
;; The information is taken from the 'secretaries' according to the following
;; algorithm:
;;  1a. Check whether both hypothesis elements (call them ELT1 and ELT2)
;;      are well defined (cf. CORRESP-ELT in AMBR/CORRESP.LSP). If either of
;;      them isn't, fizzle out.
;;      (If an element happens to be a temporary agent who has (just) died
;;       then CORRESP-ELT will return NIL because links to dead agents are
;;       destroyed automatically.)
;;  1b. Check whether ELT1 and ELT2 are visible. If either of them isn't, then
;;      fizzle out.
;;  2. Check for the special case of 'trivial' hypotheses. Namely, check
;;     whether ELT1 is the same (EQ) as ELT2  (like in water<-->water).
;;     If this is the case, double the weight of the links to ELT1 (=ELT2) in
;;     order to compensate for the normalization of the weights. (In other
;;     words, there will be a single link with weight 2 instead of two links
;;     with weights 1.)
;;  3. Issue hypothesis-registration requests to secretaries (see SECRETAR.LSP)
;;     (Note that two only one HR-request is sent for trivial hypotheses.)
;;  4. Wait for the answer(s).
;;
;; TO DO: Periodically check whether secretr. are still visible. Fizzle if not.

(defmethod trigger-symbolic-processor ((host embryo-hypoth-agent)
                                       (event (eql :just-created)) )
  (declare (values S-PROGN-return-type))
  (unless (active-processor-p host)
    (symbolic-microcycle host))    ; Prepare to read the input zone for step 4.
  (let ((driver-elt    (corresp-elt host *driver-elt-label*))
        (recipient-elt (corresp-elt host *recipient-elt-label*)) )
   (s-progn host
     [ensure-hypothesis-elements host]                                 ; step 1
     (cond ([trivial-corresp-p host]                                   ; step 2
              (double-trivial-weights host)                            ; step 2
              [send-HR-request driver-elt host :trivial] )             ; step 3
           (t ; non-trivial hypothesis -- send two HR-requests
              [send-HR-request driver-elt host *recipient-elt-label*]  ; step 3
              [send-HR-request recipient-elt host *driver-elt-label*]) ; step 3
     )))) ;; HANDLE-SYMBOL will take care of the secretary answers.    ; step 4


(defun ensure-hypothesis-elements (hypothesis)
  "Check once again that both elements are still visible. If not, fizzle."
  (declare (type hypoth-agent hypothesis))
  (let ((elt1 (corresp-elt hypothesis *driver-elt-label*))
        (elt2 (corresp-elt hypothesis *recipient-elt-label*)) )
    (when (or (null elt1) (not (agent-visible-p elt1))
              (null elt2) (not (agent-visible-p elt2)) )
      (fizzle-agent hypothesis) )))


;; The 'secretary answer' (see AMBR/SECRETAR.LSP) is a symbolic structure
;; that secretaries produce in response to a hypothesis-registration request.
;; It has a datum field which carries the answer proper.
;;
;; As hypotheses usually involve two elements (and hence two secretaries),
;; there are two secretary answers. These two answers must be analyzed together
;; but they arrive at the input zone one by one. Therefore, the first one is
;; buffered until the second one arrives too. When both answers are gathered,
;; they (more precisely their data) are passed to ANALYZE-SECRETARY-ANSWERS.
;;
;; In the special case for 'trivial' hypotheses there is only one secretary
;; (because the two elements are the same). In this case, the first secretary
;; answer is also the last one. It is not buffered but instead two copies of
;; it are passed to ANALYZE-SECRETARY-ANSWERS. (Thus, the answer analyser need
;; not treat trivial hypotheses in a special way.)
;;
;; TO DO: Periodically check whether secretr. are still visible. Fizzle if not.

(defmethod handle-symbol ((host embryo-hypoth-agent)
                          (answer secretary-answer) )
  (declare (values S-PROGN-return-type))
  (let* ((buffer-contents (agent-buffer host))
         (first-answer (find-if #'secretary-answer-p buffer-contents)) )
    (s-progn host
      [ensure-hypothesis-elements host]  ; are both elements still visible?
      (s-eval *default-AMBR-INTERNAL-consumption* nil) ; for reading buffer etc.
      (cond ((trivial-corresp-p host)
                 [analyze-secretary-answers host         ; only one answer?
                          (secretary-answer answer)      ; pass 2 copies of it
                          (secretary-answer answer)])
            ((null first-answer)                         ; is this the 1st one?
                 (push answer (agent-buffer host)))      ; wait for the 2nd
            (t   (setf (agent-buffer host)               ; this is the 2nd
                       (delete-if #'secretary-answer-p   ; purge 1st from buff
                                  buffer-contents))
                 [analyze-secretary-answers  host
                          (secretary-answer first-answer)    ; unpack data
                          (secretary-answer       answer)]) ))))



;; ANALYZE-SECRETARY-ANSWERS decides whether the embryo will establish or
;; resign. It should resign if it is a duplicate of another hypothesis.
;;
;; Secretary answers can be the following:
;;   * NIL  -- 'You are the sole hypothesis, establish.'
;;   * list -- 'You represent a new correspondence, establish and fight
;;              the rivals from the list."
;;   * correspondence agent -- <This case is not well defined in curr.version>
;;   * winner hypothesis    -- 'You come too late, resign.'
;;   * mature hypothesis    -- 'You are a duplicate, resign.'
;;   * embryo hypothesis    -- 'One of U should establish, the other -- resign.'
;;
;; If we abstract away the details, answers are only of two kinds:
;;   ESTABLISH -- denoted by a (possibly empty) list of competing hypotheses
;;   RESIGN    -- denoted by an agent (the 'favorite')
;;
;; When the two secretary answers agree, do the corresponding action:
;;   ESTABLISH -- proceed with ESTABLISH-HYPOTHESIS
;;   RESIGN    -- proceed with RESIGN-OR-FIZZLE
;;
;; When the two secretary answers contradict, proceed with ANALYZE-AMBIGUOUS-
;;                                                                     ANSWERS
;; TO DO: Handle correspondence (and not hypoth) agents in secretary answers.

(eval-when (compile load eval)
  (proclaim-consumption 'analyze-ambiguous-answers :explicit-S-PROGN) )

(defmethod analyze-secretary-answers ((embryo embryo-hypoth-agent)
                                       answer1 answer2 )
  ;; Determine whether EMBRYO should establish or resign.
  (declare (type (or list corr-agent) answer1 answer2)
           (values S-PROGN-return-type) )
  (s-progn embryo
    [ensure-hypothesis-elements embryo]  ; are both elements still visible?
    (s-eval *default-AMBR-INTERNAL-consumption* nil)  ; pay for COND etc.
    (cond ((and (listp answer1) (listp answer2))
                [establish-hypothesis embryo (union1 answer1 answer2)])
          ((and (agentp answer1) (agentp answer2))
                [resign-or-fizzle embryo answer1 answer2])
          ((and (agentp answer1) (listp answer2))
                [analyze-ambiguous-answers embryo answer1 answer2])
          ((and (listp answer1) (agentp answer2))
                [analyze-ambiguous-answers embryo answer2 answer1]) )))

(defmethod analyze-secretary-answers ((x t) a1 a2)
  (declare (ignore a1 a2))
  (error "ANALYZE-SECRETARY-ANSWERS: ~S is not an embryo hypothesis." x ))


;; On rare (anomalous) occasions, the two secretaries' answers contradict.
;; That is, one answer is ESTABLISH and the other -- RESIGN. In this case,
;; consider the RESIGN answer more closely.
;; If it is a corresp. agent or a winner hypothesis, then check whether it
;;  satisfies EQUIVALENT-CORRESP-P with the current embryo and act accordingly.
;; If it is a mature hypothesis then resign in its favor.
;; Otherwise it is an embryo. Disambiguate on the basis of activation levels.
;; (See the remarks on the function CHECK-FOR-DUPLICATES in AMBR/SECRETAR.LSP.)
;;
;; (UNION1 is a 'stable' implementation of UNION -- see DUAL/GENERAL.LSP.)

(defun analyze-ambiguous-answers (embryo R-answer E-answer)
  "R-ANS says to resign, E-ANS says to establish."
   (declare (type embryo-hypoth-agent embryo)
            (type corr-agent R-answer)
            (type list E-answer) )
   (s-progn embryo
     (cond ((or (agent-type R-answer :corr :hypoth)       ; corr-but-not-hypoth
                (agent-type R-answer '(:hypoth :winner))) ; or winner-hypoth
              (if [equivalent-corresp-p embryo R-answer]
                  [resign-hypothesis embryo R-answer]     ; relevant justif.
                  [fizzle-agent embryo] ))                ; irrelevant justif.
           ((agent-type R-answer '(:hypoth :mature))      ; mature-hypoth
              [resign-hypothesis embryo R-answer])
           ;; else both are embryos -- disambiguate using activation levels
           ((< (agent-activation embryo) [agent-activation R-answer])
              [resign-hypothesis embryo R-answer])        ; more active embryo
           (t [establish-hypothesis embryo E-answer]) ))) ; other should resign


;; On negative secretary answers, the embryo either resigns or fizzles.
;; It resigns when it is a duplicate of an older hypothesis -- a 'favorite'.
;; It fizzles when there is a winner-hypothesis (see AMBR/PROMOTN.LSP) which
;;   does not satisfy EQUIVALENT-CORRESP-T with the embryo.

(defun resign-or-fizzle (embryo answer1 answer2)
  (declare (type corr-agent answer1 answer2)
           (values S-PROGN-return-type) )
  (s-progn embryo
     (let ((favorite [choose-favorite answer1 answer2]))
       (if (or (agent-type favorite :embryo)
               (agent-type favorite :mature))
           [resign-hypothesis embryo favorite]
           ;; else FAVORITE is a winner-hypoth or a corr-agent
           (if [equivalent-corresp-p embryo favorite]
               [resign-hypothesis embryo favorite]     ; relevant justification
               [fizzle-agent embryo] )))))             ; irrelevant justif.


;; On rare occasions the two secretaries may have different favorites.
;; Disambiguate according to the following rules:
;;   R1. Correspondence agents always take precedence over hypothesis agents.
;;   R2. Winner hypotheses always take precedence over mature hypotheses.
;;   R3. Mature hypotheses always take precedence over embryo hypotheses.
;;   R4. Embryo hypotheses always take precedence over dead hypotheses.
;;   R5. Ceteris paribus, the favorite with higher activation level wins.
;;
;; TO DO: Keep rule R1 in tune with the (putative) corr-but-not-hypoth protocol.

(defun choose-favorite (f1 f2)
  "Choose in favor of whom to resign."
  (declare (type corr-agent f1 f2))
  (labels ((corr-but-not-hypoth-p (agent) (agent-type agent :corr :hypoth))
           (winner-hypoth-p (agent) (agent-type agent '(:hypoth :winner)))
           (mature-hypoth-p (agent) (agent-type agent '(:hypoth :mature)))
           (embryo-hypoth-p (agent) (agent-type agent '(:hypoth :embryo)))
           (ceteris-paribus ()
             (or (and (corr-but-not-hypoth-p f1) (corr-but-not-hypoth-p f2))
                 (and (winner-hypoth-p f1) (winner-hypoth-p f2))
                 (and (mature-hypoth-p f1) (mature-hypoth-p f2))
                 (and (embryo-hypoth-p f1) (embryo-hypoth-p f2))
                 (and (dead-agent-p    f1) (dead-agent-p    f2)) ))
           (return-more-active ()
             (if (> (agent-activation f1) (agent-activation f2))
                 f1
                 f2 )))
    (cond ((ceteris-paribus)           (return-more-active))   ; rule R5
          ((corr-but-not-hypoth-p f1)  f1)                     ; rule R1
          ((corr-but-not-hypoth-p f2)  f2)                     ; rule R1
          ((winner-hypoth-p       f1)  f1)                     ; rule R2
          ((winner-hypoth-p       f2)  f2)                     ; rule R2
          ((mature-hypoth-p       f1)  f1)                     ; rule R3
          ((mature-hypoth-p       f2)  f2)                     ; rule R3
          ((embryo-hypoth-p       f1)  f1)                     ; rule R4
          ((embryo-hypoth-p       f2)  f2)                     ; rule R4
          (t (error "AMBRC::CHOOSE-FAVORITE: Bad argument(s): ~S and ~S."
             f1 f2))  )))



;;;;;;  ******  R E S I G N I N G   A   H Y P O T H E S I S  *****
;;;
;;;  Resigning occurs when the analysis of secretary information reveals that
;;;  there already is another hypothesis agent that has the same elements.
;;;  (That is, the two hypotheses satisfy EQUIVALENT-CORRESP-P.)
;;;  In this case, the new hypothesis hands over its 'justification' to
;;;  the older one. At the end of the transaction, instead of two hypotheses
;;;  with one justification each, there is one hypothesis with two justificatns.
;;;
;;;  The driver situation (see AMBR/CORRESP.LSP) is not handed over because
;;;  the favorite should have the same driver.
;;;
;;;  All information accumulated so far in the resigning hypothesis is handed
;;;  over to the 'favorite'.  This is the so called 'heritage'.  It includes
;;;  the T-LINKs and the symbols from the input zone and/or buffer.

;;  Resigning is done according to the following algorithm.
;;    1. Check once again whether both elements are still visible.
;;    2. Check whether FAVORITE is still visible. If it isn't (i.e. it
;;       has died in the interim) then fizzle out.
;;    3. Transfer all heritage from the dying agent to FAVORITE by calling
;;       the function TRANSFER-HERITAGE (see below).
;;    4. Having handed over everything, fizzle out (see AMBR/FIZZLE.LSP).

(defmethod resign-hypothesis ((resigning-hypothesis hypoth-agent)
                              (favorite corr-agent))
  ;; RESIGNING-HYPOTHESIS resigns in favor of FAVORITE.
  (declare (values S-PROGN-return-type))
  #+:DUAL-DEBUG
     (unless (or (dead-agent-p favorite)
                 (equivalent-corresp-p resigning-hypothesis favorite))
       (cerror "Do _not_ resign and continue."
           "RESIGN-HYPOTHESIS: ~S and ~S are not equivalent hypotheses."
           resigning-hypothesis favorite)
       (return-from resign-hypothesis nil) )
  (s-progn resigning-hypothesis
    [ensure-hypothesis-elements resigning-hypothesis]                  ; step 1
    (when (not [agent-visible-p favorite])                             ; step 2
      (fizzle-agent resigning-hypothesis))
    [transfer-heritage resigning-hypothesis favorite]                  ; step 3
    (fizzle-agent resigning-hypothesis) ))                             ; step 4


(defmethod resign-hypothesis ((x t) (y t))
  (error "RESIGN-HYPOTHESIS: ~S is not a hypothesis agent or ~
          ~S is not a correspondence agent."  x y  ))


;;  'Transferring heritage' is a process in which a temporary correspondence
;;  agent (usually hypothesis-agent) hands over all its useful information
;;  to another correspondence agent.  The former is called RESIGNING-CORRESP
;;  and the latter is called FAVORITE (also 'heir').
;;  The actual transfer is done according to the following algorithm.
;;    1. Create an excitatory link from MENTOR to FAVORITE by sending a write
;;       request to MENTOR. (Cf. FORMULATE-MP-NCR in AMBR/MARKPASS.LSP.)
;;       (Note that no write requests are sent to the elements because the
;;        favorite already receives support from them.)
;;    2. Handle over the justification of RESIGNING-CORRESP to FAVORITE by
;;       sending a write request to the latter.
;;    3. Send to FAVORITE all 'heritable' symb. structs (see HERITABLE-SYMBOL-P)
;;       accumulated in the input zone and the buffer of RESIGNING-CORRESP.
;;       (See comments below for comments on a bug in an older version.)
;;    4. Hand over the contents of the :T-LINK slot to FAVORITE.
;;       (Note: this slot will be empty in embryo hypotheses.)

(eval-when (compile load eval)
  (proclaim-consumption 'transfer-heritage-justifications :explicit-S-PROGN )
  (proclaim-consumption 'transfer-heritage-symbols        :explicit-S-PROGN )
  (proclaim-consumption 'transfer-heritage-T-LINKs        :explicit-S-PROGN )
)
(defun  transfer-heritage (resigning-corresp favorite)
  (declare (type temp-corr-agent resigning-corresp)
           (type corr-agent favorite)
           (values S-PROGN-return-type))
  (s-progn resigning-corresp
    [transfer-heritage-justifications resigning-corresp favorite]    ; step 1+2.
    [transfer-heritage-symbols        resigning-corresp favorite]    ; step 3.
    [transfer-heritage-T-LINKs        resigning-corresp favorite] )) ; step 4.


(defun  transfer-heritage-justifications (resigning-corresp favorite)
  (declare (values S-PROGN-return-type))
  (let ((mentor-conrefs (corresp-justif resigning-corresp NIL)))
   (case (length mentor-conrefs)                       ; ^^^--symref-only-p
     (0 nil)    ; no justifications at all (which is anomalous)
     (1         ; single justif -- the typical case for embryo-hypotheses
        (s-progn resigning-corresp
          (let ((mentor (conref-reference (first mentor-conrefs)))
                (weight (conref-weight    (first mentor-conrefs))) )
            [redirect-mentor-support resigning-corresp mentor favorite]
            [send-write-request favorite 'add-corresp-justif mentor weight])))
     (t         ; many justifications
        (s-progn resigning-corresp
          (dolist (mentor-conref mentor-conrefs)
            (let ((mentor (conref-reference mentor-conref))
                  (weight (conref-weight    mentor-conref)) )
              [redirect-mentor-support resigning-corresp mentor favorite]  ; 1.
              [send-write-request favorite 'add-corresp-justif mentor      ; 2.
                                                               weight])))) )))

(defun  transfer-heritage-symbols (resigning-corresp favorite)        ; step 3.
  (declare (values S-PROGN-return-type))
  (s-progn resigning-corresp
    (let* ((input-symbols  (agent-input-zone resigning-corresp))
           (buffer-symbols [agent-buffer resigning-corresp])
           (all-symbols    (union1 input-symbols buffer-symbols))
           (heritable-symbols (remove-if (complement #'heritable-symbol-p)
                                         all-symbols)) )
      (dolist (symbol heritable-symbols)
        [receive-symbol favorite symbol])) ))                         ; step 3.

(defun  transfer-heritage-T-LINKs (resigning-corresp favorite)        ; step 4.
  (declare (values S-PROGN-return-type))
  (when (locate-mfr-component resigning-corresp :t-link)
    (s-progn resigning-corresp
       (dolist (conref (get-filler resigning-corresp :t-link))
         (let ((symref (conref-reference conref))
               (weight (conref-weight    conref)))
           [send-write-request favorite 'add-link :t-link
                                             symref weight :priority :old]
           (when (minusp weight)          ; negative links are always symmetric
             #+:DUAL-DEBUG
               (assert (simple-symref-p symref))    ; a rival hypothesis
             [send-write-request symref 'add-link :t-link
                                           favorite *contradict-hypoth-weight*
                                           :priority :old]))) )))


(defun redirect-mentor-support (resigning-corresp mentor favorite)
  "Send a write request to MENTOR to support FAVORITE instead of RES-HYP."
  (declare (type temp-corr-agent resigning-corresp)
           (type AMBR-agent mentor)
           (type corr-agent favorite) )
  (let ((mentor-slot (locate-mfr-component mentor :t-link)))
    (unless (null mentor-slot)
      (let ((old-conref (find resigning-corresp (get-filler mentor-slot)
                              :test #'symref-match  :key #'conref-reference)))
        (unless (null old-conref)
          (send-write-request mentor 'add-link :t-link favorite
                                               (conref-weight old-conref)
                                               :priority #'max) )))))


;; HERITABLE-SYMBOL-P controls what kinds of symbolic structures are transferred
;; from RESIGNING-HYPOTHESIS to FAVORITE during step 3. of TRANSFER-HERITAGE.
;;
;; At present, the only type of symbolic structures considered 'heritable' is
;; the marker-intersection report (MP-X).  Such reports are re-sent to FAVORITE
;; because they may help it during the top-down structure-correspondence process
;; (see AMBR/STR_CORR.LSP).
;;
;; Fizzle messages are not transferred to FAVORITE, of course.
;; Write requests are not transferred either because they could lead to
;; self-referential links in FAVORITE's microframe.
;;
;; No other kinds of symb. structs should ever be fed to HERITABLE-SYMBOL-P.

(defun  heritable-symbol-p (symbol)
  "Used by TRANSFER-HERITAGE to judge which symbolic structures to transfer."
  (declare (type symbolic-structure symbol)
           (values boolean) )
  (typecase symbol
    (marker-intersection-report  T)
    (write-request             nil)
    (fizzle-message            nil)
    (t (warn "Don't know whether ~S is 'heritable' or not. Assuming it isn't."
             symbol)
       nil) ))
;;
;; Note, however, that I once spent a whole day (June 2, 1998) debugging the
;; model at this point.  The whole story is quite instructive from a methodo-
;; logical point of view and I'll tell it briefly here.  The old version of
;; (ver. 2.2.1) TRANSFER-HERITAGE used the following specification for step 3:
 ;;  3. Send to FAVORITE all symbolic structures accumulated in the input zone
 ;;     (but _not_ the buffer) of RESIGNING-CORRESP. In particular, there
 ;;     may be marker-intersection reports that will facilitate FAVORITE during
 ;;     the structure-correspondence process. There may also be write-requests
 ;;     requests sent to RESIGNING-CORRESP by other (resigning) hypotheses.
;; This algorithm led to the following buggy behavior:
;; Suppose there are two duplicate hypotheses -- F and H.  F is the favorite
;; and hence the secretary sends to H a secretary answer of type 'resign'.
;; H enters TRANSFER-HERITAGE but suspends the operation due to lack of energy.
;; Meanwhile the rating mechanism (see AMBR/RATING.LSP) promotes F as winner.
;; The secretary sends fizzle-messages to many 'losers' on its secretary list,
;; H included (see ELIMINATE-LOSERS-AFTER-PROMOTION in AMBR/PROMOTN.LSP).
;; That is how a fizzle message appears in the input zone of H, which is about
;; to resign (i.e. fizzle out) anyway.  Unfortunately, though, this happens
;; exactly at the moment when H resumes  working on TRANSFER-HERITAGE.  It
;; indiscriminatively hands over to F _all_ symbolic structures from its
;; input zone.  The net result is that the just-promoted favorite F receives a
;; (misguided) fizzle message and dies.
;;
;; How did this bug appear?
;;   The mechanisms for RESIGN-HYPOTHESIS were designed and implemented long
;;   ago.  At the time, the repertoire of symbolic structures sent to hypothesis
;;   agents was qute limited and it was safe to transfer everything.  So the
;;   method for TRANSFER-HERITAGE was tested OK and worked well for thousands
;;   of runs.  Later on, however, long after the details and the implicit
;;   assumptions of RESIGN-HYPOTHESIS were taken for granted and forgotten,
;;   I added other mechanisms to the model -- rating, promotion, skolemization,
;;   etc.  These mechanisms introduced new kinds of messages and in particular
;;   fizzle-messages. The new messages violated the old assumptions, thus
;;   leading to bugs.
;; Why was this bug hard to cope with?
;;   The bug depended on very rare coincidences and thus the program worked
;;   fine most of the time.  It appeared while I was testing an entirely
;;   different (and unrelated) mechanism.  Worse yet, the new mechanism
;;   involved sending fizzle messages to hypothesis agents.  Therefore, when
;;   the trace showed that  '#<FZ xxx> received by hypothesis zzz', the obvious
;;   place to look for the bug was in the new pieces of code having nothing
;;   to do with it.  Another complication which proved to be _very_ misguiding
;;   was that the FIZZLE-MESSAGE-SENDER (see AMBR/FIZZLE.LSP) of the 'fatal'
;;   message did not point to the agent that has really sent it (due to the
;;   re-sending done by TRANSFER-HERITAGE).
;; What is the moral of the story?
;;   - Always keep in mind that each new mechanism changes the timing of many
;;     operations and thus may give rise to coincidences that have never
;;     happened during previous testing.  Therefore, the new bug may actually
;;     be a low-frequency old bug.  This is very unfortunate because it means
;;     that one must always test the whole model, not only the new mechanisms.
;;   - Undocumented assumptions fire back.  However, even if I have documented
;;     the assumption about TRANSFER-HERITAGE, I still doubt whether this piece
;;     of documentation (burried in the file OLD&DULL.LSP) would influence
;;     the design of the file NEWFANCY.LSP.  Therefore, it could be said that
;;     even documented assumptions fire back and must be buttressed by ASSERT
;;     clauses and the like.
;;   - Re-sending messages is generally a bad idea.  Each message carries
;;     implicit assumptions about its receiver.  These assumptions may have
;;     been relied upon (and may be been checked) by the sender of the message.
;;     Re-sending the message actually means changing the receiver, which might
;;     not be what the original sender meant.




;;;;;;  ******  E S T A B L I S H I N G   A   H Y P O T H E S I S  *****
;;;
;;;  Secretary checks are over and have been passed successfully. The embryo
;;;  hypothesis has the right to exist (raison d'etre). It now enters the
;;;  mature period of its 'life'. Its existence from now is devoted to two
;;;  major goals:
;;;   -- Suppress rival hypotheses in the constraint satisfaction network.
;;;   -- Sprout out children.
;;;  The first goal is pursued by making up inhibitory links.
;;;  The second goal is pursued by going through the structure-correspondence
;;;  process (see AMBR/STR_CORR.LSP).
;;;
;;;  This portion of the file deals with the first goal -- making up inhibitory
;;;  links to suppress competitors. Information about who exactly are the
;;;  competitors comes from the analysis of secretary information. This analysis
;;;  can be: (i) performed by the hypothesis itself (see ANALYZE-SECRETARY-
;;;  ANSWERS above) or (ii) received in the input zone by another (resigned)
;;;  hypothesis. Case (i) occurs once - immediately after inquiring secretaries;
;;;  case (ii) is continous -- write requests may appear in the input zone at
;;;  any time. The former is implemented below, the latter is implemented by the
;;;  general mechanism for handling write-requests (see DUAL/ARCHIT/SYMBOLIC).
;;;
;;;  All inhibitory links are symmetric. Fan-in links are created by sending
;;;  write requests to the agent at the opposite side.
;;;
;;;  Competitors are ordered according to their activation levels.  The main
;;;  (i.e. most active) rival is handled first.  The activation level of the
;;;  establishing hypothesis cannot exceed that of the main rival.  This is
;;;  to eliminate artifacts caused by the unduly high embryo activation levels.
;;;  (In earlier versions of the model [prior to ver. 2.2.2], later hypotheses
;;;   tended to beat earlier ones.)
;;;
;;;  Secretary-answers always carry freshly consed lists (see AMBR/SECRETAR.LSP)
;;;  Thus it is safe to apply destructive functions such as DELETE-IF and SORT.

(defmethod establish-hypothesis ((hypothesis embryo-hypoth-agent)
                                 (competitors list) )
  "Create inhibitory links to COMPETITORS and then trigger structure-corresp."
  (declare (values S-PROGN-return-type))
  (let* ((active-competitors (delete-if (complement #'agent-visible-p)
                                        competitors))
         (sorted-competitors (sort active-competitors #'>
                                                 :key #'agent-activation)) )
    (s-progn hypothesis
      [ensure-hypothesis-elements hypothesis]  ; both elements still visible?
      (dolist (competitor sorted-competitors)  ; fan-in
        ;; Links added here, moved by TRANSFER-HERITAGE-AUX, deleted autom.
        [send-write-request competitor 'add-link :t-link
                                       hypothesis *contradict-hypoth-weight*])
      (dolist (competitor sorted-competitors)  ; fan-out
        [add-link hypothesis :t-link competitor *contradict-hypoth-weight*])
      (adjust-embryo-activation-level hypothesis
                                      sorted-competitors)
      [embryo->mature-hypothesis hypothesis]
      [structure-correspondence hypothesis] )))

(defmethod establish-hypothesis ((x t) (y t))
  (error "ESTABLISH-HYPOTHESIS: ~S is not an embryo hypothesis agent or ~
          ~S is not a list (of competitors)."  x y ))

(defun adjust-embryo-activation-level (embryo sorted-competitors)
  "Avoid artifacts caused by unduly high embryo activation levels."
  (declare (type hypoth-agent embryo)               ; maturing-embryo or dead
           (type list sorted-competitors) )
  (let ((main-rival (find-if #'(lambda (hyp)
                                 (and (not (dead-agent-p hyp))
                                      (not (agent-type hyp :embryo))))
                             sorted-competitors)))
    ;; MAIN-RIVAL is the most active living non-embryo competing hypothesis.
    (if (null main-rival)
        nil                ; don't change EMBRYO's activation level
        (let ((upper-bound (agent-activation main-rival))
              (lower-bound *hypoth-zero-act*)
              (embryo-act  (agent-activation embryo)) )
          (setf (agent-activation embryo)
                (min upper-bound
                     (max lower-bound embryo-act)))) )))

(defun embryo->mature-hypothesis (embryo)
  "Transform an embryo hypothesis into a mature hypothesis."
  (declare (type hypoth-agent embryo)    ; embryo or dead
           (values hypoth-agent) )       ; mature or dead
  (ensure-hypothesis-elements embryo)
  (unless (dead-agent-p embryo)
    (let ((TYPE-slot (locate-mfr-component embryo :type)))
      (remove-filler-elt TYPE-slot :embryo  nil)  ; avoid notifying twice
      (add-filler-elt    TYPE-slot :mature  :notify-p T) )
    (change-class embryo 'mature-hypoth-agent) )
   embryo)

;; Compare with MATURE->WINNER-HYPOTHESIS defined in AMBR/PROMOTN.LSP

;;;;;;;  End of file AMBR/HYPOTH.LSP
