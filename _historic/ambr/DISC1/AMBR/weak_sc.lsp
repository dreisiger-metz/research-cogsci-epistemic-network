;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/weak_SC.lsp    ; see also AMBR/str_corr.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    'Weak' structure correspondence (or 'SC-2')
;;; DEPENDS-ON: DUAL; ambr/secretar.lsp, ambr/hypoth.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    14-06-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...

         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;    'WEAK' STRUCTURE CORRESPONDENCE    ;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR-CORE")

;;;; The key concept defined in this file is WEAK STRUCTURE CORRESPONDENCE
;;;; (or SC-2 for short).  This file is an extension of AMBR/STR_CORR.LSP.
;;;;
;;;; In many cases it is bad to allow the SC mechanism create new hypotheses
;;;; but it is desirable to allow it create additional justification links
;;;; between existing hypotheses.  This is the purpose of the weak SC.
;;;;
;;;; For example, suppose that two situations -- SIT-WTP and SIT-MTF -- are
;;;; being mapped.  Suppose further that INITST-WTP and INITST-MTF represent
;;;; their respective initial states and that the marker-passing mechanism
;;;; (see AMBR/MAKER.LSP) has established the hypoth INITST-WTP<-->INITST-MTF.
;;;; Each of the two states has many S-slots pointing to the elements of the
;;;; respective situation and the initial relations between them.  Thus, the
;;;; two states resemble propositions of type AND and, therefore, one could
;;;; wish to generate SC-motivated hypotheses about the arguments of these
;;;; AND-like propositions.  Applying the usual structure-correspondence
;;;; mechanism (see AMBR/STR_CORR.LSP) indiscriminatively, however, will lead
;;;; to proliferation of useless hypotheses such as WATER-WTP<==>LOW-TEMP-MTF.
;;;; For that reason, init-states (and all agents having the tag :SITUATION in
;;;; general) are exempted from the usual top-down structure correspondence.
;;;;
;;;; On the other hand, consider the hypothesis WATER-WTP<-->MILK-MTF.  It is
;;;; based on another justification (namely, both are liquids), not because
;;;; WATER-WTP belongs to INITST-WTP and MILK-MTF belongs to INITST-MTF.
;;;; Still, this hypothesis is consistent with INITST-WTP<-->INITST-MTF and
;;;; it is desirable to establish excitatory links between the two.  This
;;;; improves the connectivity of the constraint-satisfaction network (see
;;;; AMBR/CSNET.LSP) and strengthens the structural constraint on mapping.
;;;;
;;;; The main idea of the 'weak' structure correspondence is to find out
;;;; the consistent hypotheses (via FIND-ANY-HYPOTHESIS from AMBR/SECRETAR.LSP)
;;;; and to establish links directly.  (In contrast, 'strong' SC creates
;;;; new embryo-hypotheses which either establish or resign in favor of some
;;;; 'favorite' -- see AMBR/STR_CORR.LSP and =/HYPOTH.LSP.)
;;;;
;;;; There are two major types of SC-2, conventionally termed BOTTOM-UP SC-2
;;;; and TOP-DOWN SC-2 (compare with AMBR/STR_CORR.LSP).
;;;; TOP-DOWN SC-2 deals with situations, init-states, end-states, etc.  It
;;;; is named 'state-SC' for that reason.  It looks for hypotheses involving
;;;; the members of these states.
;;;; The example above presents a case of top-down SC-2 (or 'state SC').
;;;;
;;;; BOTTOM-UP SC-2 looks for hypotheses about the relations involving two
;;;; hypoth-elements. For example, suppose that TEAPOT-WTP is an argument to
;;;; the proposition ON-WTP and that TEAPOT-MTF is an arg to IN-MTF.  Suppose
;;;; further that there is a hypothesis TEAPOT-WTP<-->TEAPOT-MTF.  Then the
;;;; bottom-up SC-2 will establish a T-LINK with ON-WTP<-->IN-MTF provided such
;;;; hypothesis exists.  If such hyp doesn't exist, SC-2 does _not_ create it.
;;;; (Note that this is the reverse of the ordinary top-down SC-1 which     )
;;;; (constructs hyps about arguments based on hyps about propositions.     )
;;;; (Here, SC-2 looks for hyps about propositions based on hyps about args.)
;;;;
;;;; Throughout this file, the term 'wanted hypothesis' refers to the hypothesis
;;;; agent (if any) to which the weak SC mechanism attempts to create a link.
;;;;
;;;; The phantom-mediated structure-corr. mechanism may be blocked by setting
;;;; *WEAK-SC-FLAG* to NIL.  This flag defaults to T (see AMBR/DEFS.LSP).


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: weak-structure-correspondence, bottom-up-SC-2, top-down-SC-2 ;
;;
;;          SC-memo, make-SC-memo,
;;          SC-memo-driver-elt, SC-memo-recipient-elt, SC-memo-driver-sit,
;;          SC-memo-m2h-weight, SC-memo-h2m-weight, SC-memo-trials
;;
;; The functions in this file depend on the following parameters from DEFS.LSP:
;;   *hypoth->SC2-mentor-weight*, *SC2-mentor-homog-weight*, *SC2-mentor-
;;   heterog-weight*, *number-of-state-SC-trials*, and *state-SC-wait-period*.

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric bottom-up-SC-2 (hypothesis)
  (:documentation
  "If HYPOTHESIS is about arguments, look for hyps about the propositions." ))

(defgeneric top-down-SC-2 (hypothesis)
  (:documentation
  "If HYPOTHESIS is about situations, look for hypotheses about the members." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;  ***********   S C   M E M O S   ************
;;;
;;;  This portion of the file defines the class SC-MEMO and its periphernalia.
;;;
;;;  SC memos are symbolic structures describing a 'wanted' hypothesis.
;;;  The agent that does the SC sends the memos to itself and then handles
;;;  them via HANDLE-SYMBOL.  Thus the weak-SC routine breaks in two parts:
;;;   1. creation of SC memos  -- one for each 'wanted' hypothesis.
;;;   2. execution of SC memos -- the hypothesis described by the memo is
;;;        looked for and, if found, appropriate link(s) are established.
;;;
;;;  Sometimes one and the same memo is tried several times -- until the wanted
;;;  hypothesis shows up or the memo owner gives up.  SC memos wait wrapped in
;;;  alarm clocks between such trials.  See *STATE-SC-WAIT-PERIOD*, etc.
;;;
;;;  Each SC memo is a data structure with the following fields:
;;;   + DRIVER-ELT    -- the driver element of the wanted hyp (see CORRESP.LSP)
;;;   + RECIPIENT-ELT -- the recipient element thereof
;;;   + DRIVER-SIT    -- the driver situation of the wanted hyp
;;;   + M2H-WEIGHT    -- the weight of the MENTOR->HYPOTH link
;;;   + M2H-WEIGHT    -- the weight of the HYPOTH->MENTOR link (0.0 for none)
;;;   + TRIALS        -- if failed, how many more times to handle the memo?
;;;

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass SC-memo (symbolic-structure)
    ((driver-elt    :reader      SC-memo-driver-elt
                    :type        secretary-agent
                    :initarg     :driver-elt
                    :initform    (required-argument)  )
     (recipient-elt :reader      SC-memo-recipient-elt
                    :type        secretary-agent
                    :initarg     :recipient-elt
                    :initform    (required-argument)  )
     (driver-sit    :reader      SC-memo-driver-sit
                    :type        instance-agent       ; see AMBR/SITUATN.TXT
                    :initarg     :driver-sit
                    :initform    (required-argument)  )
     (m2h-weight    :accessor    SC-memo-m2h-weight   ; mentor->hypoth weight
                    :type        float
                    :initarg     :m2h-weight
                    :initform    0.0  )
     (h2m-weight    :accessor    SC-memo-h2m-weight   ; hypoth->mentor weight
                    :type        float
                    :initarg     :h2m-weight
                    :initform    0.0  )
     (trials        :accessor    SC-memo-trials
                    :type        integer
                    :initarg     :trials
                    :initform    0  )
    )
   (:documentation "Description of a hypoth 'wanted' by weak struct.corresp." ))
) ; eval-when


;;;;  Constructor

(defun make-SC-memo (driver-elt recipient-elt driver-sit
                     &key (m2h-weight 0.0) (h2m-weight 0.0) (trials 0) )
  (declare (type secretary-agent driver-elt recipient-elt)
           (type instance-agent driver-sit)
           (type number m2h-weight h2m-weight trials)
           (values SC-memo) )
  (make-instance 'SC-memo :driver-elt    driver-elt
                          :recipient-elt recipient-elt
                          :driver-sit    driver-sit
                          :m2h-weight    m2h-weight
                          :h2m-weight    h2m-weight
                          :trials        trials     ))

;;;;;;  Printing methods

(defmethod  print-object ((memo SC-memo) stream)
  (if (and (slot-boundp memo 'driver-elt)
           (slot-boundp memo 'recipient-elt)
           (typep (SC-memo-driver-elt    memo) 'secretary-agent)
           (typep (SC-memo-recipient-elt memo) 'secretary-agent) )
      (format stream "#<SCM ~A ~A>"
                     (agent-name (SC-memo-driver-elt    memo))
                     (agent-name (SC-memo-recipient-elt memo)) )
      (format stream "#<malformed SC-memo>") ))


(defmethod DUAL-describe ((memo SC-memo)
                          &optional (stream *standard-output*) )
  (format stream "~&~S is a ~A.~%"
                 memo (type-of memo) )
  (format stream "~&  Its driver elt is: ~S~%"
          (if (slot-boundp memo 'driver-elt)
              (SC-memo-driver-elt memo)
              "(unbound)"))
  (format stream "~&  Its recipient elt is: ~S~%"
          (if (slot-boundp memo 'recipient-elt)
              (SC-memo-recipient-elt memo)
              "(unbound)"))
  (format stream "~&  Its driver situation is: ~S~%"
          (if (slot-boundp memo 'driver-sit)
              (SC-memo-driver-sit memo)
              "(unbound)"))
  (format stream "~&  Its MENTOR->HYPOTH weight is: ~S~%"
          (if (slot-boundp memo 'm2h-weight)
              (SC-memo-m2h-weight memo)
              "(unbound)"))
  (format stream "~&  Its HYPOTH->MENTOR weight is: ~S~%"
          (if (slot-boundp memo 'h2m-weight)
              (SC-memo-h2m-weight memo)
              "(unbound)"))
  (format stream "~&  Its trial counter is: ~S~%"
          (if (slot-boundp memo 'trials)
              (SC-memo-trials memo)
              "(unbound)"))
  (values))


;;;;  Type-checking methods for the accessors

(defmethod  SC-memo-driver-elt ((x t))
  (error "SC-MEMO-DRIVER-ELT: ~S is not a SC-memo." x ))

(defmethod  SC-memo-recipient-elt ((x t))
  (error "SC-MEMO-RECIPIENT-ELT: ~S is not a SC-memo." x ))

(defmethod  SC-memo-driver-sit ((x t))
  (error "SC-MEMO-DRIVER-SIT: ~S is not a SC-memo." x ))

(defmethod  SC-memo-m2h-weight ((x t))
  (error "SC-MEMO-M2H-WEIGHT: ~S is not a SC-memo." x ))

(defmethod  (setf SC-memo-m2h-weight) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF SC-MEMO-M2H-WEIGHT): ~S is not a SC-memo." x ))

(defmethod  SC-memo-h2m-weight ((x t))
  (error "SC-MEMO-H2M-WEIGHT: ~S is not a SC-memo." x ))

(defmethod  (setf SC-memo-h2m-weight) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF SC-MEMO-H2M-WEIGHT): ~S is not a SC-memo." x ))

(defmethod  SC-memo-trials ((x t))
  (error "SC-MEMO-TRIALS: ~S is not a SC-memo." x ))

(defmethod  (setf SC-memo-trials) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF SC-MEMO-TRIALS): ~S is not a SC-memo." x ))



;;;;;;  ******  INITIALIZING THE WEAK STRUCTURE-CORRESPONDENCE  ******
;;;
;;;  This section deals with generation of SC-memos.  The hypothesis agent
;;;  then sends these memos to itself and handles them one by one.
;;;

;; WEAK-STRUCTURE-CORRESPONDENCE is the entry point to the SC-2 mechanism.
;; It is invoked by the function STRUCTURE-CORRESPONDENCE (see AMBR/STR_CORR).
;; *WEAK-SC-FLAG* (see AMBR/DEFS.LSP) may be used to block the SC-2 mechanism.

(defun weak-structure-correspondence (hypoth)
  "Sprout 'phantom' hypotheses to establish 'coherent' links b/n exising hyps."
  #+:DUAL-DEBUG (assert (corresp-type hypoth :INSTANCE))
  (when *weak-SC-flag*
    (s-progn hypoth
       (when (corresp-type hypoth :SITUATION)   ; both elts are states?
         [top-down-SC-2 hypoth])
       [bottom-up-SC-2 hypoth] )))

(defun send-SC-memo-to-self (host memo)  ; see consumption in AMBR/PROCLAIM.LSP
  (receive-symbol host memo) )


;;;;;;;;;;  TOP-DOWN SC-2
;;;
;;; Top-down SC takes place only when the 'parent' hypothesis involves two
;;; 'states'. The criterion is that the :TYPE slots of both elements
;;; contain the tags :INSTANCE and :SITUATION.
;;;
;;; E.g. Parent hyp:  initst-ICC<..>initst-MTF
;;;      Wanted hyps: ice-cube-ICC<-->teapot-MTF, in-ICC<..>in-MTF, etc.
;;;

;; TOP-DOWN-C-COREFS retrieves the agents stored in :C-COREF facets of any
;; S-slot of a given agent.  Only the 'simple symrefs' (i.e. bare agents) are
;; considered (see DUAL/ARCHIT/SYMREF.LSP).  The agents are divided into two
;; subsets -- 'aspects' and 'relations' -- depending on the TYPE facet of the
;; respective S-slot.  TOP-DOWN-C-COREFS returns a list of two lists.
;; For example, suppose that the agent INITST-ICC has the following S-slots:
;;  (defagent  initst-ICC                           ; see KB/EPISODIC/T_ICC.LSP
;;     :slot1  :type :aspect    :c-coref ice-cube-ICC
;;     :slot2  :type :relation  :c-coref in-ICC
;;     :slot3  :type :relation  :c-coref T-of-ICC   )
;;
;; Then  (top-down-C-COREFs initst-ICC)  -->  ((ice-cube-ICC)       ; aspects
;;                                             (in-ICC T-of-ICC) )  ; relations
;;
;; (Compare with PROPOSITION-ARGS defined in AMBR/KREPRES.LSP.)

(defun top-down-C-COREFs (agent)
  "List all agents contained in C-COREF facets of any S-slot of AGENT."
  (declare (values list))    ; of two lists of agents
  (let ((aspect-list nil)
        (relation-list nil) )
    (dolist (S-slot (agent-S-slots agent))            ; vvv-- first-only-p
      (let ((TYPE-tag       (get-filler S-slot :TYPE nil T))
            (C-COREF-symref (get-filler-refs! S-slot :C-COREF nil T)) )
        (when (simple-symref-p C-COREF-symref)
          (case type-tag
            (:aspect    (push C-COREF-symref aspect-list))
            (:relation  (push C-COREF-symref relation-list))
            (t          nil)) )))    ; :ACTION or some user-defined type
    (list (nreverse aspect-list)
          (nreverse relation-list)) ))


;; TOP-DOWN-SC-2 retrieves the top-down C-COREFs of each hypothesis element
;; and then formulates a SC-memo for each possible pair.
;; It should be applied only when both elements have the tag :SITUATION.
;;
;; The C-COREFs are divided in two subsets -- aspects and relations.  Thus,
;; four possible combinations are possible: asp-asp, rel-rel, asp-rel, rel-asp.
;; The first two combinations generate SC-memos with non-zero trial counter
;; and, therefore, they will be tried many times if the first attempt fails.
;; The other two combinations are tried too (they are not ruled out a priory)
;; but only once -- their SC-MEMO-TRIALS counter is set to 0.

(eval-when (compile load eval)
  (proclaim-consumption 'do-one-SC-memo
                        (+ (get-consumption 'make-SC-memo)
                           (get-consumption 'send-SC-memo-to-self)) )
  (proclaim-consumption 'top-down-SC-2-aux :explicit-S-PROGN )
) ; eval-when

(defmethod top-down-SC-2 ((hypoth mature-hypoth-agent))
  (declare (values S-PROGN-return-type))
  #+:DUAL-DEBUG (assert (corresp-type hypoth '(:INSTANCE :SITUATION)))
  (let ((driver-state    (corresp-elt hypoth *driver-elt-label*))
        (recipient-state (corresp-elt hypoth *recipient-elt-label*))
        (driver-sit    (corresp-driver hypoth)) )   ; see AMBR/CORRESP.LSP
    (labels ((do-one-SC-memo (driver recipient m2h-weight trials)
               (send-SC-memo-to-self hypoth
                  (make-SC-memo driver
                                recipient
                                driver-sit                               ; LET
                                :m2h-weight m2h-weight                   ;argmt
                                :h2m-weight *hypoth->SC2-mentor-weight*  ;param
                                :trials     trials)) )
             (top-down-SC-2-aux (dr-set rec-set m2h-weight trials)
               (s-progn hypoth
                  (dolist (dr-elt dr-set)
                    (dolist (rec-elt rec-set)
                      [do-one-SC-memo dr-elt rec-elt m2h-weight trials])))) )
      ;; Main body of TOP-DOWN-SC-2
      (s-progn hypoth
        [ensure-hypothesis-elements hypoth]   ; both elements still visible?
        (let* ((driver-C-COREFs      [top-down-C-COREFs driver-state])
               (driver-aspects       (first  driver-C-COREFs))
               (driver-relations     (second driver-C-COREFs))
               (recipient-C-COREFs   [top-down-C-COREFs recipient-state])
               (recipient-aspects    (first  recipient-C-COREFs))
               (recipient-relations  (second recipient-C-COREFs))
               (homogeneous-states-p [homogeneous-states-p driver-state
                                                           recipient-state])
               (mentor-weight (if homogeneous-states-p       ; don't check for
                                  *SC2-mentor-homog-weight*  ; each indiv. pair
                                  *SC2-mentor-heterog-weight*)) )
          [top-down-SC-2-aux driver-aspects recipient-aspects       ; asp-asp
                             mentor-weight  *number-of-state-SC-trials*]
          [top-down-SC-2-aux driver-relations recipient-relations   ; rel-rel
                             mentor-weight  *number-of-state-SC-trials*]
          [top-down-SC-2-aux driver-aspects recipient-relations     ; asp-rel
                             *SC2-mentor-heterog-weight*  0]
          [top-down-SC-2-aux driver-relations recipient-aspects     ; rel-asp
                             *SC2-mentor-heterog-weight*  0]  )))))

(defmethod top-down-SC-2 ((x t))
  (error "TOP-DOWN-SC-2: ~S is not a mature hypothesis." x ))


;;;;;;;;;;  BOTTOM-UP SC-2
;;;
;;; E.g. Parent hyp:  teapot-WTP<-->teapot-MTF  ==>
;;;      Wanted hyps: initst-WTP<-->initst-MTF, made-of-WTP<-->made-of-MTF, etc.
;;;
;;; Bottom-up SC-2 is the weakest SC possible -- it creates uni-directional
;;; links only (as opposed to the symmetric links in all other SCs).  That is,
;;; a T-LINK is established from the parent hypothesis to the 'wanted' one,
;;; but the parent hyp is not added to the justification list of the 'wanted'.
;;; (Implementationally this is achieved by setting H2M-WEIGHT to 0.0)
;;;
;;; In most cases bottom-up SC-2 and top-down SC-1 (or -2) create the same
;;; links (although with different weights -- SC-2 is weaker).  For example:
;;;  + Bottom-up SC-2 performed by TEAPOT-WTP<-->TEAPOT-MTF will create T-LINK
;;;      to MADE-OF-WTP<-->MADE-OF-MTF with weight (e.g.) 0.2.
;;;  + Top-down SC-1 performed by MADE-OF-WTP<-->MADE-OF-MTF will create
;;;      justification links to TEAPOT-WTP<-->TEAPOT-MTF.  In addition, it
;;;      will create T-LINKS in the opposite direction -- from TPOT<-->TPOT to
;;;      MD-OF<-->MD-OF.  These links are typically stronger (e.g. weight 0.3)
;;;      and subsume the links created by the bottom-up SC-2, if any.
;;;      (All calls to ADD-LINK use :PRIORITY #'MAX, see DUAL/ARCHIT/LINKS.LSP.)
;;;
;;; If the two mechanisms yield overlapping result, why waste time on bottom-up
;;; SC-2 at all?
;;; This mechanism is useful (though not terribly useful) for two reasons:
;;;   1. There are cases in which it creates links that noone else would.
;;;      In particular, this is the case of cross-mapping of asymmetrical
;;;      relations.  Consider for example the following two propositions:
;;;        (in x y)  and  (in a b)   with the mapping  X<-->B and Y<-->A.
;;;      Bottom-up SC-2 performed by X<-->B will link it to INxy<-->INab.
;;;      By contrast, the top-down SC-1 will consider only X<-->A and Y<-->B,
;;;      thus failing to create any links involving the cross-hypotheses.
;;;   2. Bottom-up SC-2 links are often created earlier than those of top-down
;;;      SC-1.  Thus they improve the connectivity of the constraint satisfac-
;;;      tion network during the (cruicial) early stage of its relaxation.
;;;

;; BOTTOM-UP-C-COREFS produces a list of all agents in which a given agent is
;; a S-slot.  This information is kept in the :C-COREF G-slot of the agent
;; in question. For example, suppose that TEAPOT-WTP is defined like this:
;;   (defagent teapot-WTP  ...
;;    :c-coref ((initst-WTP . :slot3) (on-WTP . :slot2) (made-of-WTP . :slot1)))
;;   Then  (bottom-up-C-COREFs teapot-WTP) --> (initst-WTP on-WTP made-of-WTP)
;;
;; Only the 'extended symrefs' are considered (see DUAL/ARCHIT/SYMREF.LSP).
;; This function is very similar to ARGUMENT-PROPS defined in AMBR/KREPRES.LSP.

(defun bottom-up-C-COREFs (agent)
  "List all agents in which AGENT is a S-slot."
  (declare (values list))   ; of agents
  (mapcan #'(lambda (symref)
              (if (extended-symref-p symref)     ; e.g. (#$on-WTP . :slot2)
                  (list (symref-agent symref))
                  nil))                          ; see p.129 in CLtL1
          (get-filler-refs! agent :c-coref) ))


;; BOTTOM-UP-SC-2 retrieves the bottom-up C-COREFs of each hypothesis element
;; and then formulates a SC-memo for each possible pair.

(defmethod bottom-up-SC-2 ((hypoth mature-hypoth-agent))
  (declare (values S-PROGN-return-type))
  (let ((driver-elt    (corresp-elt hypoth *driver-elt-label*))
        (recipient-elt (corresp-elt hypoth *recipient-elt-label*))
        (driver-sit    (corresp-driver hypoth)) )   ; see AMBR/CORRESP.LSP
   (s-progn hypoth
     [ensure-hypothesis-elements hypoth]   ; both elements still visible?
     (let* ((driver-C-COREFs    [bottom-up-C-COREFs driver-elt])
            (recipient-C-COREFs [bottom-up-C-COREFs recipient-elt])
            (homog-states-p     [homogeneous-states-p driver-elt recipient-elt])
            (mentor-weight  (if homog-states-p               ; don't check
                                *SC2-mentor-homog-weight*    ; each indiv. pair
                                *SC2-mentor-heterog-weight*)) )
       (dolist (driver-agent driver-C-COREFs)
         (dolist (recipient-agent recipient-C-COREFs)
           (let ((memo [make-SC-memo driver-agent
                                     recipient-agent
                                     driver-sit
                                     :m2h-weight mentor-weight  ; same for all
                                     :h2m-weight 0.0       ; no hyp->mentor link
                                     :trials     0   ]))   ; don't retry
             [send-SC-memo-to-self hypoth memo]))) ))))

(defmethod bottom-up-SC-2 ((x t))
  (error "BOTTOM-UP-SC-2: ~S is not a mature hypothesis." x ))



;;;;;;  ******  HANDLING SC-MEMOS  ******
;;;
;;;  The routine for handling a SC-memo involves two main steps:
;;;   1. Find out the 'wanted' hypothesis described by the memo. This is done
;;;         by looking at the secretary list of the _recipient_ element.
;;;         (It is chosen because its list is supposed to be much shorter.)
;;;         The function FIND-ANY-HYPOTHESIS (from AMBR/SECRETAR.LSP) returns
;;;         the first hypothesis that matches the description, if any.
;;;   2a. If a hypothesis is found, create links as described by the memo
;;;   2b. If there is no such hypothesis, decide whether to give up or to
;;;         retry later (see POSTPONE-SC-MEMO).

(defmethod handle-symbol ((hypoth hypoth-agent)     ; mature, maybe winner,...
                          (memo SC-memo) )
  (declare (values S-PROGN-return-type))
  (s-progn hypoth
    (suspended-value-bind (wanted-hyp)                               ; step 1.
                          [find-any-hypothesis hypoth           ; host
                                (SC-memo-recipient-elt memo)    ; secretary
                                (SC-memo-driver-elt    memo)    ; other-elt
                                (SC-memo-driver-sit    memo)]   ; driver-sit
      (cond (wanted-hyp                          ; 'wanted' hypoth is found
                [create-SC-2-links hypoth wanted-hyp memo])     ; step 2a.
            ((plusp (SC-memo-trials memo))       ; not found, retry later
                [postpone-SC-memo hypoth memo])                 ; step 2b.
            (t  nil) ))))                        ; not found and no more trials


;; POSTPONE-SC-MEMO sets an alarm clock and wraps the SC-memo in its clock
;; message (see DUAL/ARCHIT/TIME.LSP).  The alarm clock will take care to
;; send the memo back to its owner after the specified period of time.
;; See parameter *STATE-SC-WAIT-PERIOD* in AMBR/DEFS.LSP.

(defun  postpone-SC-memo (hypoth SC-memo)
  "Arranges (by setting an alarm clock) that SC-memo will be handled later."
  (declare (type hypoth-agent hypoth)
           (type SC-memo SC-memo)
           (values alarm-clock) )
  #+:DUAL-DEBUG (assert (plusp (SC-memo-trials SC-memo)))
  (decf (SC-memo-trials SC-memo))
  (set-alarm-clock                             ; see DUAL/ARCHIT/TIME.LSP
       (make-alarm-clock (gensym "SC-MEMO")    ; unique clock-type each time
                         hypoth )              ; owner
       *state-SC-wait-period*                  ; period
       SC-memo) )                              ; message


;; CREATE-SC-2-LINKS is the culmination (and the raison d'etre) of the whole
;; 'weak' SC mechanism.  It creates T-LINKs according to the weights prescribed
;; by the SC-memo.  Zero weight means 'no link'.

(defun  create-SC-2-links (mentor-hypoth wanted-hypoth SC-memo)
  "Creates T-LINKs on the basis of 'weak' structure correspondence."
  (declare (type hypoth-agent mentor-hypoth wanted-hypoth)
           (type SC-memo SC-memo)
           (values S-PROGN-return-type) )
  (let ((mentor->hypoth-weight (SC-memo-m2h-weight SC-memo))
        (hypoth->mentor-weight (SC-memo-h2m-weight SC-memo)) )
    (s-progn mentor-hypoth
      (unless (zerop hypoth->mentor-weight)
        [send-write-request wanted-hypoth
                            'add-link :t-link           ; order remote link
                            mentor-hypoth hypoth->mentor-weight
                            :priority #'max] )
      (unless (zerop mentor->hypoth-weight)
        (when [agent-visible-p wanted-hypoth]
          [add-link mentor-hypoth :t-link                 ; create local link
                    wanted-hypoth mentor->hypoth-weight
                    :priority #'max])) )))


;;;;;;;  End of file AMBR/WEAK_SC.LSP
