;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/str_corr.lsp   ; see also AMBR/weak_SC.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Structure correspondence mechanism
;;; DEPENDS-ON: DUAL; ambr/krepres.lsp, ambr/ambr_ag.lsp,
;;;                   ambr/corresp.lsp, ambr/hypoth.lsp, ambr/weak_sc.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    16-06-97
;;; UPDATED:    23-02-98 -- Support for 'driver situations'.
;;; UPDATED:    06-05-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...
;;;;;;;;
;;; TO DO:      Consider a fifth slot to corr-agents -- CORR-OFFSPRING -- that
;;;             keeps the hypotheses generated via structure correspondence.
;;;             (Now they are T-LINK'ed and are inaccessible for symb.procesng)
;;; TO DO:      Reconsider consumptions of operations within xxx-SC.
;;; TO DO:      Make SC-memos (see AMBR/WEAK_SC.LSP) for non-corresponding args
;;;             of asymmetric relations during top-down SC-1.


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;   S T R U C T U R E   C O R R E S P O N D E N C E   ;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR-CORE")

;;;; The key concept defined in this file is STRUCTURE CORRESPONDENCE (SC).
;;;; See section 4.3.4 in "Extensions of DUAL and AMBR".
;;;; See also file AMBR/WEAK_SC.LSP for an extension of the SC mechanism.
;;;;
;;;; The structure-correspondence mechanism generates new hypotheses on the
;;;; basis of existing hypotheses.  It also creates excitatory links between
;;;; coherent hypotheses.  It is carried out by the mature hypothesis-agents.
;;;;
;;;; There are two major types of SC, conventionally termed BOTTOM-UP SC and
;;;; TOP-DOWN SC.  Newer versions of AMBR (ver.2.2.2+) also have the so-called
;;;; 'weak SC' (see AMBR/WEAK.LSP) which creates new links only, not new hyps.
;;;;
;;;; BOTTOM-UP SC takes place when there is a hypothesis involving two instance-
;;;; agents.  It generates new hypotheses that their respective concept-agents
;;;; and situation-agents (if any) also correspond.  For example, starting
;;;; from the hypothesis WATER-WTP<-->MILK-MTF the bottom-up SC would generate
;;;; the hypotheses WATER<==>MILK and SIT-WTP<==>SIT-MTF.
;;;;
;;;; TOP-DOWN SC takes place when there is a hypothesis involving two proposi-
;;;; tions, i.e. two instance-agents having the tag :RELATION in their TYPE
;;;; slots.  It generates new hypotheses that the respective arguments of
;;;; the two propositions also correspond.
;;;;
;;;; It frequently happens that the SC-generated hypotheses are not really
;;;; new -- the same agents have been already put into correspondence.  In
;;;; the above example, suppose that the bottom-up SC has generated the hypoth
;;;; SIT-WTP<==>SIT-MTF on the basis of the hypothesis WATER-WTP<-->MILK-MTF.
;;;; After a while another hypothesis, eg. TEAPOT-WTP<-->TEAPOT-MTF, constructs
;;;; the hypothesis SIT-WTP<=1=>SIT-MTF.  The latter, however, is a duplicate
;;;; of SIT-WTP<==>SIT-MTF.  This duplication is detected by the secretaries
;;;; of SIT-WTP and SIT-MTF (see AMBR/SECRETAR.LSP) and the second hypothesis
;;;; will resign in favor of the first (see RESIGN-HYPOTHESIS in =/HYPOTH.LSP).
;;;; The net result is that SIT-WTP<==>SIT-MTF will have two justifications.
;;;; In this way SIT-WTP<=1=>SIT-MTF actually results in a new 'justification'
;;;; link instead-of a full-fledged 'mature' hypothesis.  ('Weak' structure
;;;; correspondence see AMBR/WEAK_SC.LSP, also aim at creation of new links.)


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: structure-correspondence, bottom-up-SC, top-down-SC,
;;          cartesian-product
;;
;; The functions in this file depend on the following parameters from DEFS.LSP:
;;   *hypoth->element-weight*, *hypoth->situation-weight*, *hypoth->SC1-mentor-
;;   weight*, *SC1-mentor-homog-weight*. (*SC1-mentor-heterog-weight* not yet.)
;; They also depend on REMOTE-GENERAL-HYPOTHESIS-P defined in AMBR/SKOLEM1.LSP.
;; ...

;; CARTESIAN-PRODUCT takes two lists and returns a list of pairs...
;;  ...

;; ...

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric bottom-up-SC (hypothesis)  ; see BOTTOM-UP-SC-2 in AMBR/WEAK_SC.LSP
  (:documentation
  "If HYPOTHESIS is about instances, construct hypothesis about concepts." ))

(defgeneric top-down-SC (hypothesis)   ; see TOP-DOWN-SC-2 in AMBR/WEAK_SC.LSP
  (:documentation
  "If HYPOTHESIS is about propositions, construct hypotheses about the args." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;  ******  S T R U C T U R E   C O R R E S P O N D E N C E  *****
;;;
;;;  Bottom-up ...
;;;  Top-down ...  (i) easy, (ii) depending on marker-intersection reports
;;;  ...
;;;
;;; Convention: Hypotheses generated by the marker-passing mechanism use
;;;             the character sequence  '<-->'  in their names (see MARKER.LSP).
;;;             By contrast, hypotheses generated by the structure-correspon-
;;;             dence mechanism use the character sequence  '<==>'.
;;;

(defun structure-correspondence (hypoth)
  "Create new hypotheses to extend the mapping initiated by HYPOTH."
  (declare (type mature-hypoth-agent hypoth))
  (s-progn hypoth
     [ensure-hypothesis-elements hypoth]       ; both elts still visible?
     (when [corresp-type hypoth :INSTANCE]     ; both elts are instances?
       [bottom-up-SC hypoth]
       (when (corresp-type hypoth :RELATION)   ; both elts are propositions?
         (unless [remote-general-hypothesis-p hypoth]   ; see AMBR/SKOLEM1.LSP
           [top-down-SC hypoth]))
       [weak-structure-correspondence hypoth] )))       ; see AMBR/WEAK_SC.LSP


(defun formulate-SC-NCR (parent-hypoth driver-elt recipient-elt mentor-weight)
  "Formulates a NC-request on the basis of structure correspondence."
  ;; See AMBR/CORRESP.LSP for documentation on the four fields of corr-agents.
  ;; See FORMULATE-MP-NCR in AMBR/MARKER.LSP for marker-passing NCRs.
  (declare (type corr-agent parent-hypoth)
           (type (or instance-agent concept-agent) driver-elt recipient-elt)
           (type number mentor-weight)
           (values node-construction-request) )
  (let ((mentor-conref (make-conref (make-symref parent-hypoth :t-link)
                                    mentor-weight))
        (driver-elt-filler    (make-conref driver-elt
                                           *hypoth->element-weight*))
        (recipient-elt-filler (make-conref recipient-elt
                                           *hypoth->element-weight*))
        (justification-filler (make-conref parent-hypoth
                                           *hypoth->SC1-mentor-weight*))
        (driver-sit-filler    (make-conref (corresp-driver parent-hypoth)
                                           *hypoth->situation-weight*)) )
    (make-NC-request
          mentor-conref                                           ; mentor
          'embryo-hypoth-agent                                    ; agent-type
          (SC-GENNAME-template driver-elt recipient-elt)          ; GENNAME
          (list (list *driver-elt-label*    :type    :aspect)     ; slots
                (list *recipient-elt-label* :type    :aspect)
                (list *justification-label* :type    :aspect)
                (list *driver-sit-label*    :type    :aspect)
                (list *driver-elt-label*    :c-coref driver-elt-filler   )
                (list *recipient-elt-label* :c-coref recipient-elt-filler)
                (list *justification-label* :c-coref justification-filler)
                (list *driver-sit-label*    :c-coref driver-sit-filler   ) )
          nil )))                                                 ; no mail


;;  Convention: Hypotheses generated by the structure-correspondence mechanism
;;  use the character sequence  '<==>'  in their names.  By contrast, hypoth.
;;  generated by the marker-passing mechanism use  '<-->'.
;;  See MP-GENNAME-TEMPLATE in AMBR/MARKER.LSP.
;;
(defun SC-GENNAME-template (driver recipient)
  "Construct a GENNAME template for the name of a SC-generated hypothesis."
  (list "~A<=~@[~D~]=>~A"            ; see GENNAME in DUAL/ARCHIT/BASIC.LSP
        (agent-name driver)          ; e.g:  WATER<==>MILK   or, if used
        nil                               ;  WATER<=1=>MILK  or, if used
        (agent-name recipient)) )         ;  WATER<=2=>MILK  etc.



;;;;;  ******  BOTTOM-UP STRUCTURE CORRESPONDENCE  ******
;;;
;;; E.g.  water-1<-->milk-3   ==>  | water<==>milk
;;;                                | sit-Y1<==>sit-A
;;;
;;; Situation-hypotheses are created before concept-hypotheses because it
;;; is advantageous to establish links with the powerful hypothesis
;;; SIT-xxx<-->SIT-yyy as early as possible.  Such link usually increases
;;; the activation level of the hypothesis doing bottom-up SC, which in
;;; turn gives it better chances to recruit node constructors.
;;;
;;; See BOTTOM-UP-SC-2 in AMBR/WEAK_SC.LSP

(defmethod bottom-up-SC ((hypoth mature-hypoth-agent))
  (declare (values S-PROGN-return-type))
  #+:DUAL-DEBUG (assert (corresp-type hypoth :INSTANCE))
  (let* ((elt1     (corresp-elt hypoth *driver-elt-label*))
         (elt2     (corresp-elt hypoth *recipient-elt-label*))
         (concept1 (instance->concept elt1))
         (concept2 (instance->concept elt2))
         (sit1     (agent-situation elt1))
         (sit2     (agent-situation elt2)) )
   (s-progn hypoth
      [ensure-hypothesis-elements hypoth]   ; both elements still visible?
      (unless (or (null sit1)    (null sit2)        ; :SITUATION chain broken
                  (eq elt1 sit1) (eq elt2 sit2) )   ; ELTs are situation-agents
        [send-NC-request hypoth
              [formulate-SC-NCR hypoth sit1 sit2
                                       *SC1-mentor-HOMOG-weight*]])
      (unless (or (null concept1) (null concept2))  ; :INST-OF chain broken
        [send-NC-request hypoth
              [formulate-SC-NCR hypoth concept1 concept2
                                       *SC1-mentor-HOMOG-weight*]]) )))

(defmethod bottom-up-SC ((x t))
  (error "BOTTOM-UP-SC: ~S is not a mature hypothesis." x ))



;;;;;  ******  TOP-DOWN STRUCTURE CORRESPONDENCE  ******
;;;
;;; Top-down SC takes place only when the 'parent' hypothesis involves two
;;; propositions. The criterion is that the :TYPE slots of both elements
;;; contain the tags :INSTANCE and :RELATION.
;;;
;;; The essence of top-down SC is that if two propositions are mapped their
;;; arguments should also be mapped.
;;; This is in line with Gentner's (1983) systematicity principle.
;;;
;;; The major challenge with top-down SC is to determine which slot(s) from
;;; the 'left' proposition to map to which slot(s) from the 'right' prop.
;;; The so-called PIVOT-CONCEPT is of paramount importance for determining
;;; this slot-to-slot mapping. The pivot concept is the common (super)class
;;; of the two propositions. There are two cases:
;;;   -- 'easy' case -- the two propositions are instances of the same relation
;;;   -- 'difficult' case -- they are instances of different relations
;;;
;;; The easy case can be completed without any additional information.
;;; The difficult case needs additional information which is (sometimes)
;;; provided by the marker-passing mechanism.
;;;
;;; Example of the 'easy' case:
;;;   Let the hypothesis in-1<-->in-7 represent the (tentative) correspondence
;;;   between the propositions:
;;;     in-1:   (in water-1 teapot-1)   and
;;;     in-7:   (in milk-7  glass-7)
;;;   Both propositions are instances of the same relation (namely IN).
;;;   The concept IN will be used as pivot concept. Its description states
;;;   that it is an asymmetric relation. Therefore, each argument (first or
;;;   second) from in-1 is mapped only to the corresponding (first or second)
;;;   argument from in-7. In other words, the top-down SC in this case
;;;   generates the hypotheses   water-1<==>milk-7  and  teapot-1<==>glass-7 .
;;;
;;; Example of the 'difficult' case:
;;;   Let the hypothesis  on-1<-->in-3  represent the (tentative) correspondence
;;;   between the propositions:
;;;     on-2:   (on teapot-1 plate-1)
;;;     in-3:   (in imm-heater-1 water-3)
;;;   These two propositions are instances of two different relations (namely
;;;   ON and IN). Suppose that the marker-passing mechanism has detected that
;;;   the relations ON and IN have a common superclass IN-TOUCH.
;;;   Therefore, the concept IN-TOUCH will be used as pivot. Its description
;;;   states (through :PEER facets) that the IN-TOUCH is a symmetric relation.
;;;   Therefore all four possible hypotheses are generated:
;;;     teapot-1<==>imm-heater-1,  teapot-1<==>water-3,
;;;     plate-1<==>imm-heater-1, and  plate-1<==>water-3 .
;;;
;;; See also TOP-DOWN-SC-2 in AMBR/WEAK_SC.LSP
;;;

;;  Algorithm:
;;    1. If the buffer contains a MP-X-report (stored there by the embryo)
;;       then use it.
;;    2. If the input zone contains a MP-X-report then use it.
;;    2. Otherwise, try the easy case.
;;    3. If this also fails then go to sleep, hoping that MP-X-reports will
;;       sooner or later appear in the input zone.

(eval-when (compile load eval)
  (proclaim-consumption 'top-down-SC-aux  :explicit-S-PROGN )
  (proclaim-consumption 'top-down-SC-from-scratch
                        (get-consumption 'top-down-SC-aux) )  ; not really...
  (proclaim-consumption 'top-down-SC-from-markers
                        (get-consumption 'top-down-SC-aux) )
)
(defmethod top-down-SC ((hypoth mature-hypoth-agent))
  (declare (values S-PROGN-return-type))
  (if (corresp-type hypoth '(:INSTANCE :RELATION))
      (s-progn hypoth
        [ensure-hypothesis-elements hypoth]   ; both elements still visible?
        (let ((MP-X-reports [look-for-cached-MP-X hypoth]))
          (if (endp MP-X-reports)
              [top-down-SC-from-scratch hypoth]
              (dolist (MP-X MP-X-reports)
                [top-down-SC-from-markers hypoth MP-X])) ))
      #+:DUAL-DEBUG
      (warn "TOP-DOWN-SC: ~S is not about propositions." hypoth)
  ))

(defmethod top-down-SC ((x t))
  (error "TOP-DOWN-SC: ~S is not a mature hypothesis." x ))


(defun look-for-cached-MP-X (hypoth)
  "Help TOP-DOWN-SC by looking for MP-X-reports cached in the local memory."
  (declare (type mature-hypoth-agent hypoth)
           (values list) )  ; of MP-X-reports
  (let ((from-buffer (remove-if (complement #'MP-X-report-p)
                                (agent-buffer hypoth)))
        (from-input  (remove-if (complement #'MP-X-report-p)
                                (agent-input-zone hypoth))) )
    (unless (null from-buffer)
      (setf (agent-buffer hypoth)
            (delete-if #'MP-X-report-p (agent-buffer hypoth))) )
    (unless (null from-input)
      (setf (agent-input-zone hypoth)
            (delete-if #'MP-X-report-p (agent-input-zone hypoth))) )
    (union1 from-input from-buffer :key #'MPX-intersection) ))
    ;; See DUAL/GENERAL.LSP for documentation of UNION1.


(defun top-down-SC-from-scratch (hypoth)
  (declare (values S-PROGN-return-type))   ; of TOP-DOWN-SC-AUX
  ;; ASSUMPTION: (corresp-type hypoth '(:INSTANCE :RELATION))
  (let* ((elt1 (corresp-elt hypoth *driver-elt-label*))
         (elt2 (corresp-elt hypoth *recipient-elt-label*))
         (concept (instance->concept elt1)) )
    (flet ((track-slots (elt)
             (let ((path (list elt concept))      ; Cf. AMBR/MARKER.LSP
                   (result (list elt)))
               (dolist (S-slot (agent-S-slots elt) (nreverse result))
                 (push (cons (slot-label S-slot)
                             (track-slot-in-path (slot-label S-slot) path))
                       result))) ))
      (if (eq concept (instance->concept elt2))   ; 'easy' case?
          (top-down-SC-aux hypoth
                           concept
                           (track-slots elt1)
                           (track-slots elt2))
          nil) ; 'difficult' case -- go to sleep in hope for MP-X-reports
      )))

(defun top-down-SC-from-markers (hypoth MP-X)
  (declare (values S-PROGN-return-type)
           (type marker-intersection-report MP-X) )
  ;; ASSUMPTION: (corresp-type hypoth '(:INSTANCE :RELATION))
  (top-down-SC-aux hypoth
                   (MPX-intersection   MP-X)
                   (MPX-driver-path    MP-X)
                   (MPX-recipient-path MP-X) ))


;;;; Housekeeping functions related to MP-X reports:
;;

(defmethod handle-symbol ((embryo embryo-hypoth-agent)
                          (MP-X marker-intersection-report) )
  (declare (values S-PROGN-return-type))
  (s-progn embryo
    [ensure-hypothesis-elements embryo]   ; both elements still visible?
    (s-eval *default-AMBR-INTERNAL-consumption* 
            (pushnew MP-X (agent-buffer embryo))) ))

(defmethod handle-symbol ((hypoth mature-hypoth-agent)
                          (MP-X marker-intersection-report))
  (declare (values S-PROGN-return-type))
  (when (corresp-type hypoth '(:instance :relation))   ; top-down SC warranted?
    (top-down-SC-from-markers hypoth MP-X) ))



;;;;;  ******  GENERATION OF 'CHILD' HYPOTHESES  ******
;;
;; The bulk of the work during top-down structure correspondence is done by
;; the function TOP-DOWN-SC-AUX. It takes four arguments:
;;     1. the (parent) hypothesis,
;;     2. the pivot concept, and
;;   3&4. two lists representing slot-to-slot correspondences. Each list has
;;        a 'header' containing the element (the child) followed by a list of
;;        conses. Each cons represents an elementary correspondence between
;;        slots. For instance, the cons (:SLOT1 . :SLOT2) means that slot1 in
;;        the child is subordinate to slot2 in the pivot concept.
;;        (See also TRACK-SLOT-IN-PATH in AMBR/MARKER.LSP)
;;
;; Example of a full call:
;;  (top-down-SC-aux   #$in-1<==>on-2     ; parent hypothesis
;;                     #$in-touch         ; pivot concept
;;                     (list #$in-1 '(:slot1 . :slot1) '(:slot2 . :slot2))
;;                     (list #$on-2 '(:slot1 . :slot2) '(:slot2 . :slot1)) )

;; TOP-DOWN-SC-AUX works according to the following algorithm:
;;   1. Check whether a SC with this pivot concept has been performed already.
;;      To that end, used pivot concepts are stored in the buffer:
;;    1a. If PIVOT is present in the buffer then exit without doing anything.
;;    1b. Otherwise store PIVOT in the buffer and continue.
;;   2. Compute the set of slot-to-slot mappings.
;;  ;3. ;; In this version, do not bother to store the mappings in the buffer
;;  ;   ;; and then to filter them out during the analysis of subsequent
;;  ;   ;; MP-X-reports (if any).
;;   4. Collect the operands of each proposition by following :C-COREFs in the
;;      S-slots of ELT1 and ELT2.  (See PROPOSITION-ARGS in AMBR/KREPRES.LSP.)
;;   5. Choose what mentor weight to use -- 'homogeneous' or 'heterogeneous'?
;;   6. Formulate and send node-construction requests for each candidate-
;;      hypothesis.
;;
;; TO DO: Formulate and send to self a SC-memo (see AMBR/WEAK_SC.LSP) for each
;;        rejected arg pair.  That is, for pairs from the set produced by
;;        COMPUTE-SLOT-TO-SLOT-CORRESPONDENCE, make NC-requests and send them
;;        to node constructors.  For the remaining pairs, make SC-memos and
;;        send them to self.
;;

(defun top-down-SC-aux (hypoth pivot corr1 corr2)
  (declare (type mature-hypoth-agent hypoth)
           (type concept-agent pivot)
           (list corr1 corr2)
           (values S-PROGN-return-type) )
  (unless (member pivot (agent-buffer hypoth))                      ; step 1a
    (push pivot (agent-buffer hypoth))                              ; step 1b
    (let ((child1 (first corr1))
          (child2 (first corr2))
          (slot-to-slot (compute-slot-to-slot-correspondence        ; step 2
                                              (peer-slots pivot) 
                                              (rest corr1) (rest corr2)))
          (ops1 nil) (ops2 nil) )  ; to be set below
      (s-progn hypoth
        (s-eval (* 3 *default-AMBR-INTERNAL-consumption*) nil) ; for steps 1+2
        [ensure-hypothesis-elements hypoth]   ; both elements still visible?
        ;; Step 3 is not implemented yet
        (setq ops1 [proposition-args child1])                       ; step 4
        (setq ops2 [proposition-args child2])                       ; step 4
        (dolist (pair (substitute-ops-for-slots slot-to-slot ops1 ops2))
          (let* ((op1 (cAr pair))
                 (op2 (cDr pair))
                 (mentor-weight [SC1-mentor-weight op1 op2])        ; step 5
                 (NC-request [formulate-SC-NCR hypoth op1 op2       ; step 6
                                                      mentor-weight]) )
            [send-NC-request hypoth NC-request] ))                  ; step 6
      ))))


(defun substitute-ops-for-slots (slot-to-slot ops1 ops2)
  ;; Destructively substitute within SLOT-TO-SLOT
  ;; First, treat the CAR of each pair, replace with OPERANDs from OPS1
  (dolist (pair slot-to-slot)
    (let ((operand (cdr (assoc (cAr pair) ops1))))  ; see p. 433 in CLtL2
      (if (null operand)          ; this slot does not have a :C-COREF facet?
          (setq slot-to-slot (delete pair slot-to-slot))
          (setf (cAr pair) operand) )))
  ;; Second, treat the CDR of each pair, replace with OPERANDs from OPS2
  (dolist (pair slot-to-slot)
    (let ((operand (cdr (assoc (cDr pair) ops2))))
      (if (null operand)          ; this slot does not have a :C-COREF facet?
          (setq slot-to-slot (delete pair slot-to-slot))
          (setf (cDr pair) operand) )))
  ;; Finally, return the new list (which is still bound to the old variable)
  slot-to-slot )


;; SC1-MENTOR-WEIGHT inspects the tags in the TYPE and MODALITY slots of the
;; two entity-agents and determines the weight of the link from the 'parent'
;; hypothesis to the new agents that will be requested.
;; There are two weights -- 'homo-' and 'heterogeneous' -- see AMBR/DEFS.LSP.
;; The former is greater than the latter, thus favoring homogeneous hyps.

(defun  SC1-mentor-weight (child1 child2)
  "*SC1-mentor-HOMOG-weight*  or  *SC1-mentor-HETEROG-weight*"
  (declare (type (or instance-agent concept-agent) child1 child2)
           (type (member :concept-concept :sit-sit :entity-entity) mode)
           (values float) )
  (let* ((type1 (get-filler child1 :type))
         (type2 (get-filler child2 :type))
         (instance/concept-match-p
             (or (and (member :instance  type1) (member :instance  type2))
                 (and (member :concept   type1) (member :concept   type2))))
         (object/relation-match-p
             (or (and (member :object    type1) (member :object    type2))
                 (and (member :relation  type1) (member :relation  type2))
                 (and (member :situation type1) (member :situation type2)))) )
    (cond ((not instance/concept-match-p) *SC1-mentor-heterog-weight*)
          ((not object/relation-match-p)  *SC1-mentor-heterog-weight*)
          ((member :concept  type1)       *SC1-mentor-homog-weight*  )
          ((member :instance type1)    ; two instances -- check MODALITY too
             (if (homogeneous-states-p child1 child2)  ; see AMBR/KREPRES.LSP
                 *SC1-mentor-homog-weight*             ; e.g. init<-->init
                 *SC1-mentor-heterog-weight*))         ; e.g. init<-->goal
          (t     *SC1-mentor-heterog-weight* )) ))     ; bad TYPE filler(s)


;; PEER-SLOTS partitions the set of all S-slots of PIVOT into equivalence
;; subsets based on the :PEER facets in these slots.
;; This is a generalization of the notion of symmetric binary relations.
;; For instance, the two operands to IN-TOUCH are interchangeable 'peers'.
;; By extension, one can think of a relation with, e.g. three slots, two of
;; which are peers and the third is not. In this case, PEER-SLOTS should
;; return something like:  ((:slot1 :slot2) (:slot3))
;; PEER-SLOTS always returns a list of lists. Each sublist represents a
;; equivalence subset. These subsets form a disjoint partition of the set
;; of all specific slots of PIVOT.
;; With asymmetric relations (a very common case), PEER-SLOTS will produce
;; a list of singleton lists.
;;
;; (Note: PEER-SLOTS assumes that the :PEER facets correctly represent an
;;  equivalence relation among the S-slots of PIVOT. In future versions of
;;  the programs this will be checked (e.g. by ESTABLISH-AGENT-INTEGRITY))

(defun peer-slots (pivot &aux (subsets '()))
  "Partition the set of PIVOT's S-slots into :peer equivalence subsets."
  (declare (type concept-agent pivot)
           (values list) )
  (flet ((locate-subset (slot-label)
           (cond ((find-if #'(lambda (sublist) 
                               (find slot-label sublist))
                           subsets))                       ; return existing
                 (t (let ((new-subset (list slot-label)))  ; construct new
                      (push new-subset subsets)            ;   store it
                       new-subset)) )))                    ;   and return it
    (dolist (S-slot (agent-S-slots pivot))
      (let ((subset (locate-subset (slot-label S-slot))))
        (dolist (peer (get-filler S-slot :peer))
          (unless (member peer subset)
            (nconc subset (list peer)) ))))   ; add destructively
    (nreverse subsets)))


;; The purpose of COMPUTE-SLOT-TO-SLOT-CORRESPONDENCE can schematically be
;; stated like this: given a mapping from A to P, a mapping from B to P, and
;; a partition of P, compute all legitimate pairs of the elements of A and B.
;; For example: 
;;   1. let P has three elements partitioned '((1) (2 3))
;;   2. let A has three elements mapped to those of P in the following way:
;;        A1 -> P1, A2 -> P2, A3 -> P3   
;;   3. let B has four elements mapped B1 -> P1, B2 -> P2, B3 -> P3, B4 -> P1
;; This situation will be represented by the following values of the arguments:
;;   partition <-- '((1) (2 3))
;;   a-list1   <-- '((1 . 1) (2 . 2) (3 . 3))
;;   a-list2   <-- '((1 . 1) (2 . 2) (3 . 3) (4 . 1))
;; Then, COMPUTE-SLOT-TO-SLOT-CORRESPONDENCE should produce the following
;; list of pairs (or some permutation thereof):
;;   '((1 . 1) (1 . 4) (2 . 2) (2 . 3) (3 . 2) (3 . 3))
;;
;; To achieve this, it uses the following algorithm:
;;   1. Build a table like this:         P  |  A  |  B       represented as:
;;       (There is one row for each    ------+-----+-------
;;        element of the pivot P.        1  | (1) | (1 4)     '((1 (1) (1 4))
;;        The cells denote the corres-   2  | (2) | (2)         (2 (2) (2))
;;        pondences A->P and B->P.)      3  | (3) | (3)         (3 (3) (3)) )
;;   2. Merge rows in the table according to PARTITION like this:
;;         P    |   A   |   B      represented as:
;;       -------+-------+-------
;;        (1)   | (1)   | (1 4)                   '(((1)   (1)   (1 4))
;;        (2 3) | (2 3) | (2 3)                     ((2 3) (2 3) (2 3)) )
;;      Table 2 contains one row for each partition subclass.
;;   3. Compute the cartesian product AxB for each row of table 2:
;;        ((1 . 1) (1 . 4))
;;        ((2 . 2) (2 . 3) (3 . 2) (3 . 3))
;;   4. Unite everything into a single list of pairs and return that list.

(defun compute-slot-to-slot-correspondence (partition a-list1 a-list2)
  "Generates candidates for top-down structure correspondence."
  (declare (type list partition a-list1 a-list2))
  (let* ((table1 (compute-P-A-B-table1 partition a-list1 a-list2))  ; step 1
         (table2 (compute-P-A-B-table2 partition table1))           ; step 2
         (result '()) )
    (dolist (row table2)
      (dolist (pair (cartesian-product (second row) (third row)))   ; step 3
        (pushnew pair result) ))                                    ; step 4
    (nreverse result) ))
 ;; UNION is avoided because the order of pairs in the final list is
 ;; not specified by the Common Lisp standard.  This order is important
 ;; for the behavior of the architecture because it affects the creation
 ;; of new hypotheses which compete for resources, etc.
 ;; (See UNION1 in DUAL/GENERAL.LSP.)

(defun compute-P-A-B-table1 (partition A-mappings B-mappings)
  (declare (type list partition A-mappings B-mappings))
  (flet ((generate-row (P-element &aux A-cell B-cell)
           (dolist (m A-mappings)
             (when (eq P-element (cdr m))
               (push (car m) A-cell)))
           (dolist (m B-mappings)
             (when (eq P-element (cdr m))
               (push (car m) B-cell)))
           (list P-element (nreverse A-cell) (nreverse B-cell)) ))
    (let ((table1 '()))
      (dolist (subset partition)
        (dolist (P-elt subset)
          (push (generate-row P-elt) table1)))
      (nreverse table1) )))

(defun compute-P-A-B-table2 (partition table1)
  (declare (type list partition table1))
  (flet ((generate-row (peer-subclass &aux A-cell B-cell)
           (dolist (P-elt peer-subclass)
             (setq A-cell
                   (nconc A-cell (second (find P-elt table1 :key #'first))))
             (setq B-cell
                   (nconc B-cell (third  (find P-elt table1 :key #'first)))) )
           (list peer-subclass A-cell B-cell) ))
    (mapcar #'generate-row partition) ))


(defun cartesian-product (set1 set2)
  "Take two lists and return a list of all possible pairs b/n them."
  (declare (type list set1 set2))
  (let ((result '()))
    (dolist (e1 set1 (nreverse result))
      (dolist (e2 set2)
        (push (cons e1 e2) result) ))))


;;;;;   *****  TEST CASE   ******
;;

#|
(in-package "AMBR")

(defagent P concept-agent
  :type  (:concept :relation)
  :slot1
    :type :aspect
  :slot2
    :type :aspect
    :peer :slot3
  :slot3
    :type :aspect
    :peer :slot2
)

(defagent A instance-agent
  :type     (:instance :relation)
  :inst-of  P
  :slot1
    :type     :aspect
    :inst-of  (P . :slot1)
    :c-coref  a1
  :slot2
    :type     :aspect
    :inst-of  (P . :slot2)
    :c-coref  a2
  :slot3
    :type     :aspect
    :inst-of  (P . :slot3)
    :c-coref  a3
)

(defagent B instance-agent
  :type     (:instance :relation)
  :inst-of  P
  :slot1
    :type     :aspect
    :inst-of  (P . :slot1)
    :c-coref  b1
  :slot2
    :type     :aspect
    :inst-of  (P . :slot2)
    :c-coref  b2
  :slot3
    :type     :aspect
    :inst-of  (P . :slot3)
    :c-coref  b3
  :slot4
    :type     :aspect
    :inst-of  (P . :slot1)
    :c-coref  b4
)

(defagent a1 instance-agent :type nil)
(defagent a2 instance-agent :type nil)
(defagent a3 instance-agent :type nil)
(defagent b1 instance-agent :type nil)
(defagent b2 instance-agent :type nil)
(defagent b3 instance-agent :type nil)
(defagent b4 instance-agent :type nil)


(defagent hypoth embryo-hypoth-agent
  :type   :hypoth
  :slot1
    :type     :aspect
    :c-coref  A
  :slot2
    :type     :aspect
    :c-coref  B
  :slot3
    :type     :aspect
    :c-coref  C
)

(dolist (ag '(P A B hypoth a1 a2 a3 b1 b2 b3 b4))
  (setf (agent-activation (find-agent ag))
        1.0) )


;; If you now evaluate the form:
;;  (let ((*ignore-suspensions* t))
;;    (trigger-symbolic-processor #$hypoth :just-created))
;;
;; the following hypotheses should be created (in order):
;;   p<==>p, b1<==>a1, b4<==>a1, 
;;   b2<==>a2, b3<==>a2, b2<==>a3, b3<==>a3.
|#

;;;;;;;  End of file AMBR/STR_CORR.LSP
