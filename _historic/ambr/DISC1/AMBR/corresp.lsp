;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/corresp.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Correspondence agents (e.g. MILK<-->WATER).
;;; DEPENDS-ON: DUAL; AMBR/defs.lsp, AMBR/ambr_ag.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    23-12-97
;;; UPDATED:    07-05-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...
;;;;;;;;
;;; TO DO:      Finish the documentation.
;;; TO DO:      Consider a fifth slot to corr-agents -- CORR-OFFSPRING -- that
;;;             keeps the hypotheses generated via structure correspondence.


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;     C O R R E S P O N D E N C E   A G E N T S     ;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR-CORE")

;;;; The key concept defined in this file is CORR-AGENT -- an agent representing
;;;  the correspondence between two instances or concepts.  The correspondence
;;;  agent may be regarded as data structure keeping information about the
;;;  mapping between the two.
;;;
;;;  One particular type of correspondence agents -- the so-called HYPOTHESIS
;;;  AGENTS -- are major participants in the 'constraint satisfaction network'.
;;;  (See AMBR/HYPOTH.LSP and AMBR/CSNET.LSP for further description of them.)
;;;
;;;  By convention, corr-agents have the character sequences "<-->" or "<==>"
;;;  in their names.  For example: WATER-1<-->MILK-2, WATER<==>MILK, etc.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: corresp-elt,    *driver-elt-label*, *recipient-elt-label*,
;;          corresp-justif, add-corresp-justif, *justification-label*,
;;          corresp-driver, add-corresp-driver, *driver-situation-label* ;
;;          equivalent-corresp-p, trivial-corresp-p, double-trivial-weights,
;;          corresp-type
;;
;; The functions in this file depend on the parameters *HYPOTH->MP-MENTOR-
;; WEIGHT*, *HYPOTH->SITUATION-WEIGHT*, and *HYPOTH->SC1-MENTOR-WEIGHT* from
;; AMBR/DEFS.LSP
;;
;; (Note: Earlier versions of AMBR (prior to 2.1) used 'hypothesis terminology')
;; (      instead of 'correspondence terminology'.  Thus, the current function )
;; (      CORRESP-ELT used to be HYPOTHESIS-ELT, CORRESP-JUSTIF used to be     )
;; (      HYPOTHESIS-JUSTIFICATION, etc.  It was judged, however, that these   )
;; (      functions are applicable to all kinds of correspondences. Therefore, )
;; (      they were moved from AMBR/HYPOTH.LSP to a separate file (this file). )


;;;;;  ***** CORRESPONDENCES AS DATA STRUCTURES *****
;;
;;  Each correspondence agent keeps four pieces of information -- the two
;;  agents being mapped, the justification for that mapping, and the 'driver
;;  situation'.  The micro-frame of each corr-agent has four specific slots
;;  which keep (in their :C-COREF facet) the following information:
;;    -- the agent being mapped from the driver situation
;;    -- the agent being mapped from the recipient situation
;;    -- the justification -- a list of connectionist references
;;    -- the 'driver situation' -- a situation-agent or NIL
;;
;;  These four fields are kept in four S-slots.  For the sake of data
;;  abstraction, the exact S-labels are hidden behing the constants
;;  *DRIVER-ELT-LABEL*, RECIPIENT-ELT-LABEL*, *JUSTIFICATION-LABEL*, and
;;  *DRIVER-SIT-LABEL*.
;;
;;  The justification field merits further comment.
;;  When a hypothesis is created by the marker-passing mechanism, the justi-
;;  fication field contains a reference to the marker-intersection node.
;;  When a hypothesis is created by the structure-correspondence mechanism,
;;  the justification field contain a reference to the superordinate hypoth.
;;  There may be more than one justifications (e.g. two marker intersections
;;  and a superordinate hypothesis). In this case, the justification field
;;  contains a list of references.
;;  The connectionist aspect of the hypothesis agent sends (and receives)
;;  excitatory activation to all justification agents.
;;
;;  When there are duplicate hypotheses, the newer one resigns in favor of
;;  the older one. The justifications of the two hypotheses are combined.
;;  (Duplicates are detected by inquiring secretaries, see below.)
;;
;;  The driver field also merits further comment ...
;;  It is either a situation-agent (see AMBR/SITUATN.TXT) or NIL...
;;  ... It is kept in a separate slot for two reasons: (i) speeds up the
;;      access and (ii) it is indispensable for hypotheses involving
;;      concept-agents (rather than instance-agents).

;;
;; ...

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric corresp-elt  (corr-agent elt-label)
  (:documentation "One of the two agents being mapped." ))

(defgeneric corresp-justif  (corr-agent &optional symref-only-p)
  (:documentation  "The list of justifications for a correspondence agent." ))

(defgeneric corresp-driver (corr-agent &optional first-only-p)
  (:documentation "The driver situation(s) of a correspondence agent, if any."))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

;;;;;;  Readers for the fields of correspondence-agents
;;

(defconstant *driver-elt-label* :slot1
  "In corresp.agents, the slot keeping the elt from the driver situation." )
(defconstant *recipient-elt-label* :slot2
  "In corresp.agents, the slot keeping the elt from the recipient situation." )
(defconstant *justification-label* :slot3
  "In corresp.agents, the slot keeping the justification." )
(defconstant *driver-sit-label* :slot4
  "In corresp.agents, the slot keeping the driver situation." )

(defmethod corresp-elt ((corresp corr-agent) (elt-label symbol))
  (declare (values (or AMBR-agent null)))
  #+:DUAL-DEBUG 
  (unless (or (eq elt-label *driver-elt-label*)
              (eq elt-label *recipient-elt-label*))
    (error "CORRESP-ELT: Bad element label: ~S." elt-label))
  (get-filler-refs! corresp elt-label :c-coref T) )

(defmethod corresp-elt ((symref cons)         ; extended-symref
                        (elt-label symbol))
  (declare (ignore elt-label)
           (values (or AMBR-agent null)))
  (unless (and (extended-symref-p symref)
               (typep (symref-agent symref) 'corr-agent) )
    (error "CORRESP-ELT: ~S is not an extended symbolic reference." symref ))
  (corresp-elt (symref-agent symref) (symref-slot symref)) )

(defmethod corresp-elt ((x t) (y t))
  (error "CORRESP-ELT: invalid argument(s) ~S and ~S." x y ))


(defmethod corresp-justif ((corresp corr-agent)
                           &optional (symref-only-p T) )
  (declare (values list))    ; of symrefs or conrefs
  (if symref-only-p
      (get-filler-refs! corresp *justification-label* :c-coref NIL)
      (get-filler       corresp *justification-label* :c-coref NIL) ))

(defmethod corresp-justif ((x t) &optional (symref-only-p T) )
  (declare (ignore symref-only-p))
  (error "CORRESP-JUSTIF: ~S is not a correspondence agent." x ))


(defmethod corresp-driver ((corresp corr-agent) &optional (first-only-p T))
  (declare (values (or null instance-agent list)))
  (get-filler-refs! corresp *driver-sit-label* :c-coref first-only-p) )

(defmethod corresp-driver ((x t) &optional first-only-p)
  (declare (ignore first-only-p))
  (error "CORRESP-DRIVER: ~S is not a correspondence agent." x ))


;;;;;;  Writers for the fields of correspondence-agents
;;
;; There is no ADD-CORRESP-ELT because the two elements are supplied at
;; the moment of creating the correspondence and do not change ever since.
;;
;; See *HYPOTH->MP-MENTOR-WEIGHT*, *HYPOTH->SC1-MENTOR-WEIGHT*, and
;; *HYPOTH->SITUATION-WEIGHT* in AMBR/DEFS.LSP.

(defun add-corresp-justif (corr new-justification weight
                                                  &key (priority #'max) )
  "Adjoins NEW-JUSTIFICATION to the justification list of a corresp. agent."
  (declare (values list)    ; augmented conref-list
           (type corr-agent corr)
           (type number weight)
           (type (or (member :new :old) function) priority)
           (type AMBR-agent new-justification) )
  (unless (dead-agent-p new-justification)
    (add-link  corr                    ; host micro-frame
               *justification-label*   ; S-slot label
               new-justification       ; reference of the new link
               weight                  ; weight of the new link
               :facet-label :c-coref   ; facet label
               :priority    priority   ; see ADJOIN-CONREF in DUAL/ARCHIT/CONREF
               :order       :fifo )))  ; append at back of the list

(defun add-corresp-driver (corr situation weight &key (priority #'max) )
  "Adjoins SITUATION to the driver list of a correspondence agent."
  (declare (values list)    ; augmented conref-list
           (type corr-agent corr)
           (type number weight)
           (type (or (member :new :old) function) priority)
           (type AMBR-agent situation) )
  (unless (dead-agent-p situation)
    (add-link  corr                    ; host micro-frame
               *driver-sit-label*      ; S-slot label
               situation               ; reference of the new link
               weight                  ; weight of the new link
               :facet-label :c-coref   ; facet label
               :priority    priority   ; see ADJOIN-CONREF in DUAL/ARCHIT/CONREF
               :order       :fifo )))  ; append at back of the list


;;;;;;  Checking the TYPE slot of correspondence elements
;;
;;  See AGENT-TYPE in AMBR/KREPRES.LSP
;;  Note that   (corresp-type corr ...)  checks the type of CORR's elements
;;       while  (agent-type   corr ...)  checks the type of CORR itself.

(defun corresp-type (corresp yes-tags &optional no-tags)
  "Does CORRESP involve agents satisfying AGENT-TYPE with YES- and NO-TAGS?"
  (declare (type corr-agent corresp)
           (type (or symbol list) yes-tags no-tags)  ; e.g. :instance, :relation
           (values boolean) )
  (and (agent-type (corresp-elt corresp *driver-elt-label*)
                   yes-tags  no-tags)
       (agent-type (corresp-elt corresp *recipient-elt-label*)
                   yes-tags  no-tags)) )


;;;;;;  Some additional functions and predicates
;;

(defun equivalent-corresp-p (c1 c2)
  "Do C1 and C2 denote the same correspondence between agents?"
  (declare (type corr-agent c1 c2)
           (values boolean) )
  (let ((c1.d (corresp-elt c1 *driver-elt-label*))
        (c1.r (corresp-elt c1 *recipient-elt-label*))
        (c2.d (corresp-elt c2 *driver-elt-label*))
        (c2.r (corresp-elt c2 *recipient-elt-label*)))
    (and c1.d c1.r c2.d c2.r      ; all elements defined?
         (eq c1.d c2.d)
         (eq c1.r c2.r)) ))       ; NOTE: water<==>milk /= milk<==>water


(defun trivial-corresp-p (corresp)
  "Do CORRESP involve identical elements (e.g. water<==>water)?"
  (declare (type corr-agent corresp)
           (values boolean))
  (let ((elt1 (corresp-elt corresp *driver-elt-label*))
        (elt2 (corresp-elt corresp *recipient-elt-label*)) )
    (and elt1 elt2                ; all elements defined?
         (eq elt1 elt2)) ))

(defun double-trivial-weights (corresp)
  "Double the weight from a trivial CORRESP to its single element."
  (declare (type corr-agent corresp))
  #+:DUAL-DEBUG (assert (trivial-corresp-p corresp))
  (flet ((do-one (slot notify-p)
           (add-link corresp  slot
                              (corresp-elt corresp slot)
                              (* 2.0 *hypoth->element-weight*)   ; sic
                              :facet-label :c-coref
                              :priority    :new
                              :notify-p    notify-p) ))
    (do-one *driver-elt-label*  nil)    ; avoid notifying twice
    (do-one *recipient-elt-label* T) ))
 ;; Related to DOUBLE-HYPOTHESIS-WEIGHT from AMBR/SECRETAR.LSP.
 ;; This is a job for MODIFY-LINK-WEIGHT but it is not defined yet.
 ;; See DUAL/ARCHIT/LINKS.LSP and DUAL/TO_DO.TXT.


;;;;;;;  End of file AMBR/CORRESP.LSP
