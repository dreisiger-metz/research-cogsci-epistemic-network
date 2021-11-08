;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/marker.lsp
;;; VERSION:    2.2.2  ; see AMBR/VERSION.LSP
;;; PURPOSE:    Marker-passing mechanism in AMBR.
;;; DEPENDS-ON: DUAL, particularly DUAL/archit/mp_agent.lsp;  ambr/defs.lsp,
;;;             ambr/proclaim, =/krepres, =/ambr_ag, =/corresp, and =/goalinpt
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    18-05-97
;;; UPDATED:    28-04-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;     M A R K E R   P A S S I N G   M E C H A N I S M     ;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR-CORE")

;;;; The key concepts defined in this file are AMBR-MARKER, MARKER-INTERSECTION-
;;;; REPORT, and MP-AMBR-AGENT. ...
;;;;
;;;; The class AMBR-MARKER inherits from the base class MARKER defined in
;;;; DUAL/ARCHIT/MP_AGENT.LSP.  It has two subclasses -- T-MARKER and B-MARKER.
;;;;  + T-MARKERs are the ordinary markers known from previous versions (prior
;;;;      to ver. 2.2.3).  They are emitted spontaneously by instance-agents
;;;;      upon entering WM.
;;;;  + B-MARKERs are a special kind of markers used by transfer mechanisms
;;;;      to create general hypotheses for base-to-target skolemization.
;;;;      They are emitted explicitly by 'unmapped' base elements (see AMBR/
;;;;      UNMAPPED.LSP) and intersect only with T-markers whose owners are
;;;;      free-standing instance-agents (i.e. general propositions or prototype
;;;;      instances, see AMBR/SKOLEM.TXT for terminology).
;;;;
;;;; The names T- and B-MARKER are chosen to reflect the difference between
;;;; T-DRIVER and B-DRIVER tags (see AMBR/GOALINPT.LSP and AMBR/???.LSP).


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: MP-symbolic-structure, AMBR-marker, T-marker, B-marker,
;;          agent-marker-color, send-T-marker, send-B-marker ;
;;
;;          marker-intersection-report, MP-X-report-p, make-MP-X-report,
;;          MPX-intersection, MPX-driver-path, MPX-recipient-path,
;;          track-slot-in-path
;;
;; Depends on *MP-mentor-homog-weight, (not yet *MP-mentor-heterog-weight*,)
;;  *hypoth->MP-mentor-weight*, *hypoth->element-weight*, *hypoth->situation-
;;  weight*, and *element->hypoth-weight* defined in AMBR/DEFS.LSP.
;; Inherits many methods from DUAL/ARCHIT/MP_AGENT.LSP.

;; ...

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric agent-marker-color (agent)
  (:documentation "The color of the marker emitted by the agent." ))
  ;; See AGENT-SITUATION from AMBR/KREPRES.LSP.

;; See DUAL/ARCHIT/MP_AGENT.LSP.

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;  **********    AMBR  MARKERS   *********
;;;
;;;  This portion of the file defines the base class AMBR-MARKER and its two
;;;  subclasses -- T-MARKER and B-MARKER.
;;;  AMBR markers inherit all their slots and most of their methods from
;;;  the general (DUAL) class MARKER (see DUAL/ARCHIT/MP_AGENT.LSP).
;;;  The new classes are provided as hooks for some specialized methods and
;;;  in particular for COMPLEMENTARY-MARKER-P.
;;;
;;;  The function MARKER-TYPE (see DUAL/ARCHIT/MP_AGENT.LSP) distinguishes the
;;;  two kinds.  It returns the symbol T-MARKER or B-MARKER respectively.

(eval-when (compile load eval)
  (defclass  MP-symbolic-structure (symbolic-structure)
    ()
    (:documentation
    "Symbolic structure related to the marker-passing mechanism. Base class." ))

  (defclass AMBR-marker  (marker MP-symbolic-structure)
    ()  ; no direct slots, all slots are inherited
    (:documentation  "Marker with a situation-agent serving as color." ))

  (defclass T-marker  (AMBR-marker)
    ()  ; no direct slots, all slots are inherited
    (:documentation  "AMBR marker generating target-to-base hypotheses." ))

  (defclass B-marker  (AMBR-marker)
    ()  ; no direct slots, all slots are inherited
    (:documentation  "AMBR marker generating base-to-general hypotheses." ))
) ; eval-when


;;;;;;  Predicates on (pairs of) markers.
;;
;;  T- and B-markers differ mainly with respect to COMPLEMENTARY-MARKERS-P.
;;  (The other difference is in the MP-X reports that they generate, see below.)
;;  See DUAL/ARCHIT/MP_AGENT.LSP for the generic functions definitions.

(defmethod  equivalent-markers-p ((marker1 AMBR-marker) (marker2 AMBR-marker))
  (declare (values boolean))
  (and (eq  (marker-type   marker1) (marker-type   marker2))
       (eql (marker-origin marker1) (marker-origin marker2))
       (eql (marker-color  marker1) (marker-color  marker2)) ))


;; The complementarity relation depends on the type of the two markers:
;;  + TT -- Two T-markers are complementary iff:
;;            TT1. they have different origins,
;;            TT2. they have different 'colors' (i.e. T-driver situations), and
;;            TT3. at least one 'color' satisfies T-DRIVER-P (AMBR/GOALINPT.LSP)
;;                 ('Colors' are actually situation-agents, cf.AMBR/SITUATN.TXT)
;;  + BB -- Two B-markers are never complementary.
;;  + TB -- A T-marker is complementary to a B-marker (or vice versa) iff:
;;            TB1. they have different origins and
;;            TB2. the 'color' of the T-marker is NIL (which means that its
;;                 origin is a 'free-standing' instance, see AMBR/SITUATN.TXT).
;;                 Note that the 'color' of the B-marker is never NIL; it is a
;;                 situation-agent satisfying T-DRIVER-P (NB: not B-driver-p).

(defmethod  complementary-markers-p ((marker1 T-marker)     ; TT
                                     (marker2 T-marker))    ; (this was the  )
  (declare (values boolean))                                ; (only method in)
  (let ((color1 (marker-color marker1))                     ; (older versions)
        (color2 (marker-color marker2)))
    (and (not (eq (marker-origin marker1) (marker-origin marker2)))    ; TT1
         (not (eq color1 color2))                                      ; TT2
         (or  (T-driver-p color1)                                      ; TT3
              (T-driver-p color2))
         T)))

(defmethod  complementary-markers-p ((marker1 B-marker)     ; BB
                                     (marker2 B-marker))
  nil)  ; two B-markers are never complementary

(defmethod  complementary-markers-p ((T-marker T-marker)    ; TB
                                     (B-marker B-marker))
  (declare (values boolean))
  (and (not (eq (marker-origin T-marker) (marker-origin B-marker)))    ; TB1
       (null (marker-color T-marker))                                  ; TB2
       T))

(defmethod  complementary-markers-p ((B-marker B-marker)    ; BT --> TB
                                     (T-marker T-marker))
  (declare (values boolean))
  (and (not (eq (marker-origin T-marker) (marker-origin B-marker)))    ; TB1
       (null (marker-color T-marker))                                  ; TB2
       T))



;;;;;;  **********  EMISSION OF T-MARKERS  *********
;;
;; The global marker-passing process (and, for that matter, the whole process
;; of mapping) starts by spontaneous emission of markers.  The only type of
;; agents that emit markers spontaneously in AMBR are the instance-agents.
;; The only type of markers emitted spontaneously are T-markers.
;;
;; Instance agents emit a T-marker whenever they enter the working memory.
;; (In other words, the retrieval mechanism triggers the mapping mechanism.)
;; The marker is sent to the concept-agent of which the instance belongs.
;; The relevant concept is stored in the :INST-OF slot and is retrieved via
;; INSTANCE->CONCEPT (see AMBR/KREPRES.LSP).  No marker is emitted if the
;; :INST-OF slot is empty.  (The slot should be empty only for just-created
;; instance-agents.  Otherwise it should point to exactly one concept-agent.)
;;
;; The 'color' of the marker is filled by the situation-agent to which the
;; instance-agent in question is affiliated.  See AMBR/SITUATN.TXT for more
;; details on situation-agents and affiliation.
;;
;; In the new versions of AMBR [2.2.0+] instance-agents are not marker-passing
;; agents.  They do not have the procedural knowledge for handling markers
;; and reporting marker intersections.  In particular, they do not give rise
;; to new agents and hence do not need the 'elaborate symbolic microcycle'
;; of NCR-sending agents (see DUAL/ARCHIT/NC_AGENT.LSP and AMBR/AMBR_AG.LSP).
;;
;; The global variable *MARKER-EMISSION-FLAG* (defined in AMBR/DEFS.LSP)
;; allows to block the marker-passing mechanism. Thus one can study the
;; behavior of the model with and without mapping.
;;
;; Note that B-markers are not emitted in this way.  Note also the big
;; differences between the functions SEND-T-MARKER and SEND-B-MARKER below.

(defmethod trigger-symbolic-processor ((agent instance-agent)
                                       (event (eql :enter-WM)) )
  (let ((concept-agent (instance->concept agent
                                  :cerror-p nil)))  ; see AMBR/KREPRES.LSP
    (unless (null concept-agent)                    ; just created [Skolem] ?
      (send-T-marker agent concept-agent) )))

(defun send-T-marker (instance concept)
  "INSTANCE sends a T-marker to its respective CONCEPT."
  ;; ASSUMPTION: (eq concept (instance->concept instance))
  (declare (type instance-agent instance)
           (type concept-agent concept)
           (values S-PROGN-return-type) )
  (when *marker-emission-flag*
    (unless (active-processor-p instance) ; prepare to handle the input zone in
      (symbolic-microcycle instance))     ; case sth comes while sending marker
    (s-progn instance
       (let ((marker [make-marker instance 'T-marker
                                  :color (agent-marker-color instance)
                                  :path  (list instance)] ))
         (unless [agent-visible-p concept]
           (s-eval *default-AMBR-SEND-consumption* nil)  ; wait a bit if needed
           (unless [agent-visible-p concept]
             (s-eval *default-AMBR-SEND-consumption* nil)))   ; wait a bit more
         [receive-symbol concept marker] ))))                       ; go ahead


(defmethod agent-marker-color ((agent instance-agent))
  (agent-situation agent) )           ; defined in AMBR/KREPRES.LSP

(defmethod agent-marker-color ((x t))
  (cerror "Return NIL and continue."
    "AGENT-MARKER-COLOR: ~S is not an instance and hence has no affiliation."
    x ))


;;;;;;  Emission of B-markers
;;
;;  B-markers are not emitted spontaneously.  Rather, they are explicitly sent
;;  by the transfer mechanism while trying to do base-to-target skolemization.
;;  See AMBR/UNMAPPED.LSP and AMBR/???.LSP for details.
;;
;;  Note that SEND-B-MARKER takes three arguments and is not suspendable.
;;  Thus it is quite different from SEND-T-MARKER above.

(defun send-B-marker (instance concept marker-color)
  "INSTANCE sends a B-marker to its respective CONCEPT."
  ;; ASSUMPTION: (eq concept (instance->concept instance))
  (declare (type instance-agent instance marker-color)
           (type concept-agent concept)
           (values B-marker) )
  (receive-symbol concept (make-marker instance 'B-marker
                                       :color marker-color
                                       :path  (list instance))))



;;;;;;  **********    MARKER-INTERSECTION REPORTS   *********
;;;
;;;  This portion of the file defines the class MARKER-INTERSECTION-REPORT.
;;;  Marker intersection reports (or MP-X-reports for short) are symbolic
;;;  structures exchanged between agents and carrying information about
;;;  marker intersections.
;;;  Used during top-down structure-correspondence (see AMBR/STR_CORR.LSP).
;;;
;;;  Note that the same MP-X-reports are used for both T- and B-markers.
;;;  There are two cases -- TT and TB -- which are treated uniformly by the
;;;  program but merit separate documentation:
;;;    + TT -- Both markers are T-markers (the typical case).  The origin of
;;;             DRIVER-MARKER satisfies T-DRIVER-P, see COMPLEMENTARY-MARKERS-P.
;;;    + TB -- DRIVER-MARKER is a T-marker and RECIPIENT-MARKER is a B-marker.
;;;             The origin of DRIVER-MARKER is NIL, see COMPLEMENTARY-MARKERS-P.
;;;             (Note that the variable names are somewhat misleading in this )
;;;             (case. It is the B-marker that actually 'drives' the process. )
;;;             (These names are chosen to match CORRESP-ELT slot labels.     )
;;;             (See *DRIVER- vs. *RECIPIENT-ELT-LABEL* in  AMBR/CORRESP.LSP. )
;;;  Note that the other two combinations never happen:
;;;    - BB -- Two B-markers never satisfy COMPLEMENTARY-MARKERS-P (see above).
;;;    - BT -- REPORT-MARKER-INTERSECTION (see below) takes care to put the
;;;            T-marker in the DRIVER-MARKER position.

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass marker-intersection-report (MP-symbolic-structure)
    ((intersection    :reader    MPX-intersection
                      :type      MP-AMBR-agent
                      :initarg   :intersection
                      :initform  (required-argument) )
     (driver-path     :reader    MPX-driver-path
                      :type      list
                      :initarg   :driver-path
                      :initform  (required-argument) )
     (recipient-path  :reader    MPX-recipient-path
                      :type      list
                      :initarg   :recipient-path
                      :initform  (required-argument) )
    )
    (:documentation "A symbolic structure reporting a marker intersection." ))
) ; eval-when


;;;;  Constructor (and path analyzer)

(defun  make-MP-X-report (driver-marker recipient-marker)
  "Constructor for marker intersection reports"
  (declare (values marker-intersection-report)
           (type T-marker driver-marker)
           (type AMBR-marker recipient-marker) )    ; T- or B-marker
  #+:DUAL-DEBUG
  (unless (complementary-markers-p driver-marker recipient-marker)
    (error "MAKE-MP-X-REPORT: ~S and ~S are not complementary."
           driver-marker recipient-marker ))
  (flet ((extract-intersection ()
           (let ((intersection-1 (first (marker-path driver-marker)))
                 (intersection-2 (first (marker-path recipient-marker))) )
             (if (and (not (null intersection-1))           ; empty path?
                      (eq intersection-1 intersection-2))
                 intersection-1
                 (error "MAKE-MP-X-REPORT: ~S and ~S does not intersect."
                        driver-marker recipient-marker))) ))
    (make-instance 'marker-intersection-report
                   :intersection (extract-intersection)
                   :driver-path (analyze-marker-path driver-marker)
                   :recipient-path (analyze-marker-path recipient-marker)) ))

(defun  analyze-marker-path (marker)
"Compute a list of slot-to-slot correspondences on the basis of MARKER's path."
  (declare (type AMBR-marker marker)
           (values list) )
  (let ((origin (marker-origin marker))
        (path (reverse (marker-path marker))) )
    (unless (eq (first path) origin)
      (error "ANALYZE-MARKER-PATH: Illegal marker path: ~S. ~
              (It does not begin with marker's origin ~S.)" 
             path origin))
    (let ((result (list origin)))
      (dolist (S-slot (agent-S-slots origin) (nreverse result))
        (push (cons (slot-label S-slot)
                    (track-slot-in-path (slot-label S-slot) path))
              result))) ))

(defun track-slot-in-path (S-label path)
  "Follow the inheritance chain for a given S-slot in PATH."
  (declare (type symbol S-label)
           (type cons path)   ; non-empty list of nodes
           (values symbol) )  ; S-slot-label or NIL
  (if (= 1 (length path))
      S-label
      (let* ((curr-node (first  path))
             (next-node (second path))
             (superordinates (get-slot-superordinates    ; see AMBR/KREPRES.LSP
                                       curr-node S-label))
             (next-symref (find next-node superordinates :key #'symref-agent)) )
        (cond ((null next-symref) nil)           ; superordinate chain broken
              ((simple-symref-p next-symref)
                   #+:DUAL-DEBUG
                   (warn "Bad (simple-symref) superordinate ~S for ~S.~A"
                         next-symref curr-node S-label )
                   nil)
              ((extended-symref-p next-symref)      ; this level traversed OK
                   (track-slot-in-path (symref-slot next-symref)    ; recurse
                                       (rest path)))) )))


;;;;;;  Printing methods

(defmethod  print-object ((MP-X marker-intersection-report) stream)
  (if (and (slot-boundp MP-X 'intersection)
           (agentp (MPX-intersection MP-X)) )
      (format stream "#<MP-X ~A>" (agent-name (MPX-intersection MP-X)))
      (format stream "#<malformed MP-X>") ))


(defmethod DUAL-describe ((MP-X marker-intersection-report)
                          &optional (stream *standard-output*) )
  (format stream "~&~S is a ~A.~%"
                 MP-X  (type-of MP-X) )
  (format stream "~&  Its node of intersection is: ~S~%"
          (if (slot-boundp MP-X 'intersection)
              (MPX-intersection MP-X)
              "(unbound)"))
  (format stream "~&  Its first path is: ~S~%"
          (if (slot-boundp MP-X 'driver-path)
              (MPX-driver-path MP-X)
              "(unbound)"))
  (format stream "~&  Its second path is: ~S~%"
          (if (slot-boundp MP-X 'recipient-path)
              (MPX-recipient-path MP-X)
              "(unbound)"))
  (values))


;;;;;;  Type predicate

(declaim (inline MP-X-report-p))
(defun MP-X-report-p (thing)
  (and (typep thing 'marker-intersection-report) T))


;;;;  Type-checking methods for the accessors

(defmethod  MPX-intersection ((x t))
  (error "MPX-INTERSECTION:  ~S is not a marker intersection report." x) )

(defmethod  MPX-driver-path ((x t))
  (error "MPX-DRIVER-PATH:  ~S is not a marker intersection report." x) )

(defmethod  MPX-recipient-path ((x t))
  (error "MPX-RECIPIENT-PATH:  ~S is not a marker intersection report." x) )



;;;;;;; ******    M A R K E R - P A S S I N G   A G E N T S    ******
;;;
;;;  Most of the methods are inherited from MP-DUAL-AGENTs (see DUAL/ARCHIT/
;;;  MP_AGENT.LSP and in particular the following methods:
;;;   + HANDLE-SYMBOL -- triggers and coordinates the whole process of
;;;       receiving and handling a marker
;;;   + PREPROCESS-NEW-MARKER -- checks whether the marker is really new and
;;;       stores it in the buffer
;;;   + GO-THROUGH-OLD-MARKERS -- checks for marker intersections and
;;;       triggers REPORT-MARKER-INTERSECTION on all marker pairs that
;;;       satisfy COMPLEMENTARY-MARKERS-P
;;;   + REPORT-MARKER-INTERSECTION -- formulates a node-construction request
;;;       on the basis of a marker intersection.
;;;
;;;  Reporting a marker intersection is the only aspect of the marker-passing
;;;  process that is specific to MP-AMBR agents. Therefore, only methods for
;;;  REPORT-MARKER-INTERSECTION need be defined here.  All other methods are
;;;  inherited from MP-DUAL-agents (see DUAL/ARCHIT/MP_AGENT.LSP).


;;  REPORT-MARKER-INTERSECTION takes the following assumptions for granted:
;;   A1. The pair of markers satisfies COMPLEMENTARY-MARKERS-P.
;;   A2. The origins of both markers are visible (checked by caller).
;;   A3. The terminal nodes of both paths are the same (i.e. markers intersect).
;;
;;  REPORT-MARKER-INTERSECTION carefully orders the two markers before passing
;;  them to FORMULATE-MP-NCR because this order will (through the mediation of
;;  MAKE-MP-X-REPORT) determine the CORRESP-ELT order of the hypothesis
;;  that will be constructed on the basis of the intersection -- 'driver marker'
;;  goes to *DRIVER-ELT-LABEL* and 'recipient marker' to *RECIPIENT-ELT-LABEL*
;;  (see AMBR/CORRESP.LSP).
;;  The order depends on the type of the markers and on T-DRIVER-P:
;;   # TT. When both markers are T-markers, the 'color' of at least one of them
;;         must satisfy T-DRIVER-P (see AMBR/GOALINPT.LSP).  This follows from
;;         the fact that the pair of markers satisfies COMPLEMENTARY-MAKRERS-P.
;;     + TT0. When neither marker color satisfies  T-DRIVER-P, signal an error.
;;     + TT1. When one satisfies and the other not, put the one that satisfies
;;            as DRIVER-MARKER (i.e. as first argument to FORMULATE-MP-NCR).
;;     + TT2. When both marker colors satisfy T-DRIVER-P, issue two
;;            symmetrical NC-requests.
;;   # TB. With heterogeneous markers, always put the T-marker as 'driver' and
;;         the B-marker as 'recipient'.
;;         (Note that the terminology is somewhat misleading in this case as )
;;         (it is actually the B-marker that drives the process. 'Target-    )
;;         (position-marker' vs. 'base-pos...' would be better terminology.  )
;;   # BB. Two B-markers never satisfy COMPLEMENTARY-MARKERS-P. Signal an error.

(eval-when (compile load eval)
  (proclaim-consumption 'report-two-intersections :explicit-S-PROGN) )

(defmethod report-marker-intersection ((intersection MP-AMBR-agent)    ; TT
                                       (old-marker T-marker)
                                       (new-marker T-marker) )
  (declare (values S-PROGN-return-type))
  (s-progn intersection
     (let* ((old-driver-p [T-driver-p (marker-color old-marker)])
            (new-driver-p [T-driver-p (marker-color new-marker)])
            (flags (list old-driver-p new-driver-p)) )
       (cond ((equal flags '( t  nil))                                 ; TT1
                 [send-NC-request intersection
                       [formulate-MP-NCR intersection old-marker new-marker]] )
             ((equal flags '(nil  t ))                                 ; TT1
                 [send-NC-request intersection
                       [formulate-MP-NCR intersection new-marker old-marker]] )
             ((equal flags '( t   t ))                                 ; TT2
                 [report-two-intersections intersection old-marker new-marker] )
             (t  (cerror "Do not report any marker intersection and continue."
                    "REPORT-MARKER-INTERSECTION: no T-driver among ~S and ~S."
                         old-marker new-marker)) ))))

(defmethod report-marker-intersection ((intersection MP-AMBR-agent)    ; TB
                                       (T-marker T-marker)
                                       (B-marker B-marker) )
  (s-progn intersection
    [send-NC-request intersection
          [formulate-MP-NCR intersection T-marker B-marker]] ))

(defmethod report-marker-intersection ((intersection MP-AMBR-agent)    ; BT=TB
                                       (B-marker B-marker)
                                       (T-marker T-marker) )
  (s-progn intersection
    [send-NC-request intersection
          [formulate-MP-NCR intersection T-marker B-marker]] ))

(defmethod report-marker-intersection ((intersection MP-AMBR-agent)    ; BB
                                       (old-marker B-marker)
                                       (new-marker B-marker) )
 (cerror "Do not report any marker intersection and continue."
         "REPORT-MARKER-INTERSECTION: Both ~S and ~S are B-markers."
         old-marker new-marker ))


(defun report-two-intersections (intersection marker1 marker2)         ; TT2
  ;; When both markers are drivers, make two hypotheses instead of one.
  ;; Decide which one to serve first on the basis of the activation levels.
  (declare (values S-PROGN-return-type))
  (let* ((act1 (agent-activation (marker-origin marker1)))
         (act2 (agent-activation (marker-origin marker2)))
         (marker-A (if (<= act1 act2) marker1 marker2))
         (marker-B (if (<= act1 act2) marker2 marker1)) )
    (s-progn intersection
       [send-NC-request intersection
                [formulate-MP-NCR intersection marker-A marker-B]]
       [send-NC-request intersection
                [formulate-MP-NCR intersection marker-B marker-A]] )))


(defun formulate-MP-NCR (intersection driver-marker recipient-marker)
  "Formulates a NC-request on the basis of marker intersection."
  ;; See AMBR/CORRESP.LSP for documentation on the four fields of corr-agents.
  ;; See FORMULATE-SC-NCR in AMBR/STR_CORR.LSP for structure-correspondence NCRs
  (declare (type MP-AMBR-agent intersection)
           (type AMBR-marker driver-marker recipient-marker)
           (values node-construction-request) )
  (let* ((driver-elt    (marker-origin driver-marker))
         (recipient-elt (marker-origin recipient-marker))
         (driver-sit    (determine-driver-sit-from-markers driver-marker
                                                           recipient-marker))
         (mentor-weight (if (homogeneous-states-p driver-elt recipient-elt)
                            *MP-mentor-homog-weight*       ; e.g. init<-->init
                            *MP-mentor-heterog-weight*))   ; e.g. init<-->goal
         (mentor-conref (make-conref (make-symref intersection :t-link)
                                     mentor-weight))
         (driver-elt-filler    (make-conref driver-elt
                                            *hypoth->element-weight*))
         (recipient-elt-filler (make-conref recipient-elt
                                            *hypoth->element-weight*))
         (justification-filler (make-conref intersection
                                            *hypoth->MP-mentor-weight*))
         (driver-sit-filler    (make-conref driver-sit
                                            *hypoth->situation-weight*)) )
    (make-NC-request
          mentor-conref                                           ; mentor
          'embryo-hypoth-agent                                    ; agent-type
          (MP-GENNAME-template driver-elt recipient-elt)          ; GENNAME
          (list (list *driver-elt-label*    :type    :aspect)     ; slots
                (list *recipient-elt-label* :type    :aspect)
                (list *justification-label* :type    :aspect)
                (list *driver-sit-label*    :type    :aspect)
                (list *driver-elt-label*    :c-coref driver-elt-filler   )
                (list *recipient-elt-label* :c-coref recipient-elt-filler)
                (list *justification-label* :c-coref justification-filler)
                (list *driver-sit-label*    :c-coref driver-sit-filler   ) )
          (list (make-MP-X-report driver-marker                   ; mail
                                  recipient-marker)) )))


(defun  determine-driver-sit-from-markers (driver-marker recipient-marker)
  (declare (values instance-agent))  ; situation-agent satisfying T-DRIVER-P
  (etypecase recipient-marker
    (T-marker                               ; both markers are T-markers
        (marker-color driver-marker))       ; DRIVER-MARKER should be affiliatd
    (B-marker                               ; heterogeneous marker pair
        (marker-color recipient-marker)) )) ; DRIVER-MARKER shld be free-standg



;;  Convention: Hypotheses generated by the marker-passing mechanism use
;;  the character sequence  '<-->'  in their names.  By contrast, hypotheses
;;  generated by the structure-correspondence mechanism use  '<==>'.
;;  See SC-GENNAME-TEMPLATE in AMBR/STR_CORR.LSP.
;;
(defun MP-GENNAME-template (driver recipient)
  "Construct a GENNAME template for the name of a MP-generated hypothesis."
  (list "~A<-~@[~D~]->~A"            ; see GENNAME in DUAL/ARCHIT/BASIC.LSP
        (agent-name driver)          ; e.g:  WATER3<-->MILK4   or, if used
        nil                               ;  WATER3<-1->MILK4  or, if used
        (agent-name recipient)) )         ;  WATER3<-2->MILK4  etc.


;;;;;;;  End of file AMBR/MARKER.LSP
