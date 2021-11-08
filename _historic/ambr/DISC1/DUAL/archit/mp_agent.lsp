;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/MP_agent.lsp
;;; VERSION:    1.1.2   ; see DUAL/VERSION.LSP
;;; PURPOSE:    Marker-passing abilities of DUAL agents.
;;; DEPENDS-ON: DUAL/packages.lsp, DUAL/general.lsp, DUAL/arciht/sprogn.lsp,
;;;             DUAL/archit/DUAL_ag.lsp, DUAL/archit/NC_agent.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    19-05-97 [1.0]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;;
;;; TO DO:      Document everything.
;;; TO DO:      Sort the list of MP-neighbors according to link weights.
;;; TO DO:      Garbage collection: (i) during idle waits, (ii) regularly.
;;;


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;     L O C A L   M A R K E R - P A S S I N G     ;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is MP-DUAL-AGENT -- a DUAL agent
;;;; with marker-passing capabilities.
;;;;
;;;; ...


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: marker, markerp,
;;          make-marker, copy-marker,
;;          marker-type, valid-marker-type-p,
;;          marker-color, marker-origin, marker-path,
;;          marker-origin-visible-p,
;;          complementary-markers-p, equivalent-markers-p ;
;;
;;          MP-DUAL-agent, agent-markers, agent-MP-neighbors,
;;          emit-marker, store-marker, delete-marker ;
;;
;;          preprocess-new-marker,  ;; handle-symbol
;;          go-through-old-markers, report-marker-intersection,
;;          mark-MP-neighbors
;;

;; ...

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric make-marker (origin marker-type &key color path)
  (:documentation "Constructor for markers. (Cf. COPY-MARKER)" ))

(defgeneric copy-marker (original-marker new-node-to-path)
  (:documentation
    "Constructor for markers; useful in marker-passing. (Cf. MAKE-MARKER)" ))

(defgeneric emit-marker (origin marker-type &key color path)
  (:documentation "Create and propagate a new marker starting from ORIGIN." ))

(defgeneric store-marker (host marker)
  (:documentation
   "Adds (destructively) MARKER to the end of HOST's AGENT-MARKER list." ))

(defgeneric delete-marker (host marker)
  (:documentation
   "Delete (destructively) MARKER from HOST's AGENT-MARKER list." ))

(defgeneric complementary-markers-p (marker1 marker2)
  (:documentation "Is the intersection of these two markers interesting?" ))

(defgeneric equivalent-markers-p (marker1 marker2)
  (:documentation "T if MARKER1 is equivalent to MARKER2 and NIL otherwise." ))

(defgeneric preprocess-new-marker (host marker)
  (:documentation
    "Analyze a marker to ensure that its origin is visible, etc." ))

(defgeneric go-through-old-markers (host new-marker)
  (:documentation
    "Compare NEW-MARKER with old markers and report any intersections." ))

(defgeneric report-marker-intersection (intersection old-marker new-marker)
  (:documentation
    "Request a new (hypothesis) agent on the grounds of marker intersection." ))

(defgeneric mark-MP-neighbors (sender marker)
  (:documentation
    "SENDER sends (copies of) MARKER to all agents on its MP-NEIGHBORS list." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;  **********    M A R K E R S   ***********
;;;
;;;  This portion of the file defines the class MARKER, two constructors
;;;  for markers, printing methods, etc.
;;;

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass marker  (symbolic-structure)
    ((origin   :reader      marker-origin
               :type        DUAL-agent
               :initarg     :origin
               :initform    (required-argument)  )
     (color    :reader      marker-color
               :type        t               ; e.g. integer, keyword, agent, etc.
               :initarg     :color
               :initform    (required-argument)  )
     (path     :reader      marker-path
               :type        list
               :initarg     :path
               :initform    (required-argument)  )
    )
   (:documentation "Marker -- a symbolic structure passed b/n agents." ))
) ; eval-when


;;;;  Type predicate and associated functions

(declaim (inline markerp))
(defun  markerp (thing)
  (and (typep thing 'marker) t))

(defun  marker-type (marker)
  (declare (values symbol))
  (if (markerp marker)
      (class-name (class-of marker))
      (error "MARKER-TYPE: ~S is not a marker." marker) ))

(defun valid-marker-type-p (thing)
  "Predicate that returns T iff THING is a valid marker type."
  (and (symbolp thing)
       (find-class thing nil)
       (subtypep thing 'marker)
       T ))


;;;;  Constructors

(defmethod  make-marker ((origin DUAL-agent)
                         marker-type   &key color path )
  (make-instance marker-type :origin origin
                             :color  color
                             :path   path ))

(defmethod  make-marker :before ((origin DUAL-agent)
                                 marker-type   &key color path )
  (declare (ignore origin color))
  (unless (valid-marker-type-p marker-type)
    (error "MAKE-MARKER: ~S is not a valid marker type."  marker-type ))
  (unless (listp path)
    (error "MAKE-MARKER: ~S is not a valid marker path."  path ))
) ;; end of the error-signaling before-method.

(defmethod  make-marker ((origin t) marker-type &key color path)
  (declare (ignore marker-type color path))
  (error "MAKE-MARKER: ~S is not an agent."  origin ))


(defmethod  copy-marker  ((old-marker marker)
                          (new-node-to-path DUAL-agent) )
  (make-instance (marker-type   old-marker)
         :origin (marker-origin old-marker)
         :color  (marker-color  old-marker)
         :path   (cons new-node-to-path
                       (marker-path old-marker)) ))

(defmethod  copy-marker  ((old-marker marker)
                          (dummy-node-to-path null) )
  ;; When the second parameter is NIL, make a verbatim copy.
  (make-instance (marker-type   old-marker)
         :origin (marker-origin old-marker)
         :color  (marker-color  old-marker)
         :path   (marker-path   old-marker) ))

(defmethod  copy-marker ((x t) (y t))
  (error "COPY-MARKER: ~S is not a marker or ~S is not an agent-or-NIL."
         x y ))


;;;;;;  Printing methods

(defmethod  print-object ((marker marker) stream)
  (if (slot-boundp marker 'origin)
      (format stream "#<MRK ~A>" (agent-name (marker-origin marker)))
      (format stream "#<malformed marker>") ))


(defmethod DUAL-describe  ((marker marker)
                           &optional (stream *standard-output*))
  (format stream "~&~S is a ~A.~%"
          marker (marker-type marker) )
  (format stream "~&  Its color  is: ~S~%"
          (if (slot-boundp marker 'color)
              (marker-color marker)
              "(unbound)"))
  (format stream "~&  Its origin is: ~S~%"
          (if (slot-boundp marker 'origin)
              (marker-origin marker)
              "(unbound)"))
  (format stream "~&  Its path   is: ~S~%"
          (if (slot-boundp marker 'path)
              (marker-path marker)
              "(unbound)"))
  (values))


;;;;  Type-checking methods for the accessors

(defmethod  marker-color ((x t))
  (error "MARKER-COLOR:  ~S is not a marker." x) )

(defmethod  marker-origin ((x t))
  (error "MARKER-ORIGIN:  ~S is not a marker." x) )

(defmethod  marker-path ((x t))
  (error "MARKER-PATH:  ~S is not a marker." x) )


;;;;;;  Predicates on (pairs of) markers.

(defmethod  complementary-markers-p ((marker1 marker) (marker2 marker))
  (declare (values boolean))
  (and (not (eql (marker-origin marker1) (marker-origin marker2)))
       (not (eql (marker-color  marker1) (marker-color  marker2))) ))

(defmethod  equivalent-markers-p ((marker1 marker) (marker2 marker))
  (declare (values boolean))
  (and (eql (marker-origin marker1) (marker-origin marker2))
       (eql (marker-color  marker1) (marker-color  marker2)) ))

(defmethod complementary-markers-p ((x t) (y t))
  (error "COMPLEMENTARY-MARKERS-P: ~S and/or ~S is not a marker" x y ))

(defmethod equivalent-markers-p ((x t) (y t))
  (error "EQUIVALENT-MARKERS-P: ~S and/or ~S is not a marker" x y ))



;;;;;;  *****    M A R K E R - P A S S I N G   A G E N T S    *****
;;;
;;;  This portion of the file defines the class MP-DUAL-AGENT.
;;;  MP-DUAL-AGENTs are DUAL agents with marker-passing capabilities.
;;;

;;;;;;   Class definitions  (and accessor methods)

(eval-when (compile load eval)
  (defclass MP-DUAL-agent (NCR-sending-agent DUAL-agent)
    ((MP-neighbors  :reader      agent-MP-neighbors  ; for the external protocol
                    :accessor    MP-neighbors        ; for internal use
                    :type        list                ; list of agents
                    :initform    nil )
     (markers       :reader      agent-markers       ; for the external protocol
                    :accessor    MP-markers          ; for internal use
                    :type        list                ; list of markers
                    :initform    nil )
    )
    (:documentation
      "DUAL agent with marker-passing capabilities. A mix-in class." ))
) ; eval-when

;;;;;;  Printing methods

(defmethod  agent-descriptor-string ((agent MP-DUAL-agent))
  (if (eq (type-of agent) 'MP-DUAL-agent)
      "a MP DUAL agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod DUAL-describe :after ((ag MP-DUAL-agent)
                                 &optional (stream *standard-output*))
  (format stream "~&  Its MP-neighbors are: ~S~%"
          (if (slot-boundp ag 'MP-neighbors)
              (agent-MP-neighbors ag)
              "(unbound slot)") )
  (format stream "~&  Its current markers are: ~S~%"
          (if (slot-boundp ag 'markers)
              (agent-markers ag)
              "(unbound slot)") )
  (values))


;;;;  Type-checking methods for the accessors

(defmethod  agent-MP-neighbors ((x t))
  (if x
    (error "AGENT-MP-NEIGHBORS:  ~S is not a marker-passing agent." x)
    (error "AGENT-MP-NEIGHBORS applied to NIL (perhaps #$missing-agent).") ))

(defmethod  MP-neighbors ((x t))
  (if x
    (error "DUAL-CORE::MP-NEIGHBORS:  ~S is not a marker-passing agent." x)
 (error "DUAL-CORE::MP-NEIGHBORS applied to NIL (perhaps #$missing-agent).") ))

(defmethod  (setf MP-neighbors) (new-value (x t))
  (declare (ignore new-value))
  (if x
   (error
    "(SETF DUAL-CORE::MP-NEIGHBORS):  ~S is not a marker-passing agent." x)
   (error
    "(SETF DUAL-CORE::MP-NEIGHBORS) applied to NIL (perhaps #$missing-agent).")
  ))

(defmethod  agent-markers ((x t))
  (if x
    (error "AGENT-MARKERS:  ~S is not a marker-passing agent." x)
    (error "AGENT-MARKERS applied to NIL (perhaps #$missing-agent).") ))

(defmethod  MP-markers ((x t))
  (if x
    (error "DUAL-CORE::MP-MARKERS:  ~S is not a marker-passing agent." x)
    (error "DUAL-CORE::MP-MARKERS applied to NIL (perhaps #$missing-agent).") ))

(defmethod  (setf MP-markers) (new-value (x t))
  (declare (ignore new-value))
  (if x
   (error
    "(SETF DUAL-CORE::MP-MARKERS):  ~S is not a marker-passing agent." x)
   (error
    "(SETF DUAL-CORE::MP-MARKERS) applied to NIL (perhaps #$missing-agent).") ))



;;;;;;;;  *********  Local marker-passing  **********
;;;
;;; See subsection 3.3.5. in "DUAL Report #1"
;;;

;;;;;; Handling a marker amounts to the following (see 3.3.5 in DR#1):
;;
;;  0. Pop the marker from the input queue.
;;  1. Preprocess the marker via the generic function PREPROCESS-NEW-MARKER
;;       (see below).  It returns a value that is either NIL or a marker.
;;       If the value is NIL, stop.
;;  2. If the value from step 1. is a marker, store it in the buffer.
;;  3. Go through old markers and check each one against the new one.
;;       This is done by the generic fun GO-THROUGH-OLD-MARKERS (see below).
;;       It returns a boolean indicating whether there was any intersection.
;;  4. Check whether there was at least one intersection at step 3.
;;       If yes then stop, do _not_ pass the new marker further.
;;       If no then trigger MARK-MP-NEIGHBORS.

(defmethod  handle-symbol  ((host MP-DUAL-agent) (marker marker))
  (declare (values S-PROGN-return-type))
  (s-progn host
     (suspended-value-bind (new-marker)
                [preprocess-new-marker host marker]             ; step 1.
       (unless (null new-marker)
         [store-marker host new-marker]                         ; step 2.
         (suspended-value-bind  (intersection-p)
                    [go-through-old-markers host new-marker]    ; step 3.
           (if intersection-p                                   ; step 4.
               nil      ; do not propagate new marker
               [mark-MP-neighbors host new-marker] ))))))


;;;;    Preprocessing a marker amounts to the following:
;;
;;  1. Check whether the origin of the marker is visible. If not, return NIL.
;;  2. Check for duplicates. If deja vu, return NIL.
;;     A 'duplicate' means that the new marker satisfies EQUIVALENT-MARKERS-P
;;     with some old marker stored in the buffer.
;;  3. Append self to the path of the marker usind COPY-MARKER. Return the
;;     the new marker.

(defmethod  preprocess-new-marker  ((host MP-DUAL-agent) (marker marker))
  (declare (values mailbox))    ; (s-values (or null marker))
  (s-progn host
     (cond ((not [marker-origin-visible-p marker])           ; step 1.
               (s-values nil))
           ((find marker (agent-markers host)
                         :test #'equivalent-markers-p)       ; step 2.
               (s-values nil))      ; deja vu
           (t  (s-values [copy-marker marker host])) )))     ; step 3.

(defmethod preprocess-new-marker ((x t) (y t))
  (error "PREPROCESS-NEW-MARKER: ~S is not a MP agent or ~S is not a marker."
         x y ))


(defun marker-origin-visible-p (marker)
  "Predicate that checks whether the origin of MARKER is visible."
  (declare (type marker marker)
           (values boolean) )
  (agent-visible-p (marker-origin marker)) )


;;;;    Checking for marker intersections amounts to the following:
;;
;;  1. Go through the old markers in the buffer (in FIFO order) and do the
;;     following with each one of them (call it OLD-MARKER):
;;   1a. Check whether the origin of OLD-MARKER is visible. If not, remove
;;       it from the buffer. [Garbage collection]
;;   1b. Check whether OLD-MARKER and NEW-MARKER are complementary (in the
;;       sense of COMPLEMENTARY-MARKERS-P). If they aren't, go to next old mrk.
;;       If they are, trigger REPORT-MARKER-INTERSECTION and set a flag noting
;;       that an intersection is found.
;;   1c. Go to the next old marker in the buffer.
;;  2. Return a boolean value indicating whether there was any intersection.

(defmethod  go-through-old-markers ((host MP-DUAL-agent) (new-marker marker))
  (declare (values mailbox))    ; (s-values boolean)
  (let ((intersection-p nil))
    (s-progn host
       (dolist (old-marker (agent-markers host))                  ; step 1.
         (cond ((not [marker-origin-visible-p old-marker])
                   [delete-marker host old-marker])               ; step 1a.
               ([complementary-markers-p old-marker new-marker]
                   (setq intersection-p t)                        ; step 1b.
                   [report-marker-intersection host
                                  old-marker new-marker]) ))
       (s-values intersection-p) )))                              ; step 2.

(defmethod go-through-old-markers ((x t) (y t))
  (error "GO-THROUGH-OLD-MARKERS: ~S is not a MP agent or ~S is not a marker."
         x y ))


;;;;;; Reporting a marker amounts to the following:
;;
;;  1. Make a symbolic-structure of type NODE-CONSTRUCTION-REQUEST via a
;;     call to MAKE-NC-REQUEST (defined in DUAL/ARCHIT/NC_AGENT.LSP).
;;  2. Send the NC-REQUEST constructed at step 1.
;;
;;  Step 2. is implemented by the function SEND-NC-REQUEST defined in
;;    DUAL/ARCHIT/NC_AGENT.LSP. Marker-passing agents inherit from the mix-in
;;    class NCR-SENDING-AGENT (see DUAL/ARCHIT/NC_AGENT.LSP). In other words,
;;    they have an elaborate symbolic-microcycle and a separate queue for
;;    unsent NC-requests.
;;
;;  Step 1., however, is model-specific. Therefore, only an error-signalling
;;    method for REPORT-MARKER-INTERSECTION is defined here. The 'real' methods
;;    should  be provided by other (model-specific) modules of the program.
;;    These methods may assume that the origins of both markers are visible
;;    because that has just been checked (see HANDLE-SYMBOL above).
;;    A sample method is provided to serve as a template for such methods.
;;
;; To do: Garbage collection -- periodically go through all markers in store
;;    and check whether their origins are sitll visible. If not, discard.
;;

(defmethod report-marker-intersection (x m1 m2)
  (error "REPORT-MARKER-INTERSECTION: ~S is not a marker-passing agent or ~
          some of ~S and ~S is not a marker."  x m1 m2 ))

;; Sample method

#|
(defmethod report-marker-intersection ((intersection MP-DUAL-agent)
                                       (old-marker marker) (new-marker marker) )
  ;; NEW-MARKER meets OLD-MARKER at INTERSECTION node.
  (declare (values S-PROGN-return-type)
           (inline formulate-MP-NCR) )
  (let (NC-request)  ; to be set below
    (s-progn intersection
      (setq NC-request
            (s-eval *default-INTERNAL-consumption*     ; see DUAL/PROCLAIM.LSP
               (formulate-MP-NCR intersection old-marker new-marker)))
      [send-NC-request intersection NC-request] )))

(defun formulate-MP-NCR (intersection old-marker new-marker)
  "Formulates a NC-request on the basis of marker intersection."
  (declare (values node-construction-request))
  (let ((mentor (make-conref (make-symref intersection :t-link) 1.0))
        (origin1 (marker-origin old-marker))
        (origin2 (marker-origin new-marker)) )
    (make-NC-request mentor                              ; mentor
                     'temp-DUAL-agent                    ; agent-type
                     (list "~A<-~@[~D~]->~A"             ; GENNAME template
                           (agent-name origin1) nil (agent-name origin2))
                     (list '(:type  :instance)           ; slots
                           '(:slot1 :type :aspect)
                           `(:slot1 :c-coref ,(make-conref intersection 1.0))
                           '(:slot2 :type :aspect)
                           `(:slot2 :c-coref ,(make-conref origin1 1.0))
                           '(:slot3 :type :aspect)
                           `(:slot3 :c-coref ,(make-conref origin2 1.0)) )
                     (list old-marker new-marker) )))    ; mail
|#

;; The rest is easy:

(defmethod  store-marker ((host MP-DUAL-agent) (marker marker))
  (setf (MP-markers host)
        (nconc (MP-markers host) (list marker))) )    ; FIFO order

(defmethod  delete-marker ((host MP-DUAL-agent) (marker marker))
  (setf (MP-markers host)
        (delete marker (MP-markers host))) )          ; FIFO order

(defmethod  emit-marker ((origin MP-DUAL-agent) marker-type
                                                &key color path )
  ;; Send a marker to self. Usually triggered when entering WM.
  (declare (values S-PROGN-return-type))
  (unless (active-processor-p origin)
    (symbolic-microcycle origin))   ; prepare to handle input
  (s-progn origin
    [receive-symbol origin
                    [make-marker origin marker-type
                                 :color color  :path path]] ))

(defmethod  mark-MP-neighbors ((sender MP-DUAL-agent) (marker marker))
  (declare (values S-PROGN-return-type))
  (let ((pass1 (agent-MP-neighbors sender))
        (pass2 nil) )
    (s-progn sender
       (dolist (receiver pass1)
         (if [agent-visible-p receiver]
             [receive-symbol receiver marker]
             (push receiver pass2)) )
       (dolist (receiver (nreverse pass2))     ; don't check for visibility
         [receive-symbol receiver marker]) ))) ; RECEIVE-SYMBOL ignores invisib.


(defmethod  store-marker  ((x t) (y t))
  (error "STORE-MARKER: ~S is not a MP agent or ~S is not a marker." x y ))

(defmethod  delete-marker  ((x t) (y t))
  (error "DELETE-MARKER: ~S is not a MP agent or ~S is not a marker." x y ))

(defmethod  emit-marker  ((x t) marker-type &key color path)
  (declare (ignore marker-type color path))
  (error "EMIT-MARKER: ~S is not a marker-passing agent." x ))

(defmethod  mark-MP-neighbors  ((x t) (y t))
  (error "MARK-MP-NEIGHBORS: ~S is not a MP agent or ~S is not a marker." x y ))



;;;;;;  Maintaining the MP-NEIGHBORS list up-to-date

(defmethod  establish-microframe-integrity :after ((agent MP-DUAL-agent)
                                                   justification)
  (declare (ignore justification))
  (setf (MP-neighbors agent) (collect-MP-neighbors agent)) )

(defun  collect-MP-neighbors (agent)
  "Traverse all G-slots with MP-labels, collecting all MP-references."
  (declare (type MP-DUAL-agent agent)
           (values list) )
  (let ((result '()))
    (dolist (G-slot (agent-G-slots agent) (nreverse result))
      (when (MP-label-p (slot-label G-slot))
        (dolist (symref (get-filler-refs! G-slot))
          (when (simple-symref-p symref)
            (pushnew symref result) ))))))
  ;; TO DO: Sort the list of MP-neighbors according to the weight of the links.


;;;;;;  Marker buffer is volatile memory (see DUAL/ARCHIT/SYMPROC1.LSP)

(defmethod  clear-volatile-memory :after ((agent MP-DUAL-agent))
  (setf (MP-markers agent) nil) )


;;;;;;;  End of file DUAL/ARCHIT/MP_AGENT.LSP
