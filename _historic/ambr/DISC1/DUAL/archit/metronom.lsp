;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/metronom.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Metronomes -- sources of rhytmic activities in DUAL.
;;; DEPENDS-ON: DUAL/defs.lsp, archit/dual_ag.lsp, archit/time.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    27-03-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...
;;;;;;;
;;; TO DO:      Define (and document) class RADIO-STATION with slots EVENT and
;;;             SUBSCRIBERS and make METRONOME inherit from it.

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;;     M  E  T  R  O  N  O  M  E  S      ;;;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")


;;;; The key concepts defined in this file is METRONOME...
;;;;
;;;; DUAL specification ...
;;;;
;;;; ... Compare with alarm clocks in DUAL/ARCHIT/TIME.LSP ...
;;;;   Alarm clocks have only one OWNER; metronomes have a set of SUBSCRIBERS.
;;;;   Alarm clocks sends messages via RECEIVE-SYMBOL; metronomes emit events
;;;;   (e.g. :METRONOME-PULSE) via TRIGGER-SYMBOLIC-PROCESSOR.
;;;;

;; Example:
;;   (make-metronome  'marker-GC           ; name
;;                    :marker-GC           ; event (which is a keyword symbol)
;;                    25.0                 ; time period
;;                    :comment  "For garbage collection of dead markers."
;;                    :subscribers  'MP-DUAL-agent )   ; see ARCHIT/MP_AGENT.LSP
;;
;;   ;; Define a method for handling the new event, see DUAL/ARCHIT/SYMPROC2.LSP
;;   (defmethod  trigger-symbolic-processor ((agent MP-DUAL-agent)
;;                                           (event (eql :MARKER-GC)) )
;;     (unless (active-processor-p agent)  ; prepare to handle the input zone in
;;       (symbolic-microcycle agent))      ; case sth comes during S-PROGN below
;;     (s-progn agent
;;       <remove all markers with invisible origin>... ))



;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: metronome, make-metronome,
;;          *metronomes*, find-metronome, remove-metronome ;
;;          metronome-name, metronome-comment,
;;          metronome-event, metronome-subscribers,
;;          metronome-period, metronome-time-remaining ;
;;          tick-metronome, tick-all-metronomes,
;;          run-metronome, do-all-subscribers

;; ...

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric run-metronome (metronome)
  (:documentation
    "Emit a 'metronome pulse' -- trigger the processors of all subscribers." ))

(defgeneric do-all-subscribers (subscribers event)
  (:documentation
    "Trigger the symbolic processor of all metronome subscribers." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass metronome (time-related-DUAL-object)
    ((name        :initarg        :name
                  :reader         metronome-name
                  :type           symbol
                  :initform       (required-argument) )
     (comment     :initarg        :comment
                  :accessor       metronome-comment
                  :type           (or null string)
                  :initform       nil  )
     (event       :initarg        :event
                  :reader         metronome-event
                  :type           symbol        ; e.g :METRONOME-PULSE
                  :initform       (required-argument) )
     (subscribers :initarg        :subscribers
                  :accessor       metronome-subscribers
                  :type           (or list
                                      working-memory
                                      symbol )  ; valid-DUAL-agent-type-p
                  :initform       nil  )
     (period      :initarg        :period
                  :accessor       metronome-period
                  :type           float
                  :initform       (required-argument) )
     (time-remaining
                  :accessor       metronome-time-remaining
                  :type           float
                  :initform       9999.999   )
    )
    (:documentation
      "A device that triggers a set of subscribers at regular intervals." ))
) ; eval-when


;;;;  Global registry and bookkeeping functions
;;
;;  The implementation maintains a set of active metronomes.  It is
;;  implemented as a list and is bound to the global variable *METRONOMES*.
;;
;;  Metronomes are added to the set via MAKE-METRONOME and removed from there
;;  via REMOVE-METRONOME.  Two metronomes cannot share the same name.

(defvar  *metronomes*  nil             ; a list of metronome-objects
  "The set of active metronomes." )

(defun find-metronome (name)
  "Looks for an active metronome with the specified NAME."
  (declare (values (or null metronome))
           (type symbol name) )
  (find name *metronomes* :key #'metronome-name) )

(defun remove-metronome (metronome)
  "Removes METRONOME from the set of active metronomes."
  (declare (type (or null symbol metronome) metronome)
           (values boolean) )
  (etypecase  metronome
    (null       nil)        ; (remove-metronome (find-metronome 'no-such-name))
    (symbol     (remove-metronome (find-metronome metronome)))
    (metronome  
       (prog1 
         (and (find metronome *metronomes*) T)
         (setq *metronomes* (remove metronome *metronomes*)))) ))


;;;; Constructor

(defun make-metronome (name event period &key comment subscribers
                                                      (cerror-p T) )
  (declare (type symbol name event)
           (type number period)
           (type (or null string) comment)
           (type (or list working-memory symbol) subscribers)
           (values metronome) )
  #+:DUAL-DEBUG (assert (plusp period))
  (when (and subscribers (symbolp subscribers))
    (assert (valid-DUAL-agent-type-p subscribers)))  ; see ARCHIT/DUAL_AG
  (when (find-metronome name)                        ; name collision ?
    (when cerror-p
      (cerror "Replace old with new and continue."
              "MAKE-METRONOME: There already is a metronome with name ~S."
              name ))
    (remove-metronome name) )
  (let ((new-metronome (make-instance 'metronome
                                      :name     name
                                      :event    event
                                      :period   (coerce period 'float)
                                      :comment  comment
                                      :subscribers  subscribers )))
    (setf (metronome-time-remaining new-metronome)
          (coerce period 'float))
    (push new-metronome *metronomes*)
    new-metronome ))


;;;;  Type-checking methods for the accessors.

(defmethod  metronome-name ((x t))
  (error "METRONOME-NAME:  ~S is not a metronome." x ))

(defmethod  metronome-comment ((x t))
  (error "METRONOME-COMMENT:  ~S is not a metronome." x ))

(defmethod  (setf metronome-comment) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF METRONOME-COMMENT):  ~S is not a metronome." x ))

(defmethod  metronome-event ((x t))
  (error "METRONOME-EVENT:  ~S is not a metronome." x ))

(defmethod  metronome-subscribers ((x t))
  (error "METRONOME-SUBSCRIBERS:  ~S is not a metronome." x ))

(defmethod  (setf metronome-subscribers) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF METRONOME-SUBSCRIBERS):  ~S is not a metronome." x ))

(defmethod  metronome-period ((x t))
  (error "METRONOME-PERIOD:  ~S is not a metronome." x ))

(defmethod  (setf metronome-period) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF METRONOME-PERIOD):  ~S is not a metronome." x ))

(defmethod  metronome-time-remaining ((x t))
  (error "METRONOME-TIME-REMAINING:  ~S is not a metronome." x ))

(defmethod  (setf metronome-time-remaining) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF METRONOME-TIME-REMAINING):  ~S is not a metronome." x ))


;;;;;;  Printing methods

(defmethod print-object ((metronome metronome) stream)
  (if (and (slot-boundp metronome 'name)
           (slot-boundp metronome 'event) )
      (format stream "#<METRONOME ~S ~S>"
              (metronome-name metronome)
              (metronome-event metronome) )
      (format stream "#<malformed metronome>" ) ))

(defmethod DUAL-describe ((metronome metronome)
                          &optional (stream *standard-output*))
  (format stream "~&~S is a ~A.~%"
          metronome (class-name (class-of metronome)) )
  (format stream "~&  Its name is: ~S~%"
                 (if (slot-boundp metronome 'name)
                     (metronome-name metronome)
                     "(unbound, which is anomalous)" ) )
  (format stream "~&  Its comment is: ~S~%"
                 (and (slot-boundp metronome 'comment)
                      (metronome-comment metronome)) )
  (format stream "~&  Its event is: ~S~%"
                 (if (slot-boundp metronome 'event)
                     (metronome-event metronome)
                     "(unbound, which is anomalous)" ) )
  (format stream "~&  Its subscribers are: ~S~%"
                 (if (slot-boundp metronome 'subscribers)
                     (metronome-subscribers metronome)
                     "(unbound, which is anomalous)" ) )
  (format stream "~&  Its period is: ~,2F~%"
                 (if (slot-boundp metronome 'period)
                     (metronome-period metronome)
                     "(unbound, which is anomalous)" ) )
  (format stream "~&  Its next pulse will be after ~,2F units of time.~%"
                 (if (slot-boundp metronome 'time-remaining)
                     (metronome-time-remaining metronome)
                     9999.9 ) )
  (values))



;;;;;;  *****   METRONOME  MANAGEMENT   *****
;;;
;;;  The implementation maintains a set of active metronomes in the global
;;;  variable *METRONOME* -- see above.
;;;
;;;  The job of a metronome is to send 'pulses' to its subscribers at regular
;;;  intervals. The main work is done by the generic fun DO-ALL-SUBSCRIBERS.
;;;  See also TRIGGER-SYMBOLIC-PROCESSOR in DUAL/ARCHIT/SYMPROC2.LSP.

(defmethod  do-all-subscribers  ((subscribers null)
                                 (event symbol) )
  (declare (ignore subscribers event))
  nil )

(defmethod  do-all-subscribers  ((subscribers list)
                                 (event symbol) )
  (declare (values null))
  (dolist (subscriber subscribers)
    (when (agent-visible-p subscriber)
      (trigger-symbolic-processor subscriber event) )))

(defmethod  do-all-subscribers  ((WM working-memory)
                                 (event symbol) )
  (declare (values null))
  (do*-all-wm (subscriber WM)
    (trigger-symbolic-processor subscriber event) ))

(defmethod  do-all-subscribers  ((agent-type symbol)
                                 (event symbol) )
  (declare (values null))
  (assert (valid-DUAL-agent-type-p agent-type))
  (do-all-agents (agent)
    (when (and (agent-visible-p agent)
               (typep agent agent-type))
      (trigger-symbolic-processor agent event) )))

(defmethod  do-all-subscribers  ((x t) (y t))
  (error "DO-ALL-SUBSCRIBERS: Bad argument(s) ~S and/or ~S."  x y ))


;; REMOVE-DEAD-SUBSCRIBERS is useful when METRONOME-SUBSCRIBERS is a list.
(defun remove-dead-subscribers (metronome)
  (declare (type metronome metronome))
  (let ((subscribers (metronome-subscribers metronome)))
    (when (listp subscribers)
      (setf (metronome-subscribers metronome)
            (remove-if #'dead-agent-p subscribers)) )))


;; RUN-METRONOME is generic and thus can be spied (see DUAL/INTRFACE/SPY_TOOL).
(defmethod  run-metronome ((metronome metronome))
  (declare (values symbol))           ; e.g. :METRONOME-PULSE
  (let ((event (metronome-event metronome)))
    (incf (metronome-time-remaining metronome)              ; reset the timer
          (metronome-period metronome))
    (remove-dead-subscribers metronome)                     ; collect garbage
    (do-all-subscribers (metronome-subscribers metronome)   ; do the main work
                        event)
    event ))


(defun  tick-metronome (metronome &optional (time-slice *time-slice*) )
  "Decrements the time remaining and runs the metronome if necessary."
  (declare (type metronome metronome)
           (type float time-slice)
           (values float) )      ; 0.0 means 'just run'
  (let ((new-time-remaining (decf (metronome-time-remaining metronome)
                                  time-slice)))
    (cond ((plusp new-time-remaining)  new-time-remaining)
          (t  (run-metronome metronome)
              0.0)) ))

(defun tick-all-metronomes (&optional (time-slice *time-slice*))
  "Applies TICK-METRONOME to all active metronomes."
  ;; Metronomes CANNOT be removed from the set during the loop.
  (declare (values null))
  (dolist (metronome *metronomes*)
    (tick-metronome metronome time-slice) ))

;; Compare with TICK-ALARM-CLOCK, RUN-ALARM-CLOCK, etc. in DUAL/ARCHIT/TIME.LSP


;;;;;;;  End of file DUAL/ARCHIT/METRONOM.LSP
