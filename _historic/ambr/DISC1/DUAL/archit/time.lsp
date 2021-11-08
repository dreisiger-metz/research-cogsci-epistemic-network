;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/time.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Provide DUAL agents with a limited sense of time.
;;; DEPENDS-ON: DUAL/defs.lsp, DUAL/archit/dual_ag.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    25-03-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...
;;;;;;;;;;;;
;;; TO DO:      Add a field to ALARM-CLOCKs such that one and the same 'owner'
;;;             can have several clocks of the same type (e.g. WEAK-SC).

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;     T I M E - R E L A T E D   FACILITIES    ;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")


;;;; The key concepts defined in this file are ALARM-CLOCK and CLOCK-MESAGE.
;;;;
;;;; This file and its successor DUAL/ARCHIT/METRONOM.LSP extend the specifi-
;;;; cation of the architecture set forth in "DUAL Report #1".  In other words,
;;;; the facilities implemented in this file are _not_ documented in DR#1.
;;;; This extension endows DUAL agents with a 'sense of time'.  The new
;;;; abilities are limited to avoid the notion of a global absolute clock.
;;;; (Such clock is nevertheless provided -- see the variable *TIME* -- but
;;;;  only for the purposes of the implementation and the interface.)
;;;;
;;;; More specifically, DUAL agents can use ALARM-CLOCKS and METRONOMES.
;;;; The former are implemented in this file, the latter in =/METRONOM.LSP.
;;;;
;;;;
;;;;;;;;;;;;;;;;;  ALARM CLOCKS  ;;;;;;;;;;;
;;;;
;;;; Consider the following scenario:  Agent A sends some request to agent B.
;;;; Agent B is expected to handle the request and to send an answer back to A.
;;;; Assume, however, that agent B drops out of the working memory and does
;;;; not manage to handle the request.  Due to this failure A may wait forever.
;;;; To cope with such situation, agent A may set an ALARM CLOCK and then let
;;;; its symbolic processor 'fall asleep'.  After the specified period of time
;;;; the alarm clock will 'wake up' the symbolic processor by sending it a
;;;; CLOCK-MESSAGE.  This message may trigger an appropriate action, e.g.
;;;; checking whether it is still meaningful to wait for an answer from agent B.
;;;;
;;;; The phrase 'a symbolic processor falls asleep' means that the processor
;;;; stops its symbolic microcycle (see DUAL/ARCHIT/SYMPROC2.LSP) because the
;;;; symbolic input zone is empty.  Note that the agent itself remains active
;;;; in the connectionist sense and continues to belong to the working memory.
;;;; (Implementationally, the symbolic processor is removed from *AGENDA* but
;;;;  the host agent remains in *WM*; see DUAL/ARCHIT/AGENDA.LSP.)
;;;;
;;;; Setting an alarm clock does not mean that the symbolic processor automa-
;;;; tically goes to sleep.  It may remain active if there are more symbols
;;;; to handle from the input zone.  Setting an alarm clock simply relieves
;;;; the processor of the necessity to 'stay alert' (busy wait).
;;;;
;;;;;;;;
;;;; A few words about the implementation of alarm clocks.  An alarm clock is a
;;;; data structure with the following fields:
;;;;  + name    -- a symbol that identifies (in conjunction with OWNER) the clk
;;;;  + comment -- optional string
;;;;  + owner   -- a DUAL agent that will be 'waken up' by the clock
;;;;  + time    -- the amount of time that remains until the clock 'rings'
;;;;  + message -- a symbolic structure that will be sent to OWNER (wrapped
;;;;                                        in a 'clock message' -- see below).
;;;;
;;;; There cannot be two alarm clocks with the same (NAME OWNER) pair -- see
;;;; the function FIND-ALARM-CLOCK below.
;;;; (Note: This proved a bad design decision. It does not allow one and the  )
;;;; (      same agent to maintain several clocks of the same type.           )
;;;; (      In AMBR/WEAK_SC.LSP  I was forced to use GENSYM clock names.      )
;;;; (      Future versions may add a new field (e.g. 'ID-number') to the key )
;;;; (      of the relation.                                                  )
;;;;
;;;;
;;;;;;;;;;;;;;;;;  CLOCK  MESSAGES  ;;;;;;;;;;;
;;;;
;;;; Alarm clocks 'wake up' their respective symbolic processor by sending it
;;;; a special symbolic structure.  The general mechanism for symbolic exchange
;;;; (see RECEIVE-SYMBOL in DUAL/ARCHIT/SYMPROC2.LSP) ensures that this will
;;;; activate the symbolic processor if needed (see TRIGGER-SYMBOLIC-PROCESSOR).
;;;;
;;;; To speed up the processing of symbolic messages sent by alarm clocks, such
;;;; messages are put at the _front_ of the symbolic input zone.  Thus, they
;;;; by-pass the default FIFO discipline for handling the input queue.
;;;;
;;;; This file defines a special class called CLOCK-MESSAGE for the purposes
;;;; of alarm clocks.  The clock message is a data structure with a single
;;;; field -- the message itself.  The message itself should be a symbolic
;;;; structure (see DUAL/ARCHIT/SYMPROC1.LSP).
;;;;
;;;; Handling a clock message amounts to opening it and applying the function
;;;; HANDLE-MESSAGE to the enclosed symbolic structure.
;;;;
;;;;
;;;; DUAL-based models may use alarm clocks and clock messages 'as is' or
;;;; define their customized sub-classes.
;;;;
;;;; The file DUAL/INTRFACE/TIME.LSP defines some additional utilities.



;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: *time*, clock-tick,   ;; see *time-slice* in DUAL/DEFS.LSP
;;          time-related-DUAL-object, time-related-symbolic-structure,
;;          clock-message, send-clock-message ;
;;          alarm-clock, make-alarm-clock,
;;          alarm-clock-name, alarm-clock-comment, alarm-clock-owner,
;;          alarm-clock-time-remaining, alarm-clock-message ;
;;          find-alarm-clock, find-or-make-alarm-clock,
;;          set-alarm-clock, deactivate-alarm-clock,
;;          number-of-active-alarm-clocks ;
;;          tick-alarm-clock, tick-all-alarm-clocks, run-alarm-clock
;;
;; See also the parameter *TIME-SLICE* in DUAL/DEFS.LSP.


;; *TIME*
;;
;;   A global variable that stores the time since the beginning of the
;;   simulation (in units of simulated time).
;;
;;   DUAL-core functions should _not_ refer to that variable.  DUAL agents
;;   have only a limited 'sense' of time and do not have access to a global
;;   and absolute time scale.
;;
;;   This variable should neither be read nor written directly. This would be
;;   a violation of the specification of the architecture.
;;   Instead of reading the value stored in *TIME*, use alarm clocks and
;;   metronomes (see DUAL/ARCHIT/METRONOM.LSP).
;;   Instead of explicitly changing the value of *TIME*, use the function
;;   CLOCK-TICK for this purpose.
;;
;;   *TIME* is advertized in the external protocol chiefly for the purposes
;;   of the interface -- it is useful for verbose messages, logs, and the like.
;;

;; CLOCK-TICK  (&optional time-slice)  -->  current-time
;;
;;   A function that advances the system's internal clock with one elementary
;;   step.  (Note that the conceptual specification postulates continuous time.
;;   It is discretized in 'time slices' for implementational purposes only.)
;;
;;   TIME-SLICE should be a positive floating-point number.  When not supplied,
;;   it defaults to the global parameter *TIME-SLICE* (see DUAL/DEFS.LSP).
;;   It is not recommended to override this default.
;;
;;   The function increments the global variable *TIME* and returns the new
;;   value.  It also calls the functions TICK-ALL-ALARM-CLOCKS (defined below)
;;   and TICK-ALL-METRONOMES (defined in DUAL/ARCHIT/METRONOM.LSP).
;;
;;   The algorithm is shown below; the actual implementation may contain
;;   additional details.
;;       (incf *time* time-slice)
;;       (run-all-alarm-clocks time-slice)
;;       (run-all-metronomes   time-slice)
;;       *time*


;; TIME-RELATED-DUAL-OBJECT
;; TIME-RELATED-SYMBOLIC-STRUCTURE
;;
;;   Symbols that are the proper names of the base classes of all time-related
;;   DUAL objects and symbolic-structures, respectively.
;;   TIME-RELATED-DUAL-OBJECT a subclass of DUAL-OBJECT and superclass of
;;     ALARM-CLOCK, METRONOME, and TIME-RELATED-SYMBOLIC-STRUCTURE.
;;   TIME-RELATED-SYMBOLIC-STRUCTURE is a subclass of SYMBOLIC-STRUCTURE and
;;     TIME-RELATED-DUAL-OBJECT and is a superclass of CLOCK-MESSAGE.
;;
;;   Time-related symbolic structures are treated in a special manner by
;;     RECEIVE-SYMBOL (see DUAL/ARCHIT/SYMPROC2.LSP).  They are not added to
;;     the end of the input queue like all other symbolic structures being
;;     received.  Rather they are pushed to the front (LIFO order) in order
;;     to speed up their handling by the symbolic processor.

;; ...
;; ...      clock-message, send-clock-message ;
;; ...      alarm-clock, make-alarm-clock,
;; ...      alarm-clock-name, alarm-clock-comment, alarm-clock-owner,
;; ...      alarm-clock-time-remaining, alarm-clock-message ;
;; ...      find-alarm-clock, find-or-make-alarm-clock,
;; ...      set-alarm-clock, deactivate-alarm-clock,
;; ...      number-of-active-alarm-clocks ;
;; ...      tick-alarm-clock, tick-all-alarm-clocks, run-alarm-clock
;;

;; Example: (set-alarm-clock (find-or-make-alarm-clock 'my-clock-type
;;                                                     owner-agent)
;;                           *default-time-interval*
;;                           my-message )

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

;;;;;;;;;  *****  GLOBAL TIME VARIABLE  *****
;;
;; ***  DUAL-CORE functions should _not_ refer to that variable!  ***
;;
;; See also *TIME-SLICE* in DUAL/DEFS.LSP.

(defvar *time* 0.0
  "Time since the beginning of simulation (in units of simulated time)." )

(defun  clock-tick  (&optional (time-slice *time-slice*))
  "Advance the system's internal clock by one elementary step."
  (declare (type float time-slice)
           (values float) )
  (incf *time* time-slice)
  (tick-all-alarm-clocks time-slice)     ; defined below
  (tick-all-metronomes   time-slice)     ; defined in DUAL/ARCHIT/METRONOM.LSP
  *time* )



;;;;;;;;;  *****  CLOCK MESSAGES  *****
;;

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass time-related-DUAL-object (DUAL-object)
    () ; No slots, this is a base class.
    (:documentation
      "The superclass of alarm clocks, metronomes, etc. A base class." ))

  (defclass time-related-symbolic-structure (symbolic-structure
                                             time-related-DUAL-object)
    () ; No slots, this is a base class.
    (:documentation
      "The superclass of clock messages and the like. A base class." ))

  (defclass clock-message (time-related-symbolic-structure)
    ((message     :initarg        :message
                  :reader         clock-message
                  :type           symbolic-structure
                  :initform       (required-argument) )
    )
    (:documentation "A message sent by alarm clocks to agents." ))
) ; eval-when


;;;; Constructor

(defun send-clock-message (receiver message)
  "Constructs and sends an alarm message."
  (declare (type DUAL-agent receiver)
           (type symbolic-structure message)
           (values clock-message) )
  (receive-symbol receiver
                  (make-instance 'clock-message
                                 :message message)) )


;;;;  Type-checking method for the reader

(defmethod  clock-message ((x t))
  (error "CLOCK-MESSAGE:  ~S is not a clock message." x))


;;;;;;  Printing methods

(defmethod print-object ((msg clock-message) stream)
  (if (slot-boundp msg 'message)
      (format stream "#<AM ~S>" (clock-message msg))
      (format stream "#<malformed clock message>" ) ))

(defmethod DUAL-describe ((msg clock-message)
                          &optional (stream *standard-output*))
  (format stream "~&~S is a ~A.~%"
          msg (class-name (class-of msg)) )
  (format stream "~&  The message is: ~S~%"
                 (if (slot-boundp msg 'message)
                     (clock-message msg)
                     "(unbound, which is anomalous)" ) )
  (values))


;;;;;;;;;;;;;;  RECEIVING AND HANDLING CLOCK MESSAGES
;;;
;;;  Time-related symbolic structures (and in particular clock messages) are
;;;  treated specially by RECEIVE-SYMBOL -- LIFO instead of FIFO order.

(defmethod  receive-symbol ((spr symbolic-processor)
                            (time-symbol time-related-symbolic-structure))
  (setf (agent-input-zone spr)
        (CONS time-symbol (agent-input-zone spr)) )    ; LIFO
  time-symbol )

  ;; See DUAL/ARCHIT/SYMPROC2.LSP for the regular method of RECEIVE-SYMBOL.
  ;; Ordinary symbolic structures are put at the _end_ of the queue (FIFO).
  ;; See also the :AROUND method in DUAL/ARCHIT/HYBRID.LSP.


;; Handling a clock message amounts to opening it and applying HANDLE-SYMBOL
;; to the enclosed symbolic structure.

(defmethod  handle-symbol ((spr symbolic-processor)
                           (clock-message clock-message) )
  (let ((enclosed-symbolic-structure (clock-message clock-message)))
    (handle-symbol spr enclosed-symbolic-structure) ))



;;;;;;;;;  *****  ALARM CLOCKS  *****
;;

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass alarm-clock (time-related-DUAL-object)
    ((name        :initarg        :name
                  :reader         alarm-clock-name
                  :type           symbol
                  :initform       (required-argument) )
     (comment     :initarg        :comment
                  :accessor       alarm-clock-comment
                  :type           (or null string)
                  :initform       nil   )
     (owner       :initarg        :owner
                  :reader         alarm-clock-owner
                  :type           DUAL-agent
                  :initform       (required-argument) )
     (message     :accessor       alarm-clock-message
                  :type           T        ; not yet wrapped in a CLOCK-MESSAGE
                  :initform       nil   )
     (time-remaining
                  :accessor       alarm-clock-time-remaining
                  :type           float
                  :initform       9999.999   )
    )
    (:documentation
      "A device that sends a message to its owner after some time." ))
) ; eval-when


;;;; Constructor

(defun make-alarm-clock (name owner &optional comment)
  (declare (type symbol name)
           (type DUAL-agent owner)
           (type (or null string) comment)
           (values alarm-clock) )
  (unless (agentp owner)
    (error "MAKE-ALARM-CLOCK: ~S is not an agent." owner ))
  (make-instance 'alarm-clock  :name    name
                               :owner   owner
                               :comment comment ))
  ;; Use SET-ALARM-CLOCK to attach the new alarm clock to *ALARM-CLOCKS*.


;;;;  Type-checking methods for the accessors.

(defmethod  alarm-clock-name ((x t))
  (error "ALARM-CLOCK-NAME:  ~S is not an alarm clock." x ))

(defmethod  alarm-clock-comment ((x t))
  (error "ALARM-CLOCK-COMMENT:  ~S is not an alarm clock." x ))

(defmethod  (setf alarm-clock-comment) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF ALARM-CLOCK-COMMENT):  ~S is not an alarm clock." x ))

(defmethod  alarm-clock-owner ((x t))
  (error "ALARM-CLOCK-OWNER:  ~S is not an alarm clock." x ))

(defmethod  alarm-clock-message ((x t))
  (error "ALARM-CLOCK-MESSAGE:  ~S is not an alarm clock." x ))

(defmethod  (setf alarm-clock-message) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF ALARM-CLOCK-MESSAGE):  ~S is not an alarm clock." x ))

(defmethod  alarm-clock-time-remaining ((x t))
  (error "ALARM-CLOCK-TIME-REMAINING:  ~S is not an alarm clock." x ))

(defmethod  (setf alarm-clock-time-remaining) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF ALARM-CLOCK-TIME-REMAINING):  ~S is not an alarm clock." x ))


;;;;;;  Printing methods

(defmethod print-object ((clock alarm-clock) stream)
  (if (and (slot-boundp clock 'name)
           (slot-boundp clock 'owner)
           (agentp (alarm-clock-owner clock)))
      (format stream "#<CLOCK ~S ~S>" 
              (alarm-clock-name clock) 
              (alarm-clock-owner clock) )
      (format stream "#<malformed alarm clock>" ) ))

(defmethod DUAL-describe ((clock alarm-clock)
                          &optional (stream *standard-output*))
  (format stream "~&~S is an ~A.~%"
          clock (class-name (class-of clock)) )
  (format stream "~&  Its name is: ~S~%"
                 (if (slot-boundp clock 'name)
                     (alarm-clock-name clock)
                     "(unbound, which is anomalous)" ) )
  (format stream "~&  Its comment is: ~S~%"
                 (and (slot-boundp clock 'comment)
                      (alarm-clock-comment clock)) )
  (format stream "~&  Its owner is: ~S~%"
                 (if (slot-boundp clock 'owner)
                     (alarm-clock-owner clock)
                     "(unbound, which is anomalous)" ) )
  (format stream "~&  Its message is: ~S~%"
                 (if (slot-boundp clock 'message)
                     (alarm-clock-message clock)
                     "(unbound, which is anomalous)" ) )
  (format stream "~&  Its will ring after ~,2F units of time.~%"
                 (if (slot-boundp clock 'time-remaining)
                     (alarm-clock-time-remaining clock)
                     9999.9 ) )
  (values))



;;;;;;  *****    ALARM  CLOCK  MANAGEMENT   *****
;;;
;;;  The implementation maintains a set of active alarm clocks.  It is
;;;  implemented as an adjustable array with a fill pointer and is bound to
;;;  the global variable *ALARM-CLOCKS*.
;;;
;;;  Alarm clocks are added to the set via SET-ALARM-CLOCK and removed from
;;;  there via DEACTIVATE-ALARM-CLOCK.  They are automatically removed when
;;;  their time expires and they have been run -- see TICK-ALL-ALARM-CLOCKS.

(defvar  *alarm-clocks*
         (make-array  100   ; adjust to the expected value of your application
                      :element-type '(or null alarm-clock)
                      :fill-pointer 0
                      :adjustable t
                      :initial-element nil )
  "Set of active alarm clocks waiting for their time to ring."  )


(declaim (inline number-of-active-alarm-clocks))
(defun number-of-active-alarm-clocks ()
  (fill-pointer *alarm-clocks*) )

(defun find-alarm-clock (name owner)
  "Looks for an active alarm clock with NAME and OWNER."
  (declare (values (or null alarm-clock))
           (type symbol name)
           (type DUAL-agent owner) )
  (find-if #'(lambda (clock)
               (and (eq name  (alarm-clock-name  clock))
                    (eq owner (alarm-clock-owner clock)) ))
           *alarm-clocks* ))

(defun find-or-make-alarm-clock (name owner &optional comment)
  (declare (values alarm-clock)
           (type symbol name)
           (type DUAL-agent owner) )
  (cond ((find-alarm-clock name owner))               ; if found, return it
        (t (make-alarm-clock name owner comment)) ))  ; otherwise make new
  ;; Note that the new clock is not set (i.e. attached to *ALARM-CLOCKS*).


(defun set-alarm-clock (alarm-clock time-interval message)
  "Sets ALARM-CLOCK to send MESSAGE to its owner after TIME-INTERVAL."
  (declare (type alarm-clock alarm-clock)
           (type number time-interval)
           (values alarm-clock) )
  (setf (alarm-clock-time-remaining alarm-clock) 
        (coerce time-interval 'float))
  (setf (alarm-clock-message alarm-clock) 
        message)
  (if (find alarm-clock *alarm-clocks*)
      nil                                                 ; already there
      (vector-push-extend alarm-clock *alarm-clocks*) )   ; add new clock
  alarm-clock )


(defun deactivate-alarm-clock (alarm-clock)
  "Prevents an alarm clock from ringing."
  (declare (type (or null alarm-clock) alarm-clock)
           (values boolean) )
  (if (null alarm-clock)
      nil                  ; (deactivate-... (find-alarm-clock 'not-there ...))
      (let ((pos (position alarm-clock *alarm-clocks*)))
        (cond ((null pos)  nil)
              (t (setf (aref *alarm-clocks* pos)
                       (vector-pop *alarm-clocks*))    ; move LAST in the gap
                 (setf (aref *alarm-clocks* (fill-pointer *alarm-clocks*))
                       nil)                            ; LAST is now free
                 T )) )))


;;;;;;;;;   Ticking and running alarm clocks.
;;

(defun  run-alarm-clock (alarm-clock)
  "ALARM-CLOCK 'rings' -- sends the message to its owner."
  (declare (type alarm-clock alarm-clock))
  (let ((message (alarm-clock-message alarm-clock)))
    (send-clock-message (alarm-clock-owner alarm-clock)
                        message )
    (deactivate-alarm-clock alarm-clock)   ; detach from *ALARM-CLOCKS*
    message ))


(defun  tick-alarm-clock (alarm-clock &optional (time-slice *time-slice*) )
  "Decrements the time remaining and runs the clock if necessary."
  (declare (type alarm-clock alarm-clock)
           (type float time-slice)
           (values float) )      ; 0.0 means 'just run'
  (let ((new-time-remaining (decf (alarm-clock-time-remaining alarm-clock)
                                  time-slice)))
    (cond ((plusp new-time-remaining)  new-time-remaining)
          (t  (run-alarm-clock alarm-clock)
              0.0)) ))


(defun tick-all-alarm-clocks (&optional (time-slice *time-slice*))
  "Applies TICK-ALARM-CLOCK to all active clocks."
  ;; Clocks may be removed from the set during the loop.
  ;; Compare with DO-ALL-PROCESSES in DUAL/ARCHIT/AGENDA.LSP
  (declare (values null))
  (flet ((tick-one (curr-pos curr-clock)
           (tick-alarm-clock curr-clock time-slice)
           (eq curr-clock (aref *alarm-clocks* curr-pos)) ))
    (let ((k 0))  ; index in the array *ALARM-CLOCKS*
      (loop
         ;; VARIANT:   d := (fill-ptr - k) decreases with 1 on each iteration
         ;; INVARIANT: When d > 0, A[k] contains a clock to be ticked
         ;;            and all A[0] ... A[k-1] have been ticked already.
         (when (>= k (fill-pointer *alarm-clocks*))
            (return nil))
         (if (tick-one k (aref *alarm-clocks* k))
             (incf k)               ; go to next position
             )                      ; intercepting removal -- stay on same pos.
       ))))


;;;;;;;  End of file DUAL/ARCHIT/TIME.LSP
