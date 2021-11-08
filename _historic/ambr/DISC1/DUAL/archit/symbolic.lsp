;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/symbolic.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Symbolic aspect of DUAL agents (cf. DUAL/ARCHIT/CONNNECT.LSP).
;;; DEPENDS-ON: defs.lsp, general.lsp, proclaim.lsp,
;;;             archit/mcrframe.lsp, archit/symproc1.lsp, archit/agenda.lsp,
;;;             archit/sprogn.lsp, archit/symproc2.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    09-04-97 [1.0]
;;; UPDATED:    14-12-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;        SYMBOLIC ASPECT OF DUAL AGENTS          ;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is SYMBOLIC-AGENT.
;;;; SYMBOLIC-AGENT integrates the declarative-symbolic aspect with the
;;;; procedural-symbolic aspect of a DUAL agent.
;;;; The file defines a constructor for symbolic agents and some additional
;;;; routines.
;;;;
;;;; A symbolic agent is a combination of a micro-frame (see DUAL/ARCHIT/
;;;; MCRFRAME.LSP) and a symbolic processor (see DUAL/ARCHIT/SYMPROCx.LSP).
;;;; It also inherits structure and functionality from base-agents (see DUAL/
;;;; ARCHIT/BASIC.LSP).
;;;;
;;;; This file also implements a mechanism for remotely modifying the micro-
;;;; frame of a symbolic agent.  The specification of the architecture does
;;;; not allow an agent to modify the microframe of another agent directly.
;;;; Rather, the former sends a 'write request' to the latter who can then
;;;; choose to carry it out and modify its (local) micro-frame.
;;;;
;;;; The write-request mechanism documented in this file can in fact support
;;;; not only write requests but requests to execute arbitrary function.
;;;; In other words, the set of possible OPERATIONs (see below) is not limited
;;;; to ADD-LINK, REMOVE-FACET and the like. It could be any function with
;;;; global function definition whose first argument is an agent (the receiver).
;;;;


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: symbolic-agent, make-symb-agent,
;;          write-request, send-write-request
;;

;; MAKE-SYMB-AGENT (name &key :comment :G-slots :S-slots :prompt-p)  -->
;;                                                              -->  new-agent
;;
;;   A function that constructs (and registers) a symbolic agent.
;;   NAME must be a symbol (but not  NIL).
;;   :PROMPT-P must be either T or NIL; if not supplied, it defaults to T.
;;   If NAME is already in use, MAKE-SYMB-AGENT signals an error depending on
;;     the value of :PROMPT-P. If it is T (the default), a continuable error
;;     is signalled giving the user an opportunity to supply a new value
;;     for NAME. If :PROMPT-P is NIL, a non-continuable error is signalled.
;;   :COMMENT must be a string; if not supplied, it defaults to NIL.
;;   :G- and :S-SLOTS must be lists of G- or S-slots, respectively. When
;;   not supplied, they default to the empty list.
;;   MAKE-SYMB-AGENT returns the new agent, after applying ESTABLISH-
;;     MICROFRAME-INTEGRITY to its micro-frame.  In addition, it initializes
;;     the OWNER fields of all G-SLOTS and S-SLOTS to NEW-AGENT.
;;
;;   After a successful call to MAKE-SYMB-AGENT, the following holds:
;;     (find-agent name)             -->  new-agent
;;     (agentp new-agent)            -->  T
;;     (agent-name new-agent)        -->  name
;;     (agent-comment new-agent)     -->  comment  ; or NIL
;;     (agent-G-slots new-agent)     -->  G-slots  ; or NIL
;;     (agent-S-slots new-agent)     -->  S-slots  ; or NIL
;;     (agent-input-zone new-agent)  -->  NIL
;;

;; SEND-WRITE-REQUEST (receiver operation &rest arguments)  -->  write-request
;;
;;   A generic function that constructs a write request on the basis of
;;   OPERATION and ARGUMENTS and sends it to RECEIVER.
;;
;;   RECEIVER should be a symbolic process; if not, an error is signaled.
;;   OPERATION should be a symbol that names a global function whose first
;;     argument is an agent.  If OPERATION is not a symbol, or it does not
;;     satisfy FBOUNDP, or it does satisfy SPECIAL-OPERATOR-P or MACRO-FUNCTION,
;;     then SEND-WRITE-REQUEST signals an error.
;;   ARGUMENTS should be a list of LISP objects suitable for passing to
;;     the function denoted by OPERATION.  The first parameter to the function
;;     denoted by OPERATION should be a symbolic agent (the receiver of the
;;     write request).  The second, third, etc. parameter of the function
;;     denoted by OPERATION are substituted by ARGUMENTS. &optional and &key
;;     parameters are supported if the ARGUMENTS list is arranged appropriately.
;;   The function returns the write request that has been sent to RECEIVER.
;;
;;   The write request is sent to RECEIVER via a call to the generic function
;;   RECEIVE-SYMBOL documented in DUAL/ARCHIT/SYMPROC2.LSP.  As a consequence,
;;   the whole machinery for receiving symbolic structures will be invoked.
;;   The new write request, which is a symbolic structure, will be stored in
;;   the input zone of RECEIVER (see DUAL/ARCHIT/SYMPROC1.LSP) and will wait
;;   its turn to be handled by the symbolic processor of RECEIVER.
;;
;;   This file defines a method for HANDLE-SYMBOL (see DUAL/ARCHIT/SYMPROC2.LSP)
;;   specializing on write requests.  This method is written by means of a
;;   S-PROGN (see DUAL/ARCHIT/SPROGN.LSP) and suspendably executes the following
;;   LISP form:   (apply OPERATION HOST ARGUMENTS)
;;   where OPERATION and ARGUMENTS are supplied by SEND-WRITE-REQUEST and have
;;   been carried inside the write request.  HOST is the RECEIVER of the write
;;   request and the agent whose micro-frame the caller of SEND-WRITE-REQUEST
;;   wants to change.  The actual symbolic operation changing the micro-frame
;;   will be carried locally by the symbolic processor of RECEIVER (i.e. HOST).
;;
;;   The consumption of OPERATION will be taken into account if it has been
;;   proclaimed via PROCLAIM-CONSUMPTION (see DUAL/PROCLAIM and =/CONSUM.LSP).
;;
;;   Example:
;;   To add a new link to the agent #$AG, one may use the following form:
;;      (send-write-request #$ag 'add-link label symref weight :order :lifo)
;;
;;   See DUAL/ARCHIT/LINKS.LSP for documentation of ADD-LINK, REMOVE-LINK, etc.
;;

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric send-write-request (receiver operation &rest arguments)
  (:documentation "Ask RECEIVER to perform OPERATION on its local processor." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;;;;;  *****   SYMBOLIC-AGENTS   *****    ;;;;;;;;;;;;;

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass symbolic-agent (micro-frame symbolic-processor)
    ()  ; no slots defined here, all slots are inherited
    (:documentation "Symbolic aspect of DUAL agents; a mixin class."))
)

;;;;  Constructor

(defun make-symb-agent (name &key comment G-slots S-slots (prompt-p t) )
  "Makes a symbolic agent and registers it into the total pool of agents."
  (declare (values symbolic-agent)
           (type (or null string) comment)
           (type list G-slots S-slots) )
  (let ((new-agent (allocate-agent name 'symbolic-agent prompt-p)))
    (when  comment                          ; COMMENT supplied?
      (setf (agent-comment new-agent) comment))
    (when  G-slots                          ; G-SLOTS supplied?
      (dolist (G-slot G-slots)
        (setf (slot-owner G-slot) new-agent))
      (setf (frame-G-slots new-agent) G-slots))
    (when  S-slots                          ; S-SLOTS supplied?
      (dolist (S-slot S-slots)
        (setf (slot-owner S-slot) new-agent))
      (setf (frame-S-slots new-agent) S-slots))
    (dual-core::initialize-T-LINK-flag new-agent)    ; see DUAL/ARCHIT/LINKS.LSP
    (establish-microframe-integrity new-agent
                                    :just-created)
    new-agent))


;;;;;;  Printing methods

(defmethod  agent-descriptor-string ((agent symbolic-agent))
  (if (eq (type-of agent) 'symbolic-agent)
      "a symbolic agent"
      (format nil "an agent of type ~S" (type-of agent)) ))



;;;;;;;  *****   REMOTELY  MODIFYING  THE MICRO-FRAME  *****    ;;;;;;;;
;;;
;;; The specification of the architecture does not allow an agent to modify
;;; the microframe of another agent directly. Rather, the former sends a
;;; 'write request' to the latter who can then choose to carry it out and
;;; modify its (local) micro-frame.
;;;

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass write-request (symbolic-structure)
    ((operation   :reader    write-request-operation
                  :type      symbol    ; e.g. 'ADD-LINK, 'REMOVE-FACET
                  :initarg   :operation
                  :initform  (required-argument)  )
     (arguments   :reader    write-request-arguments
                  :type      list   ; e.g. (SLOT-LABEL FACET-LABEL FILLER)
                  :initarg   :arguments
                  :initform  (required-argument)  )
    )
    (:documentation
    "A symbolic structure requesting the receiver to modify its micro-frame." ))
) ; eval-when

;;;;  Constructor

(defun  make-write-request (operation arguments)
  "Constructor for write requests."
  (declare (values write-request)
           (type symbol operation)
           (type list arguments) )
  (make-instance 'write-request :operation operation
                                :arguments arguments) )


;;;;;;  Printing methods

(defmethod  print-object ((wreq write-request) stream)
  (if (slot-boundp wreq 'operation)
      (format stream "#<WR ~A>" (write-request-operation wreq))
      (format stream "#<malformed write request>") ))

(defmethod DUAL-describe  ((wreq write-request)
                            &optional (stream *standard-output*))
  (format stream "~&~S is a ~A.~%"
          wreq (type-of wreq) )
  (format stream "~&  Its operation  is: ~A~%"
          (if (slot-boundp wreq 'operation)
              (write-request-operation wreq)
              "(unbound)"))
  (format stream "~&  Its arguments are: ~S~%"
          (if (slot-boundp wreq 'arguments)
              (write-request-arguments wreq)
              "(unbound)"))
  (values))


;;;;  Sending, receiving, and handling write-requests.

(defmethod send-write-request ((receiver symbolic-agent)
                               (operation symbol)
                               &rest arguments )
  ;; OPERATION should be a symbol that names a function (e.g. ADD-LINK).
  (if (and (fboundp operation)
           (not (special-operator-p operation))
           (not (macro-function operation)) )
      (receive-symbol receiver (make-write-request operation arguments))
      (error "SEND-WRITE-REQUEST: ~S does not name a function." operation) ))


(defmethod handle-symbol ((host symbolic-agent) (wreq write-request))
  (let ((operation (write-request-operation wreq)))
    ;; OPERATION should be a symbol that names a function.
    (if (and (fboundp operation)
             (not (special-operator-p operation))
             (not (macro-function operation)) )
        (s-progn host
           (s-eval (get-consumption operation t)
                   (apply (symbol-function operation)
                          host
                          (write-request-arguments wreq)) ))
        (error "HANDLE-SYMBOL: the OPERATION of a write request (~S) ~
                does not name a function." operation) )))


;;;;  Error-signaling methods

(defmethod  write-request-operation ((x t))
  (error "DUAL-CORE::WRITE-REQUEST-OPERATION:  ~S is not a write request." x) )

(defmethod  write-request-arguments ((x t))
  (error "DUAL-CORE::WRITE-REQUEST-ARGUMENTS:  ~S is not a write request." x) )


(defmethod send-write-request ((x t) (y t) &rest symbols)
  (declare (ignore symbols))
  (error "SEND-WRITE-REQUEST: ~S is not a symbolic agent or ~S is not ~
          a symbol (denoting an operation)."   x y) )

;; Error-signalling methods for HANDLE-SYMBOL are defined in ARCHIT/SYMPROC2.LSP


;;;;;;;  End of file DUAL/ARCHIT/SYMBOLIC.LSP
