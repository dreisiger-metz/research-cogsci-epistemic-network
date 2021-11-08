;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/symproc1.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Symbolic processor of DUAL agents (part 1).
;;; DEPENDS-ON: defs.lsp, archit/basic.lsp, archit/mcrframe.lsp, archit/links.ls
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    02-06-97 [1.0]
;;; UPDATED:    28-11-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;;
;;; TO DO:      Consider adding an EFFICIENCY slot. (See AGENT-EFFICIENCY below)


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;      S Y M B O L I C    P R O C E S S O R   (part 1)      ;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concepts in this file are SYMBOLIC-PROCESSOR and SYMBOLIC-STRUCTURE
;;;; This is the first of two files dealing with symbolic processors.
;;;; The first part (this file) defines the class SYMBOLIC-PROCESSOR and its
;;;; housekeeping methods. The second part (see ARCHIT/SYMPROC2.LSP) defines
;;;; the inner working of symbolic processors.
;;;;
;;;; This file is designed in such a way that it does _not_ depend on
;;;; ARCHIT/AGENDA.LSP (rather, AGENDA.LSP depends on it). In contrast, the
;;;; file ARCHIT/SYMPROC2.LSP does depend on ARCHIT/AGENDA.LSP.
;;;; (The mutual dependence b/n SYMPROC and AGENDA was the reason of splitting
;;;;  SYMPROC.LSP into SYMPROC1.LSP and SYMPROC2.LSP)
;;;;
;;;; A symbolic processor is a kind of agent (see ARCHIT/BASE.LSP).
;;;; It has a name and a comment and is registered into the global register.
;;;;
;;;; In addition, the symbolic processor have 'local memory' (see section
;;;; 3.2.5.1 in "DUAL Report #1") and is able to perform symbolic computations.
;;;; Part of the local memory is permanent -- this is the agent's micro-frame.
;;;; It is not dealt with here, see DUAL/ARCHIT/MCRFRAME.LSP.
;;;; The other part of the local memory is 'volatile memory'. As its name
;;;; implies, it is not stable but its contents is lost when the activation
;;;; level of the agent falls below a certain threshold (see ARCHIT/CONNECT.LSP)
;;;; The main purpose of this file is to provide facilities for working with
;;;; the volatile memory.
;;;;
;;;; Volatile memory further subdivides into 'symbolic input zone' and 'buffer'
;;;; (see section 3.2.5.1 in DR#1).
;;;;
;;;; The symbolic input zone is the place where other agents can send SYMBOLIC-
;;;; STRUCTURES to a given agent. Symbolic structures can be arbitrary objects.
;;;; (That's the main difference between the _symbolic_ input zone treated here
;;;; and the _connectionist_ input zone that is used in the process of
;;;; spreading activation (see DUAL/ARCHIT/CONNECT.LSP).)
;;;; Basically, the symbolic input zone acts like a queue: symbolic structures
;;;; are received from outside and processed on a first-in first-out basis
;;;; inside the symbolic processor.
;;;;
;;;; The rest of the volatile memory is the buffer. It is entirely for agent's
;;;; internal use. It can keep any object or set of objects and is manipulated
;;;; by the symbolic processor during the symbolic computations.
;;;;
;;;; The specification of DUAL postulates that the size of the local memory
;;;; is limited (section 3.2.5.1 in DR#1). It, however, fails to specialize
;;;; what happens when this limit is exceeded. The latter situation "should
;;;; hardly ever occur in well-tuned DUAL models."
;;;; The current implementation imposes no limits on the size of the local
;;;; memory -- the micro frame can take any number of slots (see DUAL/ARCHIT/
;;;; MCRFRAME.LSP), the input zone can receive and hold any number of symbolic
;;;; structures, and the so does the buffer. It is up to the programs built
;;;; on top of the architecture to ensure that the local memory never gets
;;;; too big.
;;;;
;;;;
;;;; The other important part of a SYMBOLIC-PROCESSOR is the symbolic processor
;;;; itself. The program that implements the work of the symbolic processor
;;;; is located in DUAL/ARCHIT/SYMPROC2.LSP, a file which must be compiled
;;;; after the file DUAL/ARCHIT/AGENDA.LSP.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: symbolic-processor, make-symb-processor,
;;          active-processor-p, agent-efficiency,
;;          agent-input-zone, symbolic-structure,
;;          clear-volatile-memory
;;
;; See also  *default-agent-efficiency*  defined in DEFS.LSP.
;;

;; SYMBOLIC-STRUCTURE
;;
;;   A symbol that is the proper name of the base class of all symbolic
;;   structures exchanged between symbolic agents.
;;   Later files of the implementation define numerous subclasses of SYMBOLIC-
;;   STRUCTURE.  For instance, MARKER, WRITE-REQUEST, etc.


;; MAKE-SYMB-PROCESSOR (name &key :comment :prompt-p)  -->  new-processor
;;
;;   A function that constructs (and registers) a symbolic processor.
;;   NAME must be a symbol (but not  NIL).
;;   If NAME is already in use, a continuable error is signalled.
;;   :PROMPT-P must be either T or NIL; if not supplied, it defaults to T.
;;   If NAME is already in use, MAKE-SYMBOLIC-PROCESSOR signals an error
;;     depending on the value of :PROMPT-P. If it is T (the default), a
;;     continuable error is signalled giving the user an opportunity to supply
;;     a new value for NAME. If :PROMPT-P is NIL, a non-continuable (fatal)
;;     error is signalled.
;;   :COMMENT must be a string; if not supplied, it defaults to NIL.
;;   MAKE-SYMB-PROCESSOR returns the new symbolic processor.
;;
;;   After a successful call to MAKE-SYMB-PROCESSOR, the following holds:
;;     (find-agent name)                 -->  new-processor
;;     (agentp new-processor)            -->  T
;;     (agent-name new-processor)        -->  name
;;     (agent-comment new-processor)     -->  comment  ; or NIL
;;     (agent-input-zone new-processor)  -->  NIL
;;

;; AGENT-INPUT-ZONE (agent)  -->  list-of-symbolic-structures
;; (SETF AGENT-INPUT-ZONE)
;;
;;   A generic function that accesses the symbolic input zone of AGENT.
;;   Signals an error if AGENT is not a symbolic processor.
;;   Returns a (possibly empty) list of symbolic structures.
;;
;;   AGENT-INPUT-ZONE may be used with SETF. From outside the agent, however,
;;   it is generally a much better idea to use RECEIVE-SYMBOL (see SYMPROC2.LSP)
;;   (SETF AGENT-INPUT-ZONE) is intended primarily for private operations.
;;   (The most typical private operation HANDLE-NEXT-SYMBOL, invoked by
;;    SYMBOLIC-MICROCYCLE defined in DUAL/ARCHIT/SYMPROC2.LSP)
;;
;;   The input zone is 'volatile memory' (see DR#1) and is cleared whenever
;;   the agent falls out of the working memory (see CLEAR-VOLATILE-MEMORY).
;;
;;   It is always true that:
;;      (agent-input-zone (make-symb-processor ...))  -->  NIL
;;      (progn (clear-volatile-memory agent)
;;             (agent-input-zone agent) )             -->  NIL
;;

;; ACTIVE-PROCESSOR-P (agent)  -->  T or NIL
;;
;;   A generic function that checks whether the symbolic processor of AGENT
;;   is currently working (T) or not (NIL).
;;   Signals an error if AGENT is not a symbolic processor.

;; AGENT-EFFICIENCY (agent)  -->  efficiency-coefficient
;;
;;   A generic function that reads the efficiency coefficient of the symbolic
;;   processor of AGENT (see 3.2.5.3. in "DUAL Report #1").
;;   AGENT should have a symbolic processor; if not, an error is signaled.
;;   The function returns a single float in the interval [0, 1].
;;
;;   In the current version of the program all agents of the same type have
;;   the same efficiency coefficient. This could change in future versions.
;;   Unless explicitly stated otherwise in the definition of a new class of
;;   agents, AGENT-EFFICIENCY returns by default the value bound to the
;;   global variable *DEFAULT-AGENT-EFFICIENCY* defined in DUAL/DEFS.LSP.


;; CLEAR-VOLATILE-MEMORY (agent)  -->  NIL
;;
;;   A generic function that clears the volatile memory of AGENT.
;;   According to the specification of DUAL (section 3.2.5.1 in DR#1),
;;   the symbolic input zone and the buffer are 'volatile memory' and lose
;;   all their contents when the agent's activation level falls below a
;;   certain threshold. The function defined here is to be invoked upon
;;   such conditions. (This is done automatically by the routines from
;;   DUAL/ARCHIT/WORKMEM.LSP.)
;;
;;   CLEAR-VOLATILE-MEMORY always returns NIL. It signals an error if AGENT
;;   is not a symbolic processor.
;;
;;   It is always true that:
;;     (let* ((x  (clear-volatile-memory agent))
;;            (iz (agent-input-zone agent)) )
;;       (list x iz ))                        -->  (NIL NIL)
;;
;;
;;   The classes that inherit from SYMBOLIC-PROCESSOR should provide :AFTER
;;   methods for CLEAR-VOLATILE-MEMORY to clear any additional volatile
;;   memories that they provide.
;;   For instance, if AGENT is also a micro-frame (see ARCHIT/MCRFRAME.LSP),
;;   CLEAR-VOLATILE-MEMORY deletes all temporary slots and facets by invoking
;;   the generic funtction REMOVE-ALL-TEMPORARY-LINKS (see ARCHIT/LINKS.LSP).
;;

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric  active-processor-p (agent)
  (:documentation "Returns T if the symbolic processor of AGENT is active." ))

(defgeneric  agent-efficiency (agent)
  (:documentation "The efficiency coefficient of the symbolic processor." ))

(defgeneric  clear-volatile-memory (agent)
  (:documentation "Clears the volatile memory. To be used when exiting WM." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass symbolic-structure (DUAL-object)
    () ; No slots, this is a base class.
    (:documentation
      "The superclass of markers, requests, etc. (A base class.)" ))

  (defclass symbolic-processor (base-agent)
    ((symb-input  :accessor    agent-input-zone
                  ;; do not confuse with CONN-INPUT-ZONE (see ARCHIT/CONNECT)
                  :type        list      ; a queue for symbolic structures
                  :initform    nil  )
     ;; The BUFFER, though specified in DR#1, is not defined here. It is more
     ;;   convenient that each subclass defines its own buffer-like slots.
     ;; In later versions of the program, an EFFICIENCY slot will be added.
     ;;   See *DEFAULT-AGENT-EFFICIENCY* and the methods for AGENT-EFFICIENCY.
    )
    (:documentation "Symbolic processor of DUAL agents; a mixin class."))
)

;;;;  Constructor

(defun make-symb-processor (name &key comment prompt-p)
  "Makes a symbolic processor and registers it into the total pool of agents."
  (declare (values symbolic-processor)
           (type (or null string) comment) )
  (let ((new-processor (allocate-agent name 'symbolic-processor prompt-p)))
    (when  comment                          ; COMMENT supplied?
      (setf (agent-comment new-processor) comment))
    new-processor))


;;;;  Type-checking methods for the accessors

(defmethod  agent-input-zone ((x t))
  (if x
    (error "AGENT-INPUT-ZONE:  ~S is not a symbolic processor." x)
    (error "AGENT-INPUT-ZONE applied to NIL (perhaps #$missing-agent).") ))

(defmethod  (setf agent-input-zone) (new-value (x t))
  (declare (ignore new-value))
  (if x
   (error "(SETF AGENT-INPUT-ZONE):  ~S is not a symbolic processor." x)
   (error "(SETF AGENT-INPUT-ZONE) applied to NIL (perhaps #$missing-agent).")))


;;;;;;  Printing methods

(defmethod  agent-descriptor-string ((agent symbolic-processor))
  (if (eq (type-of agent) 'symbolic-processor)
      "a symbolic processor"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod DUAL-describe :after ((ag symbolic-processor)
                                  &optional (stream *standard-output*))
  (format stream "~&  Its symbolic processor is now ~:[in~;~]active."
              ; no error checking (I've refrained from using CAUSES-AN-ERROR-P)
                 (active-processor-p ag) )
  (format stream "~&  Its efficiency coefficient is: ~6,3F~%"
              ; no error checking (I've refrained from using CAUSES-AN-ERROR-P)
                 (agent-efficiency ag) )
  (format stream "~&  The contents of its input zone is: ~S~%"
                (if (slot-boundp ag 'symb-input)
                    (agent-input-zone ag)
                    "(unbound slot)") )
  (values))


;;;;;;  Inner workings of symbolic processors, part 1 (see ARCHIT/SYMPROC2.LSP)

(defmethod  active-processor-p ((spr symbolic-processor))
  ;; The functions defined in DUAL/ARCHIT/AGENDA.LSP maintain a flag.
  (if (zerop (agent-flag spr *on-agenda-flag*))
      NIL
      T ))

(defmethod  active-processor-p ((x t))
  (if x
    (error "ACTIVE-PROCESSOR-P:  ~S is not a symbolic processor." x)
    (error "ACTIVE-PROCESSOR-P applied to NIL (perhaps #$missing-agent).") ))


(defmethod  agent-efficiency ((spr symbolic-processor))
  (declare (ignore spr))
    ;; In future versions of the program, each agent will have its local
    ;; EFFICIENCY  slot so that efficiencies will be modifiable through learning
    ;; For now, however, the efficiency coefficient is static:
  *default-agent-efficiency* )                          ; see DUAL/DEFS.LSP

(defmethod  agent-efficiency ((x t))
  (if x
    (error "AGENT-EFFICIENCY:  ~S is not a symbolic processor." x)
    (error "AGENT-EFFICIENCY applied to NIL (perhaps #$missing-agent).") ))


(defmethod  clear-volatile-memory ((spr symbolic-processor))
  (setf (agent-input-zone spr) nil)
)

(defmethod clear-volatile-memory :after ((mfr micro-frame))
  (remove-all-temporary-links mfr) )           ; see DUAL/ARCHIT/LINKS.LSP

(defmethod  clear-volatile-memory ((x t))
  (if x
    (error "CLEAR-VOLATILE-MEMORY:  ~S is not a symbolic processor." x)
    (error "CLEAR-VOLATILE-MEMORY applied to NIL (perhaps #$missing-agent).") ))


;;;;;;;  End of file DUAL/ARCHIT/SYMPROC1.LSP
