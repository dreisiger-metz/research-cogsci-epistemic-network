;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/symproc2.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Symbolic processor of DUAL agents (part 2).
;;; DEPENDS-ON: defs.lsp, general.lsp, proclaim.lsp, archit/symproc1.lsp,
;;;             archit/agenda.lsp, archit/sprogn.lsp, archit/brackets.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    02-06-97 [1.0]
;;; UPDATED:    14-12-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;
;;; TO_DO:      Add garbage collection to SYMBOLIC-MICROCYCLE.
;;;


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;        S Y M B O L I C    P R O C E S S O R         ;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; This file is a continuation of DUAL/ARCHIT/SYMPROC1.LSP. It deals with the
;;;; symbolic processor of an object of type SYMBOLIC-PROCESSOR.
;;;;
;;;; (DUAL/ARCHIT/SYMPROC1.LSP defined the classes SYMBOLIC-STRUCTURE and
;;;;  SYMBOLIC-PROCESSOR. It also defined the generic fun AGENT-INPUT-ZONE.)
;;;;
;;;; The other important part of a SYMBOLIC-PROCESSOR is the symbolic processor
;;;; itself. It has a built-in routine and executes it when certain
;;;; preconditions are met. The function TRIGGER-SYMBOLIC-PROCESSOR, defined
;;;; in this file, is the intended means of triggering the symbolic processor.
;;;; Once triggered, it then handles the symbolic structures that come to
;;;; its input zone. The exact method for HANDLE-SYMBOL is left empty and
;;;; may be shadowed by more specific methods in other parts of the program.
;;;;
;;;; The symbolic processing consumes energy (see section 3.2.5.4 in DR#1 and
;;;; files DUAL/ARCHIT/AGENDA.LSP and DUAL/ARCHIT/SUSPEND.TXT). Each symbolic
;;;; processor is characterized by an efficiency coefficient (AGENT-EFFICIENCY
;;;; in DUAL/ARCHIT/SYMPROC1.LSP).


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: trigger-symbolic-processor,  :symbol-input
;;          symbolic-microcycle,
;;          receive-symbol, handle-symbol, handle-next-input
;;
;;   See DUAL/CONSUM.LSP for the consumption of these operations.


;; RECEIVE-SYMBOL (receiver symbolic-structure)  -->  symbolic-structure
;;
;;   A generic function that stores SYMBOLIC-STRUCTURE into the input zone of
;;   RECEIVER. The new structure is appended to the end of the queue.
;;   (Thus, symbolic structures are handled on a first-in-first-out basis
;;    (but note the exception below). See also HANDLE-NEXT-SYMBOL.)
;;
;;   RECEIVER should be a symbolic process; if not, an error is signaled.
;;   SYMBOLIC-STRUCTURE should be a symbolic structure (see ARCHIT/SYMPROC1.LSP)
;;   If it isn't, an error is signaled.
;;   The function returns SYMBOLIC-STRUCTURE.
;;
;;   As a side effect of the operation, the generic function TRIGGER-SYMBOLIC-
;;   PROCESSOR  is invoked with arguments RECEIVER and :SYMBOL-INPUT. This
;;   happens _after_ SYMBOLIC-STRUCTURE has been stored in the input zone.
;;   Thus, the symbolic processor (which might have been idle up to that point)
;;   is informed that a new symbol has arrived.
;;
;;   The consumption of RECEIVE-SYMBOL is proclaimed in PROCLAIM.LSP.
;;
;;   The method for RECEIVE-SYMBOL described here assumes that RECEIVER is
;;   always visible. DUAL/ARCHIT/HYBRID.LSP defines additional method that
;;   checks for visibility.
;;
;;   See file DUAL/ARCHIT/TIME.LSP for an exceptional case related to receiving
;;   time-related symbolic structures.  They are handled on a LIFO basis.

;; HANDLE-SYMBOL (host symbolic-structure)  -->
;;                                     -->  :SUSPENDED or :SUSPENSION-IGNORED
;;
;;   A generic function that handles SYMBOLIC-STRUCTURE by executing the
;;   built-in routine of the symbolic processor of the agent HOST.
;;   This is achieved by executing a 'suspendable' process working under HOST.
;;   The suspendable process is usually expressed in terms of the macro S-PROGN.
;;   (See DUAL/ARCHIT/AGENDA.LSP for documentation of suspended processes.
;;    See also DUAL/ARCHIT/SUSPEND.TXT for documentation of S-PROGN.)
;;
;;   HOST should be a symbolic processor; if not, an error is signaled.
;;   SYMBOLIC-STRUCTURE may be any LISP object.
;;   HANDLE-SYMBOL returns what S-PROGN returns, that is, one of the keywords
;    :SUSPENDED and :SUSPENSION-IGNORED. See DUAL/ARCHIT/SPROGN.LSP for details.
;;
;;   This file defines only two primary methods for HANDLE-SYMBOL.
;;   They have the following method signatures:
;;     1. (host symbolic-processor) (symbolic-structure symbolic-structure)
;;     2. (x t) (y t)
;;   Method 1. returns :SUSPENDED (when *IGNORE-SUSPENSIONS* is NIL) or
;;     :SUSPENSION-IGNORED (when *IGNORE-SUSPENSIONS* is T)  without doing
;;     anything with SYMBOLIC-STRUCTURE.
;;     That is, HANDLE-SYMBOL ignores the symbolic structure unless it has a
;;     specific method to handle it.
;;   Method 2. signals an error.
;;
;;   Other modules of the program are encouraged to define other primary methods
;;   specialize on other classes of agents and/or symbolic-structures.
;;   These methods will shadow the empty method described as Method 1. above.
;;   For example, the file ARCHIT/MP_AGENT.LSP defines the class MP-DUAL-AGENT
;;   (which is a subclass of SYMBOLIC-PROCESSOR).
;;   It also defines the class MARKER (which is a subcl. of SYMBOLIC-STRUCTURE).
;;   Finally, it defines a primary method for HANDLE-SYMBOL that specializes
;;   on MP-DUAL-AGENTs and MARKERs to implement the marker-passing mechanism.
;;
;;   The consumption of HANDLE-SYMBOL is proclaimed in DUAL/PROCLAIM.LSP.
;;
;;   The generic function HANDLE-SYMBOL is a very important tool for building
;;   DUAL-based models.  Typically, these models define various classes of
;;   agents and various classes of symbolic structures.  A great part of
;;   the behavior of each agent consists of differential treatment of the
;;   symbolic structures appearing in its input zone.  HANDLE-SYMBOL allows
;;   to customize this process in a flexible and modular way.  For example, the
;;   AMBR model uses more than a dozen specialized methods for this function.
;;

;; HANDLE-NEXT-INPUT (agent)  -->  NIL or :SUSPENDED or :SUSPENSION-IGNORED
;;
;;   A generic function that checks the input zone of AGENT and handles
;;   the first symbolic structure in it (if any). The simbolic structures
;;   in the input zone are handled on a first-in-first-out basis.
;;   (See also RECEIVE-SYMBOL.)
;;
;;   AGENT should be a symbolic processor; if not, an error is signaled.
;;   The function returns NIL if the input zone is empty. If it isn't empty,
;;   the first symbolic structure is removed from the input zone and passed
;;   to HANDLE-SYMBOL. HANDLE-NEXT-INPUT then returns whatever HANDLE-SYMBOL
;;   returns (that is, either :SUSPENDED or :SUSPENSION-IGNORED).
;;
;;   It is always true that:
;;      (if (eq (handle-next-input agent) 'nil)
;;          (endp (agent-input-zone agent))  t)   -->  T
;;
;;   HANDLE-NEXT-INPUT is invoked automatically by SYMBOLIC-MICROCYCLE.
;;   The consumption of HANDLE-NEXT-INPUT is proclaimed in DUAL/CONSUM.LSP.


;; SYMBOLIC-MICROCYCLE  (agent)  -->  NIL or :SUSPENDED or :SUSPENSION-IGNORED
;;
;;   A generic function that implements one cycle of the built-in routine of
;;   the symbolic processor of AGENT.
;;
;;   AGENT should be a symbolic processor; if not, an error is signaled.
;;
;;   SYMBOLIC-MICROCYCLE works according to the following (suspendable) algrthm:
;;     1. If the input zone of AGENT is empty, return NIL and stop.
;;     2. Otherwise, (suspendedly) invoke HANDLE-NEXT-SYMBOL.
;;     3. Loop back to step 1.
;;
;;   Other modules of the program may provide additional methods (primary or
;;   secondary) for SYMBOLIC-MICROCYCLE. Care should be taken, however, that
;;   all the symbolic structures from AGENT's input zone are handled sooner
;;   or later. Moreover, they should be handled on a first-in-first-out basis.
;;   (The latter requirement is postulated in the conceptual specification of
;;   the architecture -- see section 3.2.5.4. in "DUAL Report #1".)
;;


;; TRIGGER-SYMBOLIC-PROCESSOR (agent event)  -->  unspecified
;;
;;   A generic function that triggers AGENT's built-in routine on the grounds
;;   that EVENT has occurred.
;;   AGENT should be a symbolic processor; if not, an error is signaled.
;;   EVENT should be a symbol, preferably from the keyword package. If it
;;   isn't a symbol, an error is signaled.
;;   It is not specified what TRIGGER-SYMBOLIC-PROCESSOR returns. The default
;;   method returns NIL but this may be overridden by more specific methods.
;;   One likely return value will be :SUSPENDED because built-in routines
;;   will likely be expressed in terms of S-PROGN (see DUAL/ARCHIT/SPROGN.LSP).
;;   In particular, this is the case of SYMBOLIC-MICROCYCLE.
;;
;;   User-defined classes of agents are encouraged to provide primary methods
;;   for TRIGGER-SYMBOLIC-PROCESSOR to implement specific built-in routines.
;;
;;   EVENT should be a (keyword) symbol specifying the event that justifies
;;   the call to TRIGGER-SYMBOLIC-PROCESSOR.
;;   Up to now, only one event is recognized -- :SYMBOL-INPUT. This message is
;;   issued whenever there is a new symbol in the input zone, see RECEIVE-SYMBOL.
;;   User-defined classes are encouraged to provide methods that handle other
;;   events.
;;
;;   This file defines three primary methods for TRIGGER-SYMBOLIC-PROCESSOR.
;;   They have the following method signatures:
;;     1. (spr symbolic-processor) (event (eql :symbol-input))
;;     2. (spr symbolic-processor) (event symbol)
;;     3. (x t) (y t)
;;   Methods 2. and 3. signal errors.
;;   Method 1. works according to the following algorithm:
;;     -- if the processor is idle (see ACTIVE-PROCESSOR-P in ARCHIT/SYMPROC1),
;;          invoke SYMBOLIC-MICROCYCLE
;;     -- otherwise, return NIL without doing anything else. (The processor
;;          is working and will notice any new symbols in the input zone).
;;
;;   The function ADD-TO-WM issues an :ENTER-WM event (see ARCHIT/WORK_MEM.LSP).
;;   Node constructors (see DUAL/ARCHIT/NC_AGENT.LSP) send a :JUST-CREATED event
;;     to the newly-constructed agents.
;;   Metronomes (see DUAL/ARCHIT/METRONOM.LSP) also send metronome-specific
;;     events (e.g. :METRONOME-PULSE).
;;

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric  trigger-symbolic-processor (agent event)
  (:documentation
  "Trigger AGENT's built-in routine on the grounds that EVENT has occurred."))

(defgeneric  symbolic-microcycle (symbolic-processor)
  (:documentation "One cycle of symbolic processor's operation." ))

(defgeneric  receive-symbol (receiver symbolic-structure)
  (:documentation "Receive SYMBOLIC-STRUCTURE in the symbolic input zone." ))

(defgeneric  handle-symbol (host symbolic-structure)
  (:documentation "Handle SYMBOLIC-STRUCTURE from the symbolic input zone." ))

(defgeneric  handle-next-input (agent)
  (:documentation
   "Pop the next symbolic structure from the input zone and handle it." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;  Inner workings of symbolic processors, part 2 (see ARCHIT/SYMPROC1.LSP)

(defmethod  receive-symbol ((spr symbolic-processor)
                            (symbol symbolic-structure))
  (declare (values symbolic-structure))
  (setf (agent-input-zone spr)
        (nconc (agent-input-zone spr) (list symbol)))   ; FIFO
  symbol)

(defmethod  receive-symbol :after ((spr symbolic-processor)
                                   (symbol symbolic-structure))
  (declare (ignore symbol))
  (trigger-symbolic-processor spr :symbol-input) )  ; activate the processor

  ;; See DUAL/ARCHIT/TIME.LSP for an exceptional case involving time-related
  ;; symbolic structures.  They are put at the _front_ of the queue (LIFO).
  ;; See also the :AROUND method in DUAL/ARCHIT/HYBRID.LSP.


(defmethod  handle-symbol ((spr symbolic-processor)
                           (symbol symbolic-structure) )
  ;; This empty method is to be shadowed by more specific methods.
  #+:DUAL-DEBUG
    (warn "~A: I don't know how to handle ~S; Ignoring it." spr symbol)
  (if *ignore-suspensions*    ; Imitate S-PROGN's return values
      :suspension-ignored
      :suspended) )


(defmethod  handle-next-input ((spr symbolic-processor))
  (declare (values S-PROGN-return-type))
  (if (endp (agent-input-zone spr))
      nil
      (handle-symbol spr (pop (agent-input-zone spr))) ))


(defmethod  trigger-symbolic-processor ((spr symbolic-processor)
                                        (event (eql :symbol-input)) )
  (declare (values S-PROGN-return-type))
  (if (active-processor-p spr)
      nil      ; the processor is working and will notice the new symbol.
      (symbolic-microcycle spr) ))


(defmethod  symbolic-microcycle ((host symbolic-processor))
  (declare (values S-PROGN-return-type))
  (s-progn host
     (unless (endp (agent-input-zone host))   ; until empty input zone
       [handle-next-input host]               ; pop a symbol and handle it
       [symbolic-microcycle host] )))         ; loop
     ;; To do: Add garbage collection.
     ;; See also ARCHIT/NC_AGENT.LSP -- it defines two specific microcycles.


;;;; Error-signaling methods

(defmethod  receive-symbol ((x t) (y t))
  (error "RECEIVE-SYMBOL:  ~S is not a symbolic processor or ~
          ~S is not a symbolic structure."  x y ))

(defmethod  handle-symbol ((x t) (y t))
  (error "HANDLE-SYMBOL:  ~S is not a symbolic processor or ~
          ~S is not a symbolic structure."  x y ))

(defmethod  handle-next-input ((x t))
  (if x
    (error "HANDLE-NEXT-INPUT:  ~S is not a symbolic processor." x)
    (error "HANDLE-NEXT-INPUT applied to NIL (perhaps #$missing-agent).") ))


(defmethod  trigger-symbolic-processor ((spr symbolic-processor)
                                        (event symbol) )
  (cerror "Ignore the event and continue."
          "TRIGGER-SYMBOLIC-PROCESSOR: ~
           ~S does not know how to handle the event ~S."
          spr event) )

(defmethod  trigger-symbolic-processor ((x t) (y t))
  (if x
    (error "TRIGGER-SYMBOLIC-PROCESSOR: ~S is not a symbolic processor or ~
            ~S is not a symbol."  x y)
    (error "TRIGGER-SYMBOLIC-PROCESSOR applied to NIL ~
            (perhaps #$missing-agent).") ))


;;;;;;;  End of file DUAL/ARCHIT/SYMPROC2.LSP
