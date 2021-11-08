;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/agenda.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Agenda of suspended processes waiting for energy
;;; DEPENDS-ON: packages.lsp, defs.lsp,
;;;             archit/basic.lsp, archit/connect.lsp, archit/symproc1.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    20-03-97 [1.0]
;;; UPDATED:    28-11-97 [1.1.0]
;;; UPDATED:    28-01-98 [1.1.1]
;;; UPDATED:    18-04-98 [1.1.2]
;;; UPDATED:    18-05-98 Doing several *SYMB-SUBCYCLES* within one *TIME-SLICE*.
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...
;;;;;;;;
;;; TO DO:      Implement SP stacks with arrays. (Lists produce garbage.)

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;   AGENDA  FOR  SUSPENDED  PROCESSES   ;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; >>>>>>>>    See file  DUAL/ARCHIT/SUSPEND.TXT  for unified   <<<<<<<<<
;;;; >>>>>>>>    documentation of suspendable processing.         <<<<<<<<<

;;;; The key concepts defined in this file are SUSPENDED-PROCESS and AGENDA.
;;;;
;;;; DUAL specification postulates that each agent has a symbolic processor
;;;; that perform symbolic computations -- sequences of 'operations'.
;;;; The symbolic processor consumes 'energy' in order to carry out this task.
;;;; (See section 3.2.5.3. in "DUAL Report #1".)
;;;;
;;;; SUSPENDED PROCESSES defined in this file implement this notion.
;;;;   In a sentence, a suspended process is a process waiting for energy.
;;;;   Each suspended process is an object with three main fields:
;;;;     -- host   -- the agent performing the process;
;;;;     -- energy -- the energetic balance := supply - consumption;
;;;;     -- stack  -- a stack of closures that keeps the state of the process.
;;;;
;;;; Generally, if ENERGY < 0, the process has to wait. Due to external sources,
;;;; the energy gradually increases. When it exceeds zero, the process advances
;;;; a little. Advancing a process is done by FUNCALL-ing closures from its
;;;; stack. They are called for their side effects, one of which is decreasing
;;;; the energy level. When the latter drops below 0, the process stops again.
;;;;
;;;;;;;;;;
;;;; A DUAL system consists of many agents. Each one of them can potentially be
;;;; executing a symbolic process at any instant in time. Thus, there are many
;;;; independent processes that run, wait, run again, and finally terminate.
;;;;
;;;; The AGENDA is the current implementation of the notion of having several
;;;; independent (suspended) processes running in parallel.
;;;; Agendas belong strictly to the implementational domain. The conceptual
;;;; specification of the architecture does not use the term.
;;;;
;;;; Basically, the agenda is simply a set of suspended processes.
;;;; (Note: Our agenda is NOT like the agendas used in some other AI programs.
;;;;   In particular, it does not control the priority of the processes.
;;;;   All processes with positive energetic balance run, all others wait.)
;;;;
;;;;;;;;;;
;;;; The specification of DUAL postulates that each DUAL agent has only one
;;;; symbolic processor (see section 3.2.5.3 of DR#1). From that follows, that
;;;; there should be at most one suspended process with a given host.
;;;; The present implementation, however, does not inforce that.
;;;;
;;;; Note, in particular, the following code fragments:
;;;;   1.  (push-to-stack           (find-or-make-process host) form)
;;;;   2.  (bury-at-bottom-of-stack (find-or-make-process host) form)
;;;;   3.  (push-to-stack           (make-process host)         form)
;;;;
;;;;   All three fragments arrange that FORM will be executed by the symbolic
;;;;   processor of the agent HOST.
;;;;   When it is idle (i.e. (find-process host) --> NIL), the three requests
;;;;   are equivalent. When HOST is not idle, however, there is a big diffrnce:
;;;;      1--> the new FORM will be executed _before_ any old forms that
;;;;                                  are currently waiting to run under HOST.
;;;;      2--> the new FORM will be executed _after_ any old forms under HOST.
;;;;      3--> the new FORM will be executed _in_parallel_ with the old forms.
;;;;   In the last case (3), there will be two suspended processes on agenda
;;;;   having the same host. In 'conceptual' terms, this means that the agent
;;;;   HOST has two independed symbolic processors running in parallel.
;;;;


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: suspended-process, make-process, suspended-process-p,
;;          process-host, process-energy, process-stack,
;;          push-to-stack, bury-at-bottom-of-stack,
;;          supply-energy, consume-energy-with-losses, busy-wait,
;;          activate-process, run-process,
;;          *agenda*, number-of-processes,
;;          find-process, find-or-make-process, remove-from-agenda,
;;          do-all-processes, run-all-processes, run1-all-processes
;;
;; Note also:  *expected-number-of-processes*, *symb/conn-ratio*,
;;             *time-slice*, and *symb-subcycles*  defined in DUAL/DEFS.LSP.
;;
;; The file DUAL/INTRFACE/AGENDA.LSP defines some convenient shorthands.

;; *AGENDA*
;;
;;   A global variable that stores the current set of suspended processes.
;;
;;   It is not recommended to alter the contents of this variable through
;;   any means other than the appropriate housekeeping functions:
;;     MAKE-PROCESS, FIND-OR-MAKE-PROCESS, and REMOVE-FROM-AGENDA.

;; NUMBER-OF-PROCESSES ()  --> number
;;
;;   A function that returns the number of suspended processes that are
;;   currently on *AGENDA*.
;;   Always returns a non-negative integer.
;;
;;   NUMBER-OF-PROCESSES is equivalent to  (length *agenda*)  except that
;;   is potentially more efficient.


;; MAKE-PROCESS (host)  -->  new-process
;;
;;   A function that creates a suspended process and puts it on *AGENDA*.
;;   HOST must be an agent, otherwise an error is signaled.
;;   No checks are made whether a process with the same host already is on
;;   agenda. Better use FIND-OR-MAKE-PROCESS instead of MAKE-PROCESS.
;;   MAKE-PROCESS returns the new suspended process.
;;
;;   After a successful call to MAKE-PROCESS, the following conditions hold:
;;     (find-process host)                -->  new-process
;;     (active-processor-p host)          -->  T
;;     (suspended-process-p new-process)  -->  T
;;     (process-host new-process)         -->  host
;;     (process-energy new-process)       -->  0.0
;;     (process-stack new-process)        -->  NIL
;;

;; SUSPENDED-PROCESS-P  (thing)  -->  T or NIL
;;
;;   A predicate that checks whether something is a suspended process.


;; PROCESS-HOST (process)  -->  agent
;;
;;   A generic function that reads the host of PROCESS. Returns an agent.
;;   Each suspended process always has a host which is assigned to the
;;   process at the moment of its construction (by a call to MAKE-PROCESS)
;;   and cannot be modified.
;;   PROCESS must be a suspended process, otherwise an error is signaled.
;;
;;   It is always true that:
;;      (process-host (find-or-make-process host))  -->  host
;;      (setf (process-host process) 'whatever)     signals an error

;; PROCESS-ENERGY (process)  -->  energy-balance
;; (SETF PROCESS-ENERGY)
;;
;;   A generic function that accesses the energy balance of PROCESS.
;;   Returns a single-float. May be used with SETF (though this is not recomm.)
;;   Signals an error if PROCESS is not a suspended process.
;;
;;   The 'energy balance' of a process is the difference between supply and
;;   consumption. When it is negative, the process must wait. When it is
;;   positive, the process can (and should) advance (see RUN-PROCESS below).
;;
;;   It is not recommended to use explicit calls to (SETF (PROCESS-ENERGY ..)..)
;;   Use ACTIVATE-PROCESS, SUPPLY-ENERGY, and CONSUME-ENERGY-WITH-LOSSES instead
;;
;;   It is always true that:
;;      (process-energy (make-process host))  -->  0.0

;; PROCESS-STACK (process)  -->  list-of-closures
;; (SETF PROCESS-STACK)
;;
;;   A generic function that accesses the stack of PROCESS.
;;   Returns a list of closures. May be used with SETF (though not recommended).
;;   Signals an error if PROCESS is not a suspended process.
;;
;;   It is not recommended to use explicit calls to (SETF (PROCESS-STACK ..)..)
;;   Use PUSH-TO-STACK, BURY-AT-BOTTOM-OF-STACK, and RUN-PROCESS instead.
;;
;;   It is always true that:
;;      (process-stack (make-process host))  -->  nil
;;
;;   See the discussion on stack's debuggability in DUAL/ARCHIT/SPROGN.LSP.
;;   See the PROC facility in DUAL/INTRFACE/AGENDA.LSP.


;; SUPPLY-ENERGY (process amount)  -->  new-energy-balance
;;
;;   A function that adds (algebraically) an AMOUNT of energy to the energy
;;   balance of PROCESS. The energy is considered to come from some unspecified
;;   external source. (Cf. ACTIVATE-PROCESS.)
;;
;;   PROCESS should be a suspended process, otherwise an error is signaled.
;;   AMOUNT should be a number. (It usually is positive.)
;;   The function returns the new energetic balance of PROCESS.
;;
;;   More precisely, (SUPPLY-ENERGY PROCESS AMOUNT) is equivalent to:
;;     (incf (process-energy process)
;;           amount)

;; CONSUME-ENERGY-WITH-LOSSES (process amount)  -->  new-energy-balance
;;
;;   A function that subtracts (algebraically) the energy equivalent to
;;   AMOUNT of work from the energy balance of PROCESS.
;;   PROCESS should be a suspended process, otherwise an error is signalled.
;;   AMOUNT should be a number. (It usually is positive.)
;;   The function returns the new energetic balance of PROCESS.
;;
;;   Not all _energy_ is converted into useful _work_, however. There are
;;   some losses that depend on the efficiency coefficient of the host.
;;   (See the generic function AGENT-EFFICIENCY, defined in ARCHIT/SYMPROC1.LSP)
;;
;;   Usually, the eff. coeficient is less than 1 so that the decrement in the
;;   energy balance is bigger than AMOUNT. In other words, the combination:
;;     (progn (supply-energy p amount) (consume-energy-with-losses p amount))
;;   leaves the process P with worse energy balance.
;;
;;   More precisely, (CONSUME-ENERGY-WITH-LOSSES PROCESS AMOUNT) is equiv. to:
;;     (decf (process-energy process)
;;           (/ amount (agent-efficiency (process-host process))))
;;


;; ACTIVATE-PROCESS (process)  -->  new-energy-balance
;;
;;   A function that supplies a suspended process with energy produced by the
;;   connectionist aspect of the host (see DUAL/ARCHIT/CONNECT.LSP).
;;   This function is to be invoked on each 'symbolic subcycle'.
;;   In essence, it integrates connectionist energy over time. Therefore, it
;;   depends on the parameters *TIME-SLICE* and *SYMB-SUBCYCLES* defined in
;;   DUAL/DEFS.LSP.
;;
;;   The parameter *SYMB/CONN-RATIO* (see DEFS.LSP) is also taken into account.
;;   Large values (> 1) on this parameter increase the speed of symbolic
;;   processing relative to the spreading activation speed.
;;
;;   More precisely, (ACTIVATE-PROCESS PROCESS) is equivalent to:
;;     (supply-energy process
;;                    (* (agent-activation (process-host process))
;;                       (/ *time-slice* *symb-subcycle*)
;;                       *symb/conn-ratio*) )
;;
;;   WARNING!!!  The implementation is free to optimize the expression above by
;;     precomputing the constant term and pulling it out of the inner loop.
;;     Therefore, local binding of special variables is not guaranteed to work.


;; BUSY-WAIT (carrier)  -->  suspended-process or nil
;;
;;   A function that sets the energetic balance to 0.0. This is convenient if
;;   the process has to wait for some external event. The process will be
;;   resumed on the next connectionist 'cycle' (see ACTIVATE-PROCESS).
;;
;;   CARRIER should be either an agent or a suspended process. When it is an
;;   agent, the corresponding process (if any) is retrieved via FIND-PROCESS.
;;   If CARRIER is neither an agent nor a process, BUSY-WAIT signals an error.
;;   The function returns the suspended process when there is one, or NIL.
;;   (NIL is returned only when CARRIER is an agent and  (find-process carrier)
;;    returns NIL.)
;;
;;   When BUSY-WAIT returns non-NIL, it is always true that:
;;     (zerop (process-energy (busy-wait carrier)))  -->  T
;;


;; PUSH-TO-STACK (process &rest forms)  -->  new-stack-contents
;;
;;   A macro that expands into:  `(push (process-stack ,process)
;;                                      #'(lambda () ,.forms))
;;
;;   The effect of executing this macro is that FORMS will be executed _before_
;;   any closures that are already waiting in the stack.
;;
;;   See the discussion on stack's debuggability in DUAL/ARCHIT/SPROGN.LSP.

;; BURY-AT-BOTTOM-OF-STACK (process &rest forms)  -->  new-stack-contents
;;
;;   A macro that is analogous to PUSH-TO-STACK except that the new closure
;;   is added at the _bottom_ of the stack, not at the top.
;;   (NOTE: If the generic function PROCESS-STACK has side effects, it is
;;          _not_ guaranteed that it will be executed only once.)
;;
;;   The effect of executing this macro is that FORMS will be executed _after_
;;   all closures that are already waiting in the stack.


;; RUN-PROCESS (process)  -->  :COMPLETED  or  :SUSPENDED
;;
;;   A function that advances a suspended process as far as possible.
;;   Returns either :COMPLETED or :SUSPENDED.
;;   Signals an error if PROCESS is not a suspended process.
;;
;;   The process is advanced according to the following algorithm:
;;     1. If the stack is empty, remove the process from *AGENDA*, return
;;           the keyword :COMPLETED, and stop.
;;     2. If the energy balance is >= 0, pop a closure from (the top of) the
;;           stack and execute it. Then loop back to step 1.
;;     3. If the energy balance is negative, return :SUSPENDED and stop.
;;
;;   The closures in the stack are executed for their side effects only.
;;   Two important side effects are:
;;     -- a closure can push other closures to the stack
;;     -- a closure can consume energy (see CONSUME-ENERGY-WITH-LOSSES)
;;
;;   The closures should take care that the loop eventually terminates,
;;   either because of empty stack or because of negative energetic balance.
;;
;;   The macro S-PROGN, defined in DUAL/ARCHIT/SPROGN.LSP, is the method of
;;   choice for dealing with suspended computations.
;;   See also RUN-ALL-PROCESSES below.
;;
;;   It is always true that (provided RUN-PROCESS returns, of course):
;;      (ecase (run-process proc)
;;        (:suspended (minusp (process-energy proc)))       -->  T
;;        (:completed (null   (find proc *agenda* ))) )


;; FIND-PROCESS (host)  -->  process  or  NIL
;;
;;   A function that looks in *AGENDA* for a process whose PROCESS-HOST
;;   matches (EQ) HOST.
;;   HOST should be an agent, otherwise and error is signaled.
;;
;;   If there isn't any matching process, FIND-PROCESS returns NIL.
;;   If there is, FIND-PROCESS returns the (leftmost) process.

;; FIND-OR-MAKE-PROCESS (host)  -->  process
;;
;;   A function that looks in *AGENDA* for a process whose PROCESS-HOST
;;   matches (EQ) HOST. If there isn't any, a new process is constructed,
;;   put on *AGENDA* and returned.
;;   HOST must be an agent, otherwise an error is signaled.
;;   FIND-OR-MAKE-PROCESS returns a suspended process.
;;
;;   FIND-OR-MAKE-PROCESS first checks whether the needed process is already
;;   on *AGENDA* (via FIND-PROCESS). If such process is found, it is returned.
;;   Otherwise, FIND-OR-MAKE-PROCESS appeals to MAKE-PROCESS.
;;
;;   After a successful call to FIND-OR-MAKE-PROCESS, the following holds:
;;     (find-process host)                -->  new-process
;;     (active-processor-p host)          -->  T
;;     (suspended-process-p new-process)  -->  T
;;     (process-host new-process)         -->  host


;; REMOVE-FROM-AGENDA (carrier)  -->  number
;;
;;   A function that removes a suspended process from *AGENDA*.
;;   CARRIER should be either an agent or a suspended process. When it is an
;;   agent, the corresponding process (if any) is retrieved via FIND-PROCESS.
;;   If CARRIER is neither an agent nor a process, BUSY-WAIT signals an error.
;;   The function returns the number of processes that remain on agenda.
;;
;;   When the suspended process denoted by CARRIER is located, it is removed
;;   from *AGENDA*. As an additional safety measure, the energetic level of
;;   the 'dead' process is set to -1000.
;;   If there isn't any process that matches CARRIER, REMOVE-FROM-AGENDA does
;;   not do anything. (It still returns the number of processes, of course.)


;; DO-ALL-PROCESSES (var [resultform]) {form}*   -->  dependent-on-RESULTFORM
;;
;;   A macro that iterates through all suspended processes on *AGENDA* (in an
;;   unspecified order).  Similar to DOLIST.
;;
;;   VAR should be a symbol. It is not evaluated and serves as a variable name.
;;   RESULTFORM is an arbitrary LISP form. When not supplied it defaults to NIL.
;;
;;   DO-ALL-PROCESSES executes the forms in the body once for each suspended
;;   process on *AGENDA*, in some implementation-dependent order, with the
;;   variable VAR bound to the process.  Then RESULTFORM (a single form, not
;;   an implicit PROGN) is evaluated, and the result is the value of the
;;   DO-ALL-PROCESSES form. When the RESULTFORM is evaluated, the control
;;   variable VAR is still bound and has the value NIL.  If RESULTFORM is
;;   omitted, the result is NIL.
;;
;;   An explicit RETURN statement may be used to terminate the loop and return
;;   a specified value.  Declarations may appear at the front of the body. Tags
;;   and GO's are not allowed (i.e. DO-ALL-PROCESSES is not an implicit TAGBODY)
;;
;;   If the user-supplied forms in the body _remove_ a suspended process from
;;   the agenda while DO-ALL-PROCESSES is in progress, the results are
;;   unpredictable, with one exception: it is possible to remove the process
;;   currently being analyzed. For example, RUN-PROCESS does this when the
;;   process stack is empty.
;;   Processes may be _added_ to the agenda while DO-ALL-PROCESSES is in
;;   progress (via a call to MAKE-PROCESS or FIND-OR-MAKE-PROCESS). It is
;;   implementation-dependent whether the user-supplied forms will be called on
;;   the newly-added processes during the current execution of DO-ALL-PROCESSES.
;;   It is guaranteed, however, that the newly-added processes will be heeded
;;   during next invocations of DO-ALL-PROCESSES.
;;
;;   Example:
;;     (do-all-processes (proc) (supply-energy proc 0.5)))  -->  NIL
;;
;;   See also DO-ALL-AGENTS in DUAL/ARCHIT/BASIC.LSP,
;;     DO-ALL-REFERENCES in DUAL/ARCHIT/MCRFRAME.LSP, and
;;     DO-ALL-WM in DUAL/ARCHIT/WORK_MEM.LSP.


;; RUN-ALL-PROCESSES  ()  -->  N
;; RUN1-ALL-PROCESSES ()  -->  N
;;
;;   Ideally, RUN-ALL-PROCESSES is the only functions from this file to be
;;   called explicitly. (The rest are used implicitly by S-PROGN, S-EVAL, etc.)
;;
;;   The function RUN-ALL-PROCESSES is one of the two pillars of DUAL's
;;   macrolevel. It implements the so-called 'symbolic mAcrocycle'.
;;   (Don't confuse with the generic function SYMBOLIC-MiCROCYCLE defined in
;;    DUAL/ARCHIT/SYMPROC2.LSP. See also MAIN-DUAL-CYCLE in DUAL/TOPLEVEL.LSP.)
;;   The connectionist counterpart of RUN-ALL-PROCESSES is the function SPREAD
;;   (the 'connectionist macrocycle') defined in DUAL/ARCHIT/SPREAD.LSP.
;;
;;   One symbolic macrocycle divides into several symbolic subcycles with
;;   proportionally smaller discretization steps. The parameter *SYMB-SUBCYCLES*
;;   controls the number of subcycles within a macrocycle.
;;
;;   The function RUN-ALL-PROCESSES does *SYMB-SUBCYLCES* calls to RUN1-ALL-
;;   PROCESSES and finally returns the number of proc's remaining on *AGENDA*.
;;
;;   In turn, the symbolic subcycle forks into several (pseudo)parallel symbolic
;;   MICROcycles (see DUAL/ARCHIT/SYMPROC2.LSP).  More precisely, RUN1-ALL-
;;   PROCESSES works according to the following algorithm:
;;     1. Invoke ACTIVATE-PROCESS on all processes on *AGENDA*.
;;     2. Find the 'active subset' of *AGENDA*, i.e. pick up processes whose
;;        energy balance is greater than zero (inclusive). Those are the
;;        processes that are ready to run.
;;     3. Sort the active subset according to the energy balance putting the
;;        most active process first.
;;     4. Invoke RUN-PROCESS on each process from the active subset in order.
;;        RUN-PROCESS may add new processes to the main *AGENDA*. It may
;;        also remove self (that is, the process being run) from *AGENDA*.
;;        Changes to *AGENDA* take effect on the next cycle.
;;     5. Return the new number of processes (as if by NUMBER-OF-PROCESSES).
;;

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

;;  Push :DUAL-DEBUG to the *FEATURES* list in order to be able to see
;;  closures as human-readable lambdas.  See DUAL/START_ME.LSP for details.

(defvar  *agenda*
         (make-array  *expected-number-of-processes*
                      :element-type '(or null suspended-process)
                      :fill-pointer 0
                      :adjustable t
                      :initial-element nil )
  "Set of suspended processes waiting for energy."  )

(declaim (inline number-of-processes))
(defun number-of-processes ()
  (fill-pointer *agenda*) )


;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass suspended-process (DUAL-object)
    ((host        :initarg        :host
                  :reader         process-host
                  :type           symbolic-processor
                  :initform       (required-argument)
                  :documentation  "The agent doing the computations." )
     (energy      :accessor       process-energy
                  :type           float
                  :initform       0.0
                  :documentation  "The energy balance: supply - consumption."  )
     (stack       :accessor       process-stack
                  :type           list
                  :initform       nil
                  :documentation  "A stack of closures."  )
   #+:DUAL-DEBUG
   ;; See the section on 'debugging suspendable code' in DUAL/ARCHIT/SPROGN.LSP.
     (l-stack     :accessor       process-l-stack
                  :type           list
                  :initform       nil
                  :documentation  "LAMBDA equivalents to the closures in STACK.")
    )
    (:documentation "Process waiting for energy.")
  ) ; defclass
) ; eval-when

;;;; Constructor

(defun make-process (host)
  "Constructs a suspended-process and pushes it to *AGENDA*."
  (declare (values suspended-process))
  (unless (agentp host)
    (error "MAKE-PROCESS: ~S is not an agent." host ))
  (let ((new-process (make-instance 'suspended-process :host host)))
    (vector-push-extend new-process *agenda*)
    (setf (agent-flag host *on-agenda-flag*) 1) ; see ACTIVE-PROCESSOR-P in
    new-process))                                   ; DUAL/ARCHIT/SYMPROC1.LSP


;;;; Type predicate

(declaim (inline suspended-process-p))

(defun suspended-process-p (thing)
  (if (typep thing 'suspended-process) t nil))


;;;;  Type-checking methods for the accessors

(defmethod  process-host ((x t))
  (error "PROCESS-HOST:  ~S is not a suspended process." x))

(defmethod  (setf process-host) (new-value (x t))
  (declare (ignore new-value))
  (error "PROCESS-HOST cannot be used with SETF. Hosts are immutable.") )

(defmethod  process-energy ((x t))
  (error "PROCESS-ENERGY:  ~S is not a suspended process." x))

(defmethod  (setf process-energy) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF PROCESS-ENERGY):  ~S is not a suspended process." x))

(defmethod  process-stack ((x t))
  (error "PROCESS-STACK:  ~S is not a suspended process." x))

(defmethod  (setf process-stack) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF PROCESS-STACK):  ~S is not a suspended process." x))


;;;;;;  Printing methods

(defmethod print-object ((sp suspended-process) stream)
   (format stream "#<SP ~S>"
                  (if (slot-boundp sp 'host)
                      (process-host sp)
                      "(no host)") ))

(defmethod DUAL-describe ((sp suspended-process)
                          &optional (stream *standard-output*))
   (format stream "~&~S is a suspended process.~%" sp)
   (format stream "~&  Its host is: ~S~%"
                  (if (slot-boundp sp 'host)
                      (process-host sp)
                      "(unbound, which is anomalous)" ) )
   (format stream "~&  Its energy is: ~:[(unbound slot)~;~:*~6,3F~]~%"
                  (if (slot-boundp sp 'energy)
                      (process-energy sp)
                      nil) )
   (format stream "~&  Its stack ~:[is unbound~;~:*contains ~D closure~:P~]:~%"
                  (if (slot-boundp sp 'stack)
                      (length (process-stack sp))
                      nil) )
   #+:DUAL-DEBUG (format stream "~{    ~S~%~}" (process-l-stack sp))
  (values))


;;;; Stack-related routines

(defmacro push-to-stack (proc &rest forms)
  "Pushes closure(s) to PROCESS-STACK."
  #-:DUAL-DEBUG
   `(push #'(lambda () ,.forms)
          (process-stack ,proc))
  #+:DUAL-DEBUG
   `(progn
      (push #'(lambda () ,.forms)       ; for the LISP evaluator
            (process-stack ,proc))
      (push '(lambda () ,.forms)        ; overhead for the human user
            (process-l-stack ,proc)) )
) ; push-to-stack
;; See the section on 'debugging suspendable code' in DUAL/ARCHIT/SPROGN.LSP.

(defmacro bury-at-bottom-of-stack (proc &rest forms)
  "Adds closure(s) at the bottom of PROCESS-STACK."
  #-:DUAL-DEBUG
   `(setf (process-stack ,proc)
          (nconc (process-stack ,proc)
                 (list #'(lambda () ,.forms))))
  #+:DUAL-DEBUG
   `(progn
      (setf (process-stack ,proc)                 ; for the LISP evaluator
            (nconc (process-stack ,proc)
                   (list #'(lambda () ,.forms))))
      (setf (process-l-stack ,proc)               ; overhead for the human user
            (nconc (process-l-stack ,proc)
                   (list '(lambda () ,.forms)))) )
) ; bury-at-bottom-of-stack


(defun run-process (process)
  "Runs a process until all energy is consumed. If completed, remove."
  (declare (type suspended-process process)
           (values (member :completed :suspended)) )
  (loop
    (cond ((endp (process-stack process))               ; Everything completed?
              (remove-from-agenda-aux process)
              (return :completed) )
          ((not (minusp (process-energy process)))      ; Energy available?
              #+:DUAL-DEBUG (pop (process-l-stack process))
              (funcall (pop (process-stack process)))      ; run one step
              )                                            ; and loop
          (t  (return :suspended)) )))                  ; Give up and wait.

 ;; (Consider wrapping an UNWIND-PROTECT to protect the operation that pops
 ;;  the lambda stack against errors during evaluation of the closure
 ;;  from the main stack.  See the example at p.189 in CLtL2.  The simpler
 ;;  strategy of popping the l-stack first should work in most cases.)

 ;; (REMOVE-FROM-AGENDA sets the energy level to -1000 and thus blocks the
 ;;  'dead' process from being worked upon by RUN-PROCESS.)

;;;; Energy-related functions

(defvar  *time-subslice*  (* (/ *time-slice* *symb-subcycles*)
                             *symb/conn-ratio*)
  "Should _always_ be  (* (/ *time-slice* *symb-subcycles*) *symb/conn-ratio*)"
  )
(declaim (type float *time-subslice*))
  ;; Optimization for RUN-ALL-PROCESSES and RUN1-ALL-PROCESSES (see below).
  ;; Pull this calculation out of the inner loop.

(defun activate-process (process)
"Supplies energy produced by the connectionist aspect of PROCESS' host.
Depends on *SYMB/CONN-RATIO*, *TIME-SLICE*, and *SYMB-SUBCYCLES*."
  (declare (type suspended-process process))
  (supply-energy process
                 (* *time-SUBslice*
                    (agent-activation (process-host process))) ))

(defun supply-energy (process amount)
  "Supply (exogeneous) AMOUNT of energy to PROCESS. Return new energy balance."
  (declare (type suspended-process process)
           (type float amount))
  (incf (process-energy process) amount))

(defun consume-energy-with-losses (process amount)
  "Decrement PROCESS' energy balance, taking into account AGENT-EFFICIENCY."
  (declare (type suspended-process process)
           (type float amount))
  (decf (process-energy process)
        (/ amount (agent-efficiency (process-host process)))) )

(defun busy-wait (carrier)
  "Set the energetic balance to zero. CARRIER is an agent or a process."
  (declare (type (or symbolic-processor suspended-process) carrier)
           (values (or null suspended-process)) )
  (etypecase carrier
    (symbolic-processor (let ((proc (find-process carrier)))
                          (cond ((null proc) nil)
                                (t (setf (process-energy proc) 0.0) proc))))
    (suspended-process  (setf (process-energy carrier) 0.0) carrier) ))


;;;;;;  *****    A G E N D A   facilities    *****
;;;
;;;  The agenda is implemented as an adjustable array with a fill pointer.
;;;  The agenda is kept in the global variable *AGENDA*.  There is also an
;;;  auxiliary variable for the purposes of RUN-ALL-PROCESSES (see below).
;;;

(defun find-process (host)
  "Looks for a process with HOST in *AGENDA*. Returns the process or NIL."
  ;; For more robustness, FIND-PROCESS does not depend on *ON-AGENDA-FLAG*.
  (declare (values (or null suspended-process)))
  (find host *agenda* :key #'process-host) )

(defun find-or-make-process (host)
  "Looks for a process with HOST in *AGENDA*. Costructs one if needed."
  (declare (values suspended-process)
           (type (or symbolic-processor suspended-process) host) )
  ;; FIND-OR-MAKE-PROCESS does not rely on *ON-AGENDA-FLAG*.
  (cond ((find-process host) )
        (t (make-process host)) ))


(defun remove-from-agenda (carrier)
  "Removes a (completed) process from *AGENDA*."
  (declare (type (or symbolic-processor suspended-process) carrier)
           (values fixnum) )
  (etypecase carrier
    (symbolic-processor (let ((proc (find-process carrier)))
                          (if (null proc)
                              (number-of-processes)
                              (remove-from-agenda-aux proc)) ))
    (suspended-process  (remove-from-agenda-aux carrier)) ))

(defun remove-from-agenda-aux (process)
  (declare (values fixnum)
           (type suspended-process process) )
  (setf (agent-flag (process-host process) *on-agenda-flag*) 0)
  (setf (process-energy process) -1000.0 )  ; in case smbdy persists to run it
  (when (zerop (fill-pointer *agenda*))     ; pathological case: empty *AGENDA*
    (return-from remove-from-agenda-aux 0))
  (let ((pos (position process *agenda*)))
    (unless (null pos)                                    ; unless not there
      (setf (aref *agenda* pos) (vector-pop *agenda*))    ; move LAST in the gap
      (setf (aref *agenda* (fill-pointer *agenda*)) nil)) ; allow for garb.coll.
    (fill-pointer *agenda*) ))    ; The number of remaining proc.



;;;;;;;;;   Iteration across all processes on *AGENDA*    ;;;;;;;;;;
;;

(defmacro do-all-processes  (header &body body)
  "Iterates over all suspended processes on *AGENDA*. Similar to DOLIST."
  (unless (and (listp header)
               (<= 1 (length header) 2)
               (symbolp (first header)) )    ; var-name
    (error "DO-ALL-PROCESSES: Malformed header in ~S."
           (list* 'do-all-processes header body) ))
  (let ((var-name (first header))
        (result-form (second header)) )  ; possibly NIL for one-element headers
    `(block nil
       (dualc::do-all-processes-aux
         #'(lambda (,var-name) ,@body))
       ,(DOLIST-finalization var-name result-form)) ))   ; see ARCHIT/BASIC.LSP

#+:ACLPC
(eval-when (load)      ; provide a more readable message for the status line
  (when allegro:*save-lambda-lists*
    (setf (get 'do-all-processes 'allegro:lambda-list)
          '((var &optional result) &rest body)) ))

(defun do-all-processes-aux (fun)
  "Calls FUN over all suspended-processes on *AGENDA*. Returns NIL."
  (declare (type (function (suspended-process) t) fun) )
  (flet ((do-one-call (curr-pos curr-proc)
           (funcall fun curr-proc)          ; FUN may call REMOVE-FROM-AGENDA
           (eq curr-proc (aref *agenda* curr-pos)) ))
    (let ((k 0))  ; index in the array *AGENDA*
      (loop
         ;; VARIANT:   d := (fill-ptr - k) decreases with 1 on each iteration
         ;; INVARIANT: When d > 0, A[k] contains a process to be fetched to FUN
         ;;            and all A[0] ... A[k-1] have been FUN-ed already.
         (when (>= k (fill-pointer *agenda*))
            (return nil))
         (if (do-one-call k (aref *agenda* k))
             (incf k)               ; go to next position
             )                      ; intercepting removal -- stay on same pos.
       ))))


;;;;;;  *****    S Y M B O L I C   M A C R O C Y C L E   *****
;;;
;;;  'Symbolic macrocycle' is defined as the union of all symbolic processing
;;;  that happen during one elementary time interval (*TIME-SLICE*).
;;;  In this program, the symbolic macrocycle is implemented by the function
;;;  RUN-ALL-PROCESSES.  The sumbolic macrocycle is divided into several
;;;  'symbolic subcycles' with proportionally smaller time discretization.
;;;  (Do not confuse the symbolic mAcrocycle with the symbolic mIcrocycle.
;;;   The microcycle depends on the agent -- see DUAL/ARCHIT/SYMPROC2.LSP.)
;;;
;;;  One sumbolic macrocycle divides into several symbolic subcycles -- the
;;;  function RUN-ALL-PROCESSES is roughly equivalent to:
;;;    (dotimes (sub *symb-subcycles*)   ; *SYMB-SUBCYCLES* defined in DEFS.LSP
;;;      (run1-all-processes))
;;;
;;;  In turn, one symbolic subcycle forks into several (pseudo)parallel symbolic
;;;  MICROcycles (see DUAL/ARCHIT/SYMPROC2.LSP) -- the function RUN1-ALL-PROC.
;;;  is _roughly_ equivalent to:          ;; See more detailed algorithm below.
;;;    (do-all-processes (proc) (activate-process proc))
;;;    (do-all-processes (proc) (run-process proc))
;;;
;;;  The *TIME-SLICE* of a macrocycle is divided by the number of subslices
;;;  to yield *TIME-SUBSLICE* (see below).  Theoretically, *TIME-SLICE* is
;;;  infinitesimally small and *SYMB-SUBCYCLES* is 1.  For efficiency reasons,
;;;  however, *TIME-SLICE* is left somewhat larger and is divided into *TIME-
;;;  SUBSLICE*'s.  Thus the big connectionist loop (through the whole WM) is
;;;  done less frequently while the small symbolic loop (through the agenda) is
;;;  still fine enough.
;;;  (The connectionist aspect is more inert and works well at coarser      )
;;;  ( discretization levels.  Therefore, it has been chosen to do several  )
;;;  ( symb. subcycles within one conn. cycle (see DUAL/ARCHIT/SPREAD.LSP). )
;;;
;;;  On each 'symbolic subcycle' the active subset of *AGENDA* is copied
;;;  to *AGENDA-AUX* where it is sorted and run.  This subset should be
;;;  quite small. Theoretically, *TIME-SLICE* (and *TIME-SUBSLICE* even more)
;;;  is infinitesimally small.  The implementation fits entirely the conceptual
;;;  specification when *TIME-SUBSLICE* is made so small that at any single
;;;  time subslice there is at most one runable process on *AGENDA*.  Very low
;;;  values of *TIME-SLICE* (and hence *TIME-SUBSLICE*) are very inefficient,
;;;  however.  Therefore, the program allows for more than one runable processes
;;;  during a time subslice -- this is the 'active subset' of *AGENDA* mentioned
;;;  above.
;;;
;;;  The runable processes are sorted according to their energy levels.  Thus,
;;;  the most active process will fire first, which approximates the conceptual
;;;  specification of the architecture.  The approximation is not perfect,
;;;  however, as revealed by the following counter-example:
;;;     proc  energy       operation   consumption
;;;    ---------------------------------------------
;;;      X     0.15           X-1        0.01
;;;                           X-2        0.1
;;;      Y     0.12           Y-1        0.02
;;;                           Y-2        0.1
;;;
;;;  The present implementation will trigger the operations in the order:
;;;    X-1, X-2, Y-1, Y-2
;;;  while the theoretically motivated order is:
;;;    X-1, Y-1, X-2, Y-2
;;;
;;;  A more precise (and inefficient) implementation should re-arrange the
;;;  agenda after each FUNCALL, possibly abandoning one process in favor of
;;;  another and then returning back to the first.  This is not done in the
;;;  present implementation, however, based on the following considerations:
;;;    -- The 'perfectionist strategy' is much less efficient.
;;;    -- The anomalous situations like those in the example above are quite
;;;       rare in practice. The activation accumulated during a single time
;;;       slice, inflated by multiplication by *SYMB/CONN-RATIO* and
;;;       *TIME-SLICE*, is usually insufficient to cover the consumptions of
;;;       two operations in a row.  Consequently, each process from the 'active
;;;       set' advances only one step and stops.  Then the next in queue runs
;;;       one step, then the next, etc., ordered by energy levels.
;;;    -- Even when anomalies do happen, they rarely involve agents which
;;;       compete directly with one another.  Thus, the small error in the
;;;       timing of operations is irrelevant.
;;;    -- The specification of DUAL leaves some room for indeterminacy.
;;;       Section 3.2.5.5. of DR#1 says that 'ties are (almost) impossible
;;;       in theory, very rare in practice, and may be resolved arbitrarily'.
;;;
;;;  There is a rudimentary step facility for suspendable processes.  It is
;;;  documented and implemented (ST and ST!) in DUAL/INTRFACE/AGENDA.LSP.

(defvar  *agenda-aux*                  ; for internal use only
         (make-array  (ceiling *expected-number-of-processes* 2)
                      :element-type '(or null suspended-process)
                      :fill-pointer 0
                      :adjustable t
                      :initial-element nil )
  "Subset of *AGENDA* with non-negative PROCESS-ENERGY." )


;; RUN-ALL-PROCESSES involve the following steps:
;;   1. Make sure that *TIME-SUBSLICE* is set correctly.
;;   2. Do *SYMB-SUBCYCLES* calls to RUN1-ALL-PROCESSES (i.e. subcycles).
;;   3. Return the new number of processes.

(defun run-all-processes ()
  "Do *SYMB-SUBCYCLES* calls to RUN1-ALL-PROCESSES (i.e. subcycles)"
  (setq *time-subslice*                            ; SETQ, not LET
        (* (/ *time-slice* (coerce *symb-subcycles* 'float))
           *symb/conn-ratio*))
  (dotimes (sub *symb-subcycles*)    ;
    (run1-all-processes))            ; <-- main work
  (number-of-processes) )


;; RUN1-ALL-PROCESSES does the main work -- it implements one symbolic SUBcycle
;; and involves the following steps.
;;   1. Invoke ACTIVATE-PROCESS on all processes on *AGENDA*.
;;   2. Find the 'active subset' of *AGENDA*, i.e. the processes with non-nega-
;;      tive energy balance. Those are the processes that are ready to run
;;      during the current cycle. Copy them to *AGENDA-AUX*.
;;      (Usually *AGENDA-AUX* contains only a few processes. Ideally only one.)
;;   3. Sort *AGENDA-AUX* putting most active process first.
;;   4. Invoke RUN-PROCESS on the sorted subset (i.e. on *AGENDA-AUX*).
;;      RUN-PROCESS may cause processes to be added to or removed from *AGENDA*.
;;   5. Return the new number of processes.
(defun run1-all-processes ()
  "Do ACTIVATE-PROCESS and RUN-PROCESS to all processes in *AGENDA*."
  (setf (fill-pointer *agenda-aux*) 0)              ; prepare
  (dotimes (k (fill-pointer *agenda*))
    (let ((proc (aref *agenda* k)))                 ; steps 1 and 2
      (if (minusp (activate-process proc))            ; update energy balance
          nil                                         ; not ready to run
          (vector-push-extend proc *agenda-aux*)) ))  ; ready to run
  (sort *agenda-aux* #'>= :key #'process-energy)    ; step 3
  (dotimes (k (fill-pointer *agenda-aux*))          ; step 4
    (run-process (aref *agenda-aux* k)))
  (number-of-processes) )                           ; step 5

  ;; The mechanism for recruiting node-constructors (see DUAL/ARCHIT/NC_AGENT)
  ;; depends on the ordering of *AGENDA-AUX*.  Consider it carefully whenever
  ;; you alter RUN-ALL-PROCESSES, REMOVE-FROM-AGENDA-AUX, etc.


;;;;;;;;; Debugging scaffold
#|
#+:DUAL-DEBUG
(defmethod agent-activation ((agent symbolic-processor))
  ;; Note that this collides with the method from DUAL/ARCHIT/CONNECT.LSP
  (duali::prompt-and-read "~&  How much energy to supply to ~S ? "
                         (agent-name agent)) )
|#


;;;;;;;  End of file DUAL/ARCHIT/AGENDA.LSP
