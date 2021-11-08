;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/connect.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Connectionist aspect of DUAL agents (cf. ARCHIT/SYMBOLIC.LSP).
;;; DEPENDS-ON: DUAL/archit/basic.lsp, DUAL/archit/act_fun.lsp, =/conref.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    11-03-97 [1.0]
;;; UPDATED:    11-11-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;;;
;;; TO DO:      Define a generic fun MODIFY-LINK-WEIGHT (see DUAL/TO_DO.TXT).
;;; TO DO:      Provide for efficient weight learning by moving the code for
;;;             weight normalization from ADD-NEIGHBORS to ACTIVATE-NEIGHBORS.
;;;             This entails adding a slot to keep the summary weight, etc.
;;;             See DUAL/TO_DO.TXT for details. Also DUAL/ARCHIT/WORK_MEM.LSP.


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;        CONNECTIONIST ASPECT OF DUAL AGENTS          ;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is CONNECTIONIST-AGENT.
;;;; The file defines functions for constructing and dealing with such agents.
;;;;
;;;; The connectionist agent acts like a unit in a neural network (see sections
;;;; 3.2.4 and 3.4.2 in "DUAL Report #1").
;;;; Conn. agents inherit structure and functionality from 'base agents'.
;;;;
;;;; The file dual/INTRFACE/connect.lsp defines additional useful utilities
;;;; (e.g. FAN-IN) that do not belong to the conceptual specification of DUAL.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: connectionist-agent, make-conn-agent,
;;          agent-activation, act, agent-visible-p,
;;          agent-output,
;;          agent-neighbors, activate-neighbors,
;;          add-neighbors, remove-neighbors, normalize-neighbors,
;;          prepare-to-receive-activation, receive-activation,
;;          agent-new-activation, update-activation,
;;

;; MAKE-CONN-AGENT (name &key :comment :neighbors :prompt-p)  -->  new-agent
;;
;;   A function that constructs (and registers) a connectionist agent.
;;   NAME must be a symbol (but not  NIL).
;;   :PROMPT-P must be either T or NIL; if not supplied, it defaults to T.
;;   If NAME is already in use, MAKE-CONN-AGENT signals an error depending on
;;     the value of :PROMPT-P. If it is T (the default), a continuable error
;;     is signaled giving the user an opportunity to supply a new value
;;     for NAME. If :PROMPT-P is NIL, a non-continuable error is signaled.
;;   :COMMENT must be a string; if not supplied, it defaults to NIL.
;;   :NEIGHBORS must be a list of conn. references (see DUAL/ARCHIT/CONREF.LSP).
;;   If not supplied, :NEIGHBORS defaults to the empty list.
;;   MAKE-CONN-AGENT returns the new agent.
;;
;;   After a successful call to MAKE-BASE-AGENT, the following conditions hold:
;;     (find-agent name)            -->  new-agent
;;     (agentp new-agent)           -->  T
;;     (agent-name new-agent)       -->  name
;;     (agent-comment new-agent)    -->  comment  ; or NIL
;;     (agent-neighbors new-agent)  -->  conref-list
;;

;; AGENT-ACTIVATION (agent)  -->  activation-level
;; (SETF AGENT-ACTIVATION)
;;
;;   A generic function that accesses the activation level of AGENT.
;;   Returns a single float. May be used with SETF.
;;   Signals an error if AGENT is not a connectionist agent.
;;   ACT is a synonym for AGENT-ACTIVATION.
;;   Compare with AGENT-NEW-ACTIVATION and UPDATE-ACTIVATION.

;; ACT (agent)  -->  activation-level
;; (SETF ACT)
;;
;;   This function is a shorthand for AGENT-ACTIVATION.

;; AGENT-VISIBLE-P (agent)  -->  T or NIL
;;
;;   A function that returns T if AGENT is visible (see section 2.2.4.3 in
;;   DR#1) and NIL otherwise.
;;
;;   (agent-visible-p agent)  is equivalent to  (plusp (agent-activation agent))

;; AGENT-NEIGHBORS (agent)  -->  conref-list
;;
;;   A generic function that returns the list of AGENT's conn. neighbors.
;;   Returns a list of connectionist references (see DUAL/ARCHIT/CONREF.LSP).
;;   Cannot be used with SETF; use ADD-NEIGHBORS and REMOVE-NEIGHBORS instead.
;;   Signals an error if AGENT is not a connectionist agent.
;;
;;   The (non-generic) function DUAL-INTERFACE:FAN-OUT is a synonym of
;;   AGENT-NEIGHBORS. See also DUAL-INTERFACE:FAN-IN in INTRFACE/CONNECT.LSP.

;; AGENT-OUTPUT (agent)  -->  output-level
;;
;;   A generic function that returns the amount of activation (a single-float)
;;   that AGENT sends to its neighbors.
;;   Signals an error if AGENT is not a connectionist agent.
;;
;;   The output is a function of the current activation level of the agent.
;;   (See section ??? in "DUAL Report #1" -- 'output function'.)
;;   In the present implementation, the output function is IDENTITY. That is,
;;   (agent-output agent) is always equal to (agent-activation agent).
;;   DUAL programmers may define new kinds of agents and supply them with
;;   different output functions. CLIP-xxx and THRESHOLD-xxx defined in
;;   ARCHIT/ACT_FUN.LSP may be useful for this purpose.
;;


;; PREPARE-TO-RECEIVE-ACTIVATION (agent)  -->  NIL
;;
;;   A generic function that prepares AGENT to receive activation.
;;   This message should be sent to each agent at the beginning of each
;;   cycle, prior to any calls to RECEIVE-ACTIVATION (see below).
;;   The function always returns NIL and is used for its side effect only.
;;   Signals an error if AGENT is not a connectionist agent.

;; RECEIVE-ACTIVATION (receiver amount)  -->  received-amount
;;
;;   A generic function that causes RECEIVER to receive AMOUNT of activation.
;;   RECEIVER should be a connectionist agent (if not, an error is signaled).
;;   AMOUNT should be a single-float. The function returns the amount received.
;;
;;   The AMOUNT is added (algebraically) to the accumulated activation received
;;   since the last call to PREPARE-TO-RECEIVE-ACTIVATION.

;; ACTIVATE-NEIGHBORS (sender)  -->  NIL
;;
;;   A generic function that causes SENDER to send some connectionist output
;;   to all its neighbors in accordance with the weights of the links.
;;   SENDER should be a connectionist agent (if not, an error is signaled).
;;   The function always returns NIL and is used for its side effect only.
;;
;;   The function sends RECEIVE-ACTIVATION messages to all agents in the
;;   AGENT-NEIGHBORS list of the SENDER. The amount sent to each neighbor
;;   is (AGENT-OUTPUT sender) multiplied by the corresponding weight.
;;


;; AGENT-NEW-ACTIVATION (agent net-input &rest more-inputs)  -->
;;                                                  -->  new-activation-level
;;
;;   A generic function that computes the 'activation function' of AGENT
;;   (see subsection 3.2.4.1. in "DUAL Report #1").
;;   Returns the new activation level (a single float) without updating
;;   AGENT's current activation level. (See UPDATE-ACTIVATION).
;;   Signals an error if AGENT is not a connectionist agent.
;;
;;   The new activation is computed on the basis of the current activation
;;   level (given by AGENT-ACTIVATION) and NET-INPUT.
;;   In the present implementation, the activation function is
;;   DUAL-ACT-FUN defined in DUAL/ARCHIT/ACT_FUN.LSP.
;;   DUAL programmers may define new kinds of agents and supply them with
;;   different activation functions. Predefined functions xxx-ACT-FUN
;;   from DUAL/ARCHIT/ACT_FUN.LSP may be useful for this purpose.
;;
;;   The &REST parameter is provided for functions (e.g. GROSSBERG-ACT-FUN)
;;   that distinguish two or more kinds of inputs. All methods defined in
;;   this file ignore this parameter.

;; UPDATE-ACTIVATION (agent)  -->  new-activation-level
;;
;;   A generic function that updates the activation level of AGENT.
;;   Returns the new activation level (a single float).
;;   Signals an error if AGENT is not a connectionist agent.
;;
;;   (UPDATE-ACTIVATION agent) is roughly equivalent to:
;;          (setf (agent-activation agent)
;;                (agent-new-activation agent accumulated-input))
;;   where ACCUMULATED-INPUT is the amount accumulated in the connectionist
;;   input zone of the agent through calls to RECEIVE-ACTIVATION.
;;

;;;;  A typical connectionist cycle may be implemented as follows:
;;;;    (progn (do-all-agents (ag) (prepare-to-receive-activation ag))
;;;;           <activate some agents from external sources>
;;;;           (do-all-agents (ag) (activate-neighbors ag))
;;;;           (do-all-agents (ag) (update-activation ag)) )
;;;;
;;;;  See also the function SPREAD defined in DUAL/ARCHIT/WORK_MEM.LSP.


;; NORMALIZE-NEIGHBORS (agent &key control destructive-p)  -->  normalized-list
;;
;;   A generic function that normalizes the weights of AGENT's outgoing links.
;;   AGENT should be a connectionist agent (if not, an error is signaled).
;;   :CONTROL should be T, NIL, or a positive single float. It defaults to T.
;;   :DESTRUCTIVE-P should be T or NIL; it defaults to NIL.  It is passed
;;     over to NORMALIZE-WEIGHTS (defined in DUAL/ARCHIT/CONREF.LSP).
;;   The function returns the new list of neighbors. It contains references
;;   to the same agents but the weights of the link are changed.
;;
;;   The normalized neighbor-list replaces the old AGENT-NEIGHBORS list.
;;
;;   The real work is done by the function NORMALIZE-WEIGHTS depending on
;;   the value of the  CONTROL parameter:
;;     -- NIL -- no normalization
;;     -- T   -- normalization using default SUM in NORMALIZE-WEIGHTS
;;     -- sum -- normalization using (normalize-weights ... sum)
;;
;;   See also the funciton NORMALIZE-WM-WEIGHTS defined in ARCHIT/WORK_MEM.LSP.


;; ADD-NEIGHBORS (agent conref-list
;;                &key priority order destructive-p normalize)  -->
;;                                                         -->  augmented-list
;;
;;   A generic function that augments the AGENT-NEIGHBORS list of AGENT with
;;   new connectionist references.
;;   AGENT should be a connectionist agent (if not, an error is signaled).
;;   CONREF-LIST should be a list of conrefs (see DUAL/ARCHIT/CONREF.LSP).
;;   :PRIORITY, :ORDER, and :DESTRUCTIVE-P have the same interpretation and
;;     default values as in ADJOIN-CONREF (defined in DUAL/ARCHIT/CONREF.LSP).
;;   :NORMALIZE should be suitable for the 2nd parameter of NORMALIZE-NEIGHBORS.
;;   If not supplied, :NORMALIZE defaults to NIL.
;;   The function returns the new list of neighbors (a list of conrefs).
;;
;;   ADD-NEIGHBORS adjoins each member of CONREF-LIST to the original neighbor
;;   list of AGENT using ADJOIN-CONREF.
;;
;;   The augmented neighbor-list replaces the old AGENT-NEIGHBORS list of AGENT.
;;
;;   Finally, NORMALIZE-NEIGHBORS is called and ADD-NEIGHBORS returns the
;;   normalized conref list produced in this way. In other words, the form:
;;      (add-neighbors agent conref-list
;;                     :priority p :normalize norm :destructive-p destr)
;;   is equivalent to the sequence:
;;     (progn
;;       (add-neighbors agent conref-list :priority p :destructive-p destr)
;;       (normalize-neighbors agent :control norm :destructive-p destr))


;; REMOVE-NEIGHBORS (agent non-grata &key :normalize)  -->  reduced-list
;;
;;   A generic function that _destructively_ removes neighbors from the
;;   AGENT-NEIGHBORS list of AGENT.
;;   AGENT should be a connectionist agent (if not, an error is signaled).
;;   NON-GRATA should be a list of connectionist agents or the keyword :ALL.
;;   :NORMALIZE should be suitable for the :CONTROL parameter of NORMALIZE-
;;     NEIGHBORS.  If not supplied, :NORMALIZE defaults to NIL.
;;   The function returns the new list of neighbors (a list of conrefs).
;;
;;   REMOVE-NEIGHBORS removes all members of the list NON-GRATA from the
;;   neighbor list of AGENT.
;;   If NON-GRATA is bound to the keyword :ALL, all neighbors are removed.
;;
;;   The reduced neighbor-list replaces the old AGENT-NEIGHBORS list of AGENT.
;;
;;   Finally, the form (NORMALIZE-NEIGHBORS AGENT :CONTROL NORMALIZE) is
;;   evaluated and REMOVE-NEIGHBORS returns the normalized conref list produced
;;   in this way. In other words, the form:
;;      (remove-neighbors agent non-grata :normalize norm)
;;   is equivalent to the form:
;;      (progn
;;        (remove-neighbors agent non-grata)
;;        (normalize-neighbors agent :control norm :destructive-p T))
;;
;;   It is always true that:
;;      (remove-neighbors agent :all)                   -->  NIL
;;      (progn  (remove-neighbors agent :all)
;;              (agent-neighbors agent) )               -->  NIL
;;      (progn  (remove-neighbors ag1 (list ag2))
;;              (find ag2 (agent-neighbors ag1)
;;                        :key #'conref-reference) )    -->  NIL
;;

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric  agent-output (agent)
  (:documentation  "Connectionist output (based on the current act. level)." ))

(defgeneric  agent-new-activation (agent net-input &rest more-inputs)
  (:documentation
    "Computes the new activation level. Does not update AGENT-ACTIVATION." ))

(defgeneric  prepare-to-receive-activation (agent)
  (:documentation  "Prepares the agent to receive activation." ))

(defgeneric  receive-activation (receiver  amount)
  (:documentation "Receives activation in the connectionist input zone." ))

(defgeneric  activate-neighbors  (sender)
  (:documentation  "Sends activation to neighbors." ))

(defgeneric  update-activation  (agent)
  (:documentation  "Updates and returns the activation level of AGENT." ))

(defgeneric  normalize-neighbors (agent &key control destructive-p)
  (:documentation  "Normalize the weights of the outgoing links." ))

(defgeneric  add-neighbors (agent conref-list
                            &key priority order destructive-p normalize)
  (:documentation  "Augments the AGENT-NEIGHBORS list with new conrefs." ))

(defgeneric  remove-neighbors (agent non-grata &key normalize)
  (:documentation  "Removes neighbors from the AGENT-NEIGHBORS list." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass connectionist-agent  (base-agent)
    ((activation    :accessor    agent-activation
                    :type        single-float
                    :initform    0.0  )
     (conn-input    :accessor    conn-input-zone
                    ;; don't confuse with AGENT-INPUT-ZONE (see ARCHIT/SYMPROC1)
                    :type        single-float
                    :initform    0.0  )
     (neighbors     :accessor    conn-neighbors      ; for internal use
                    :reader      agent-neighbors     ; for the external protocol
                    :type        list                ; list of CONREFs
                    :initform    nil )
    )
    (:documentation "Connectionist aspect of DUAL agents; a mixin class.") )
) ; eval-when

;;;;  Constructor

(defun make-conn-agent (name &key comment neighbors prompt-p)
  "Makes a connectionist agent and registers it into the total pool of agents."
  (declare (values connectionist-agent)
           (type (or null string) comment)
           (type list neighbors) )
  (let ((new-agent (allocate-agent name 'connectionist-agent prompt-p)))
    (when  comment                          ; COMMENT supplied?
      (setf (agent-comment new-agent) comment))
    (when  neighbors                        ; NEIGHBORS supplied?
      (setf (conn-neighbors new-agent) neighbors))
    new-agent))


;;;;  Type-checking methods for the accessors

(defmethod  agent-activation ((x t))
  (if x
    (error "AGENT-ACTIVATION:  ~S is not a connectionist agent." x)
    (error "AGENT-ACTIVATION applied to NIL (perhaps #$missing-agent).") ))

(defmethod  (setf agent-activation) (new-value (x t))
  (declare (ignore new-value))
  (if x
   (error "(SETF AGENT-ACTIVATION):  ~S is not a connectionist agent." x)
   (error "(SETF AGENT-ACTIVATION) applied to NIL (perhaps #$missing-agent).")))

(defmethod  conn-input-zone ((x t))
  (if x
     (error
      "DUAL-CORE::CONN-INPUT-ZONE:  ~S is not a connectionist agent." x)
     (error
      "DUAL-CORE::CONN-INPUT-ZONE applied to NIL (perhaps #$missing-agent).")))

(defmethod  (setf conn-input-zone) (new-value (x t))
  (declare (ignore new-value))
  (if x
   (error
    "(SETF DUAL-CORE::CONN-INPUT-ZONE):  ~S is not a connectionist agent." x)
   (error
    "(SETF DUAL-CORE::CONN-INPUT-ZONE) applied to NIL (perhaps #$missing-agent).") ))

(defmethod  conn-neighbors ((x t))
  (if x
    (error
      "DUAL-CORE::CONN-NEIGHBORS:  ~S is not a connectionist agent." x)
    (error
      "DUAL-CORE::CONN-NEIGHBORS applied to NIL (perhaps #$missing-agent).") ))

(defmethod  (setf conn-neighbors) (new-value (x t))
  (declare (ignore new-value))
  (if x
   (error
    "(SETF DUAL-CORE::CONN-NEIGHBORS):  ~S is not a connectionist agent." x)
   (error
    "(SETF DUAL-CORE::CONN-NEIGHBORS) applied to NIL (perhaps #$missing-agent).") ))

(defmethod  agent-neighbors ((x t))
  (if x
    (error "AGENT-NEIGHBORS:  ~S is not a connectionist agent." x)
    (error "AGENT-NEIGHBORS applied to NIL (perhaps #$missing-agent).") ))

(defmethod  (setf agent-neighbors) (new-value (x t))
  (declare (ignore new-value))
  (error "AGENT-NEIGHBORS cannot be used with SETF.") )


;;;;   Shorthand functions:

;; ACT is a synonym for AGENT-ACTIVATION
(declaim  (inline act |(setf-act)| ))
(defun act (agent)
  (agent-activation agent))
(defun |(setf act)| (agent x)
  (setf (agent-activation agent) x) )
(defsetf act |(setf act)| )

;; AGENT-VISIBLE-P is a synonym for  (PLUSP (AGENT-ACTIVATION ...))
(declaim (inline agent-visible-p))
(defun agent-visible-p (agent)
  (plusp (agent-activation agent)))

;;;;;;  Printing methods

(defmethod  agent-descriptor-string ((agent connectionist-agent))
  (if (eq (type-of agent) 'connectionist-agent)
      "a connectionist agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod DUAL-describe :after ((ag connectionist-agent)
                                 &optional (stream *standard-output*))
  (format stream "~&  Its current activation level is~:[ unbound ~;: ~6,3F~]~%"
                 (slot-boundp ag 'activation) (agent-activation ag) )
  (format stream "~&  Its output is: ~6,3F~%"
              ; no error checking (I've refrained from using CAUSES-AN-ERROR-P)
                 (agent-output ag) )
  (format stream "~&  Its neighbors are: ~S~%"
                 (if (slot-boundp ag 'neighbors)
                     (agent-neighbors ag)
                     "(unbound slot)") )
  (format stream "~&  Its net input is: ~:[(unbound slot)~;~:*~6,3F~]~%"
                 (if (slot-boundp ag 'conn-input)
                     (conn-input-zone ag)
                     nil) )
  (values))


;;;;;;  Connectionist workings of individual agents

(defmethod  agent-output ((ag connectionist-agent))
  (declare (values single-float))
  (agent-activation ag) )                   ; Identity output function

(defmethod  agent-output ((x t))
  (if x
    (error "AGENT-OUTPUT:  ~S is not a connectionist agent." x)
    (error "AGENT-OUTPUT applied to NIL (perhaps #$missing-agent).") ))


(defmethod  agent-new-activation ((ag connectionist-agent)
                                  (net-input float)
                                  &rest more-inputs )
  (declare (inline DUAL-act-fun)
           (optimize (speed 3))
           (type single-float net-input)
           (ignore more-inputs)
           (values single-float) )
  ;; Default act.f. is (optimized) DUAL-ACT-FUN, defined in ARCHIT/ACT_FUN.LSP.
  (DUAL-act-fun (agent-activation ag) net-input) )

(defmethod  agent-new-activation ((ag connectionist-agent)
                                  (net-input number)
                                  &rest more-inputs )
  (apply #'agent-new-activation ag (coerce net-input 'float) more-inputs) )

(defmethod  agent-new-activation ((x t) (y t) &rest z)
  (declare (ignore z))
  (if x
    (error "AGENT-NEW-ACTIVATION:  ~S is not a conn. agent or ~S is not a number."
           x y)
    (error "AGENT-NEW-ACTIVATION applied to NIL (perhaps #$missing-agent).") ))


(defmethod  prepare-to-receive-activation  ((ag connectionist-agent))
  (setf (conn-input-zone ag) 0.0)
  nil )

(defmethod  prepare-to-receive-activation ((x t))
  (if x
    (error "PREPARE-TO-RECEIVE-ACTIVATION:  ~S is not a connectionist agent." x)
    (error "PREPARE-TO-RECEIVE-ACTIVATION applied to NIL (perhaps #$missing-agent).") ))


(defmethod  receive-activation  ((ag connectionist-agent)
                                 (amount float))
  (declare (type single-float amount))
  (incf (conn-input-zone ag) amount)
  amount)

(defmethod  receive-activation  ((ag connectionist-agent)
                                 (amount number))
  (receive-activation ag (coerce amount 'float)))

(defmethod  receive-activation  ((x t) (y t))
  (if x
    (error "RECEIVE-ACTIVATION: ~
            ~S is not a connectionist agent or ~S is not a number."
           x y)
    (error "RECEIVE-ACTIVATION applied to NIL (perhaps #$missing-agent).") ))


(defmethod  update-activation  ((ag connectionist-agent))
  (setf  (agent-activation ag)
         (agent-new-activation ag (conn-input-zone ag)) ))

(defmethod  update-activation  ((x t))
  (if x
    (error "UPDATE-ACTIVATION:  ~S is not a connectionist agent." x)
    (error "UPDATE-ACTIVATION applied to NIL (perhaps #$missing-agent).") ))


(defmethod  activate-neighbors ((sender connectionist-agent))
  (declare (values null))
  (let ((my-output  (agent-output sender)))
    (dolist  (nbr (conn-neighbors sender) nil)
      (receive-activation (conref-reference nbr)
                          (* my-output
                             (the single-float (conref-weight nbr))))) ))

(defmethod  activate-neighbors ((x t))
  (if x
    (error "ACTIVATE-NEIGHBORS:  ~S is not a connectionist agent." x)
    (error "ACTIVATE-NEIGHBORS applied to NIL (perhaps #$missing-agent).") ))


;;;;;;  Methods dealing with neighbors and conref-lists

(defmethod  normalize-neighbors ((agent connectionist-agent)
                                 &key (control t) (destructive-p nil) )
  (declare (type (or boolean single-float) control)
           (values list))
  (cond ((null control) (conn-neighbors agent))  ; NIL means 'no normalization'
        ((eq t control)
           (setf (conn-neighbors agent)
                 (normalize-weights (conn-neighbors agent)
                                    :destructive-p destructive-p)))
        (t (setf (conn-neighbors agent)
                 (normalize-weights (conn-neighbors agent)
                       :sum control :destructive-p destructive-p))) ))

(defmethod  normalize-neighbors ((x t) &key control destructive-p )
  (declare (ignore control destructive-p))
  (if x
    (error "NORMALIZE-NEIGHBORS:  ~S is not a connectionist agent." x)
    (error "NORMALIZE-NEIGHBORS applied to NIL (perhaps #$missing-agent).") ))


(defmethod  add-neighbors ((agent connectionist-agent)
                           conref-list
                           &key (priority :new)
                                (normalize nil)
                                (order    :fifo)
                                (destructive-p nil) )
  (declare (type list conref-list)
           (type (or (member :new :old) function)  priority)
           (type (or boolean single-float) normalize)
           (type (member :fifo :lifo) order)
           (values list) )
  (let ((draft (if destructive-p
                   (conn-neighbors agent)                  ; use original
                   (mapcar #'copy-conref                   ; use a copy
                           (conn-neighbors agent)))  ))
    (dolist (cr conref-list)
      (setq draft (adjoin-conref cr draft :priority priority :order order
                                 :destructive-p T)) )  ; always destructive
    (setf (conn-neighbors agent) draft)
    (normalize-neighbors agent :control normalize :destructive-p T) ))

(defmethod add-neighbors  ((x t) y &key priority normalize order destructive-p)
  (declare (ignore y priority normalize order destructive-p))
  (if x
    (error "ADD-NEIGHBORS:  ~S is not a connectionist agent." x)
    (error "ADD-NEIGHBORS applied to NIL (perhaps #$missing-agent).") ))


(defmethod  remove-neighbors ((agent connectionist-agent)
                              non-grata
                              &key (normalize nil) )
  (declare (type list non-grata)
           (type (or boolean single-float) normalize)
           (values list) )
  (let ((reduced-list (conn-neighbors agent)))
    (dolist (exile non-grata)
      (setq reduced-list (delete exile reduced-list :key #'conref-reference)) )
    (setf (conn-neighbors agent) reduced-list)
    (normalize-neighbors agent :control normalize :destructive-p T) ))

;; (remove-neighbors agent :all)  removes all neighbors.
(defmethod  remove-neighbors ((agent connectionist-agent)
                              (non-grata (eql :all))
                              &key normalize )
  (declare (ignore normalize))
  (setf (conn-neighbors agent) nil))

(defmethod remove-neighbors  ((x t) y &key normalize)
  (declare (ignore y normalize))
  (if x
    (error "REMOVE-NEIGHBORS:  ~S is not a connectionist agent." x)
    (error "REMOVE-NEIGHBORS applied to NIL (perhaps #$missing-agent).") ))


;;;;;;;  End of file DUAL/ARCHIT/CONNECT.LSP
