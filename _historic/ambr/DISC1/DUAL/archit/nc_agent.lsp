;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/NC_agent.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Node-constructor agents
;;; DEPENDS-ON: archit/DUAL_ag.lsp, archit/temp_ag.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    03-06-97
;;; UPDATED:    11-05-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...
;;;;;;;
;;; TO DO:      Document everything.
;;; TO DO:      Elaborate the comments in NC-requests.
;;;


       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;;     N O D E   C O N S T R U C T O R S     ;;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is NODE-CONSTRUCTOR -- a special
;;;; DUAL agent capable of constructing agents (or nodes).
;;;;
;;;; ...


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: node-constructor,
;;          *node-constructors*, make-node-constructors,
;;          free-p, recruit-node-constructor,
;;          ; handle-symbol, :just-created
;;
;;          node-construction-request, make-NC-request,
;;          NCR-mentor, NCR-agent-type, NCR-genname, NCR-slots, NCR-mail
;;
;;          NCR-sending-agent, agent-NCR-queue, send-NC-request
;;

;; ...


;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric free-p (centralized-resource)
  (:documentation "Returns T iff CENTRALIZED-RESOURCE is available." ))
  ;; Other files may define other centralized resources and methods for FREE-P.
  ;;
  ;; FREE-P has been defined generic in order to allow for unified treatment of
  ;; all centralized resources in the architecture.  Right now [ver. 1.1.2]
  ;; node constructors are the only such resource and hence there is only one
  ;; method for FREE-P.

(defgeneric send-NC-request (sender NC-request)
  (:documentation "Puts NC-REQUEST to the NCR-queue of SENDER." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;  **********    NODE-CONSTRUCTION REQUESTS   **********
;;;
;;;  This portion of the file defines the class NODE-CONSTRUCTION-REQUEST.
;;;  Node-construction requests (or NC-requests for short) are symbolic
;;;  structures passed to node constructors and describing the agent
;;;  to be created.
;;;

;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass  node-construction-request  (symbolic-structure)
    ((mentor     :reader    NCR-mentor
                 :type      cons      ; (satisfies conref-p)
                 :initarg   :mentor
                 :initform  (required-argument)  )
     (agent-type :reader    NCR-agent-type
                 :type      symbol    ; (satisfies valid-DUAL-agent-type-p)
                 :initarg   :agent-type
                 :initform  (required-argument) )
     (genname    :reader    NCR-genname
                 :type      cons      ; arguments for GENNAME (see ARCHIT/BASIC)
                 :initarg   :genname
                 :initform  (required-argument) )
     (slots      :reader    NCR-slots
                 :type      list      ; of slot-filler pairs
                 :initarg   :slots
                 :initform  (required-argument)
                 :documentation "The slots and facets of the new agent." )
     (mail       :reader    NCR-mail
                 :type      list      ; of symbolic structures
                 :initarg   :mail
                 :initform  (required-argument)
                 :documentation "Symbols to be sent to the new agent." )
    )
    (:documentation "Order for (and specification of) a new DUAL agent." ))
) ; eval-when


;;;;  Constructor

(defun  make-NC-request (mentor agent-type genname slots mail)
  "Constructor for node-construction requests."
  (declare (values node-construction-request)
           (type list slots mail) )
  (if (and (conref-p mentor)
           (valid-DUAL-agent-type-p agent-type)
           (subtypep agent-type 'temp-DUAL-agent)
           (consp genname)  (stringp (first genname))  (> (length genname) 2) )
      (make-instance 'node-construction-request  :mentor mentor
                                                 :agent-type agent-type
                                                 :genname genname
                                                 :slots slots
                                                 :mail mail)
      (error "MAKE-NC-REQUEST: Illegal argument(s):  ~S  ~S  ~S."
              mentor agent-type genname) ))


;;;;;;  Printing methods

(defmethod  print-object ((NC-request node-construction-request) stream)
  (if (and (slot-boundp NC-request 'mentor)
           (conref-p (NCR-mentor NC-request))  ;e.g.((#$mentor . :t-link) . 0.5)
           (slot-boundp NC-request 'agent-type)
           (slot-boundp NC-request 'genname)
           (slot-boundp NC-request 'slots)
           (slot-boundp NC-request 'mail) )
      (format stream "#<NCR ~A>"               ;e.g. #<NCR mentor>
        (agent-name (symref-agent (conref-reference (NCR-mentor NC-request)))))
      (format stream "#<malformed NC-request>") ))


(defmethod DUAL-describe  ((NC-request node-construction-request)
                           &optional (stream *standard-output*))
  (format stream "~&~S is a ~A.~%"
                 NC-request  (type-of NC-request) )
  (format stream "~&  The mentor of the new node is : ~S~%"
          (if (slot-boundp NC-request 'mentor)
              (NCR-mentor NC-request)
              "(unbound)"))
  (format stream "~&  The new node will be of type: ~S~%"
          (if (slot-boundp NC-request 'agent-type)
              (NCR-agent-type NC-request)
              "(unbound)"))
  (format stream "~&  The GENNAME template is: ~S~%"
          (if (slot-boundp NC-request 'genname)
              (NCR-genname NC-request)
              "(unbound)"))
  (format stream "~&  The slots of the new node will be: ~S~%"
          (if (slot-boundp NC-request 'slots)
              (NCR-slots NC-request)
              "(unbound)"))
  (format stream "~&  The mail for the new node is: ~S~%"
          (if (slot-boundp NC-request 'mail)
              (NCR-mail NC-request)
              "(unbound)"))
  (values))


;;;;  Type-checking methods for the readers

(defmethod  NCR-mentor ((x t))
  (error "NCR-MENTOR: ~S is not a node-construction request." x) )

(defmethod  NCR-agent-type ((x t))
  (error "NCR-AGENT-TYPE: ~S is not a node-construction request." x) )

(defmethod  NCR-genname ((x t))
  (error "NCR-GENNAME: ~S is not a node-construction request." x) )

(defmethod  NCR-slots ((x t))
  (error "NCR-SLOTS: ~S is not a node-construction request." x) )

(defmethod  NCR-mail ((x t))
  (error "NCR-MAIL: ~S is not a node-construction request." x) )



;;;;;;  ***********   N O D E   C O N S T R U C T O R S   *************
;;;
;;;  This portion of the file defines the class NODE-CONSTRUCTOR.
;;;  Node constructors are special DUAL agents that are capable of creating
;;;  new (temporary) agents.
;;;


;;;;;;   Class definition

(eval-when (compile load eval)
  (defclass node-constructor (special-DUAL-agent)
    ()  ; no slots defined here, all slots are inherited
    (:documentation "Special DUAL agent capable of creating new agents." ))
) ; eval-when

;;;;;;  Printing methods

(defmethod  agent-descriptor-string ((agent node-constructor))
  (if (eq (type-of agent) 'node-constructor)
      "a node constructor"
      (format nil "an agent of type ~S" (type-of agent)) ))


;;;;;;  Maintain a central registry of node constructors

(defvar *node-constructors* nil
  "A list of all node constructors in the system." )

(defun make-node-constructors  (number &key (package *package*)
                                            (mode :append) )
  "Creates a few node constructors and registers them in *NODE-CONSTRUCTORS*."
  (declare (values list)
           (type (integer 0 *) number)
           (type package package)
           (type (member :old :new :append :at-least) mode) )
  (ecase  mode
    (:old      (when (endp *node-constructors*)
                 (setq *node-constructors*
                       (make-node-constructors-aux number package))) )
    (:new      (setq *node-constructors*
                     (make-node-constructors-aux number package)) )
    (:append   (setq *node-constructors*
                     (append *node-constructors*
                             (make-node-constructors-aux number package))) )
    (:at-least (let ((old-length (length *node-constructors*)))
                 (when (> number old-length)
                   (setq *node-constructors*
                         (append *node-constructors*
                             (make-node-constructors-aux (- number old-length)
                                                         package))))) ))
  *node-constructors* )

(defun make-node-constructors-aux  (number package)
  (let ((*package* package)
        (result '()) )
    (dotimes (k number (nreverse result))
      (push (make-DUAL-agent (genname "*~A~D*" "NC" 1) 'node-constructor)
            result)) ))


;;;;;;  Recruiting node constructors
;;
;;  NCR-sending agents (see next section) constantly try to 'recruit' a free
;;  node constructor and to send it a NC request.  In this organization, node
;;  constructors act as servers and NCR-sending agents as clients. The protocol
;;  for serving NC requests must meet the following requirements:
;;    1. Clients have the initiative -- they contact the server, not vice versa.
;;    2. A server may serve only one client at a time.
;;    3. Spooling is not allowed.  (Otherwise a node constructor may waste time
;;       on requests whose sender has died in the interim.)
;;    4. A server may be idle only when there aren't any waiting clients.
;;    5. When several clients compete for the same server, the most active
;;       client has priority.
;;
;;  It proved to be very tricky business to satisfy these requirements without
;;  causing too much overhead (i.e. re-sorting the agenda in the inner loop).
;;  This area gave rise to quite a few subtle bugs. Beware!
;;
;;  Requirement 1. is met by the usual mechanism of DUAL interactions --
;;  clients send symbolic structures (NC requests in this case) to the server.
;;
;;  Requirement 3. is met by making NC requests wait in the clients rather
;;  than the servers.  (Note that this deviates from the usual interaction
;;  protocol in which the symbolic structures are kept in the input zone of
;;  the recipient.)  There is a (mix-in) class of agents --  NCR-SENDING
;;  agents -- which have a special queue for node construction requests.
;;
;;  Requirement 2. follows from the restriction that only 'free' node
;;  constructors may be sent NC requests. The client must first 'recruit' the
;;  server and only then engage it with its request. A lock is set to prevent
;;  other clients from recruiting a busy server.  See functions FREE-P and
;;  RECRUIT-NODE-CONSTRUCTOR below.
;;
;;  Requirement 4. is met by making NCR-sending agents persistently call the
;;  function RECRUIT-NODE-CONSTRUCTOR.  NCR-sending agents have 'elaborate
;;  symbolic microcycle' (see below) and stay on *AGENDA* until they send
;;  all requests from their NCR queue.  Typically, the agenda contains several
;;  (sometimes dozens) suspended processes engaged solely in recruiting node
;;  constructors.  Thus we come to Requirement 5.
;;
;;  Requirement 5. is the trickiest of all. Previous versions (up to 1.1.1)
;;  relied on the ordering established by RUN-ALL-PROCESSES (see DUAL/ARCHIT/
;;  AGENDA.LSP).  This, however, is not enough as the following counter-example
;;  demonstrates:
;;    Suppose that during some particular invocation of RUN-ALL-PROCESSES the
;;    'active subset' of the agenda contains two clients (A and B) and a server
;;    S.  Suppose that S is almost ready to complete a previous request.
;;    Finally, suppose A is more active than B and hence should have priority.
;;    It may happen that the suspended processes are ordered on the agenda
;;    according to their enery levels like this: ... A ... S ... B ...
;;    Now, A runs, attempts to recruit a node constructor, and fails.  S runs
;;    next, completes its previous request, and becomes available.  This allows
;;    B, which runs last, to recruit it.  Thus, B is served before A in spite
;;    of its lower priority.
;;    Simulation experiments with the AMBR model showed that this anomaly
;;    led to virtually random recruitment order -- active clients hardly had
;;    any priority over less active ones.  Worse yet, the most active clients
;;    sometimes had disadvantage as they stood near the top of the agenda.
;;
;;  These problems arise from the asynchronous recruitment attempts.  The
;;  solution adopted here is to impose a limited synchronization -- the node
;;  constructors may become free only at the beginning of a symbolic macrocycle
;;  (a call to RUN-ALL-AGENTS).  In terms of the example above, this means that
;;  the order will be  .. S .. A .. B ..  which guarantees that A will have
;;  priority.  This is achieved by altering the symbolic microcycle (see DUAL/
;;  ARCHIT/SYMPROC2.LSP) of the node constructors in such a way that they
;;  do not leave the agenda when they complete a request but wait until the
;;  beginning of the next macrocycle.

(defmethod free-p  ((NC node-constructor))
  (and (zerop (agent-flag NC *recruited-flag*)) ; the lock is not set
       (not (active-processor-p NC))            ; the processor is idle
       (endp (agent-input-zone NC)) ))          ; there are no pending requests

(defun recruit-node-constructor ()
  "Return a free node constructor or NIL if all are busy."
  (declare (values (or null node-constructor)))
  (let ((NC (find-if #'free-p *node-constructors*)))
    (unless (null NC) 
      (setf (agent-flag NC *recruited-flag*) 1))  ; set a lock
    NC ))

(defmethod receive-symbol :after ((NC node-constructor)
                                  (symbol symbolic-structure) )
  (declare (ignore symbol))
  (setf (agent-flag NC *recruited-flag*) 0) )  ; release the lock; the non-empty
                                               ; input zone now does its job
  ;; *RECRUITED-FLAG* is defined in DUAL/ARCHIT/BASIC.LSP
  ;; It belongs to the domain of the implementation and is not advertized.


(defmethod  symbolic-microcycle ((host node-constructor))
  (declare (values S-PROGN-return-type))
  (s-progn host
     (cond ((endp (agent-input-zone host))
               (busy-wait host)     ; skip this macrocycle (see AGENDA.LSP)
               (s-eval 0.001 nil))  ; stay on agenda until the next macrocycle
           (t  ;; Do the ordinary microcycle -- see DUAL/ARCHIT/SYMPROC2.LSP
               [handle-next-input host]           ; pop a symbol and handle it
               [symbolic-microcycle host]) )))    ; loop
  ;; Compare with the 'elaborate microcycle' of NCR-sending agents below.


;;;;;; Inner workings of node constructors
;;
;; The work of a node constructor while handling a NC-request is broken down
;; into the following phases:
;;   0. Dechipher the mentor slot of the NC-request to retrieve MENTOR,
;;      MENTOR-LINK, and MENTOR-WEIGHT.
;;   1. Make sure that MENTOR is visible.
;;   2a. Create the new agent (call it CHILD). CHILD's initial activation is
;;       equal to MENTOR's current activation.
;;   2b. Setf (agent-comment CHILD)  to a string mentioning MENTOR.
;;       (This step, like all comments in general, is irrelevant from
;;        'conceptual' point of view.)
;;   2c. Create link from MENTOR to CHILD with label MENTOR-LINK and weight
;;       MENTOR-WEIGHT. To do this, the node constructor is licensed to
;;       alter MENTOR's micro-frame.
;;   3. Create and fill-in CHILD's slots.
;;   4. Trigger the symbolic processor of CHILD (with a :JUST-CREATED message).
;;   5. Send mail (i.e. symbolic structures) to CHILD.
;;
;; Steps 1+2a+2c must be executed as one 'atomic' operation. This is achieved
;; by the (non-suspendable) function CONSTRUCT-NEW-AGENT.  The consumption of
;; this combined symbolic step is proclaimed in the file DUAL/CONSUM.LSP.
;; Step 2c establishes a 'umbilical cord' from MENTOR to the newborn CHILD.
;; Without the umbilical cord the child may die at birth due to lack of energy.

(defun construct-new-agent (NC-request)   ; phases 0+1+2
  "Used by method HANDLE-SYMBOL (node-constructor node-construction-request)."
  (declare (type node-construction-request NC-request)
           (values (or null DUAL-agent)) )
  ;; No error-checks, rely on MAKE-NC-REQUEST.
  (let* ((mentor-conref (NCR-mentor NC-request))
         (raw-symref (conref-reference mentor-conref))      ; phase 0
         (mentor (symref-agent raw-symref))
         (mentor-link (symref-slot raw-symref))  ; usually :T-LINK
         (mentor-weight (conref-weight mentor-conref))
         (child-comment (format nil "Created by ~A at time ~,2F."
                                    (agent-name mentor) *time*)) )
    (if (agent-visible-p mentor)                            ; phase 1
        (let* ((mentor-act (agent-activation mentor))
               (new-name (apply #'genname (NCR-genname NC-request)))
               (agent-type (NCR-agent-type NC-request))
               (child (make-DUAL-agent  new-name            ; phase 2
                            agent-type  :prompt-p nil
                            :initial-act mentor-act           ; phase 2a
                            :comment child-comment)) )        ; phase 2b
          (add-link mentor mentor-link child mentor-weight)   ; phase 2c
          child )
        nil )))

(eval-when (compile load eval)
  (proclaim-consumption 'create-slots :explicit-S-PROGN )
  (proclaim-consumption 'finalize     :explicit-S-PROGN )
)

(defmethod handle-symbol ((node-constructor node-constructor)
                          (NC-request node-construction-request) )
  (declare (values S-PROGN-return-type))
  (let (child )  ; to be set at phase 2 and used throughout
   (s-progn node-constructor
     (flet ((create-slots ()
              (dolist (slot-description (NCR-slots NC-request))  ; phase 3
                (ecase (length slot-description)
                  (2  [add-G-slot child
                                  (first slot-description)       ; G-label
                                  :filler (second slot-description)])
                  (3  [add-facet  child
                                  (first slot-description)       ; S-label
                                  (second slot-description)      ; facet label
                                  :filler (third slot-description)]) )))
            (finalize ()
              [trigger-symbolic-processor child :just-created]   ; phase 4
              (dolist (symbol (NCR-mail NC-request))             ; phase 5
                [receive-symbol child symbol])) )
       ;; Main body of HANDLE-SYMBOL -- putting all phases together.
       (setq child [construct-new-agent NC-request])  ; phases 0+1+2
       (unless (null child)
         [create-slots]                               ; phase 3
         [finalize] )                                 ; phases 4+5
     ))))


(defmethod  trigger-symbolic-processor ((agent temp-DUAL-agent)
                                        (event (eql :just-created)) )
  #+:DUAL-DEBUG
    (format t "~&;; ~S ignores a :JUST-CREATED event." agent)
  nil )



;;;;;;  *****    N C R - S E N D I N G   A G E N T S   *****
;;;
;;;  This portion of the file defines the class NCR-SENDING-AGENT.
;;;  NCR-sending agents are agents that send node-construction requests.
;;;  Since node constructors are a centralized limited resource in DUAL,
;;;  it frequently happens that an agent has to wait in order to recruit
;;;  such node constructor. NCR-sending agents are specialized in this.
;;;  They keep unsent NC-requests in a queue and periodically check for
;;;  a free NC without blocking the input queue (i.e. there are no busy
;;;  waits). To do that, NCR-sending agents have an elaborate symbolic
;;;  microcycle.
;;;  The class NCR-SENDING-AGENT is a mix-in class. Classes that inherit
;;;  from it should arrange that it takes precedence to DUAL-AGENT in
;;;  order to enforce the elaborate symbolic microcycle over the ordinary one.
;;;

;;;;;;   Class definitions  (and accessor methods)

(eval-when (compile load eval)
  (defclass NCR-sending-agent (DUAL-agent)
    ((NCR-queue  :accessor agent-NCR-queue
                 :type        list     ; of NC-requests
                 :initform    nil )
    )
    (:documentation
      "DUAL agent sending node-construction requests without busy waiting." ))
) ; eval-when


;;;;;;  Printing methods

;; No method for AGENT-DESCRIPTOR-STRING is provided because NCR-SENDING-AGENT
;; is a mix-in class.

(defmethod DUAL-describe :after ((ag NCR-sending-agent)
                                 &optional (stream *standard-output*))
  (format stream "~&  It  waits to send NC-requests: ~S~%"
          (if (slot-boundp ag 'NCR-queue)
              (agent-NCR-queue ag)
              "(unbound slot)") )
  (values))


;;;;  Type-checking methods for the accessors

(defmethod  agent-NCR-queue ((x t))
  (if x
    (error "AGENT-NCR-QUEUE:  ~S is not a NCR-sending agent." x)
    (error "AGENT-NCR-QUEUE applied to NIL (perhaps #$missing-agent).") ))

(defmethod  (setf agent-NCR-queue) (new-value (x t))
  (declare (ignore new-value))
  (if x
   (error "(SETF AGENT-NCR-QUEUE):  ~S is not a NCR-sending agent." x)
   (error "(SETF AGENT-NCR-QUEUE) applied to NIL (perhaps #$missing-agent).") ))


;;;;;;  NCR-queue is volatile memory (see DUAL/ARCHIT/SYMPROC1.LSP)

(defmethod  clear-volatile-memory :after ((agent NCR-sending-agent))
  (setf (agent-NCR-queue agent) nil) )



;;;;;;; Elaborate symbolic microcycle
;;;
;;; NCR-sending agents have an elaborate symbolic-microcycle. They maintain two
;;; queues -- the input zone (as always) and a queue of unsent NC-requests.
;;; When all node constructors are busy, NC-requests wait in the queue
;;; and do not block the input zone. Thus, there is no 'busy wait'.
;;;
;;; The method below supersedes the 'ordinary' symbolic microcycle defined
;;; in DUAL/ARCHIT/SYMPROC2.LSP.  See also the microcycle of node constructors.
;;;
;;; To do: Garbage collection during idle waits.

(eval-when (compile load eval)
  (proclaim-consumption 'work-on-NC-requests :explicit-S-PROGN )
)

(defmethod  symbolic-microcycle ((host NCR-sending-agent))
  (declare (values S-PROGN-return-type))
  (labels ((non-empty-input () (not (endp (agent-input-zone host))) )
           (non-empty-NCR   () (not (endp (agent-NCR-queue host))) )
           (work-on-NC-requests ()
             (s-progn host
                (if (non-empty-NCR)
                    (let ((NC [recruit-node-constructor]))
                      (cond ((not (null NC))     ; succeeded to recruit a NC
                               (let ((NC-request (pop (agent-NCR-queue host))))
                                 [receive-symbol NC NC-request] ; send first NCR
                                 (work-on-NC-requests)))        ; loop
                            ;; Must wait for a NC. (They are busy now.)
                            ((non-empty-input)
                                [handle-next-input host])  ; while waiting
                            (t  nil) ))                    ; nothing to do
                              ; ^^^-- consider (BUSY-WAIT) at that point
                    nil))) )  ; the queue for NC-requests is empty
    ;; Main body of SYMBOLIC-MICROCYCLE
    (s-progn host
       [work-on-NC-requests]              ; try to recruit a node constructor
       (when (non-empty-input)
             [handle-next-input host] )   ; pop a symbol from the input zone
       (if (or (non-empty-input)
               (non-empty-NCR ))
           [symbolic-microcycle host]     ; loop
           nil) )))                       ; terminate
  ;; To do: Add garbage collection to make use of the busy-wait periods.

;; With this microcycle, requesting the construction of a new agent amounts
;; to formulating a node-construction request and appending it to the NCR queue.
;; The generic function SEND-NC-REQUEST supports the second step of this process.

(defmethod send-NC-request ((sender NCR-sending-agent)
                            (NC-request node-construction-request) )
  (setf (agent-NCR-queue sender)
        (nconc (agent-NCR-queue sender) (list NC-request)) ))   ; FIFO order

(defmethod send-NC-request ((x t) (y t))
  (error "SEND-NC-REQUEST: ~S is not a NCR-sending agent or ~
          ~S is not a node-construction request."  x y ))


;;;;;;;  End of file DUAL/ARCHIT/NC_AGENT.LSP
