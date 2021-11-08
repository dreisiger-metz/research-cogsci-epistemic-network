;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/coalitn.lsp
;;; VERSION:    1.1.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Coalitions (i.e. interconnected groups) of agents
;;; DEPENDS-ON: DUAL/archit/dual_ag.lsp; DUAL/intrface/defs.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    12-03-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...
;;;;;;;;;
;;;  TO DO:     Finish the documentation.  (DEFCOALITION, MEMOIZE-P, etc.)


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;       C O A L I T I O N S       ;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl:in-package  "DUAL-INTERFACE")

;;;; The key concept defined in this file is COALITION -- a 'team' of
;;;; collaborating DUAL agents.  Coalitions are the key theoretical construct
;;;; at the MESO-LEVEL of the architecture (see section 3.3. in "DUAL Report
;;;; #1"). They have three very important properties: they are _decentralized_,
;;;; _emergent_, and _dynamic_.  None of these properties is present at the
;;;; level of individual DUAL agents (the micro-level).  It is at the level
;;;; of coalitions where these properties appear in the architecture for the
;;;; first time.  Having appeared at the meso-level, these properties propagate
;;;; to upper levels and become characteristic of the DUAL approach to
;;;; cognitive modeling as a whole.
;;;;
;;;; DUAL agents are simple, they cannot do much in isolation.  Therefore,
;;;; they depend on one another and form coalitions.  A COALITION is a set of
;;;; agents and a pattern of interactions among them.  The members of a
;;;; coalition exchange activation and symbolic information.
;;;;
;;;; From the point of view of the computer implementaion, it is important
;;;; to note that coalitions are emergent.  They are present only implicitly
;;;; in the DUAL system.  It is the _external_ observer who can interpret the
;;;; organization of the system and its behavior in terms of coalitions.
;;;; Therefore, coalitions belong to the DUAL interface and are placed in
;;;; the interface package.  The "DUAL-core" package deals only with individual
;;;; DUAL agents and they are the sole legitimate material for building models
;;;; that comply to the specification of the architecture.
;;;;
;;;; Stated differently, you should not use functions like COALITION-MEMBERS
;;;; or RETRIEVAL-INDEX in the symbolic routines of individual agents.  Doing
;;;; so violates the boundary between DUAL core and implementation.
;;;; On the other hand, coalitions are indispensable tools for conceptualizing
;;;; and analyzing the DUAL models.  Therefore, the implementation supports
;;;; coalitions explicitly and provides a number of functions for analyzing
;;;; them (e.g. RETRIEVAL-INDEX).
;;;;
;;;; It should be noted, finally, that the class COALITION that exists in the
;;;; program is only a rough approximation of the theoretical notion of
;;;; 'coalition'.   Theoretical coalitions are dynamic and without clear-cut
;;;; boundaries.  Coalitions in this implementations fundamentally boil down
;;;; to (almost) static black-and-white sets of agents.  Beware!


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: coalition, coalition-p,
;;          coalition-name, coalition-comment, coalition-members, coalition-head
;;          make-coalition, defcoalition,
;;          *all-coalitions*, find-coalition, coa ;
;;
;;          count-active-members, ca,
;;          retrieval-index, Luce-retrieval-index, ri,
;;          total-retrieval-index, *total-RI-coalitions*,
;;
;;          coalition-user1, coalition-user2, coalition-user3

;; COALITION
;;
;;   A symbol that is the proper name of the class of coalitions.
;;   (See CLOS specification.) As such, it is a valid type specifier.

;; *ALL-COALITIONS*
;;
;;   A global variable bound to the list of all coalitions created via
;;   the function MAKE-COALITION.
;;
;;   It is not recommended to alter the value of this variable in any way.
;;   It is advertized in the external protocol to allow for MAPCAR and DOLIST.
;;
;;   *TOTAL-RI-COALITIONS* (see below) typically is a subset of *ALL-COALITIONS*
;;

;; MAKE-COALITION  (name &key comment members head)  -->  new-coalition
;;
;;   A function that creates a coalition and registers it into *ALL-COALITIONS*.
;;   NAME must be a symbol (but not  NIL).
;;   If NAME is already in use, a continuable error is signalled and the
;;     user is prompted to supply a new name.
;;   COMMENT must be a string; if not supplied, it defaults to NIL.
;;   MEMBERS must be a (possibly empty) list of DUAL agents.
;;   HEAD must be either NIL (the default) or a DUAL agent that is a member
;;     of MEMBERS.
;;
;;   After a successful call to MAKE-COALITION, the following conditions hold:
;;     (find-coalition name)              -->  new-coalition
;;     (agentp new-coalition)             -->  T
;;     (coalition-name    new-coalition)  -->  name
;;     (coalition-comment new-coalition)  -->  comment  ; or NIL
;;     (coalition-members new-coalition)  -->  members  ; or NIL
;;     (coalition-head    new-coalition)  -->  head     ; or NIL
;;     (coalition-user1   new-coalition)  -->  nil
;;     (coalition-user2   new-coalition)  -->  nil
;;     (coalition-user3   new-coalition)  -->  nil
;;
;; The macro DEFCOALITION provides convinient interface to MAKE-COALITION.

;; DEFCOALITION  (name &rest initargs)  - ->  new-coalition
;;
;;   A macro that macroexpands into a call to MAKE-COALITION.
;;
;;   Syntax (in BNF):  (defcoalition  name
;;                        [documentation-string]
;;                        [:head agent-name]
;;                        [:members ({agent-name}*)]  )
;;
;;   The names are not evaluated.  Agent names are fed to FIND-AGENT...
;;   ...


;; COALITION-P  (thing)  -->  T or NIL
;;
;;   A predicate that checks whether something is a coalition.

;; FIND-COALITION (name)  -->  coalition or NIL
;; COA            (name)  -->  coalition or NIL
;;
;;   A function that looks for a coalition whose name is NAME. If a coalition
;;   with the same (eq) COALITION-NAME is found in *ALL-COALITIONS*, it is
;;   returned as the value of FIND-COALITION. If no such coalition is found,
;;   FIND-COALITION returns NIL.
;;   If NAME is not a symbol, an error is signaled.
;;
;;   The function COA is a shorthand for FIND-COALITION.


;; COALITION-NAME (coalition)  -->  name
;;
;;   A generic function that reads the name of COALITION. Returns a symbol.
;;   Each agent coalition has a name and two different coalitions cannot have
;;   the same name. The name is assigned to the coalition at the moment of its
;;   construction (by a call to MAKE-COALITION or DEFCOALITION) and cannot be
;;   modified.
;;   It is always true that (provided *ALL-COALITIONS* is not corrupted):
;;      (find-coalition (coalition-name coalition))  -->  coalition
;;      (coalition-name (find-coalition name))       -->  name
;;      (coalition-name nil)                         signals an error
;;      (setf (coalition-name agent) 'new-name)      signals an error

;; COALITION-COMMENT (coalition)  -->  comment
;; (SETF COALITION-COMMENT)
;;
;;   A generic function that accesses the comment of COALITION.
;;   Returns a string or NIL. May be used with SETF.
;;   COALITION should be either a coalition or a symbol.  If it is a symbol,
;;     it is passed to FIND-COALITION and the resulting coalition is used.
;;     If FIND-COALITION returns NIL, a message is printed on *STANDARD-OUTPUT*.
;;
;;   (SETF COALITION-COMMENT) does not accept symbols as arguments.

;; COALITION-MEMBERS (coalition)  -->  member-list
;; (SETF COALITION-MEMBERS)
;;
;;   A generic function that accesses the member-list of COALITION.
;;   Returns a (possibly empty) list of DUAL-agents. May be used with SETF.
;;   COALITION should be either a coalition or a symbol.  If it is a symbol,
;;     it is passed to FIND-COALITION and the resulting coalition is used.
;;     If FIND-COALITION returns NIL, a message is printed on *STANDARD-OUTPUT*.
;;
;;   (SETF COALITION-MEMBERS) does not accept symbols as arguments.
;;   (SETF COALITION-MEMBERS) checks whether the new value is a list of agents.
;;     If it isn't, a continuable error is signaled; if continued, the old
;;     member-list is not changed.

;; COALITION-HEAD (coalition)  -->  head-agent
;; (SETF COALITION-HEAD)
;;
;;   A generic function that accesses the 'head agent' of COALITION.
;;   Returns either NIL or a DUAL agent.
;;   COALITION should be either a coalition or a symbol.  If it is a symbol,
;;     it is passed to FIND-COALITION and the resulting coalition is used.
;;     If FIND-COALITION returns NIL, a message is printed on *STANDARD-OUTPUT*.
;;
;;   (SETF COALITION-HEAD) does not accept symbols as arguments.
;;   (SETF COALITION-MEMBERS) checks whether the new value satisfies AGENTP and
;;     is a member of COALITION-MEMBERS.  If it isn't, a continuable error is
;;     signaled; if continued, the old coalition head is not changed.


;; COALITION-USER1 (coalition)  -->  user-field-1
;; COALITION-USER2 (coalition)  -->  user-field-2
;; COALITION-USER3 (coalition)  -->  user-field-3
;; (SETF COALITION-USER1)
;; (SETF COALITION-USER2)
;; (SETF COALITION-USER3)
;;
;;   There are three fields in each coalition that are left for use of
;;   individual models.  These are the accessors for these fields.


;; COUNT-ACTIVE-MEMBERS  (coalition)  -->  (values active-count total-count)
;; CA (coalition-or-symbol)           -->  (values active-count total-count)
;;
;;   Functions that count the active members of a coalition (i.e. the
;;   members that satisfy AGENT-VISIBLE-P, see DUAL/ARCHIT/CONNECT.LSP).
;;
;;   COALITION should be either a coalition or a list of agents. If it isn't,
;;    an error is signalled.
;;   The function returns two values -- the number of active members and
;;    the total number of agents in the coalition.
;;
;;   The function CA is a shorthand for COUNT-ACTIVE-MEMBERS.
;;   Its argument may be a coalition, list, or a symbol.
;;   Symbols are passed to FIND-COALITION and then to COUNT-ACTIVE-MEMBERS.
;;    (Note that when FIND-COALITION returns NIL on a symbol, CA returns 0.0)


;; RETRIEVAL-INDEX (coalition &optional memoize-p)  -->  retrieval-index
;;
;;   A generic function that computes the 'retrieval index' for COALITION.
;;
;;   The retrieval index is the average activation level of all members of the
;;    coalition, including those whose activation level is zero.
;;
;;   COALITION should be a coalition, a list, or a symbol.
;;   MEMOIZE-P ....   When not supplied, it defaults to NIL.
;;
;;   ...
;;
;;   See also the shorthand RI.

;; TOTAL-RETRIEVAL-INDEX  (&optional coa-list memoize-p)  -->  total-ri
;; *TOTAL-RI-COALITIONS*
;;
;;   A function that computes the sum of the retrieval indices of the items
;;   in COA-LIST.
;;
;;   COA-LIST should be a list of items suitable for passing to RETRIEVAL-INDEX.
;;     When not supplied, COA-LIST defaults to *TOTAL-RI-COALITIONS*.
;;     If COA-LIST is empty, the function returns 0.0.
;;   MEMOIZE-P has the same interpretation as in RETRIEVAL-INDEX. When not
;;     supplied, it defaults to NIL.
;;
;;   *TOTAL-RI-COALITIONS* is a global variable keeping the default value for
;;   the optional argument to TOTAL-RETRIEVAL-INDEX.  Its initial value is NIL.
;;   No function in this file changes the value of *TOTAL-RI-COALITIONS* -- it
;;   must be set by the user.


;; LUCE-RETRIEVAL-INDEX (coalition &optional coa-list memoize-p)  -->  Luce-ri
;;
;;   A function that computes the 'Luce retrieval index' for COALITION.
;;
;;   The Luce retrieval index is computed by the formula:
;;      (/ (retrieval-index coalition)
;;         (total-retrieval-index coa-list))
;;    except that certain memoizations may take place depending on MEMOIZE-P.
;;    If both terms in the formula satisfy ZEROP, the function returns 0.0.
;;    If the numerator is non-zero and the denominator is zero, an error is
;;    signaled.
;;
;;   COALITION should be suitable for passing to RETRIEVAL-INDEX.
;;   COA-LIST should be a list of objects suitable for passing to TOTAL-
;;    RETRIEVAL-INDEX.  If not supplied, it defaults to *TOTAL-RI-COALITIONS*.
;;   MEMOIZE-P has the same interpretation as in RETRIEVAL-INDEX. Note, however,
;;    that the default value is different -- it is T for LUCE-RETRIEVAL-INDEX.
;;
;;   See also the shorthand RI.

;; RI  (coalition &optional coa-list memoize-p)  -->  (values ri Luce-ri)
;;
;;   A function that is a shorthand for RETRIEVAL-INDEX and LUCE-RETRIEVAL-INDEX
;;   It returns two values -- the raw index and the Luce index.
;;
;;   COALITION should be suitable for passing to RETRIEVAL-INDEX.
;;   COA-LIST should be suitable for passing to LUCE-RETRIEVAL-INDEX (and hence
;;     to TOTAL-RETRIEVAL-INDEX).  Defaults to *TOTAL-RI-COALITIONS*.
;;   MEMOIZE-P has the same interpretation as in RETRIEVAL-INDEX. Note, however,
;;     that the default value is different -- it is T for RI.


;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric  retrieval-index  (coalition &optional memoize-p)
  (:documentation  "Average activation level of members." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definition  (and accessor methods)

(eval-when (compile load eval)
  (defclass coalition (DUAL-interface-object)
    ((name      :reader      coalition-name
                :type        symbol
                :initarg     :name
                :initform    (required-argument) )
     (comment   :accessor    coalition-comment
                :type        (or null string)
                :initform    nil  )
     (members   :accessor    coalition-members
                :type        list
                :initform    nil  )
     (head      :accessor    coalition-head
                :type        (or null DUAL-agent)
                :initform    nil  )
     ;; Auxiliary slots
     (ret-idx   :accessor    memoized-retrieval-index     ; internal use only
                :type        (or null cons)      ; (cons *time* (ri ...))
                :initform    nil
                :documentation "Used by function RETRIEVAL-INDEX."  )
     (user1     :accessor    coalition-user1              ; advertized
                :initform    nil  )
     (user2     :accessor    coalition-user2              ; advertized
                :initform    nil  )
     (user3     :accessor    coalition-user3              ; advertized
                :initform    nil  )
    ))
) ; eval-when


;;;;;;  Printing methods

(defmethod print-object  ((coa coalition) stream)
  (if (slot-boundp coa 'name)
      (format stream  "#<coa ~S>" (coalition-name coa))
      (format stream  "#<anonymous coalition>") ))

(defmethod DUAL-describe  ((coa coalition)
                           &optional (stream *standard-output*))
  (format stream  "~&~S is an object of type ~S named ~S."
          coa (type-of coa) (if (slot-boundp coa 'name)
                                (coalition-name coa)
                                "(no name)") )
  (format stream "~&  Its comment string is: ~A~%"
          (if (and (slot-boundp coa 'comment)
                   (coalition-comment coa))          ; supplied?
              (coalition-comment coa)
              "(no comment)"))
  (format stream "~&  Its members are: ~S~%"
          (if (slot-boundp coa 'members)
              (coalition-members coa)
              "(unbound)"))
  (format stream "~&  Its head is: ~S~%"
          (if (slot-boundp coa 'head)
              (coalition-head coa)
              "(unbound)"))
  (values))


;;;;  Type predicate

(declaim (inline coalition-p))
(defun coalition-p (thing)
  (and (typep thing 'coalition) T ))


;;;;;;;;;;;   Housekeeping functions
;;
;;  There is a global registry for all coalitions implemented as a list.
;;  This list is bound to the global variable *ALL-COALITIONS*, which is in
;;  the domain of the implementation. Users interact with the registry through
;;  the functions of the external protocol: MAKE-COALITION, FIND-COALITION,
;;  REMOVE-COALITION, and DO-ALL-COALITIONS.

(defvar  *all-coalitions*  '()
         "List of all coalitions registered in the system." )

(defun find-coalition (name)
  "Looks for a coalition by (the symbol) NAME. See also the macro COA."
  (declare (values (or coalition null)))
  (assert (symbolp name))
  (find name *all-coalitions* :key #'coalition-name) )


(defun make-coalition  (name &key comment members head)
  "Makes a and registers a coalition."
  (declare (values coalition)
           (type (or null string) comment)
           (type list members)
           (type (or null DUAL-agent) head) )
  (assert (and (symbolp name)
               (not (null name))      ; NIL is not valid
               (null (find-coalition name)))
          (name)                      ; continuable error
          "MAKE-COALITION: ~S is not a symbol or is already in use."
          name)
  (let ((new-coa (make-instance 'coalition :name name)))
    (when  comment                    ; COMMENT supplied?
      (setf (coalition-comment new-coa) comment))
    (when  members                    ; MEMBERS supplied?
      (when (notevery #'agentp members)
        (cerror "Include only valid members and continue."
                "MAKE-COALITION: Invalid members ~S in coalition ~S."
                members name)
        (setq members (remove-if (complement #'agentp) members)) )
      (setf (coalition-members new-coa) members) )
    (when  head                       ; HEAD supplied?
      (if (member head members)
          (setf (coalition-head new-coa) head)
          (cerror "Use NIL as coalition head and continue."
              "~S cannot be a head of coalition ~S because it is not a member."
                  head name) ))
    ;; Register the new coalition (FIFO order) and return it to user.
    (setq *all-coalitions*
          (append *all-coalitions* (list new-coa)))
    new-coa ))


;;;;;;;;  Shorthand macros  ;;;;;;;;;

(declaim (inline coa))
(defun coa (name)
  "Shorthand for FIND-COALITION."
  (find-coalition name) )

(defmacro defcoalition (name &rest initargs)   ; Syntax:
  "Shorthand for MAKE-COALITION."              ; (DEFCOALITION  coalition-name
  (let ((comment nil)  ; to be set below       ;   [documentation-string]
        (members nil)    ; to be set below     ;   [:head agent-name]
        (head    nil) )                        ;   [:members list-of-names]
    (when (stringp (first initargs))           ; )
      (setq comment (pop initargs)) )
    (when (eq :head (first initargs))
      (pop initargs)
      (setq head (pop initargs)) )
    (when (eq :members (first initargs))
      (pop initargs)
      (setq members (pop initargs)) )
    (unless (null initargs)
      (warn "DEFCOALITION: Ignoring unknown initargs ~S." initargs ))
    ;; The actual macroexpansion:
    `(make-coalition ',name
           :comment ,comment
           :members (mapcar #'find-agent ',members)
           :head    (cond ((null ',head) nil)
                          ((find-agent ',head))
                          (t (cerror "Use NIL as coalition-head and continue."
                                     "DEFCOALITION: Cannot find-agent ~S."
                                     ',head)))) ))


;;;;;;;  Auxillary and type-checking methods for the accessors.

(defmethod  coalition-name ((x t))
  (error "COALITION-NAME:  ~S is not a coalition." x))

(defmethod  (setf coalition-name) (new-value (x t))
  (declare (ignore new-value))
  (error "COALITION-NAME cannot be used with SETF. Names are immutable.") )


(defmethod  coalition-comment ((name symbol))
  (let ((coa (find-coalition name)))
    (if coa
        (coalition-comment coa)
        (format nil "Cannot find coalition named ~S." name) )))

(defmethod  coalition-comment ((x t))
  (error "COALITION-COMMENT:  ~S is not a coalition." x ))

(defmethod  (setf coalition-comment) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF COALITION-COMMENT):  ~S is not a coalition." x ))


(defmethod  coalition-members ((name symbol))
  (let ((coa (find-coalition name)))
    (if coa
        (coalition-members coa)
        (format nil "Cannot find coalition named ~S." name) )))

(defmethod  coalition-members ((x t))
  (error "COALITION-MEMBERS:  ~S is not a coalition." x ))

(defmethod  (setf coalition-members) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF COALITION-MEMBERS):  ~S is not a coalition." x ))

(defmethod  (setf coalition-members) :around  (members (coa coalition))
  (if (and (listp members)
           (every #'agentp members))
      (call-next-method)
      (cerror "Keep the old members and continue."
              "Illegal coalition members ~S for ~S in SETF."
              members coa) ))


(defmethod  coalition-head ((name symbol))
  (let ((coa (find-coalition name)))
    (if coa
        (coalition-head coa)
        (format nil "Cannot find coalition named ~S." name) )))

(defmethod  coalition-head ((x t))
  (error "COALITION-HEAD:  ~S is not a coalition." x ))

(defmethod  (setf coalition-head) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF COALITION-MEMBERS):  ~S is not a coalition." x ))

(defmethod  (setf coalition-head) :around  (head (coa coalition))
  (if (or (null head)
          (and (agentp head)
               (member head (coalition-members coa)) ))
      (call-next-method)
      (cerror "Keep the old head and continue."
              "(SETF COALITION-HEAD): ~S is not a member of ~S."
              head coa) ))



;;;;;;;;;  Retrieval indices show the degree of activation of a given coalition

(defvar *total-RI-coalitions*  nil
  "Default for the optional argument to TOTAL-RETRIEVAL-INDEX." )

(defun  count-active-members (coalition)
  "Returns two values: active-count and total-count."
  (declare (type (or coalition list) coalition)
           (values integer integer) )
  (let ((members (etypecase coalition
                   (coalition (coalition-members coalition))
                   (list      coalition)))
        (active-count 0)
        (total-count 0) )
    (dolist (agent members)
      (incf total-count)
      (when (agent-visible-p agent)
        (incf active-count)) )
    (values active-count total-count) ))


(defmethod  retrieval-index  ((coa coalition) &optional (memoize-p NIL))
  (declare (values float))                                       ; ^^^
  (let ((memo (memoized-retrieval-index coa)))
    (if (and memoize-p                                ; if memoization allowed
             memo                                     ; and have memo
             (= *time* (car memo)))                   ; which is up to date
        (cdr memo)
        (let ((ret-idx (retrieval-index               ; compute from list
                         (coalition-members coa))))
          (setf (memoized-retrieval-index coa)        ; cache for future use
                (cons *time* ret-idx))
          ret-idx) )))

(defmethod  retrieval-index  ((agents list) &optional memoize-p)
  ;; This method is the one that actually computes retrieval indices.
  (declare (ignore memoize-p)
           (values float) )
  (let ((sum-act 0.0)
        (count   0) )
    (dolist (agent agents)
      (incf count)
      (incf sum-act (agent-activation agent)) )
    (if (zerop count)          ; empty list?
        0.0
        (/ sum-act count)) ))

(defmethod  retrieval-index  ((name symbol) &optional memoize-p)
  (declare (values float))
  (assert (find-coalition name)
          (name)
          "RETRIEVAL-INDEX: ~S does not name a coalition." name )
  (retrieval-index (find-coalition name) memoize-p) )

(defmethod  retrieval-index  ((x t) &optional memoize-p)
  (declare (ignore memoize-p))
  (error "Bad argument to RETRIEVAL-INDEX: ~S." ))


(defun  total-retrieval-index (&optional (coa-list *total-RI-coalitions*)
                                         (memoize-p NIL) )
  (let ((result 0.0))
    (dolist (coa coa-list result)
      (incf result (retrieval-index coa memoize-p))) ))


(defun  Luce-retrieval-index (coalition
                              &optional (coa-list *total-RI-coalitions*)
                                        (memoize-p T))  ; NB: Different default
 "Retrieval index of a COALITION relative to the total index of COA-LIST."
  (declare (type list coa-list)
           (values float) )      ; [0,1]
  (let ((index (retrieval-index coalition memoize-p))
        (total-index (total-retrieval-index coa-list memoize-p)) )
    (cond ((not (zerop total-index))  (/ index total-index))
          ((zerop index)              0.0)                    ; (/ 0 0)
          (t  (cerror "Return 0.0 and continue."
                      "Divizion by zero in (Luce-retrieval-index ~S ...)."
                      coalition)
              0.0) )))


;;;; Shorthands

(defun ca (coalition)
  "Shorthand for COUNT-ACTIVE-MEMBERS."
  (declare (type (or coalition list symbol) coalition)
           (values integer integer) )
  (when (symbolp coalition)
    (setq coalition (find-coalition coalition)) )
  (count-active-members coalition) )
  ;; If FIND-COALITION returns NIL, CA will return 0.0.

(defun ri (coalition &optional (coa-list *total-RI-coalitions*)
                               (memoize-p T) )  ; NB: Diff.from RETRIEVAL-INDEX
  "Shorthand for (values RETRIEVAL-INDEX LUCE-RETRIEVAL-INDEX)."
  (declare (values float float))
  (values (retrieval-index coalition memoize-p)
          (Luce-retrieval-index coalition coa-list memoize-p)) )


;;;;;;;;  End of file  DUAL/INTRFACE/COALITN.LSP
