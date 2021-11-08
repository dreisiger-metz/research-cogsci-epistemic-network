;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/temp_ag.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Temporary DUAL agents
;;; DEPENDS-ON: archit/mcrframe.lsp, archit/work_mem.lsp, archit/DUAL_ag.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    10-06-97
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;;
;;; TO DO:      Document everything
;;; TO DO:      Better treatment of dead agents. In particular, define a
;;;             generic function FORGET-ABOUT which can serve for customizing
;;;             the clean-up process.
;;; TO DO:      Garbage collection.
;;;


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;     T E M P O R A R Y   A G E N T S      ;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is TEMP-DUAL-AGENT ...
;;;;
;;;; ...


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: temp-DUAL-agent, temp-agent-p,
;;          dead-temp-agent, dead-agent-p,
;;          temp-agent-dying-class,
;;          kill-temp-agent, cut-links-to-agent
;;


;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric kill-temp-agent (temp-agent)
  (:documentation "Killing a temporary agent." ))

(defgeneric temp-agent-dying-class (temp-agent)
  (:documentation "What TEMP-AGENT will become after its death." ))

(defgeneric cut-links-to-agent (dead-agent &optional where)
  (:documentation "Go through WHERE and remove any references to DEAD-AGENT." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definition

(eval-when (compile load eval)
  (defclass temp-DUAL-agent (DUAL-agent temporary-DUAL-object)
    ()  ; No specific slots, all slots are inherited.
    (:documentation "Temporary DUAL agent. It dies when leaving WM." ))

  (defclass dead-temp-agent (temp-DUAL-agent)
    ()  ; No specific slots, all slots are inherited.
    (:documentation "The 'ghost' of a dead temp-DUAL-agent." ))
) ; eval-when


;;;;;;  Printing methods

(defmethod  agent-descriptor-string ((agent temp-DUAL-agent))
  (if (eq (type-of agent) 'temp-DUAL-agent)
      "a temporary DUAL agent"
      (format nil "an agent of type ~S" (type-of agent)) ))

(defmethod  agent-descriptor-string ((agent dead-temp-agent))
  (if (eq (type-of agent) 'dead-temp-agent)
      "a dead temporary agent"
      (format nil "an agent of type ~S" (type-of agent)) ))


;;;;  Type predicate

(declaim (inline temp-agent-p))
(defun temp-agent-p (thing)
  (and (typep thing 'temp-DUAL-agent) T) )
(defun dead-agent-p (thing)
  (and (typep thing 'dead-temp-agent) T) )


;;;;;;  Auxiliary method for the constructor (see DUAL/ARCHIT/DUAL_AG.LSP)

(defmethod establish-agent-integrity :before ((agent temp-DUAL-agent))
  (let ((TYPE-slot (add-G-slot agent :type              ; add a slot
                                     :order    :lifo    ;   push to front
                                     :priority :old     ;   only if needed
                                     :notify-p nil)) )
    (add-filler-elt TYPE-slot :temporary                ; add a tag
                    :priority :old  :order :fifo        ;   only if needed
                    :notify-p nil) ))    ; the primary method will notify


;;;;;;  Killing temporary agents

(defmethod remove-from-WM :after ((agent temp-DUAL-agent)
                                  &optional (reset-p T) )
  (when (and reset-p
             (not (dead-agent-p agent)))    ; avoid killing twice
    (kill-temp-agent agent) ))


(defmethod kill-temp-agent ((agent temp-DUAL-agent))
  (declare (values boolean))
  (change-class agent
                (temp-agent-dying-class agent))
  (cut-links-to-agent agent)
  (remove-agent agent)
  )

(defmethod kill-temp-agent ((agent dead-temp-agent))
  #+:DUAL-DEBUG
    (warn "KILL-TEMP-AGENT: Killing a dead agent: ~S" agent)
  (cut-links-to-agent agent t)     ; collect garbage thoroughly
  nil)

(defmethod kill-temp-agent ((agent DUAL-agent))
  (error "KILL-TEMP-AGENT: Cannot kill a permanent agent: ~S" agent ))

(defmethod kill-temp-agent ((x t))
  (if x
      (error "KILL-TEMP-AGENT: ~S is not an agent." x)
      (error "KILL-TEMP-AGENT applied to NIL (perhaps #$missing-agent)." x) ))


(defmethod temp-agent-dying-class ((agent temp-DUAL-agent))
  (declare (ignore agent))
  'dead-temp-agent )      ; default class for dead agents, may be overridden.

(defmethod temp-agent-dying-class ((agent base-agent))
  (error "TEMP-AGENT-DYING-CLASS: ~S is a permanent agent." agent ))

(defmethod temp-agent-dying-class ((x t))
  (error "TEMP-AGENT-DYING-CLASS: ~S is not an agent." x ))


(defmethod  cut-links-to-agent ((dead-agent base-agent)
                                &optional (where (cons *WM* *special-WMs*)) )
  (declare (values null))
  (flet ((cut-AGENT (reference-holder)
           (cut-links-to-agent-aux reference-holder dead-agent) ))
    (cond ((eq where t)  (do-all-agents (ag)
                           (cut-AGENT ag)) )
          ((listp where) (dolist (WM where)
                           (do-all-wm (ag WM)
                             (cut-AGENT ag))) )
          (t  (error "CUT-LINKS-TO-AGENT: ~S is neither T nor a list of WMs."
                     where)) )))

(defmethod cut-links-to-agent ((x t) &optional where)
  (declare (ignore where))
  (error "CUT-LINKS-TO-AGENT: ~S is not an agent." x))

(defun cut-links-to-agent-aux (host dead)
  "Remove from HOST any references to DEAD."
  (declare (values null)
           (type base-agent host dead) )
  (let ((dummy-conref (make-conref dead 0.0))) ; be nice to REMOVE-FILLER-ELT
    (flet ((is-there-p ()
             (find dead (agent-neighbors host) :key #'conref-reference))
           (remove-it (fholder)
             (remove-filler-elt fholder dummy-conref nil)) )
      (when (is-there-p)
        (dolist (G-slot (agent-G-slots host))  ; usually it's in a G-slot
          (remove-it G-slot))
        (establish-microframe-integrity  host
                              :dead-neighbor)  ; reinitialize neighbor list
        (when (is-there-p)   ; still there somewhere?
          (dolist (S-slot (agent-S-slots host))
            (dolist (facet (S-slot-facets S-slot))
              (remove-it facet)))
          (establish-microframe-integrity  host
                                :dead-neighbor) )
        nil))))


;;;;;  Dead agents ignore anything that comes upon them.

(defmethod receive-activation ((dead dead-temp-agent) (amount number))
  (cut-links-to-agent dead)    ; collect garbage once again
  amount )    ; conform to the external protocol of RECEIVE-ACTIVATION

(defmethod receive-symbol ((dead dead-temp-agent) (symbol symbolic-structure))
  (cut-links-to-agent dead)    ; collect garbage once again
  symbol )    ; conform to the external protocol of RECEIVE-SYMBOL

(defmethod agent-activation ((dead dead-temp-agent))
  (declare (ignore dead))
  0.0)   ; invisible forever


;;;;;;;  End of file DUAL/ARCHIT/TEMP_AG.LSP
