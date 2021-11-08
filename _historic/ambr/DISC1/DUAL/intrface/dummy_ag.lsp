;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/dummy_ag.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Dummy agents and forward references
;;; DEPENDS-ON: DUAL/archit/mcrframe.lsp, DUAL/intrface/defs.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov
;;; CREATED:    24-04-97
;;; UPDATED:    03-02-98 [1.1]
;;; UPDATED:    18-04-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;;;;;     D U M M Y   A G E N T S       ;;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-INTERFACE")

;;;; This file defines ...
;;;; ...
;;;;


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: find-or-make-agent,
;;          make-DUAL-agent-1,
;;          check-for-dummies, dummy-agent-p
;;


;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

(eval-when (compile load eval)
  (defclass  dummy-agent (micro-frame DUAL-interface-object)
    ()
    (:documentation "Forward referenced agent. To be CHANGE-CLASSed later." ))
)

(defun dummy-agent-p (thing)
  "Checks whether an agent has been referenced but not yet defined."
  (declare (values boolean))
  (and (typep thing 'dummy-agent) T ))

(defun make-dummy-agent (name)
  "Makes a substitute for a full-fledged agent which will be constructed later."
  (declare (values dummy-agent))
  (DUAL-core::allocate-agent  name 'dummy-agent nil) )

(defun dummy->DUAL-agent (agent agent-type)
   "Transforming a dummy agent into a 'real' one by changing its class."
   (declare (type dummy-agent agent)
            (values DUAL-agent))
   (change-class agent agent-type)
   agent )


(defun make-DUAL-agent-from-dummy (dummy agent-type &rest initargs)
  "Makes a DUAL agent from a dummy agent with the same name."
  (declare (values DUAL-agent))
  (assert  (and (symbolp agent-type)
                (subtypep agent-type 'DUAL-agent))
          (agent-type)
          "~S is not a valid DUAL-agent type."  agent-type)
  (let ((new-agent (dummy->DUAL-agent dummy agent-type)))
    (apply #'initialize-agent new-agent initargs)
    (establish-agent-integrity new-agent)
    new-agent ))

(defun make-DUAL-agent-1 (name agent-type &rest initargs)
  "Dispatcher b/n MAKE-DUAL-AGENT and MAKE-DUAL-AGENT-FROM-DUMMY."
  (declare (values DUAL-agent))
  (assert (symbolp name) ()
          "MAKE-DUAL-AGENT-1: ~
           ~S is not a symbol and hence cannot be an agent name."
           name)
  (let ((agent (find-agent name)))
    (etypecase agent
      (null              ; no such agent --> start from scratch
          (apply #'make-DUAL-agent name agent-type initargs))
      (dummy-agent       ; forward-referenced agent (a dummy) --> transform
          (apply #'make-DUAL-agent-from-dummy agent agent-type initargs))
      (base-agent        ; collision
          (error "MAKE-DUAL-AGENT-1: Attempt to redefine existing agent ~S."
                 agent)) )))

(defun find-or-make-agent (agent-name)
  (cond ((not (symbolp agent-name))
           (error "FIND-OR-MAKE-AGENT: ~
                  ~S is not a symbol and hence cannot be an agent name."
                  agent-name))
        ((find-agent agent-name))       ; if exists, return it
        (t (make-dummy-agent agent-name)) ))


(defun check-for-dummies ()
  "Returns a list of all unresolved dummies (i.e. forward-referenced agents)."
  (let ((result '()))
    (do-all-agents (agent)
      (when (dummy-agent-p agent)
        (push agent result)))
    result ))


;;;;;;;  End of file DUAL/INTRFACE/DUMMY_AG.LSP
