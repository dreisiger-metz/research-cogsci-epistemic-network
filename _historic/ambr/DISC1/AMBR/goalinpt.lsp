;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/goalinpt.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Goal and input lists of the AMBR model.
;;; DEPENDS-ON: DUAL; ambr/defs.lsp, ambr/proclaim.lsp, ambr/krepres.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    14-02-98 [2.2.0] -- Based on a part of AMBR/TOPLEVEL.LSP.
;;; UPDATED:    07-05-98 [2.2.2]
;;; UPDATED:    14-08-98 -- The 'official release'
;;; UPDATED:    ...


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;    G O A L   A N D   I N P U T   L I S T S    ;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR-CORE")

;;;; This file defines the two sources of activation in the AMBR model --
;;;; the so-called GOAL and INPUT lists (or nodes).  In fact, they are
;;;; implemented as special working memories (see DUAL/ARHCIT/WORK_MEM.LSP).
;;;;
;;;; Section 3.4.2 of "DUAL Report #1" describes these two nodes like this:
;;;;   Activation can enter the DUAL [and AMBR] network from a special node
;;;;   called INPUT NODE. This node models the influence of the environment.
;;;;   In future models it will be replaced by a whole formation -- the
;;;;   VISUAL ARRAY.  At the time being, however, the perceptual mechanisms
;;;;   in the architecture are extremely limited.  There is only one node
;;;;   which is a constant (and strong) source of activation.  The human
;;;;   user of the system attaches some agents to that node, thus allowing
;;;;   for the spread of activation.  In this case we speak of _exogeneous_
;;;;   _activation_.  It is the medium of perceptual stimulation in DUAL
;;;;   models.
;;;;
;;;;   There are sources of _endogeneous_activation_ too.  Most importantly,
;;;;   there is a special GOAL NODE which is always active.  It is, in a
;;;;   very rudimentary sense, the medium of the 'intentions' of the system.
;;;;   When the model is working on some task, the agent(s) representing the
;;;;   goal of the task is connected (again by the human user) to the goal
;;;;   node and thus receives continuous and strong support from it.  In DUAL
;;;;   there may be several goals connected to the goal node simultaneously
;;;;   and competing for the resources of the system.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;   This file also deals with DRIVER TAGS.  There are two such tags:
;;;;     + :T-DRIVER -- distinguishes the Target situation.  Added to the :TYPE
;;;;         slot of the target situation-agent manually when presenting the
;;;;         target problem to the model.
;;;;     + :B-DRIVER -- distinguishes the Base situation.  Added automatically
;;;;         (by the promotion mechanism, see AMBR/PROMOTN.LSP) when the
;;;;         secretary of the target situation promotes some LTM situation
;;;;         as winner.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: *goal*, *input*,
;;          add-to-goal/input, remove-from-goal/input ;
;;
;;          T-driver-p, B-driver-p, add-T-DRIVER-tag, add-B-DRIVER-tag
;;
;; The functions and variables defined in this file depend on:
;;   *DEFAULT-GOAL-CAPACITY* and *DEFAULT-INPUT-CAPACITY* defined in AMBR/DEFS.

;; *GOAL*
;; *INPUT*
;;
;;   Global variables ...

;; ADD-TO-GOAL/INPUT  (agent weight)  -->  T or NIL
;;
;;   A function that ...
;;
;;   TAG should be either :GOAL or :INPUT. If it isn't, an error is signaled.
;;   ...

;; REMOVE-FROM-GOAL/INPUT  (agent &optional reset-p)  -->  T or NIL
;;
;;   A function that ...
;;
;;   Note that ulike REMOVE-FROM-WM, the default value for RESET-P is NIL.

;; As a side effect of adding/removing an agent to *GOAL* or *INPUT*, a tag is
;; added/removed from the :TYPE slot of the agent.
;; For instance-agents, a tag is added/removed from the respective situation-
;; agent, if any -- see AMBR/KREPRES.LSP and AMBR/SITUATN.TXT.


;; T-DRIVER-P (situation-agent)  -->  T or NIL
;; B-DRIVER-P (situation-agent)  -->  T or NIL
;;
;;   Functions that check for driver tags in the TYPE slot of SITUATION-AGENT.
;;
;;   T-DRIVER-P returns T iff there is a :T-DRIVER tag in the TYPE slot.
;;   B-DRIVER-P returns T iff there is a :B-DRIVER tag in the TYPE slot.
;;
;;   SITUATION-AGENT should be a situation agent (see AMBR/SITUATN.TXT and
;;     AMBR/KREPRES.LSP) although this is not checked.
;;
;;   It is possible that one and the same agent satisfies both predicates.

;; ADD-T-DRIVER-TAG (situation-agent)  -->  :T-DRIVER
;; ADD-B-DRIVER-TAG (situation-agent)  -->  :B-DRIVER
;;
;;   Functions for adding driver tags to the TYPE slot of SITUATION-AGENT.
;;
;;   The functions do not check whether SITUATION-AGENT is really a situation
;;   agent (see AMBR/SITUATN.TXT).

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   *GOAL* and *INPUT* are global variables

(defvar *input*  (make-special-WM 10 *default-input-capacity*
                                  :comment "Input list" )
  "A special working memory holding the agents on the 'input list'." )

(defvar *goal*  (make-special-WM 10 *default-goal-capacity*
                                  :comment "Goal list" )
  "A special working memory holding the agents on the 'goal list'." )

(setq *special-WMs* (list *input* *goal*))    ; see DUAL/ARCHIT/WORK_MEM.LSP


;;;;;;  Attaching an agent to *GOAL* or *INPUT*

(defun  add-to-goal/input (agent weight tag)
  "Attach AGENT to the goal or input list depending on TAG."
  (declare (type AMBR-agent agent)
           (type number weight)
           (values boolean) )
  (let ((old-WM (now-in-WM agent))
        (new-WM (ecase tag
                  (:goal  *goal*)
                  (:input *input*))) )
    (cond ((null old-WM)                            ; outside any WM ?
              (add-to-WM agent new-WM weight))
          ((eq old-WM *WM*)                         ; in the primary WM ?
              (remove-from-WM agent nil)
              (add-to-WM agent new-WM weight))
          ((eq old-WM new-WM)                       ; already there ?
              (add-goal/input-tag agent tag)        ; just in case...
              nil)
          (t  (cerror "Remove ~S from ~S, add it to ~S, and continue."
                      "ADD-TO-GOAL/INPUT: ~S is attached to ~S."
                      agent old-WM new-WM)
              (remove-from-WM agent nil)
              (add-to-WM agent new-WM weight)) )))


(defmethod  add-to-WM :after  ((agent AMBR-agent)
                               (WM (eql *goal*))  &optional weight )
  (declare (ignore WM weight))
  (add-goal/input-tag agent :goal) )

(defmethod  add-to-WM :after  ((agent AMBR-agent)
                               (WM (eql *input*))  &optional weight )
  (declare (ignore WM weight))
  (add-goal/input-tag agent :input) )

(defun add-goal/input-tag (agent tag)
  (declare (type (member :goal :input) tag))
  ;; Add a tag to the agent itself
  (let ((type-slot (locate-mfr-component agent :type)))
    (if (null type-slot)    ; missing :TYPE slot, which is anomalous
        (add-G-slot agent :type  :filler tag)
        (add-filler-elt type-slot tag
                        :priority :old  :order :lifo)))
  ;; For instance-agents, add a tag to the respective situation-agent, if any.
  (when (agent-type agent :instance)
    (let ((situation (agent-situation agent)))
      (unless (or (null situation)            ; unless free-standing instance
                  (eq agent situation))       ; or implicit self-affiliation
        (add-goal/input-tag situation tag)))) )


;;;;;;  Removing an agent from *GOAL* or *INPUT*

(defun  remove-from-goal/input (agent &optional (reset-p NIL))
  "Remove AGENT from the goal or input list."          ; ^^^
  (declare (type AMBR-agent agent))
  (if (member (now-in-WM agent) (list *goal* *input*))
      (remove-from-WM agent reset-p)
      nil ))

(defmethod  dual-core::remove-from-WM-aux :after  ((agent AMBR-agent)
                                                   (WM (eql *goal*)) )
  (declare (ignore WM))
  (remove-goal/input-tag agent :goal) )

(defmethod  dual-core::remove-from-WM-aux :after  ((agent AMBR-agent)
                                                   (WM (eql *input*)) )
  (declare (ignore WM))
  (remove-goal/input-tag agent :input) )

(defun remove-goal/input-tag (agent tag)
  (declare (type (member :goal :input) tag))
  ;; Remove the tag from the agent itself
  (remove-filler-elt (locate-mfr-component agent :type) tag)
  ;; For 'last' inst.-agents, remove the tag from the respective situation-agent.
  (when (agent-type agent :instance)
    (let ((situation (agent-situation agent)))
      (unless (or (null situation)            ; unless free-standing instance
                  (eq agent situation)        ; or implicit self-affiliation
                  (find situation             ; or some other member remains
                        (WM-contents (ecase tag
                                       (:goal  *goal*)
                                       (:input *input*)))
                        :key #'agent-situation))
        (remove-filler-elt (locate-mfr-component situation :type) tag)))) )


;;;;;;;;;;;   ******   DRIVER  TAGS   ******
;;

(defun add-T-DRIVER-tag (situation-agent)
  "Add the tag :T-DRIVER to the TYPE slot of SITUATION-AGENT."
  (declare (type instance-agent situation-agent))
  (let ((type-slot (locate-mfr-component situation-agent :type)))
    (if (null type-slot)    ; missing :TYPE slot, which is anomalous
        (add-G-slot situation-agent :type :filler :T-DRIVER)
        (add-filler-elt type-slot :T-DRIVER :priority :old :order :lifo))
    :T-DRIVER))

(defun add-B-DRIVER-tag (situation-agent)
  "Add the tag :B-DRIVER to the TYPE slot of SITUATION-AGENT."
  (declare (type instance-agent situation-agent))
  (let ((type-slot (locate-mfr-component situation-agent :type)))
    (if (null type-slot)    ; missing :TYPE slot, which is anomalous
        (add-G-slot situation-agent :type :filler :B-DRIVER)
        (add-filler-elt type-slot :B-DRIVER :priority :old :order :lifo))
    :B-DRIVER))


(defun T-driver-p (situation-agent)
  "Check for the tag :T-DRIVER in the TYPE slot."
  (agent-type situation-agent :T-DRIVER))

(defun B-driver-p (situation-agent)
  "Check for the tag :B-DRIVER in the TYPE slot."
  (agent-type situation-agent :B-DRIVER))


;;;;;;;  End of file AMBR/GOALINPT.LSP
