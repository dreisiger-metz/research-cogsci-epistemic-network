;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/with_ag.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    WITH-AGENT, a companion of DEFAGENT.
;;; DEPENDS-ON: archit/mcrframe.lsp, archit/dual_ag.lsp, intrface/defagent.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    04-04-98 [1.1.2]
;;; UPDATED:    04-06-98 Added PPRINT-WITH-AGENT.
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;;    W I T H - A G E N T    M A C R O    ;;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-INTERFACE")

;;;; This file defines the WITH-AGENT macro which is a supplement to DEFAGENT.
;;;;
;;;; The purpose of DEFAGENT is to construct a brand new agent and to fill in
;;;; its slots.  The purpose of WITH-AGENT is to add new slots to an existing
;;;; agent and/or to add new fillers to the existing slots.  The objective for
;;;; WITH-AGENT can be illustrated as follows:
;;;;   Suppose that there is a concept and several different instances of it.
;;;;   The modeler wants to have top-down (:INSTANCE) links from the concept-
;;;;   agent to the instance-agents.  The latter, however, are defined much
;;;;   later, possibly in different files.
;;;;
;;;;   Thus, one often wants to avoid writing like this.
;;;;     (defagent  water   concept-agent
;;;;       :subc      liquid
;;;;       :instance  (water-1  water-2  water-3)       ; <--  not good
;;;;       ;;;   )
;;;;     (defagent  water-1  instance-agent
;;;;       :inst-of   water
;;;;       ;;;   )
;;;;     ;; Other agent definitions, possibly across different files...
;;;;     (defagent  water-2  instance-agent
;;;;       ;inst-of   water
;;;;       ;;;   )
;;;;   The drawbacks of such approach are numerous.  Agents such as WATER-1
;;;;   are referenced long before they are defined, possibly even in different
;;;;   files, which hinders error diagnostics.  Worse yet, whenever a new
;;;;   instance is added somewhere, the file describing the respective concept
;;;;   should be changed.
;;;;
;;;;   The WITH-AGENT macro allows to define the same agents and links in
;;;;   a more modular way:
;;;;     ;;;  File  KB/SEMANTIC.LSP
;;;;       (defagent  water   concept-agent
;;;;         :subc      liquid
;;;;         :instance  (water-1       ; reference only some of the instances
;;;;                    )    ; other insts will be added via WITH-AGENT later
;;;;         ;;;   )
;;;;       (defagent  water-1  instance-agent
;;;;         :inst-of   water
;;;;         ;;;   )
;;;;     ;;; End of file  KB/SEMANTIC.LSP.  This file remains invariant even
;;;;                               ; though new episodes can be added freely.
;;;;
;;;;     ;;; File KB/BASE_SIT.LSP
;;;;       (defagent  water-2  instance-agent
;;;;         :inst-of   water
;;;;         ;;;   )
;;;;       (WITH-AGENT  water         ; modify the micro-frame of WATER,
;;;;         :instance  water-2 )     ; adding a link from it to WATER-2
;;;;     ;;; End of file  KB/BASE_SIT.LSP
;;;;
;;;;     ;;; File  MY_EXP/EPISODES.LSP
;;;;       (defagent   water-666 ...)
;;;;       (with-agent  water
;;;;         :instance  water-666 )
;;;;     ;;; End of file MY_EXP/EPISODES.LSP


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: with-agent, with-agent-aux
;;          pprint-WITH-AGENT
;;

;; SYNTAX (in BNF):
;;
;; (WITH-AGENT  agent-name
;;   { G-slot-description }*
;;   { S-slot-description }*
;; )
;;
;; G-SLOT- and S-SLOT-DESCRIPTIONs are the same as in DEFAGENT.
;;
;; As you see, the syntax of WITH-AGENT is very similar to that of DEFAGENT.
;; The main differences are the following:
;;   + WITH-AGENT does not support documentation strings;
;;   + WITH-AGENT does not mention the AGENT-TYPE -- the agent name is
;;       immediately followed by slot definitions.
;;
;; The semantics of the two macros, however, is quite different:
;;   + DEFAGENT expects a 'fresh' agent name and complains if an agent with
;;       such name already exists.  DEFAGENT constructs a brand new agent
;;       given that name and defines various slots and facets.
;;   + WITH-AGENT expects a name of an existing agent and complains if such
;;       agent does not exist.  WITH-AGENT uses the name to retrieve the
;;       agent (which has been defined via DEFAGENT) and adds new slots,
;;       facets, or fillers thereof.
;;
;;;;;;;
;;
;; MACROEXPANSION  OF  WITH-AGENT  is a call to WITH-AGENT-AUX of the form:
;;
;;  `(with-agent-aux  ',agent-name
;;      :G-slots (list
;;                 (make-G-slot .... )
;;                 ....
;;               )
;;      :S-slots (list
;;                 (make-S-slot :slot1  :comment ,doc-string
;;                    :facets (list
;;                              (make-facet .... )
;;                              .... ))
;;                 (make-S-slot :slot2  .... )
;;                 ....
;;               )
;;   )
;;
;; WITH-AGENT-AUX is a function that invokes ADD-x-SLOT, ADD-FILLER, or
;;   ADD-FILLER-ELT (see DUAL/ARCHIT/MCRFRAME.LSP and =/FILLER.LSP).
;;
;; Weight defaulting and forward referencing is the same as in DEFAGENT.
;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

;; All parsing is done by PARSE-DEFAGENT-BODY, see DUAL/INTRFACE/DEFAGENT.LSP.

(defmacro with-agent (agent-name &rest initargs)
  "Adds new slots to an existing agent. See DEFAGENT."
  (when (stringp (first initargs))
    (cerror "Ignore the comment and continue."
            "Comment-strings are not allowed in WITH-AGENT: ~S ~S"
            agent-name (first initargs))
    (pop initargs) )
  (multiple-value-bind (G-slots S-slots)
                       (parse-DEFAGENT-body agent-name initargs)
   `(with-agent-aux ',agent-name
                    :G-slots (delete 'nil (list ,.G-slots))
                    :S-slots (delete 'nil (list ,.S-slots))) ))
                                   ;; If continued, CERROR returns NIL.


;;;;  Functional implementation of WITH-AGENT

(defun  with-agent-aux (agent-name &key G-slots S-slots)
  "Adds new slots to an existing agent. See WITH-AGENT macro."
  (declare (values (or base-agent null)))
  (let ((agent (find-agent agent-name)))
    (cond ((null agent)
              (cerror "Skip this agent and continue."
                      "WITH-AGENT-AUX: Cannot find agent with name ~S."
                      agent-name )
              nil )
          ((dummy-agent-p agent)
              (cerror "Skip this agent and continue."
                 "WITH-AGENT-AUX: ~S has been referenced but not yet defined."
                      agent-name )
              nil )
    (t  ; the agent is located, do the main work
      (dolist (G-slot G-slots)
        (let ((old-G-slot (locate-mfr-component agent (slot-label G-slot))))
          (if old-G-slot
              (combine-fholders old-G-slot G-slot)
              (add-G-slot agent (slot-label G-slot)
                                :filler (fholder-filler G-slot)
                                :notify-p nil) )))
      (dolist (S-slot S-slots)
        (let ((old-S-slot (locate-mfr-component agent (slot-label S-slot))))
          (if old-S-slot
              (combine-S-slots old-S-slot S-slot)
              (add-S-slot agent (slot-label S-slot)
                                :comment (S-slot-comment S-slot)
                                :notify-p nil) )))
      (establish-agent-integrity agent) ))))


(defun combine-fholders (old-fholder new-fholder)         ; G-slots and facets
  (declare (type filler-holder old-fholder new-fholder))  ; DUAL/ARCHIT/FILLER
  (dolist (new-elt (reverse (fholder-filler new-fholder)))
    (add-filler-elt old-fholder new-elt  :notify-p nil) )
  (fholder-filler old-fholder) )

(defun combine-S-slots (old-S-slot new-S-slot)
  (declare (type S-slot old-S-slot new-S-slot))       ; DUAL/ARCHIT/SLOTS.LSP
  (when (and (null (S-slot-comment old-S-slot))
             (not (null (S-slot-comment new-S-slot))))
    (setf (S-slot-comment old-S-slot)
          (S-slot-comment new-S-slot)) )
  (dolist (new-facet (S-slot-facets new-S-slot))
    (let ((old-facet (locate-mfr-component old-S-slot (slot-label new-facet))))
      (if old-facet
          (combine-fholders old-facet new-facet)
          (add-facet old-S-slot (slot-label new-facet)  nil
                                :filler (fholder-filler new-facet)
                                :notify-p nil) ))) )


;;;;;;;;  Writing WITH-AGENT forms to character streams
;;

(defun  pprint-WITH-AGENT (WITH-AGENT-form
                           &optional (stream *standard-output*))
  (declare (type list WITH-AGENT-form)
           (type stream stream) )
  #+:DUAL-DEBUG (assert (eq 'with-agent (first WITH-AGENT-form)))
  (let ((*print-case* :downcase)
        (*print-level* nil)
        (*print-length* nil))
    (format stream "~%(with-agent  ~S"
                   (second WITH-AGENT-form))              ; agent-name
    (pprint-DEFAGENT-slots (rest (rest WITH-AGENT-form))  ; slots
                           stream)             ; see DUAL/INTRFACE/DEFAGENT.LSP
    (format stream "~%)~%" )
    (values) ))


;;;;;;;  End of file DUAL/INTRFACE/WITH_AG.LSP
