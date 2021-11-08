;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-interface -*-

;;; FILE:       AMBR/intrface/genkb.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Tools for generating variants of the knowledge base.
;;; DEPENDS-ON: DUAL, DUAL/INTRFACE/; ambr/intrface/defs.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    03-06-98 [2.2.2]
;;; UPDATED:    14-08-98 The 'official release'
;;; UPDATED:    ...
;;;;;;;
;;; TO DO:      Add explicit support for the special agent *OTHER*.
;;;             (See AMBR/KB/SEMANTIC/ABSTRACT.LSP for documentation about it.)


       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;;;     KNOWLEDGE  BASE  GENERATION     ;;;;;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package  "AMBR-INTERFACE")

;;;; This file defines ...


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: generate-KB, generate-KB-immediately, print-GENKB-herald ;
;;
;;          GENKB-template, register-GENKB-template, *GENKB-heralds*,
;;          find-GENKB-template, remove-all-GENKB-templates,
;;          combine-GENKB-descriptors, do-all-GENKB-templates,
;;          process-GENKB-template, GENKB->links
;;
;;          sample, biased-coin-p
;;
;; Also, AMBR/INTRFACE/DEFS.LSP has a whole section on GENKB.

;; ...

;; SYNTAX of GENKB-TEMPLATE (in BNF):
;; Note that it is a function, not a macro.
;;
;; (GENKB-TEMPLATE
;;   [:HERALD    herald-string]
;;   [:TEMPLATES '( {template}* )]    ; the quote is needed because
;; )                                  ; GENKB-TEMPLATE is a function
;;
;; template ::= ( agent-name {descriptor}* )
;;
;; herald-string ::= <satisfies STRINGP>
;; agent-name    ::= <satisfies SYMBOLP>
;;
;; descriptor ::= { A-LINK-descriptor  |    ; the user may define
;;                  INSTANCE-descriptor }   ; new descriptor types
;;
;; A-LINK-descriptor   ::= ( :A-LINK   {A-LINK-subdescriptor}*   )
;; INSTANCE-descriptor ::= ( :INSTANCE {INSTANCE-subdescriptor}* )
;;
;; A-LINK-subdescriptor   ::= (agent-name probability)
;; INSTANCE-subdescriptor ::= (agent-name frequency)
;;
;; probability ::= <satisfies FLOATP>    ; between 0.0 and 1.0
;; frequency   ::= <satisfies INTEGERP>  ; positive
;;

;; Example:  (see AMBR/KB/EPISODIC/B_WTP.LSP)
;;  (genKB-template
;;    :herald  "Base sit.WTP -- Water in a Teapot on a hot Plate."
;;    :templates '(
;;      (teapot         (:instance (tpot-WTP     3)) )
;;      (hot-plate      (:instance (hplate-WTP   5))
;;                      (:a-link   (T-of-WTP-p  0.2)) )
;;      (temperature-of (:instance (T-of-WTP-w1  3)
;;                                 (T-of-WTP-p   3))
;;                      (:a-link   (high-T-WTP  0.1)) )
;;      (in             (:instance (in-WTP       1)) )
;;      (on             (:instance (on-WTP       1)) )
;;  ))


;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric  GENKB->links (label descriptors)
  (:documentation
    "Generate (probabilistically) links based on GENKB descriptors." ))

(defgeneric  combine-GENKB-descriptors (label old-descriptors new-descriptors)
  (:documentation "Label-specific union of two descriptor lists." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;;;;;;   ****  RANDOM SAMPLING  ****
;;

(declaim (inline biased-coin-p))
(defun biased-coin-p (prob-success)                       ; for A-LINKs
  "Returns T with probability p=PROB-SUCCESS and NIL with q=1-p."
  (declare (type (float 0.0 1.0) prob-success))
  (< (random 1.0) prob-success) )

(defun sample (pairs &optional total-frequency)           ; for INSTANCE links
  "Select and return one pair with probability proportional to its frequency."
  (declare (type cons pairs)      ; non-empty list of of <item, freq> pairs.
           (type (or number null) total-frequency) )
  (when (null total-frequency)          ; not supplied and must be computed?
    (setq total-frequency (second (first pairs)))
    (dolist (pair (rest pairs))
      (incf total-frequency (second pair))) )
  (assert (and (numberp total-frequency) (plusp total-frequency)))
  ;; The sampling is done using Knuth's 'Algorithm S' cited in section 11.2
  ;; of Jon Bentley's book "Programming Pearls" (Addison-Wesley, 1980).
  (let ((rnd (random total-frequency)))
    (dolist (pair pairs :error)
      (decf rnd (second pair))
      (if (minusp rnd)
          (return pair)
          )  ; continue
    )))


;;;;;;;;  *** Auxiliary predicates ***
;;
;;   template      ::= (agent-name {descriptor}*)
;;   descriptor    ::= (link-label {subdescriptor}*)
;;   subdescriptor ::= (agent-name number)

(defun GENKB-subdescriptor-p (thing)
  (and (listp thing)
       (= 2 (length thing))
       (symbolp (first thing))
       (numberp (second thing)) ))

(defun GENKB-descriptor-p (thing)
  (and (listp thing)
       (link-label-p (first thing))
       (listp (rest thing))
       ))   ; no check for (every #'subdescriptor-p ...)

(defun GENKB-template-p (thing)
  (and (listp thing)
       (symbolp (first thing))
       (not (null (first thing)))
       (listp (rest thing))
       (every #'GENKB-descriptor-p (rest thing)) ))



;;;;;;;;;;;   *****   G E N K B   T A B L E   *****
;;
;;  The global registry is implemented as a hash table indexed by AGENT-NAME.
;;  The initial size of the hash table is taken from the global constant
;;  *EXPECTED-NUMBER-OF-GENKB-TEMPLATES* defined in AMBR/INTRFACE/DEFS.LSP.
;;  The hash table itself is bound to the global variable *GENKB-TABLE*
;;  which is in the domain of the implementation.  Users interact with the
;;  table through the functions of the external protocol: GENKB-TEMPLATE,
;;  REGISTER-GENKB-TEMPLATE, FIND-GENKB-TEMPLATE, and DO-ALL-GENKB-TEMPLATES.

(defvar  *GENKB-table*     ; not advertized in the external protocol
         (make-hash-table  :size  *expected-number-of-GENKB-templates*
                           :rehash-size 1.5
                           :test  #'eq )   ; keys are agent-names, i.e. symbols
         "Hash table of all GENKB templates, indexed by agent-names." )

(defvar *GENKB-heralds* nil             ; advertized in the external protocol
  "List of the herald strings of all loaded GENKB templates." )


(defun remove-all-GENKB-templates ()
  "Clears the GENKB table."
  (clrhash *GENKB-table*)
  nil)

(defun find-GENKB-template (agent-name)
  "Retrieve the GENKB template, if any, for the agent with the given name."
  (declare (values (or list null)))
  (assert (and (symbolp agent-name) (not (null agent-name))))
  (values (gethash agent-name *GENKB-table*)) )


(defun  register-GENKB-template (template)
  "Store TEMPLATE in the GENKB table.  Combine with old templates if needed."
  (declare (type list template)  ; template ::= ( agent-name {descriptor}* )
           (values list) )       ; the new or updated template; NIL on errors
  (if (not (GENKB-template-p template))
      (cerror "Ignore it and continue."
              "REGISTER-GENKB-TEMPLATE: Invalid template ~S."
              template)
      (let* ((agent-name (first template))
             (old-template (find-GENKB-template agent-name)) )
        (when (null old-template)
          (setq old-template (cons agent-name nil)))    ; empty template
        (setf (gethash agent-name *GENKB-table*)
              (register-GENKB-template-aux template old-template)) )))

(defun register-GENKB-template-aux (new-template old-template)
  (declare (type list new-template old-template)
           (values list) )         ; combined template
  #+:DUAL-DEBUG (assert (eq (first new-template) (first old-template)))
  (let ((agent-name (first old-template))
        (descriptors (rest old-template)) )       ; start with OLD, adjoin NEW
    (dolist (new-descriptor (rest new-template))  ; skip agent-name
      ;; descriptor ::= (link-label {subdescriptor}*)
      (let* ((link-label (first new-descriptor))
             (old-descriptor (find link-label descriptors :key #'first)) )
        (when (null old-descriptor)
          (setq old-descriptor (cons link-label nil))  ; empty descriptor
          (push old-descriptor descriptors) )
        (setf (rest old-descriptor)                    ; update destructively
              (combine-GENKB-descriptors link-label
                                         (rest old-descriptor)   ; subdescr's
                                         (rest new-descriptor))) ))
    (cons agent-name descriptors) ))  ; destructively modified


;; COMBINE-GENKB-DESCRIPTORS is a generic function that combines SUBdescriptors
;; depending on the specific LINK-LABEL.  It is called by REGISTER-GENKB-...
;; (even when there is no old template to combine with) to ensure that all
;; subdescriptors pass the label-specific consistency checks.
;; Only default methods are defined here, the label-specific ones are below.

(defmethod  combine-GENKB-descriptors ((label symbol)
                                       (old-descriptors list)    ; actually
                                       (new-descriptors list) )  ; SUBdescr's
  (cerror "Combine using APPEND and continue."
          "COMBINE-GENKB-DESCRIPTORS has no method for this link label: ~S"
          label)
  (append old-descriptors new-descriptors) )

(defmethod  combine-GENKB-descriptors ((label t) (old-desc t) (new-desc t))
  (cerror "Replace with an empty template and continue."
          "Bad argument(s) for COMBINE-GENKB-DESCRIPTORS: ~S, ~S and ~S."
          label old-desc new-desc)
  nil)  ; instead of combined subdescriptor-list


;; GENKB-TEMPLATE is the main utility intended to appear in KB source files.
;; Note that it is a function, not a macro.
;; Syntax:  (GENKB-TEMPLATE
;;            [:HERALD    herald-string]
;;            [:TEMPLATES '( {template}* )] )

(defun  GENKB-template (&key (herald (required-argument))
                             (templates nil))
  "Register a template for later use by GENERATE-KB."
  (declare (type string herald)    ; e.g. "Template for base sit ABC, ver.1.0."
           (type list templates))
  (push herald *GENKB-heralds*)    ; see AMBR/INTRFACE/DEFS.LSP
  (dolist (template templates)
    (register-GENKB-template template))
  herald )


;;;;;;;;;   Iteration across all GENKB templates
;;
;;  Compare with DO-ALL-AGENTS in DUAL/ARCHIT/BASIC.LSP.

(defmacro do-all-GENKB-templates  (header &body body)
  "Iterates over all registered GENKB templates. Similar to DOLIST."
  (unless (and (listp header)
               (<= 1 (length header) 2)
               (symbolp (first header)) )    ; var-name
    (error "Malformed header in ~S."
           (list* 'do-all-GENKB-templates header body) ))
  (let ((var-name (first header))
        (result-form (second header))   ; possibly NIL for one-element headers
        (dummy-arg (gensym "DUMMY")) )
    `(block nil
       (maphash #'(lambda (,dummy-arg ,var-name)
                    (declare (ignore ,dummy-arg))
                    ,@body)
                *GENKB-table* )
       ,(DUAL-core::DOLIST-finalization var-name          ; see DUAL/ARCHIT/
                                        result-form)) ))  ;          /BASIC.LSP

#+:ACLPC
(eval-when (load)      ; provide a more readable message for the status line
  (when allegro:*save-lambda-lists*
    (setf (get 'do-all-GENKB-templates 'allegro:lambda-list)
          '((var &optional result) &rest body)) ))



;;;;;;;;;;;   ****  PROCESSING  G E N K B  TEMPLATES  ****
;;
;; GENKB->LINKS does the main work.  It produces a list to be spliced into a
;; WITH-AGENT body -- note the use of MAPCAN in PROCESS-GENKB-TEMPLATE below.
;; This list must always consist of alternating link-labels and fillers in
;; order to conform to the syntax of the G-slot section of WITH-AGENT -- see
;; DUAL/INTRFACE/WITH_AG and =/DEFAGENT.LSP.
;;
;; GENKB->LINKS is generic and uses EQL specializes on LINK-LABEL. This design
;; allows for incremental support for new link labels in GENKB templates.
;; At present, two kinds of labels are supported -- INSTANCE and A-LINK.
;; The respective methods are defined in separate subsections below.

(defmethod  GENKB->links  ((link-label T) (descriptors T))
  (warn "Bad argument(s) for GENKB->LINKS: ~S and ~S. Ignoring this template."
        link-label descriptors)
  nil)   ; return an empty list which will be ignored by PROCESS-GENKB-TEMPLATE


;; PROCESS-GENKB-TEMPLATE transforms templates into WITH-AGENT forms.
;; See DUAL/INTRFACE/WITH_AG.LSP for documentation of WITH-AGENT.

(defun process-GENKB-template (template)
  "Compose a WITH-AGENT form based on a GENKB template."
  (declare (type list template))  ; template ::= ( agent-name {descriptor}* )
  (list* 'with-agent                            ; compose a WITH-AGENT form
         (first template)                       ; agent-name
         (mapcaN #'(lambda (descriptor)
                     ;; descriptor ::= ( link-label {subdescriptor}* )
                     (GENKB->links (first descriptor)
                                   (rest  descriptor)))
                 (rest template)) ))            ; descriptors from the template


;;;;;;;;  Support for INSTANCE links
;;
;;  The variable *INSTANCE-link-GENKB-pattern* in AMBR/INTRFACE/DEFS.LSP stores
;;  a list of float numbers supplying the weights of the new INSTANCE links.
;;  For example, the pattern  (0.4 0.3 0.2 0.1)  means that four links will
;;  be constructed -- the first with weight 0.4, the second with 0.3, etc.

;; INSTANCE-descriptor    ::= ( :INSTANCE {INSTANCE-subdescriptor}* )
;; INSTANCE-subdescriptor ::= (agent-name frequency)
;; frequency              ::= <satisfies INTEGERP>  ; positive

(defmethod GENKB->links ((label (eql :INSTANCE))
                         (descriptors list))
  (let ((total-frequency 0))
    (dolist (desc descriptors)      ; sum up TOTAL-FREQUENCY
      (incf total-frequency (second desc)))
    (if (not (plusp total-frequency))
        nil                         ; empty DESCRIPTORS or negative freqencies
        (let ((link-list nil))
          ;; Accumulate a random sample with replacement in LINK-LIST.
          (dolist (weight *INSTANCE-link-GENKB-pattern*)
            (let* ((random-descriptor (sample descriptors total-frequency))
                   (curr-agent (first random-descriptor))
                   (old-pair (find curr-agent link-list :key #'first)) )
              (if (null old-pair)
                  (push (list curr-agent weight) link-list)
                  (incf (second old-pair) weight)) ))    ; re-sampled agent
          ;; To be spliced into the body of a WITH-AGENT form.
          (list :INSTANCE (nreverse link-list)) ))))


(defmethod  combine-GENKB-descriptors ((label (eql :INSTANCE))
                                       (old-descriptors list)    ; actually
                                       (new-descriptors list) )  ; SUBdescr's
  (flet ((valid-INSTANCE-descriptor-p (descriptor)
           (and (GENKB-subdescriptor-p descriptor)
                (plusp (second descriptor)) ))
         (destructively-combine-descriptors (old-d new-d)
           (incf (second old-d)      ; <--- simple summation of frequencies
                 (second new-d))) )  ; e.g. (cup 3) + (cup 2) --> (cup 5)
    (dolist (new-desc new-descriptors)
      (if (valid-INSTANCE-descriptor-p new-desc)
          (let ((old-desc (find (first new-desc)
                                old-descriptors :key #'first)))
            (if (null old-desc)
                (push new-desc old-descriptors)
                (destructively-combine-descriptors old-desc new-desc)))
          (cerror "Ignore it and continue."
                  "GENKB-DESCRIPTOR: Invalid INSTANCE descriptor ~S."
                  new-desc) ))
    old-descriptors ))    ; destructively modified


;;;;;;;;  Support for A-LINKs
;;
;;  All A-LINKs generated here have the same weight given by the variable
;;  *A-LINK-GENKB-weight* defined in AMBR/INTRFACE/DEFS.LSP.
;;  Each weight is created independently from the others.  The template
;;  defines the probability for each individual link.

;; A-LINK-descriptor    ::= ( :A-LINK   {A-LINK-subdescriptor}*   )
;; A-LINK-subdescriptor ::= (agent-name probability)
;; probability          ::= <satisfies FLOATP>    ; between 0.0 and 1.0

(defmethod GENKB->links ((label (eql :A-LINK))
                         (descriptors list))
  (let ((link-list nil))
    (dolist (desc descriptors)
      (when (biased-coin-p (second desc))    ; probability of success
        (push (list (first desc) *A-LINK-GENKB-weight*)
              link-list) ))
    (if (null link-list)
        nil
        (list :A-LINK link-list) )))         ; to be spliced into WITH-AGENT


(defmethod  combine-GENKB-descriptors ((label (eql :A-LINK))
                                       (old-descriptors list)    ; actually
                                       (new-descriptors list) )  ; SUBdescr's
  (labels ((valid-A-LINK-descriptor-p (descriptor)
             (and (GENKB-subdescriptor-p descriptor)
                  (<= 0.0 (second descriptor) 1.0)) )    ; probability
           (combine-probabilities (old-P new-P)
             (- (+ old-P new-P)        ; <--- probabilistic OR
                (* old-P new-P)) )     ; P = 1 - q1*q2 = 1 - (1-p1)*(1-p2) = ...
           (destructively-combine-descriptors (old-desc new-desc)
             (setf (second old-desc)
                   (combine-probabilities (second old-desc)
                                          (second new-desc))) ))
    (dolist (new-desc new-descriptors)
      (if (valid-A-LINK-descriptor-p new-desc)
          (let ((old-desc (find (first new-desc)
                                old-descriptors :key #'first)))
            (if (null old-desc)
                (push new-desc old-descriptors)
                (destructively-combine-descriptors old-desc new-desc)))
          (cerror "Ignore it and continue."
                  "GENKB-DESCRIPTOR: Invalid A-LINK descriptor ~S."
                  new-desc) ))
    old-descriptors ))    ; destructively modified



;;;;;;;;;;;   ****  GENERATING  G E N K B  FILES  ****
;;

(defun  print-GENKB-herald (stream)
  "Prints a number of comments related to KB generation."
  (declare (type stream stream))
  (format stream "~%;; This file has been generated automatically." )
  (format stream "~%;; It is a random sample of concept-to-instance links." )
  (format stream "~%;; See AMBR/INTRFACE/GENKB.LSP for details." )
  (format stream "~%~%;; Description of current GENKB parameters:" )
  (dolist (string (reverse *GENKB-parameter-descriptions*))   ; INTRFACE/DEFS
    (format stream "~%;;  ~A" string))
  (format stream "~%~%;; Heralds of all GENKB templates underlying this file:")
  (dolist (herald (reverse *GENKB-heralds*))
    (format stream "~%;;  ~A" herald))
  (format stream "~%~%" )
  (force-output stream )
  (values) )


(defun generate-KB (destination package-name
                    &key (herald-p T) (require-name nil) (verbose-p T))
  "Write a series of WITH-AGENT forms to DESTINATION."
  (declare (type (or string stream (member t eval :eval)) destination)
           (type (or string null) require-name) )
  (cond ((member destination '(eval :eval))         ; evaluate immediately
           (generate-KB-immediately verbose-p) )
        ((eq destination t)                         ; use *STANDARD-OUTPUT*
           (generate-KB *standard-output* package-name
                        :require-name require-name
                        :herald-p herald-p  :verbose-p NIL) )
        ((stringp destination)                      ; file name
           (let (count)
             (with-open-file (stream destination :direction :output)
               (when verbose-p
                 (format t "~%;; Opening GENKB file ~S..." destination ))
               (setq count (generate-KB stream package-name
                                 :require-name require-name
                                 :herald-p herald-p :verbose-p verbose-p)) )
             (when verbose-p
               (format t "~%;; Closing GENKB file ~S.~%" destination ))
             count ))
        ((streamp destination)                      ; destination stream itself
           (generate-KB-aux destination package-name  ; <--- real work
                            herald-p require-name verbose-p) )
        (t (error "The first argument to GENERATE-KB must be one of:~@
                   stream, filename-string, T, EVAL, or :EVAL.")) ))


(defun generate-KB-aux (stream package-name herald-p require-name verbose-p)
  (declare (type stream stream)
           (type string package-name)
           (type (or string null) require-name) )
  (let ((package (find-package package-name)))
    (when (null package)
      (cerror "Use the current package and continue."
              "GENERATE-KB: Cannot find package named ~S."
              package-name)
      (setq package *package*) 
      (setq package-name (package-name *package*)) )
    (let ((*package* package)         ; avoid symbol qualifiers in the file
          (*print-case* :downcase)
          (*print-level* nil)
          (*print-length* nil)
          (count 0) )
      (when herald-p
        (print-GENKB-herald stream))
      (format stream "~%(cl:in-package ~S)~%~%" package-name )
      (when (stringp require-name)
        (format stream "(require ~S)~%~%" require-name ))
      (do-all-GENKB-templates (template)        ; <--- main work
        (incf count)
        (pprint-WITH-AGENT (process-GENKB-template template)
                           stream) )            ; see DUAL/INTRFACE/WITH_AG.LSP
      (format stream "~%~%(check-for-unresolved-references)" )
      (when herald-p
        (format stream "~%~%;; There are ~D WITH-AGENT forms in this file.~%"
                       count))
      (when verbose-p
        (format t "~%;;  A total of ~D WITH-AGENT forms generated."
                  count))
    count )))  ; GENERATE-KB-AUX


(defun  generate-KB-immediately (&optional (verbose-p T))
  "Generate WITH-AGENT forms and execute them immediately."
  (let ((count 0))
    (do-all-GENKB-templates (template)
      (incf count)
      (eval (process-GENKB-template template)) )
    (when verbose-p
      (format t "~%;; A total of ~D WITH-AGENT forms generated and executed."
                count))
    (check-for-unresolved-references) ))


;;;;;;;;;;
;;
;; Sample usage of GENERATE-KB:
;;
;; (dotimes (number 100)
;;   (generate-KB 
;;      (format nil "D:\\DUAL\\AMBR\\KB\\GENKB\\genkb~3,'0D.lsp" number) 
;;      "AMBR"
;;      :require-name "ltm") )
;;


;;;;;;  End of file  AMBR/INTRFACE/GENKB.LSP
