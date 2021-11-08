;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/defagent.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    DEFAGENT, a macro interface to MAKE-DUAL-AGENT
;;; DEPENDS-ON: DUAL/labels.lsp, DUAL/archit/dual_ag.lsp,
;;;             DUAL/intrface/dummy_ag.lsp, DUAL/intrface/readfilr.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    24-04-97 [1.0]
;;; UPDATED:    04-02-98 [1.1.1] Re-write everything, support the full grammar.
;;; UPDATED:    04-06-98 [1.1.2] Add PPRINT-DEFAGENT and PPRINT-MICRO-FRAME.
;;;                              Fix the order of MAKE-FACET clauses in S-slots.
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;;;;    D E F A G E N T    M A C R O    ;;;;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-INTERFACE")

;;;; This file defines the DEFAGENT macro which is interface to the function
;;;; MAKE-DUAL-AGENT defined in DUAL/ARCHIT/DUAL_AG.LSP.
;;;; ...
;;;; Forward references and dummies ... (see DUAL/INTRFACE/DUMMY_AG.LSP).
;;;;


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: defagent, parse-defagent-body
;;
;;          agent->DEFAGENT, pprint-DEFAGENT, pprint-DEFAGENT-slots,
;;          pprint-micro-frame, mfr
;;

;; SYNTAX (in BNF):
;;
;; (DEFAGENT  agent-name  agent-type
;;   [documentation-string]
;;   { G-slot-description }*
;;   { S-slot-description }*
;; )
;;
;; agent-name ::= <satisfies SYMBOLP>
;; agent-type ::= <satisfies VALID-DUAL-AGENT-TYPE-P>  ; DUAL/ARCHIT/DUAL_AG.LSP
;; documentation-string ::=  <satisfies STRINGP>
;;
;; G-slot-description ::= G-slot-label filler
;;
;; S-slot-description ::= S-slot-label
;;                          [documentation-string]
;;                          { facet-description }*
;;
;; facet-description  ::= G-slot-label filler
;;
;; G-slot-label ::= <satisfies G-SLOT-LABEL-P>         ; see DUAL/PROCLAIM.LSP
;; S-slot-label ::= <satisfies S-SLOT-LABEL-P>         ; see DUAL/PROCLAIM.LSP
;;
;; filler    ::=  { NIL | number    | ({number}+)      ; see DUAL/INTRFACE/
;;                      | tag       | ({tag}+)                 ; /READFILR.LSP
;;                      | reference | ({reference}+) }
;;
;; tag       ::=  <satisfies TAG-P>                    ; see DUAL/PROCLAIM.LSP
;; number    ::=  <satisfies INTEGERP>
;; reference ::=  { symbolic-reference |
;;                  connectionist-reference }
;; symbolic-reference      ::=  { agent-name | (agent-name . slot-label) }
;; connectionist-reference ::=  { symbolic-reference |          ; default weight
;;                                (symbolic-reference weight) }
;; weight ::= <satisfies NUMBERP>
;;
;;;;;;;
;;
;; MACROEXPANSION  OF  DEFAGENT  is a call to MAKE-DUAL-AGENT-1 of the form:
;;
;;  `(make-DUAL-agent-1  ',agent-name ',agent-type
;;      :comment  ,doc-string
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
;; MAKE-DUAL-AGENT-1 is defined in DUAL/INTRFACE/DUMMY_AG.LSP and is an
;; extended version of MAKE-DUAL-AGENT (defined in DUAL/ARCHIT/DUAL_AG.LSP)
;; that handles forward-referenced agents.
;;
;;;;;;;
;;
;; Stand-alone symbolic references are transformed into connectionist ref's
;; by WEIGHT DEFAULTING.  See DUAL/INTRFACE/READFILR.LSP for details.
;;
;;;;;;;
;;
;; Limitations of the current implementation:
;;   -- No ways for user customization.
;;   -- Inadequate error diagnostics.

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;  The DEFAGENT macro itself
;;
;; (DEFAGENT  agent-name  agent-type
;;   [documentation-string]
;;   { G-slot-description }*
;;   { S-slot-description }*
;; )

(defmacro defagent (agent-name agent-type &rest initargs)
  "Converts human-readable agent definitions into a call to MAKE-DUAL-AGENT-1."
  (let ((comment (if (stringp (first initargs))
                     (pop initargs)
                     nil )))
    (multiple-value-bind (G-slots S-slots)
                         (parse-DEFAGENT-body agent-name initargs)
      `(make-DUAL-agent-1 ',agent-name ',agent-type
                          :comment ,comment
                          :G-slots (delete 'nil (list ,.G-slots))
                          :S-slots (delete 'nil (list ,.S-slots))) )))
                                         ;; If continued, CERROR returns NIL.


;;;;;  PARSE TABLES are demporary data structures used by the DEFAGENT parser.
;;

(eval-when (compile load eval)
  (defclass  parse-table  (Dual-interface-object)
    ((agent-name  :initarg     :agent-name
                  :reader      ptable-agent-name
                  :type        symbol
                  :initform    (required-argument) )
     (initargs    :initarg     :initargs
                  :accessor    ptable-initargs
                  :type        list
                  :initform    (required-argument) )
     (token       :accessor    ptable-token
                  :type        t
                  :initform    nil )
     (G-slots     :accessor    ptable-G-slots
                  :type        list          ; of  (label . make-form)  pairs
                  :initform    nil     )
     (S-slots     :accessor    ptable-S-slots
                  :type        list
                  :initform    nil     )
    )
    (:documentation "Temporary structures used by DEFAGENT and WITH-AGENT." )
  ) ; defclass
) ; eval-when


;;;;;  Functions dealing with parse tables.

(defun  make-parse-table (agent-name initargs)
  (declare (values parse-table))
  (make-instance 'parse-table  :agent-name agent-name
                               :initargs   initargs ))

(defun  next-token  (ptable)
  ;; Pops an INITARG, stores it as TOKEN, and returns it.
  (declare (type parse-table ptable))
  (setf (ptable-token ptable)
        (pop (ptable-initargs ptable))) )


(defun register-slot (ptable label make-form)
  ;; Puts a slot definition in PTABLE. Signals error on duplicates.
  (declare (type parse-table ptable)
           (type list make-form) )
  (cond ((G-slot-label-p label)
            (if (find label (ptable-G-slots ptable) :key #'car)
                (cerror "Ignore the second slot definition and continue."
                        "Slot ~S is defined twice in ~A."
                        label (ptable-agent-name ptable) )
                (push (cons label make-form)
                      (ptable-G-slots ptable)) ))
        ((S-slot-label-p label)
            (if (find label (ptable-S-slots ptable) :key #'car)
                (cerror "Ignore the second slot definition and continue."
                        "Slot ~S is defined twice in ~A."
                        label (ptable-agent-name ptable) )
                (push (cons label make-form)
                      (ptable-S-slots ptable)) ))
        (t  (error "DUAL-CORE::REGISTER-SLOT: ~S is not a slot label."
                   label)) ))


;;;;;  Determining the type of a token

(defun token-type (token)
  (cond  ((G-slot-label-p token)  :G-slot-label )
         ((S-slot-label-p token)  :S-slot-label )
         ((null token)            nil )
         (t                       :non-label ) ))


;;;;;  The DEFAGENT parser makes a parse-table to communicate b/n subfunctions.
;;

(defun parse-defagent-body (agent-name initargs)
  (declare (type symbol agent-name)
           (type list initargs)
           (values list list) )    ; (values G-slots S-slots)
  (let ((ptable (make-parse-table agent-name initargs)))
    ;; Loop through G-slot definitions
    (loop
      (case (token-type (next-token ptable))        ; pop a token and dispatch
        ((nil :S-slot-label) (return))              ; go to S-slots
        (:G-slot-label       (read-G-slot ptable))  ; read this G-slot and loop
        (t  (return) )))                            ; user-customizable initarg ?
    ;; Loop through S-slot definitions
    (loop
      (case (token-type (ptable-token ptable))
        (:S-slot-label (read-S-slot ptable))        ; read this S-slot and loop
        (t  (return)) ))
    ;; Check for unrecognized initargs  (user customization may be added later)
    (unless (null (ptable-token ptable))            ; any more tokens ?
       (cerror "Ignore extra initargs and continue."
               "PARSE-DEFAGENT-BODY: Extra initargs ~S in the body of ~S."
               (cons (ptable-token ptable) (ptable-initargs ptable))
               (ptable-agent-name ptable) ))
    ;; G- and S-slot definitions have been accumulated in PTABLE
    (values (mapcar #'cdr (nreverse (ptable-G-slots ptable)))
            (mapcar #'cdr (nreverse (ptable-S-slots ptable)))) ))


;;;;  Reading G-slots
;;
;; G-slot-description ::= G-slot-label filler

(defun read-G-slot (ptable)
  "Translates a raw G-slot definition into a call to MAKE-G-SLOT."
  ;; The current token in PTABLE is a G-slot-label.
  (declare (type parse-table ptable))
  (let* ((G-label    (ptable-token ptable))      ; memorize old token
         (raw-filler (next-token   ptable))      ; pop a new token
         (filler     (read-filler G-label        ; check it and supply defaults
                                  raw-filler))
         (make-form  (if (eq filler :error)
                         `(cerror "Ignore this slot and continue."
                            "~S is not a valid slot filler for ~S in ~S."
                            ',raw-filler ',G-label ',(ptable-agent-name ptable))
                         `(make-G-slot ,G-label :filler ,filler)) ))
    (register-slot ptable
                   G-label
                   make-form) ))


;;;;  Reading S-slots
;;
;; S-slot-description ::= S-slot-label [documentation-string]
;;                          { facet-description }*

(defun read-S-slot (ptable)
  "Translates a raw S-slot definition into a call to MAKE-S-SLOT."
  ;; The current token in PTABLE is a S-slot-label.
  (declare (type parse-table ptable))
  (let* ((S-label (ptable-token ptable))
         (comment (if (stringp (first (ptable-initargs ptable)))
                      (next-token ptable)
                      nil))
         (facet-defs  nil)
         (final-facet-defs nil) )
    ;; Collect facet definitions
    (loop
      (case (token-type (next-token ptable))         ; pop a token and dispatch
        ((nil :S-slot-label) (return))               ; terminate
        (:G-slot-label                               ; register facet and loop
            (let* ((facet-label (ptable-token ptable))
                   (facet-def   (read-facet   ptable)) )
              (if (find facet-label facet-defs :key #'car)
                  (cerror "Ignore the second facet definition and continue."
                          "Facet ~S is defined twice in ~S in agent ~S."
                          facet-label S-label (ptable-agent-name ptable) )
                  (push (cons facet-label facet-def)
                        facet-defs) )))
        (t  (return) )))                             ; customizable initarg ?
    (dolist (cons-cell facet-defs)
      (unless (null (cdr cons-cell))                 ; returned by CERROR
        (push (cdr cons-cell) final-facet-defs)))
    ;; Construct and register S-slot definition
    (register-slot ptable
              S-label
              `(make-S-slot ,S-label
                     :comment ,comment
                     :facets  (list ,.final-facet-defs))) ))


;;;;  Reading facets
;;
;; facet-description  ::= G-slot-label filler

(defun read-facet (ptable)
  "Translates a raw facet definition into a call to MAKE-FACET."
  ;; The current token in PTABLE is a G-slot-label.
  (declare (type parse-table ptable)
           (values list) )   ; a MAKE-FACET form
  (let* ((G-label    (ptable-token ptable))      ; memorize old token
         (raw-filler (next-token   ptable))      ; pop a new token
         (filler     (read-filler G-label        ; check it and supply defaults
                                  raw-filler)) )
    (if (eq filler :error)
        `(cerror "Ignore this facet and continue."
                 "~S is not a valid facet filler for ~S in ~S."
                 ',raw-filler ',G-label ',(ptable-agent-name ptable))
        `(make-facet ,G-label :filler ,filler) )))


;;;;  The function READ-FILLER is defined in DUAL/INTRFACE/READFILR.LSP.


;;;  File transducer (for debugging purposes)
#|
(defun macroexpand-DEFAGENT (in-name out-name &optional (verbose-p t))
  (let ((*print-level* nil) 
        (*print-length* nil) 
        (*print-pretty* nil)
        (eof-marker (cons :eof nil)) )
     (with-open-file (in in-name :direction :input)
       (with-open-file (out out-name :direction :output)
         (do ((form (read in nil eof-marker) (read in nil eof-marker)))
             ((eq form eof-marker) :done)
            (if (eq 'defagent (first form))
                (progn (print (macroexpand-1 form) out)
                       (when verbose-p (format t "~A " (second form))))
                (print form out)) )))))
|#


;;;;;;;  Generating a DEFAGENT form based on an agent

(defun  agent->DEFAGENT (agent)
  "Generate a DEFAGENT form based on AGENT."
  (declare (type micro-frame agent)
           (values list) )
  (nconc
    (list 'DEFAGENT
          (agent-name agent)
          (class-name (class-of agent)))
    (if (agent-comment agent)
        (list (agent-comment agent))
        nil)
    (mapcan #'fholder->DEFAGENT-clause
            (agent-G-slots agent))
    (mapcan #'S-slot->DEFAGENT-clause
            (agent-S-slots agent))
  ))

(defun  fholder->DEFAGENT-clause (fholder)
  "Used by AGENT->DEFAGENT when generating ASCII representations of agents."
  (declare (type link fholder)    ; G-slot or facet, see DUAL/ARCHIT/FILLER.LSP
           (values list) )
  (flet ((handle-conref (conref)
           (let ((symref (conref-reference conref))
                 (weight (conref-weight conref)) )
             (if (simple-symref-p symref)
                 (list (agent-name symref) 
                       weight)
                 (list (cons (agent-name (symref-agent symref))
                             (symref-slot symref))
                       weight))) ))
    (let ((slot-label  (slot-label fholder))
          (filler      (fholder-filler fholder)) )
      (ecase (fholder-filler-type fholder)
        ((nil)               (list slot-label nil))
        (:list-of-tags       (list slot-label filler))
        (:list-of-numbers    (list slot-label filler))
        (:list-of-references (list slot-label (mapcar #'handle-conref filler)))
        (:malformed-filler   (list slot-label "!!! MALFORMED FILLER !!!")) ))))

(defun  S-slot->DEFAGENT-clause (S-slot)
  "Used by AGENT->DEFAGENT when generating ASCII representations of agents."
  (declare (type S-slot S-slot)
           (values list) )
  (nconc
    (list (slot-label S-slot))
    (if (S-slot-comment S-slot)
        (list (S-slot-comment S-slot))
        nil)
    (mapcan #'fholder->DEFAGENT-clause
            (S-slot-facets S-slot))
  ))


;;;;;;;  Writing DEFAGENT forms to character streams

(defun  pprint-DEFAGENT-slots (DEFAGENT-slots
                               &optional (stream *standard-output*))
  (declare (type list DEFAGENT-slots)
           (type stream stream) )
  (let ((*print-case* :downcase)
        (*print-level* nil)
        (*print-length* nil)
        (slot-label nil) )
   (labels ((abbreviated-list-p (filler)
              (or (numberp filler)
                  (and (symbolp filler)            ; tag  or  agent-name
                       (not (null filler)))
                  (and (consp filler)              ; (agent-name . :slot1)
                       (symbolp (car filler))
                       (slot-label-p (cdr filler)))
                  (and (consp filler)              ; (symref weight)
                       (not (numberp (first filler)))
                       (numberp (second filler)))
                  (stringp filler) ))              ; "!!! malformed filler !!!"
           (write-G-slot (slot-label filler)
             (when (abbreviated-list-p filler)
               (setq filler (list filler)))
             (format stream "~%  ~10S " slot-label)  ; two leading spaces
             (cond ((every #'tag-p filler)
                      (format stream "~S" filler))   ; print on one line
                   ((every #'numberp filler)
                      (format stream "~S" filler))   ; print on one line
                   (t ; else FILLER is a non-empty list of references
                      (format stream "~:[(~S~{~%~14T~S~} )~;~S~*~]"
                              (null (rest filler))   ; print on successive lines
                              (first filler)
                              (rest  filler))) ))
           (write-facet (slot-label filler)
             (when (abbreviated-list-p filler)
               (setq filler (list filler)))
             (format stream "~%    ~10S " slot-label) ;four leading spaces
             (cond ((every #'tag-p filler)
                      (format stream "~S" filler))   ; print on one line
                   ((every #'numberp filler)
                      (format stream "~S" filler))   ; print on one line
                   (t ; else FILLER is a non-empty list of references
                      (format stream "~:[(~S~{~%~16T~S~} )~;~S~*~]"
                              (null (rest filler))   ; print on successive lines
                              (first filler)
                              (rest  filler))) ))
           (write-S-slot ()   ; uses and modifies outer SLOT-LABEL
             (format stream "~%  ~8S " slot-label)   ; two leading spaces
             (when (stringp (first DEFAGENT-slots))  ; S-slot comment ?
               (format stream "~S" (pop DEFAGENT-slots)))
             (loop   ; facets
               (setq slot-label (pop DEFAGENT-slots))
               (cond ((null slot-label)            (return))   ; all slots done
                     ((S-slot-label-p slot-label)  (return))   ; this slot done
                     ((G-slot-label-p slot-label)
                        (write-facet slot-label
                                     (pop DEFAGENT-slots)))    ; filler
                     (t (format stream "~%;;Unrecognized facet label -- ~S."
                                       slot-label)) ))) )
      ;; Main body of PPRINT-DEFAGENT-SLOTS
      (loop   ; G-slots
        (setq slot-label (pop DEFAGENT-slots))
        (cond ((null slot-label)            (return))   ; proceed with S-slots
              ((S-slot-label-p slot-label)  (return))   ; proceed with S-slots
              ((G-slot-label-p slot-label)
                 (write-G-slot slot-label
                               (pop DEFAGENT-slots)))   ; filler
              (t (format stream "~%;;Unrecognized G-slot label -- ~S."
                                slot-label)) ))
      (loop   ; S-slots
        (if (S-slot-label-p slot-label)
            (write-S-slot)              ; and loop
            (return) ))                 ; SLOT-LABEL should be NIL -- terminate
      (values) )))  ; end of PPRINT-DEFAGENT-SLOTS


(defun  pprint-DEFAGENT (DEFAGENT-form
                         &optional (stream *standard-output*))
  "Pretty-print a DEFAGENT form."
  (declare (type list DEFAGENT-form)
           (type stream stream) )
  #+:DUAL-DEBUG (assert (eq 'defagent (first DEFAGENT-form)))
  (let ((*print-case* :downcase)
        (*print-level* nil)
        (*print-length* nil)
        (slots (rest (rest (rest DEFAGENT-form)))) )
    (format stream "~%(defagent  ~S   ~S"
                   (second DEFAGENT-form)      ; agent-name
                   (third  DEFAGENT-form) )    ; agent type
    (when (stringp (first slots))              ; comment ?
      (format stream "~%  ~S" (pop slots)))
    (pprint-DEFAGENT-slots slots stream)       ; the main work
    (format stream "~%)~%" )
    (values) ))


;;;;;;;  Pretty-printing micro-agents
;;
;; PPRINT-MICRO-FRAME is a superposition of PPRINT-DEFAGENT and AGENT->DEFAGENT.
;; This straightforward approach generates much garbage for DEFAGENT forms.
;; Future versions may redefine PPRINT-MICRO-FRAME to do printing itself.

(defun pprint-micro-frame (agent
                           &optional (stream *standard-output*))
  "Pretty-prints a DEFAGENT form describing AGENT's micro-frame."
  (declare (type micro-frame agent)       ; see DUAL/ARCHIT/MCRFRAME.LSP
           (values) )
  (pprint-DEFAGENT (agent->DEFAGENT agent)
                   stream ))


;;  MFR is a shorthand for PPRINT-MICRO-FRAME

(defun  mfr (agent &optional (stream *standard-output*))
  "A shorthand for PPRINT-MICRO-FRAME"
  (pprint-micro-frame agent stream) )


;;;;;;;  End of file DUAL/INTRFACE/DEFAGENT.LSP
