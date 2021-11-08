;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/readrfilr.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Reading symbolic and connectionist references
;;; DEPENDS-ON: DUAL/proclaim.lsp, DUAL/labels.lsp, DUAL/archit/symref.lsp,
;;;             DUAL/archit/conref.lsp, DUAL/intrface/dummy_ag.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    04-02-98 [1.1]   Was DUAL/INTRFACE/READREF.LSP
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;     READING FILLERS OF VARIOUS TYPES     ;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-INTERFACE")

;;;; This file defines the function READ-FILLER which is used by the macros
;;;; DEFAGENT and WITH-AGENT (see DUAL/INTRFACE/DEFAGENT and =/WITH_AG.LSP).
;;;;
;;;; READ-FILLER takes a 'raw filler' and generates a LISP form to be
;;;; incorporated in the expansion of the aforementioned macros.
;;;;
;;;; See DUAL/ARCHIT/FILLER.LSP for detailed description of filler types.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: read-filler, default-weight
;;

;; READ-FILLER  (G-label raw-filler)  -->  LISP-form
;;
;;   ....

;; DEFAULT-WEIGHT (link-label)  -->  default-weight   (or cerror)
;;
;;   ....


;;;;;;;
;;
;; Stand-alone symbolic references are transformed into connectionist ref's
;; by WEIGHT DEFAULTING. For instance, the G-slot definition
;;    :SUBC binary-relation        is converted to
;;    :SUBC (binary-relation 1.0)  because the DEFAGENT machinery 'knows' that
;;                                 the default weight of a :SUBC link is 1.0.
;; See PROCLAIM-LINK in DUAL/PROCLAIM.LSP and DUAL/LABELS.LSP.
;;
;; One more example: The G-slot definition
;;    :SUBC (binary-relation (attribute . :slot1))              is converted to
;;    :SUBC ((binary-relation (default-weight :subc))
;;           ((attribute . :slot1) (default-weight :subc)))     and finally to
;;
;;    (make-G-slot :SUBC
;;       :filler (list
;;         (make-conref (make-symref (find-or-make-agent 'binary-relation))
;;                      (default-weight :subc))
;;         (make-conref (make-symref (find-or-make-agent 'attribute)
;;                                   :slot1)
;;                      (default-weight :subc)) ))
;;
;;
;; FIND-OR-MAKE agent is defined in DUAL/INTRFACE/DUMMY_AG.LSP and is an
;; extended version of FIND-AGENT (defined in DUAL/ARCHIT/BASIC.LSP) that
;; handles forward-referenced agents.
;;
;;;;;;;
;;
;; See also:
;;   G-slot-label-p, S-slot-label-p, slot-label-p   defined in DUAL/PROCLAIM.LSP
;;   tag-p, DUAL-keyword-p                          defined in DUAL/PROCLIAM.LSP
;;   symbolp, keywordp, numberp, stringp            defined by Common LISP
;;

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

;;;;;;;  Main function advertized in the external protocol

(defun  read-filler (G-label raw-filler)
  "Translates RAW-FILLER into LISP forms that compute the real filler."
  (multiple-value-bind (label-type label-type-aux)
                       (label-type G-label)
    #+:DUAL-DEBUG
      (unless (eq :G-slot label-type)
        (error "READ-FILER: ~S is not a G-slot label."  G-label))
    #-:DUAL-DEBUG
      (declare (ignore label-type))   ; assume it is :G-SLOT
    (ecase label-type-aux
      (:tag        (read-tag-list    raw-filler G-label))
      (:number     (read-number-list raw-filler G-label))
      (:reference  (read-conref-list raw-filler G-label)) )))


;;;;;;;  Reading fillers of type :TAG and :NUMBER
;;
;;  Such fillers must satisfy TAG-P or NUMBERP or be homogeneous list thereof.

(defun  read-tag-list (raw-filler G-label)
  ;; Assume G-LABEL is of type  (:G-SLOT :TAG).
  (cond ((tag-p raw-filler)
            (list 'quote raw-filler))
        ((and (listp raw-filler) (every #'tag-p raw-filler))
            (list 'quote raw-filler))
        ((and (listp raw-filler) (notevery #'tag-p raw-filler))
            (cerror "Continue compilation and handle the error at load time."
                    "~S is not a homogeneous list of tags as required by ~S."
                    raw-filler G-label)
            :error)
        (t  (cerror "Continue compilation and handle the error at load time."
                    "~S is neither a tag nor a list of tags as required by ~S."
                    raw-filler G-label)
            :error )))

(defun  read-number-list (raw-filler G-label)
  ;; Assume G-LABEL is of type  (:G-SLOT :NUMBER).
  (cond ((integerp raw-filler)
            raw-filler)
        ((and (listp raw-filler) (every #'integerp raw-filler))
            (list 'quote raw-filler))
        ((and (listp raw-filler) (notevery #'integerp raw-filler))
            (cerror "Continue compilation and handle the error at load time."
                  "~S is not a homogeneous list of integers as required by ~S."
                    raw-filler G-label)
            :error)
        (t  (cerror "Continue compilation and handle the error at load time."
                "~S is neither an integer nor a list thereof as required by ~S."
                raw-filler G-label)
            :error )))


;;;;;;;  Reading fillers of type :REFERENCE
;;
;; reference ::=  { symbolic-reference |
;;                  connectionist-reference }
;; symbolic-reference      ::=  { agent-name | (agent-name . slot-label) }
;; connectionist-reference ::=  { symbolic-reference |          ; default weight
;;                                (symbolic-reference weight) }
;; weight ::= <satisfies NUMBERP>


;;;; Reading symbolic references

(defun raw-symref-p (thing)
  "Predicate that checks if something can be transformed into a symref."
  (flet ((raw-agent-name-p (thing)
           (and (symbolp thing)                    ; e.g. WATER, COLOR-OF-65
                (not (DUAL-keyword-p thing))) ))   ; avoid tags and slot labels
    (or (raw-agent-name-p thing)              ; agent-name
        (and (consp thing)                    ; (agent-name . slot-label)
             (raw-agent-name-p (car thing))
             (slot-label-p     (cdr thing)))) ))

(defun read-symref (raw-symref)
  "Translates a raw symref into a call to MAKE-SYMREF."
  ;; Assume that RAW-SYMREF satisfies RAW-SYMREF-P.
  (etypecase raw-symref
    (symbol `(make-symref (find-or-make-agent ',raw-symref)))
    (cons   `(make-symref (find-or-make-agent ',(car raw-symref))
                          ',(cdr raw-symref))) ))


;;;; Reading connectionist references

(defun  raw-conref-p (thing)
  "Predicate that checks if something can be transformed into a conref."
  ;; (symref weight)  ==  (symref . (number . nil))
  (and (consp thing)
       (raw-symref-p (first thing))
       (consp (cdr thing))
       (numberp (second thing))
       (null (cddr thing))) )

(defun read-conref (raw-conref link-label)
  "Translates a raw conref into a call to MAKE-CONREF."
  #+:DUAL-DEBUG
    (unless (link-label-p link-label)
      (error "READ-CONREF: ~S is not a link label."  link-label ))
  (cond ((raw-symref-p raw-conref)         ; name  or  (name . slot)
            `(make-conref ,(read-symref raw-conref)
                          (default-weight ',link-label)) )
        ((raw-conref-p raw-conref)         ; (raw-symref weight)
            (list 'make-conref
                  (read-symref (first raw-conref))
                  (second raw-conref)) )
        (t  (error "~S cannot be converted into a connectionist reference."
                   raw-conref)) ))

(defun  read-conref-list (raw-filler link-label)
  "Translates a list of raw conrefs into a list of calls to MAKE-CONREF."
  ;; Assume LINK-LABEL is of type  (:G-SLOT :REFERENCE).
  (flet ((ref-p (thing)
           (or (raw-symref-p thing)
               (raw-conref-p thing)) ))
    (cond ((ref-p raw-filler)
             (read-conref raw-filler link-label) )
          ((and (listp raw-filler) (every #'ref-p raw-filler))
             (cons 'list (mapcar #'(lambda (raw-conref)
                                     (read-conref raw-conref link-label))
                                 raw-filler)) )
          ((and (listp raw-filler) (notevery #'ref-p raw-filler))
              (cerror "Continue compilation and handle the error at load time."
                "~S is not a homogeneous list of references as required by ~S."
                      raw-filler link-label)
              :error) 
          (t  (cerror "Continue compilation and handle the error at load time."
                "~S is neither a reference nor a list of refs as required by ~S."
                      raw-filler link-label)
              :error ))))


(defun default-weight (link-label)
  ;; See PROCLAIM-LINK and LINK-DEFAULT-WEIGHT in DUAL/PROCLAIM.LSP.
  (cond ((link-default-weight link-label))    ; if non-NIL, return it
        (t (cerror "Use zero weight and continue."
                   "DEFAULT-WEIGHT: Cannot determine the default weight of ~S."
                   link-label)
            0.0) ))


;;;;;;;  End of file DUAL/INTRFACE/READFILR.LSP
