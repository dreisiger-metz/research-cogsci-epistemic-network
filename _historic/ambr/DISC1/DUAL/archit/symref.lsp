;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/symref.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Support for symbolic DUAL references (symref's).
;;; DEPENDS-ON: DUAL/archit/basic.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    15-03-97 [1.0]
;;; UPDATED:    09-11-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;         S Y M B O L I C      R E F E R E N C E S          ;;;;;;
  ;;;;;;             /*  agent  or  (agent.slot)  */               ;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is SYMREF ('symbolic reference').
;;;; The file defines functions for constructing and dealing with symrefs.
;;;;
;;;; A symref is a reference to a DUAL agent or one of its slots.
;;;; (See section 2.2.3.3. of "DUAL Report #1".)
;;;; There are two types of symrefs -- SIMPLE-SYMREF and EXTENDED-SYMREF.
;;;; The former are simply (references to) agents. Two simple symrefs are
;;;; 'the same' if they satisfy EQ.
;;;; Extended symrefs have the form: (agent.slot). They are 'the same' if
;;;; their corresponding fields satisfy EQ.
;;;; The SLOT field of an extended symref should satisfy SLOT-LABEL-P
;;;; (defined in DUAL/PROCLAIM.LSP and DUAL/LABELS.LSP).
;;;;
;;;; Examples:   CHAIR17          is a simple symbolic reference
;;;;             CHAIR17.SLOT1    is an extended symbolic reference
;;;;
;;;; IMPORTANT NOTE: Simple symrefs should be indistinguishable from agents.
;;;; In other words, it should always hold that:
;;;;    (eq agent (make-symref agent))                 -->  T
;;;;    (if (agentp thing) (simple-symref-p thing) t)  -->  T
;;;;    (if (simple-symref-p thing) (agentp thing) t)  -->  T
;;;; This is necessary because some (connectionist) modules of the program
;;;; does not have the notion 'symref' and work with 'base agents' only.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: make-symref, symref-agent, symref-slot, symref->string,
;;          simple-symref-p, extended-symref-p, symref-p,
;;          symref-match, symref-equal, simplify-symref

;; MAKE-SYMREF (agent &optional slot)  -->  symref
;;
;;   A function that constructs a symref.
;;   AGENT should be an agent. If not, an error is signalled.
;;   SLOT is optional. If not specified, the result is a simple symref.
;;   If SLOT is specified, an extended symref is constructed.
;;   If SLOT is specified but does not satisfy SLOT-LABEL-P an error is
;;   signaled. (The type predicate SLOT-LABEL-P is defined in DUAL/PROCLAIM.LSP)
;;   It is not checked whether AGENT really possesses such SLOT.
;;
;;   After a successful call to MAKE-SYMREF, the following conditions hold:
;;     (or (simple-symref-p symref)
;;         (extended-symref-p symref))        -->  T
;;     (symref-agent symref)                  -->  agent
;;     (symref-slot  symref)                  -->  slot  or  NIL
;;
;;  It is always true that for each agent AGENT (not only 'base agents'):
;;     (eq agent (make-symref agent))  -->  T   ; simple symrefs are agents


;; SYMREF-AGENT (symref)  -->  agent
;;
;;   A function that returns the AGENT field of a symref.
;;   SYMREF may be simple or extended.
;;   If SYMREF is not a symref, and error is signaled.

;; SYMREF-SLOT (symref)  -->  slot-label  or  NIL
;;
;;   A function that returns the SLOT field of an symref.
;;   If SYMREF is an extended symref, its SLOT field is returned.
;;   If SYMREF is a simple symref, NIL is returned.
;;   If SYMREF is not a symref at all, an error is signaled.


;; SIMPLE-SYMREF-P   (object)  -->  T or NIL
;;
;;   A function that checks whether a given object is a simple symbolic ref.
;;   (Note also that simple symrefs are undistinguishable from bare agents.)
;;
;;   It is always true that:
;;      (simple-symref-p (make-symref agent))           -->  T
;;      (simple-symref-p (make-symref agent 'slot1))    -->  NIL
;;      (simple-symref-p (make-symref agent :slot1))    -->  NIL
;;      (simple-symref-p nil)                           -->  NIL
;;      (simple-symref-p 'agent-name)                   -->  NIL
;;   and also:
;;      (if (agent-p thing) (simple-symref-p thing) t)  -->  T
;;      (if (simple-symref-p thing) (agent-p thing) t)  -->  T

;; EXTENDED-SYMREF-P (object)  -->  T or NIL
;;
;;   A function that checks whether a given object is an extended symref.
;;
;;   It is always true that:
;;      (extended-symref-p (make-symref agent :slot1))    -->  T
;;      (extended-symref-p (make-symref agent))           -->  NIL
;;      (extended-symref-p nil)                           -->  NIL
;;      (extended-symref-p 'agent-name)                   -->  NIL
;;   and also:
;;      (if (agent-p thing) (extended-symref-p thing) t)  -->  NIL
;;      (if (extended-symref-p thing) (agent-p thing) t)  -->  NIL

;; SYMREF-P          (object)  -->  T or NIL
;;
;;   A function that is equvalent to:   (or (simple-symref-p object)
;;                                          (extended-symref-p object))


;; SYMREF-MATCH  (symref1 symref2)  -->  T or NIL
;;
;;   A predicate that returns T if the two symrefs match and NIL otherwise.
;;   They 'match' if their SYMREF-AGENTs are the same (EQ).

;; SYMREF-EQUAL  (symref1 symref2)  -->  T or NIL
;;
;;   A predicate that returns T if the two symrefs are equal and NIL otherwise.
;;   They are 'equal' if they are of the same type (simple or extended),
;;   their AGENT fields are EQ, and their SLOT fields (if any) are also EQ.
;;
;;   It is always true that:
;;      (if (symref-equal sr1 sr2) (symref-match sr1 sr2) t)  -->  T

;; SIMPLIFY-SYMREF (symref)  -->  simple-symref
;;
;;   A function that coerces an extended symref into a simple one by
;;   discarding its SLOT field.
;;   If SYMREF is already simple, it is returned without change.
;;   If SYMREF is not a symref, an error is signalled.

;; SYMREF->STRING (symref)  -->  string
;;
;;   A function that forms and returns a (relatively) human-readable string
;;   representation of a symref.
;;   If SYMREF is not a symref, PRINT-SYMREF passes its argument to:
;;                                                      (format nil "~A" arg)
;;   Examples:
;;     (symref->string (make-symref #$chair17))         -->  "chair17"
;;     (symref->string (make-symref #$chair17 :slot1))  -->  "chair17.slot1"
;;     (symref->string 3.14)                            -->  "3.14"

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

;;  In this implementation, simple symrefs are BASE-AGENTs and extended
;;  symrefs are kept in cons cells.

(eval-when (compile load eval)
  (declaim (inline  simple-symref-p
                    symref-p
                    symref-agent
                    symref-slot
                    symref-match
                    symref-equal   ))
) ; eval-when

(defun simple-symref-p (object)
  "Is this a simple symbolic reference?."
  (declare (values boolean))
  (agentp object) )

(defun extended-symref-p (object)
  "Is this an extended symbolic reference of the form (agent.slot) ?."
  (declare (values boolean))
  (and (consp object)
       (typep (car object) 'base-agent)
       (slot-label-p (cdr object)) ))

(defun symref-p (object)
  "Is this a symbolic reference, simple or extended?."
  (declare (values boolean))
  (or (simple-symref-p object)
      (extended-symref-p object)) )


(defun  make-symref  (agent &optional slot)
  "Constructs and returns a symbolic reference."
  (unless (and (agentp agent)
               (or (null slot)
                   (slot-label-p slot)))
    (error "MAKE-SYMREF: ~S is not an agent or ~S is not a valid slot label."
           agent slot))
  (if slot
      (cons agent slot) 
      agent))


(defun  symref-agent  (symref)
  "The AGENT field of a symbolic reference."
  (cond ((simple-symref-p symref)   symref)
        ((extended-symref-p symref) (car symref))
        (t (error "SYMREF-AGENT: ~S is not a symbolic reference." symref)) ))

(defun  symref-slot  (symref)
  "The SLOT field of a symbolic reference. (NIL if simple reference.)"
  (cond ((extended-symref-p symref) (cdr symref))
        ((simple-symref-p symref)   nil)
        (t (error "SYMREF-SLOT: ~S is not a symbolic reference." symref)) ))


(defun symref-match (sr1 sr2)
  "A predicate that returns T if the two symrefs match and NIL otherwise."
  (declare (values boolean))
  (eq (symref-agent sr1)
      (symref-agent sr2)))

(defun symref-equal (sr1 sr2)
  "A predicate that returns T if the two symrefs are equal and NIL otherwise."
  (equal sr1 sr2))   ; No error checking


(defun simplify-symref (symref)
  "Discard the SLOT field of an (extended) symref."
  (cond ((extended-symref-p symref) (symref-agent symref))
        ((simple-symref-p symref)   symref)
        (t (error "SIMPLIFY-SYMREF: ~S is not a symbolic reference." symref))
  ))


(defun symref->string (symref)
  "A human-readable string representation of a symbolic reference."
  (declare (values string))
  (let ((*print-case* :downcase))
    (cond  ((simple-symref-p symref)
                      (format nil "~A" (agent-name symref)))
           ((extended-symref-p symref)
                      (format nil "~A.~A" (agent-name (symref-agent symref))
                                          (symref-slot  symref)))
           (t (format nil "~A" symref)) )))


;;;;;;;  End of file DUAL/ARCHIT/SYMREF.LSP
