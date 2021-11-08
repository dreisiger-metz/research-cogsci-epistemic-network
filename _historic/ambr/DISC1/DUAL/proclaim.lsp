;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-

;;; FILE:       DUAL/proclaim.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Some fundamentals for DUAL's implementation
;;; DEPENDS-ON: DUAL/packages.lsp, DUAL/defs.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    26-01-98 [1.1.0]
;;; UPDATED:    03-02-98 [1.1.1]
;;; UPDATED:    24-07-98 [1.1.2]  The 'official release'
;;; UPDATED:    ...

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;     P R O C L A M A T I O N   F A C I L I T I E S    ;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; This file provides facilities for proclaiming various DUAL parameters.
;;;; The current implementation supports proclamation for:
;;;;   -- consumption of symbolic operations  -- see DUAL/ARCHIT/BRACKETS.LSP
;;;;   -- tags (e.g. :TEMPORARY)              -- see DUAL/ARCHIT/FILLER.LSP, ...
;;;;   -- labels for slots and facets         -- see DUAL/ARCHIT/SLOTS.LSP, ...
;;;;   -- link weights depending on the label -- see DUAL/INTRFACE/READFILR.LSP
;;;;
;;;; The functions defined here conceptually belong to other files. I've moved
;;;; them to this file in order to define them earlier, which in turn provides
;;;; the opportunity to proclaim DUAL parameters from the very beginning.
;;;; Thus, for instance, the files from DUAL/ARCHIT directory will use the
;;;; proclaimed consumption of the various operations.
;;;;

;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: proclaim-consumption, get-consumption ;
;;          proclaim-label, label-type,
;;            G-slot-label-p, S-slot-label-p, slot-label-p ;
;;          proclaim-link, link-descriptor, link-default-weight,
;;            link-label-p, temp-link-label-p, MP-label-p ;
;;          proclaim-tag, tag-type, tag-p ;
;;          DUAL-keyword-p


;; PROCLAIM-CONSUMPTION  (operation-name consumption-specifier)  -->
;;                                                  -->  consumption-specifier
;; GET-CONSUMPTION  (operation-name &optional numeralize-p)  -->
;;                                                  -->  consumption-specifier
;;
;;   These two functions are documented in DUAL/ARCHIT/BRACKETS.LSP.


;; PROCLAIM-LABEL (label type &optional type-aux)  -->  (values type type-aux)
;; LABEL-TYPE  (label)  -->  (values type type-aux)
;;
;;   ...
;;

;; G-SLOT-LABEL-P (thing)  -->  :TAG , :NUMBER , :REFERENCE , or  NIL
;; S-SLOT-LABEL-P (thing)  -->  T or NIL
;;   SLOT-LABEL-P (thing)  -->  :TAG , :NUMBER , :REFERENCE , T , or  NIL
;;
;;   ...

;; PROCLAIM-LINK  (label &key default-weight reset-all
;;                            temporary-p marker-passing-p)  -->  link-descr
;; LINK-DESCRIPOR (label)  -->  link-descr
;;
;;   ...
;;

;; LINK-LABEL-P        (thing)  -->  T  or  NIL
;; TEMP-LINK-LABEL-P   (thing)  -->  T  or  NIL
;; MP-LABEL-P          (thing)  -->  T  or  NIL
;; LINK-DEFAULT-WEIGHT (label)  -->  weight  or  NIL
;;
;;   ...


;; PROCLAIM-TAG  (symbol type)  -->  type
;; TAG-TYPE      (symbol)       -->  type
;; TAG-P         (thing)        -->  T  or  NIL
;;
;;   ...

;; DUAL-keyword-p (symbol)  -->  T  or  NIL
;;
;;   ...

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

(cl:in-package  "DUAL-CORE")


;;;;;;  *********  CONSUMPTION PROCLAMATIONS  *********
;;
;;  See file DUAL/ARCHIT/BRACKETS.LSP.

(defun proclaim-consumption (operation-name consumption-specifier)
  "Proclaims how much energy is needed for an operation to complete."
  (assert (symbolp operation-name))
  (cond ((eq consumption-specifier 'nil)
             (remprop operation-name 'consumption)  nil )
        ((numberp consumption-specifier)
             (assert (not (minusp consumption-specifier)))
             (setf (get operation-name 'consumption)
                   (coerce consumption-specifier 'single-float)) )
        ((eq consumption-specifier ':explicit-S-PROGN)
             (setf (get operation-name 'consumption)
                   ':explicit-S-PROGN) )
        (t   (error
               "PROCLAIM-CONSUMPTION: ~S is not a valid consumption specifier."
               consumption-specifier)) ))

(defun get-consumption (operation-name &optional numeralize-p)
  (declare (type symbol operation-name)
           (values (or single-float (member nil :explicit-S-PROGN))) )
  (let ((table-entry (get operation-name 'consumption)))
    (if numeralize-p
        (numeralize-consumption-specifier table-entry)
        table-entry) ))



;;;;;;  *********  LABEL PROCLAMATIONS  *********
;;
;;  See "DUAL Report #1", section 3.2.3.4.
;;  See files DUAL/ARCHIT/FILLER, =/SLOTS, =/MCRFRAME, and =/LINKS.LSP.
;;  See also DUAL/INTRFACE/READFILR and =/DEFAGENT.LSP.

(defvar *valid-G-slot-labels* nil
  "Internal variable.  Access it through PROCLAIM-LABEL." )
(defvar *valid-S-slot-labels* nil
  "Internal variable.  Access it through PROCLAIM-LABEL." )

(let  ((label-table (make-hash-table :size 50 :test #'eq )))

  (defun proclaim-label  (label type &optional type-aux)
    "Proclaim that a (keyword) SYMBOL is a valid label."
    (assert (symbolp label))
    (flet ((clear-label ()
              ;; Clears LABEL from the table and all subsidiary variables.
              (setq *valid-G-slot-labels*
                    (remove label *valid-G-slot-labels*))
              (setq *valid-S-slot-labels*
                    (remove label *valid-S-slot-labels*))
              (proclaim-link label :reset-all t)    ; not label ==> not link
              (remhash label label-table) )
           (add-label (&aux (new-table-entry (list type type-aux)) )
              ;; Adds LABEL to the table and all subsidiary variables.
              (ecase type
                (:G-slot (unless (member type-aux '(:tag :number :reference))
                           (error "Bad label proclamation for ~S -- ~S."
                                  label new-table-entry ))
                         (pushnew label *valid-G-slot-labels*)
                         (setf (gethash label label-table) new-table-entry))
                (:S-slot (pushnew label *valid-S-slot-labels*)
                         (setf (gethash label label-table) new-table-entry)) )))
      ;; Main body of PROCLAIM-LABEL
      (if (null type)
          (clear-label)
          (let ((old-table-entry (gethash label label-table)))
            (cond ((null old-table-entry)
                      (add-label) )
                  ((and (eq type     (first  old-table-entry))
                        (eq type-aux (second old-table-entry)))
                      nil )   ; already there
                  (t  #+:DUAL-DEBUG
                        (warn "Altering the label type of ~S from ~S to ~S."
                              label old-table-entry (list type type-aux))
                      (clear-label)
                      (add-label)) )))
      (values type type-aux) ))

  (defun label-type (label)
    "As proclaimed by PROCLAIM-LABEL. Return two values."
    #+:DUAL-DEBUG
    (unless (symbolp label)
       (error "LABEL-TYPE: ~S is not a symbol." label ))
    (let ((table-entry (gethash label label-table)))
      (if (null table-entry)
          (values nil nil)
          (values-list table-entry) )))

) ; end of LABEL-TABLE

(declaim (inline G-slot-label-p S-slot-label-p slot-label-p))
(defun G-slot-label-p (thing)
  "Returns T if THING is a valid G-slot-label and NIL otherwise."
  (and (member thing *valid-G-slot-labels*) T))
(defun S-slot-label-p (thing)
  "Returns T if THING is a valid S-slot-label and NIL otherwise."
  (and (member thing *valid-S-slot-labels*) T))
(defun slot-label-p (thing)
  "Returns T if THING is a valid slot label and NIL otherwise."
  (or (G-slot-label-p thing)
      (S-slot-label-p thing)))



;;;;;;  *********  LINK PROCLAMATIONS  *********
;;
;;  Link labels are a subset of the G-slot labels (see DR#1, section 3.2.3.5).
;;  See files DUAL/ARCHIT/LINKS.LSP.
;;  See also DUAL/INTRFACE/READFILR and =/DEFAGENT.LSP.

(defvar *valid-link-labels* nil
  "Internal variable.  Access it through PROCLAIM-LINK." )
(defvar *temp-link-labels* nil
  "Internal variable.  Access it through PROCLAIM-LINK." )
(defvar *MP-labels* nil
  "Internal variable.  Access it through PROCLAIM-LINK." )

(let  ((link-table (make-hash-table :size 20 :test #'eq )))

  (defun proclaim-link  (label &key temporary-p  marker-passing-p
                                    reset-all    default-weight  )
    "Proclaim the properties of the links with a given label."
    (when reset-all
      (setq *valid-link-labels* (remove label *valid-link-labels*))
      (setq *temp-link-labels*  (remove label *temp-link-labels*))
      (setq *MP-labels*         (remove label *MP-labels*))
      (remhash label link-table)
      (if (or temporary-p marker-passing-p default-weight)
          nil   ; continue, do _not_ quit PROCLAIM-LINK
          (return-from PROCLAIM-LINK nil)) )
    ;; Register the new (or refreshed) link:
    (unless (equal '(:G-slot :reference)
                   (multiple-value-list (label-type label)))
      (error "PROCLAIM-LINK: ~S cannot be a link label because ~
              it is not a G-slot label."  label ))
    (pushnew label *valid-link-labels*)
    (if temporary-p
        (pushnew label *temp-link-labels*)
        (setq *temp-link-labels* (remove label *temp-link-labels*)))
    (if marker-passing-p
        (pushnew label *MP-labels*)
        (setq *MP-labels* (remove label *MP-labels*)))
    (let ((table-entry (list (cons :default-weight default-weight)
                             (cons :temporary-p temporary-p)
                             (cons :marker-passing-p marker-passing-p) )))
      (setf (gethash label link-table) table-entry)
      table-entry ))

  (defun link-descriptor (label)
    "As proclaimed by PROCLAIM-LINK. Return an association list."
    (if (symbolp label)
        (values (gethash label link-table))
        (cerror "Return NIL and continue."
                "LINK-DESCRIPTOR: ~S is not a symbol." 
                label) ))

) ; end of LINK-TABLE

(declaim (inline link-label-p temp-link-label-p MP-label-p
                 link-default-weight))
(defun link-label-p (thing)
  "Returns T if THING is a valid link label or NIL otherwise."
  (and (member thing *valid-link-labels*) T))
(defun temp-link-label-p (thing)
  "Returns T if THING is a temporary link label or NIL otherwise."
  (and (member thing *temp-link-labels*) T))
  ;; See also TEMP-LINK-P defined in DUAL/ARCHIT/LINKS.LSP
(defun MP-label-p (thing)
  "Returns T if THING is a valid marker-passing label or NIL otherwise."
  (and (member thing *MP-labels*) T))
(defun link-default-weight (label)
  (cdr (assoc :default-weight (link-descriptor label))))



;;;;;;  *********  TAG PROCLAMATIONS  *********
;;
;;  See file DUAL/ARCHIT/FILLER.LSP and section 3.2.3.3. in "DUAL Report #1".

(defvar *valid-tags* nil
  "Internal variable.  Access it through PROCLAIM-TAG." )

(let  ((tag-table (make-hash-table :size 50 :test #'eq )))

  (defun proclaim-tag (symbol type)
    "Proclaim that a (keyword) SYMBOL is a valid tag."
    (assert (symbolp symbol))
    (cond ((null type)
              (setq *valid-tags* (remove symbol *valid-tags*))
              (remhash symbol tag-table))
           (t (pushnew symbol *valid-tags*)
              (setf (gethash symbol tag-table) type)) )
    type )

  (defun tag-type (symbol)
    "As proclaimed by PROCLAIM-TAG."
    (if (symbolp symbol)
        (values (gethash symbol tag-table))
        (cerror "Return NIL and continue."
                "TAG-TYPE: ~S is not a symbol." 
                symbol) ))

) ; end of TAG-TABLE

(declaim (inline tag-p))
(defun tag-p (thing)
  "Returns T iff THING is a valid tag (see DR#1, section 3.2.3.3)."
  (and (member thing *valid-tags*) T))

(defun DUAL-keyword-p (symbol)
  "Predicate that checks if SYMBOL is one of the reserved DUAL keywords."
  (or (slot-label-p symbol)
      (tag-p        symbol)))


;;;;;;;  End of file DUAL/PROCLAIM.LSP
