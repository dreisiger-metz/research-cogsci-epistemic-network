;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       archit/slots.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Basic facilities for G-slots, S-slots, and facets.
;;; DEPENDS-ON: defs.lsp, archit/basic.lsp, archit/symref.lsp, archit/conref.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    14-03-97 [1.0]
;;; UPDATED:    13-11-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;   S L O T S   A N D   F A C E T S   ;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")


;;;; The key concepts defined in this file are G-slots, S-slots, and facets.
;;;; In addition to them, it defines several other concepts (i.e. classes).
;;;;
;;;; This file builds upon DUAL/ARCHIT/FILLER.LSP and in turn provides building
;;;; blocks for DUAL/ARCHIT/MCRFRAME.LSP and DUAL/ARCHIT/LINKS.LSP.
;;;;
;;;; According to DUAL's specification (section 3.2.3 in DR#1), each agent
;;;; has a micro-frame.  Each micro-frame is a collection of SLOTS.
;;;; There are two major kinds of slots -- general slots (G-SLOTS) and
;;;; frame-specific slots (S-SLOTS). S-slots (and only they) have FACETS.
;;;; Facets may be conceived as slots within slots.
;;;;
;;;; G-SLOTS and FACETS are FILLER-HOLDERS (see DUAL/ARCHIT/FILLER.LSP).
;;;; They inherit all their filler-related functionality (e.g. FHOLDER-FILLER,
;;;; SET-FILLER, and ADD-FILLER-ELEMENT) from there.
;;;;
;;;; Slots and facets are not intended to stand alone. They are organized
;;;; in MICRO-FRAMES (see DUAL/ARCHIT/MCRFRAME.LSP).  Conversely, micro-frames
;;;; and S-slots are said to be SLOT-BUNDLES.
;;;;
;;;; All these concepts belong to the general class of MICROFRAME-COMPONENTS.
;;;; The relationships between the main components are shown below:
;;;;
;;;;                    microframe-component
;;;;                  /         |           \                      base-agent
;;;;     filler-holder      base-slot        slot-bundle            /
;;;;                  \    /         \      /           \         /
;;;;                   link           S-slot             micro-frame
;;;;                  /    \
;;;;            G-slot      facet
;;;;
;;;;
;;;; Each slot (and facet) has a LABEL -- a symbol satisfying SLOT-LABEL-P
;;;; (defined in DUAL/PROCLAIM.LSP and DUAL/LABELS.LSP.). G-slots have labels
;;;; satisfying G-SLOT-LABEL-P, S-slots have labels satisfying S-SLOT-LABEL-P,
;;;; and facets have labels satisfying G-SLOT-LABEL-P.  The specification of
;;;; DUAL postulates that the same symbol cannot satisfy both G- and S-SLOT-
;;;; LABEL-P at the same time. (In other words, G-SLOT-LABEL and S-SLOT-LABEL
;;;; are disjoint subtypes of SYMBOL.)
;;;; In this implementation, all labels are in the keyword package.
;;;;
;;;; Some G-slot labels denote TEMPORARY G-slots and facets.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: base-slot, G-slot, S-slot, facet, slot-bundle,
;;          link, temporary-link, temporary-G-slot, temporary-facet ;
;;
;;          make-G-slot, make-S-slot, make-facet,
;;          slot-label, S-slot-facets, S-slot-comment ;
;;          slot-owner, notify-owner
;;
;; This file also defines the template of the generic function ESTABLISH-
;; MICROFRAME-INTEGRITY. Its methods are implemented in DUAL/ARCHIT/MCRFRAME.LSP
;;

;; Different slot and facet labels are defined in DUAL/PROCLAIM and =/LABEL.LSP.
;; The vocabulary of slot labels can be extended without precompilation of DUAL.
;; The functions defined in this file check the type of the labels via the
;; predicates G-SLOT-LABEL-P, S-SLOT-LABEL-P, SLOT-LABEL-P, LINK-LABEL-P, and
;;   TEMP-LINK-LABEL-P. All these predicates take a symbol and check whether
;;   it is a member of the corresponding variable.


;; MAKE-G-SLOT  (label &key :owner :filler)  -->  new-G-slot
;;
;;   A function that constructs a G-slot with a given label.
;;   LABEL must satisfy G-SLOT-LABEL-P; if not, an error is signaled.
;;   :OWNER must be either NIL or a micro-frame (see DUAL/ARCHIT/MCRFRAME.LSP).
;;     If not supplied, it defaults to NIL.
;;   :FILLER must be a 'valid' filler (see DUAL/ARCHIT/FILLER.LSP).
;;     If not supplied, it defaults to NIL which denotes an empty slot.
;;   MAKE-G-SLOT returns the new slot.
;;
;;   After a successful call to MAKE-G-SLOT, the following conditions hold:
;;     (slot-label new-G-slot)      -->  label
;;     (slot-owner new-G-slot)      -->  owner   or  NIL
;;     (fholder-filler new-G-slot)  -->  filler  or  NIL
;;     (typep new-G-slot 'G-slot)   -->  T
;;     (if (temp-link-label-p label) (typep new-G-slot 'temp-G-slot) T)  -->  T
;;
;;   Cf. ADD-G-SLOT in DUAL/ARCHIT/MCRFRAME.LSP and ADD-LINK in =/LINK.LSP.

;; MAKE-S-SLOT  (label &key :comment :owner :facets)  -->  new-S-slot
;;
;;   A function that constructs a S-slot with a given label.
;;   LABEL must satisfy S-SLOT-LABEL-P; if not, an error is signalled.
;;   :COMMENT must be a string; if not supplied, it defaults to NIL.
;;   :OWNER must be either NIL or a micro-frame (see DUAL/ARCHIT/MCRFRAME.LSP).
;;     If not supplied, it defaults to NIL.
;;   :FACETS must be a list of facets; if not supplied, it defaults to '().
;;   MAKE-S-SLOT returns the new slot.
;;
;;   In addition to storing :FACETS in the S-slot, MAKE-S-SLOT also initializes
;;   the OWNER field of each facet with a reference to the new S-SLOT.
;;
;;   After a successful call to MAKE-S-SLOT, the following conditions hold:
;;     (slot-label new-S-slot)      -->  label
;;     (slot-owner new-s-slot)      -->  owner    or  NIL
;;     (S-slot-comment new-S-slot)  -->  comment  or  NIL
;;     (S-slot-facets new-S-slot)   -->  facets   or  NIL
;;     (fholder-filler new-S-slot)  signals an error, S-slots don't have fillers
;;
;;   Cf. ADD-S-SLOT and ADD-FACET defined in DUAL/ARCHIT/MCRFRAME.LSP.

;; MAKE-FACET  (label &key :owner :filler)  -->  new-facet
;;
;;   A function that constructs a facet with a given label.
;;   LABEL must satisfy G-SLOT-LABEL-P; if not, an error is signaled.
;;   :OWNER must be either NIL or a S-slot. If not supplied, it defaults to NIL.
;;   :FILLER must be a 'valid' filler (see DUAL/ARCHIT/FILLER.LSP).
;;     If not supplied, it defaults to NIL which denotes an empty facet.
;;   MAKE-FACET returns the new facet.
;;
;;   After a successful call to MAKE-FACET, the following conditions hold:
;;     (slot-label new-facet)      -->  label
;;     (slot-owner new-facet)      -->  owner   or  NIL
;;     (fholder-filler new-facet)  -->  filler  or  NIL
;;     (if (temp-link-label-p label) (typep new-facet 'temp-facet) T)  -->  T
;;
;;   (Note that the names above begin with SLOT-xxx and not FACET-xxx.
;;    This is because facets inherit most of their behavior from 'base' slots.)
;;
;;   Cf. ADD-FACET in DUAL/ARCHIT/MCRFRAME.LSP and ADD-LINK in =/LINKS.LSP.


;; SLOT-LABEL (slot)  -->  label
;;
;;   A generic function that reads the label of a slot or facet.
;;   Each slot/facet always has a label. The label is assigned to the
;;   slot or facet at the moment of its construction (by a call to
;;   MAKE-x-SLOT or MAKE-FACET) and cannot be modified.
;;   It is always true that:
;;      (G-slot-label-p (slot-label G-slot))  -->  T
;;      (S-slot-label-p (slot-label S-slot))  -->  T
;;      (G-slot-label-p (slot-label facet) )  -->  T
;;
;;   Note that there is no function named FACET-LABEL.

;; SLOT-OWNER (slot)  -->  owner  or  NIL
;; (SETF SLOT-OWNER)
;;
;;   A generic function that accesses the owner of a slot or facet.
;;   May be used with SETF to alter the owner.
;;   The owner of a G- or S-slot must be a micro-frame or NIL.
;;   The owner of a facet must be a S-slot.
;;   On inappropriate owners,  (SETF SLOT-OWNER)  signals an error.
;;
;;   See also NOTIFY-OWNER and ESTABLISH-MICROFRAME-INTEGRITY.

;; NOTIFY-OWNER (slot)  -->  unspecified
;;
;;   A generic function that is called when a SLOT is changed in order to
;;   notify the owner of the slot (a micro-frame or a S-slot) for the change.
;;
;;   SLOT should be one of the following: G-slot, S-slot, or facet.  If it
;;     is not, an error is signaled.
;;   The function results in a call to ESTABLISH-MICROFRAME-INTEGRITY,
;;     defined in DUAL/ARCHIT/MCRFRAME.LSP.  Therefore, its return value is
;;     unspecified; as is the value of ESTABLISH-MICROFRAME-INTEGRITY.
;;   If the owner is not known, that is  (slot-owner slot)  returns NIL,
;;     NOTIFY-OWNER does nothing and returns NIL.
;;   If SLOT is actually a facet, then the owning S-slot (if any) is
;;     retrieved and NOTIFY-OWNER is applied recursively to it.
;;
;;   Most of the functions that alter a micro-frame component (e.g. SET-FILLER,
;;     ADD-FILLER-ELEMENT, REMOVE-FILLER-ELEMENT, ADD-FACET, REMOVE-SLOT, etc.)
;;     typically call this function unless their NOTIFY-P argument is NIL.
;;     The methods of NOTIFY-OWNER are defined in such a way as to insure
;;     that ESTABLISH-MICROFRAME-INTEGRITY is called at most once during
;;     the dynamic extent of the caller of NOTIFY-OWNER.
;;
;;   It is not recommended to define additional methods for this function.
;;   Customize the behavior of ESTABLISH-MICROFRAME-INTEGRITY instead.


;; S-SLOT-FACETS (S-slot)  -->  facet-list
;; (SETF S-SLOT-FACETS)
;;
;;   A generic function that accesses the facets of S-SLOT.
;;   Returns a (possibly empty) list of facets. May be used with SETF.
;;   Signals an error if S-SLOT is not a S-slot.
;;
;;   The recommended functions for reading and writing facets are:
;;   LOCATE-MFR-COMPONENT, ADD-FACET, and REMOVE-FACET (see ARCHIT/MCRFRAME.LSP)

;; S-SLOT-COMMENT (S-slot)  -->  comment
;; (SETF S-SLOT-COMMENT)
;;
;;   A generic function that accesses the comment of S-SLOT.
;;   Returns a string or NIL. May be used with SETF.
;;   Signals an error if S-SLOT is not a S-slot.
;;
;;   S-slots have dummy labels like :slot1, :slot2, etc. Therefore, it
;;   is convenient to have a means to attach comments to them.

;;
;;;;;; Generic function(s) pertaining to the external protocol

;; NOTIFY-OWNER is defined in DUAL/ARCHIT/FILLER.LSP.

(defgeneric  establish-microframe-integrity (mframe justification)
  (:documentation "Propagate change in one slot to other slots." ))
  ;; The methods for this function are defined in DUAL/ARCHIT/MCRFRAME.LSP.

;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definitions  (and accessor methods)

(eval-when (compile load eval)
  (defclass  slot-bundle  (microframe-component)
    ()  ; no slots
    (:documentation  "Base class for S-SLOT and MICRO-FRAME. Mix-in." ))

  (defclass  base-slot  (microframe-component)
    ((label  :initarg     :label
             :reader      slot-label
             :type        symbol
             :initform    (required-argument) )
     (owner  :initarg     :owner
             :accessor    slot-owner
             :type        (or null slot-bundle) 
             :initform    nil )
    )
    (:documentation  "Base class for G-SLOT, S-SLOT, and FACET.") )

  (defclass  link  (base-slot filler-holder)   ; see DUAL/ARCHIT/FILLER.LSP
    () )   ; inherited slots only

  (defclass  temporary-link  (link temporary-DUAL-object)
    () )   ; inherited slots only

  (defclass  G-slot  (link)
    ()   ; inherited slots only
    (:documentation "General slot.") )

  (defclass  temporary-G-slot  (G-slot temporary-link)
    () )   ; inherited slots only

  (defclass  facet  (link)
    ()   ; inherited slots only
    (:documentation "Facet in a S-slot.") )

  (defclass  temporary-facet  (facet temporary-link)
    () )   ; inherited slots only

  (defclass  S-slot  (base-slot slot-bundle)
    ((facets      :initarg     :facets
                  :accessor    S-slot-facets
                  :type        list
                  :initform    nil     )
     (comment     :initarg     :comment
                  :accessor    S-slot-comment
                  :type        string
                  :initform    nil     )
    )
    (:documentation "Frame-specific slot: a collection of facets.") )
) ; eval-when


;;;;  Constructors

(defun make-G-slot (label &key filler owner)
  "Constructor for G-SLOTs."
  (declare (values G-slot))
  (unless (G-slot-label-p label)
    (error "MAKE-G-SLOT: ~S is not a valid G-slot label." label ))
  (let ((new-slot (if (temp-link-label-p label)
                      (make-instance 'temporary-G-slot :label label)
                      (make-instance 'G-slot :label label)) ))
    (when filler
       (set-filler new-slot filler nil) )
    (when owner
       (if (agentp owner)         ; microframes are [base] agents
           (setf (slot-owner new-slot) owner)
           (error "MAKE-G-SLOT: ~S is not a micro-frame."  owner) ))
    new-slot))

(defun make-S-slot (label &key comment owner facets)
  "Constructor for S-SLOTs."
  (declare (values S-slot)
           (type (or null string) comment)
           (type list facets) )
  (unless  (S-slot-label-p label)
    (error "MAKE-S-SLOT: ~S is not a valid S-slot label." label ))
  (let ((new-slot (make-instance 'S-slot :label label)))
    (when comment
       (setf (S-slot-comment new-slot) comment) )
    (when owner
       (if (agentp owner)         ; microframes are [base] agents
           (setf (slot-owner new-slot) owner)
           (error "MAKE-S-SLOT: ~S is not a micro-frame."  owner) ))
    (when facets
       (dolist (facet facets)
         (setf (slot-owner facet) new-slot))
       (setf (S-slot-facets new-slot) facets) )
    new-slot))

(defun make-facet (label &key filler owner)
  "Constructor for facets."
  (declare (values facet))
  (unless (G-slot-label-p label)
    (error "MAKE-FACET: ~S is not a valid facet label." label ))
  (let ((new-facet (if (temp-link-label-p label)
                       (make-instance 'temporary-facet :label label)
                       (make-instance 'facet :label label)) ))
    (when filler
       (set-filler new-facet filler nil) )
    (when owner
       (if (typep owner 'S-slot)
           (setf (slot-owner new-facet) owner)
           (error "MAKE-FACET: ~S is not a S-slot."  owner) ))
    new-facet))


;;;;  Type-checking methods for the accessors

;; For BASE-SLOT
(defmethod  slot-label ((x t))
  (error "SLOT-LABEL:  ~S is not a slot or facet." x ))

(defmethod  (setf slot-label) (new-value (x t))
  (declare (ignore new-value))
  (error "SLOT-LABEL cannot be used with SETF. Slot labels are immutable." ))

(defmethod  slot-owner ((x t))
  (error "SLOT-OWNER:  ~S is not a slot or facet." x ))

(defmethod  (setf slot-owner) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF SLOT-OWNER):  ~S is not a slot or facet." x ))

;; For S-SLOT
(defmethod  S-slot-facets ((x t))
  (error "S-SLOT-FACETS:  ~S is not a S-slot." x ))

(defmethod  (setf S-slot-facets) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF S-SLOT-FACETS):  ~S is not a S-slot." x ))

(defmethod  S-slot-comment ((x t))
  (error "S-SLOT-COMMENT:  ~S is not a S-slot." x ))

(defmethod  (setf S-slot-comment) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF S-SLOT-COMMENT):  ~S is not a S-slot." x ))


;;;;;;  Printing methods

(defmethod print-object  ((slot base-slot) stream)
  (format stream  "#<~A ~A>"
          (type-of slot)
          (if (slot-boundp slot 'label)
              (slot-label slot)
              "(no label)")))

(defmethod DUAL-describe  ((slot base-slot)
                            &optional (stream *standard-output*))
  (format stream "~&~S is a ~S labeled ~S.~%"
          slot  (type-of slot)  (if (slot-boundp slot 'label)
                                    (slot-label slot)
                                    "(no label)" ))
  (format stream "~&  Its owner is: ~S~%"
          (if (slot-boundp slot 'owner)
              (slot-owner slot)
              "(unbound)" ))
  (values))

(defmethod DUAL-describe :after ((S-sl S-slot)
                                  &optional (stream *standard-output*))
  (format stream "~&  Its comment string is: ~A~%"
          (if (and (slot-boundp S-sl 'comment)
                   (S-slot-comment S-sl))              ; supplied
              (S-slot-comment S-sl)
              "(no comment)"))
  (format stream "~&  Its facets are: ~S~%"
          (if (slot-boundp S-sl 'facets)
              (S-slot-facets S-sl)
              "(unbound)" ))
  (values))

(defmethod DUAL-describe :after ((fh filler-holder)
                                  &optional (stream *standard-output*))
  (format stream "~&  Its filler is: ~S~%"
          (if (slot-boundp fh 'filler)
              (fholder-filler fh)
              "(unbound)" ))
  (values))


;;;;;;  Maintaining microframe consistency.
;;
;;  Filler-altering functions call the generic function NOTIFY-OWNER whenever
;;  the filler is changed. The template of this generic function is defined
;;  in DUAL/ARCHIT/FILLER.LSP. The methods for this function are defined here.
;;  In turn, they call ESTABLISH-MICROFRAME-INTEGRITY, a generic function
;;  whose template is defined in this file and whose methods are supplied
;;  by DUAL/ARCHIT/MCRFRAME.LSP.

(defmethod  notify-owner ((slot base-slot))
  ;; For G-slots and S-slots.
  (let ((owner (slot-owner slot)))
    (when owner
      (establish-microframe-integrity owner slot) )))

(defmethod  notify-owner ((facet facet))
  (let ((owner (slot-owner facet)))
    (when owner
      (notify-owner owner) )))  ; OWNER is a S-slot and will relay the message.

(defmethod notify-owner ((x t))
  (error "NOTIFY-OWNER:  ~S is not a slot or facet." x ))


;;;;;;;  End of file DUAL/ARCHIT/SLOTS.LSP
