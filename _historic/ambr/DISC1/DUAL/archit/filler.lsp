;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/filler.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Fillers and filler-holders
;;; DEPENDS-ON: defs.lsp, archit/basic.lsp, archit/symref.lsp, archit/conref.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    12-11-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;   FILLERS   AND   FILLER-HOLDERS     ;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; An important part of each DUAL agent is its micro-frame (see section
;;;; 3.2.3 in "DUAL Report #1").  The micro-frame is a collection of slots
;;;; and facets and is implemented in DUAL/ARCHIT/SLOTS.LSP and DUAL/ARCHIT/
;;;; MCRFRAME.LSP.  This file defines and implements the preliminary concepts
;;;; FILLER, FILLER-TYPE, and FILLER-HOLDER (or FHOLDER) for short.
;;;;
;;;; Each filler holder may (and usually does) have a filler. The specification
;;;; of the architecture imposes stringent restrictions on the types of objects
;;;; that may serve as slot- and facet fillers in DUAL.
;;;; The set of possible filler types is listed below.

;;; FILLER TYPES
;;; ~~~~~~~~~~~~
;;; The specification of DUAL (see section 3.2.3.3. in DR#1) postulates that
;;; there are seven filler types which can be classified into three groups:
;;; constants, references, and (heterogeneous) lists.
;;;
;;;  #| DUAL world              | LISP world         | Example
;;; --+-------------------------+--------------------+------------------
;;;     constant                . (or tag fixnum)    .
;;;  1    tag                   .   tag              . :aspect
;;;  2    number                .   fixnum           . 3
;;;     reference               . conref             .
;;;  3    ref. to a micro-frame .   simple conref    . (chair17 1.0)
;;;  4    ref. to a slot        .   extended conref  . (chair17.slot1 -0.3)
;;;     list                    . list               .
;;;  5    list of tags          .   list of tags     . (:relation :action)
;;;  6    list of numbers       .   list of fixnums  . (1 2 1)
;;;  7    list of references    .   list of conrefs  . ((ag1 1.0) (ag2.sl1 0.4))
;;;
;;; DUAL specification also states that lists are ordered (i.e. (1 2) /= (2 1))
;;; and that an object of an elementary type can always be converted into a
;;; list with one element.
;;;
;;; The type of a filler is computed by the function FILLER-TYPE (see below).
;;;
;;; In this implementation, elementary fillers are converted into one-element
;;; lists and stored in this way.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: microframe-component, filler-holder,
;;          filler-type, fholder-filler-type,
;;          fholder-filler, set-filler,
;;          add-filler-elt, remove-filler-elt
;;
;; This file also defines the template of the generic function NOTIFY-OWNER.
;; Its methods are implemented in DUAL/ARCHIT/SLOTS.LSP.
;;

;; FILLER-TYPE  (filler)  -->  keyword
;;
;;   A function that determines the 'conceptual' type of FILLER.
;;   It is a vague analog to TYPE-OF.
;;
;;   'Conceptual' type means a type as defined in the conceptual specification
;;   of the architecture. There are six 'conceptual' types: :TAG, :LIST-OF-TAGS,
;;   :NUMBER, :LIST-OF-NUMBERS, :REFERENCE, and :LIST-OF-REFERENCES.
;;   In addition to these six keywords, FILLER-TYPE may also return NIL or
;;   :MALFORMED-FILLER.
;;
;;   'Conceptual' filler types translate into LISP data types as follows:
;;     NIL                --  (null filler)
;;     :NUMBER            --  (numberp filler)
;;     :TAG               --  (tag-p filler)           ; see DEFS.LSP
;;     :REFERENCE         --  (conref-p filler)        ; see ARCHIT/CONREF.LSP
;;     :LIST-OF-xxx       --  (and (listp filler)
;;                                 (every #'xxx-p filler))
;;     :MALFORMED-FILLER  --  anything else
;;
;;   Examples:
;;     ;; (setq chair17.slot1 (make-symref (make-DUAL-agent 'chair17) :slot1)))
;;     (filler-type nil)                              -->  NIL
;;     (filler-type '(:aspect :relation :action))     -->  :LIST-OF-TAGS
;;     (filler-type chair17.slot1)                    -->  :MALFORMED-FILLER
;;     (filler-type (make-conref chair17.slot1 0.9))  -->  :REFERENCE
;;

;; FHOLDER-FILLER-TYPE (fholder)  -->  type-descriptor
;;
;;   A generic function that returns the 'conceptual' type of the filler
;;   stored in FHOLDER.
;;   The function returns one of the following symbols: NIL, :LIST-OF-NUMBERS,
;;   :LIST-OF-TAGS, :LIST-OF-REFERENCES, and :MALFORMED-FILLER.
;;   (In this implementation, elementary fillers are converted into one-element
;;    lists and stored in this way. Hence, the keywords :NUMBER, :TAG, and
;;    :REFERENCE are never returned by FHOLDER-FILLER-TYPE.)
;;
;;   FHOLDER should be a filler holder. If it isn't, an error is signaled.


;; FHOLDER-FILLER  (fholder &optional refs-only-p)  -->  filler
;;
;;   A generic function that reads the filler of FHOLDER.
;;
;;   FHOLDER should be a filler holder. If it isn't, an error is signaled.
;;   REFS-ONLY-P should be either T or NIL. It defaults to NIL.
;;   FHOLDER-FILLER always returns a list. (Elementary fillers are stored as
;;   one-element lists.)  The list is heterogeneous -- its elements are
;;   all of the same type.
;;
;;   In the important (and very frequent) special case when the filler is of
;;   type :LIST-OF-REFERENCES, the references under question are connectionist
;;   references (see DUAL/ARCHIT/CONREF.LSP). In other words, they have (raw)
;;   weights attached to them. Oftentimes, however, the caller of FHOLDER-FILLER
;;   does not 'care' of the weights. In previous versions of the program, the
;;   idiom   (mapcar #'conref-reference (fholder-filler fh))   was commonly
;;   used. This generated lots of garbage and is optimized in the current
;;   version by the introduction of the optional parameter REFS-ONLY-P.
;;
;;   When REFS-ONLY-P is NIL (the default), the filler is returned 'as is'.
;;   When REFS-ONLY-P is T, the result depends on the type of the filler.
;;   For NIL, :LIST-OF-TAGS, and :LIST-OF-NUMBERS  the function returns NIL.
;;   For :LIST-OF-REFERENCES, the function returns a list of symbolic
;;   references (see DUAL/ARCHIT/SYMREF.LSP) instead of a list of conrefs.
;;
;;   It is always true that:
;;     (eq (fholder-filler-type fholder)
;;         (filler-type (fholder-filler fholder)))    -->  T
;;   On the other hand, on :LIST-OF-REFERENCEs when REFS-ONLY-P is set to T,
;;     (filler-type (fholder-filler fholder T))  returns :MALFORMED-FILLER
;;   because a list of _symbolic_ references is not a valid filler.
;;
;;   Compare with GET-FILLER vs. GET-FILLER-REFS! in DUAL/ARCHIT/LINKS.LSP.
;;

;; SET-FILLER  (fholder new-filler &optional notify-p)  -->  filler
;;
;;   A generic function that sets the filler of FHOLDER to NEW-FILLER.
;;
;;   FHOLDER should be a filler holder. If it isn't, an error is signaled.
;;   NEW-FILLER should be a valid filler. If  (filler-type new-filler)
;;     returns :MALFORMED-FILLER, SET-FILLER signals an error.
;;   NOTIFY-P should be T or NIL. When not supplied, it defaults to T.
;;   The function 'echoes back' the actual value stored in FHOLDER. It is
;;     always a list.
;;
;;   If NOTIFY-P is T (the default), SET-FILLER calls the generic
;;   function NOTIFY-OWNER (implemented in DUAL/ARCHIT/SLOTS.LSP).
;;   (Rationale: A change of a filler of one slot may entail changes in
;;    other slots in order to maintain the consistency of the micro-frame.)
;;
;;   SET-FILLER is not recommended for use by client programs.
;;   (SETF GET-FILLER) should be used instead -- see DUAL/ARCHIT/LINKS.LSP.
;;
;;   Examples:
;;     ;; (setq conref1 (make-conref #$ag1 1.0)
;;     ;;       conref2 (make-conref (make-symref #$ag2 :slot1) 0.75) )
;;     (set-filler fholder :relation)  -->  (:relation)
;;     (fholder-type fholder)          -->  :LIST-OF-TAGS
;;
;;     (set-filler fholder conref1)    -->  ((#$ag1 . 1.0))
;;     (set-filler fholder
;;        (list conref1 conref2))  -->  ((#$ag1 . 1.0)((#$ag2 . :slot1) . 0.75))
;;     (fholder-filler fholder)    -->  ((#$ag1 . 1.0)((#$ag2 . :slot1) . 0.75))
;;     (fholder-filler fholder t)  -->  (#$ag1 (#$ag2 . :slot1))
;;

;; ADD-FILLER-ELT (fholder new-elt &key notify-p order priority)  -->  T or NIL
;;
;;   A generic function that adds NEW-ELT to the filler of FHOLDER.
;;
;;   FHOLDER should be a filler holder. If it isn't, an error is signaled.
;;   NEW-ELT should be a valid filler of an 'elementary' type, that is,
;;     (filler-type new-elt)  should return  :REFERENCE, :NUMBER, or :TAG.
;;   :NOTIFY-P should be T or NIL. When not supplied, it defaults to T.
;;   :ORDER and :PRIORITY should be suitable for passing to the function
;;     ADJOIN-CONREF defined in DUAL/ARCHIT/CONREF.LSP. They have the same
;;     interpretation and default values (:FIFO and :NEW, respectively).
;;   The function returns T if it has actually changed the filler of FHOLDER or
;;     NIL if NEW-ELT was not actually new and thus no changes were warranted.
;;
;;   ADD-FILLER-ELT checks carefully (via FILLER-TYPE and FHOLDER-FILLER-TYPE)
;;   whether NEW-ELT is valid and compatible with the type of the old elements
;;   stored in FHOLDER (if any).  If NEW-ELT is a malformed filler, or does not
;;   agree with the old elements, ADD-FILLER-ELT signals an error and FHOLDER
;;   remains unchanged.
;;
;;   ADD-FILLER-ELT works according to the following algorithm:
;;     1. The filler type of NEW-ELT is determined by a call to FILLER-TYPE.
;;        On malformed or non-elementary fillers, an error is signaled.
;;     2. The type of the filler of FHOLDER is determ'd by FHOLDER-FILLER-TYPE.
;;     3. The two types are checked for consistency. On malformed fillers or
;;        inconsistencies, an error is signaled.
;;     4. It is checked whether the FHOLDER already possesses an element which
;;        is the 'same' as NEW-ELT. (The same means 'EQL' for fillers of type
;;        :NUMBER or :TAG, and 'CONREF-MATCH' for :REFERENCEs.)
;;     5. If NEW-ELT isn't really new (as determined at step 4.), ADD-FILLER-ELT
;;        returns NIL without modifying the filler holder.
;;     6. If NEW-ELT is new, then it is added to the filler list as controlled
;;        by :ORDER and :PRIORITY. The operation is non-destructive.
;;        Finally, ADD-FILLER-ELT returns T and, unless NOTIFY-P is NIL, calls
;;        NOTIFY-OWNER.
;;
;;   If ADD-FILLER-ELT returns without error, it is always true (regardless of
;;   whether the value returned is T or NIL) that FHOLDER contains an element
;;   that is the 'same' as NEW-ELT.
;;
;;   ADD-LINK (defined in DUAL/ARCHIT/LINKS.LSP) is more convenient
;;   than ADD-FILLER-ELT in most cases.
;;

;; REMOVE-FILLER-ELT (fholder elt &optional notify-p)  -->  T or NIL
;;
;;   A generic function that non-destructively removes an element from the
;;   filler of FHOLDER. It is the counterpart to ADD-FILLER-ELT.
;;
;;   FHOLDER should be a filler holder. If it isn't, an error is signaled.
;;   ELT may be an arbitrary LISP object. The interesting case, however, is
;;   when it is a 'valid' filler element whose type agree with the type of
;;   the filler of PLACE. (Otherwise, REMOVE-FILLER-ELT simply returns NIL.)
;;   (When ELT is a symbolic reference (see ARCHIT/SYMREF.LSP) it is treated
;;    like a connectionist reference (see ARCHIT/CONREF.LSP) with zero weight.)
;;   NOTIFY-P should be T or NIL. When not supplied, it defaults to T.
;;   REMOVE-FILLER-ELEMENT returns T if FHOLDER has been changed; else -- NIL.
;;
;;   REMOVE-FILLER-ELT works according to the following algorithm:
;;     1. The filler type of ELT is determined  (by a call to FILLER-TYPE).
;;        If ELT is a :MALFORMED-FILLER, REMOVE-FILLER-ELT returns NIL.
;;     2. The type of the filler of FHOLDER is determ'd by FHOLDER-FILLER-TYPE.
;;     3. The two types are checked for consistency. If they are inconsistent,
;;        REMOVE-FILLER-ELT returns NIL.
;;     4. Otherwise, it is checked whether FHOLDER already possesses an element
;;        which is the 'same' as ELT. (The same means 'EQL' for fillers of
;;        type :NUMBER or :TAG, and 'CONREF-MATCH' for :REFERENCEs.)
;;     5. If ELT is not the 'same' as any of the elements in FHOLDER,
;;        REMOVE-FILLER-ELT returns NIL.
;;     6. Otherwise, ELT is destructively removed from the list and
;;        REMOVE-FILLER-ELT returns T. In addition, NOTIFY-OWNER is called
;;        unless NOTIFY-P is NIL.
;;
;;   If REMOVE-FILLER-ELT returns without error, it is always true (regardless
;;   of whether the value returned is T or NIL) that FHOLDER does not contain
;;   any elements that are the 'same' as ELT.
;;
;;   REMOVE-LINK (defined in DUAL/ARCHIT/LINKS.LSP) is more convenient
;;   than REMOVE-FILLER-ELT in most cases.
;;

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric  fholder-filler-type (fholder)
  (:documentation  "Determines the type of the filler stored in FHOLDER." ))

(defgeneric fholder-filler (fholder &optional refs-only-p)
  (:documentation "Retrieves the filler of a filler holder." ))

(defgeneric set-filler (fholder new-filler &optional notify-p)
  (:documentation "Sets the filler of FHOLDER." ))

(defgeneric  add-filler-elt (fholder new-elt &key notify-p order priority)
  (:documentation "Adjoins NEW-ELT to the filler of FHOLDER." ))

(defgeneric  remove-filler-elt (fholder elt &optional notify-p)
  (:documentation "Removes ELT from the filler of FHOLDER." ))


(defgeneric  notify-owner (slot)
  (:documentation "Notify owner that the filler has changed." ))
  ;; The methods for this function are defined in DUAL/ARCHIT/SLOTS.LSP.

;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definitions  (and accessor methods)

(eval-when (compile load eval)
  (defclass  microframe-component  (DUAL-object)
    ()  ; no slots, this class is a hook for methods
    (:documentation  "The common superclass of slots, facets, etc." ))

  (defclass  filler-holder  (microframe-component)
    ((filler      :accessor    fh-filler
                  :type        list
                  :initform    nil     )
     (filler-aux  :accessor    fh-aux
                  :type        list
                  :initform    nil
                  :documentation  "List of symrefs (as opposed to conrefs)." )
    )
  (:documentation  "Filler-holding component of a slot; a mixin class.") )
   ;;  Fillers of 'elementary types' are kept as one-element lists.
   ;;  The auxiliary slot is used only for fillers of type :LIST-OF-REFERENCES
   ;;  and keep the equivalent _symref_ list.
) ; eval-when


;;;;  Constructors
;; These are mix-in classes and are not intended to have stand-alone instances.


;;;;  Type-checking methods for the accessors

(defmethod  fh-filler ((x t))
  (error "DUAL-CORE::FH-FILLER:  ~S is not a filler holder." x))

(defmethod  (setf fh-filler) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF DUAL-CORE::FH-FILLER):  ~S is not a filler holder." x))

(defmethod  fh-aux ((x t))
  (error "DUAL-CORE::FH-AUX:  ~S is not a filler holder." x))

(defmethod  (setf fh-aux) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF DUAL-CORE::FH-AUX):  ~S is not a filler holder." x))


;;;;;;  Printing method

(defmethod print-object  ((fholder filler-holder) stream)
  (format stream  "#<~A ~A>"
          (type-of fholder)
          (if (slot-boundp fholder 'filler)
              (fh-filler fholder)
              "(no filler)")))

;; An :after method for DUAL-DESCRIBE is defined in DUAL/ARCHIT/SLOTS.LSP


;;;; Functions dealing with filler types

(defun filler-type (filler)
  "Determines the type of FILLER. Returns a keyword like :REFERENCE, :TAG, etc."
  (declare (values symbol))
  (cond ((null filler)       nil)
        ((numberp filler)    :number)
        ((tag-p filler)      :tag)
        ((conref-p filler)   :reference)
        ((listp filler)      (filler-type-aux filler))
        (t                   :malformed-filler) ))

(defun filler-type-aux (filler-list)
  (declare (type cons filler-list)
           (values symbol))
    (case (filler-type (first filler-list))
      (:number    (if (every #'(lambda (f) (eq :number (filler-type f)) )
                             filler-list)
                      :list-of-numbers
                      :malformed-filler))
      (:tag       (if (every #'(lambda (f) (eq :tag (filler-type f)) )
                             filler-list)
                      :list-of-tags
                      :malformed-filler))
      (:reference (if (every #'(lambda (f) (eq :reference (filler-type f)) )
                          filler-list)
                   :list-of-references
                   :malformed-filler))
      (t :malformed-filler) ))


(defmethod  fholder-filler-type ((fh filler-holder))
  (if (and (slot-boundp fh 'filler) (slot-boundp fh 'filler-aux))
      (let ((filler (fh-filler fh)) )
        (cond ((null filler)             nil)
              ((not (consp filler))      :malformed-filler)
              ((numberp  (first filler)) :list-of-numbers)  ; don't check EVERY
              ((tag-p    (first filler)) :list-of-tags)
              ((conref-p (first filler)) :list-of-references)
              (t                         :malformed-filler) ))
      :malformed-filler ))

(defmethod fholder-filler-type ((x t))
  (error "FHOLDER-FILLER-TYPE: ~S is not a filler-holder." x))


(defun filler-types-match  (item-type list-type)
  (declare (values (member :number :tag :reference :malformed-filler)))
  (case item-type
    (:number    (if (member list-type '(nil :list-of-numbers))
                    :number
                    :malformed-filler))
    (:tag       (if (member list-type '(nil :list-of-tags))
                    :tag
                    :malformed-filler))
    (:reference (if (member list-type '(nil :list-of-references))
                    :reference
                    :malformed-filler))
    (t  :malformed-filler) ))


;;;; Methods for reading and writing fillers

;; FH-FILLER, (SETF FH-FILLER), FH-AUX, and (SETF FH-AUX) are not advertized
;; in the external protocol.

(defmethod fholder-filler ((fh filler-holder) &optional (refs-only-p nil) )
  (declare (values list))
  (if refs-only-p
      (fh-aux fh)
      (fh-filler fh)) )

(defmethod FHOLDER-filler ((x t) &optional (refs-only-p nil))
  (declare (ignore refs-only-p))
  (error "FHOLDER-FILLER: ~S is not a filler holder." x))


(defmethod set-filler ((fh filler-holder) new-filler
                                          &optional (notify-p t) )
  (flet ((write-fh (main-slot aux-slot)
           (setf (fh-filler fh) main-slot)
           (setf (fh-aux fh) aux-slot)
           (when notify-p
             (notify-owner fh))  ; see DUAL/SLOTS.LSP
           main-slot ))          ; return value advertized in the protocol
    (case (filler-type new-filler)
      ((nil)               (write-fh nil nil))
      ((:tag  :number)     (write-fh (list new-filler) nil))
      ((:list-of-tags
        :list-of-numbers)  (write-fh new-filler nil))
      (:reference          (write-fh (list new-filler)
                                     (list (conref-reference new-filler)) ))
      (:list-of-references (write-fh new-filler
                                     (mapcar #'conref-reference new-filler) ))
      (t (error "(SET-FILLER): ~S is not a valid filler." new-filler)) )))


(defmethod set-filler ((x t) new-filler &optional (notify-p t))
  (declare (ignore new-filler notify-p))
  (error "SET-FILLER: ~S is not a filler holder." x ))


;;;;  Adding individual filler elements

(defmethod  add-filler-elt ((fh filler-holder) new-elt
                                               &key (notify-p t)
                                                    (order :fifo)
                                                    (priority :new) )
  (declare (type (or (member :new :old) function)  priority)
           (type (member :fifo :lifo) order)
           (values boolean) )
  (let ((old-filler (fh-filler fh)))
    (case (filler-types-match (filler-type new-elt)
                              (filler-type old-filler))
      ((:tag  :number)
         (cond ((member new-elt old-filler) nil)
               (t (setf (fh-filler fh)
                        (ecase order           ; PRIORITY makes no difference
                          (:lifo (cons new-elt old-filler))
                          (:fifo (append old-filler (list new-elt))) ))
                  (when notify-p
                    (notify-owner fh))
                  T )))
      (:reference
         (multiple-value-bind (new-filler change-flag)
                   (adjoin-conref new-elt old-filler
                       :order order  :priority priority  :destructive-p nil)
           (cond (change-flag  (set-filler fh new-filler notify-p)
                               T)
                 (t            nil)) ))
      (t (error "ADD-FILLER-ELT: Incompatible filler types of ~S and ~S."
                new-elt old-filler )) )))

(defmethod  add-filler-elt ((x t) new-elt &key notify-p order priority)
  (declare (ignore new-elt notify-p order priority))
  (error "ADD-FILLER-ELT: ~S is not a filler holder."  x ))


(defun  adjoin-filler-elt  (fholder elt &rest key-args)
  "Adds ELT to the filler of FHOLDER provided it is not already there."
  (if (member elt (fholder-filler fholder))
      nil
      (apply #'add-filler-elt fholder elt key-args) ))


;;;;  Removing individual filler elements

(defmethod remove-filler-elt ((fh filler-holder) elt &optional (notify-p t))
  (declare (values boolean))
  (let* ((old-filler (fh-filler fh))
         (elt-aux (if (symref-p elt) (make-conref elt 0.0) elt))
         (types (filler-types-match (filler-type elt-aux)
                                    (filler-type old-filler)))
         (equality-predicate (if (eq types :reference)
                                 #'conref-match
                                 #'eql )) )
    (cond ((eq types :malformed-filler) nil)
          ((member elt-aux old-filler :test equality-predicate)
             (set-filler fh (remove elt-aux old-filler :test equality-predicate)
                            notify-p)
             T )
          (t nil) )))

(defmethod  remove-filler-elt ((x t) elt &optional notify-p)
  (declare (ignore elt notify-p))
  (error "REMOVE-FILLER-ELT: ~S is not a filler holder."  x ))


;;;;;;;  End of file DUAL/ARCHIT/FILLER.LSP
