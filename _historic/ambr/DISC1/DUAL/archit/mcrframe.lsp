;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/mcrframe.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Organizing slots into micro frames.
;;; DEPENDS-ON: defs.lsp, archit/basic.lsp, archit/filler.lsp, archit/slots.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    17-03-97 [1.0]
;;; UPDATED:    13-11-97 [1.1]
;;; UPDATED:    18-04-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;     M I C R O    F R A M E S      ;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is MICRO-FRAME. It obeys the
;;;; specification laid out in "DUAL Report #1" (section 3.2.3.)
;;;;
;;;; Two things are characteristc of micro-frames:
;;;;   1. Micro-frames are a kind of agents (see DUAL/ARCHIT/BASIC.LSP).
;;;;   2. Micro-frames are slot bundles (see DUAL/ARCHIT/SLOTS.LSP).
;;;;
;;;; Example of a micro-frame (after fig. 3.2.3.1 in DR#1):
;;;;   COLOR-OF                          ; micro-frame with four slots
;;;;     :type        :concept           ;   G-slot filled by a tag
;;;;     :subc        (phys-rel 1.0)     ;   G-slot (also :SUBC link)
;;;;     :slot1                          ;   S-slot with two facets
;;;;        :type     :aspect            ;     facet labeled :TYPE
;;;;        :c-coref  (object 1.0)       ;     facet filled by a conref
;;;;     :slot2  "Hello there..."        ;   S-slot with a comment
;;;;        :type     :aspect            ;     facet owned by :SLOT2
;;;;        :c-coref  (color 1.0)        ;     facet (also :C-COREF link)
;;;;
;;;; The file defines functions for constructing and dealing with micro-frames.
;;;; It uses the building blocks defined in ARCHIT/FILLER.LSP and =/SLOTS.LSP


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: micro-frame, make-micro-frame,
;;          agent-G-slots, agent-S-slots,
;;          add-G-slot, add-S-slot, add-facet,
;;          remove-slot, remove-facet ;
;;
;;          locate-mfr-component, establish-microframe-integrity,
;;          do-all-references
;;

;; MAKE-MICRO-FRAME (name &key :comment :G-slots :S-slots :prompt-p)  -->
;;                                                             -->  new-mframe
;;
;;   A function that creates (and registers) a micro-frame.
;;   NAME must be a symbol (but not  NIL).
;;   :PROMPT-P must be either T or NIL; if not supplied, it defaults to T.
;;   If NAME is already in use, MAKE-MICRO-FRAME signals an error depending on
;;     the value of :PROMPT-P. If it is T (the default), a continuable error
;;     is signaled giving the user an opportunity to supply a new value
;;     for NAME. If :PROMPT-P is NIL, a non-continuable error is signalled.
;;   :COMMENT must be a string; if not supplied, it defaults to NIL.
;;   :G- and :S-SLOTS must be lists of G- or S-slots, respectively. When
;;   not supplied, they default to the empty list.
;;   MAKE-MICRO-FRAME returns the new micro-frame, after applying ESTABLISH-
;;     MICROFRAME-INTEGRITY to it.
;;
;;   In addition to storing :G-SLOTS and :S-SLOTS in the micro-frame,
;;   MAKE-MICRO-FRAME initializes their OWNER fields to point to NEW-MFRAME.
;;
;;   After a successful call to MAKE-MICRO-FRAME, the following conditions hold:
;;     (find-agent name)            -->  new-mframe
;;     (agentp new-mframe)          -->  T
;;     (agent-name new-mframe)      -->  name
;;     (agent-comment new-mframe)   -->  comment  ; or NIL
;;     (agent-G-slots new-mframe)   -->  G-slots
;;     (agent-S-slots new-mframe)   -->  S-slots
;;
;; The macro DEFAGENT (see DUAL/INTRFACE/DEFAGENT.LSP) is much more convenient.
;; See also PPRINT-MICRO-FRAME and its shorthand MFR in the same file.


;; AGENT-G-SLOTS  (mframe)  -->  G-slot-list
;; AGENT-S-SLOTS  (mframe)  -->  S-slot-list
;;
;;   Generic functions that read the slot-lists of MFRAME.
;;   Return a (possibly empty) list of slots of the corresponding kind.
;;   Signal an error if MFRAME is not a micro-frame.
;;
;;   The recommended function for using slots is LOCATE-MFR-COMPONENT.


;; LOCATE-MFR-COMPONENT  (descriptor &optional label1 label2)  -->
;;                                                    --> mfr-component or NIL
;;
;;   A generic function that looks for a micro-frame component.
;;   (Recall that mfr-components are: the micro-frames themselves, G-slots,
;;   S-slots, and facets.)  This function should be the method of choice
;;   for locating such components.  It is a foundation for GET-FILLER and
;;   GET-FILLER-REFS! defined in DUAL/ARCHIT/LINKS.LSP.
;;   The function returns the component if one is found, or NIL otherwise.
;;   It signals an error on incompatible labels and/or descriptor.
;;
;;   DESCRIPTOR may be one of the following: micro-frame, G- or S-slot, facet,
;;     extended-symref (see DUAL/ARCHIT/SYMREF.LSP), conref (see =/CONREF.LSP),
;;     non-NIL symbol (interpreted as a micro-frame name), or NIL.
;;   LABEL1 and LABEL2 should satisfy SLOT-LABEL-P (see DUAL/LABELS.LSP).
;;     If not supplied, they default to NIL.
;;
;;   The behavior of LOCATE-MFR-COMPONENT depends on the type of DESCRIPTOR
;;   according to the following table:
;;
;;    DESCRIPTOR    LABEL1    LABEL2      Behavior of LOCATE-MFR-COMPONENT
;;   -------------------------------------------------------------------------
;;    micro-frame     --        --        Return that micro-frame
;;    micro-frame  slot-label   --        Look for a G- or S-slot
;;    micro-frame   S-label   G-label     Look for a facet in a S-slot
;;    micro-frame   G-label   nonNIL      Signal an error
;;
;;    ext-symref      --        --        Look for a G- or S-slot
;;    ext-symref    G-label     --        Look for a facet in a S-slot
;;
;;    G-slot          --        --        Return that G-slot
;;    G-slot        nonNIL    whatever    Signal an error
;;
;;    S-slot          --        --        Return that S-slot
;;    S-slot        G-label     --        Look for a facet
;;    S-slot        S-label     --        Signal an error
;;    S-slot       slot-label nonNIL      Signal an error
;;
;;    facet           --        --        Return that facet
;;    facet         nonNIL    whatever    Signal an error
;;
;;    conref       whatever   whatever    Recurse on (conref-reference conref)
;;    symbol       whatever   whatever    Recurse on (find-agent symbol)
;;    NIL          whatever   whatever    Return NIL
;;   -------------------------------------------------------------------------


;; ADD-G-SLOT (mframe G-label &key filler order priority notify-p)  -->
;;                                                             -->  new-G-slot
;;
;;   A generic function that adds a G-slot to a micro-frame or updates
;;   the filler of the slot, provided it is already there.
;;   MFRAME should be a micro-frame; if not, an error is signalled.
;;   G-LABEL should satisfy G-SLOT-LABEL-P; if not, an error is signalled.
;;   FILLER should be suitable for passing to SET-FILLER (see ARCHIT/FILLER.LSP)
;;     When not provided, it defaults to NIL.
;;   ORDER should be either :FIFO or :LIFO.  It defaults to :FIFO.
;;   PRIORITY should be either :NEW or :OLD. It defaults to :NEW.
;;   NOTIFY-P has the same interpretation and default value as in SET-FILLER.
;;     Note that when NOTIFY-P is nonNIL, the microframe is notified (by calling
;;     NOTIFY-OWNER) that a change has occured, thus allowing for updating
;;     related slots.
;;   ADD-G-SLOT returns the new G-slot. It may or may not be EQ to some of
;;     the G-slots that MFRAME already possessed.
;;
;;   ADD-G-SLOT first checks whether there already is a G-slot with this label.
;;   If there is and PRIORITY is :OLD, the old slot is returned without change.
;;   If there is and PRIORITY is :NEW, the filler of the old slot is changed
;;     by calling the form  (set-filler <G-slot> filler notify-p)  and the
;;     G-slot is returned.
;;   If there isn't any old slot labeled G-LABEL, a new G-slot is constructed
;;     and added to the front (:LIFO order) or tail (:FIFO order) of the G-slot
;;     list of MFRAME. Then, its filler is changed and the slot is returned.
;;
;;   It is always true that, provided MFRAME is a micro-frame, G-LABEL is
;;   a G-slot-label, and FILLER is a valid filler:
;;      (setq G-slot (add-G-slot mframe G-label filler))   -->  G-slot
;;      (locate-mfr-component mframe G-label)              -->  G-slot
;;      (slot-label G-slot)                                -->  G-label
;;      (fholder-filler G-slot)                            -->  filler
;;
;;      (add-G-slot mframe G-label 'foo :old)     -->  G-slot
;;      (slot-label G-slot)                       -->  G-label
;;      (fholder-filler G-slot)                   -->  filler   ; survivor
;;
;;   Compare with ADD-LINK in DUAL/ARCHIT/LINKS.LSP.


;; ADD-S-SLOT (mframe S-label &key comment order priority notify-p)  -->
;;                                                              -->  new-S-slot
;;
;;   A generic function that adds a S-slot to a micro-frame or updates
;;   the comment of the slot, provided it is already there.
;;   MFRAME should be a micro-frame; if not, an error is signalled.
;;   S-LABEL should satisfy S-SLOT-LABEL-P; if not, an error is signalled.
;;   If provided, COMMENT should be a string or NIL. It defaults to NIL.
;;   ORDER should be either :FIFO or :LIFO.  It defaults to :FIFO.
;;   PRIORITY should be either :NEW or :OLD. It defaults to :NEW.
;;   NOTIFY-P has the same interpretation and default value as in SET-FILLER.
;;   ADD-S-SLOT returns the new S-slot. It may or may not be EQ to some of
;;   the S-slots that MFRAME already possessed.
;;
;;   ADD-S-SLOT is analogous to ADD-G-SLOT except that it works with
;;   a comment and not with a filler. (S-slots don't have fillers.)
;;   The comment is changed using the form:
;;       (setf (S-slot-comment <S-slot>) comment)
;;
;;   Whenever S-SLOT-COMMENT is changed, S-SLOT-FACETS is also changed and
;;   set to NIL. Thus, the S-slots are always added without facets. (Facets
;;   are added using ADD-FACET documented below.)
;;
;;   It is always true that, provided MFRAME is a micro-frame and S-LABEL is
;;   a S-slot-label:
;;      (setq S-slot (add-S-slot mframe S-label))   -->  S-slot
;;      (locate-mfr-component mframe S-label)       -->  S-slot
;;      (slot-label S-slot)                         -->  S-label
;;      (S-slot-facets S-slot)                      -->  NIL
;;
;;   Compare with ADD-LINK in DUAL/ARCHIT/LINKS.LSP.


;; ADD-FACET (slot-bundle label1 label2
;;                        &key filler order priority notify-p)  -->  new-facet
;;
;;   A generic function that adds a facet (and possibly a S-slot) to a micro-
;;   frame or updates the filler of the facet, provided it is already there.
;;   SLOT-BUNDLE should be either a micro-frame or a S-slot; else error is sign.
;;     When SLOT-BUNDLE is a micro-frame, LABEL1 should satisfy S-SLOT-LABEL-P
;;     and LABEL2 should satisfy G-SLOT-LABEL-P. If not, an error is signaled.
;;     When SLOT-BUNDLE is a S-slot, LABEL1 should satisfy G-SLOT-LABEL-P (else
;;     an error is signaled). LABEL2 is ignored.
;;   FILLER, ORDER, PRIORITY and NOTIFY-P have the same interpretation and
;;     default values as in ADD-G-SLOT.
;;   ADD-FACET returns the new facet. It may or may not be EQ to some of the
;;     facets that  SLOT-BUNDLE already possessed. If necessary, ADD-FACET
;;     first adds a S-slot in order to create a hook for the new facet.
;;
;;   Compare with ADD-LINK in DUAL/ARCHIT/LINKS.LSP.
;;


;; REMOVE-SLOT (mframe slot-label &key notify-p)  -->  T or NIL
;;
;;   A generic function that removes the slot with the given label from
;;   MFRAME. Returns T if there was such a slot or NIL otherwise.
;;   After the slot has been removed, it is no more accessible.
;;
;;   MFRAME should be a micro-frame; if not, an error is signalled.
;;   SLOT-LABEL should satisfy SLOT-LABEL-P; if not, an error is signalled.
;;   NOTIFY-P controls whether REMOVE-SLOT calls the generic function
;;     ESTABLISH-MICROFRAME-INTEGRITY at the end of its work.  When NOTIFY-P
;;     is nonNIL (the default), this latter function is called whenever
;;     a slot has been removed (that is, when REMOVE-SLOT returns T).
;;
;;   It is always true that:
;;      (progn (remove-slot mfr lbl) (locate-mfr-component mfr lbl))   -->  NIL
;;      (if (locate-mfr-component mfr lbl) (remove-slot mfr lbl) t)    -->  T
;;      (if (locate-mfr-component mfr lbl) nil (remove-slot mfr lbl))  -->  NIL
;;
;;   Compare with REMOVE-LINK in DUAL/ARCHIT/LINKS.LSP.

;; REMOVE-FACET (slot-bundle label1 label2 &key notify-p)  -->  T or NIL
;;
;;   A generic function that removes the facet with FACET-LABEL from
;;   the S-slot with S-LABEL of MFRAME.
;;   Returns T if there was such a facet or NIL otherwise.
;;
;;   SLOT-BUNDLE, LABEL1, and LABEL2 have the same interpretn as in ADD-FACET.
;;   NOTIFY-P has the same interpretation as in REMOVE-SLOT. It defaults to T.
;;   After the facet has been removed, it is not accessible via FIND-SLOT.
;;   The S-slot containing the facet is never removed, even if it is left
;;     without any facets.
;;
;;   Compare with REMOVE-LINK in DUAL/ARCHIT/LINKS.LSP.


;; ESTABLISH-MICROFRAME-INTEGRITY  (mframe justification)  -->  unspecified
;;
;;   A generic function which is used to propagate changes in one microframe
;;   component to other related components, if necessary.
;;
;;   MFRAME should be a micro-frame; if it isn't, an error is signaled.
;;   JUSTIFICATION typically is a micro-frame component.  It shows the location
;;     of the initial change to the micro-frame which has triggered the fun.
;;   The return value of ESTABLISH-MICROFRAME-INTEGRITY is unspecified.
;;   This file defines a method that returns NIL without doing anything.
;;   Other modules of the program are encouraged to provide other methods.
;;     (See in particular DUAL/ARCHIT/HYBRID.LSP.)
;;
;;   Most of the functions that alter a micro-frame component (e.g. SET-FILLER,
;;     ADD-FILLER-ELEMENT, REMOVE-FILLER-ELEMENT, ADD-FACET, REMOVE-SLOT, etc.)
;;     typically call this function unless their NOTIFY-P argument is NIL.


;; DO-ALL-REFERENCES  (var mframe-form [resultform])  {form}*  -->
;;                                               -->  dependent-on-RESULTFORM
;;
;;   A macro that iterates through all references stored in a micro-frame
;;   (in an unspecified order).  Similar to DOLIST.
;;
;;   VAR should be a symbol. It is not evaluated and serves as a variable name.
;;   MFRAME-FORM is a LISP form that produces a micro-frame. If not -- error.
;;   RESULTFORM is an arbitrary LISP form. When not supplied it defaults to NIL.
;;
;;   First DO-ALL-REFERENCES evaluates MFRAME-FORM to produce a micro-frame.
;;   It then executes the forms in the body once for each reference stored in
;;   the micro-frame, with the variable VAR bound to the reference.  The exact
;;   traversal order is implementation-dependent but it is guaranteed that each
;;   reference is visited exactly once.  (A 'reference' in this context is a
;;   connectionist reference -- see DUAL/ARCHIT/CONREF.LSP -- which is a filler
;;   of a G-slot or facet of the micro-frame.)
;;   Finally, RESULTFORM (a single form, not an implicit PROGN) is evaluated,
;;   and the result is the value of the DO-ALL-REFERENCES form.  When the
;;   RESULTFORM is evaluated, the control variable VAR is still bound and has
;;   the value NIL.  If RESULTFORM is omitted, the result is NIL.
;;
;;   An explicit RETURN statement may be used to terminate the loop and return
;;   a specified value.  Declarations may appear at the front of the body. Tags
;;   and GO's are not allowed (i.e. this is not an implicit TAGBODY).
;;
;;   The user-supplied forms in the body may _not_ add or delete any slots or
;;   facets of the micro-frame being traversed.
;;
;;   Example:  To collect the set of all neighbors of MFRAME, use the form:
;;     (let ((result '()))
;;       (do-all-references (conref mframe result)
;;         (setq result (adjoin-conref conref result
;;                                     :priority #'max :order :lifo))))
;;
;;   See also DO-ALL-AGENTS in DUAL/ARCHIT/BASIC.LSP,
;;     DO-ALL-WM in DUAL/ARCHIT/WORK_MEM.LSP  and
;;     DO-ALL-PROCESSES in DUAL/ARCHIT/AGENDA.LSP.


;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric locate-mfr-component (descriptor &optional label1 label2)
  (:documentation "Looks for a slot or facet in a micro-frame." ))

(defgeneric add-G-slot (mframe G-label &key filler order priority notify-p)
  (:documentation "Adds a G-slot to a micro-frame." ))

(defgeneric add-S-slot (mframe S-label &key comment order priority notify-p)
  (:documentation "Adds a S-slot to a micro-frame." ))

(defgeneric add-facet (slot-bundle label1 label2
                                   &key filler order priority notify-p)
  (:documentation "Adds a facet (and a S-slot, if needed) to a micro-frame." ))

(defgeneric remove-slot (mframe slot-label &key notify-p)
  (:documentation "Removes a G- or S-slot from a micro-frame." ))

(defgeneric remove-facet (slot-bundle label1 label2 &key notify-p)
  (:documentation "Removes a facet from a S-slot in a micro-frame." ))

;; ESTABLISH-MICROFRAME-INTEGRITY is defined in DUAL/ARCHIT/SLOTS.LSP.

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definitions  (and accessor methods)

(eval-when (compile load eval)
  (defclass  micro-frame  (slot-bundle base-agent)
    ((G-slots     :accessor    frame-G-slots       ; for internal use
                  :reader      agent-G-slots       ; for the external protocol
                  :type        list                ; list of G-SLOTs
                  :initform    nil   )
     (S-slots     :accessor    frame-S-slots       ; for internal use
                  :reader      agent-S-slots       ; for the external protocol
                  :type        list                ; list of S-SLOTs
                  :initform    nil   )
    )
    (:documentation "A micro-frame is a bundle of slots; a mixin class." ))
) ; eval-when

;;;;  Constructor

(defun make-micro-frame (name &key comment G-slots S-slots (prompt-p t) )
  "Makes a micro-frame and registers it into the total pool of agents."
  (declare (values micro-frame)
           (type (or null string) comment)
           (type list G-slots S-slots) )
  (let ((new-mfr (allocate-agent name 'micro-frame prompt-p)))
    (when  comment                          ; COMMENT supplied?
      (setf (agent-comment new-mfr) comment))
    (when  G-slots                          ; G-SLOTS supplied?
      (dolist (G-slot G-slots)
        (setf (slot-owner G-slot) new-mfr))
      (setf (frame-G-slots new-mfr) G-slots))
    (when  S-slots                          ; S-SLOTS supplied?
      (dolist (S-slot S-slots)
        (setf (slot-owner S-slot) new-mfr))
      (setf (frame-S-slots new-mfr) S-slots))
    (establish-microframe-integrity new-mfr
                                    :just-created)
    new-mfr))


;;;;  Type-checking methods for the accessors

(defmethod  agent-G-slots ((x t))
  (error "AGENT-G-SLOTS:  ~S is not a micro-frame." x))

(defmethod  (setf agent-G-slots) (new-value (x t))
  (declare (ignore new-value x))
  (error
    "AGENT-G-SLOTS cannot be used with SETF. Use ADD-G-SLOT and REMOVE-SLOT." ))

(defmethod  frame-G-slots ((x t))
  (error "DUAL-CORE::FRAME-G-SLOTS:  ~S is not a micro-frame." x))

(defmethod  (setf frame-G-slots) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF DUAL-CORE::FRAME-G-SLOTS):  ~S is not a micro-frame." x))

(defmethod  agent-S-slots ((x t))
  (error "AGENT-S-SLOTS:  ~S is not a micro-frame." x))

(defmethod  (setf agent-S-slots) (new-value (x t))
  (declare (ignore new-value x))
  (error
    "AGENT-S-SLOTS cannot be used with SETF. Use ADD-FACET and REMOVE-SLOT." ))

(defmethod  frame-S-slots ((x t))
  (error "DUAL-CORE::FRAME-S-SLOTS:  ~S is not a micro-frame." x))

(defmethod  (setf frame-S-slots) (new-value (x t))
  (declare (ignore new-value))
  (error "(SETF DUAL-CORE::FRAME-S-SLOTS):  ~S is not a micro-frame." x))


;;;;;;  Printing methods

(defmethod DUAL-describe :after ((mfr micro-frame)
                                 &optional (stream *standard-output*))
  (format stream "~&  Its general slots are: ~S~%"
                 (if (slot-boundp mfr 'G-slots)
                     (agent-G-slots mfr)
                     "(unbound)" ) )
  (format stream "~&  Its specific slots are: ~S~%"
                 (if (slot-boundp mfr 'S-slots)
                     (agent-S-slots mfr)
                     "(unbound)" ) )
  (values))


;;;;;;;;;;;   Inner workings of micro-frames    ;;;;;;;;;;;;;

;; (This portion is suitable for block compilation. In this way the calls
;;  to find-G-slot etc. are compiled as local functions by the CMU compiler)
#+:CMU (declaim (ext:start-block locate-mfr-component
                                 add-G-slot  add-S-slot  add-facet
                                 remove-slot  remove-facet  ))

(defun find-G-slot (mframe G-label)       ; no error-checking
  (declare (values (or null G-slot))
           (type micro-frame mframe) )
  (find-if #'(lambda (label) (eq label G-label))
           (frame-G-slots mframe)
           :key #'slot-label) )

(defun find-S-slot (mframe S-label)       ; no error-checking
  (declare (values (or null S-slot))
           (type micro-frame mframe) )
  (find-if #'(lambda (label) (eq label S-label))
           (frame-S-slots mframe)
           :key #'slot-label) )

(defun find-facet (S-slot facet-label)    ; no error-checking
  (declare (values (or null facet))
           (type (or null S-slot) S-slot) )
  (if S-slot
      (find-if #'(lambda (label) (eq label facet-label))
               (S-slot-facets S-slot)
               :key #'slot-label)
      nil))


;;;;  Implementation of LOCATE-MFR-COMPONENT

(defmethod locate-mfr-component ((nl null)
                                 &optional label1 label2 )
  (declare (ignore nl label1 label2))
  nil )

(defmethod locate-mfr-component ((mfr micro-frame)
                                 &optional label1 label2 )
  (declare (inline find-G-slot find-S-slot)
           (values (or null microframe-component)) )
  (cond ((null label1)  mfr)
        ((G-slot-label-p label1)
            (locate-mfr-component (find-G-slot mfr label1) label2))
        ((S-slot-label-p label1)
            (locate-mfr-component (find-S-slot mfr label1) label2))
        (t  (error "LOCATE-MFR-COMPONENT:  ~S is not a valid slot label."
                   label1)) ))

(defmethod locate-mfr-component ((link link)   ; G-slot or facet
                                 &optional label1 label2 )
  (declare (ignore label2)
           (values (or null link)))
  (if (null label1)
      link
      (error "LOCATE-MFR-COMPONENT:  Too many labels ~S ~S."
             link label1 )))

(defmethod locate-mfr-component ((S-slot S-slot)
                                 &optional label1 label2 )
  (declare (inline find-facet)
           (values (or null S-slot facet)))
  (cond ((null label1)  S-slot)
        ((G-slot-label-p label1)
            (locate-mfr-component (find-facet S-slot label1) label2))
        (t  (error "LOCATE-MFR-COMPONENT:  ~S is not a valid facet label."
                   label1)) ))

(defmethod locate-mfr-component ((ref cons)    ; extended-symref or conref
                                 &optional label1 label2)
  (declare (values (or null microframe-component)) )
  (cond ((extended-symref-p ref)
           (locate-mfr-component (symref-agent ref) (symref-slot ref) label1))
        ((conref-p ref)
           (locate-mfr-component (conref-reference ref) label1 label2))
        (t (error "LOCATE-MFR-COMPONENT:  ~S is not a valid descriptor."
           ref)) ))

(defmethod locate-mfr-component ((name symbol)
                                 &optional label1 label2)
  (declare (values (or null microframe-component)) )
  (locate-mfr-component (find-agent name) label1 label2))

(defmethod locate-mfr-component ((x t)
                                 &optional label1 label2 )
  (declare (ignore label1 label2))
  (error "LOCATE-MFR-COMPONENT:  ~S is not a valid descriptor." x ))


;;;;  Adding microframe components

(defmethod add-G-slot ((mframe micro-frame)  label
                       &key (filler nil)  (notify-p t)
                            (order :fifo) (priority :new) )
  (declare (inline find-G-slot))
  (unless (G-slot-label-p label)
    (error "ADD-G-SLOT: ~S is not a valid G-slot label." label ))
  (unless (member priority '(:new :old))
    (error "ADD-G-SLOT: bad :priority argument: ~S." priority ))
  (let* ((old-slot (find-G-slot mframe label))
         (new-slot (if (null old-slot)
                       (make-G-slot label :filler filler :owner mframe)
                       old-slot)) )
    (cond ((eq old-slot new-slot)
             (when (eq priority :new)
               (set-filler old-slot filler notify-p) ))
          ;; Must create a G-slot from scratch
          ((eq order :fifo)
             (setf (frame-G-slots mframe)
                   (append (frame-G-slots mframe) (list new-slot)))
             (when notify-p
               (establish-microframe-integrity mframe new-slot)) )
          ((eq order :lifo)
             (push new-slot (frame-G-slots mframe))
             (when notify-p
               (establish-microframe-integrity mframe new-slot)) )
          (t (error "ADD-G-SLOT: bad :order argument: ~S." order) ))
    new-slot))

(defmethod add-G-slot ((x t) label &key filler order priority notify-p)
  (declare (ignore label filler order priority notify-p))
  (error "ADD-G-SLOT:  ~S is not a micro-frame." x ))


(defmethod add-S-slot ((mframe micro-frame) label
                       &key (comment nil)  (notify-p t)
                            (order :fifo)  (priority :new) )
  (declare (inline find-S-slot))
  (unless (S-slot-label-p label)
    (error "ADD-S-SLOT: ~S is not a valid S-slot label." label ))
  (unless (typep comment  '(or null string))
    (error "ADD-S-SLOT: ~S is not a valid S-slot comment. ~
            (Perhaps you are trying to supply a filler.)" comment ))
  (unless (member priority '(:new :old))
    (error "ADD-S-SLOT: bad :priority argument: ~S." priority ))
  (let* ((old-slot (find-S-slot mframe label))
         (new-slot (if (null old-slot)
                       (make-S-slot label :comment comment :owner mframe)
                       old-slot)) )
    (cond ((eq old-slot new-slot)
             (when (eq priority :new)
               (setf (S-slot-comment old-slot) comment)
               (setf (S-slot-facets  old-slot) nil)
               (when notify-p
                 (establish-microframe-integrity mframe new-slot)) ))
          ;; Must create a S-slot from scratch
          ((eq order :fifo)
             (setf (frame-S-slots mframe)
                   (append (frame-S-slots mframe) (list new-slot)))
             (when notify-p
               (establish-microframe-integrity mframe new-slot)) )
          ((eq order :lifo)
             (push new-slot (frame-S-slots mframe))
             (when notify-p
               (establish-microframe-integrity mframe new-slot)) )
          (t (error "ADD-S-SLOT: bad :order argument: ~S." order) ))
    new-slot))

(defmethod add-S-slot ((x t) label &key comment order priority notify-p)
  (declare (ignore label comment order priority notify-p))
  (error "ADD-S-SLOT:  ~S is not a micro-frame." x ))


(defmethod add-facet ((mframe micro-frame)  label1  label2
                      &key (filler nil)  (notify-p t)
                           (order :fifo) (priority :new) )
  (declare (inline find-S-slot))
  (unless (S-slot-label-p label1)
    (error "ADD-FACET: ~S is not a valid S-slot label." label1 ))
  ;; Move from micro-frame to S-slot and retry.
  (add-facet (add-S-slot mframe label1
                         :order order  :priority :old  :notify-p nil)
             label2  nil
             :filler filler
             :order order  :priority priority  :notify-p notify-p ))

(defmethod add-facet ((S-slot S-slot)  label1  label2
                      &key (filler nil)  (notify-p t)
                           (order :fifo) (priority :new) )
  (declare (inline find-S-slot find-facet)
           (ignore label2) )
  (unless (G-slot-label-p label1)
    (error "ADD-FACET: ~S is not a valid facet label." label1 ))
  (unless (member priority '(:new :old))
    (error "ADD-FACET: bad :priority argument: ~S." priority ))
  (let* ((old-facet (find-facet S-slot label1))
         (new-facet (if (null old-facet)
                        (make-facet label1 :filler filler :owner S-slot)
                        old-facet)) )
    (cond ((eq old-facet new-facet)
             (when (eq priority :new)
               (set-filler old-facet filler notify-p) ))
          ;; Must create a facet from scratch
          ((eq order :fifo)
             (setf (S-slot-facets S-slot)
                   (append (S-slot-facets S-slot) (list new-facet)))
             (when notify-p
               (notify-owner S-slot)) )
          ((eq order :lifo)
             (push new-facet (S-slot-facets S-slot))
             (when notify-p
               (notify-owner S-slot)) )
          (t (error "ADD-FACET: bad :order argument: ~S." order) ))
    new-facet ))

(defmethod add-facet ((x t) label1 label2 &key filler order priority notify-p)
  (declare (ignore label1 label2 filler order priority notify-p))
  (error "ADD-FACET:  ~S is not a micro-frame or S-slot." x ))


;;;;  Removing microframe components

(defmethod remove-slot ((mframe micro-frame)  label  &key (notify-p t) )
  (declare (values boolean)
           (inline find-G-slot find-S-slot) )
  (flet ((remove-G-slot (&aux (G-slot (find-G-slot mframe label)))
           (unless (null G-slot)
             (setf (slot-owner G-slot) nil)
             (setf (frame-G-slots mframe)
                   (remove G-slot (frame-G-slots mframe)))
             (when notify-p
               (establish-microframe-integrity mframe nil))
             t ))
         (remove-S-slot (&aux (S-slot (find-S-slot mframe label)))
           (unless (null S-slot)
             (setf (slot-owner S-slot) nil)
             (setf (frame-S-slots mframe)
                   (remove S-slot (frame-S-slots mframe)))
             (when notify-p
               (establish-microframe-integrity mframe nil))
             t)) )
    ;; Main body of REMOVE-SLOT
    (cond ((G-slot-label-p label) (remove-G-slot))
          ((S-slot-label-p label) (remove-S-slot))
          (t (error "REMOVE-SLOT: ~S is not a valid slot label." label)) )))

(defmethod remove-slot ((x t) label &key notify-p)
  (declare (ignore label notify-p))
  (error "REMOVE-SLOT:  ~S is not a micro-frame." x ))


(defmethod  remove-facet ((mframe micro-frame)  label1  label2
                          &key (notify-p t) )
  (declare (values boolean)
           (inline find-G-slot) )
  (unless (S-slot-label-p label1)
    (error "REMOVE-FACET: ~S is not a valid S-slot label." label1 ))
  (let ((S-slot (find-S-slot mframe label1)))
    (unless (null S-slot)
      (remove-facet S-slot label2 nil :notify-p notify-p) )))

(defmethod  remove-facet ((S-slot S-slot)  label1  label2
                          &key (notify-p t) )
  (declare (values boolean)
           (inline find-facet)
           (ignore label2) )
  (unless (G-slot-label-p label1)
    (error "REMOVE-FACET: ~S is not a valid facet label." label1 ))
  (let ((facet (find-facet S-slot label1)))
    (unless (null facet)
      (setf (slot-owner facet) nil)
      (setf (S-slot-facets S-slot)
            (remove facet (S-slot-facets S-slot)))
      (when notify-p
        (notify-owner S-slot)
      t)) ))

(defmethod remove-facet ((x t) label1 label2 &key notify-p)
  (declare (ignore label1 label2 notify-p))
  (error "REMOVE-FACET:  ~S is not a micro-frame or S-slot." x ))


#+:CMU (declaim (ext:end-block))   ; end of the block-compilation module


;;;;;;  Methods for ESTABLISH-MICROFRAME-INTEGRITY
;;
;;  More methods are provided by other modules of the program.
;;  See in particular DUAL/ARCHIT/HYBRID.LSP.

(defmethod  establish-microframe-integrity  ((mfr micro-frame) justification)
  (declare (ignore mfr justification))
  nil )

(defmethod establish-microframe-integrity  ((x t) justification)
  (declare (ignore justification))
  (error "ESTABLISH-MICROFRAME-INTEGRITY:  ~S is not a micro-frame." x ))


;;;;;   Iterating over the references stored in a micro-frame

(defmacro do-all-references  (header &body body)
  "Iterates over all references stored in a micro-frame. Similar to DOLIST."
  (unless (and (listp header)
               (<= 2 (length header) 3)
               (symbolp (first header)) )    ; var-name
    (error "DO-ALL-REFERENCES: Malformed header in ~S."
           (list* 'do-all-references header body) ))
  (let ((var-name (first header))
        (mframe-form (second header))
        (result-form (third  header)) )  ; possibly NIL for two-element headers
    `(block nil
       (dualc::do-all-references-aux ,mframe-form
                                     #'(lambda (,var-name) ,@body))
       ,(DOLIST-finalization var-name result-form)) ))   ; see ARCHIT/BASIC.LSP

#+:ACLPC
(eval-when (load)      ; provide a more readable message for the status line
  (when allegro:*save-lambda-lists*
    (setf (get 'do-all-references 'allegro:lambda-list)
          '((var mframe-form &optional result) &rest body)) ))

(defun  do-all-references-aux  (mfr fun)
  (declare (type function fun))
  #+:DUAL-DEBUG
    (unless (typep mfr 'micro-frame)
      (error "DUAL-CORE::DO-ALL-REFERENCES-AUX: ~S is not a micro-frame."
             mfr ))
  (flet ((do-fholder (fholder)
           (case (fholder-filler-type fholder)
             (:reference           (funcall fun (fholder-filler fholder)))
             (:list-of-references  (mapc fun (fholder-filler fholder)))
             (t  nil)) ))
    (dolist (G-slot (agent-G-slots mfr))
      (do-fholder G-slot))
    (dolist (S-slot (agent-S-slots mfr))
      (dolist (facet (S-slot-facets S-slot))
        (do-fholder facet)))
    nil ))


;;;;;;;  End of file DUAL/ARCHIT/MCRFRAME.LSP
