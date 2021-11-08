;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/links.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Facilities for adding, removing, and using links.
;;; DEPENDS-ON: archit/filler.lsp, archit/slots.lsp, archit/mcrframe.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    25-11-97 [1.1]
;;; UPDATED:    01-06-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...
;;;;;;;;
;;; TO DO:      Provide an efficient facility for changing the (raw) weight
;;;             of a link.  This will be useful for eventual learning
;;;             mechanisms in the architecture.


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;        L  I  N  K  S         ;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is LINK.  The file provides utilities
;;;; for creating, removing, and using links.
;;;;
;;;; Two things are characteristic of links (see DUAL/ARCHIT/SLOTS.LSP):
;;;;   1. Links have labels (see DUAL/ARCHIT/SLOTS.LSP) and these labels
;;;;        satisfy G-SLOT-LABEL-P (see DUAL/PROCLAIM.LSP and DUAL/LABELS.LSP).
;;;;   2. Links are filler-holders (see DUAL/ARCHIT/FILLER.LSP) and the
;;;;        type of their filler is :REFERENCE or :LIST-OF-REFERENCES.
;;;;        Links also have weights, stored in the WEIGHT field of the
;;;;        (connectionist) reference in the filler (see DUAL/ARCHIT/CONREF.LSP)
;;;;
;;;; LINK is a secondary concept in the implementation (cf. sections 3.2.2.6.
;;;; and 3.2.3.5. in "DUAL Report #1").  Links are in fact G-slots or facets.
;;;;
;;;; All functions in this file are in fact higher-level interface to the
;;;; primitives defined in DUAL/ARCHIT/FILLER, =/SLOTS, and =/MCRFRAME.LSP.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: get-filler, get-filler-refs!,
;;          add-link, remove-link,
;;          temp-link-p, remove-all-temporary-links
;;

;; GET-FILLER  (descriptor &optional label1 label2 first-only-p)  -->
;;                                                      -->  filler  or  NIL
;;
;;   A generic function that locates a micro-frame component and retrieves
;;   its filler.  May be used with SETF as documented below.
;;
;;   DESCRIPTOR, LABEL1, and LABEL2 should be suitable for passing to the
;;     generic function LOCATE-MFR-COMPONENT (see DUAL/ARCHIT/MCRFRAME.LSP).
;;   FIRST-ONLY-P should be either T or NIL. It defaults to NIL.
;;
;;   GET-FILLER works according to the following algorithm:
;;     1. The relevant micro-frame component is located by a call to LOCATE-
;;        MFR-COMPONENT.  If no such component exists, GET-FILLER returns NIL.
;;     2. Otherwise, the filler of the component located at step 1. is
;;        retrieved.  (If the component is a S-slot, an error is signaled.)
;;     3. If FIRST-ONLY-P is NIL (the default), the filler retrieved at step 2.
;;        is returned 'as is'.
;;     4. If FIRST-ONLY-P is nonNIL, only the first element of the filler list
;;        from step 2. is returned.
;;
;;   In particular, when the type of the filler is :LIST-OF-REFERENCES, the
;;   function returns a list of _connectionist_ references when FIRST-ONLY-P is
;;   NIL, and a single such reference when FIRST-ONLY-P is nonNIL.
;;   Compare with GET-FILLER-REFS! which would return _symbolic_ reference(s).
;;

;; (SETF GET-FILLER)  (new-filler descriptor &optional label1 label2)  -->
;;                                                            -->  new-filler
;;
;;   A generic function that provides a SETF method for writing a filler of
;;   a micro-frame component.
;;
;;   NEW-FILLER should be suitable for passing to SET-FILLER.  If it isn't,
;;     an error is signaled.
;;   DESCRIPTOR, LABEL1, and LABEL2 should be suitable for passing to the
;;     generic function LOCATE-MFR-COMPONENT (see DUAL/ARCHIT/MCRFRAME.LSP).
;;     If they do not specify a filler-holder, an error is signaled.
;;   Note that, unlike GET-FILLER, no FIRST-ONLY-P parameter is supported.
;;   The function returns NEW-FILLER (in accordance to the SETF protocol).
;;
;;   Compare with SET-FILLER defined in DUAL/ARCHIT/FILLER.LSP.


;; GET-FILLER-REFS!  (descriptor &optional label1 label2 first-only-p)  -->
;;                                                       -->  symref(s)  or NIL
;;
;;   A function that locates a micro-frame component and retrieves the
;;   symbolic references contained in its filler.
;;
;;   This function optimizes the frequent special case when the type of the
;;   filler in question is :LIST-OF-REFERENCES and the caller of GET-FILLER
;;   is interested only in the symbolic aspect of the references.
;;   GET-FILLER returns a _connectionist_ reference (or a list thereof).
;;   In contrast, GET-FILLER-REFS! returns a _symbolic_ reference (or a list).
;;
;;   GET-FILLER-REFS! takes for granted that the type of the filler (if any)
;;   is :LIST-OF-REFERENCES.  If the type of the filler is something else
;;   (e.g. :LIST-OF-NUMBERS), GET-FILLER-REFS! returns NIL. Compare with the
;;   REFS-ONLY-P parameter to FHOLDER-FILLER defined in DUAL/ARCHIT/FILLER.LSP.
;;
;;   GET-FILLER-REFS! has the same behavior as GET-FILLER, except that:
;;     -- it is an ordinary function;
;;     -- it returns a (list of) _symbolic_ references instead of conrefs;
;;     -- it cannot be used with SETF.
;;

;; Examples:
;;   (setq ag1 (make-micro-frame 'agent1)  ag2 (make-micro-frame 'agent2))
;;   (setq ag1.inst-of (make-symref ag1 :inst-of)
;;         ag1.slot1   (make-symref ag1 :slot1) )
;;   (add-G-slot ag1 :type  :filler '(:concept :temporary))
;;   (add-G-slot ag1 :inst-of)
;;   (add-S-slot ag1 :slot1)
;;   (add-facet  ag1 :slot1 :c-coref  :filler (make-conref ag2 1.0))
;;
;;   (get-filler ag1 :type)                    -->  (:concept :temporary)
;;   (get-filler 'agent1 :type nil t)          -->  :concept
;;   (get-filler 'agent1 :type t)              signals an error
;;   (get-filler ag1.inst-of)                  -->  nil
;;   (get-filler ag1)                          signals an error
;;   (get-filler ag1 :slot1)                   signals an error
;;   (get-filler ag1 :slot1 :c-coref)          -->  ((#$agent2 . 1.0))
;;   (get-filler ag1 :slot1 :c-coref t)        -->  (#$agent2 . 1.0)
;;   (get-filler ag1.slot1 :c-coref nil t)     -->  (#$agent2 . 1.0)
;;   (get-filler-refs! ag1.slot1 :c-coref)     -->  (#$agent2)
;;   (get-filler-refs! ag1 :slot1 :c-coref t)  -->  #$agent2
;;
;;   (setf (get-filler ag1 :inst-of)
;;         (make-conref ag1.slot1 0.75))  -->  ((#$ag1 . :slot1) . 0.75)
;;   (get-filler 'agent1 :inst-of)        -->  (((#$ag1 . :slot1) . 0.75))
;;   (get-filler-refs! ag1.inst-of nil nil t)  -->  (#$ag1 . :slot1)
;;
;;   (push :word (get-filler ag1 :type))  -->  (:word :concept :temporary)
;;   (get-filler ag1 :type nil t)         -->  :word
;;   (get-filler-refs! ag1 :type)         -->  nil
;;
;;   (setf (get-filler-refs! ag1 :type) nil)   signals an error
;;   (setf (get-filler ag1 :type) nil)         -->  nil


;; ADD-LINK (mframe slot-label symref weight
;;           &key facet-label notify-p order priority)  -->  G-slot  or  facet
;;
;;   A generic function that adds a link to MFRAME by adding a connectionist
;;   reference to the filler list of the respective G-slot or facet.  If there
;;   isn't such a slot/facet, ADD-LINK creates it and sets its filler.
;;
;;   MFRAME should be a micro-frame; if not, an error is signaled.
;;   SLOT-LABEL should satisfy LINK-LABEL-P (defined in DUAL/PROCLIAM.LSP) or
;;     S-SLOT-LABEL-P.  If it satisfies the former, FACET-LABEL (if any) is
;;     ignored and the link is stored in a G-slot in the micro-frame.
;;     If it satisfies the latter, FACET-LABEL should be provided and should
;;     satisfy LINK-LABEL-P.  If SLOT-LABEL does satisfies neither LINK-LABEL-P
;;     nor S-SLOT-LABEL-P, an error is signaled.
;;   SYMREF should satisfy SYMREF-P (defined in DUAL/ARCHIT/SYMREF.LSP).
;;     If it doesn't, an error is signaled.  This symbolic reference specifies
;;     the end (or 'receiver') of the link.  According to the specification of
;;     DUAL, it may point either to an agent ('simple symref') or to one of
;;     its slots ('extended symref').
;;   WEIGHT should be a number and specifies the 'raw weight' of the link.
;;     (See section 3.2.4.2. in "DUAL Report #1").  If it isn't a number
;;     (or is a complex number), an error is signaled.
;;   NOTIFY-P, ORDER, and PRIORITY has the same interpretation and default
;;     values as in ADD-FILLER-ELT (see DUAL/ARCHIT/FILLER.LSP).
;;   The function returns the G-slot or facet that is the 'keeper' of the new
;;   link.  It may be a brand new one or an old one.  If it is an old one,
;;   the other references contained in its filler are not affected.
;;
;;   This function is to be preferred to ADD-G-SLOT, ADD-FACET, and
;;

;; REMOVE-LINK (mframe slot-label reference &key facet-label notify-p)  -->
;;                                                               -->  T or NIL
;;
;;   A generic function that removes a link from the micro-frame.
;;   This is the counterpart to ADD-LINK.
;;
;;   MFRAME, SLOT-LABEL, and FACET-LABEL have the same interpretation and
;;     default values as in ADD-LINK.  Note the absence of WEIGHT.
;;   REFERENCE should be a symref or conref.  If it isn't, an error is signaled.
;;     If REFERENCE is a connectionist reference, its weight is dropped and
;;     the resulting symbolic reference is used.
;;   The function returns T if the micro-frame has been changed, else -- NIL.
;;
;;   REMOVE-LINK works by locating the relevant G-slot or facet using
;;     LOCATE-MFR-COMPONENT and then calling REMOVE-FILLER-ELT.  The other
;;     filler of the slot/facet are not affected.  The slot/facet is not
;;     removed even if its new filler is NIL.
;;
;;   This function is to be preferred to REMOVE-SLOT, REMOVE-FACET, and
;;   REMOVE-FILLER-ELT when applicable.


;; TEMP-LINK-P  (fholder)  -->  T or NIL
;;
;;   A function that returns T if the SLOT-LABEL of FHOLDER satisfies the
;;   predicate TEMP-LINK-LABEL-P (deifined in DUAL/PROCLAIM.LSP) and NIL
;;   otherwise.
;;
;;   FHOLDER should be a G-slot or a facet. If it isn't, an error is signaled.
;;
;;   Examples:
;;     (temp-link-p (locate-mfr-component #$ag1 :t-link))         -->  T
;;     (temp-link-p (locate-mfr-component #$ag1 :slot1 :t-link))  -->  T
;;     (temp-link-p (locate-mfr-component #$ag1 :inst-of)         -->  NIL
;;     (temp-link-p (locate-mfr-component #$ag1 :slot1)           error
;;

;; REMOVE-ALL-TEMPORARY-LINKS (mframe &key notify-p)  -->  T or NIL
;;
;;   A generic function that goes through all G-slots and facets of MFRAME
;;   and removes all that satisfy TEMP-LINK-P.
;;
;;   MFRAME should be a micro-frame. If it isn't, an error is signaled.
;;   NOTIFY-P should be T or NIL.  When not supplied, it defaults to T.
;;   The function returns T if the micro-frame has been changed or NIL otherwise
;;
;;   The removal is done directly.  Therefore, any user-supplied :BEFORE and
;;     :AFTER methods for REMOVE-SLOT and REMOVE-FACET will not be invoked.
;;   If NOTIFY-P is nonNIL, ESTABLISH-MICROFRAME-INTEGRITY is called after
;;     all temporary links have been removed.  It is called at most once.
;;
;;   Other modules of the program may provide :AFTER methods to clean up the
;;   traces of the 'dead' :T-LINKs.
;;
;;   (Rationale: According to the specification of DUAL (section 3.2.6.2. in
;;      DR#1) temporary links (T-LINKs) are not really part of the micro-frame.
;;      Rather, they are kept in agent's 'volatile memory' (see ARCHIT/SYMPROC1)
;;      and are deleted whenever the activation level of the host drops below
;;      a threshold. The present function supports this feature.)
;;
;;   See also the function SLOTS->LINKS defined in DUAL/ARCHIT/HYBRID.LSP and
;;   the function CLEAR-VOLATILE-MEMORY defined in DUAL/ARCHIT/SYMPROC1.LSP.
;;

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric  get-filler (descriptor &optional label1  label2  first-only-p)
  (:documentation  "Locate a micro-frame componennt and retrieve its filler." ))
;; GET-FILLER-REFS! is a non-generic function

(defgeneric (setf get-filler) (new-filler descriptor &optional label1 label2)
  (:documentation  "Locate a micro-frame component and modify its filler." ))


(defgeneric add-link (mframe slot-label symref weight
                      &key facet-label notify-p order priority)
  (:documentation "Add a link to a micro-frame." ))

(defgeneric remove-link (mframe link-label symref &key facet-label notify-p)
  (:documentation "Remove a link from a micro-frame." ))

(defgeneric remove-all-temporary-links (mframe &key notify-p)
 (:documentation
   "Removes all G-slot/facets with TEMP-LINK-LABELs from a micro-frame."))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

;;;;;;   Class definitions  (and accessor methods)

;;  The class LINK is defined in DUAL/ARCHIT/SLOTS.LSP.
;;  It inherits from two other classes -- BASE-SLOT and FILLER-HOLDER.


;;;;;;   Utilities for accessing a link

(defmethod  get-filler (descriptor &optional label1  label2
                                             (first-only-p nil) )
  "Locate a micro-frame component and get its filler."
  (declare (type symbol label1 label2) )
  (let ((fholder (locate-mfr-component descriptor label1 label2)))
    (cond ((null fholder) nil)                              ; holder not found
          (first-only-p (first (fholder-filler fholder)))   ; first elt, if any
          (t  (fholder-filler fholder)) )))                 ; whole filler list

(defmethod  (setf get-filler)  (new-filler descriptor &optional label1 label2)
  (declare (type symbol label1 label2))
  (set-filler  (locate-mfr-component descriptor label1 label2)
               new-filler)
  new-filler)  ; value according to the SETF protocol

(defun get-filler-refs!  (descriptor &optional label1  label2
                                               (first-only-p nil) )
  "Get only the symbolic references and ignore the weights."
  (let ((fholder (locate-mfr-component descriptor label1 label2)))
    (cond ((null fholder) nil)                              ; holder not found
          (first-only-p (first (fholder-filler fholder T))) ; first elt, if any
          (t  (fholder-filler fholder T)) )))               ; whole filler list


;;;;;;   Utilities for adding and removing links

(defmethod add-link ((mframe micro-frame)
                     (slot-label symbol)
                     symref
                     (weight number)
                     &key   (facet-label nil)    (notify-p    t)
                            (order       :fifo)  (priority    :new) )
  (declare (values link))
  (unless (symref-p symref)
    (error "ADD-LINK: ~S is not a symbolic reference." symref ))
  (let  ((link (find-or-make-link mframe slot-label facet-label order)))
    (add-filler-elt  link  (make-conref symref weight)
                     :notify-p notify-p  :order order  :priority priority)
    link ))

(defmethod add-link ((x t) (y t) symref (z t)
                     &key facet-label notify-p order priority)
  (declare (ignore symref facet-label notify-p order priority))
  (error  "ADD-LINK:  ~S is not a micro-frame or ~S is not a slot label or ~
                      ~S is not a number."  x y z ))

(defun  find-or-make-link  (mframe slot-label facet-label
                            &optional (order :fifo))
  ;; ADD-G-SLOT and ADD-FACET make a filler-holder if necessary.
  ;; If there already is one, :PRIORITY is set to :OLD to keep old references.
  (cond ((link-label-p  slot-label)  ; link labels are a subset of G-slot labels
            (add-G-slot mframe slot-label  ; ignore FACET-LABEL
                        :priority :old  :order order  :notify-p nil ))
        ((S-slot-label-p slot-label)
            (add-facet  mframe slot-label facet-label
                        :priority :old  :order order  :notify-p nil ))
        (t  (error "DUAL-CORE::FIND-OR-MAKE-LINK:  Bad label: ~S."
                   slot-label)) ))


(defmethod remove-link ((mframe micro-frame)
                        (slot-label symbol)
                        reference
                        &key  (facet-label nil)  (notify-p t) )
  (declare (values boolean))
  (unless (or (symref-p reference) (conref-p reference))
    (error "REMOVE-LINK: ~S is not a reference." reference ))
  (let ((link (locate-mfr-component mframe slot-label facet-label)))
    (unless (null link)
      (remove-filler-elt link reference notify-p) )))

(defmethod remove-link ((x t) (y t) reference &key facet-label notify-p)
  (declare (ignore reference facet-label notify-p))
  (error "REMOVE-LINK:  ~S is not a micro-frame or ~S is not a symbol."
         x y ))


;;;;;;;;  Special treatment of temporary links
;;;;
;;;;  DUAL's specification postulates that temporary links (like :T-LINK)
;;;;  are not stored in the micro-frame itself. Rather, they are stored in
;;;;  the 'volatile memory' of the agent (see DUAL/ARCHIT/SYMPROC.LSP) and
;;;;  are deleted when the agent falls out of the working memory.
;;;;
;;;;  In current implementation, T-LINKs are kept in the micro-frame, that
;;;;  is, together with the permanent links. The facilities below are provided
;;;;  for efficient deletion of temporary links, since this happens often.

(defun temp-link-p (fholder)
  (temp-link-label-p (slot-label fholder)))

(defun initialize-T-LINK-flag  (mframe)
  (declare (values (member 0 1)))
  (flet ((check-S-slot (S-slot)
           (if (some #'temp-link-p (S-slot-facets S-slot))
               (setf (agent-flag mframe *t-link-flag*) 1)
               nil)))
    (cond ((some #'temp-link-p (agent-G-slots mframe))
             (setf (agent-flag mframe *t-link-flag*) 1) )
          ((some #'check-S-slot (agent-S-slots mframe)) )
          (t 0) )))

(defmethod add-G-slot :after ((mframe micro-frame)
                              (G-label  symbol)
                              &key filler order priority notify-p )
  (declare (ignore filler order priority notify-p))
  (when (temp-link-label-p G-label)
    (setf (agent-flag mframe *t-link-flag*) 1) ))

(defmethod add-facet :after ((mframe micro-frame)
                             (S-label symbol)
                             (facet-label symbol)
                             &key  filler order priority notify-p )
  (declare (ignore S-label filler order priority notify-p))
  (when (temp-link-label-p facet-label)
    (setf (agent-flag mframe *t-link-flag*) 1) ))


(defmethod remove-all-temporary-links ((mframe micro-frame)
                                       &key (notify-p t) )
  (declare (values boolean))
  (cond ((zerop (agent-flag mframe *t-link-flag*))
           NIL)    ; nothing created --> nothing to delete
        (t (setf (frame-G-slots mframe)
                 (remove-if #'temp-link-p (frame-G-slots mframe)))
           (dolist (S-slot (frame-S-slots mframe))
             (setf (S-slot-facets S-slot)
                   (remove-if #'temp-link-p (S-slot-facets S-slot))) )
           (setf (agent-flag mframe *t-link-flag*) 0)
           (when notify-p
             (establish-microframe-integrity mframe :t-link))
           T )))  ; there must have been at least one deletion


;;;;;;;  End of file DUAL/ARCHIT/LINKS.LSP
