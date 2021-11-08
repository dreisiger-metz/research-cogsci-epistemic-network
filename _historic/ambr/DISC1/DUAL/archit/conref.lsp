;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/conref.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Support for connectionist references (conref's).
;;; DEPENDS-ON: DUAL/archit/basic.lsp, DUAL/archit/symref.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    17-06-97 [1.0]
;;; UPDATED:    09-11-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;
;;; TO_DO:      (deftype conref () '(cons symref float))   in a portable manner


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;     C O N N E C T I O N I S T    R E F E R E N C E S      ;;;;;;
  ;;;;;;               /*  (symref . weight)  */                   ;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is CONREF ('connectionist reference').
;;;; The file defines functions for constructing and dealing with conrefs and
;;;; lists of conrefs.
;;;;
;;;; A conref is a data structure that keeps a symbolic reference (a link)
;;;; to some agent together with a numeric weight of that link.
;;;; The link itself is represented as a 'symref' (see DUAL/ARCHIT/SYMREF.LSP).
;;;; Symrefs are of two kinds: 'simple' (agent only) and 'extended' (ag.slot).
;;;;
;;;;  Conceptually, CONREF is a data type with two fields:
;;;;    -- REFERENCE -- a symref
;;;;    -- WEIGHT    -- a single-float


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: make-conref, copy-conref,
;;          conref-reference, conref-weight, conref-p
;;          adjoin-conref, normalize-weights

;; MAKE-CONREF (symref weight)  -->  conref
;;
;;   A function that creates a conref given its two fields.
;;   SYMREF should be a symbolic reference, WEIGHT should be a number.

;; COPY-CONREF (original)  -->  new-conref
;;
;;   A function that makes a copy of a conref.  The new conref is equivalent
;;   (i.e. satisfies CONREF-EQUAL) to the original but shares no structure
;;   with it.

;; CONREF-REFERENCE (conref)  -->  symref
;;
;;   A function that returns the REFERENCE field of a conref.

;; CONREF-WEIGHT  (conref)  -->  weight
;;
;;   A function that returns the WEIGHT field of a conref. It is a single-float.

;; CONREF-P (thing)  -->  T or NIL
;;
;;   A function that checks whether something is a conref.

;; CONREF-MATCH  (conref1 conref2)  -->  T or NIL
;;
;;   A predicate that returns T if the two conrefs match and NIL otherwise.
;;   They 'match' if their reference fields satisfy SYMREF-MATCH, that is, if
;;   they point to the same agent, slots notwithstanding (see ARCHIT/SYMREF.LSP)

;; CONREF-EQUAL  (conref1 conref2)  -->  T or NIL
;;
;;   A predicate that returns T if the two conrefs are equal and NIL otherwise.
;;   They are 'equal' if their reference fields satisfy SYMREF-EQUAL and
;;   their weight fields satisfy =.


;; ADJOIN-CONREF (conref conref-list &key :priority :order :destructive-p)  -->
;;                                  -->  (values new-conref-list change-flag)
;;
;;   A function for pushing a conref into a list of conrefs, provided that
;;   it is not already a member.
;;
;;   CONREF should be a connectionist reference.
;;   CONREF-LIST should be a (possibly empty) list of conrefs.
;;   PRIORITY should take one of the following values: :NEW, :OLD, or a
;;     function that takes two arguments of type single-float and returns
;;     a single-float. PRIORITY defaults to :NEW.
;;   ORDER should take one of the two values :FIFO and :LIFO. Defaults to :FIFO.
;;   DESTRUCTIVE-P should be either T or NIL. It defaults to NIL.
;;   The function returns two values: (i) the new conref list and (ii) a
;;     boolean value indicating whether it differs from the original list.
;;
;;   ADJOIN-CONREF works according to the following algorithm:
;;
;;     1. CONREF is checked against the elements of CONREF-LIST using the
;;        predicate CONREF-MATCH.
;;     2. If there is no match, CONREF is incorporated into the list (as
;;        described below) and the resulting list is returned. It is one
;;        element longer than the initial list. The second value returned is T.
;;     3. If there is a match, i.e. there is some OLD-CONREF in the list which
;;        satisfies CONREF-MATCH with the new CONREF, then there is collision.
;;        In such case, the action taken depends on the value of :PRIORITY :
;;         -- :OLD -- the old conref is retained and CONREF-LIST is
;;                    returned without change. In other words, the whole call
;;                    to ADJOIN-CONREF is a void operation. This is idicated
;;                    by NIL in the second value.
;;         -- :NEW -- the new conref is substituted for the old one in CONREF-
;;                    LIST (as described below) and the resulting list is
;;                    returned.  The second value returned is T.
;;         -- FUN  -- a new conref is constructed in a manner described below.
;;                    It is then substituted for OLD-CONREF in CONREF-LIST
;;                    and the resulting list is returned. Second value -- T.
;;        In all cases when there is a match, the length of NEW-CONREF-LIST
;;        is the same as the length of the original CONREF-LIST.
;;     When :DESTRUCTIVE-P is bound to NIL (the default), ADJOIN-CONREF
;;     guarantees that CONREF-LIST will never be modified destructively.
;;
;;     To 'incorporate' a new CONREF to CONREF-LIST (at step 2 above):
;;      -- if :ORDER is :LIFO then CONREF is consed to the front of the list;
;;      -- if :ORDER is :FIFO then CONREF is appended to the back of the list
;;         (via APPEND or NCONC depending on the value of :DESTRUCTIVE-P).
;;
;;     Substitution of a new conref for a matching old one (at step 3 above)
;;      is done via SUBSTITUTE or NSUBSTITUTE depending on :DESTRUCTIVE-P.
;;
;;     When :PRIORITY is bound to a function, a new conref is constructed:
;;      -- with REFERENCE field obtained as follows:
;;           (let ((old-symref (conref-reference old-conref))
;;                 (new-symref (conref-rererence new-conref)))
;;             (if (symref-equal new-symref old-symref)
;;                 new-symref
;;                 (simplify-symref new-symref)))   ; the 'common denominator'
;;      -- and with WEIGHT field obtained as follows:
;;           (funcall priority (conref-weight new-conref)
;;                             (conref-weight old-conref))
;;
;;   Examples (only the first value is shown):
;;     (adjoin-conref '(x.1) nil)                    -->  ((x.1))
;;     (adjoin-conref '(x.1) '((y.0)) :order :lifo)  -->  ((x.1) (y.0))
;;     (adjoin-conref '(x.1) '((y.0)) :order :fifo)  -->  ((y.0) (x.1))
;;     (adjoin-conref '(x.1) '((y.0)))               -->  ((y.0) (x.1))
;;     (adjoin-conref '(x.1) '((x.0) (y.0)))         -->  ((x.1) (y.0))
;;     (adjoin-conref '(x.1) '((y.0) (x.1)))         -->  ((y.0) (x.1))
;;     (adjoin-conref '(x.1) '((y.0) (x.0)))         -->  ((y.0) (x.1))
;;     (adjoin-conref '(x.1) '((y.0) (x.0)) :priority :old)  -->  ((y.0) (x.0))
;;     (adjoin-conref '(x.1) '((y.0) (x.0)) :priority :new)  -->  ((y.0) (x.1))
;;     (adjoin-conref '(x.1) '((y.0) (x.0)) :priority #'max) -->  ((y.0) (x.1))
;;     (adjoin-conref '(x.sl1 . 1) '((x.sl1 . 0)) :priority #'max)  -->
;;                                                          -->  ((x.sl1 . 1))
;;     (adjoin-conref '(x.sl1 . 1) '((x.sl2 . 0)) :priority #'max)  -->
;;                                                          -->  ((x . 1))
;;     (let ((foo '((y.0))))                      -->  (((y.0) (x.1))
;;       (list (adjoin-conref '(x.1) foo) foo))         ((y.0)) )
;;     (let ((foo '((y.0))))
;;       (list (adjoin-conref '(x.1) foo          -->  (((y.0) (x.1))
;;                      :destructive-p t) foo))         ((y.0) (x.1)) )
;;

;; NORMALIZE-WEIGHTS  (conref-list &key :sum :destructive-p)  -->
;;                                                -->  normalized-conref-list
;;
;;   A function that takes a list of connctionist references and normalizes
;;   the weights so that the sum of their absolute values equals SUM.
;;   CONREF-LIST should be a list of conref's.
;;   :SUM should be a positive single-float; it defaults to 1.0 .
;;   :DESTRUCTIVE-P should be T or NIL; it defaults to NIL.  When NIL, a
;;     freshly consed list is returned; otherwise the weights are spliced
;;     into the old cells.
;;
;;   In the pathological case when all weights in the initial list are zero,
;;   NORMALIZE-WEIGHTS signals an error.
;;
;;   Examples:
;;     (normalize-weights '((x . 1.0) (y . 0.6) (z -0.4)))    -->
;;                                       -->  ((x . 0.5) (y . 0.3) (z . -0.2))
;;     (normalize-weights '((x . 0.8) (y . -0.7)) :sum 0.75)  -->
;;                                       -->  ((x . 0.4) (y . -0.35))
;;     (normalize-weights nil)                     -->  nil
;;     (normalize-weights '((x . 0.0) (y . 0.0)))  -->  error
;;     (normalize-weights some-list :sum -1.0)     -->  error
;;
;;   See also the functions NORMALIZE-NEIGHBORS defined in DUAL/ARCHIT/CONNNECT
;;   and NORMALIZE-WM-WEIGHTS defined in DUAL/ARCHIT/WORK_MEM.LSP.

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

;;  In this implementation, CONREFs are kept in cons cells.
;;  Among other things, this makes lists of conrefs equivalent to a-lists.

(declaim (inline  make-conref
                  copy-conref
                  conref-reference
                  conref-weight   ))

(defun  make-conref  (symref weight)
  "Constructs and returns a connectionist reference."
  (declare (type number weight))
  (cons symref (coerce weight 'single-float)))

(defun  conref-reference  (conref)
  "The REFERENCE field of a connectionist reference."
  (car conref))

(defun  conref-weight  (conref)
  "The WEIGHT field of a connectionist reference."
  (declare (values single-float))
  (cdr conref))

(defun  conref-p (thing)
  (declare (values boolean))
  (and (consp thing)
       (symref-p (car thing))
       (floatp   (cdr thing)) ))

(defun  copy-conref (original)
  "Makes an identical copy of a connectionist reference."
  (declare (inline make-conref conref-reference conref-weight))
  (make-conref (conref-reference original) (conref-weight original)) )

(defun conref-match (cr1 cr2)
  "A predicate that returns T if the two conrefs 'match' and NIL otherwise."
  (declare (type cons cr1 cr2)
           (values boolean))
  (symref-match (conref-reference cr1)
                (conref-reference cr2)))

(defun conref-equal (cr1 cr2)
  "A predicate that returns T if the two conrefs are equal and NIL otherwise."
  (declare (type cons cr1 cr2)
           (values boolean))
  (and (symref-equal (conref-reference cr1) (conref-reference cr2))
       (= (conref-weight cr1) (conref-weight cr2)) ))

(defun destructively-modify-weight (conref new-weight)
  ;; Not advertized in the external protocol.
  (declare (type cons conref)
           (type number new-weight) )
  (setf (cdr conref) (coerce new-weight 'single-float))
  conref )


(defun  adjoin-conref (new-conref conref-list
                       &key (priority :new) (order :fifo) (destructive-p nil) )
  "Adding a new conref into the list, provided it is not already a member."
  (declare (type cons new-conref)
           (type list conref-list)
           (type (or (member :new :old) function)  priority)
           (type (member :fifo :lifo) order)
           (values list boolean) )
  (let ((old-conref (find new-conref conref-list :test #'conref-match) ))
    (flet ((combine-conrefs ()
             (let ((new-symref (conref-reference new-conref))
                   (old-symref (conref-reference old-conref))
                   (new-weight (funcall priority (conref-weight new-conref)
                                                 (conref-weight old-conref))) )
               (if (symref-equal new-symref old-symref)
                   (make-conref new-symref new-weight)
                   (make-conref (simplify-symref new-symref)  ; common denomintr
                                new-weight)) ))
           (incorporate-new-conref ()
             (cond ((eq order :lifo) (cons new-conref conref-list))
                   ((eq order :fifo) (if destructive-p
                                       (nconc conref-list (list new-conref))
                                       (append conref-list (list new-conref))))
                   (t (error "ADJOIN-CONREF: ~S is neither :FIFO nor :LIFO."
                             order)) ))
           (substitute-new-for-old (new)
             (if destructive-p
                 (nsubstitute new old-conref conref-list)
                 (substitute new old-conref conref-list))) )
      ;; Main body of ADJOIN-CONREF
      (cond ((null old-conref)                         ; no collisions
                (values (incorporate-new-conref) t))
            ((eq priority :old)                        ; no change
                (values conref-list nil))
            ((eq priority :new)
                (values (substitute-new-for-old new-conref) t))
            ((functionp priority)
                (values (substitute-new-for-old (combine-conrefs)) t))
            (t (error "ADJOIN-CONREF: bad :PRIORITY argument -- ~S."
                      priority))) )))


(defun normalize-weights (conref-list &key (sum 1.0) (destructive-p nil))
  "Normalizes the weights so that the sum of their absolute values = SUM."
  (declare (type list conref-list)
           (type single-float sum)
           (values list) )
  (flet ((sum-weights (&aux (s 0.0))
           (dolist (cr conref-list)
             (incf s (abs (conref-weight cr))))
           s)
         (multiply-weights (coef)
           (if destructive-p
               (dolist (cr conref-list conref-list)
                 (destructively-modify-weight cr (* coef (conref-weight cr))))
               (mapcar #'(lambda (cr) (make-conref (conref-reference cr)
                                                   (* coef (conref-weight cr))))
                       conref-list))) )
    ;; Main body of NORMALIZE-WEIGHTS
    (let ((old-sum (sum-weights)))
      (cond ((null conref-list) nil)
            ((< sum 0.001) (error "NORMALIZE-WEIGHTS: ~S is negative." sum))
            ((< old-sum 0.001)
                      (error "NORMALIZE-WEIGHTS: all weights in ~S are zeros."
                             conref-list))
            (t (multiply-weights (/ sum old-sum))) ))))


;;;;;;;  End of file DUAL/ARCHIT/CONREF.LSP
