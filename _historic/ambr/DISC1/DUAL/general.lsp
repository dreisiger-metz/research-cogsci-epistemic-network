;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/general.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Some fundamentals for DUAL's implementation
;;; DEPENDS-ON: DUAL/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    14-03-97 [1.0]
;;; UPDATED:    15-09-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2]  The 'official release'
;;; UPDATED:    ...

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;     GENERAL  CLASSES  AND  METHODS  FOR  DUAL     ;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package  "DUAL-CORE")


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: DUAL-object, temporary-DUAL-object,
;;          DUAL-describe,
;;          boolean,
;;          union1


;; UNION1 (list1 list2 &key test test-not key)  -->  union-list
;;
;;  A function that computes the union of LIST1 and LIST2.
;;
;;  UNION1 complies to the standard for the Common Lisp function UNION.
;;  (See p.428 in CLtL2.)  The standard, however, says that "There is
;;  no guarantee that the order of elements in the result will reflect
;;  the ordering of the arguments in any particualar way.  The imple-
;;  mentation is therefore free to use any of a variety of strategies."
;;
;;  The order of elements in the result of UNION1 is implementation-
;;  dependent too.  However, it is dependent on the implementation of
;;  DUAL and _not_ on the implementation of Common Lisp.  Thus, it is
;;  highly recommended to use UNION1 instead of UNION.  In this way
;;  the behavior of DUAL-based models will depend less on the Common
;;  Lisp platform that runs it.


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

(deftype  boolean ()  '(member t nil))

(eval-when (compile load eval)
  (defclass DUAL-object ()
    ()                    ; No slots; this class is a hook for methods.
    (:documentation "The common superclass of all DUAL classes." ))

  (defclass temporary-DUAL-object (DUAL-object)
    () )                  ; No slots; this class is a hook for methods.
) ; eval-when

(defgeneric DUAL-describe  (object  &optional stream)
  (:documentation "DUAL-specific substitute for DESCRIBE-OBJECT." ))
  ;; See also the #\? macro character in DUAL/UTILS.LSP

(defmethod  DUAL-describe  ((object DUAL-object)
                            &optional (stream *standard-output*))
  (describe-object object stream) )

(defmethod  DUAL-describe  ((thing t)
                            &optional (stream *standard-output*))
  (describe thing stream) )


(defun union1 (list1 list2 &key (test #'eql) (key #'identity))
  "Stable implementation of Common Lisp primitive UNION."
  (declare (type list list1 list2)
           (type (or null function) test key)
           (values list) )
  (let ((result list1))
    (dolist (elt list2 result)
      (pushnew elt result :test test :key key))))

#|  CMU version (needed because CMUCL did not handle keyargs very well).
    ;; More precisely, the macroexpansion of PUSHNEW generated a call like
    ;; this:   (adjoin elt result :test NIL ...) , NIL is not a function...
(defun union1 (list1 list2)   ; keyargs not allowed
  "Stable implementation of Common Lisp primitive UNION."
  (declare (type list list1 list2)
           (values list) )
  (let ((result list1))
    (dolist (elt list2 result)
      (pushnew elt result))))

|#

;;;;;;;  End of file DUAL/GENERAL.LSP
