;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/nonport2.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Non-portable constructs and extensions.
;;; DEPENDS-ON: DUAL/nonport2.lsp; DUAL/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    19-05-97 [1.0]
;;; UPDATED:    14-09-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2]  The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;      N O N - P O R T A B L E  (after)     ;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;; This file put some implementation-specific patches into the DUAL-CORE
;;; package.  See the file DUAL/NONPORT1.LSP for more details.
;;; (NONPORT1.LSP is intended to be loaded first, followed by PACKAGES.LSP
;;;  and finally NONPORT2.LSP.)


;;;;;;;;;;;;;;    C M U C L - D E P E N D E N T     ;;;;;;;;;;;;;;;;

;; The CMU Common Lisp compiler -- Python -- does extensive type checking.
;; Therefore, it encourages the use of TYPE declarations. It supports the
;; VALUES declaration as an extension to Common Lisp. This declaration is
;; semantically equivalent to a THE form wrapped around the body of the
;; special form in which the VALUES declaration appears. For instance:
;;
;; (defun foo (x)                  ==      (defun foo (x)
;;   (declare (values list))       ==        (the list
;;   <body...> )                   ==          <body...>  ))
;;
;; (defun baz (y)                  ==      (defun baz (y)
;;   (declare (type symbol y)      ==        (declare (function baz (symbol)
;;            (values cons  ))     ==                    cons))
;;   <body...> )                   ==        <body...> )
;;
;; Since VALUES is an implementation-dependent declaration specifier, it
;; is not recognized by other Lisp compilers. The proclamation below
;; inhibits compiler warnings in these cases.

#-:CMU (proclaim '(declaration values))
#+(and :CMU :PCL) (pushnew 'values PCL::*non-variable-declarations*)


;; Under the same heading of extensive type checking, CMU CLisp defines
;; the function REQUIRED-ARGUMENT which is used as the default value for
;; keyword arguments that must always be supplied. Since it is known to the
;; compiler to never return, it will avoid any compile-time type errors that
;; will result from a default value inconsistent with the declared type. When
;; this function is called, it signals an error indicating that a required
;; keyword argument was not supplied. This function is also useful for slot
;; :INITFORMs.

#-:CMU (defun required-argument ()
         (error "A required initarg or keyword-arg was not supplied."))

;; #+:CMU (import 'extensions:required-argument)
;; Replaced with a clause in defpackage. See PACKAGES.LSP.


;; CMU CLisp does not have its 'native' CLOS. It uses a Portable Common Loops
;; implementation of CLOS instead. It seems that the latter does not interact
;; well with DESCRIBE. In particular, the generic function DESCRIBE-OBJECT is
;; undefined and DESCRIBE does not refer to it. As a consequence, user-supplied
;; methods for DESCRIBE-OBJECT do not gain control.
;; I provide a simple definition of DESCRIBE-OBJECT here as well as a patch
;; DESCRIBE1. See also DUAL-DESCRIBE (in GENERAL.LSP) and #\? macro character.

#+(and :CMU :PCL)
(eval-when (compile load eval)
  (defun describe1 (object &optional (stream-designator *standard-output*))
    "A patch to DESCRIBE that goes through DESCRIBE-OBJECT."
    (pcl::describe-object object (case stream-designator
                                   ((nil) *standard-output*)
                                   ((t)   *terminal-io*)
                                   (t     stream-designator))) )
) ; eval-when



;;;;;;;;;;;;;;    A L L E G R O - D E P E N D E N T     ;;;;;;;;;;;;;;;;

; See also  VALUES and REQUIRED-ARGUMENT in the CMU section above


;;;;;;;;;;;;    M A C I N T O S H - D E P E N D E N T     ;;;;;;;;;;;;;;

; See also  VALUES and REQUIRED-ARGUMENT in the CMU section above


;;;;; End of file DUAL/NONPORT2.LSP;  see also DUAL/NONPORT1.LSP
