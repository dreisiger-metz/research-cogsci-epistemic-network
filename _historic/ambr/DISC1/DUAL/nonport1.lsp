;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Common-LISP-user -*-

;;; FILE:       DUAL/nonport1.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Non-portable constructs and extensions. 
;;; DEPENDS-ON: NIL
;;;             This file is intended to be loaded before any other files. 
;;;             See also ARCHIT/NONPORT2.LSP
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    14-02-97 [1.0]
;;; UPDATED:    14-09-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2]  The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;      N O N - P O R T A B L E  (before)     ;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "COMMON-LISP-USER")

;;; The implementation of DUAL adheres to the ANSII Common Lisp standard.
;;; It steers away from the 'risky features' like the condition system,
;;; pretty printer, or logical pathnames. Although it heavily depends on
;;; CLOS, it makes no recourse to the meta-object protocol (except for
;;; the DUAL-interface, see file DUAL/INTRFACE/GFUN_SPY.LSP). Thus, I hope
;;; that it is easily portable. It should run on all ANSII- and CLtL2-
;;; compliant LISP implementations.
;;;
;;; The program does not use any non-portable forms. It does use, however,
;;; some deprecated ones like  (eval-when (COMPILE LOAD EVAL) ...) for the
;;; sake of backward compatibility. :TEST-NOT and -IF-NOT forms are avoided.
;;;
;;; So far, the program has been tested successfully on the following platforms:
;;;  -- Carnegie Mellon University Common Lisp (ver. 17f) augmented
;;;       with PCL implementation of CLOS
;;;  -- Allegro Common Lisp for Windows (ver. 3.0.2, standard edition)
;;;
;;; A port to Macintosh Common Lisp is forthcoming. If you port this program to
;;; other LISP implementations, please contact me at APETROV@COGS.NBU.ACAD.BG .
;;; In this way, your ports may be included to the 'official release' of DUAL.
;;;
;;; The tests revealed some inconsistencies of the aforementioned LISP
;;; implementations with the ANSII standard. This file contains fixes
;;; to those inconsistencies. See also DUAL/NONPORT2.LSP.
;;;
;;; The code below is conditionalized (via #+ and #-) with respect to the
;;; following features:
;;;   :CMU       -- Carnegie Mellon University Common Lisp
;;;   :ACLPC     -- Allegro Common Lisp for Windows, ver. 3.0.2 
;;;   :MACL      -- Macintosh Common Lisp
;;;


;;;;;;;;;;;;;;    C M U C L - D E P E N D E N T     ;;;;;;;;;;;;;;;;

;;; Arrange for the fact that CMU CLisp does not have its 'native' CLOS
;;; and uses the PCL implementation instead. Thus, some old symbols
;;; related to classes must be shadowed.

#+(and :CMU :PCL (not :CLOS))
(eval-when (compile load eval)
  (shadowing-import '(PCL:class-of           PCL:class-name
                      PCL:built-in-class     PCL:find-class
                      PCL:class-direct-superclasses
                      PCL::class             PCL::structure-class
                      PCL::standard-class)
                    (find-package "COMMON-LISP"))
  (export 'PCL::describe-object (find-package "PCL"))
)


;; The (generic) function FIND-METHOD seems to be undefined in CMUCL 17f.
;; Only the symbol is there (an external symbol in the COMMON-LISP package).
;; The fix below alleviates this problem.  Note that CLOS specification
;; postulates that FIND-METHOD is generic.  The definition below is taken
;; from the book of Kiczales et. al. (1991) "The Art of the Metaobject
;; Protocol", p.302.

#+(and :CMU :PCL (not :CLOS))
(unless (fboundp 'cl::find-method)
  (defun cl::find-method (gf qualifiers specializers &optional (errorp t))
    (let ((method (find-if #'(lambda (method)
                               (and (equal qualifiers
                                           (pcl:method-qualifiers method))
                                    (equal specializers
                                           (pcl:method-specializers method)) ))
                           (pcl:generic-function-methods gf))))
      (if (and (null method) errorp)
          (error "No such method for ~S."
                 (pcl:generic-function-name gf))
          method)))
)




;;;;;;;;;;;;;;    A L L E G R O - D E P E N D E N T     ;;;;;;;;;;;;;;;;

;; The ANSII Common Lisp standard has replaced the function SPECIAL-FORM-P
;; with SPECIAL-OPERATOR-P because the former is a misnomer.
;; Allegro Common Lisp (ver. 3.0.2), however, sticks to the older convention.
;; More precisely, its COMMON-LISP package uses SPECIAL-FORM-P while its
;; ALLEGRO package has also SPECIAL-OPERATOR-P. Put the latter into the
;; COMMON-LISP package.

#+:ACLPC
(eval-when (compile load eval)
  (let ((cl-symbol (find-symbol "SPECIAL-OPERATOR-P"
                                (find-package "COMMON-LISP")))
        (allegro-symbol (find-symbol "SPECIAL-OPERATOR-P"
                                     (find-package "ALLEGRO")))
        (*package* (find-package "COMMON-LISP")) )
    (cond ((null cl-symbol)    (import allegro-symbol))
          ((fboundp cl-symbol) nil)
          (t                   (shadowing-import allegro-symbol)) )
    (export (find-symbol "SPECIAL-OPERATOR-P"))
))


;; X3J13 voted to deprecate the use of :TEST-NOT keyword arguments and
;; of -IF-NOT sequence functions. It also voted to add the COMPLEMENT
;; function, intended to reduce or eliminate the need for these deprecated
;; features. The deprecated features are _not_ used in any DUAL programs.
;; In Allegro Common LISP, the symbol is present and exported from the
;; COMMON-LISP package but it has no global function definition.

#+:ACLPC
(unless (fboundp 'cl::complement)
  (defun cl::complement (fun)   ; see CLtL2, p.392
    (declare (type function fun))
    "Complement of a predicate."
    #'(lambda (&rest arguments)
        (not (apply fun arguments)))) 
)


;; The functions related to the Metaobject Protocol are not placed in the
;; COMMON-LISP package.  They are placed in the ALLEGRO package as external
;; symbols.  The intention is that COMMON-LISP-USER uses both COMMON-LISP
;; and ALLEGRO packages and thus has access to all symbols.
;;
;; The current implementation, however, has chosen _not_ to depend on the
;; symbols from ALLEGRO.  To enforce this, DUAL related packages do not use
;; it.  From that follows that they do not have access to the symbols related
;; to the Metaobject Protocol.  The patch below imports some of these symbols
;; into the COMMON-LISP package.
;;
;; The Metaobject Protocol is not heavily used in the current implementation.
;; At present, the only file that explicitly uses MOP is DUAL/INTRFACE/SPY.LSP.
;; The patch below manipulates only symbols that (i) are used in the program,
;; (ii) are not exported from the COMMON-LISP package, and (iii) are related
;; to MOP.  The file DUAL/INTRFACE/SPY.LSP uses other MOP functions (namely
;; ADD-METHOD, REMOVE-METHOD, and FIND-METHOD) but they are considered part
;; of CLOS and are thus placed in the COMMON-LISP package in Allegro CL 3.0.2.

#+:ACLPC
(let ((MOP-symbols '("GENERIC-FUNCTION-LAMBDA-LIST"
                     "EXTRACT-SPECIALIZER-NAMES"     ))
      (*package* (find-package "COMMON-LISP")) )
  (dolist (s MOP-symbols)
    (import (find-symbol s (find-package "ALLEGRO")))
    (export (find-symbol s)) ))

;; Moreover, it turns out that the function EXTRACT-SPECIALIZER-NAMES is not
;; defined in ACL 3.0.2.  Only the symbol is exported from the ALLEGRO package.
;; The function is defined here.  See p.189 in "The Art of the Metaobject
;; Protocol" by Kiczales et al. (1991).

#+:ACLPC
(defun allegro:extract-specializer-names (specialized-lambda-list)
  "Takes a specialized lambda list and returns its specializer names."
  (let ((result nil))
    (dolist (arg specialized-lambda-list)
      (when (member arg lambda-list-keywords)
        (return))   ; required arguments ended -- break the loop
      (etypecase arg
        (list    (push (second arg) result))    ; (var class-name)
        (symbol  (push 't result)) ))           ; var  -->  (var T)
    (nreverse result) ))


;; ACL for Windows uses what it calls 'the super-paren feature'.
;; Namely, the square brackets are defined so that
;;   (((a) (b c]   reads   (((a) (b c)))
;; In DUAL, the brackets are used to denote 'suspendable forms' (see
;; DUAL/ARCHIT/BRACKETS.LSP for details).
;; Therefore, we need to disable the super-paren feature:

#+:ACLPC
(eval-when (compile load eval)
  (set-syntax #\[ 'constituent)
  (set-syntax #\] 'constituent)
)


;;;;;;;;;;;;    M A C I N T O S H - D E P E N D E N T     ;;;;;;;;;;;;;;

;; Nothing yet


;;;;; End of file DUAL/NONPORT1.LSP;  see also DUAL/NONPORT2.LSP
