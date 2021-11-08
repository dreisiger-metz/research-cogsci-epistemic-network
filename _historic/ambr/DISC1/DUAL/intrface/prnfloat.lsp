;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/prnfloat.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Patch to PRINT-OBJECT specifying default printing for FLOATs.
;;; DEPENDS-ON: DUAL/intrface/defs.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    05-05-97 [1.0]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...


;; SYMBOLS:  <none>
;;
;; See  *PRINT-FLOAT-DIRECTIVE*  in INTRFACE/DEFS.LSP
;;

(cl:in-package "DUAL-INTERFACE")

(defmethod print-object :around ((fl float) (stream t))
  (if (and (boundp '*print-float-directive*)
           (stringp *print-float-directive*) )
      (let* ((directive *print-float-directive*)
             (*print-float-directive* nil) )  ; avoid recursive call by FORMAT
        (format stream  directive  fl)
        fl )
      (call-next-method) ))   ; call the system-supplied method

;;;; End of file DUAL/INTRFACE/PRNFLOAT.LSP
