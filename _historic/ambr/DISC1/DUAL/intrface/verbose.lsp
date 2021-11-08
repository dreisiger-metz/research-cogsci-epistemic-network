;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       dual/intrface/verbose.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Specialized spy tools for generating verbose execution traces.
;;; DEPENDS-ON: DUAL/archit/*.lsp, DUAL/intrface/gfun_spy.lsp, =/spy_tool.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    19-05-97
;;; UPDATED:    24-03-98 [1.1.2] -- Use spy tools (DUAL/INTRFACE/SPY_TOOL.LSP).
;;;                         Verbose templates now encapsulated in spy tools.
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...
;;;;;;;;;
;;; TO DO:      Document the external protocol.


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;;       V E R B O S E   SPY  TOOLS      ;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package  "DUAL-INTERFACE")


;;;; The main concept defined in this file is VERBOSE-TOOL -- a kind of
;;;; spy tool specialized for generating verbose execution traces.
;;;;
;;;; ...
;;;;
;;;; See DUAL/INTRFACE/GFUN_SPY and =/SPY_TOOL.LSP for the spy mechanism.
;;;; See DUAL/INTRFACE/SET_SPY.LSP for individual spy settings.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: verbose-tool, make-verbose-tool, find-verbose-tool ;
;;          verbose, verbose-status ;
;;          *verbose-stream*, print-verbose-header


;; VERBOSE-TOOL
;;
;;   A symbol that is the proper name of the class of verbose tools.
;;   As such, it is a valid type specifier.
;;   The class VERBOSE-TOOL is a subclass of the class SPY-TOOL.


;; MAKE-VERBOSE-TOOL (name &key gfun-name lambda-list cerror-p) --> verbose-tool
;;
;;   A function that makes and registers a verbose tool...
;;
;;   Returns an implementation-dependent object representing the new verbose tool.
;;   ...
;;
;;   The call to MAKE-VERBOSE-TOOL is usually followed immediately by a call to
;;   INSTALL-SPY-TOOL.  See file DUAL/SET_SPY.LSP for examples.
;;

;; FIND-VERBOSE-TOOL (name)  -->  verbose-tool or NIL
;;
;;   A function that looks for a verbose tool with a given name.
;;
;;   ...


;; VERBOSE  (tool-name template &optional operation activate-p)  -->
;;                                                       -->  new-template
;;
;;   A function that sets the template of the spy-tool with TOOL-NAME.
;;
;;   TEMPLATE should be T, NIL, a coalition, a symbol naming a class, or a
;;     list of such symbols or coalitions.
;;     (See CHANGE-SPY-TEMPLATE in DUAL/INTRFACE/SPY_TOOL.LSP.)
;;   OPERATION should be one of the following:
;;    + :SET    -- the template is set to TEMPLATE
;;    + :ADD    --
;;    + :REMOVE --
;;    + :AUTO
;;
;;   ...   (See CHANGE-SPY-TEMPLATE in DUAL/INTRFACE/SPY_TOOL.LSP.)


;; VERBOSE-STATUS  (name)  -->  (values  ....)
;;
;;   A function that ....
;;   ...


;; *VERBOSE-STREAM*
;;
;;   A global variable ....
;;   By default bound to *STANDARD-OUTPUT*

;; PRINT-VERBOSE-HEADER ()  -->  NIL
;;
;;   A function that prints to *VERBOSE-STREAM* some implementation-dependent
;;   string preparing the ground for verbose messages.
;;   Individual verbose methods are advised to begin with a call to PRINT-
;;   VERBOSE-HEADER.  This ensures that the message will begin on a new line
;;   and that current time (the value of the variable *TIME*) will be printed.
;;
;;   Example: The code fragment:
;;      (print-verbose-header)
;;      (format *verbose-stream* "an event ~S happened." event-foo)
;;    will output to *verbose-stream*, beginning on a new line, something like:
;;      ;; T=30.50, an event <event foo> happened.

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;



;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;;;  Class definition

(eval-when (compile load eval)
  (defclass  verbose-tool  (spy-tool)
   ()   ; All slots inherited from SPY-TOOL.
   (:documentation "Spy tool for generating verbose execution traces." ))
) ; eval-when

;;;;;  The identifier of all verbose tools is the symbol 'VERBOSE-TOOL.

(defmethod spy-tool-identifier ((verbose-tool verbose-tool))
  'verbose-tool)


;;;;;;;;  Housekeeping functions and constructor
;;
;;  There is a hash table indexed by verbose-tool names.

(defvar  *verbose-table*
         (make-hash-table  :size  20
                           :rehash-size 1.5
                           :test  #'eq )          ; Keys are symbols
         "Internal hash table for the purposes of the 'verbose' mechanism." )

(defun find-verbose-tool  (name)
  "Look for a verbose tool with a given name."
  (declare (type symbol name)
           (values (or null verbose-tool)) )
  (values (gethash name *verbose-table*)) )


(defun make-verbose-tool (name &key (gfun-name name)
                                    (lambda-list T) (cerror-p nil) )
  (declare (type symbol name gfun-name)
           (values verbose-tool) )
  (let ((tool (find-verbose-tool name)))
    (cond ((null tool)
              (setf (gethash name *verbose-table*)
                    (make-spy-tool name 'verbose-tool
                                   gfun-name  :lambda-list lambda-list
                                              :cerror-p cerror-p)) )
          (t  (when cerror-p
                (cerror "Continue using the old verbose tool."
                        "A verbose tool named ~S already exists."
                        name))
              tool)) ))


(defmethod  uninstall-spy-tool :after ((verbose-tool verbose-tool)
                                       &optional (deactivate-p T))
  (declare (ignore deactivate-p))
  (remhash (spy-tool-name verbose-tool) *verbose-table*) )



;;;;;   The function VERBOSE provides easy access to the verbose template.

(defun  verbose (name template &optional (operation :auto)
                                         (activate-p T) )
  "Change the template for the verbose tool with a given name."
  (declare (type (member :auto :set :add :remove :null) operation)
           (values (or symbol list)) )   ; new-template or :NOT-FOUND
  (let ((verbose-tool (find-verbose-tool name)))
    (if (null verbose-tool)
      :not-found
      (let ((old-template (spy-tool-template verbose-tool)))
        (when (eq operation :auto)  ; Resolve :AUTO operations.
          (cond ((eq template T)    (setq operation :set))
                ((eq template nil)  (setq operation :set))
                ((listp template)   (setq operation :set))
                (t                  (setq operation :add)) ))  ; bare item
        (change-spy-template verbose-tool
                    (combine-spy-templates old-template template operation)
                    activate-p)) )))


;;;;;;;;  Inspecting and reporting the status of a verbose tool.
;;
;; Compare with SPY-STATUS and REPORT-SPY-STATUS in DUAL/INTRFACE/GFUN_SPY.LSP.

(defun verbose-status (name)
  "Check the status of a verbose tool. Returns an indexed list."
  (declare (type symbol name)
           (values list) )
  (let ((verbose-tool (find-verbose-tool name)))
    (if (null verbose-tool)
        nil
        (multiple-value-bind (individual-active-p generic-active-p)
                             (spy-tool-active-p verbose-tool)
          (let ((individual-spy (spy-tool-indiv-spy verbose-tool)))
            (list :template   (spy-tool-template verbose-tool)
                  :qualifier  (spy-qualifier individual-spy)
                  :comment    (spy-comment   individual-spy)
                  :individual-active-p individual-active-p
                  :generic-function    (gfun-spy-name
                                             (spy-tool-gfun-spy verbose-tool))
                  :generic-active-p    generic-active-p ))) )))


;;;;;;;;;   Providing a special-purpose verbose stream.

(defvar *verbose-stream*  *standard-output*
  "The stream to which 'verbose' messages are directed." )

(defun print-verbose-header ()
  (format *verbose-stream* "~&;; T=~,2F, " *time*))


;;;;;;; End of file DUAL/INTRFACE/VERBOSE.LSP
