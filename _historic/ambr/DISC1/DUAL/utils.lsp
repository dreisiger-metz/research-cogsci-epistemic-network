;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-INTERFACE -*-

;;; FILE:       DUAL/utils.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Defines some useful functions and macros.
;;; DEPENDS-ON: DUAL/packages.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    17-02-97 [1.0]
;;; CREATED:    15-09-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2]  The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;     U  T  I  L  I  T  I  E  S      ;;;;;;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl:in-package  "DUAL-INTERFACE")

;; SYMBOLS: causes-an-error-p
;;          make-obsolete
;;          prompt-and-read
;;          =~=
;;          squared-difference
;;          set-equal
;;          find-asymptote
;; Note also the \? macro character.

;; This macro is useful for debugging, in particular with ASSERT.
(defmacro  causes-an-error-p  (form)
  "Returns non-NIL iff evaluation of FORM causes an error."
  `(multiple-value-bind  (resulting-value condition)
                         (ignore-errors (values ,form))
     (declare (ignore resulting-value))
     condition))


;; ?<...> is a shorthand for (DUAL-DESCRIBE <...>)  ; see DUAL/GENERAL.LSP
(defun \?-reader (stream char)
  (declare (ignore char))
  (let ((thing (eval (read stream t nil t))))
    (DUAL-describe thing) ))
(set-macro-character #\? #'\?-reader)


;; MAKE-OBSOLETE alters a function definition so that the new definition
;; signals an error. This is useful when a name of a function is changed
;; but the old name has not been replaced everywhere in the source.
;; (See p.870 in Norvig (1992))
(defun make-obsolete (fn-name)
  "Print an error if an obsolete function is called."
  (setf (symbol-function fn-name)
        #'(lambda (&rest args)
            (declare (ignore args))
            (error "~&Obsolete function: ~S." fn-name))))


;; PROMPT-AND-READ prints some prompt and then reads user input.
;; (See p.867 in Norvig (1992))
(defun prompt-and-read (ctl-string &rest args)
  "Print a prompt and read a reply."
  (apply #'format t ctl-string args)
  (finish-output)
  (read) )


(defun =~= (x y &optional (tolerance 0.001))
  "Numerical equality test with tolerance."
  (< (abs (- x y)) tolerance))

(defun squared-difference (x y)
  "Computes the squared difference of two numbers."
  (declare (type number x y)
           (values number) )
  (let ((diff (- x y)))
    (* diff diff) ))

(defun set-equal (set1 set2)
  "Predicate that tests whether two sets are equal."
  (declare (type list set1 set2)
           (values boolean))
  (and (subsetp set1 set2 :test #'equal)
       (subsetp set2 set1 :test #'equal)))


;; FIND-ASYMPTOTE looks for the asymptote level of a given activation
;; function starting from some given initial level A0 and fixed INPUT.
;; It terminates either when two successive values are closer than
;; :TOLERANCE or when :CYCLES is exceeded. In the latter case the
;; function returns ':NO-CONVERGENCE.
(defun  find-asymptote (fun a0 input &key (tolerance 0.00001)
                                          (cycles 1000) )
  "Asymptote level of a given activation FUN for fixed INPUT."
  (let ((old-act a0)
        (new-act (funcall fun a0 input)))
    (dotimes (i cycles ':no-convergence)
      (when (=~= old-act new-act tolerance)      ; convergence?
         (return new-act))
      (setf old-act new-act)
      (setf new-act (funcall fun old-act input))
)))


;;  COUNT-SYMBOLS counts all symbols owned by a package.
;;  This is useful, for instance, to determine what value to supply
;;  as DEFPACKAGE's :SIZE.
#|
(defun count-symbols (package &optional (external-only nil) &aux (count 0))
  "Counts all symbols owned by PACKAGE."
  (flet ((count-symbol (symbol)
           (when (eq (symbol-package symbol)
                     package)
             (incf count)) ))
    (if external-only
        (do-external-symbols (symbol package)
          (count-symbol symbol))
        (do-all-symbols (symbol package)
          (count-symbol symbol)) )
    count))

|#


;;  TRANSLATE-CR/LF is useful when porting text files from MS-DOS to UNIX and
;;  back. When in DOS->UNIX mode, it substitutes each occurence of the two
;;  character sequence CR/LF (hex 0D 0A) with single LF.  Conversely, the
;;  UNIX->DOS mode transforms each LF into a CR/LF pair.  (This latter
;;  conversion is most conveniently done by using WinZIP open TAR files.)
(defun translate-CR/LF  (input-filename output-filename
                         &optional (mode :DOS->UNIX) )
  "Translates  CR/LF <--> LF  for porting ASCII files b/n DOS and UNIX."
  (with-open-file (in input-filename :direction :input)
    (with-open-file (out output-filename :direction :output)
      (ecase mode
        (:DOS->UNIX  (translate-CR/LF-->LF in out))
        (:UNIX->DOS  (translate-LF-->CR/LF in out)) ))))

(defun translate-LF-->CR/LF (input-stream output-stream)
  (let ((eof-marker :eof))
    (do ((char (read-char input-stream nil eof-marker)
               (read-char input-stream nil eof-marker)))
        ((eq char eof-marker) :done)
      (if (eq char #\Linefeed)
          (progn 
            (write-char #\Return   output-stream)
            #-:ACLPC    ;; Allegro CLisp/Win ouputs CR/LF on #\Return.
              (write-char #\Linefeed output-stream))
          (write-char char output-stream)) )))

(defun translate-CR/LF-->LF (input-stream output-stream)
  (let ((eof-marker :eof))
    (do ((char (read-char input-stream nil eof-marker)
               (read-char input-stream nil eof-marker)))
        ((eq char eof-marker) :done)
      (if (and (eq #\Return char)
               (eq #\Linefeed (peek-char nil input-stream nil eof-marker)))
          nil  ; skip CR and loop to LF
          (write-char char output-stream)) )))


;;;;   End of file  DUAL/UTILS.LSP
