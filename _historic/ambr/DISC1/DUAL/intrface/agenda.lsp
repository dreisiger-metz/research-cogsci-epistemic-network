;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/agenda.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Interface to DUAL/ARCHIT/AGENDA.LSP
;;; DEPENDS-ON: packages.lsp, archit/agenda.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    02-06-97 [1.0]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...


(cl:in-package "DUAL-INTERFACE")

;; SYMBOLS:  ra,
;;           proc, st, st!, *step-what*
;;


;;;; RA is an interface to RUN-ALL-PROCESSES (see DUAL/ARCHIT/AGENDA.LSP)
;; See also SPR in DUAL/INTRFACE/SPREAD.LSP.
(defun ra (&optional (cycles 1))
  "RUN-ALL-PROCESSES several times and finally show NUMBER-OF-PROCESSES."
  (dotimes (k cycles)
    (run-all-processes))
  (number-of-processes) )


;;;; PROC describes the suspended process running under AGENT or gives
;;;; summary information about the number of all processes on *AGENDA*.
(defun proc (&optional (host t) (print-pretty *print-pretty*))
  "Describe the suspended process running under HOST."
  (cond ((eq host t) (number-of-processes))
        ((agentp host)
           (let ((proc (find-process host))
                 (*print-pretty* print-pretty) )
             (if (null proc)
                 :idle
                 (DUAL-describe proc)))) ))


;;;; ST and ST! are step utilities for suspendable processes. Note that
;;;; they bypass the energetic balance.
;;;; ST is like right arrow, ST! -- like down arrow.

(defvar *step-what*  nil
  "The default for the second argument to ST. Should be a suspended proc.")

#+:DUAL-DEBUG
(defun st (&optional (cycles-or-agent 1))
  "A STEP facility for suspended processes."
  (etypecase cycles-or-agent
    (integer
       (cond ((null *step-what*) :idle)
             (t   (dotimes (k cycles-or-agent)
                     (unless (endp (process-stack *step-what*))
                        (pop (dualc::process-l-stack *step-what*))
                        (funcall (pop (process-stack *step-what*))) ))
                  (DUAL-describe  *step-what*)) ))
    (symbolic-processor
       (setq *step-what* (find-process cycles-or-agent))
       (cond ((null *step-what*) :idle)
             (t  (format t "~%;; Ready to step ~S." *step-what*)
                 (DUAL-describe *step-what*))))
    (suspended-process
       (setq *step-what* cycles-or-agent)
       (format t "~%;; Ready to step ~S." *step-what*)
       (DUAL-describe *step-what*)) ))

#-:DUAL-DEBUG
(defun st (&optional (cycles-or-agent 1))
  "NOT AVAILABLE. (A STEP facility for suspended processes.)"
  (declare (ignore cycles-or-agent))
  (format t "~&;; The step facility for suspended processes is not available ~
             because :DUAL-DEBUG was set off during compilation.~%" ))


#+:DUAL-DEBUG
(defun st! (&optional (levels 1) (proc *step-what*))
  (if (null proc)
      :idle
      (let* ((initial-stack-depth (length (process-stack proc)))
             (desired-stack-depth (max 0 (- initial-stack-depth levels))) )
        (do ()                                      ; while
            ((>= desired-stack-depth
                 (length (process-stack proc))) )
          (pop (dualc::process-l-stack proc))
          (funcall (pop (process-stack proc))) )
        (DUAL-describe  proc)) ))

#-:DUAL-DEBUG
(defun st! (&optional (levels 1) (proc *step-what*))
  "NOT AVAILABLE. (A STEP facility for suspended processes.)"
  (declare (ignore levels proc))
  (format t "~&;; The step facility for suspended processes is not available ~
             because :DUAL-DEBUG was off during compilation.~%" ))


;;;; End of file DUAL/INTRFACE/AGENDA.LSP
