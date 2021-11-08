;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR_exp/CM1_FDO/act_elt.lsp
;;; VERSION:    1.0
;;; PURPOSE:    Tracking the activation levels of members of a coalition.
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; DEPENDS-ON: DUAL; AMBR; AMBR/KB/*.lsp
;;; CREATED:    06-07-98
;;; UPDATED:    ...


         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;;;      ACTIVATION  HISTORIES      ;;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "AMBR")

;;;; PURPOSE:  A tool for tracking activation levels of the members of a given
;;;;           coalition. Produces a data file for import into MathCAD.
;;;;
;;;; IND_VARS: *time*
;;;;
;;;; DEP_VARS: a long list (see below)
;;;;
;;;; OUTFILE:  "D:\\dual\\ambr_exp\\cm1_fdo\\act_elt.dat"
;;;;
;;;; MAIN_FUN:  generate-data-file (coalition &optional file-name)


;;;;;;;;  EXPERIMENTAL DESIGN:
;;;;
;;;;  Extremely simple -- record the activation level of each coalition member
;;;;                      at regular intervals
;;;;


;;;;;;;   ******   EXPERIMENTS   ******

(defvar *outfile-name*
         "D:\\dual\\ambr_exp\\cm1_fdo\\act_elt.dat" )

(defvar *data-stream* *standard-output* )


(defun generate-data-file (coalition
                           &optional (file-name *outfile-name*))
  (with-open-file (file file-name :direction :output)
    (format t "~%Now writing data file ~A  " file-name)
    (finish-output *standard-output*)
    (let ((*data-stream* file))
      (if (eq (do-experiment coalition) :no-errors)
          (format t "~%   Experiment successful.")
          (format t "~%   Experiment failed."    ))
      (finish-output *standard-output*) ))
  (format t "~%Finished working on ~A  " file-name)
  (values) )


;;;;;;;   ******   INDIVIDUAL  EXPERIMENT  ******

(defparameter  *number-of-periods* 120 )    ; 120 * 2.5 = 300
(defparameter  *time-per-period*   2.5 )

(defun do-experiment (coalition)
  (ignore-errors
    (report-period coalition)           ; a line with zeros at *time* 0.0
    (dotimes (k *number-of-periods*)
      (ambr *time-per-period*)          ;;; <-- the heavy load
      (report-period coalition)
      (format t "." ) )
    :no-errors ))
  
(defun report-period (coalition)
  (format *data-stream* "~%~5,1,,'*,'0F"
          *time* )
  (dolist (ag (coalition-members coalition))
    (format *data-stream* " ~5,3,,'*,'0F"
            (agent-activation ag)) ))




;;;;;;;;;;  End of file  AMBR_EXP/CM1_FDO/ACT_ELT.LSP
