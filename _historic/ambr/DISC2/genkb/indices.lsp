;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR_exp/CM1_FDO/indices.lsp
;;; VERSION:    1.0
;;; PURPOSE:    Recording the retrieval and mapping indices of all coalitions.
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; DEPENDS-ON: DUAL; AMBR; AMBR/KB/*.lsp
;;; CREATED:    07-07-98
;;; UPDATED:    ...


         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;;;;      RETRIEVAL  AND  MAPPING  INDICES     ;;;;;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "AMBR")

;;;; PURPOSE:  A tool for tracking retrieval and mapping indices of all
;;;;           coalitions. Produces a data file for import into MathCAD.
;;;;
;;;; IND_VARS: *time*
;;;;
;;;; DEP_VARS: a long list (see below)
;;;;
;;;; OUTFILE:  "D:\\dual\\ambr_exp\\cm1_fdo\\indices.dat"
;;;;
;;;; MAIN_FUN:  write-indices-to-file (final-time &optional file-name)


;;;;;;;;  EXPERIMENTAL DESIGN:
;;;;
;;;;  Extremely simple -- record the four indices at regular intervals.
;;;;
;;;;  The indices are: * retrieval index         --  DUAL/INTRFACE/COALITN.LSP
;;;;                   * Luce retrieval index
;;;;                   * linear mapping index    --  AMBR/INTRFACE/MAPPING.LSP
;;;;                   * quadratic mapping index
;;;;


;;;;;;;   ******   EXPERIMENTS   ******

(defvar *outfile-name*
         "D:\\dual\\ambr_exp\\cm1_fdo\\indices.dat" )

(defvar *data-stream* *standard-output* )


(defun write-indices-to-file (final-time
                              &optional (file-name *outfile-name*))
  (with-open-file (file file-name :direction :output)
    (format t "~%Now writing data file ~A  " file-name)
    (finish-output *standard-output*)
    (let ((*data-stream* file))
      (if (eq (do-experiment (coerce final-time 'float))
              :no-errors)
          (format t "~%   Experiment successful.")
          (format t "~%   Experiment failed."    ))
      (finish-output *standard-output*) ))
  (format t "~%Finished working on ~A  " file-name)
  (values) )


;;;;;;;   ******   INDIVIDUAL  EXPERIMENT  ******

(defparameter  *time-per-period*   2.5 )

(defun do-experiment (final-time)
  (declare (type float final-time))
  (ignore-errors
    (report-indices)                    ; a line with zeros at *time* 0.0
    (decf final-time 0.0001)            ; tolerance for floating-point #'>
    (do ((next-stop (+ *time* *time-per-period*)
                    (+ next-stop *time-per-period*)))
        ((> *time* final-time))         ; termination condition
      (ambr (- next-stop *time*))     ;;; <-- the heavy load
      (report-indices)
      (format t "." ) )
    :no-errors ))
  

(defun report-indices ()                                 ;vvv-- memoize
  (report-indices-aux #'(lambda (coa) (retrieval-index coa T)))
  (report-indices-aux #'(lambda (coa) (if (eq coa *target*)
                                          0.0
                                          (Luce-retrieval-index coa))))
  (report-indices-aux #'(lambda (coa) (mapping-index coa :mode :linear)))
  (report-indices-aux #'(lambda (coa) (mapping-index coa :mode :quadratic)))
) ; each time period writes four lines in the data file

(defun report-indices-aux (index-function)
  (format *data-stream* "~%~5,1,,'*,'0F ~5,2,,'*,'0@F~{ ~5,2,,'*,'0@F~}"
          *time*
          (funcall index-function *target*)
          (mapcar  index-function *total-RI-coalitions*) ))    ; all bases


;;;;;;;;;;  End of file  AMBR_EXP/CM1_FDO/INDICES.LSP
