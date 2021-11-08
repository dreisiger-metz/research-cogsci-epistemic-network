;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/expermt/mainexp.lsp
;;; VERSION:    1.0
;;; PURPOSE:    Exploring the AMBR3 model -- main experiment of my Ph.D.Thesis
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; DEPENDS-ON: DUAL; AMBR; AMBR/KB/*.lsp
;;; CREATED:    06-07-98
;;; UPDATED:    ...


        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;    M A I N   E X P E R I M E N T     ;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;; This file implements the main series of experiments of Alexander Petrov's
;;;; Ph.D. Thesis -- the 1000 runs of the model over 10 target problems and
;;;; 100 variants of the knowledge base. See section 6.2 of the Thesis.


;;;; PURPOSE:   To explore the solutions of 10 target problems over 100 KBs.
;;;;
;;;; IND_VARS:  KB#,  target,  *time*
;;;;
;;;; DEP_VARS:  a long list (see below)
;;;;
;;;; OUTFILES:  "D:\\dual\\ambr_exp\\mainexp\\kb???.dat"
;;;;
;;;; MAIN_FUN:  generate-data-file (KB-number)


;;;;;;;;  EXPERIMENTAL DESIGN:
;;;;
;;;;  One data file per KB:
;;;;    10 targets per KB  (i.e. 10 runs of the model)
;;;;    10 periods of 20.0 time units each  (i.e. 200.0 time units per run)
;;;;     1 line in the data file per period (i.e. 100 lines in each data file)
;;;;


;;;;;;;   ******   EXPERIMENTS   ******

(defparameter *outfile-name-prefix*
              "D:\\dual\\ambr_exp\\mainexp\\kb" )

;; *GENKB-PREFIX* is defined in AMBR/KB/SIT_CODE.LSP

(defparameter *target-name-prefix*
              "D:\\dual\\ambr\\kb\\episodic\\t_" )


(defparameter *target-situation-names*      ; 10 levels
    '("WB1" 
      "WG1"
      "HM1" "HM2"
      "CM1" "CM2"
      "ICC"
      "SF1" "SF2"
      "EHW"        ))


(defvar *data-stream* *standard-output* )

(defvar *KB-number* 0 )


;; Deactivate all spy tools, see AMBR/INTRFACE/SET_SPY.LSP
(verbose 'receive-symbol nil)
(verbose 'skolemize nil)
(verbose 'affiliate-agent nil)


(defun generate-data-file (KB-number)
  (let ((filename (format nil "~A~3,'0D.dat"
                              *outfile-name-prefix* KB-number)))
    (with-open-file (file filename :direction :output)
      (format t "~%Now writing data file ~A~%" filename)
      (finish-output *standard-output*)
      (let ((*data-stream* file))
        (dolist (target-name *target-situation-names*)
          (if (eq (do-experiment KB-number target-name) :no-errors)  ; <----
              (format t " success~%")
              (format t " *** failed ***~%") ))
          (finish-output *standard-output*)) )
    (format t "Finished working on ~A~%" filename)
    (values) ))



;;;;;;;   ******   INDIVIDUAL  EXPERIMENT  ******

(defparameter  *number-of-periods* 10 )
(defparameter  *time-per-period*   20.0 )

(defun do-experiment (KB-number target-name)
  (declare (type integer KB-number)
           (type string  target-name) )      ; see *TARGET-NAMES* above
  (ignore-errors
    (initialize-experiment KB-number target-name)
    (format t "   ~8A " (coalition-name *target*))
    (dotimes (k *number-of-periods*)
      (ambr *time-per-period*)          ;;; <-- the heavy load
      (report-period)
      (format t "." ) )
    :no-errors ))
  

(defun initialize-experiment (KB-number target-name)
  (clear-everything-in-DUAL)
  (fmakunbound 'attach-target)       ;  the new target file will define it anew
  #+:ACLPC (remprop 'attach-target 'top::definitions)
  (let (*standard-output* (make-broadcast-stream))    ; > nul
    (mk:oos "semantic" :load :verbose nil)            ; load all semantic files
    (mk:oos "base-sit" :load :verbose nil) )          ; load all base episodes
  (setq *KB-number* KB-number)
  (load (format nil "~A~3,'0D.lsp"
                *genKB-prefix* KB-number)             ; load genKB file
        :verbose nil)
  (load (format nil "~A~A.lsp"
                *target-name-prefix* target-name)     ; load target file
        :verbose nil)
  (attach-target nil)                ; defined in each target file
  (remove-all-genKB-templates)
  (ambri::filter-hypotheses T)       ; set mapping-index memo to NIL
  (make-node-constructors *number-of-node-constructors* :mode :new)
  (enforce-AMBR-parameters)
  *target* )


;;;;;;  ******  REPORTING  RESULTS  *******
;;
;; ...
;;

;; See AMBR/KB/SIT_CODE.LSP for situation codes.
;; ASSUMPTION: Each coalition-name coincides with the agent-name of the head.

(defun report-period ()
  (report-independent-vars)    ; KB#, target-code, and *time*
  (report-global-vars)         ; WM-size, *agenda*, etc.
  (report-target-vars)         ; ca, ri, cm, cw, mi1, mi2
  (multiple-value-bind (win-agent win-hyp)          ; see AMBR/SECRETAR.LSP
                       (driver-elt-mapping (coalition-head *target*))
    (let* ((winner (if (null win-agent)
                       nil
                       (find-coalition (agent-name win-agent))))
           (win-p  (cond ((null win-hyp)                0)    ; no hyp at all
                         ((agent-type win-hyp :winner)  1)    ; :winner
                         (t                             0)))  ; :mature
           (second (find-second-best winner)) )
      (report-winner-vars winner win-p)
      (report-second-vars second) ))
  (force-output *data-stream*)
)


(defun find-second-best (winner)
  (declare (type (or null coalition) winner)
           (values (or null coalition)) )
  (let ((second  nil)
        (max-MI1 -999.99) )
    (unless (null winner)
      (dolist (coa *total-RI-coalitions*)      ; see DUAL/INTRFACE/COALITN.LSP
        (when (and (not (eq coa winner))
                   (> (mapping-index coa :mode :linear) max-MI1))
          (setq second coa)
          (setq max-MI1 (mapping-index coa :mode :linear)) )))
    second ))


(defun report-independent-vars ()
  (format *data-stream* "~%~3,'0D ~2,'0D ~5,1,,'*,'0F"
          (situation-code *target*)
          *KB-number*
          *time* ))

(defun report-global-vars ()
  (let ((total 0) (concepts 0) (instances 0)
        (hyps  0) (matures  0) (winners   0) )
    (do-all-WM (agent *WM*)
      (unless (dead-agent-p agent)
        (incf total)
        (let ((type (get-filler agent :TYPE)))
          (when (member :concept  type) (incf concepts ))
          (when (member :instance type) (incf instances))
          (when (member :hypoth   type) (incf hyps     ))
          (when (member :mature   type) (incf matures  ))
          (when (member :winner   type) (incf winners  )) )))
    (incf total     (WM-size *input*))
    (incf instances (WM-size *input*))
    (incf total     (WM-size *goal* ))
    (incf instances (WM-size *goal* ))
    (format *data-stream*
            " ~3,'0D ~6,2,,'*,'0F ~3,'0D ~3,'0D ~3,'0D ~3,'0D ~2,'0D ~3,'0D"
            total
            (+ (WM-total-act *WM*) (WM-total-act *goal*) (WM-total-act *input*))
            concepts  instances  hyps  matures  winners
            (number-of-processes)) ))


(defun report-target-vars ()
  (format *data-stream*
          " ~2,'0D ~4,2,,'*,'0F ~2,'0D ~5,2,,'*,'0@F ~5,2,,'*,'0@F"
          (count-active-members *target*)
          (retrieval-index *target* T)    ; memoize-p is T
          (count-mapped-members *target*)
          (mapping-index *target* :mode :linear   )
          (mapping-index *target* :mode :quadratic) ))


(defun report-winner-vars (winner win-p)
  (declare (type (or null coalition) winner)
           (type (member 0 1) win-p) )
  (cond ((null winner)
           (format *data-stream* " 00 0 00 0.00 0.00 00 00 -0.99 -0.99" ))
        (t (format *data-stream* " ~2,'0D ~D ~2,'0D ~4,2,,'*,'0F ~4,2,,'*,'0F"
                   (situation-code winner)
                   win-p
                   (count-active-members winner)
                   (retrieval-index winner T)
                   (Luce-retrieval-index winner) )
           (format *data-stream* " ~2,'0D ~2,'0D ~5,2,,'*,'0@F ~5,2,,'*,'0@F"
                   (count-mapped-members winner :mode :total)
                   (count-mapped-members winner :mode :winner)
                    (mapping-index winner :mode :linear)
                    (mapping-index winner :mode :quadratic))) ))


(defun report-second-vars (second)
  (declare (type (or null coalition) second))
  (cond ((null second)
           (format *data-stream* " 00 00 0.00 0.00 00 -0.99 -0.99" ))
        (t (format *data-stream* " ~2,'0D ~2,'0D ~4,2,,'*,'0F ~4,2,,'*,'0F"
                   (situation-code second)
                   (count-active-members second)
                   (retrieval-index second T)
                   (Luce-retrieval-index second) )
           (format *data-stream* " ~2,'0D ~5,2,,'*,'0@F ~5,2,,'*,'0@F"
                   (count-mapped-members second :mode :total)
                   (mapping-index second :mode :linear)
                   (mapping-index second :mode :quadratic))) ))


;;;;;;;;;;  End of file  AMBR/EXPERMT/MAINEXP.LSP
