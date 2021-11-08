;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/expermt/order.lsp
;;; VERSION:    1.0
;;; PURPOSE:    Demonstrating order effects in AMBR3
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; DEPENDS-ON: DUAL; AMBR; AMBR/KB/*.lsp
;;; CREATED:    12-07-98
;;; UPDATED:    ...


        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;      O R D E R   E F F E C T      ;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")


;;;; PURPOSE:   To demonstrate order effects in AMBR3
;;;;
;;;; IND_VARS:  KB#,  condition,  *time*
;;;;
;;;; DEP_VARS:  a long list (see below)
;;;;
;;;; OUTFILES:  "D:\\dual\\ambr_exp\\order\\order??.dat"
;;;;
;;;; MAIN_FUN:  generate-data-file (KB-set)


;;;;;;;;  EXPERIMENTAL DESIGN:
;;;;
;;;;  One data file per <set-size> KBs:
;;;;    2 conditions per KB  (i.e. 2 runs of the model)
;;;;    10 periods of 20.0 time units each  (i.e. 200.0 time units per run)
;;;;     1 line in the data file per period (i.e. 100 lines in each data file)
;;;;


;;;;;;;   ******   EXPERIMENTS   ******

(defparameter *outfile-name-prefix*
              "D:\\dual\\ambr_exp\\order\\order" )

;; *GENKB-PREFIX* is defined in AMBR/KB/SIT_CODE.LSP

(defparameter *target-file*
              "D:\\dual\\ambr\\kb\\episodic\\t_ehw.lsp" )

(defvar *KB-set-size*  5
  "Number of KBs in a data file." )


(defvar *data-stream* *standard-output* )

(defvar *KB-number* 0 )


;; Deactivate all spy tools, see AMBR/INTRFACE/SET_SPY.LSP
(verbose 'receive-symbol nil)
(verbose 'skolemize nil)
(verbose 'affiliate-agent nil)


(defun generate-data-file (KB-set)
  (let ((filename (format nil "~A~2,'0D.dat"
                              *outfile-name-prefix* KB-set)))
    (with-open-file (file filename :direction :output)
      (format t "~%Now writing data file ~A~%" filename)
      (finish-output *standard-output*)
      (let ((*data-stream* file))
        (dotimes (lower-digit *KB-set-size*)
          (if (eq (do-experiment (+ lower-digit (* KB-set *KB-set-size*)))
                  :no-errors)
              (format t " success~%")
              (format t " *** failed ***~%") ))
          (finish-output *standard-output*)) )
    (format t "Finished working on ~A~%" filename)
    (values) ))


;;;;;;;   ******   EXPERIMENT  CONDITION  ******

(defvar *condition*  0 )       ; 1 for 'egg condition, 2 for 'water condition'


(defun egg-schedule ()
  (setq *condition* 1)
   (add-to-goal/input #$egg-EHW      1.00 :input )        ;   T =  0
   (add-to-goal/input #$color-of-EHW 0.25 :input )        ;   T =  0
   (add-to-goal/input #$white-EHW    0.25 :input )        ;   T =  0
  (AMBR 5.0)                                              ;  0 ->  5
   (add-to-goal/input #$tpot-EHW     1.00 :input )        ;   T =  5
  (AMBR 5.0)                                              ;  5 -> 10
   (add-to-goal/input #$made-of-EHW  0.25 :input )        ;   T = 10
   (add-to-goal/input #$mmetal-EHW   0.25 :input )        ;   T = 10
  (AMBR 5.0)                                              ; 10 -> 15
   (add-to-goal/input #$water-EHW    1.00 :input )        ;   T = 15
  (AMBR 5.0)                                              ; 15 -> 20
  (report-period)
   (add-to-goal/input #$in-EHW-ew    0.50 :input )        ;   T = 20
  (AMBR 5.0)                                              ; 20 -> 25
   (add-to-goal/input #$in-EHW-wt    0.50 :input )        ;   T = 25
  (AMBR 5.0)                                              ; 25 -> 30
   (add-to-goal/input #$T-of-EHW     1.00 :input )        ;   T = 30
   (add-to-goal/input #$high-T-EHW   1.00 :input )        ;   T = 30
  (AMBR 5.0)                                              ; 30 -> 35
   (add-to-goal/input #$endst-EHW    1.0  :goal )         ;   T = 35
   (add-to-goal/input #$follows-EHW  1.0  :goal )         ;   T = 35
  (AMBR (- 40.0 *time*))                                  ;   T:= 40
  (report-period)
) ; egg-schedule


(defun water-schedule ()
  (setq *condition* 2)
   (add-to-goal/input #$water-EHW    1.00 :input )        ;   T =  0
   (add-to-goal/input #$T-of-EHW     1.00 :input )        ;   T =  0
   (add-to-goal/input #$high-T-EHW   1.00 :input )        ;   T =  0
  (AMBR 5.0)                                              ;  0 ->  5
   (add-to-goal/input #$tpot-EHW     1.00 :input )        ;   T =  5
  (AMBR 5.0)                                              ;  5 -> 10
   (add-to-goal/input #$made-of-EHW  0.25 :input )        ;   T = 10
   (add-to-goal/input #$mmetal-EHW   0.25 :input )        ;   T = 10
  (AMBR 5.0)                                              ; 10 -> 15
   (add-to-goal/input #$in-EHW-wt    0.50 :input )        ;   T = 15
  (AMBR 5.0)                                              ; 15 -> 20
  (report-period)
   (add-to-goal/input #$egg-EHW      1.00 :input )        ;   T = 20
  (AMBR 5.0)                                              ; 20 -> 25
   (add-to-goal/input #$in-EHW-ew    0.50 :input )        ;   T = 25
  (AMBR 5.0)                                              ; 25 -> 30
   (add-to-goal/input #$color-of-EHW 0.25 :input )        ;   T = 30
   (add-to-goal/input #$white-EHW    0.25 :input )        ;   T = 30
  (AMBR 5.0)                                              ; 30 -> 35
   (add-to-goal/input #$endst-EHW    1.00 :goal )         ;   T = 35
   (add-to-goal/input #$follows-EHW  1.00 :goal )         ;   T = 35
  (AMBR (- 40.0 *time*))                                  ;   T:= 40
  (report-period)
) ; water-schedule



;;;;;;;   ******   INDIVIDUAL  EXPERIMENT  ******

(defparameter  *number-of-periods* 8 )       ; = 10 - 2
(defparameter  *time-per-period*   20.0 )

(defun do-experiment (KB-number)
  (declare (type integer KB-number) )
  (ignore-errors
    (initialize-experiment KB-number)
    (format t "   KB ~3D, egg    " KB-number )
    (egg-schedule)
    (format t "*" )
    (dotimes (k *number-of-periods*)
      (ambr *time-per-period*)
      (report-period)
      (format t "." ) )
    ;; -----------------------
    (initialize-experiment KB-number)
    (format t "~%   KB ~3D, water  " KB-number )
    (water-schedule)
    (format t "*" )
    (dotimes (k *number-of-periods*)
      (ambr *time-per-period*)
      (report-period)
      (format t "." ) )
    (setq *condition* 0)
    :no-errors ))
  

(defun initialize-experiment (KB-number)
  (clear-everything-in-DUAL)
  (let (*standard-output* (make-broadcast-stream))    ; > nul
    (mk:oos "semantic" :load :verbose nil)            ; load all semantic files
    (mk:oos "base-sit" :load :verbose nil) )          ; load all base episodes
  (setq *KB-number* KB-number)
  (load (format nil "~A~3,'0D.lsp"
                *genKB-prefix* KB-number)             ; load genKB file
        :verbose nil)
  (load *target-file*                                 ; load target file
        :verbose nil)
  (add-T-driver-tag #$sit-EHW)                        ; proclaim as :DRIVER
  (setq *target* (find-coalition 'sit-EHW))
  (setq *total-RI-coalitions*
        (remove *target* *all-coalitions*))
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
          *condition*
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
