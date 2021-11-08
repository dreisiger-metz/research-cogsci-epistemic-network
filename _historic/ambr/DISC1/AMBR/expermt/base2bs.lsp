;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/expermt/base2bs.lsp
;;; VERSION:    1.0
;;; PURPOSE:    Mapping bases to bases (to produce similarity matrix).
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; DEPENDS-ON: DUAL; AMBR; AMBR/KB/*.lsp
;;; CREATED:    05-07-98
;;; UPDATED:    ...


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;    B A S E -> B A S E   M A P P I N G S   ;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")

;;;; PURPOSE:   ...
;;;;
;;;; IND_VARS:  KB#,  target,  *time*
;;;;
;;;; DEP_VARS:  a long list (see below)
;;;;
;;;; OUTFILES:  "D:\\dual\\ambr_exp\\base2bs\\?????.dat"
;;;;
;;;; MAIN_FUN:  generate-data-file (target from-KB to-KB)


;;;;;;;;  EXPERIMENTAL DESIGN:
;;;;
;;;;  ...
;;;;


;;;;;;;   ******   EXPERIMENTS   ******

(defparameter *outfile-name-prefix*
              "D:\\dual\\ambr_exp\\base2bs\\" )

(defparameter *genKB-name-prefix*
              "D:\\dual\\ambr\\kb\\genkb\\genkb" )


(defvar *data-stream* *standard-output* )

(defvar *KB-number* 0 )


;; Deactivate all spy tools, see AMBR/INTRFACE/SET_SPY.LSP
(verbose 'receive-symbol nil)
(verbose 'skolemize nil)
(verbose 'affiliate-agent nil)


(defun generate-data-file (target from-KB to-KB)
  (assert (stringp target))     ; e.g. "WTP", "IHC"
  (assert (<= 0 from-KB 99))
  (assert (<= 0 to-KB   100))
  (let ((filename (format nil "~A~A~2,'0D.dat"
                              *outfile-name-prefix* target from-KB)))
    (with-open-file (file filename :direction :output)
      (format t "~%Now writing data file ~A  " filename)
      (finish-output *standard-output*)
      (let ((*data-stream* file))
        (dotimes (k (- to-KB from-KB))
          (let ((KB-number (+ k from-KB)))
            (if (eq (do-experiment KB-number target) :no-errors)     ; <----
                (format t "~%   KB ~2,'0D successful." KB-number)
                (format t "~%   KB ~2,'0D *** failed ***." KB-number) ))
          (finish-output *standard-output*) )))
    (format t "~%Fiished working on ~A  " filename)
    (values) ))



;;;;;;;   ******   INDIVIDUAL  EXPERIMENT  ******

(defun do-experiment (KB-number target)
  (declare (type integer KB-number)
           (type symbol  target) )      ; coalition name
  (ignore-errors
    (initialize-experiment KB-number target)
    (ambr 100.0)                        ;  <----  run model for 100 time units
    (report-experiment)
    :no-errors ))
  

(defun initialize-experiment (KB-number target)
  (clear-everything-in-DUAL)
  (let (*standard-output* (make-broadcast-stream))    ; > nul
    (mk:oos "semantic" :load :verbose nil)
    (mk:oos "base-sit" :load :verbose nil) )
  (load (format nil "~A~3,'0D.lsp" 
                *genKB-name-prefix* KB-number) 
        :verbose nil)  
  (remove-all-genKB-templates)
  (ambri::filter-hypotheses T)      ; set mapping memo to NIL
  (make-node-constructors *number-of-node-constructors* :mode :new)
  (enforce-AMBR-parameters)

  (setq *KB-number* KB-number)
  (mount-target target)
)


(defun mount-target (target)
  (cond ((string-equal target "WTP") (mount-WTP))
        ((string-equal target "BF" ) (mount-BF ))
        ((string-equal target "GP" ) (mount-GP ))
        ((string-equal target "IHC") (mount-IHC))
        ((string-equal target "MTF") (mount-MTF))
        ((string-equal target "ICF") (mount-ICF))
        ((string-equal target "BPF") (mount-BPF))
        ((string-equal target "FDO") (mount-FDO))
        ((string-equal target "STC") (mount-STC))
        ((string-equal target "SFF") (mount-SFF))
        ((string-equal target "ERW") (mount-ERW))
        ((string-equal target "GWB") (mount-GWB))
        (t (error "MOUNT-TARGET: Unknown target ~S" target)) )
  ;; Now *TARGET* should be set appropriately.
  (setq *total-RI-coalitions*                 ; see DUAL/INTRFACE/COALITN.LSP
        (remove *target* *all-coalitions*))
  *target* )  ; mount-target


(defun mount-WTP ()
  (add-T-driver-tag #$sit-WTP)
  (add-to-goal/input #$T-of-WTP-w   1.0 :goal )
  (add-to-goal/input #$high-T-WTP   1.0 :goal )
  (add-to-goal/input #$water-WTP    1.0 :input )
  (add-to-goal/input #$tpot-WTP     1.0 :input )
  (add-to-goal/input #$hplate-WTP   1.0 :input )
  (add-to-goal/input #$T-of-WTP-p   0.5 :input )
  (add-to-goal/input #$in-WTP       0.5 :input )
  (add-to-goal/input #$on-WTP       0.5 :input )
  (add-to-goal/input #$made-of-WTP  0.5 :input )
  (add-to-goal/input #$mmetal-WTP   0.5 :input )
  (add-to-goal/input #$color-of-WTP 0.5 :input )
  (add-to-goal/input #$black-WTP    0.5 :input )

  (setq *target* (find-coalition 'sit-WTP)) )

(defun mount-BF ()
  (add-T-driver-tag #$sit-BF)
  (error "MOUNT-BF is not defined yet." )

  (setq *target* (find-coalition 'sit-BF)) )

(defun mount-GP ()
  (add-T-driver-tag #$sit-GP)
  (error "MOUNT-GP is not defined yet." )

  (setq *target* (find-coalition 'sit-GP)) )

(defun mount-IHC ()
  (add-T-driver-tag #$sit-IHC)
  (error "MOUNT-IHC is not defined yet." )

  (setq *target* (find-coalition 'sit-IHC)) )

(defun mount-MTF ()
  (add-T-driver-tag #$sit-MTF)
  (error "MOUNT-MTF is not defined yet." )

  (setq *target* (find-coalition 'sit-MTF)) )

(defun mount-ICF ()
  (add-T-driver-tag #$sit-ICF)
  (error "MOUNT-ICF is not defined yet." )

  (setq *target* (find-coalition 'sit-ICF)) )

(defun mount-BPF ()
  (add-T-driver-tag #$sit-BPF)
  (error "MOUNT-BPF is not defined yet." )

  (setq *target* (find-coalition 'sit-BPF)) )

(defun mount-FDO ()
  (add-T-driver-tag #$sit-FDO)
  (error "MOUNT-FDO is not defined yet." )

  (setq *target* (find-coalition 'sit-FDO)) )

(defun mount-STC ()
  (add-T-driver-tag #$sit-STC)
  (error "MOUNT-STC is not defined yet." )

  (setq *target* (find-coalition 'sit-STC)) )

(defun mount-SFF ()
  (add-T-driver-tag #$sit-SFF)
  (error "MOUNT-SFF is not defined yet." )

  (setq *target* (find-coalition 'sit-SFF)) )

(defun mount-ERW ()
  (add-T-driver-tag #$sit-ERW)
  (error "MOUNT-ERW is not defined yet." )

  (setq *target* (find-coalition 'sit-ERW)) )

(defun mount-GWB ()
  (add-T-driver-tag #$sit-GWB)
  (error "MOUNT-GWB is not defined yet." )

  (setq *target* (find-coalition 'sit-GWB)) )



;;;;;;  ******  REPORTING  RESULTS  *******
;;
;; ...
;;

;; ASSUMPTION: Each coalition-name coincides with the agent-name of the head.

(defun report-experiment ()
  (format *data-stream* "~%~2,'0D ~2,'0D"
          (situation-code *target*)
          *KB-number* )
  (multiple-value-bind (winner-agent hypoth)          ; see AMBR/SECRETAR.LSP
                       (driver-elt-mapping (coalition-head *target*))
   ;; Main dependent variables: WINNER-SIT, WINNER-FLAG, and WM-SIZE
       (if (null winner-agent)      ; no winner
           (format *data-stream* " 00 0" )
           (format *data-stream* " ~2,'0D ~:[0~;1~]"
                   (situation-code winner-agent)
                   (agent-type hypoth :winner)) )
     (format *data-stream* " ~3,'0D" (WM-size *WM*))
   ;; Target indices: RI, MI1, MI2
     (format *data-stream* " ~4,2,,'*,'0F ~5,2,,'*,'0@F ~5,2,,'*,'0@F"
             (retrieval-index *target* T)    ; memoize-p is T
             (mapping-index   *target* :mode :linear    :memoize-p NIL) ; <-- !
             (mapping-index   *target* :mode :quadratic :memoize-p T) )
   ;; Winner indices (if any): CA, RI, LUCE-RI, CM, CW, MI1, MI2
       (if (null winner-agent)       ; no winner
           (format *data-stream* " 00 0.00 0.00 00 -0.99 -0.99" )
           (let ((winner-sit (find-coalition (agent-name winner-agent))))
             (format *data-stream* " ~2,'0D ~4,2,,'*,'0F ~4,2,,'*,'0F"
                     (count-active-members winner-sit)
                     (retrieval-index winner-sit T)
                     (Luce-retrieval-index winner-sit) )   ; memoize by default
             (format *data-stream* " ~2,'0D ~2,'0D ~5,2,,'*,'0@F ~5,2,,'*,'0@F"
                     (count-mapped-members winner-sit :mode :total)
                     (count-mapped-members winner-sit :mode :winner)
                     (mapping-index winner-sit :mode :linear)
                     (mapping-index winner-sit :mode :quadratic))
  )))
  (force-output *data-stream*)
)  ; report-experiment


;;;;;;;;;;  End of file  AMBR/EXPERMT/BASE2BS.LSP
