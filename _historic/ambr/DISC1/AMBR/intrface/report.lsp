;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-interface -*-

;;; FILE:       AMBR/intrface/report.lsp
;;; VERSION:    2.2.2    ; see AMBR/version.lsp
;;; PURPOSE:    Functions reporting the state of the model to a stream.
;;; DEPENDS-ON: DUAL; AMBR; ambr/intrface/mapping.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    18-05-98 [2.2.2]
;;; UPDATED:    ...

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;     R E P O R T   F A C I L I T I E S    ;;;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "AMBR-INTERFACE")


;; This files defines the main tools for interactive experimentation.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: g, report, report-header ;
;;          mt, rt, skt ;
;;          hyps, show-hyps, do-all-hyps ;
;;          show-coa
;;

;; ...

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;   ****  Main function  ****
;;
;;  G is the main function for interactive experimentation with the model.
;;  'G' is a shorthand for 'go'. (GO itself is a reserved word in LISP.)

(defun g (&optional (delta-T 10.0))
  "Run model for DELTA-T units of time and then print out reports."
  (AMBR delta-T)             ;; <-- the main work, see AMBR/TOPLEVEL.LSP
  (report-header)
  (report) )


;;;;;   ****  REPORT  ****

(defun report-header (&optional (stream *standard-output*))
  "Prints the number of agents in WM, the number of active processes, etc."
  (let ((total  0) (concepts 0) (instances 0) (specials 0)
        (hypoth 0) (embryos  0) (matures   0) (winners  0) )
    (flet ((count (agent)
             (unless (dead-agent-p agent)
               (incf total)
               (let ((type (get-filler agent :TYPE)))
                 (when (member :concept  type) (incf concepts ))
                 (when (member :instance type) (incf instances))
                 (when (member :hypoth   type) (incf hypoth   ))
                 (when (member :embryo   type) (incf embryos  ))
                 (when (member :mature   type) (incf matures  ))
                 (when (member :winner   type) (incf winners  ))
                 (when (member :special  type) (incf specials )) ))))
      ;;;; Write the first line of the header, e.g.
      ;; Time 123.45, 348 agents in WM: 112 c, 121 i, 115 h (100+12+3)
      (do-all-WM (agent *WM*)    (count agent))
      (do-all-WM (agent *goal*)  (count agent))
      (do-all-WM (agent *input*) (count agent))
      (format stream
         "~%;; Time ~6,2F, ~3D agents in WM: ~3D c, ~3D i, ~3D h (~D+~D+~D)"
         *time* total  concepts instances  hypoth embryos matures winners )
      ;;;; Write the second line of the header, e.g.
      ;;   13 clocks; 173 ag on agenda: 38 c, 59 i, 66 h (58+7+1), 10 nc
      (setq  total  0  concepts 0  instances 0  specials 0
             hypoth 0  embryos  0  matures   0  winners  0 )
      (do-all-processes (proc) (count (process-host proc)))
      (format stream
         "~%;;  ~3D clocks; ~3D ag on agenda: ~3D c, ~3D i, ~3D h (~D+~D+~D), ~D nc~%"
         (number-of-active-alarm-clocks)
         total  concepts instances  hypoth embryos matures winners  specials )
      (values))))


(defun report (&optional (stream *standard-output*))
  "Print summaries, retrieval- and mapping-indices, etc."
  (flet ((do-one-line (sit sit-name)
           (format stream " | ~7A | ~5,3F | ~2D | ~5,3F | ~2D |   "
                   sit-name  (retrieval-index sit T)
                             (count-active-members sit)
                             (mi sit)
                             (cm sit)) ))
  (format stream "~% |  sit    |   ri  | ca |   mi  | cm |   ")
  (format stream   " |  sit    |   ri  | ca |   mi  | cm |   ")
  (format stream "~% +---------+-------+----+-------+----+   ")
  (format stream   " +---------+-------+----+-------+----+~%")
  (do-one-line (coa 'ambr::sit-WTP) "sit-WTP")
  (do-one-line (coa 'ambr::sit-BF ) "sit-BF ")
  (terpri stream)
  (do-one-line (coa 'ambr::sit-IHC) "sit-IHC")
  (do-one-line (coa 'ambr::sit-GP ) "sit-GP ")
  (terpri stream)
  (do-one-line (coa 'ambr::sit-MTF) "sit-MTF")
  (do-one-line (coa 'ambr::sit-BPF) "sit-BPF")
  (terpri stream)
  (do-one-line (coa 'ambr::sit-ICF) "sit-ICF")
  (do-one-line (coa 'ambr::sit-FDO) "sit-FDO")
  (terpri stream)
  (do-one-line (coa 'ambr::sit-STC) "sit-STC")
  (do-one-line (coa 'ambr::sit-ERW) "sit-ERW")
  (terpri stream)
  (do-one-line (coa 'ambr::sit-SFF) "sit-SFF")
  (do-one-line (coa 'ambr::sit-GWB) "sit-GWB")
  (format stream "~% +---------+-------+----+-------+----+   ")
  (format stream   " +---------+-------+----+-------+----+~%")
  (do-one-line *target* (agent-name (coalition-head *target*)))
  (format stream   " | all WM  |~6,2F |~3D |       |~3D |~%~%"
          (wm-total-act *wm*) (wm-size *wm*) (number-of-processes) )
  (values) ))



;;;;;   ****  SHOWING HYPOTHESES  ****

(defun hypoth-type-code (hypoth)
  (declare (values string))
  (cond ((agent-type hypoth :embryo)    " e")
        ((agent-type hypoth :mature)    "m ")
        ((agent-type hypoth :winner)    "WW")
        ((agent-type hypoth :temporary) " T")
        ((agent-type hypoth :corr)      "P ")  ; permanent
        (t                              "??")) )

(defun show-one-hyp (hypothesis)
  (format t "~% ~,3F  ~A~3D ~:[ ~;*~]  ~(~A~)"
            (agent-activation hypothesis)
            (hypoth-type-code hypothesis)
            (length (corresp-justif hypothesis))
            (active-processor-p hypothesis)
            (agent-name hypothesis) ))


(defun hyps (secretary)
  "Show the hypotheses registered at SECRETARY."
  (if (not (typep secretary 'secretary-agent))
      (format t "~&~S is not a secretary agent." secretary)
      (let ((total 0) (embryos 0) (matures 0) (winners 0))
        (dolist (conref (agent-hypotheses secretary nil))
          (let* ((hyp (symref-agent (conref-reference conref)))
                 (type-filler (get-filler hyp :type)) )
            (show-one-hyp hyp)
            (incf total)
            (when (member :embryo type-filler) (incf embryos))
            (when (member :mature type-filler) (incf matures))
            (when (member :winner type-filler) (incf winners)) ))
        (format t "~% ~D hyps (~D e + ~D m + ~D W)~%" 
                total embryos matures winners) ))
  (values))

(defun show-hyps (&optional (cutoff *hypoth-zero-act*) &aux (count 0))
  (sort-wm *wm*)
  (do-all-wm (ag *wm* count)
    (when (agent-type ag :corr)
      (unless (< (agent-activation ag) cutoff)
        (show-one-hyp ag)
        (incf count) ))))


(defmacro do-all-hyps (var-name &body body)
  "Iterates over all hypoth-agents in a working memory. Similar to DO-ALL-WM."
  (declare (type (symbol var-name)))
  `(do-all-WM (,var-name *wm*)
     (when (typep ,var-name 'hypoth-agent)
       ,.body)))



;;;;;   ****  MAPPING, RATING, AND SKOLEM TABLES  ****

(defun mt (&optional (coalition *target*))      ; see AMBR/INTRFACE/MAPPING.LSP
  "Show the mapping table for a given coalition."
  (if (not (coalition-p coalition))
      (format t "~&~S is not a coalition.~%" coalition)
      (let ((table (mapping-table (coalition-mapping coalition)))
            (*print-case* :downcase) )
        (dolist (row table)
          (format t "~% ~15A ~15A ~5,2F   ~@[~A~]"
                    (agent-name (first row)) 
                    (if (second row) (agent-name (second row)) "<unmapped>")
                    (third row) (fourth row))) ))
  (values))


(defun rt (secretary)                          ; see AMBR/RATING.LSP
  "Show (and return) the rating table of SECRETARY."
  (if (not (typep secretary 'secretary-agent))
      (format t "~&~S is not a secretary agent." secretary)
      (let ((table (secretary-rating-table secretary))
            (*print-case* :downcase) )
        (if (null table)
            (format t
              "~&The rating table of ~S is empty. (~:[Not a~;A~]uthorized.)"
              secretary (authorized-p secretary))
            (dolist (row (rating-table table))
              (format t "~% ~,3F  ~,3F  ~A ~20A ~@[~S~]"
                        (IR-rating row)
                        (agent-activation (IR-hypothesis row))
                        (hypoth-type-code (IR-hypothesis row))
                        (agent-name (IR-hypothesis row))
                        (IR-other row))) )
        table )))


(defun skt (hypothesis)                                 ; see AMBR/SKOLEM1.LSP
  "Retrieve the Skolem table(s) of HYPOTHESIS."
  (if (not (typep hypothesis 'hypoth-agent))
      (format t "~&~S is not a hypothesis agent." hypothesis)
      (let ((tables (remove-if (complement #'Skolem-table-p)
                               (agent-buffer hypothesis))))
        (if (= 1 (length tables))
            (first tables)
            tables)) ))



;;;;;   ****  MONITORING THE ACTIVATION LEVELS  ****
;;
;; FLOW-IN (defined in DUAL/INTRFACE/CONNECT.LSP) collects connectionist input.

(defun show-coa (coalition)
  "Show the activation levels of the agents in a coalition."
  (declare (type (or symbol coalition) coalition))
  (cond ((null    coalition) nil)      ; no such coalition
        ((symbolp coalition) (show-coa (find-coalition coalition)))
        (t  (let* ((members (copy-list (coalition-members coalition)))
                   (sorted  (sort members #'>= :key #'agent-activation)) )
              (dolist (agent sorted)
                (multiple-value-bind (non-inhibited-flow-in total-flow-in)
                                     (flow-in agent :inhibited sorted)
                  (format t "~% ~,3F  ~(~15A~)  ~6,3F ~6,3F"
                            (agent-activation agent)
                            (agent-name agent)
                            total-flow-in
                            non-inhibited-flow-in) ))
              (count-if #'plusp sorted :key #'agent-activation))) ))


;;;;;;  End of file  AMBR/INTRFACE/REPORT.LSP
