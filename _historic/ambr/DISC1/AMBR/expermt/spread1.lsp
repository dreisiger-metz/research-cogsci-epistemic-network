;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-

;;; FILE:       ambr2/expermt/spread1.lsp
;;; VERSION:    1.0
;;; PURPOSE:    Spreading activation experiment -- decay when *input* = 0.
;;; PROGRAMMER: Alexander Alexandrov Petrov
;;; DEPENDS-ON: DUAL; dual/expermt/spread.lsp
;;; CREATED:    05-05-97
;;; UPDATED:    14-01-98 -- DO-ALL-AGENTS is now a macro (see ARCHIT/BASIC.LSP)


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;    SPEADING  ACTIVATION  EXPERIMENT  1    ;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package "DUAL-CORE")
(use-package "DUAL-INTERFACE")

;;;; PURPOSE:   To explore the asymptotic behavior of the network in the
;;;;            absence of any external input.
;;;;
;;;; IND_VARS:  *decay-rate*, *excit-rate*, *threshold*
;;;;            initial-WM-size, initial-WM-total-act
;;;;
;;;; DEP_VARS:  N-cycles, asym-WM-size, asym-WM-total-act, asym-WM-sum-sq,
;;;;            asym-focus-act, focus-changes
;;;;
;;;; OUTFILES:  "C:\\EXPERMT\\spr1_???.dat"
;;;;

;;;;;;;;  EXPERIMENTAL DESIGN:
;;;;
;;;;  The experiment consists of a number of EPOCHS.
;;;;  Each epoch consists of a number of SUBEPOCH.
;;;;  Each subepoch consists of a number of TRIALS.
;;;;  Each trial consists of a number of CYCLES -- running the network.
;;;;
;;;;  EPOCHS define values for: *decay-rate*, *excit-rate*
;;;;  SUBEPOCHS define values for: initial-WM-size, initial-WM-total-act
;;;;  TRIALS generate (random) initial network states, satisfying the
;;;;    constraints set: initial-WM-size and initial-WM-total-act.
;;;;  CYCLES run the network to completion.
;;;;


;;;;;;;   ******   EXPERIMENT   ******

(defparameter *file-name-prefix*
              "C:\\EXPERMT\\spr1_" )

(defparameter *decay-rate-domain*
              '(0.1 0.25 0.5 0.75 1.0) )     ; 5 levels

(defparameter *excit-rate-domain*
              '(0.1 0.5 1.0) )               ; 3 levels

(defparameter *threshold-domain*             ; 3 levels
              '(0.05 0.1 0.2) )

;; Thus, the experiment consists of 5*3*3* = 45 epochs.


(defvar *epoch-no* 0)
(defvar *trial-no* 0)

(defvar *data-stream* *standard-output*)

(defun generate-epochs ()
  "Generates a list of epoch descriptions."
  (let ((epoch-no 0)
        (result '()) )
    (dolist (curr-decay *decay-rate-domain*)
      (dolist (curr-excit *excit-rate-domain*)
        (dolist (curr-threshold *threshold-domain*)
           (incf epoch-no)
           (push (list epoch-no
                       curr-decay
                       curr-excit
                       curr-threshold)
                 result) )))
    (nreverse result) ))


(defun do-experiment (&optional (file-name-prefix *file-name-prefix*))
  (dolist (epoch-description (generate-epochs))
    (let* ((epoch-no (first epoch-description))
           (filename (format nil "~A~D.dat" file-name-prefix epoch-no)) )
      (with-open-file (file filename :direction :output)
        (format *terminal-io* "~%Now writing data file ~A  " filename)
        (let ((*data-stream* file)
              (*random-state* (make-random-state t)) )    ; Set new seed
           (do-epoch epoch-description)
        )))))


;;;;;;;   ******   EPOCHS  and  SUBEPOCHS  ******

(defparameter *P-N-domain*     ; 4 levels    , 16 trials
              '((0.1  5)       ; P{add} = 0.1, N-trials = 5
                (0.2  5)       ; P{add} = 0.2, N-trials = 5
                (0.5  5)       ; P{add} = 0.5, N-trials = 5
                (1.0  1) )     ; P{add} = 1.0, N-trials = 1  (deterministic)
)

(defparameter *init-act-domain*    ; 4 levels
              '(t 0.5 0.75 1)      ; T stands for *THRESHOLD*
  "INIT-ACT is the initial activation of each agent in *WM*." )

;; Thus, each epoch consists of 4*4 = 16 subepochs.
;; Each epoch consists of 16*4 = 64 trials.
;; The whole experiment consists of 45*64 = 2880 trials.

(defun do-epoch (epoch-description)
  (let ((*epoch-no*   (first  epoch-description))
        (*decay-rate* (second epoch-description))
        (*excit-rate* (third  epoch-description))
        (*threshold*  (fourth epoch-description))
        (*WM-threshold* (fourth epoch-description)) )
  (setq *trial-no* 0)
  (dolist (P-N *P-N-domain*)
    (let ((P-add (first P-N))
          (N-trials (second P-N)) )
      (princ #\Space *terminal-io*)
      (dolist (init-act *init-act-domain*)
        (dotimes (n N-trials)
          (do-trial P-add init-act) ))))))


;;;;;;;   ******   TRIALS  ******

(defvar *cycles* 0  "Cycles until the network settles.")

(defvar *initial-WM-size* 0)
(defvar *initial-WM-total-act* 0.0)

(defvar *focus-changes* 0  "How many changes in the focus were there?" )
(setq *focus-change-hook*
      #'(lambda () (incf *focus-changes*)) )

(defun do-trial (P-add init-act)
  (incf *trial-no*)
  (princ "." *terminal-io*)     ; Show to user that still living....
  (initialize-WM P-add init-act)
  (setq *cycles* (run-to-asymptote))     ; The innermost cycle
  (report-trial)
  (force-output) )


(defun initialize-WM (P-add init-act)
  ;; Clean up previous.
     (setq *cycles* 0)
     (setq *focus-changes* 0)
     (setq *focus* :focus-undefined)
     (remove-all-WM *WM*)
  ;; Create new initial *WM* state (if not empty).
     (fill-WM P-add init-act)                 ; the real work
     (setq *initial-WM-size* (WM-size *WM*))
     (setq *initial-WM-total-act* (WM-total-act *WM*))
)

(defun  fill-WM  (P-add init-act)
  (let ((init-act-1 (if (eq init-act t)
                        *threshold*
                        init-act)) )
    (flet ((put-to-WM (agent)
             (add-to-WM agent *WM*)
             (setf (agent-activation agent) init-act-1) ))
      (if (< P-add 0.99)   
          (do-all-agents (ag)
            (when (< (random 1.0) P-add)
               (put-to-WM ag) ))
          (do-all-agents (ag)
            (put-to-WM ag)) )  ; deterministically
)))


;;;;;;  ******  REPORTING  RESULTS  *******
;;
;; IND_VARS:  *decay-rate*, *excit-rate*, *threshold*
;;            initial-WM-size, initial-WM-total-act
;;
;; DEP_VARS:  N-cycles, asym-WM-size, asym-WM-total-act, asym-WM-sum-sq,
;;            asym-focus-act, focus-changes


(defun report-trial ()
  ;; Independent variables
     (format *data-stream* "~%~4D ~4D  ~4,2F ~4,2F ~4,2F ~4D ~6,2F   "
               *epoch-no*
               *trial-no*
               *decay-rate*
               *excit-rate*
               *threshold*
               *initial-WM-size*
               *initial-WM-total-act* )
  ;; Dependent variables
     (format *data-stream* " ~4D    ~4D  ~6,2F ~7,3F   ~5,3F ~4D"
               *cycles*
               (WM-size *WM*)
               *WM-total-act*
               *WM-sum-sq*
               (if (eq *focus* :focus-undefined)
                   0.0
                   (agent-activation *focus*) )
               *focus-changes* )
)

;;;;;;;;;;  End of file  AMBR2/EXPERMT/SPREAD1.LSP
