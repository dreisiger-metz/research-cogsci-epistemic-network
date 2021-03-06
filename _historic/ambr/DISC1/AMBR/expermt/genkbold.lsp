;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       ambr2/kb/expermt/genkb.lsp
;;; VERSION:    3.3
;;; PURPOSE:    Templates for generating variations of the LTM (ver. 3.3).
;;; DEPENDS-ON: ambr2/kb/ltm.lsp, ...
;;; PROGRAMMER: Alexander Alexandrov Petrov
;;; CREATED:    10-6-1997
;;; UPDATED:    ...



   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;     ? ? ?
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "AMBR")

;;;; This file defines ...


;; SYMBOLS: *relation-templates*, *object-templates*,
;;          compose-file-name, generate-KB, handle-template,
;;          add-INSTANCEs, *INSTANCE-link-pattern*,
;;          add-A-LINKs, *A-LINK-weight*
;;

(defvar *A-LINK-weight* 0.2
  "The weight of the A-LINKs generated by GENERATE-KB." )

(defvar *INSTANCE-link-pattern*
        '(0.4 0.3 0.2 0.1)   ;; That is, four :INSTANCE links are created;
                             ;; The first with weight 0.4, the 2nd -- 0.3, etc
  "The pattern of :INSTANCE links generated by GENERATE-KB." )


(defvar *relation-templates*   '(
  (on   :instance ((on-1 3) (on-2 1) (on-4 1) (*other* 10)) )
  (in   :instance ((in-1 2) (in-2 1) (in-3 1) (in-4 1)
                   (in-7 1) (in-9 2) (in-10 2) (*other* 5)) )
  (temperature-of  :instance ((T-of-1 2)  (T-of-2 2)  (T-of-3 1)
                              (T-of-4 1)  (T-of-5 1)  (T-of-8 1)
                              (T-of-10 2) (T-of-11 2) (*other* 5)) )
  (cold  :instance ((cold-8 2) (*other* 5)) )
  (hot   :instance ((hot-1 3) (hot-2 1) (hot-3 2) (hot-5 1) (*other* 5)) )
  (made-of  :instance ((made-of-1 1) (made-of-2 1) (made-of-3 1)
                       (made-of-5 1) (*other* 5)) )
  (material-metal  :instance ((mmetal-1 1) (*other* 5)) )
  (material-glass  :instance ((mglass-3 1) (mglass-5 1) (*other* 5)) )
  (material-wood   :instance ((mwood-2 2) (*other* 5)) )
  (color-of :instance ((color-of-1 1) (color-of-2 1)
                       (color-of-3 1) (*other* 10)) )
  (white    :instance ((white-1 1) (*other* 10)) )
  (black    :instance ((black-1 1) (*other* 10)) )
  (green    :instance ((green-1 1) (*other* 10)) )
  (burnt-out  :instance ((burnt-out-1 2) (*other* 5))
              :a-link   ((on-2 0.1)) )
  (broken     :instance ((broken-1 2) (*other* 5))
              :a-link   ((on-4 0.1)) )
  (dissipated :instance ((dissipated-1 2) (dissipated-2 2) (*other* 5))
              :a-link   ((burnt-out-1 0.1) (broken-1 0.1)) )
)) ; end of *RELATION-TEMPLATES*


(defvar *object-templates*   '(
  (water  :instance ((water-1 1) (water-2 1) (water-3 1)
                     (water-5 1) (*other* 5))
          :a-link   ((T-of-1 0.2) (T-of-4 0.1)) )
  (milk   :instance ((milk-2 2) (*other* 5))
          :a-link   ((T-of-11 0.2)) )
  (teapot :instance ((teapot-1 3) (teapot-3 1) (*other* 5)) )
  (glass  :instance ((glass-1 1) (glass-2 1) (*other* 5))
          :a-link   ((broken-1 0.1)) )
  (bowl   :instance ((bowl-1 3) (*other* 5))
          :a-link   ((burnt-out-1 0.3)) )
  (plate  :instance ((plate-1 3) (plate-2 1) (*other* 5))
          :a-link   ((T-of-2 0.1)) )
  (fridge :instance ((fridge-1 3) (*other* 5))
          :a-link   ((T-of-10 0.1)) )
  (fire   :instance ((fire-2 2) (*other* 5)) )
  (immersion-heater  :instance ((imm-heater-1 5) (*other* 5))
                     :a-link   ((T-of-5 0.2)) )
)) ; end of *OBJECT-TEMPLATES*


;;;;;;  Main functions

(defun generate-KB (number)
  "Generates a file containing calls to ADD-LINK."
  (declare (type (integer 0 *) number))
  (let ((file-name (compose-file-name number)))
    (with-open-file (stream file-name :direction :output :if-exists :rename)
      (format stream ";; This file has been generated automatically.")
      (format stream
        "~%;; It adds 'downward' links to some agents from AMBR2/KB/LTM.LSP.")
      (format stream "~%;; See AMBR2/EXPERMT/GENKB.LSP for details.")
      (format stream "~%~%(in-package \"AMBR\")" )
      (format stream "~%~%;; Relations")
      (dolist (template *relation-templates*)
        (handle-template template stream))
      (format stream "~%~%;; Objects")
      (dolist (template *object-templates*)
        (handle-template template stream))
      (format stream "~%~%;; End of file")
      (format t "~&;; ~S written successfully." file-name) )))

(defun compose-file-name (number)
  "Composes a file name for GENERATE-KB."
  (declare (type (integer 0 *) number))
  (format nil "D:\\DUAL\\AMBR2\\KB\\GENKB\\genkb~3,'0D.lsp" number) )

(defun handle-template (template &optional (stream *standard-output*))
  "Generates calls to ADD-LINK according to a given template."
  (flet ((parser (agent-name &key instance a-link)
           (values agent-name instance a-link) ))
    (multiple-value-bind (agent-name instance-template a-link-template)
                         (apply #'parser template)
      (unless (endp instance-template)
        (add-INSTANCEs agent-name instance-template stream))
      (unless (endp a-link-template)
        (add-A-LINKs agent-name a-link-template stream))  )))


;;;;;;; Functions handling individual (kinds of) templates

(defun add-INSTANCEs (agent-name template stream)
  "Generates INSTANCE links according to TEMPLATE and *INSTANCE-LINK-PATTERN*."
  (let ((links '())
        (sample-size 0) )
    (flet ((push-link (agent weight)
             (let ((old-link (find agent links :key #'car)))
               (if (null old-link)
                   (push (cons agent weight) links)   ; reverse order is OK
                   (incf (cdr old-link) weight)))) )
      ;; Calculate SAMPLE-SIZE.
      (dolist (pair template)
        (incf sample-size (second pair)) )
      ;; Generate links by sampling (according to *INSTANCE-LINK-PATTERN*).
      (dolist (weight *INSTANCE-link-pattern*)
        (push-link (first (sample template sample-size)) weight) )
      ;; Write calls to ADD-LINK to the output stream and terminate.
      (dolist (link links)
        (format stream "~%(add-link #$~A :instance #$~A ~F)"
                       agent-name (car link) (cdr link))) )))

(defun sample (pairs total-frequency)
  ;; The second elt of each pair is frequency. (The first elt is arbutrary.)
  ;; Assume the sum of all frequencies is equal to TOTAL-FREQUENCY.
  ;; Select and return one pair with probability proportional to its frequency.
  (declare (type (integer 1 *) total-frequency))
  (let ((rnd (random total-frequency)))
    (dolist (pair pairs :error)
      (decf rnd (second pair))
      (if (minusp rnd)
          (return pair)
          )  ; continue
    )))


(declaim (inline biased-coin-p))
(defun biased-coin-p (prob-success)
  "Returns T with probability p=PROB-SUCCESS and NIL with q=1-p."
  (declare (type (float 0.0 1.0) prob-success))
  (< (random 1.0) prob-success) )

(defun add-A-LINKs (agent-name template stream)
  "Generates A-LINKs according to TEMPLATE and with *A-LINK-WEIGHT*."
  (dolist (pair template)
    (when (biased-coin-p (second pair))   ; (second pair) gives the probability
      (format stream "~%(add-link #$~A :a-link   #$~A ~F)"
                     agent-name (first pair) *A-LINK-weight*) )))



;;;;;;  End of file  AMBR2/KB/EXPERMT/GENKB.LSP
