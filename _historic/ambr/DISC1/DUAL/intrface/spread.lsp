;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/spread.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Interface to ARCHIT/SPREAD.LSP and ARCHIT/WORK_MEM.LSP
;;; DEPENDS-ON: DUAL/archit/spread.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov
;;; CREATED:    05-05-97 [1.0]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;
;;; TO_DO:      SWM in arbitrary number of columns (not just one or two).
;;;


;; SYMBOLS:  swm,
;;           spr,
;;           snapshot-WM, WM-total-act,
;;           deactivate-all
;;

(cl:in-package "DUAL-INTERFACE")


;;;;; SWM (and friends) show working-memory contents.

(defun swm (&optional (depth *SWM-depth*)
                      (WM *WM*)
                      (two-columns-p *SWM-in-two-columns-p*) )
  "Show working memory contents."
  (assert (typep depth '(or (integer 0 *) boolean))  (depth)
          "The first argument (~S) to SWM should be a positive integer, ~
           T (meaning 'all'), or NIL (meaning '0')."  depth)
  (assert (WM-p WM)  (WM)
          "The second argument (~S) to SWM should be a working memory."  WM)
  (let* ((contents (sort-WM WM))
         (size (WM-size WM))
         (lines (swm-lines depth size two-columns-p)) )
    (flet ((agent->string (ag)
             (format nil "~5,3F ~:[ ~;*~] ~(~30A~)"
                (agent-activation ag)
                (active-processor-p ag)
                (agent-name ag)) ))
      (format t "~%WM contains ~D agent~:P with total activation ~,2F.~%"
                size (WM-total-act WM) )
      (dotimes (k lines)
         (format t "  ~30@<~A~>  ~@[~A~]~%"
                   (agent->string (aref contents k))
                   (if (and two-columns-p
                            (< (+ k lines) size) )
                       (agent->string (aref contents (+ k lines)))
                       nil) ))
      (values) )))

(defun swm-lines (depth size two-columns-p)
  "How many lines will be printed by SWM."
  (flet ((consider-columns (lines)
           (if two-columns-p
               (ceiling lines 2)
               lines) ))
    (case depth     ; DEPTH is one of: T, NIL, or an integer.
      ((nil) 0)
      ((t)   (consider-columns size))
      (t     (consider-columns (min (* 2 depth) size))) )))



;;;;; SPR spreads activation a few times and then shows WM contents.
;; See also RA in DUAL/INTRFACE/AGENDA.LSP

(defun spr (&optional (cycles 1) (show-depth *SWM-depth*))
  "SPREAD several times and show WM."
  (check-type cycles integer)
  (dotimes (k cycles)
    (spread))
  (swm show-depth) )


;;;;;; Other useful functions

(defun snapshot-WM (&optional (WM *WM*))
  "Returns a list reflecting the current state of WM."
  (assert (WM-p WM)  (WM)
          "The argument (~S) to SNAPSHOT-WM should be a working memory."  WM)
  (let ((result '()))
    (do-all-wm (ag WM (nreverse result))
        (push (list ag (agent-activation ag))
              result)) ))


(defun WM-total-act (&optional (WM *WM*))
  "Returns the sum of activation of all members of WM."
  (assert (WM-p WM)  (WM)
          "The argument (~S) to WM-TOTAL-ACT should be a working memory."  WM)
  (let ((sum 0.0))
    (do-all-wm (ag WM sum)
        (incf sum (agent-activation ag))) ))


(defun deactivate-all (&optional (special-WMs *special-WMs*)
                                 (primary-WM *WM*) )
  "Sets the activation level of all agents involved to zero."
  (assert (and (listp special-WMs)
               (every #'WM-p special-WMs)) (special-WMs)
          "The first argument (~S) to DEACTIVATE-ALL ~
           should be a list of (special) working memories."
          special-WMs)
  (assert (WM-p primary-WM)  (primary-WM)
          "The second argument (~S) to DEACTIVATE-ALL ~
           should be a (primary) working memory."
          primary-WM)
  (remove-all-wm primary-WM) 
  (dolist (special-WM special-WMs)
    (do-all-wm (agent special-WM)
      (setf (agent-activation agent) 0.0) ))
)

;;;; End of file DUAL/INTRFACE/SPREAD.LSP
