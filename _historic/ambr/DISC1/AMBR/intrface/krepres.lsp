;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-interface -*-

;;; FILE:       AMBR/intrface/krepres.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Coalitions, meso-frames, etc.
;;; DEPENDS-ON: DUAL/intrface/coalitn.lsp; ambr/defs.lsp, ambr/krepres.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    26-05-98 [2.2.2]
;;; UPDATED:    14-08-98 The 'official release'
;;; UPDATED:    ...


        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;    KNOWLEDGE  REPRESENTATION  INTERFACE    ;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package  "AMBR-INTERFACE")


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: <none>

;; ...

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


(defmethod affiliate-agent :after ((new-member AMBR-agent)
                                   (situation  AMBR-agent)
                                   (weight     number) )
  (declare (ignore weight))
  (let ((coalition (find situation
                         *all-coalitions*      ; see DUAL/INTRFACE/COALITN.LSP
                         :key #'coalition-head)))
    (unless (or (null coalition)
                (member new-member (coalition-members coalition)))
      (setf (coalition-members coalition)
            (append (coalition-members coalition)
                    (list new-member))) )))

(defmethod remove-agent :after ((dying-instance instance-agent))
  (let* ((situation (agent-situation dying-instance))   ; possibly NIL
         (coalition (find situation                     ; NIL is OK for FIND
                          *all-coalitions*     ; see DUAL/INTRFACE/COALITN.LSP
                          :key #'coalition-head)) )
    (when (and situation coalition)
      (setf (coalition-members coalition)
            (remove dying-instance (coalition-members coalition))) )))



#|  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun check-secretaries (hypoth)
  (check-secretaries-aux hypoth (corresp-elt hypoth :slot1))
  (check-secretaries-aux hypoth (corresp-elt hypoth :slot2)) )

(defun check-secretaries-aux (hypoth secretary)
  (unless (find hypoth (agent-hypotheses secretary) :key #'symref-agent)
    (warn " ~S does not point to ~S." secretary hypoth)))


(defun check-mentors (hypoth)
  (dolist (mentor (corresp-justif hypoth))
    (check-mentors-aux hypoth mentor)))

(defun check-mentors-aux (hypoth mentor)
  (unless (find hypoth (get-filler-refs! mentor :t-link)
                       :key #'symref-agent)
    (warn " ~S is not a mentor of ~S." mentor hypoth)))


(defun check-rivals (hypoth)
  (let* ((l1 (agent-hypotheses (corresp-elt hypoth :slot1)))
         (l2 (agent-hypotheses (corresp-elt hypoth :slot2)))
         (u (nunion (mapcar #'symref-agent l1) (mapcar #'symref-agent l2)))
         (rivals (delete hypoth u)) )
   (dolist (rival rivals)
     (check-rivals-aux hypoth rival)) ))

(defun check-rivals-aux (hypoth rival)
  (if (locate-mfr-component hypoth :t-link)
      (let* ((conrefs (get-filler hypoth :t-link))
             (h->r (find rival conrefs :key #'conref-reference)) )
        (unless (and h->r (minusp (conref-weight h->r)))
          (warn " ~S does not inhibit ~S" hypoth rival) ))
      (warn " ~S does not have a :t-link slot to inhibit ~S" hypoth rival) ))


(defun check-CSN ()
  (do-all-hyps #'check-secretaries)
  (do-all-hyps #'check-mentors)
  (do-all-hyps #'check-rivals) )

|#


;;;;;;;;  End of file  AMBR/INTRFACE/KREPRES.LSP
