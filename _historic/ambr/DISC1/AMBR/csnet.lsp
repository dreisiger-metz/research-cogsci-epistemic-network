;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/csnet.lsp
;;; VERSION:    2.2.2    ; see AMBR/VERSION.LSP
;;; PURPOSE:    Constraint satisfaction network
;;; DEPENDS-ON: DUAL; ambr/defs.lsp, ambr/ambr_ag.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    25-12-97 -- Second part of the older file AMBR/HYPOTH.LSP.
;;; UPDATED:    14-08-98 [2.2.2] The 'official release'
;;; UPDATED:    ...
;;;;;;
;;; TO DO:      Update the method for NORMALIZE-NEIGHBORS -- :DESTRUCTIVE-P etc.
;;; TO DO:      Optimize xxx-HYPOTH-ACT-FUN.  See DUAL/ARCHIT/ACT_FUN.LSP.


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;     CONSTRAINT  SATISFACTION  NETWORK      ;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR-CORE")

;;;; This file does not define any new concept.  It implements the specific
;;;; connectionist machinery for the constraint-satisfaction network.  The
;;;; CS network contains hypothesis agents whose special activation function
;;;; is the basis for 'relaxation' of the network and emergence of a coherent
;;;; set of hypotheses.   [...]
;;;;


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: encode-hypoth-activation, decode-hypoth-activation,
;;          Grossberg-hypoth-act-fun, Rum&McCl-hypoth-act-fun,
;;          hypoth-output-fun
;;
;; The functions in this file depend on the following parameters from DEFS.LSP:
;;   *hypoth-max-act*, *hypoth-zero-act*, *hypoth-min-act*,
;;   *hypoth-excit-rate*, *hypoth-decay-rate*, *hypoth-output-factor*
;;

;; ...

;;;;;; Generic function(s) pertaining to the external protocol

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;   Class definitions -- see HYPOTH-AGENT, etc. in AMBR/AMBR_AG.LSP
;;;;;;   Type predicates   -- see HYPOTHESIS-P, etc. in AMBR/AMBR_AG.LSP
;;;;;;   Printing methods  -- see AMBR/AMBR_AG.LSP and AMBR/HYPOTH.LSP


;;;;;;  *****  INHIBITORY  INPUT  ZONE  *****
;;;
;;;  Most of the agents in the architecture DUAL receive only excitatory
;;;  input and, therefore, have only one connectionist input zone. (See
;;;  subsection 3.2.4.1 in "DUAL Report #1" and file DUAL/ARCHIT/CONNECT.LSP.)
;;;
;;;  Hypothesis agents, however, are an exception.  They compete with their
;;;  rivals by means of inhibitory links and, therefore, receive both
;;;  excitatory and inhibitory activation.  Thus, they have a second input
;;;  zone for inhibitory input. The main input zone inherited from general
;;;  DUAL agents retains its usual function as receptacle of excitatory input.
;;;
;;;  The main methods for INHIB-INPUT-ZONE are defined automatically by
;;;  DEFCLASS in the file AMBR/AMBR_AG.LSP.  The present file defines some
;;;  auxiliary methods.

(defmethod DUAL-describe :after ((agent hypoth-agent)
                                 &optional (stream *standard-output*) )
  (format stream "~&  Its inhibitory input is: ~:[(unbound slot)~;~:*~6,3F~]~%"
                 (if (slot-boundp agent 'inhib-input)
                     (inhib-input-zone agent)
                     nil) )
  (values))

(defmethod  inhib-input-zone ((x t))
  (error "AMBR-CORE::INHIB-INPUT-ZONE:  ~S is not a hypothesis agent." x ))

(defmethod  (setf inhib-input-zone) (new-value (x t))
 (declare (ignore new-value))
 (error "(SETF AMBR-CORE::INHIB-INPUT-ZONE):  ~S is not a hypothesis agent." x))


(defmethod  prepare-to-receive-activation :after ((ag hypoth-agent))
  (setf (inhib-input-zone ag) 0.0) )             ; see DUAL/ARCHIT/CONNECT.LSP


(defmethod  receive-activation  ((ag hypoth-agent)
                                 (amount float))
  (declare (type single-float amount))
  (if (plusp amount)
      (incf (dual-core::conn-input-zone  ag) amount)
      (incf (ambr-core::inhib-input-zone ag) amount) )
  amount)

(defmethod receive-activation ((dead dead-hypoth-agent)   ; see DUAL/ARCHIT/
                               (amount number))           ;         TEMP_AG.LSP
  (cut-links-to-agent dead)    ; collect garbage once again
  amount )    ; conform to the external protocol of RECEIVE-ACTIVATION


(defmethod  update-activation  ((ag hypoth-agent))
  (setf  (agent-activation ag)
         (agent-new-activation ag
                               (dual-core::conn-input-zone ag)
                               (inhib-input-zone ag)) ))



;;;;;;  *****  NO  LINK  NORMALIZATION  *****
;;;
;;;  Hypothesis agents are exceptional in yet another way -- their outgoing
;;;  links are not normalized.  This better fits the purpose of the constraint
;;;  satisfaction network.


(defmethod  normalize-neighbors ((agent hypoth-agent)
                                 &key (control nil) destructive-p )
  ;; See DUAL/ARCHIT/CONNECT.LSP for the 'main' method
  (declare (ignore control destructive-p)
           (values list))
  (agent-neighbors agent) )   ; Return the value advertized in the external
                              ; protocol without actually normalizing anything.


;;;;;;  *****  SPECIALIZED  ACTIVATION  FUNCTIONS  *****
;;;
;;;  Constraint satisfaction is implemented by the connectionist aspect
;;;  of hypothesis agents.
;;;  A modification of Grossberg-act-fun is used.
;;;  (Rum&McCl-two-tail-act-fun may also be used with similar modification.)
;;;  The neutral point (i.e. the 'zero') has been shifted to *HYPOTH-ZERO-ACT*
;;;  in order to fit the activation above *AMBR-threshold*.
;;;
;;;  Compare the functions below with that in DUAL/ARCHIT/ACT_FUN.LSP and
;;;  DUAL/ARCHIT/CONNECT.LSP.
;;;
;;;  (The links in the constraint-satisfaction network are created by other
;;;   modules -- marker-passing, structure-correspondence, secretaries.)

(declaim (inline encode-hypoth-activation decode-hypoth-activation))

(defun encode-hypoth-activation (raw-activation)
  (declare (type single-float raw-activation)
           (values single-float) )
  (+ raw-activation *hypoth-zero-act*) )

(defun decode-hypoth-activation (coded-activation)
  (declare (type single-float coded-activation)
           (values single-float) )
  (- coded-activation *hypoth-zero-act*) )


(defun  Rum&McCl-hypoth-act-fun  (coded-old-act  net-input)
  "Modification of Rum&McCl-two-tail-act-fun used in hypothesis agents."
  ;; Used in the older versions. Now Grossberg-HYPOTH-ACT-FUN is used.
  (declare (type   single-float coded-old-act net-input)
           (values single-float)
           (inline decode-hypoth-activation encode-hypoth-activation) )
  (let* ((decoded-old-act (decode-hypoth-activation coded-old-act))
         (external-factor (if (plusp net-input)
                              (* *hypoth-excit-rate*   ; positive branch
                                 net-input
                                 (- *hypoth-max-act* coded-old-act))
                              (* *hypoth-inhib-rate*   ; negative branch
                                 net-input
                                 (- coded-old-act *hypoth-min-act*)) ))
         (a-prime (- external-factor
                     (* *hypoth-decay-rate*
                        decoded-old-act))) )
    (encode-hypoth-activation (+ decoded-old-act
                                 (* a-prime *time-slice*))) ))


(defun  Grossberg-hypoth-act-fun  (coded-old-act  enet inet)
  "Modification of Grossberg-act-fun used in hypothesis agents."
  (declare (type   single-float coded-old-act enet inet)
           (values single-float)
           (inline decode-hypoth-activation encode-hypoth-activation) )
  (let* ((decoded-old-act (decode-hypoth-activation coded-old-act))
         (a-prime (+ (* (- *hypoth-decay-rate*)
                        decoded-old-act)
                     (* *hypoth-excit-rate*
                        enet
                        (- *hypoth-max-act* coded-old-act))
                     (* *hypoth-inhib-rate*
                        inet
                        (- coded-old-act *hypoth-min-act*)) )))
    (encode-hypoth-activation (+ decoded-old-act
                                 (* a-prime *time-slice*))) ))


(defmethod  agent-new-activation ((ag hypoth-agent)
                                  (net-input float)
                                  &rest more-inputs )
  (declare (type single-float net-input)
           (values single-float)
           (inline Grossberg-hypoth-act-fun) )
  (Grossberg-hypoth-act-fun (agent-activation ag)        ; old act
                            net-input                    ; enet
                            (first more-inputs)) )       ; inet


;; EMBRYO-HYPOTH-AGENTs differ from MATURE-HYPOT-AGENTs in their output functn.
;; Embryos have no output, i.e. hypotheses do not affect the activation level
;; other agents unless and until they establish themselves.
;; (The other difference is that mature hypotheses are NCR-sending agents.)

(defmethod agent-output ((ag embryo-hypoth-agent))
  (declare (ignore ag))
  0.0 )

(defmethod  agent-output ((ag mature-hypoth-agent))
  (declare (values single-float)
           (inline decode-hypoth-activation) )
  (* *hypoth-output-factor*
     (threshold-0 (decode-hypoth-activation (agent-activation ag)))) )


;;;;;;;  End of file AMBR/CSNET.LSP
