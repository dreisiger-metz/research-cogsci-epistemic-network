;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR-core -*-

;;; FILE:       AMBR/fizzle.lsp
;;; VERSION:    2.2.2  ; see AMBR/VERSION.LSP
;;; PURPOSE:    Voluntary destruction of temporary AMBR agents.
;;; DEPENDS-ON: DUAL; ambr/ambr_ag.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    04-04-98 [2.2.1]  Used parts of AMBR/HYPOTH and =/PROMOTN.LSP.
;;; UPDATED:    14-08-98 [2.2.2]  The 'official release'
;;; UPDATED:    ...


        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;;;;;;;;     F I Z Z L E   A TEMPORARY AGENT    ;;;;;;;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR-CORE")


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: fizzle-agent
;;          fizzle-message, send-fizzle-message, fizzle-message-sender
;;

;; FIZZLE-AGENT  (agent)  -->  T or NIL
;;
;;   A generic function that causes a temporary agent to die.
;;
;;   AGENT should be a (possibly dead) temporary AMBR agent. If not -- error.
;;
;;   If AGENT is dead already, FIZZLE-AGENT returns NIL.
;;   Otherwise, the symbolic processor of AGENT is blocked and it is purged
;;    out of the working memory.  This causes it to die (see DUAL/ARCHIT/
;;    TEMP_AG.LSP).  Finally, FIZZLE-AGENT returns T.


;; FIZZLE-MESSAGE
;;
;;   A symbolic structure that, when received by a temporary AMBR agent, causes
;;   it to fizzle.  Each fizzle message carries information about the agent
;;   who has sent it.

;; FIZZLE-MESSAGE-SENDER (fizzle-message)  -->  sender
;;
;;   A generic function that reads the sender of a fizzle message.  It is
;;   always an AMBR agent.
;;
;;   FIZZLE-MESSAGE should be a fizzle message. Otherwise an error is signaled.

;; SEND-FIZZLE-MESSAGE  (sender receiver)  -->  fizzle-message
;;
;;   A function that makes a fizzle message and sends it to RECEIVER.
;;

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric fizzle-agent (temp-agent)
  (:documentation "Voluntary destruction of a temporary agent." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


(defmethod  fizzle-agent ((agent temp-AMBR-agent))
  (remove-from-agenda agent)      ; terminate any symbolic processing
  (remove-from-WM agent T)        ; purge out of WM
  t)

(defmethod fizzle-agent ((dead dead-AMBR-agent))
  nil )

(defmethod fizzle-agent ((x t))
  (error "FIZZLE-AGENT: ~S is not a temporary AMBR agent." x ))



;;;;;;  ********    F I Z Z L E   M E S S A G E S    ********
;;;
;;;;;;   Class definition  (and accessor method)

(eval-when (compile load eval)
  (defclass  fizzle-message (symbolic-structure)
    ((sender   :reader     fizzle-message-sender
               :type       AMBR-agent
               :initarg    :sender
               :initform   (required-argument) )
    )
   (:documentation "Symbolic structure ordering a temporary agent to fizzle." ))
) ; eval-when

(defmethod  fizzle-message-sender ((x t))
  (error "FIZZLE-MESSAGE-SENDER: ~S is not a fizzle message." x ))

;;;;  Constructor

(defun send-fizzle-message (sender receiver)
  (declare (type AMBR-agent sender)
           (type temp-AMBR-agent receiver)
           (values fizzle-message) )
  (receive-symbol receiver (make-instance 'fizzle-message
                                          :sender sender )))

;;;;;;  Printing methods

(defmethod  print-object ((fizzle fizzle-message) stream)
  (if (and (slot-boundp fizzle 'sender)
           (agentp (fizzle-message-sender fizzle)) )
      (format stream "#<FZ ~A>"
                     (agent-name (fizzle-message-sender fizzle)))
      (format stream "#<malformed fizzle-message>") ))

(defmethod DUAL-describe ((fizzle fizzle-message)
                          &optional (stream *standard-output*) )
  (format stream "~&~S is a ~A.~%"
          fizzle (type-of fizzle) )
  (format stream "~&  Its sender is: ~S~%"
          (if (slot-boundp fizzle 'sender)
              (fizzle-message-sender fizzle)
              "(unbound)"))
  (values))


;; When a temporary agent receives a fizzle message, it fizzles.
;; In this version of AMBR, the contents of the message is not analyzed.

(defmethod handle-symbol ((agent  temp-AMBR-agent)
                          (fizzle fizzle-message) )
  (declare (ignore fizzle))       ; do not analyze contents
  (fizzle-agent agent) )


;;;;;;;  End of file AMBR/FIZZLE.LSP
