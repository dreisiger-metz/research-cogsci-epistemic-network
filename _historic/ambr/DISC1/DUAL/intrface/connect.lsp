;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/connect.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Link-related utilities  (cf. DUAL/ARCHIT/CONNECT.LSP).
;;; DEPENDS-ON: DUAL/archit/basic.lsp, =/connect.lsp, =/work_mem.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    11-11-97 [1.1]
;;; UPDATED:    25-06-98 [1.1.2] Added FLOW-IN and FLOW-OUT.
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;     L I N K - R E L A T E D   U T I L I T I E S     ;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-INTERFACE")

;;;;  This file defines a few functions which are not allowed by the
;;;;  conceptual specification of the architecture (see DR#1) and thus
;;;;  are not included in the DUAL-core package.  They are not allowed
;;;;  because they violate the principle of locality in the architecture.
;;;;  Moreover, these functions go _against_ the direction of the links.
;;;;
;;;;  This file extends the functionality of dual/ARCHIT/connect.lsp.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS:  supported-by, fan-in, fan-out,
;;           flow-in, flow-out
;;

;; FAN-OUT (agent)  -->  conref-list
;;
;;   A function that is a synonym of AGENT-NEIGHBORS (see ARCHIT/CONNECT.LSP).
;;   It is defined by:
;;      (declaim (inline fan-out))
;;      (defun fan-out (agent) (agent-neighbors agent))
;;
;;   Note that FAN-OUT is not generic while FAN-IN and AGENT-NEIGHBORS are.
;;   Compare with FLOW-OUT below.
;;

;; FAN-IN (agent &optional inhibited)  -->  conref-list
;;
;;   A generic function that returns a list of all links going _to_ AGENT.
;;   Some of the links may be inhibited and will not be listed.
;;   Returns a list of 'reversed' connectionist references -- a list that is
;;   similar to the one computed by FAN-OUT but the SYMREF field of each conref
;;   shows the _beginning_ of the link (the sender), not the _end_ of the
;;   link (the receiver).
;;
;;   AGENT should be an agent (see AGENT-P in DUAL/ARCHIT/BASIC.LSP).  If it
;;     isn't, an error is signaled.
;;   INHIBITED should be a (possibly empty) list of connectionist agents.
;;     When not supplied, it defaults to the empty list.
;;
;;   The function returns a list of conrefs (call it SUPPORT-LIST) so that:
;;     1. Each item in SUPPORT-LIST is a 'reversed conref' produced by a
;;        call to SUPPORTED-BY (see below).
;;     2. If CANDIDATE is some agent registered in the system, and CANDIDATE
;;        is not a member of the INHIBITED list, and the form
;;          (supported-by agent candidate)  returns non-NIL,
;;        then the 'reversed conref' produced by the aforementioned form is
;;        included in SUPPORT-LIST.
;;     3. All agents in the system are checked for potential supporters.
;;        (That is, FAN-IN involves an imlicit call to DO-ALL-AGENTS.)
;;     4. SUPPORT-LIST does not contain any items other than the 'reversed
;;        conrefs' produced according to step 2. above.
;;     5. The order of the 'reversed conrefs' in SUPPORT-LIST is unspecified.
;;
;;
;;   (Rationale for INHIBITED: A theoretical construct that does not appear
;;    explicitly in this documentation but is very important in practice is
;;    the so-called COALITION of agents. A coalition is a set (implemented
;;    here as a list) of agents. The members of a coalition are, by definition,
;;    interconnected; usually very interconnected. Thus, it is often trivial
;;    that AGENT gets support from the other members of its coalition.
;;    INHIBITED may be used to mask such obvious supporters and to look
;;    for support coming from outside the coalition.)
;;    Coalitions have been implemented in ver.1.1.2: DUAL/INTRFACE/COALITN.LSP.
;;
;;    Compare with FLOW-IN below.
;;

;; SUPPORTED-BY (supportee supporter)  -->  conref  or  NIL
;;
;;   A generic function that returns NIL if there is no link from SUPPORTER
;;   to SUPPORTEE or non-NIL otherwise.
;;
;;   SUPPORTEE should be an agent (not necessr connectionist). If not, error.
;;   SUPPORTER should be a connectionist agent; if it isn't, an error is signld.
;;
;;   The function acts as a semi-predicate. It returns NIL if there is no
;;   link from SUPPORTER to SUPPORTEE. If there is such link, SUPPORTED-BY
;;   returns a 'reversed conref' -- that is, a conref whose reference field
;;   contains the sender, not the receiver of the link.
;;
;;   Example:
;;    ;; Suppose  (agent-neighbors #$ag1)  -->  ((#$ag2 . 0.4) (#$ag3 . 0.6))
;;                (agent-neighbors #$ag2)  -->  ((#$ag3 . 1.0))
;;    ;; Then the following is true:
;;    (supported-by #$ag1 #$ag2)  -->  NIL
;;    (supported-by #$ag2 #$ag1)  -->  (#$ag1 . 0.4)
;;    (supported-by #$ag3 #$ag2)  -->  (#$ag2 . 1.0)
;;

;; FLOW-OUT (agent)  -->  connectionist-output
;;
;;   A function that is a synonym of AGENT-OUTPUT (see ARCHIT/CONNECT.LSP).
;;   It is defined by:
;;      (declaim (inline fan-out))
;;      (defun flow-out (agent) (agent-output agent))
;;
;;   Note that FLOW-OUT is not generic while AGENT-OUTPUT is.  Cf. FAN-OUT.
;;

;; FLOW-IN (agent &key inhibited primary-WM special-WMs)  -->  conn-input
;;
;;   A function that collects all connectionist input flowing into a given
;;   agent, possibly with the exception of some inhibited supporters.
;;   Returns two float values: non-inhibited-flow-in  and  total-flow-in.
;;
;;   AGENT should be an agent (see AGENT-P in DUAL/ARCHIT/BASIC.LSP).
;;   INHIBITED should be a (possibly empty) list of connectionist agents.
;;     When not supplied, it defaults to the empty list.
;;   PRIMARY-WM should be a 'primary working memory' and SPECIAL-WMs should be
;;     a (possibly empty) list of 'special WMs' -- see DUAL/ARCHIT/WORK_MEM.LSP.
;;     When not supplied, they default to *WM* and *SPECIAL-WMS* respectively.
;;
;;   FLOW-IN goes through PRIMARY-WM and SPECIAL-WMS in a way analogous to the
;;   function SPREAD (defined in DUAL/ARCHIT/SPREAD.LSP) and collects all
;;   activation that would go to AGENT.  No activation is actually transferred.
;;     ... inhibited vs. non-inhibited ...
;;     ...

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric supported-by (supportee supporter)
  (:documentation
    "Semipredicate wheter there is a link from SUPPORTER to SUPPORTEE." ))

(defgeneric fan-in (supportee &optional inhibited)
  (:documentation
"A list of the total support for SUPPORTEE, except for INHIBITED supporters." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


(defmethod supported-by ((supportee base-agent)
                         (supporter connectionist-agent) )
  (let ((conref (find supportee
                      (agent-neighbors supporter)
                      :key #'conref-reference)) )
    (if (null conref)
        nil
        (make-conref supporter (conref-weight conref)) )))

(defmethod supported-by ((x t) (y t))
  (error "SUPPORTED-BY: ~S or ~S is not a (connectionist) agent."
         x y ))


(defmethod fan-in ((supportee base-agent)
                               &optional (inhibited nil) )
  (declare (type list inhibited))
  (let ((result '()))
    (do-all-agents (agent result)
      (when (typep agent 'connectionist-agent)    ; potential supporter ?
        (let ((support (supported-by supportee agent)))
          (unless (or (member agent inhibited)
                      (null support) )
            (push support result))) ))))

(defmethod fan-in ((x t) &optional inhibited)
  (declare (ignore inhibited))
  (if x
    (error "FAN-IN: ~S is not an agent." x)
    (error "FAN-IN applied to NIL (perhaps #$missing-agent).") ))


(declaim (inline fan-out))
(defun fan-out (agent)
  "The list of all links going _out_ of AGENT. Synonym of AGENT-NEIGHBORS."
  (agent-neighbors agent))


;;;;;;  FLOW-IN and FLOW-OUT

(declaim (inline flow-out))
(defun flow-out (agent)
  "The activation going _out_ of AGENT. Synonym of AGENT-OUTPUT."
  (agent-output agent))

(defun  flow-in (receiver &key (inhibited nil) 
                               (primary-WM *WM*) (special-WMs *special-WMs*) )
  "The total activation flowing in RECEIVER, except from INHIBITED supporters."
  (declare (type base-agent receiver)
           (type list inhibited special-WMs)
           (type working-memory primary-WM)
           (values float float) )            ; (values non-inhibited total)
  (let ((total 0.0)
        (non-inhibited 0.0) )
    (flet ((do-one (agent)
             (let* ((conref (supported-by receiver agent))
                    (activation (if (null conref)
                                    0.0      ; no link from AGENT to RECEIVER
                                    (* (conref-weight conref)
                                       (agent-output agent)))) )
               (unless (null conref)
                 (incf total activation)
                 (unless (member agent inhibited)
                   (incf non-inhibited activation))) )))
      ;; Go through all working memories and collect activation.
      ;; See DUAL/ARCHIT/WORK_MEM.LSP and DUAL/ARCHIT/SPREAD.LSP.
      (do-all-WM (agent primary-WM)
        (do-one agent))
      (dolist (special-WM special-WMs)
        (do-all-WM (agent special-WM)
          (do-one agent))
        (when (eq (now-in-WM receiver) special-WM)        ; member of this WM ?
          (let* ((conref (find receiver (WM-conrefs special-WM)
                                        :key #'conref-reference))
                 (exogeneous-act (* (WM-act-source special-WM)
                                    (conref-weight conref))) )
            (incf total exogeneous-act)
            (incf non-inhibited exogeneous-act))) )
      (values non-inhibited total) )))


;;;;;;;  End of file DUAL/INTRFACE/CONNECT.LSP
