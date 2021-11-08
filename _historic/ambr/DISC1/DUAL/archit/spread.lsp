;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/spread.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Spreading activation mechanism.
;;; DEPENDS-ON: DUAL/defs.lsp, DUAL/archit/work_mem.lsp, DUAL/archit/dual_ag.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    29-04-97 [1.0]
;;; UPDATED:    19-12-97 [1.1]
;;; UPDATED:    24-03-98 [1.1.2]  Defined generic FOCUS-CHANGED.
;;;                               Abandoned *FOCUS-CHANGE-HOOK*.
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;    S P R E A D I N G    A C T I V A T I O N    ;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;;; The key concept defined in this file is SPREADING ACTIVATION.
;;;; The file defines the function SPREAD that implements this mechanism.
;;;; It also provides functions for monitoring the FOCUS (i.e. the most
;;;; active agent).
;;;;

;;;;;;;;;;;  SPREADING ACTIVATION MECHANISM
;;;;
;;;; Each agent is connected to a few peers via weighted links (see DUAL/ARCHIT/
;;;; CONNECT.LSP). If the agent is active, it sends activation to its neighbors.
;;;; Thus, there is a constant exchange of activation.
;;;; DUAL's specification (subsections 3.2.2.4 and 3.4.2 in "DUAL Report #1")
;;;; postulates that this exchange is continuous (like the activation itself).
;;;; In this program, this continuous exchange is approximated by a sequence
;;;; of 'cycles'. During each cycle, the activation levels of all agents are
;;;; updated in parallel. This leads to inclusion and exclusion of agents
;;;; from the working memory, as activation levels go up or down the threshold.
;;;; The function SPREAD implements one such connectionist cycle.
;;;;
;;;;;;;;;;;  FOCUS
;;;;
;;;; By definition, the most active non-special DUAL-agent at any given moment
;;;; is called to be 'in the focus' at that moment.
;;;; Each working memory (see DUAL/ARCHIT/WORK_MEM.LSP) has its own focus.
;;;; The primary working memory of the system is kept in the global variable
;;;; *WM* (defined in DUAL/ARCHIT/WORK_MEM.LSP).
;;;; This file defines another global variable -- *FOCUS* -- which keeps the
;;;; focus of *WM*. In turn,*PREVIOUS-FOCUS* keeps the previous value of *FOCUS*
;;;;

;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: spread, update-focus, focus-changed,
;;          *focus*, *previous-focus*
;;
;; Note *WM* and *SPECIAL-WMs* defined in DUAL/ARCHIT/WORK_MEM.LSP.
;; Note also *WM-threshold* and *EXPECTED-WM-SIZE* defined in DUAL/DEFS.LSP.
;;

;; SPREAD (&optional primary-WM special-WMs)  -->  new-primary-WM-size
;;
;;   A function that implements the spreading activation mechanism.
;;   One call to SPREAD represents one 'connectionist cycle': it involves
;;   updating agents' act. levels and modifying working memories accordingly.
;;
;;   PRIMARY-WM should be a working memory. Usually, it is a (or rather _the_)
;;     primary working memory (see DUAL/ARCHIT/WORK_MEM.LSP). When not supplied,
;;     PRIMARY-WM defaults to the value of the global variable *WM*.
;;   SPECIAL-WMs should be a (possibly empty) list of special working memories.
;;     When not supplied, SPECIAL-WMs defaults to the value of the global
;;     variable *SPECIAL-WMs*.
;;
;;   SPREAD returns the new size of PRIMARY-WM (see WM-SIZE).
;;
;;   All working memories passed to SPREAD may be desructively modified as
;;   agents are added to or removed from them. In particular, this is apt
;;   to happen to PRIMARY-WM.
;;
;;   SPREAD depends on the global parameter *WM-THRESHOLD*, defined in DEFS.LSP.
;;   SPREAD also depends heavily on the functions defined in DUAL/ARCHIT/
;;     CONNECT.LSP and DUAL/ARCHIT/WORK_MEM.LSP.
;;
;;   Let us introduce the following terms:
;;     -- 'the primary WM' denotes the value of PRIMARY-WM;
;;     -- 'all special WMs' denotes the union of SPECIAL-WMs;
;;     -- 'all WMs' means 'the primary WM and all special WMs'.
;;   Using this notation, the 'connectionist cycle' performed by SPREAD may
;;   be described roughly as follows:
;;     1. Call ACTIVATE-ALL-SPECIAL-WM on all special WMs.      ; fresh act.
;;     2. Call ACTIVATE-NEIGHBORS  on all agents in all WMs.    ; residual act.
;;     3. Call UPDATE-ACTIVATION on all agents in all special WMs.
;;     4. Call UPDATE-ACTIVATION on all agents in the primary WM.
;;     5. Call UPDATE-ACTIVATION on all agents that do not participate
;;        in any WM (i.e. their NOW-IN-WM is NIL) but have received some
;;        activation at steps 1. and/or 2.
;;     6. With each AGENT from steps 4. and 5. perform the appropriate
;;        action from the table below.
;;        The first column in the table indicates whether AGENT's updated
;;        activation level is below (<) or above (>=) *WM-THRESHOLD*.
;;        The second column in the table indicates AGENT's current working
;;        memory membership (as determined by NOW-IN-WM):
;;
;;        thresh now-in-WM  |  action taken                  ; comment
;;       -------------------+--------------------------------------------------
;;          <    primary-WM |  (REMOVE-FROM-WM AGENT T)      ; become inactive
;;          >=     NIL      |  (ADD-TO-WM AGENT PRIMARY-WM)  ; become active
;;          >=   primary-WM |  NIL                           ; remain active
;;          <      NIL      |  NIL                           ; remain inactive
;;
;;        Addition and removal of agents is done via calls to ADD-TO-WM and
;;        REMOVE-FROM-WM, respectively. The exact order of additions and
;;        removals is implementation-dependent.
;;        Note that no agents are ever added to or removed from any special WM.
;;     7. Now the contents of all WMs have been updated according to DUAL's
;;        specification. Call UPDATE-FOCUS to update all foci.
;;     8. Call PREPARE-TO-RECEIVE-ACTIVATION  on all agents in all WMs in
;;        order to set the stage for the next invocation of SPREAD.
;;        Meanwhile, other modules of the program may send activation to
;;        the input zones of the agents and this additional activation will
;;        be taken into account during the next connectionist cycle. (SPREAD
;;        does not reset the input zones at the beginning of the cycle; the
;;        resetting is done at the end.) This allows for 'self-activation'
;;        triggered by the symbolic aspect of the architecture.
;;     9. Finally, return the new size of PRIMARY-WM by calling WM-SIZE.
;;
;;
;;   Note that the special working memories listed in SPECIAL-WMs serve as
;;   activation sources. They activate all their agents by a weighted amount
;;   of their WM-ACT-SOURCE (see DUAL/ARCHIT/WORK_MEM.LSP). This is done by
;;   the generic function ACTIVATE-ALL-SPECIAL-WM.
;;
;;   During one call to SPREAD, all agents from all WMs receive the following
;;   messages in order:
;;     1. PREPARE-TO-RECEIVE-ACTIVATION
;;     2. ACTIVATE-NEIGHBORS  and/or           ; once
;;        RECEIVE-ACTIVATION                   ; several times, including zero
;;     3. UPDATE-ACTIVATION
;;   Finally, an agent may receive an ADD-TO-WM or REMOVE-FROM-WM message.
;;
;;   The function SPREAD is one of the two pillars of DUAL's macrolevel.
;;   The other one is the function RUN-ALL-PROCESSES from DUAL/ARCHIT/AGENDA.LSP
;;   The former implements the 'connectionist macrocycle', the latter -- the
;;   'symbolic macrocycle'.  Usually the two macrocycles alternate and one
;;   such couple is referred to as 'DUAL macrocycle'.  It is implemented
;;   by the function MAIN-DUAL-CYCLE defined in DUAL/TOPLEVEL.LSP.
;;

;; *FOCUS*
;; *PREVIOUS-FOCUS*
;;
;;   Global variables related to the focus of the primary working memory and
;;   maintained by the function UPDATE-FOCUS.
;;
;;   *FOCUS* keeps the focus of working memory stored in *WM*.
;;   *PREVIOUS-FOCUS* keeps the previous value of *FOCUS*.
;;
;;   The initial value of *FOCUS* and *PREVIOUS-FOCUS* is :FOCUS-UNDEFINED.
;;   The only standard means of changing these variables is by calling the
;;   function UPDATE-FOCUS (with no arguments or with first argument EQ to
;;   *WM*, see below).
;;

;; UPDATE-FOCUS (&optional primary-WM special-WMs)  -->  new-focus
;;
;;   A function that applies UPDATE-WM-FOCUS to PRIMARY-WM and to all (special)
;;   working memories listed in SPECIAL-WMS. When necessary, the function
;;   also updates the global variables *FOCUS* and *UPDATE-FOCUS*.
;;
;;   UPDATE-FOCUS is automatically called at the end of each spreading
;;   activation cycle (see SPREAD).
;;   In turn, UPDATE-FOCUS may call the generic function FOCUS-CHANGED.
;;
;;   PRIMARY-WM should be a working memory. Usually, it is a (or rather _the_)
;;     primary working memory (see DUAL/ARCHIT/WORK_MEM.LSP). When not supplied,
;;     PRIMARY-WM defaults to the value of the global variable *WM*.
;;   SPECIAL-WMs should be a (possibly empty) list of (special) wrkng memories.
;;     When not supplied, SPECIAL-WMs defaults to the value of the global
;;     variable *SPECIAL-WMs*.
;;   Typically, both arguments to UPDATE-FOCUS are ommited, like in SPREAD.
;;
;;   UPDATE-FOCUS returns the new focus of PRIMARY-WM (see WM-FOCUS).
;;
;;   UPDATE-FOCUS works according to the following algorithm:
;;     1. The function UPDATE-WM-FOCUS (defined in DUAL/ARCHIT/WORK_MEM.LSP) is
;;        applied to all members of SPECIAL-WMs, if any.
;;     2. The function UPDATE-WM-FOCUS is applied to PRIMARY-WM and the
;;        NEW-FOCUS resulting from this call is returned as the value
;;        of UPDATE-FOCUS.
;;     3. If PRIVATE-WM is equal (EQ) to *WM*, the following additional
;;        steps are taken:
;;          -- The (old) value of *FOCUS* is transferred to *PREVIOUS-FOCUS*
;;          -- *FOCUS* is set (by SETQ) to NEW-FOCUS (obtained at step 2.)
;;          -- If the new value of *FOCUS* is different from its old value
;;             (now stored in *PREVIOUS-FOCUS*), UPDATE-FOCUS calls the
;;             generic function FOCUS-CHANGED.
;;        If PRIVATE-WM is different from *WM* (i.e. the default has been
;;        explicitly overridden), no changes are made to any global variables.
;;
;;   Thus, provided UPDATE-FOCUS has been called with no arguments or with
;;   first argument equal to *WM*, it is always true that:
;;     (eq *focus* (WM-focus *WM*))  -->  T
;;

;; FOCUS-CHANGED (new-focus old-focus &optional WM)  -->  unspecified
;;
;;   A generic function that is called when there is a change in *FOCUS*.
;;
;;   NEW-FOCUS and OLD-FOCUS should be working-memory agents.
;;   WM should be a working memory. Usually, it is a (or rather _the_)
;;     primary working memory (see DUAL/ARCHIT/WORK_MEM.LSP). When not supplied,
;;     PRIMARY-WM defaults to the value of the global variable *WM*.
;;   The value returned by this function is not specified; it depends on the
;;   additional methods defined by DUAL-based models.  The default methods
;;   always return NIL.
;;
;;   The implementation of the architecture provides the following default
;;   methods.  DUAL-based models may provide additional methods (primary
;;   or auxiliary).
;;     1. (WM-agent WM-agent) -- returns NIL without doing anything;
;;     2. (WM-agent (eql :focus-undefined)) -- returns NIL
;;     3. ((eql :focus-undefined) WM-agent) -- returns NIL
;;     4. (T T) -- signals a continuable error.

;;;;;; Generic function(s) pertaining to the external protocol

(defgeneric focus-changed (new-focus old-focus &optional WM)
  (:documentation "Called when the focus of WM has changed." ))

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************


;;;;;;;;;;   SPREADING  ACTIVATION  MECHANISM    ;;;;;;;;;;;;;;;;;;;;

;; The just-so-activated agents are kept in *WM-CANDIDATES*.
;; If they pass the threshold, they are added to the primary WM.

(defvar *WM-candidates* (make-array
                              (ceiling *expected-WM-size* 5)
                              :element-type '(or null base-agent)
                              :fill-pointer 0
                              :adjustable t     ; set to NIL if speed is more
                                                ; important than robustness
                              :initial-element nil)
  "A set of just-so-activated agents struggling to pass *WM-THRESHOLD*" )

(defun do-all-candidates (fun)
  "Funcalls FUN on all agents currently in *WM-CANDIDATES*."
  (declare (type function fun))
  (dotimes (k (fill-pointer *WM-candidates*) nil)
    (funcall fun (aref *WM-candidates* k)) ))

(declaim (inline register-candidate undo-register-candidate))

(defun register-candidate (agent)
  (vector-push-extend agent *WM-candidates*)
  (setf (DUAL-core::agent-WM agent) '*WM-candidates*) )

(defun undo-register-candidate (agent)
  (setf (DUAL-core::agent-WM agent) nil) )


;;;;;;;;;;;  SPREAD is implemented by several separate functions:

(declaim (inline spread-emit-act      ; see p. 230 in CLtL2
                 spread-update-act
                 spread-prepare-for-next-cycle) )

;;;; Activation exchange (sending & receiving).

(defun spread-emit-act (primary-WM special-WMs)
  "First stage of the SPREAD process."
  (declare (type primary-WM primary-WM)
           (type list special-WMs) )      ; a list of special working memories
  (mapc #'activate-all-special-WM special-WMs)   ; step 1.
  (unwind-protect
    (progn                                       ; step 2.
      (do-all-wm (agent primary-WM)
        (activate-neighbors agent))
      (dolist (special-WM special-WMs)
        (do-all-wm (agent special-WM)
          (activate-neighbors agent))) )
    (do-all-candidates #'undo-register-candidate) )
  nil)

(defmethod receive-activation :after ((agent DUAL-core::WM-agent) amount)
  (declare (ignore amount))
  (when (null (now-in-WM agent))     ; new candidate?
    (register-candidate agent) ))

;; Guard against failures to UNDO-REGISTER-CANDIDATE
(defmethod  add-to-WM  :before ((agent DUAL-core::WM-agent)
                                (WM (eql '*WM-candidates*))
                                &optional weight )
  (declare (ignore weight))
  (undo-register-candidate agent) )

(defmethod  DUAL-core::remove-from-WM-aux :before ((agent DUAL-core::WM-agent)
                                                   (WM (eql '*WM-candidates*)) )
  (undo-register-candidate agent) )


;;;; Update activation, check threshold, and add to or remove from WM if needed.

(defun spread-update-act (primary-WM special-WMs)
  "Second stage of the SPREAD process."
  (declare (type primary-WM primary-WM)
           (type list special-WMs) )    ; a list of special working memories
  (dolist (WM special-WMs)                   ; step 3.
    (do-all-WM (agent WM)
      (update-activation agent) ))
  (do*-all-WM (agent primary-WM)             ; steps 4. and 6. ; Note: DO*
    (when (< (update-activation agent) *WM-threshold*)
      (remove-from-WM agent T) ))
  (do-all-candidates #'(lambda (agent)       ; steps 5. and 6.
    (if (>= (update-activation agent) *WM-threshold*)
        (add-to-WM agent primary-WM)                 ; "I've done it!"
        (prepare-to-receive-activation agent)) ))    ; "May be next time..."
  nil )


;;;; Prepare for the next cycle

(defun spread-prepare-for-next-cycle (primary-WM special-WMs)
  "Last stage of SPREAD -- prepare for the next connectionist cycle."
  (declare (type primary-WM primary-WM)
           (type list special-WMs) )    ; a list of special working memories
  (do-all-WM (agent primary-WM)
    (prepare-to-receive-activation agent) )      ; step 8a.
  (dolist (WM special-WMs)
    (do-all-WM (agent WM)
      (prepare-to-receive-activation agent) ))   ; step 8b.
  (setf (fill-pointer *WM-candidates*) 0)
  nil )


;;;;;;;;  Put it all together.
;; SPREAD is the only function advertised in the external protocol.

(defun spread (&optional (primary-WM  *WM*)
                         (special-WMs *special-WMs*) )
  "Does one cycle of spreading activation. Returns the new primary-WM size."
  (spread-emit-act   primary-WM special-WMs)              ; steps 1. - 2.
  (spread-update-act primary-WM special-WMs)              ; steps 3. - 6.
  (update-focus primary-WM special-WMs)                   ; step  7.
  (spread-prepare-for-next-cycle primary-WM special-WMs)  ; step  8.
  (WM-size primary-WM)                                    ; step  9.
)

(declaim (notinline spread-emit-act         ; see p. 230 in CLtL2
                    spread-update-act
                    spread-prepare-for-next-cycle) )


;;;;;;;;;;;     FOCUS   MANAGEMENT        ;;;;;;;;;;;;;;;

(defvar *focus* :focus-undefined
  "The most active (ordinary) DUAL agent." )

(defvar *previous-focus* :focus-undefined
  "The *FOCUS* of the previous connectionist cycle." )


(defun update-focus (&optional (primary-WM *WM*)
                               (special-WMs *special-WMs*) )
  "Update the WM-FOCUS of all WMs; update *FOCUS* and *PREVIOUS-FOCUS*."
  (mapc #'update-WM-focus special-WMs)          ; update special WMs
  (let ((focus (update-WM-focus primary-WM)))   ; update primary WM
    (when (eq primary-WM *WM*)   ; global variables relevant?
      (setq *previous-focus* *focus*)
      (setq *focus* focus)
      (when (not (eq *focus* *previous-focus*))
        (focus-changed *focus* *previous-focus* *WM*)) )  ; notify who cares
    focus ))


(defmethod  focus-changed  ((new-focus WM-agent)
                            (old-focus WM-agent)
                            &optional (WM  *WM*) )
  (declare (ignore new-focus old-focus WM))
  nil )    ; dummy method to be shadowed by DUAL-based models

(defmethod  focus-changed  ((new-focus (eql :focus-undefined))
                            (old-focus WM-agent)
                            &optional (WM  *WM*) )
  (declare (ignore new-focus old-focus WM))
  nil )    ; dummy method to be shadowed by DUAL-based models

(defmethod  focus-changed  ((new-focus WM-agent)
                            (old-focus (eql :focus-undefined))
                            &optional (WM  *WM*) )
  (declare (ignore new-focus old-focus WM))
  nil )    ; dummy method to be shadowed by DUAL-based models

(defmethod  focus-changed  ((x t) (y t) &optional (WM *WM*) )
  (declare (ignore WM))
  (cerror "Return NIL and continue."
          "FOCUS-CHANGED: ~S or ~S is not a working-memory agent."
          x y ))


(defmethod remove-all-wm :after ((WM primary-WM) &optional (reset-p t))
  (declare (ignore reset-p))
  (when (eq WM *WM*)
    (setq *previous-focus* *focus*)
    (setq *focus* (WM-focus WM)) ))  ; should be :FOCUS-UNDEFINED


;;;;;;;  End of file DUAL/ARCHIT/SPREAD.LSP
