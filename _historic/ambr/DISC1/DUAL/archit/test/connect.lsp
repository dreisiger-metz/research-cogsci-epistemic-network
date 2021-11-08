;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-

;;; FILE:       DUAL/archit/tests/connect.lsp
;;; VERSION:    1.1
;;; PURPOSE:    Test cases for functions defined in ARCHIT/CONREF.LSP
;;; DEPENDS-ON: utils.lsp  archit/basic.lsp, archit/connect.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov
;;; CREATED:    14-3-97 [1.0]
;;; UPDATED:    9-11-97 [1.1]  The 'official release'
;;; UPDATED:    ...


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;   T E S T   C A S E S   FOR   ARCHIT/CONNECT.LSP    ;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  This file checks whether the functions defined in ARCHIT/CONNECT.LSP
;;;  adhere to their external protocol. (The protocol itself is documented
;;;  in the same file -- ARCHIT/CONNECT.LSP .)  Undocumented or internal
;;;  features are not checked.
;;;
;;;  The checks are performed mostly by ASSERT clauses. The name of the
;;;  function that is being tested by a particular ASSERT clause is typed
;;;  in capital letters.
;;;
;;;  If everything is OK, this file should load without errors.
;;;  (Compilation warnings are allowed although not likely to occur.)


(use-package "DUAL-CORE")
(use-package "DUAL-INTERFACE")

(format t "~%;; Testing functions defined in ARCHIT/CONNECT.LSP...")

;; Clear the global register of all agents to avoid name collisions.
(remove-all-agents)


;; Testing MAKE-CONN-AGENT

(assert (causes-an-error-p (MAKE-CONN-AGENT 3)) )
(assert (causes-an-error-p (MAKE-CONN-AGENT "agent1")) )
(assert (causes-an-error-p (MAKE-CONN-AGENT nil)) )

(assert (MAKE-CONN-AGENT 'ag1) )
(assert (FIND-AGENT 'ag1) )
(assert (causes-an-error-p (MAKE-CONN-AGENT 'ag1))  ()
        "MAKE-CONN-AGENT should signal an error on name collisions.")

(assert (AGENTP (find-agent 'ag1)) )

(assert (MAKE-CONN-AGENT 'ag-with-comment :comment "With comment.") )
(assert (string= "With comment." (AGENT-COMMENT #$ag-with-comment)) )
(assert (null (AGENT-NEIGHBORS #$ag-with-comment)) )
(remove-agent #$ag-with-comment)

(assert (MAKE-CONN-AGENT 'ag-with-neighbors
                         :neighbors (list (make-conref #$ag1 0.7))) )
(assert (equal (AGENT-NEIGHBORS #$ag-with-neighbors)
               (list (make-conref #$ag1 0.7))) )
(remove-agent #$ag-with-neighbors)


;; Testing the type-checking methods

(assert (causes-an-error-p (AGENT-ACTIVATION 3)) )
(assert (causes-an-error-p (AGENT-ACTIVATION nil)) )
(assert (causes-an-error-p (AGENT-ACTIVATION 'foo)) )
(assert (causes-an-error-p (setf (AGENT-ACTIVATION   3) "baz")) )
(assert (causes-an-error-p (setf (AGENT-ACTIVATION nil) "baz")) )
(assert (causes-an-error-p (setf (AGENT-ACTIVATION 'foo) "baz")) )

(assert (causes-an-error-p (ACT 3)) )
(assert (causes-an-error-p (ACT nil)) )
(assert (causes-an-error-p (ACT 'foo)) )
(assert (causes-an-error-p (setf (ACT   3) "baz")) )
(assert (causes-an-error-p (setf (ACT nil) "baz")) )
(assert (causes-an-error-p (setf (ACT 'foo) "baz")) )

(assert (causes-an-error-p (AGENT-VISIBLE-P 3)) )
(assert (causes-an-error-p (AGENT-VISIBLE-P nil)) )
(assert (causes-an-error-p (AGENT-VISIBLE-P 'foo)) )

(assert (causes-an-error-p (AGENT-NEIGHBORS 3)) )
(assert (causes-an-error-p (AGENT-NEIGHBORS nil)) )
(assert (causes-an-error-p (AGENT-NEIGHBORS 'foo)) )
(assert (causes-an-error-p (SETF (AGENT-NEIGHBORS 'ag1) nil)) )

(assert (causes-an-error-p (AGENT-OUTPUT 3)) )
(assert (causes-an-error-p (AGENT-OUTPUT nil)) )
(assert (causes-an-error-p (AGENT-OUTPUT 'foo)) )

(assert (causes-an-error-p (PREPARE-TO-RECEIVE-ACTIVATION 3)) )
(assert (causes-an-error-p (PREPARE-TO-RECEIVE-ACTIVATION nil)) )
(assert (causes-an-error-p (PREPARE-TO-RECEIVE-ACTIVATION 'foo)) )

(assert (causes-an-error-p (RECEIVE-ACTIVATION 3 1.0)) )
(assert (causes-an-error-p (RECEIVE-ACTIVATION nil 1.0)) )
(assert (causes-an-error-p (RECEIVE-ACTIVATION 'foo 1.0)) )

(assert (causes-an-error-p (ACTIVATE-NEIGHBORS 3)) )
(assert (causes-an-error-p (ACTIVATE-NEIGHBORS nil)) )
(assert (causes-an-error-p (ACTIVATE-NEIGHBORS 'foo)) )

(assert (causes-an-error-p (AGENT-NEW-ACTIVATION 3 1.0)) )
(assert (causes-an-error-p (AGENT-NEW-ACTIVATION nil 1.0)) )
(assert (causes-an-error-p (AGENT-NEW-ACTIVATION 'foo 1.0)) )

(assert (causes-an-error-p (UPDATE-ACTIVATION 3)) )
(assert (causes-an-error-p (UPDATE-ACTIVATION nil)) )
(assert (causes-an-error-p (UPDATE-ACTIVATION 'foo)) )

(assert (causes-an-error-p (NORMALIZE-NEIGHBORS 3)) )
(assert (causes-an-error-p (NORMALIZE-NEIGHBORS nil)) )
(assert (causes-an-error-p (NORMALIZE-NEIGHBORS 'foo)) )

(assert (causes-an-error-p (ADD-NEIGHBORS 3 nil)) )
(assert (causes-an-error-p (ADD-NEIGHBORS nil nil)) )
(assert (causes-an-error-p (ADD-NEIGHBORS 'foo nil)) )

(assert (causes-an-error-p (REMOVE-NEIGHBORS 3 nil)) )
(assert (causes-an-error-p (REMOVE-NEIGHBORS nil nil)) )
(assert (causes-an-error-p (REMOVE-NEIGHBORS 'foo nil)) )


;; Testing the accessors

(assert (= 1.0 (setf (AGENT-ACTIVATION #$ag1) 1.0)) )
(assert (= 1.0 (AGENT-ACTIVATION #$ag1)) )
(assert (= 1.0 (ACT #$ag1)) )
(assert (AGENT-VISIBLE-P #$ag1) )

(assert (= (AGENT-OUTPUT #$ag1) (agent-activation #$ag1)) ()
        "At present, AGENT-OUTPUT should always be equal to AGENT-ACTIVATION." )

(assert (= 1.5 (setf (ACT #$ag1) 1.5)) )
(assert (= 1.5 (ACT #$ag1)) )
(assert (= 1.5 (AGENT-ACTIVATION #$ag1)) )

(assert (= (AGENT-OUTPUT #$ag1) (agent-activation #$ag1)) ()
        "At present, AGENT-OUTPUT should always be equal to AGENT-ACTIVATION." )

(assert (endp (AGENT-NEIGHBORS #$ag1)) )


;; Testing the connectionist machinery (within one isolated agent)

(do ((a 0.0 (+ a 0.1)))
    ((> a *max-act*) nil)
  (setf (act #$ag1) a)
  (do ((n 0.0 (+ n 0.1)))
      ((> n 2.0) nil)
     (assert (= (AGENT-NEW-ACTIVATION #$ag1 n)
                (DUAL-act-fun a n)) ()
                "At present, AGENT-NEW-ACTIVATION should be DUAL-ACT-FUN." ) ))

(assert (null (PREPARE-TO-RECEIVE-ACTIVATION #$ag1)) ()
        "PREPARE-TO-RECEIVE-ACTIVATION should always return NIL." )

;; Now, the input zone should be set to 0.0
(let ((new-act (UPDATE-ACTIVATION #$ag1) ))
  (assert (= new-act (AGENT-ACTIVATION #$ag1)) )
)

(setf (act #$ag1) *max-act*)
(assert (= 0.5 (RECEIVE-ACTIVATION #$ag1 0.5)) )
;; Now accumulated input should be 0.5
(assert (= (UPDATE-ACTIVATION #$ag1)
           (DUAL-act-fun *max-act* 0.5)) )

(setf (act #$ag1) *max-act*)
(assert (= -0.2 (RECEIVE-ACTIVATION #$ag1 -0.2)) )
;; Now accumulated input should be 0.3 (= 0.5 - 0.2)
(assert (= (UPDATE-ACTIVATION #$ag1)
           (DUAL-act-fun *max-act* 0.3)) )

(setf (act #$ag1) *max-act*)
(PREPARE-TO-RECEIVE-ACTIVATION #$ag1)
;; Now the input zone should be set back to 0.0
(assert (= (UPDATE-ACTIVATION #$ag1)
           (DUAL-act-fun *max-act* 0.0)) )


;; Testing neighbor-handling functions

(make-conn-agent 'ag2 :neighbors (list (make-conref #$ag1 1.0)))
(make-conn-agent 'ag3 :neighbors (list (make-conref #$ag1 1.0)
                                       (make-conref #$ag2 0.5)) )

(assert (endp (AGENT-NEIGHBORS #$ag1)) )
(assert (equal (AGENT-NEIGHBORS #$ag2) (list (make-conref #$ag1 1.0)) ) )

(assert (equal (ADD-NEIGHBORS #$ag1 (list (make-conref #$ag2 0.7)))
               (list (make-conref #$ag2 0.7))) )

(assert (equal (NORMALIZE-NEIGHBORS #$ag1)
               (list (make-conref #$ag2 1.0))) )
(assert (equal (AGENT-NEIGHBORS #$ag1)
               (list (make-conref #$ag2 1.0))) )

(assert (causes-an-error-p (NORMALIZE-NEIGHBORS #$ag1  0.0)) )
(assert (causes-an-error-p (NORMALIZE-NEIGHBORS #$ag1 -1.0)) )

(assert (endp (REMOVE-NEIGHBORS #$ag1 :all)) )
(assert (endp (AGENT-NEIGHBORS #$ag1)) )

(assert (causes-an-error-p (ADD-NEIGHBORS #$ag1 (list (make-conref #$ag2 0.0))
                                          :normalize t))   ()
        "NORMALIZE-NEIGHBORS should signal an error when the sum of orig. weights is zero." )

(assert (endp (REMOVE-NEIGHBORS #$ag2 :all)) )
(assert (equal (ADD-NEIGHBORS #$ag2 (list (make-conref #$ag1 1.0)) :priority :old)
               (list (make-conref #$ag1 1.0)) ) )
(assert (set-equal (ADD-NEIGHBORS #$ag2 (list (make-conref #$ag3 1.0)))
                   (list (make-conref #$ag1 1.0) (make-conref #$ag3 1.0))) )
(assert (set-equal (ADD-NEIGHBORS #$ag2 (list (make-conref #$ag3 0.5))
                                  :priority :old)
                   (list (make-conref #$ag1 1.0) (make-conref #$ag3 1.0))) )

(assert (equal (REMOVE-NEIGHBORS #$ag2 (list #$ag3))
               (list (make-conref #$ag1 1.0))) )
(assert (set-equal (ADD-NEIGHBORS #$ag2 (list (make-conref #$ag3 0.5))
                                  :priority :old)
                   (list (make-conref #$ag1 1.0) (make-conref #$ag3 0.5))) )
(assert (set-equal (ADD-NEIGHBORS #$ag2 (list (make-conref #$ag2 0.5))
                                  :normalize t)
                   (list (make-conref #$ag1 0.5)
                         (make-conref #$ag2 0.25)
                         (make-conref #$ag3 0.25)) ) )

(assert (set-equal (REMOVE-NEIGHBORS #$ag2 (list #$ag2) :normalize 0.6)
                   (list (make-conref #$ag1 0.4)
                         (make-conref #$ag3 0.2)) ) )


;; Testing activation exchange

(setf (act #$ag1) 1.0)
(setf (act #$ag2) 0.0)
(setf (act #$ag3) 0.0)

(REMOVE-NEIGHBORS #$ag1 :all)
(ADD-NEIGHBORS #$ag1 (list (make-conref #$ag2 1.0)
                           (make-conref #$ag3 0.5)) )

(do-all-agents #'PREPARE-TO-RECEIVE-ACTIVATION)
(assert (null (ACTIVATE-NEIGHBORS #$ag1)) ()
        "ACTIVATE-NEIGHBORS should always return NIL." )

;; Now #$ag2 should have received 1.0 unit of activation and #$ag3 -- 0.5 units
(assert (= (update-activation #$ag2)
           (DUAL-act-fun 0.0 1.0)) )
(assert (= (update-activation #$ag3)
           (DUAL-act-fun 0.0 0.5)) )


;;;; There are many, many things that remained to be tested...


;; Cleaning up:
(remove-all-agents)

(format t "~%;; Finished testing DUAL/ARCHIT/CONNECT.LSP~%")
