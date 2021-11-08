;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-

;;; FILE:       DUAL/archit/tests/basic.lsp
;;; VERSION:    1.1
;;; PURPOSE:    Test cases for functions defined in ARCHIT/BASIC.LSP
;;; DEPENDS-ON: utils.lsp  archit/basic.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    18-2-97 [1.0]
;;; UPDATED:    9-11-97 [1.1]  The 'official release'
;;; UPDATED:    ...
;;;;;;
;;; TO DO:      Test GENNAME and AGENT-FLAG.


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;   T E S T   C A S E S   FOR   ARCHIT/BASIC.LSP    ;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  This file checks whether the functions defined in ARCHIT/BASIC.LSP
;;;  adhere to their external protocol. (The protocol itself is documented
;;;  in the same file -- ARCHIT.BASIC.LSP .)  Undocumented or internal
;;;  features are not checked.
;;;
;;;  The checks are performed mostly by ASSERT clauses. The name of the
;;;  function that is being tested by a particular ASSERT clause is typed
;;;  in capital letters.
;;;
;;;  If everything is OK, this file should load without errors.
;;;  (Compilation warnings are allowed and even likely to occur.)


(use-package "DUAL-CORE")
(use-package "DUAL-INTERFACE")

(format t "~%;; Testing functions defined in ARCHIT/BASIC.LSP...")


;; Clear the global register of all agents to avoid name collisions.
(assert (null (REMOVE-ALL-AGENTS))  ()
        "REMOVE-ALL-AGENTS should always succeed and return NIL." )

(assert (causes-an-error-p (MAKE-BASE-AGENT 3)) )
(assert (causes-an-error-p (MAKE-BASE-AGENT "agent1")) )
(assert (causes-an-error-p (MAKE-BASE-AGENT nil)) )

(assert (MAKE-BASE-AGENT 'foo) )
(assert (causes-an-error-p (MAKE-BASE-AGENT 'foo))  ()
        "MAKE-BASE-AGENT should signal an error on name collisions.")

(assert (FIND-AGENT 'foo) )
(assert #$foo )
(assert (eq (FIND-AGENT 'foo) #$foo) )

(assert (AGENTP #$foo) )
(assert (not (AGENTP 'foo)) )
(assert (not (AGENTP nil)) )

(assert (causes-an-error-p (agent-name    3)) )
(assert (causes-an-error-p (agent-name    nil)) )
(assert (causes-an-error-p (agent-comment 3)) )
(assert (causes-an-error-p (agent-comment nil)) )
(assert (causes-an-error-p (agent-comment 'foo)) )

(assert (causes-an-error-p (setf (agent-name 3) 'baz)) )
(assert (causes-an-error-p (setf (agent-name nil) 'baz)) )
(assert (causes-an-error-p (setf (agent-comment   3) "baz")) )
(assert (causes-an-error-p (setf (agent-comment nil) "baz")) )
(assert (causes-an-error-p (setf (agent-comment 'foo) "baz")) )

(assert (eq 'foo (AGENT-NAME #$foo)) )
(assert (null (AGENT-COMMENT #$foo)) ()
        "When no comment, AGENT-COMMENT should default to NIL.")

(assert (MAKE-BASE-AGENT 'foo-with-comment :comment "With comment.") )
(assert (eq 'foo-with-comment (AGENT-NAME #$foo-with-comment)) )
(assert (string= "With comment." (AGENT-COMMENT #$foo-with-comment)) )
(assert (progn
          (setf (AGENT-COMMENT #$foo-with-comment) "New comment.")
          (string= "New comment." (AGENT-COMMENT #$foo-with-comment))) )

(assert (let ((agent (MAKE-BASE-AGENT 'bar)))
           (eq agent (FIND-AGENT 'bar)))  ()
        "FIND-AGENT should retrieve the same (EQ) agent.")

(assert (causes-an-error-p
          (setf (AGENT-NAME #$bar) 'new-bar))  )
(assert (eq 'bar (AGENT-NAME #$bar)) )

(assert (null (DO-ALL-AGENTS (ag)))  ()
        "DO-ALL-AGENTS should always succeed and return NIL." )

(assert (eq t (REMOVE-AGENT #$bar))  ()
        "Apllying REMOVE-AGENT on an existing agent shoult return T." )
(assert (null (FIND-AGENT 'bar))  ()
        "Removed agents should be invisible to FIND-AGENT." )
(assert (null (REMOVE-AGENT #$bar))  ()
        "Applying REMOVE-AGENT on a non-existing agent should return NIL." )

(assert (progn
          (REMOVE-AGENT #$foo-with-comment)
          (MAKE-BASE-AGENT 'foo-with-comment)         ; No COMMENT this time
          (null (agent-comment #$foo-with-comment)))  )

(assert (causes-an-error-p (REMOVE-AGENT "string argument")) )
(assert (causes-an-error-p (REMOVE-AGENT 'symbol-argument )) )

(REMOVE-ALL-AGENTS)                      ; Massacre
(let ((flag (DO-ALL-AGENTS (ag nil)      ; Return NIL if all agents are removed.
              (declare (ignore ag))
              (return t)) ))             ; Return T if there are survivors.
  (assert (null flag) ()                 ; There shouldn't be.
  "DO-ALL-AGENTS should find no agents after a call to REMOVE-ALL-AGENTS." ))

(let ((orig-list  '(foo1  foo2 foo3 foo4 foo5 foo6 foo7 foo8 foo9 foo0))
      (new-list  nil))
  (mapc #'MAKE-BASE-AGENT orig-list)
  (DO-ALL-AGENTS (ag)
    (push (agent-name ag) new-list))
  (assert  (and (subsetp orig-list new-list)
                (subsetp new-list orig-list)) ()
  "DO-ALL-AGENTS  and  (mapc #'MAKE-BASE-AGENT name-list) seem out of tune.") )

;; Clean up
(assert (null (REMOVE-ALL-AGENTS))  ()
        "REMOVE-ALL-AGENTS should always succeed and return NIL." )


(format t "~%;; Finished testing DUAL/ARCHIT/BASIC.LSP~%")
