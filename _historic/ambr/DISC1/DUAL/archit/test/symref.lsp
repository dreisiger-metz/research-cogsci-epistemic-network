;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-

;;; FILE:       DUAL/archit/tests/symref.lsp
;;; VERSION:    1.1
;;; PURPOSE:    Test cases for functions defined in ARCHIT/SYMREF.LSP
;;; DEPENDS-ON: DUAL/utils.lsp  DUAL/archit/basic.lsp, DUAL/archit/symref.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    16-3-97 [1.0]
;;; UPDATED:    9-11-97 [1.1]  The 'official release'
;;; UPDATED:    ...


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;   T E S T   C A S E S   FOR   ARCHIT/SYMREF.LSP    ;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  This file checks whether the functions defined in ARCHIT/SYMREF.LSP
;;;  adhere to their external protocol. (The protocol itself is documented
;;;  in the same file -- ARCHIT/SYMREF.LSP .)  Undocumented or internal
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

(format t "~%;; Testing functions defined in ARCHIT/SYMREF.LSP...")

;;;; Preparing the ground -- making a few symrefs ...
(remove-all-agents)
(make-base-agent 'ag1)
(make-base-agent 'ag2)

(defparameter sr-ag1     (MAKE-SYMREF #$ag1))
(defparameter sr-ag1.sl1 (MAKE-SYMREF #$ag1 :slot1))
(defparameter sr-ag1.sl2 (MAKE-SYMREF #$ag1 'slot2))
(defparameter sr-ag2     (MAKE-SYMREF #$ag2))
(defparameter sr-ag2.sl1 (MAKE-SYMREF #$ag2 :slot1))


;;;; Checking the requirement that simple symrefs are in fact agents

(assert (agentp sr-ag1) ()
        "Simple symrefs should in fact be agents.")
(assert (eq #$ag1 sr-ag1) ()
        "Simple symrefs should in fact be agents.")


;;;; Checking the data-abstraction functions

(assert (eq #$ag1 (SYMREF-AGENT sr-ag1)) )
(assert (eq #$ag2 (SYMREF-AGENT sr-ag2)) )
(assert (eq #$ag1 (SYMREF-AGENT sr-ag1.sl1)) )
(assert (eq #$ag1 (SYMREF-AGENT sr-ag1.sl2)) )

(assert (eq nil    (SYMREF-SLOT sr-ag1)) )
(assert (eq :slot1 (SYMREF-SLOT sr-ag1.sl1)) )
(assert (eq 'slot2 (SYMREF-SLOT sr-ag1.sl2)) )


;;;; Checking the error-signaling

(assert (causes-an-error-p (MAKE-SYMREF 'non-agent)) )
(assert (causes-an-error-p (MAKE-SYMREF 'non-agent :slot1)) )
(assert (causes-an-error-p (MAKE-SYMREF #$ag1 3)) )

(assert (causes-an-error-p (SYMREF-AGENT 'non-symref)) )
(assert (causes-an-error-p (SYMREF-AGENT (cons #$ag1 3))) )

(assert (causes-an-error-p (SYMREF-SLOT 'non-symref)) )
(assert (causes-an-error-p (SYMREF-SLOT (cons #$ag1 3))) )

(assert (causes-an-error-p (SIMPLIFY-SYMREF 'non-symref)) )
(assert (causes-an-error-p (SIMPLIFY-SYMREF (cons #$ag1 3))) )


;;;; Checking the predicates

(assert (SIMPLE-SYMREF-P #$ag1) )           ; bare agent
(assert (SIMPLE-SYMREF-P sr-ag1) )          ; simple symref, which is the same
(assert (not (SIMPLE-SYMREF-P sr-ag1.sl1)) )
(assert (not (SIMPLE-SYMREF-P nil)) )
(assert (not (SIMPLE-SYMREF-P 'ag1)) )      ; a symbol, not an agent

(assert (EXTENDED-SYMREF-P sr-ag1.sl1) )
(assert (not (EXTENDED-SYMREF-P #$ag1)) )   ; bare agent
(assert (not (EXTENDED-SYMREF-P sr-ag1)) )  ; simple symref, which is the same
(assert (not (EXTENDED-SYMREF-P nil)) )
(assert (not (EXTENDED-SYMREF-P 'ag1)) )    ; a symbol, not an agent

(assert (SYMREF-P #$ag1) )                  ; bare agent
(assert (SYMREF-P sr-ag1) )                 ; simple symref, which is the same
(assert (SYMREF-P sr-ag1.sl1) )             ; extended symref
(assert (not (SYMREF-P nil)) )
(assert (not (SYMREF-P 'ag1)) )             ; a symbol, not an agent


(assert (SYMREF-MATCH sr-ag1 (make-symref #$ag1)) )
(assert (SYMREF-MATCH sr-ag1.sl1 (make-symref #$ag1 :slot1)) )
(assert (SYMREF-MATCH sr-ag1 sr-ag1.sl1) )
(assert (SYMREF-MATCH sr-ag1.sl1 sr-ag1.sl2) )

(assert (not (SYMREF-MATCH sr-ag1 sr-ag2)) )
(assert (not (SYMREF-MATCH sr-ag1.sl1 sr-ag2.sl1)) )
(assert (not (SYMREF-MATCH sr-ag1 sr-ag2.sl1)) )


(assert (SYMREF-EQUAL sr-ag1 (make-symref #$ag1)) )
(assert (SYMREF-EQUAL sr-ag1.sl1 (make-symref #$ag1 :slot1)) )

(assert (not (SYMREF-EQUAL sr-ag1 sr-ag2)) )
(assert (not (SYMREF-EQUAL sr-ag1 sr-ag1.sl1)) )
(assert (not (SYMREF-EQUAL sr-ag1.sl1 sr-ag1.sl2)) )
(assert (not (SYMREF-EQUAL sr-ag1.sl1 sr-ag2.sl1)) )
(assert (not (SYMREF-EQUAL sr-ag1 sr-ag2.sl1)) )


;;;; Checking SYMPLIFY-SYMREF and SYMREF->STRING

(assert (symref-equal sr-ag1 (SIMPLIFY-SYMREF sr-ag1.sl1)) )
(assert (symref-equal sr-ag1 (SIMPLIFY-SYMREF sr-ag1)) )
(assert (symref-equal sr-ag1 (SIMPLIFY-SYMREF sr-ag1.sl2)) )

(assert (string-equal "ag1" (SYMREF->STRING sr-ag1)) )
(assert (string-equal "ag1.slot1" (SYMREF->STRING sr-ag1.sl1)) )   ; :slot1
(assert (string-equal "ag1.slot2" (SYMREF->STRING sr-ag1.sl2)) )   ; 'slot2
(assert (string-equal (format nil "~A" 3.14) (SYMREF->STRING 3.14)) )


;;;; Cleaning up
(makunbound 'sr-ag1)
(makunbound 'sr-ag1.sl1)
(makunbound 'sr-ag1.sl2)
(makunbound 'sr-ag2)
(makunbound 'sr-ag2.sl1)

(remove-all-agents)

(format t "~%;; Finished testing DUAL/ARCHIT/SYMREF.LSP~%")
