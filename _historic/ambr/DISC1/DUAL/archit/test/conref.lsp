;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-

;;; FILE:       DUAL/archit/tests/conref.lsp
;;; VERSION:    1.1
;;; PURPOSE:    Test cases for functions defined in ARCHIT/CONREF.LSP
;;; DEPENDS-ON: utils.lsp, archit/basic.lsp, archit/symref.lsp,archit/conref.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    11-3-97 [1.0]
;;; UPDATED:    9-11-97 [1.1]  The 'official release'
;;; UPDATED:    ...
;;;;;;;
;;; TO DO:      Update checks for ADJOIN-CONREF


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;   T E S T   C A S E S   FOR   ARCHIT/CONREF.LSP    ;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  This file checks whether the functions defined in ARCHIT/CONREF.LSP
;;;  adhere to their external protocol. (The protocol itself is documented
;;;  in the same file -- ARCHIT/CONREF.LSP .)  Undocumented or internal
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

(format t "~%;; Testing functions defined in ARCHIT/CONREF.LSP...")

;;;; Preparing the ground -- making a few conrefs ...
(remove-all-agents)
(make-base-agent 'x)
(make-base-agent 'y)
(make-base-agent 'z)

(defparameter x-1 (make-conref #$x 1.0))
(defparameter x-0 (make-conref #$x 0.0))
(defparameter x.sl1-0 (make-conref (make-symref #$x 'slot1) 0.0))
(defparameter y-0 (make-conref #$y 0.0))


;;;; Checking the data-abstraction functions

(let ((cr (MAKE-CONREF #$x 1.0) ))
  (assert (CONREF-P cr) )
  (assert (eq #$x (CONREF-REFERENCE cr)) )
  (assert (=  1.0 (CONREF-WEIGHT    cr)) )
  (assert (CONREF-EQUAL cr x-1) ))

(let ((cr  (MAKE-CONREF (make-symref #$x) 0.0) )
      (cr1 (MAKE-CONREF (make-symref #$x 'slot1) 0.0)) )
  (assert (CONREF-P cr) )
  (assert (CONREF-P cr1) )
  (assert (eq #$x (CONREF-REFERENCE cr)) )
  (assert (symref-match #$x (CONREF-REFERENCE cr)) )
  (assert (symref-equal #$x (CONREF-REFERENCE cr)) )
  (assert (symref-match #$x (CONREF-REFERENCE cr1)) )
  (assert (not (symref-equal #$x (CONREF-REFERENCE cr1))) )
  (assert (symref-equal (make-symref #$x 'slot1) (CONREF-REFERENCE cr1)) )
  (assert (=  0.0 (CONREF-WEIGHT    cr)) )
  (assert (=  0.0 (CONREF-WEIGHT    cr1)) )
  (assert (CONREF-EQUAL cr x-0) )
  (assert (CONREF-EQUAL cr1 x.sl1-0) )
)

(assert (not (CONREF-P nil)) )
(assert (not (CONREF-P 'agent-name)) )
(assert (not (CONREF-P       (make-symref #$x))) )
(assert (not (CONREF-P (list (make-symref #$x)))) )
(assert (not (CONREF-P (list (make-symref #$x) 0.0))) )
(assert (not (CONREF-P       (make-symref #$x 'slot1))) )
(assert (not (CONREF-P (list (make-symref #$x 'slot1)))) )
(assert (not (CONREF-P (list (make-symref #$x 'slot1) 0.0))) )

(assert (CONREF-MATCH x-1 x-0) )
(assert (CONREF-MATCH x-0 x.sl1-0) )
(assert (not (CONREF-MATCH x-0 y-0)) )


;;;; Checking ADJOIN-CONREF (on simple symrefs which are equivalent to agents)

;; (adjoin-conref '(x.1) nil)              -->  ((x.1))
(assert (equal (adjoin-conref x-1 nil)
               (list x-1)) )

;; (adjoin-conref '(x.1) '((y.0)))         -->  ((x.1) (y.0))
(assert (equal (adjoin-conref x-1 (list y-0))
               (list x-1 y-0)) )

;; (adjoin-conref '(x.1) '((x.0) (y.0)))   -->  ((x.1) (y.0))
(assert (equal (adjoin-conref x-1 (list x-0 y-0))
               (list x-1 y-0)) )

;; (adjoin-conref '(x.1) '((y.0) (x.1)))   -->  ((y.0) (x.1))
(assert (equal (adjoin-conref x-1 (list y-0 x-1))
               (list y-0 x-1)) )

;; (adjoin-conref '(x.1) '((y.0) (x.0)))   -->  ((y.0) (x.1))
(assert (equal (adjoin-conref x-1 (list y-0 x-0))
               (list y-0 x-1)) )

;; (adjoin-conref '(x.1) '((y.0) (x.0)) :priority :old)   --> ((y.0) (x.0))
(assert (equal (adjoin-conref x-1 (list y-0 x-0) :priority :old)
               (list y-0 x-0)) )

;; (adjoin-conref '(x.1) '((y.0) (x.0)) :priority :new)   --> ((y.0) (x.1))
(assert (equal (adjoin-conref x-1 (list y-0 x-0) :priority :new)
               (list y-0 x-1)) )

;; (adjoin-conref '(x.1) '((y.0) (x.0)) :priority #'max)  --> ((y.0) (x.1))
(assert (equal (adjoin-conref x-1 (list y-0 x-0) :priority #'max)
               (list y-0 x-1)) )

;;  :PRIORITY should be :NEW, :OLD, or a function.
(assert (causes-an-error-p (adjoin-conref x-1 (list x-0) :priority 'foo)) )


;;;; Checking ADJOIN-CONREF (on extended symrefs)

;;     (adjoin-conref (x.sl1 . 1) ((x.sl1 . 0)) :priority #'max)  -->
;;                                                          -->  ((x.sl1 . 1))
;;     (adjoin-conref (x.sl1 . 1) ((x.sl2 . 0)) :priority #'max)  -->
;;                                                          -->  ((x . 1))
(let ((x.sl1-1 (make-conref (make-symref #$x 'slot1) 1.0))
      (x.sl2-1 (make-conref (make-symref #$x 'slot2) 1.0)) )
  (assert (equal (adjoin-conref x.sl1-1 (list x.sl1-0) :priority #'max)
                 (list x.sl1-1)) )
  (assert (equal (adjoin-conref x.sl2-1 (list x.sl1-0) :priority #'max)
                 (list x-1)) )
  (assert (equal (adjoin-conref x-1 (list x.sl1-0) :priority #'max)
                 (list x-1)) )
)


;;;; Checking NORMALIZE-WEIGHTS

;; (normalize-weights '((x . 1.0) (y . 0.6) (z -0.4)))  -->
;;                                   -->  ((x . 0.5) (y . 0.3) (z . -0.2))
(assert (equal (NORMALIZE-WEIGHTS (list (make-conref #$x  1.0)
                                        (make-conref #$y  0.6)
                                        (make-conref #$z -0.4) ))
               (list (make-conref #$x  0.5)
                     (make-conref #$y  0.3)
                     (make-conref #$z -0.2)) ))

;; (normalize-weights '((x . 0.8) (y . -0.7)) 0.75)  -->
;;                                   -->  ((x . 0.4) (y . -0.35))
(assert (equal (NORMALIZE-WEIGHTS (list (make-conref #$x  0.8)
                                        (make-conref #$y -0.7) )
                                  0.75)
               (list (make-conref #$x  0.4)
                     (make-conref #$y -0.35)) ))

;; (normalize-weights nil)  -->  nil
(assert (null (NORMALIZE-WEIGHTS nil)) )

;; (normalize-weights '((x . 0.0) (y . 0.0)))  -->  error
(assert (causes-an-error-p (NORMALIZE-WEIGHTS (list x-0 y-0))) )

;; (normalize-weights some-list -1.0)          -->  error
(assert (causes-an-error-p (NORMALIZE-WEIGHTS (list x-1) -1.0)) )


;;;; Cleaning up
(makunbound 'x-1)
(makunbound 'x-0)
(makunbound 'x.sl1-0)
(makunbound 'y-0)

(remove-all-agents)

(format t "~%;; Finished testing DUAL/ARCHIT/CONREF.LSP~%")
