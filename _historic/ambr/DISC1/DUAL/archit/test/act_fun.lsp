;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: common-lisp-user -*-

;;; FILE:       DUAL/archit/tests/basic.lsp
;;; VERSION:    1.1
;;; PURPOSE:    Test cases for functions defined in ARCHIT/ACT_FUN.LSP
;;; DEPENDS-ON: utils.lsp  archit/act_fun.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    9-03-97 [1.0]
;;; UPDATED:    9-11-97 [1.1]  The 'official release'
;;; UPDATED:    ...


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;   T E S T   C A S E S   FOR   ARCHIT/ACT_FUN.LSP    ;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  This file checks whether the functions defined in ARCHIT/ACT_FUN.LSP
;;;  adhere to their external protocol. (The protocol itself is documented
;;;  in the same file -- ARCHIT.BASIC.LSP .)  Undocumented or internal
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

(format t "~%;; Testing functions defined in ARCHIT/ACT_FUN.LSP...")


;; Testing the clip functions:

(assert (= 0.0 (CLIP-0-1 -1.0)) )
(assert (= 0.0 (CLIP-0-1  0.0)) )
(assert (= 0.5 (CLIP-0-1  0.5)) )
(assert (= 1.0 (CLIP-0-1  1.0)) )
(assert (= 1.0 (CLIP-0-1  2.0)) )

(assert (= -1.0 (CLIP-1-1 -2.0)) )
(assert (= -1.0 (CLIP-1-1 -1.0)) )
(assert (=  0.0 (CLIP-1-1  0.0)) )
(assert (=  0.5 (CLIP-1-1  0.5)) )
(assert (=  1.0 (CLIP-1-1  1.0)) )
(assert (=  1.0 (CLIP-1-1  2.0)) )

(let ((too-little (- *min-act* 1.0))
      (in-between (/ (+ *min-act* *max-act*) 2))
      (too-much   (+ *max-act* 1.0)) )
  (assert (= *min-act*  (CLIP-MIN-MAX too-little)) )
  (assert (= *min-act*  (CLIP-MIN-MAX *min-act* )) )
  (assert (= in-between (CLIP-MIN-MAX in-between)) )
  (assert (= *max-act*  (CLIP-MIN-MAX *max-act* )) )
  (assert (= *max-act*  (CLIP-MIN-MAX too-much  )) )
)


;; Testing the threshold functions:

(let ((below (- *threshold* 1.0))
      (above (+ *threshold* 1.0)))
  (assert (= 0.0         (THRESHOLD below)) )
  (assert (= *threshold* (THRESHOLD *threshold*)) )
  (assert (= above       (THRESHOLD above)) )
)

(assert (= 0.0 (THRESHOLD-0 -1.0)) )
(assert (= 0.0 (THRESHOLD-0  0.0)) )
(assert (= 1.0 (THRESHOLD-0  1.0)) )

(assert (causes-an-error-p (THRESHOLD-THETA 0.0 -1.0))  ()
  "THRESHOLD-THETA should signal an error if its second argument is negative.")
(assert (= 0.0 (THRESHOLD-THETA -1.0 1.0)) )
(assert (= 0.0 (THRESHOLD-THETA  0.0 1.0)) )
(assert (= 1.0 (THRESHOLD-THETA  1.0 1.0)) )
(assert (= 2.0 (THRESHOLD-THETA  2.0 1.0)) )


;; Testing the Anderson activation function:

(assert (zerop (Anderson-ACT-FUN 0.0 0.0)) )
(assert (= (Anderson-ACT-FUN 0.0 0.7)
           (*  0.7  *Anderson-excit-rate*  *time-slice*)) )
(assert (= (Anderson-ACT-FUN 0.0 -0.43)
           (*  -0.43  *Anderson-excit-rate*  *time-slice*)) )

(let ((asymptote (find-asymptote #'Anderson-ACT-FUN 1.0 0.0) ))
  (assert (=~= 0.0 asymptote) ()
    "Anderson-ACT-FUN (1.0 0.0) ----> ~S which is not 0.0." asymptote) )

(let ((asymptote (find-asymptote #'Anderson-ACT-FUN -1.0 0.0) ))
  (assert (=~= 0.0 asymptote) ()
    "Anderson-ACT-FUN (-1.0 0.0) ----> ~S which is not 0.0." asymptote) )

(let ((expected-value (/ *Anderson-excit-rate* *Anderson-decay-rate*))
      (asymptote+1 (find-asymptote #'Anderson-ACT-FUN  1.0 1.0))
      (asymptote-0 (find-asymptote #'Anderson-ACT-FUN  0.0 1.0))
      (asymptote-1 (find-asymptote #'Anderson-ACT-FUN -1.0 1.0)) )
  (assert (=~= asymptote+1 expected-value) ()
      "Anderson-ACT-FUN (1.0 1.0) ----> ~S which is not p=~6,3F."
      asymptote+1 expected-value)
  (assert (=~= asymptote-0 expected-value) ()
      "Anderson-ACT-FUN (0.0 1.0) ----> ~S which is not p=~6,3F."
      asymptote-0 expected-value)
  (assert (=~= asymptote-1 expected-value) ()
      "Anderson-ACT-FUN (-1.0 1.0) ----> ~S which is not p=~6,3F."
      asymptote-1 expected-value)
)


;; Testing Rum&McCl-TWO-TAIL-ACT-FUN

(assert (zerop (Rum&McCl-TWO-TAIL-ACT-FUN 0.0 0.0)) )
(assert (= (Rum&McCl-TWO-TAIL-ACT-FUN 0.0 0.7)
           (*  0.7  *Rum&McCl-excit-rate*  *time-slice* *max-act*)) )
(assert (= (Rum&McCl-TWO-TAIL-ACT-FUN 0.0 -0.4)
           (*  0.4  *Rum&McCl-excit-rate*  *time-slice* *min-act*)) )

(let ((asymptote (find-asymptote #'Rum&McCl-TWO-TAIL-ACT-FUN 1.0 0.0) ))
  (assert (=~= 0.0 asymptote) ()
    "Rum&McCl-TWO-TAIL-ACT-FUN (1.0 0.0) ----> ~S which is not 0.0." asymptote) )

(let ((asymptote (find-asymptote #'Rum&McCl-TWO-TAIL-ACT-FUN -1.0 0.0) ))
  (assert (=~= 0.0 asymptote) ()
    "Rum&McCl-TWO-TAIL-ACT-FUN (-1.0 0.0) ----> ~S which is not 0.0." asymptote) )

(let ((expected-value (/ (* *Rum&McCl-excit-rate* *max-act*)
                         (+ *Rum&McCl-excit-rate* *Rum&McCl-decay-rate*)))
      (asymptote+1 (find-asymptote #'Rum&McCl-TWO-TAIL-ACT-FUN  1.0 1.0))
      (asymptote-0 (find-asymptote #'Rum&McCl-TWO-TAIL-ACT-FUN  0.0 1.0))
      (asymptote-1 (find-asymptote #'Rum&McCl-TWO-TAIL-ACT-FUN -1.0 1.0)) )
  (assert (=~= asymptote+1 expected-value) ()
     "Rum&McCl-TWO-TAIL-ACT-FUN (1.0 1.0) ---> ~S which is not pM/(1+p)=~6,3F."
      asymptote+1 expected-value)
  (assert (=~= asymptote-0 expected-value) ()
     "Rum&McCl-TWO-TAIL-ACT-FUN (0.0 1.0) ---> ~S which is not pM/(1+p)=~6,3F."
      asymptote-0 expected-value)
  (assert (=~= asymptote-1 expected-value) ()
     "Rum&McCl-TWO-TAIL-ACT-FUN (-1.0 1.0) ---> ~S which is not pM/(1+p)=~6,3F."
      asymptote-1 expected-value)
)

(let ((expected-value (/ (* *Rum&McCl-excit-rate* *min-act*)
                         (+ *Rum&McCl-decay-rate* *Rum&McCl-excit-rate*)))
      (asymptote+1 (find-asymptote #'Rum&McCl-TWO-TAIL-ACT-FUN  1.0 -1.0))
      (asymptote-0 (find-asymptote #'Rum&McCl-TWO-TAIL-ACT-FUN  0.0 -1.0))
      (asymptote-1 (find-asymptote #'Rum&McCl-TWO-TAIL-ACT-FUN -1.0 -1.0)) )
  (assert (=~= asymptote+1 expected-value) ()
      "Rum&McCl-TWO-TAIL-ACT-FUN (1.0 -1.0) ----> ~S which is not ~6,3F."
      asymptote+1 expected-value)
  (assert (=~= asymptote-0 expected-value) ()
      "Rum&McCl-TWO-TAIL-ACT-FUN (0.0 -1.0) ----> ~S which is not ~6,3F."
      asymptote-0 expected-value)
  (assert (=~= asymptote-1 expected-value) ()
      "Rum&McCl-TWO-TAIL-ACT-FUN (-1.0 -1.0) ----> ~S which is not ~6,3F."
      asymptote-1 expected-value)
)


;; Testing Rum&McCl-ONE-TAIL-ACT-FUN:

(assert (zerop (Rum&McCl-ONE-TAIL-ACT-FUN 0.0 0.0)) )
(assert (= (Rum&McCl-ONE-TAIL-ACT-FUN 0.0 100.0)
           (*  100.0  *Rum&McCl-excit-rate*  *time-slice* *max-act*)) )

(let ((asymptote (find-asymptote #'Rum&McCl-ONE-TAIL-ACT-FUN 1.0 0.0) ))
  (assert (=~= 0.0 asymptote) ()
    "Rum&McCl-ONE-TAIL-ACT-FUN (1.0 0.0) ----> ~S which is not 0.0." asymptote) )

(let ((expected-value (/ (* *Rum&McCl-excit-rate* *max-act*)
                         (+ *Rum&McCl-excit-rate* *Rum&McCl-decay-rate*)))
      (asymptote+1 (find-asymptote #'Rum&McCl-ONE-TAIL-ACT-FUN  1.0 1.0))
      (asymptote-0 (find-asymptote #'Rum&McCl-ONE-TAIL-ACT-FUN  0.0 1.0))
      (asymptote-1 (find-asymptote #'Rum&McCl-ONE-TAIL-ACT-FUN -1.0 1.0)) )
  (assert (=~= asymptote+1 expected-value) ()
     "Rum&McCl-ONE-TAIL-ACT-FUN (1.0 1.0) ----> ~S which is not pM/(1+p)=~6,3F."
     asymptote+1 expected-value)
  (assert (=~= asymptote-0 expected-value) ()
     "Rum&McCl-ONE-TAIL-ACT-FUN (0.0 1.0) ----> ~S which is not pM/(1+p)=~6,3F."
     asymptote-0 expected-value)
  (assert (=~= asymptote-1 expected-value) ()
     "Rum&McCl-ONE-TAIL-ACT-FUN (-1.0 1.0) ----> ~S which is not pM/(1+p)=~6,3F."
     asymptote-1 expected-value)
)

(assert (causes-an-error-p (Rum&McCl-ONE-TAIL-ACT-FUN 0.0 -1.0))  ()
  "Rum&McCl-ONE-TAIL-ACT-FUN should signal an error ~
                                    if its second argument is negative.")


;; Testing DUAL-ACT-FUN by comparing it with Rum&McCl-ONE-TAIL-ACT-FUN:

(let ((*Rum&McCl-excit-rate* *excit-rate*)        ; equalizing parameters
      (*Rum&McCl-decay-rate* *decay-rate*))
  (do ((n 0.0 (+ n 0.1)))
      ((> n 2.0) nil)
    (assert (=~= (DUAL-ACT-FUN 0.0 n)
                 (threshold (Rum&McCl-one-tail-act-fun *threshold* n))) )
    (do ((a *threshold* (+ a 0.1)))
        ((> a *max-act*) nil)
      (assert (=~= (DUAL-ACT-FUN a n)
                 (threshold (Rum&McCl-one-tail-act-fun a n))) )
    )))

(assert (causes-an-error-p (DUAL-ACT-FUN 0.0 -1.0))  ()
  "DUAL-ACT-FUN should signal an error if its second argument is negative.")


;; No tests provided for DUAL-ACT-FUN.

;; No tests provided for Grossberg-ACT-FUN.

;; No tests provided for Kokinov-ACT-FUN.


(format t "~%;; Finished testing DUAL/ARCHIT/ACT_FUN.LSP~%")
