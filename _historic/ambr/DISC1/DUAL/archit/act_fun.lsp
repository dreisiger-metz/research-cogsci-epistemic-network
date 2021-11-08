;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-core -*-

;;; FILE:       DUAL/archit/act_fun.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Definitions of various activation and output functions.
;;; DEPENDS-ON: DUAL/start_me.lsp, DUAL/packages.lsp, DUAL/defs.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    24-02-97 [1.0]
;;; UPDATED:    09-11-97 [1.1]
;;; UPDATED:    24-07-98 [1.1.2] The 'official release'
;;; UPDATED:    ...
;;;;;;;
;;; TO_DO:      Define output functions of the form: (ax + b)/(cx + d)
;;;


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;       A C T I V A T I O N   F U N C T I O N S      ;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-CORE")

;;; This file defines and implements several activation functions found in
;;; the literature. There are also some clipping and threshold functions.
;;; See section 4.3.1 in Petrov's (1996) M.Sc. Thesis for more details.


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS: DUAL-act-fun
;;          unoptimized-DUAL-act-fun   prepare-for-optimized-DUAL-act-fun
;;          clip-0-1   clip-1-1     clip-min-max
;;          threshold  threshold-0  threshold-theta
;;          Anderson-act-fun
;;          Rum&McCl-one-tail-act-fun
;;          Rum&McCl-two-tail-act-fun
;;          Grossberg-act-fun
;;          Kokinov-act-fun
;;
;; The functions defined in this file depend on the following constants
;; and parameters defined in DEFS.LSP:
;;   *min-act*
;;   *max-act*
;;   *time-slice*
;;   *threshold*     *Kokinov-threshold*
;;   *excit-rate*    *Rum&McCl-excit-rate*
;;                   *Anderson-excit-rate*
;;                   *Grossberg-excit-rate*
;;                   *Grossberg-inhib-rate*
;;   *decay-rate*    *Rum&McCl-decay-rate*
;;                   *Anderson-decay-rate*
;;                   *Grossberg-decay-rate*
;;                   *Kokinov-decay-rate*
;;

;; CLIP-0-1 (x)      -->  clipped-x
;; CLIP-1-1 (x)      -->  clipped-x
;; CLIP-MIN-MAX (x)  -->  clipped-x
;;
;;   These functions clip a (single-float) number X so that it fits into
;;   some predefined interval. The respective intervals are as follows:
;;     CLIP-0-1 :     [0.0, 1.0]
;;     CLIP-1-1 :     [-1.0, 1.0]
;;     CLIP-MIN-MAX : [*min-act*, *max-act*]       ; Constants from  DEFS.LSP
;;   Note that each change to *MAX-ACT* should be followed by a call
;;      to PREPARE-FOR-OPTIMIZED-DUAL-ACT-FUN (see below).
;;   It is always true that for each X (provided it is a single-float):
;;      (<  0.0      (clip-0-1 x)     1.0)        -->  T
;;      (< -1.0      (clip-1-1 x)     1.0)        -->  T
;;      (< *min-act* (clip-min-max x) *max-act*)  -->  T


;; THRESHOLD       (x)        -->  x  or  0.0
;; THRESHOLD-0     (x)        -->  x  or  0.0
;; THRESHOLD-THETA (x theta)  -->  x  or  0.0
;;
;;   These are three different threshold functions with different thresholds:
;;   THRESHOLD uses the global parameter *THRESHOLD* defined in DEFS.LSP;
;;   THRESHOLD-0 uses a zero threshold;
;;   THRESHOLD-THETA takes the threshold as its second argument.
;;   The argument(s) to these functions must be of type SINGLE-FLOAT.
;;   *THRESHOLD* and the second argument to THRESHOLD-THETA should be >=0.
;;   Note that each change to *THRESHOLD* should be followed by a call
;;      to PREPARE-FOR-OPTIMIZED-DUAL-ACT-FUN (see below).
;;   It is always true that for each X:
;;      (if (minusp x) (= (threshold-0 x) 0.0) t)            -->  T
;;      (if (plusp x)  (= (threshold-0 x)   x) t)            -->  T
;;      (= (threshold-theta x x) x)                          -->  T
;;      (= (threshold-0 x) (threshold-theta x 0.0))          -->  T
;;      (= (threshold x)   (threshold-theta x *threshold*))  -->  T


;; Anderson-ACT-FUN  (old-act net-input)  -->  new-act
;;
;;   The activation function used in ACT* (Anderson, 1983).
;;   It computes the new activation level of a connectionist unit from its
;;   old activation level and net input according to the following
;;   differential equation:
;;     a' = da/dt = - d * a(t)  +  E * net(t)
;;
;;   The new activation level is then computed using the Euler's method:
;;     a(t+h) = a(t) + a'(t) * h
;;
;;   The function depends on three global parameters (defined in DEFS.LSP):
;;     *Anderson-decay-rate*  -- 'd' in the formula above
;;     *Anderson-excit-rate*  -- 'E' in the formula above
;;     *time-slice*           -- discretization step 'h'
;;
;;   OLD-ACT should be of type single-float and be >= 0.
;;   NET-INPUT should be of type single-float and can take any value (+ or -).
;;   The function returns a non-negative single float.


;; Rum&McCl-TWO-TAIL-ACT-FUN  (old-act net-input)  -->  new-act
;;
;;   A generalization of the activation function proposed by Rumelhart &
;;   McClelland in their Interactive Activation Model (1981, 1982).
;;   The present function computes the new activation level of a connectionist
;;   unit from its old activation level and net input according to the
;;   differential equation:
;;     a' = da/dt = - d * a  +  E * net * (max - a)     , when  net >= 0
;;                = - d * a  +  E * net * (a - min)     , when  net <  0
;;
;;   The new activation level is then computed using the Euler's method:
;;     a(t+h) = a(t) + a'(t) * h
;;
;;   The function depends on five global parameters (defined in DEFS.LSP):
;;     *min-act*   *max-act*  -- 'min' and 'max' in the formula above
;;     *Rum&McCl-decay-rate*  -- 'd' in the formula above
;;     *Rum&McCl-excit-rate*  -- 'E' in the formula above
;;     *time-slice*           -- discretization step 'h'
;;
;;   OLD-ACT should be of type single-float and between *min-act* and *max-act*.
;;   NET-INPUT should be of type single-float and can take any value (+ or -).
;;   For very large values of |NET-INPUT|  NEW-ACT can exceed *{max|min}-act*.
;;   The function returns a single float in the range [*min-act*, *max-act*]
;;   (unless |NET-INPUT| is very large).


;; Rum&McCl-ONE-TAIL-ACT-FUN  (old-act net-input)  -->  new-act
;;
;;   A modification of Rum&McCl-TWO-TAIL-ACT-FUN which uses only the positive
;;   branch of the formula. NET-INPUT should be non-negative; MIN is assumed
;;   to be zero.
;;
;;     a' = da/dt = - d * a  +  E * net * (max - a)
;;
;;     a(t+h) = a(t) + a'(t) * h
;;
;;   The function depends on five global parameters (defined in DEFS.LSP):
;;     *max-act*              -- 'max' in the formula above
;;     *Rum&McCl-decay-rate*  -- 'd' in the formula above
;;     *Rum&McCl-excit-rate*  -- 'E' in the formula above
;;     *time-slice*           -- discretization step 'h'
;;
;;   OLD-ACT should be of type single-float and between 0 and *max-act*.
;;   NET-INPUT should be a non-negative single-float.
;;   For very large values of NET-INPUT  NEW-ACT can exceed *max-act*.
;;   The function returns a single float in the range [0, *max-act*]
;;   (unless NET-INPUT is very large).


;; DUAL-ACT-FUN (old-act net-input)              -->  new-act
;; UNOPTIMIZED-DUAL-ACT-FUN (old-act net-input)  -->  new-act
;;
;; PREPARE-FOR-OPTIMIZED-DUAL-ACT-FUN ()  -->  :DONE
;;
;;   The default activation function in DUAL (see section 4.3.1 in DR#1).
;;   DUAL-ACT-FUN can be considered as a modification of Grossberg-ACT-FUN
;;     (or Rum&McCl-ACT-FUN).  The modification is that only the positive
;;     branch is used and that there is a threshold.
;;
;;   DUAL-ACT-FUN and UNOPTIMIZED-ACT-FUN are equivalent as mathematical
;;     functions. That is, whenever the system is 'prepared' for the
;;     optimized version, it is true that for each pair <A, N>:
;;       (= (DUAL-act-fun a n) (unoptimized-DUAL-act-fun a n))
;;
;;   The functions depend on five global parameters (defined in DEFS.LSP):
;;     *max-act*     -- 'max' in the formula above
;;     *decay-rate*  -- 'd' in the formula above
;;     *excit-rate*  -- 'E' in the formula above
;;     *threshold*   -- 'theta' in the formula above
;;     *time-slice*  -- discretization step 'h'
;;
;;   UNOPTIMIZED-DUAL-ACT-FUN is 'safe' in the sense that it checks the
;;     validity of its arguments and always refers to the global parameters.
;;   DUAL-ACT-FUN is an optimized version which uses some pre-computed
;;     constants derived from the parameters.  Therefore, whenever there
;;     is a change (e.g. via SETQ) to one of the five parameters listed above,
;;     the function PREPARE-FOR-OPTIMIZED-DUAL-ACT-FUN should be called to
;;     'prepare' the environment for DUAL-ACT-FUN.
;;
;;   OLD-ACT should be of type single-float and between 0 and *max-act*.
;;   NET-INPUT should be a non-negative single-float.
;;   For very large values of NET-INPUT  NEW-ACT can exceed *max-act*.
;;   The function returns a single float in the range [0, *max-act*]
;;   (unless NET-INPUT is very large).


;; Grossberg-ACT-FUN  (old-act enet inet)  -->  new-act
;;
;;   A generalization of the activation function proposed by Grossberg (1980).
;;   The present function computes the new activation level of a connectionist
;;   unit from its old activation level and net input according to the
;;   differential equation:
;;     a' = da/dt = - d * a  +  E * enet * (max - a) + I * inet * (a - min)
;;
;;   The new activation level is then computed using the Euler's method:
;;     a(t+h) = a(t) + a'(t) * h
;;
;;   The function depends on six global parameters (defined in DEFS.LSP):
;;     *min-act*   *max-act*   -- 'min' and 'max' in the formula above
;;     *Grossberg-decay-rate*  -- 'd' in the formula above
;;     *Grossberg-excit-rate*  -- 'E' in the formula above
;;     *Grossberg-inhib-rate*  -- 'I' in the formula above
;;     *time-slice*            -- discretization step 'h'
;;
;;   OLD-ACT should be of type single-float and between *min-act* and *max-act*.
;;   ENET should be >= 0, INET <=0, and both should be of type single-float.
;;   For large values of |ENET + INET|  NEW-ACT can go out of bounds.
;;   The function returns a single-float in the range [*min-act*, *max-act*]
;;   (unless |ENET+INET| is very large).


;; Kokinov-ACT-FUN  (old-act net-input)  -->  new-act
;;
;;   The activation function proposed by Kokinov in his AMBR model (1994).
;;   It computes the new activation level of a connectionist unit from its
;;   old activation level according to the formulae:
;;     sum     = a*(1 - decay) + net
;;     new-act = 1 - thresh / sum        , if sum >  thresh
;;     new-act = 0                       , if sum <= thresh
;;   These formulae depend on two global parameters (defined in DEFS.LSP):
;;   *Kokinov-decay-rate*, and *Kokinov-threshold*.
;;   OLD-ACT should be a single-float in the interval [0.0, 1.0].
;;   NET-INPUT should be of type single-float and can take any value (+ or -).
;;   The function returns a single float in the range [0,1).

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

;;;; Clip functions
(declaim (inline clip-0-1 clip-1-1 clip-min-max))

(defun  clip-0-1 (x)
  "Clips X so that it fits into [0.0, 1.0]."
  (declare (type single-float x)
           (values single-float))
  (cond ((minusp x) 0.0)
        ((> x 1.0)  1.0)
        (t            x)))

(defun  clip-1-1 (x)
  "Clips X so that it fits into [-1.0, 1.0]."
  (declare (type single-float x)
           (values single-float))
  (cond ((< x -1.0) -1.0)
        ((> x  1.0)  1.0)
        (t             x)))

(defun  clip-min-max (x)
  "Clips X so that it fits between *MAX-ACT* and *MIN-ACT*."
  (declare (type single-float x)
           (values single-float))
  (cond ((< x *min-act*) *min-act*)
        ((> x *max-act*) *max-act*)
        (t               x)))

;;;; Threshold functions
(declaim (inline threshold threshold-0 threshold-theta))

(defun threshold-0 (x)
  "Threshold function with zero threshold."
  (declare (type single-float x)
           (values single-float))
  (if (plusp x)  x  0.0))

(defun threshold (x)
  "Threshold function depending on the global parameter *THRESHOLD*."
  ;; *THRESHOLD* is believed to be >= 0.
  (declare (type single-float x)
           (values single-float))
  (if (>= x *threshold*)  x  0.0))

(defun threshold-theta (x theta)
  "Threshold function with threshold THETA."
  (declare (type single-float x theta)
           (values single-float))
  (unless (>= theta 0.0)
    (error "THRESHOLD-THETA: The threshold THETA = ~S isn't >= 0."  theta ))
  (if (>= x theta)  x  0.0))


;;;;;;;;;;   DUAL activation function   ;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The activation function used in the current version of DUAL is defined
;;  according to the formula given in section 4.3.1 in DR#1:
;;    a(t+h) = a + a' * h
;;    a' := - d * a + E * net * (max - a)
;;
;;  There is an additional complication related to the threshold:
;;    th -> 0 : if A < *threshold*  then A is set to 0.0
;;     0 -> th: if A = 0.0  then NEXT-A is computed as if A was *threshold*
;;  In the latter case, there is a critical input value which is necessary
;;    to exceed the threshold (starting with zero activation). It is given
;;    by the formula:
;;                        d       theta
;;      critical-net :=  --- * -------------
;;                        E     max - theta
;;
;;  The function depends on five global parameters (defined in DEFS.LSP):
;;    *max-act*     -- 'max' in the formula above
;;    *decay-rate*  -- 'd' in the formula above
;;    *excit-rate*  -- 'E' in the formula above
;;    *threshold*   -- 'theta' in the formula above
;;    *time-slice*  -- discretization step 'h'
;;
;;  The discretization step 'h' should be small enough.  Otherwise activation
;;  levels go out of bounds or oscilate.  An especially dangerous situation
;;  occurs when the input to a node is very large and the initial activation
;;  is zero. For example, successive activation levels may change like this:
;;   0.0 -> 6.34 -> 0.0 -> 6.34 -> ...   or
;;   3.0 -> 3.8 -> 2.1 -> 4.5 -> 0.7 -> 5.8 -> 0.0 -> 6.34 -> 0.0 -> 6.34 ->...
;;  As a rule of thumb, D*H should be less than 0.5 (as checked by PREPARE-FOR-
;;  OPTIMIZED-DUAL-ACT-FUN below).

(defun  unoptimized-DUAL-act-fun  (old-act  net-input)
  ;; written for clarity, not for efficiency
  (declare (type   single-float old-act net-input)
           (values single-float))
  #+:DUAL-DEBUG (unless (>= net-input 0.0)
                  (error "DUAL-ACT-FUN: net input ~S isn't >= 0." net-input ))
  (flet ((next (curr-act)
           (+ curr-act (* *time-slice*
                          (- (* *excit-rate*
                                net-input
                                (- *max-act* curr-act))
                             (* *decay-rate* curr-act)))) ))
    (if (< old-act *threshold*)
        (threshold (next *threshold*))
        (threshold (next old-act)) )))


;;;;  Optimized DUAL activation function
;;
;;  Some constant sub-expressions are computed outside the inner loop.
;;
;;  WARNING!!!   PREPARE-FOR-OPTIMIZED-DUAL-ACT-FUN  must be called whenever
;;               there is a change in any of the parameters *MAX-ACT*,
;;               *THRESHOLD*, *EXCIT-RATE*, *DECAY-RATE*, or *TIME-SLICE*.

;;  This portion is suitable for block compilation. In this way the references
;;  to *ONE-MINUS-D*H* etc. are open coded by the CMU compiler.
#+:CMU (declaim (ext:start-block DUAL-act-fun
                                 prepare-for-optimized-DUAL-act-fun
                                 compute-critical-input ))

(defparameter  *one-minus-D*H*  (- 1.0 (* *decay-rate* *time-slice*))
"Should _always_ be equal to one minus *DECAY-RATE* multiplied by *TIME-SLICE*.
 See PREPARE-FOR-OPTIMIZED-DUAL-ACT-FUN."  )
 ;; This is the effective decay parameter from one cycle to the next.

(defparameter  *E*H*  (* *excit-rate* *time-slice*)
"Should _always_ be equal to the product of *EXCIT-RATE* and *TIME-SLICE*.
 See PREPARE-FOR-OPTIMIZED-DUAL-ACT-FUN."  )

(defun  compute-critical-input  ()
  "Which is the minimal input needed to exceed *THRESHOLD* ?"
  (declare (values single-float))
  (*  (/ *decay-rate* *excit-rate*)
      (/ *threshold* (- *max-act* *threshold*))) )

(defparameter  *critical-input*  (compute-critical-input)
"Should _always_ be equal to (d/E) * (theta / (max - theta)).
 See PREPARE-FOR-OPTIMIZED-DUAL-ACT-FUN."  )

(defparameter  |(1-d.h).th|  (* *one-minus-D*H* *threshold*)
  "See PREPARE-FOR-OPTIMIZED-DUAL-ACT-FUN." )

(defparameter  |E.h.(M-th)|  (* *E*H* (- *max-act* *threshold*))
  "See PREPARE-FOR-OPTIMIZED-DUAL-ACT-FUN." )

(declaim (type single-float *one-minus-D*H*  *E*H*          ; help the compiler
                            *critical-input*
                            |(1-d.h).th|  |E.h.(M-th)| ))

(defun  prepare-for-optimized-DUAL-act-fun  ()
  "To be called whenever the activation-related parameters are changed."
  (setq *one-minus-D*H*  (- 1.0 (* *decay-rate* *time-slice*)) )
  (unless (> *one-minus-D*H* 0.5)
    (error "The value of *TIME-SLICE* (and/or *DECAY-RATE*) is too big." ))
  (unless (< *threshold* *max-act*)
    (error "*THRESHOLD* should be less than *MAX-ACT*." ))
  (setq *E*H*            (* *excit-rate* *time-slice*) )
  (setq *critical-input* (compute-critical-input) )
  (setq |(1-d.h).th|     (* *one-minus-D*H* *threshold*) )
  (setq |E.h.(M-th)|     (* *E*H* (- *max-act* *threshold*)) )
  :done )


(defun  DUAL-act-fun  (old-act  net-input)
  "Default activation function of the DUAL-AGENTs."
  (declare (optimize (speed 3) (space 0))
           (type   single-float old-act net-input)
           (values single-float) )
  (cond ((> old-act *threshold*)
            (threshold (+ (* *one-minus-D*H*  old-act)
                          (* *E*H*  net-input  (- *max-act* old-act)))) )
        ((< net-input *critical-input*)
            0.0 )                               ; cannot exceed threshold
        (t  (+ |(1-d.h).th|                     ; exceeds threshold
               (* |E.h.(M-th)| net-input))) ))


#+:CMU (declaim (ext:end-block))   ; end of the block-compilation module

;;; Testing all this mess and profiling whether it is worth the trouble ...
#|
(defun test () 
  (do ((a 0.0 (+ a 0.05)))
      ((> a *max-act*))
    (princ ".")
    (do ((n 0.0 (+ n 0.05)))
        ((> n *max-act*))
      (assert (DUALI:=~= (DUAL-act-fun a n)
                         (unoptimized-DUAL-act-fun a n))) )))

(defun time-unoptimiz (&optional (epochs 1)) 
  (dotimes (k epochs)
    (do ((a 0.0 (+ a 0.05)))
        ((> a *max-act*))
      (do ((n 0.0 (+ n 0.05)))
          ((> n *max-act*))
        (unoptimized-DUAL-act-fun a n)))))
(defun time-optimiz (&optional (epochs 1))
  (declare (inline DUAL-act-fun))
  (dotimes (k epochs)
    (do ((a 0.0 (+ a 0.05)))
        ((> a *max-act*))
      (do ((n 0.0 (+ n 0.05)))
          ((> n *max-act*))
        (DUAL-act-fun a n)))))
;; Experiments (under Allegro CL for Win) show about twofold speedup.
;; (The speedup affects only the connectionist aspect, however, and only
;;  the computation of new activation levels. Most of the 'connectionist'
;;  time is spent by the generic function RECEIVE-ACTIVATION.  Another
;;  big loaf goes for SLOTS->LINKS.  Therefore, the overall gain of the 
;;  optimized DUAL-act-fun is rather small.)
|#

;;;;;;; Other activation functions
;;  Note: written for clarity, not for efficiency.

(defun  Anderson-act-fun  (old-act net-input)
  "a' = - d * a + E * net;  a(t+h) = a(t) + a'(t) * h"
  (declare (type   single-float old-act net-input)
           (values single-float))
  (let ((a-prime (- (* *Anderson-excit-rate*
                       net-input)
                    (* *Anderson-decay-rate*
                       old-act)) ))
    (+ old-act
       (* a-prime *time-slice*)) ))


(defun  Rum&McCl-two-tail-act-fun  (old-act  net-input)
  "a' =  - d * a + E * net * ((net>0)?(max - a):(a - min))"
  (declare (type   single-float old-act net-input)
           (values single-float))
  (let ((a-prime (- (* *Rum&McCl-excit-rate*
                       net-input
                       (if (plusp net-input)
                           (- *max-act* old-act)
                           (- old-act   *min-act*)))
                    (* *Rum&McCl-decay-rate*
                       old-act)) ))
    (+ old-act
       (* a-prime *time-slice*)) ))

(defun  Rum&McCl-one-tail-act-fun  (old-act  net-input)
  "a' =  - d * a + E * net * (max - a)  ; NET-INPUT assumed >= 0."
  (declare (type   single-float old-act net-input)
           (values single-float))
;  (unless (>= net-input 0.0)
;    (error "Rum&McCl-ONE-TAIL-ACT-FUN: net input ~S isn't >= 0." net-input ))
  (let ((a-prime (- (* *Rum&McCl-excit-rate*
                       net-input
                       (- *max-act* old-act))
                    (* *Rum&McCl-decay-rate*
                       old-act)) ))
    (+ old-act
       (* a-prime *time-slice*)) ))


(defun  Grossberg-act-fun  (old-act enet inet)
  "a' =  - d * a + E * enet * (max - a) + I * inet (a - min)."
  (declare (type   single-float old-act enet inet)
           (values single-float))
;  (unless (>= enet 0.0)
;    (error "GROSSBERG-ACT-FUN: excit. net input ~6,3F isn't >= 0." enet ))
;  (unless (<= inet 0.0)
;    (error "GROSSBERG-ACT-FUN: inhib. net input ~6,3F isn't <= 0." inet ))
  (let ((a-prime (+ (* (- *Grossberg-decay-rate*)
                       old-act)
                    (* *Grossberg-excit-rate*
                       enet
                       (- *max-act* old-act))
                    (* *Grossberg-inhib-rate*
                       inet
                       (- old-act *min-act*))) ))
  (+ old-act
     (* a-prime *time-slice*) )))


(defun  Kokinov-act-fun  (old-act net-input)
"new-act = (if (sum>thresh) (1 - thresh / sum) 0),
 where sum = old-act * (1 - decay) + net."
  (declare (type   single-float old-act net-input)
           (values single-float))
  (let ((sum (+ (* old-act (- 1.0 *Kokinov-decay-rate*))
                net-input)))
    (if (> sum *Kokinov-threshold*)
        (- 1.0 (/ *Kokinov-threshold*  sum))
        0.0)))


;;;;;;;  End of file DUAL/ARCHIT/ACT_FUN.LSP
