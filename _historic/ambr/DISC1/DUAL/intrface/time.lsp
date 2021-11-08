;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: DUAL-interface -*-

;;; FILE:       DUAL/intrface/time.lsp
;;; VERSION:    1.1.2    ; see DUAL/VERSION.LSP
;;; PURPOSE:    Interface to alarm clocks and metronomes.
;;; DEPENDS-ON: DUAL/archit/time.lsp, DUAL/archit/metronom.lsp
;;; PROGRAMMER: Alexander Alexandrov Petrov   (apetrov@cogs.nbu.acad.bg)
;;; CREATED:    04-04-98 [1.1.2]
;;; UPDATED:    24-07-98 The 'official release'
;;; UPDATED:    ...


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;;;;;;;;     T I M E - R E L A T E D   I N T E R F A C E     ;;;;;;;;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package "DUAL-INTERFACE")


;;  ******************************************************
;;  External protocol            *************************
;;  ******************************************************
;;
;; SYMBOLS:  show-clocks,
;;           show-metronomes
;;

;; SHOW-CLOCKS (&key show-name-p show-message-p show-comment-p
;;                                      filter  sort-p  stream)  -->  number
;;
;;   A function that shows (a subset of) the alarm clocks that are active
;;   at the moment.
;;
;;   SHOW-NAME-P, SHOW-MESSAGE-P, and SHOW-COMMENT-P are flags that control
;;    whether the respective alarm clock fields are printed.
;;    Clock owners and time-remaining are always printed.
;;    SHOW-NAME-P and SHOW-COMMENT-P default to T; SHOW-MESSAGE-P -- to NIL.
;;   FILTER should be either NIL or a function that takes one argument -- an
;;    alarm clock.  When it is bound to NIL (the default) all clocks are shown.
;;    Otherwise only those that satisfy FILTER are shown and they are counted
;;    for the final result.
;;   SORT-P is a flag showing whether the clocks should be shown in ascending
;;    order with respect to the time remaining to ring.  Defaults to T.
;;   STREAM should be an output stream.  It defaults to *STANDARD-OUTPUT*.
;;   The function returns the number of clocks that have been shown.
;;
;;   The function is non-destructive.  It does not alter the set of clocks
;;   maintained by DUAL-core functions even when SORT-P is T.
;;   The exact format of the text printed to STREAM is implementation dependent.


;; SHOW-METRONOMES ...
;;
;;   ...

;;
;;;;;;;;;;;  End of the external protocol  ;;;;;;;;;;;;


;;  ******************************************************
;;  Implementation               *************************
;;  ******************************************************

(defun show-clocks (&key (show-name-p T) (show-comment-p T) show-message-p
                         (stream *standard-output*)
                         filter  (sort-p T) )
  "Prints (a subset of) the alarm clocks that are active at the moment."
  (declare (type boolean show-name-p show-comment-p show-message-p sort-p)
           (type (or null function) filter) )
  (let ((clocks-to-show nil))
    (dotimes (k (number-of-active-alarm-clocks))
      (let ((clock (aref DUAL-core::*alarm-clocks* k)))
        (declare (type alarm-clock clock))
        (when (or (null filter)
                  (funcall filter clock) )
          (push clock clocks-to-show))))
    (if sort-p
        (setq clocks-to-show (sort clocks-to-show #'<= 
                                   :key #'alarm-clock-time-remaining))
        (setq clocks-to-show (nreverse  clocks-to-show))) ; use original order
    (dolist (clock clocks-to-show)
      (format stream "~&;; ~15S ~5,2F  ~@[~(~10A~)~] ~@[~20S~] ~@[~A~]~%"
                     ;;   owner  time     name        message  comment
              (alarm-clock-owner clock)
              (alarm-clock-time-remaining clock)
              (and show-name-p  (alarm-clock-name clock))
              (and show-message-p  (alarm-clock-message clock))
              (and show-comment-p (alarm-clock-comment clock)) ))
    (length clocks-to-show) ))


(defun show-metronomes (&key (show-name-p T)    (show-subscribers-p NIL)
                             (show-comment-p T) (stream *standard-output*)
                             (filter NIL)       (sort-p T) )
  "Prints the metronomes that are active at the moment."
  (declare (type boolean show-name-p show-comment-p show-subscribers-p sort-p)
           (type (or null function) filter) )
  (let ((metronomes-to-show *metronomes*))
    (when filter
      (setq metronomes-to-show
            (remove-if (complement filter) *metronomes*)) )
    (when sort-p
      (setq metronomes-to-show
            (sort (copy-list metronomes-to-show)       ; non-destructive sort
                   #'<=  :key #'metronome-time-remaining)) )
    (dolist (metronome metronomes-to-show)
      (format stream
              "~&;; ~5,2F ~5,2F  ~(~15A  ~@[~15A~]~)  ~@[~S~]  ~@[~A~]~%"
              ;;   period time.r    event   name       subscr  comment
              (metronome-period metronome)
              (metronome-time-remaining metronome)
              (metronome-event metronome)
              (and show-name-p  (metronome-name metronome))
              (and show-subscribers-p  (metronome-subscribers metronome))
              (and show-comment-p (metronome-comment metronome)) ))
    (length metronomes-to-show) ))


;;;;;;;  End of file DUAL/INTRFACE/TIME.LSP
