;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: AMBR -*-

;;; FILE:       AMBR/kb/sit_code.lsp
;;; VERSION:    3.0.0
;;; PURPOSE:    Numerical codes for the situations used in experiments.
;;; PROGRAMMER: Alexander Alexandrov Petrov  (apetrov@cogs.nbu.acad.bg)
;;; DEPENDS-ON: ambr/kb/episodic/*.lsp
;;; CREATED:    04-07-98
;;; UPDATED:    14-08-98 [3.0.0] The 'official release'
;;; UPDATED:    ...


     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;;;    S I T U A T I O N    C O D E S    ;;;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "AMBR")


(defparameter *genKB-prefix*
              "D:\\dual\\ambr\\kb\\genkb\\genkb" )


;; ASSUMPTION: Each coalition-name coincides with the agent-name of the head.

(defun situation-code (thing)
  (declare (type (or symbol coalition AMBR-agent) thing))
  (etypecase thing
    (AMBR-agent  (if (and (agent-type thing :instance)
                          (agent-situation thing))        ; affiliated ?
                     (situation-code (agent-name (agent-situation thing)))
                     0))
    (coalition   (situation-code (agent-name (coalition-head thing))))
    (symbol      ; coalition name
      (case thing
        (sit-WTP  01 )
        (sit-BF   02 )
        (sit-GP   03 )
        (sit-IHC  04 )
        (sit-MTF  05 )
        (sit-ICF  06 )
        (sit-BPF  07 )
        (sit-FDO  08 )
        (sit-STC  09 )
        (sit-SFF  10 )
        (sit-ERW  11 )
        (sit-GWB  12 )

        (sit-WB1  51 )
        (sit-WG1  52 )
        (sit-HM1  53 )
        (sit-HM2  54 )
        (sit-CM1  55 )
        (sit-CM2  56 )
        (sit-ICC  57 )
        (sit-SF1  58 )
        (sit-SF2  59 )
        (sit-EHW  60 )

        (t        00 ) ))))


;;;;;;;;;;  End of file  AMBR/KB/SIT_CODE.LSP
