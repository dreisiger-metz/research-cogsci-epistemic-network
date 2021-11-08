
;; This file defines :INSTANCE links for the
;; purposes of the 'ice cube experiment'.
;; Some links have been adjusted manually but
;; in a way that could have resulted from
;; the usual GENKB generation.
;;
;; See function WRITE-DATA-FILE at the end
;; of the file.
;;
;; Compare with GENKB files in AMBR/KB/GENKB/.

(cl:in-package "AMBR")

(make::operate-on-system "ltm" :compile)


(with-agent  immersion-heater
  :instance  ((*other* 1.000)
              (imm-htr-ihc 0.300) )
)

(with-agent  ice-cube
  :a-link    (t-of-icf-i 0.100)      ; 0.2
  :instance  ((ice-cube-icf 0.600)
              (*other* 0.700) )
)

(with-agent  water
  :instance  ((*other* 0.700)
              (water-bf 0.300)
              (water-ihc 0.300) )
)

(with-agent  glass
  :instance  ((*other* 0.900)
              (glass-gp 0.300)
              (glass-icf 0.100) )
)

(with-agent  cup
  :instance  ((*other* 1.300)
              (cup-ihc 0.100) )
)

(with-agent  saucer
  :instance  ((*other* 1.300)
              ;;; (saucer-IHC 0.200)
             )
)

(with-agent  low-temp
  :instance  ((low-t-icf 0.200)
              (*other* 0.800)
              (low-t-fridge 0.300) )
)

(with-agent  high-temp
  :a-link    (oven-fdo 0.200)
  :instance  ((*other* 0.400)
              (high-t-htsrc 0.300)
              (high-t-wtp 0.300)
              (high-t-ihc 0.200)
              (high-t-fdo 0.100) )
)

(with-agent  temperature-of
  :a-link    ((high-t-ihc 0.200)
              (low-t-mtf 0.200) )
  :instance  ((t-of-mtf-m 0.300)
              (t-of-gp-w 0.300)
              (t-of-fdo-o 0.300)
              (t-of-wtp-w 0.400) )
)

(with-agent  material-glass
  :instance  ((mglass-bottle 0.400)
              (*other* 0.800)
              (mglass-icf 0.100) )
)

(with-agent  material-china
  :instance  ((*other* 0.700)
              (mchina-bpf 0.400)
              (mchina-ihc 0.200) )
)

(with-agent  made-of
  :instance  ((*other* 0.400)
              (made-of-ihc 0.100)
              (cup-md-china 0.400)
              (made-of-icf 0.200)
              (glass-md-gl 0.200) )
)

(with-agent  in
  :instance  ((*other* 0.900)
              (in-ihc-wc 0.300)
              (in-wtp    0.100) )
)

(with-agent  on
  :instance  ((on-fdo 0.100)
              (*other* 0.800)
              (on-ihc 0.200)
              (on-bf 0.200) )
)

(with-agent  fridge
  :instance  ((*other* 0.550)
              (fridge-mtf 0.500)
              (fridge-icf 0.250) )
)

(with-agent  hot-plate
  :instance  ((*other* 1.100)
              (hplate-gp 0.200) )
)



;; >>>>>>>>   unrelated concepts

(with-agent  cupboard
  :instance  (*other* 1.300)
)

(with-agent  wardrobe
  :instance  (*other* 1.300)
)

(with-agent  shelf
  :instance  (*other* 1.300)
)

(with-agent  desk
  :instance  (*other* 1.300)
)

(with-agent  table
  :instance  (*other* 1.300)
)

(with-agent  sofa
  :instance  (*other* 1.300)
)

(with-agent  armchair
  :instance  (*other* 1.300)
)

(with-agent  chair
  :instance  (*other* 1.300)
)

(with-agent  couch
  :instance  (*other* 1.300)
)

(with-agent  bed
  :instance  (*other* 1.300)
)

(with-agent  can-opener
  :instance  (*other* 1.300)
)

(with-agent  torch
  :instance  (*other* 1.300)
)

(with-agent  shovel
  :instance  (*other* 1.300)
)

(with-agent  axe
  :instance  (*other* 1.300)
)

(with-agent  knife
  :instance  (*other* 1.300)
)

(with-agent  boulder
  :instance  (*other* 1.300)
)

(with-agent  pebble
  :instance  (*other* 1.300)
)

(with-agent  fire
  :instance  ((fire-bf 0.400)
              (*other* 0.900) )
)

(with-agent  tv-set
  :instance  (*other* 1.300)
)

(with-agent  lamp
  :instance  (*other* 1.300)
)

(with-agent  dish-washer
  :instance  (*other* 1.300)
)

(with-agent  oven
  :instance  ((*other* 0.800)
              (oven-fdo 0.500) )
)

(with-agent  baking-dish
  :instance  ((dish-fdo 0.400)    ; 0.9
              (*other* 0.900) )   ; 0.4
)

(with-agent  pan
  :instance  (*other* 1.300)
)

(with-agent  saucepan
  :instance  (*other* 1.300)
)

(with-agent  teapot
  :instance  ((tpot-wtp 0.600)
              (*other* 0.700) )
)

(with-agent  lion
  :instance  (*other* 1.300)
)

(with-agent  pig
  :instance  (*other* 1.300)
)

(with-agent  cat
  :instance  (*other* 1.300)
)

(with-agent  dog
  :instance  (*other* 1.300)
)

(with-agent  hen
  :instance  (*other* 1.300)
)

(with-agent  pigeon
  :instance  (*other* 1.300)
)

(with-agent  snowdrop
  :instance  (*other* 1.300)
)

(with-agent  tulip
  :instance  (*other* 1.300)
)

(with-agent  rose
  :instance  (*other* 1.300)
)

(with-agent  palm-tree
  :instance  (*other* 1.300)
)

(with-agent  birch
  :instance  (*other* 1.300)
)

(with-agent  pine
  :instance  (*other* 1.300)
)

(with-agent  oak
  :instance  (*other* 1.300)
)

(with-agent  chicken-soup
  :instance  (*other* 1.300)
)

(with-agent  vegetable-soup
  :instance  (*other* 1.300)
)

(with-agent  onion
  :instance  (*other* 1.300)
)

(with-agent  potato
  :instance  (*other* 1.300)
)

(with-agent  pepper
  :instance  (*other* 1.300)
)

(with-agent  tomato
  :instance  (*other* 1.300)
)

(with-agent  orange
  :instance  (*other* 1.300)
)

(with-agent  peach
  :instance  (*other* 1.300)
)

(with-agent  pear
  :instance  (*other* 1.300)
)

(with-agent  apple
  :instance  (*other* 1.300)
)

(with-agent  cheese
  :instance  (*other* 1.300)
)

(with-agent  egg
  :instance  (*other* 1.300)
)

(with-agent  meat-soup
  :instance  (*other* 1.300)
)

(with-agent  butter
  :instance  ((*other* 1.000)
              (butter-bpf 0.300) )
)

(with-agent  meat
  :instance  (*other* 1.300)
)

(with-agent  bread
  :instance  (*other* 1.300)
)

(with-agent  food
  :instance  (*other* 1.300)
)

(with-agent  cow
  :instance  (*other* 1.300)
)

(with-agent  chicken
  :instance  (*other* 1.300)
)

(with-agent  bag
  :instance  (*other* 1.300)
)

(with-agent  box
  :instance  (*other* 1.300)
)

(with-agent  pot
  :instance  (*other* 1.300)
)

(with-agent  plate
  :instance  (*other* 1.300)
)

(with-agent  bowl
  :instance  ((*other* 1.000)
              (bowl-bf 0.300) )
)

(with-agent  white-wine
  :instance  (*other* 1.300)
)

(with-agent  red-wine
  :instance  (*other* 1.300)
)

(with-agent  wine
  :instance  (*other* 1.300)
)

(with-agent  beer
  :instance  (*other* 1.300)
)

(with-agent  juice
  :instance  (*other* 1.300)
)

(with-agent  coke
  :instance  (*other* 1.300)
)

(with-agent  coffee
  :instance  (*other* 1.300)
)

(with-agent  tea
  :instance  (*other* 1.300)
)

(with-agent  milk
  :instance  (*other* 1.300)
)

(with-agent  bottle
  :instance  (*other* 1.300)
)

(with-agent  sour-taste
  :instance  (*other* 1.300)
)

(with-agent  salt-taste
  :instance  (*other* 1.300)
)

(with-agent  sweet-taste
  :instance  ((*other* 0.800)
              (swt-jc-taste 0.300)
              (swt-fr-taste 0.200) )
)

(with-agent  taste-of
  :instance  ((fruit-is-sweet 0.400)
              (*other* 0.800)
              (juice-is-sweet 0.100) )
)

(with-agent  square-shape
  :instance  (*other* 1.300)
)

(with-agent  rectang-shape
  :instance  ((rectang-fdo 0.400)
              (*other* 0.900) )
)

(with-agent  triang-shape
  :instance  (*other* 1.300)
)

(with-agent  circular-shape
  :instance  (*other* 1.300)
)

(with-agent  irregular-shape
  :instance  (*other* 1.300)
)

(with-agent  polygonal-shape
  :instance  (*other* 1.300)
)

(with-agent  round-shape
  :instance  (*other* 1.300)
)

(with-agent  green
  :instance  (*other* 1.300)
)

(with-agent  red
  :instance  (*other* 1.300)
)

(with-agent  black
  :instance  ((*other* 0.800)
              (black-wtp 0.500) )
)

(with-agent  white
  :instance  (*other* 1.300)
)

(with-agent  medium-temp
  :instance  (*other* 1.300)
)

(with-agent  shape-of
  :instance  ((*other* 1.100)
              (shape-of-bpf 0.200) )
)

(with-agent  color-of
  :instance  ((color-of-wtp 0.700)
              (*other* 0.600) )
)

(with-agent  is-dissipated
  :instance  ((*other* 1.000)
              (is-dissip-gp 0.300) )
)

(with-agent  is-burnt-out
  :instance  (*other* 1.300)
)

(with-agent  is-broken
  :instance  ((*other* 1.100)
              (is-broken-gp 0.200) )
)

(with-agent  burns-out
  :instance  (*other* 1.300)
)

(with-agent  breaks
  :instance  (*other* 1.300)
)

(with-agent  protects
  :instance  (*other* 1.300)
)

(with-agent  material-clay
  :instance  ((*other* 1.200)
              (mclay-pot 0.100) )
)

(with-agent  material-ice
  :instance  (*other* 1.300)
)

(with-agent  material-stone
  :instance  (*other* 1.300)
)

(with-agent  material-wood
  :instance  ((*other* 1.100)
              (mwood-bf 0.200) )
)

(with-agent  material-metal
  :instance  ((*other* 1.200)
              (mmetal-ckves 0.100) )
)

(with-agent  attached-to
  :instance  (*other* 1.300)
)

(with-agent  in-touch-with
  :instance  (*other* 1.300)
)

(with-agent  supports
  :instance  (*other* 1.300)
)

(with-agent  part-of
  :instance  (*other* 1.300)
)

(with-agent  behind
  :instance  (*other* 1.300)
)

(with-agent  in-front-of
  :instance  (*other* 1.300)
)

(with-agent  right-of
  :instance  (*other* 1.300)
)

(with-agent  left-of
  :instance  (*other* 1.300)
)

(with-agent  shallow
  :instance  (*other* 1.300)
)

(with-agent  deep
  :instance  (*other* 1.300)
)

(with-agent  low
  :instance  (*other* 1.300)
)

(with-agent  high
  :instance  (*other* 1.300)
)

(with-agent  narrow
  :instance  (*other* 1.300)
)

(with-agent  wide
  :instance  (*other* 1.300)
)

(with-agent  between
  :instance  (*other* 1.300)
)

(with-agent  prolonged
  :instance  (*other* 1.300)
)

(with-agent  brief
  :instance  (*other* 1.300)
)

(with-agent  long
  :instance  (*other* 1.300)
)

(with-agent  short
  :instance  (*other* 1.300)
)

(with-agent  end-together
  :instance  (*other* 1.300)
)

(with-agent  start-together
  :instance  (*other* 1.300)
)

(with-agent  during
  :instance  (*other* 1.300)
)

(with-agent  after
  :instance  (*other* 1.300)
)

(with-agent  before
  :instance  (*other* 1.300)
)

(with-agent  follows
  :instance  (*other* 1.300)
)

(with-agent  cause
  :instance  (*other* 1.300)
)

(with-agent  to-reach
  :instance  (*other* 1.300)
)

(with-agent  below
  :instance  (*other* 1.300)
)

(with-agent  above
  :instance  (*other* 1.300)
)

(with-agent  or
  :instance  (*other* 1.300)
)

(with-agent  and
  :instance  (*other* 1.300)
)

(with-agent  not
  :instance  (*other* 1.300)
)


(check-for-unresolved-references)

;; There are 150 WITH-AGENT forms in this file.



;;  *******************************************************************
;;  **********    A D D E N D U M        ******************************
;;  *******************************************************************

(verbose 'add-to-wm (list (coa 'sit-icf) (coa 'sit-ihc)))

(load "D:\\DUAL\\AMBR\\KB\\EPISODIC\\T_ICC.LSP")

(attach-target)


(defun  write-data-file (file-name)
  "Writes the results of the 'ice cube experiment'."
  (let ((ICF (find-coalition 'sit-ICF))
        (IHC (find-coalition 'sit-IHC))
        (MTF (find-coalition 'sit-MTF))
        (FDO (find-coalition 'sit-FDO))
        (GP  (find-coalition 'sit-GP )) )
    (with-open-file (file file-name :direction :output)
      (format t "~%Now writing data file ~A  " file-name)
      (dotimes (k 100)                              ; 100 periods
        (AMBR 2.0)                                  ; of 2 time units each
        (format file "~5,1F ~2D ~2D ~8,5F ~8,5F ~8,5F ~8,5F  ~6,3F ~6,3F ~6,3F~%"
                *time*
                (count-active-members ICF)
                (count-active-members IHC)
                (retrieval-index ICF T)  ; memoize-p is T
                (retrieval-index IHC T)
                (mapping-index   ICF :mode :linear :memoize-p T)
                (mapping-index   IHC :mode :linear :memoize-p T)
                (retrieval-index MTF T)
                (retrieval-index FDO T)
                (retrieval-index GP  T) ))
      (format t "~%Finished working on ~A  " file-name)
      (values) )))


;;;  To block the mapping of all situations except SIT-ICF,
;;;  override the method from AMBR/MARKER.LSP with the one below:
;;;
; (defmethod trigger-symbolic-processor ((agent instance-agent)
;                                        (event (eql :enter-WM)) )
;   (let ((concept-agent (instance->concept agent
;                                   :cerror-p nil)))  ; see AMBR/KREPRES.LSP
;     (unless (null concept-agent)                    ; just created [Skolem] ?
;>>     (when (or (member agent (duali::coalition-members
;>>                                (duali::find-coalition 'ambr::sit-ICC)))
;>>               (member agent (duali::coalition-members
;>>                                (duali::find-coalition 'ambr::sit-ICF))) )
;         (ambrc::send-T-marker agent concept-agent) ))))
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;;;; Log of the run in the 'parallel condition'.  June 25, 1998.

> *marker-emission-flag*
T
> *ambr-decay-rate*
1.850
> 
> (write-data-file "d:\\dual\\ambr_exp\\ice\\ice_cube.dat")
Now writing data file d:\dual\ambr_exp\ice\ice_cube.dat  
;; T=0.50, adding #$ice-cube-icf to WM.
;; T=0.62, adding #$in-ihc-wc to WM.
;; T=1.24, adding #$high-t-ihc to WM.
;; T=1.84, adding #$t-of-icf-i to WM.
;; T=1.86, adding #$low-t-icf to WM.
;; T=2.56, adding #$sit-icf to WM.
;; T=3.02, adding #$water-ihc to WM.
;; T=4.26, adding #$made-of-icf to WM.
;; T=4.58, adding #$mglass-icf to WM.
;; T=4.60, adding #$glass-icf to WM.
;; T=5.85, adding #$on-icf-ig to WM.
;; T=6.80, adding #$cup-ihc to WM.
;; T=6.85, adding #$on-ihc to WM.
;; T=6.90, adding #$initst-icf-1 to WM.
;; T=8.15, adding #$sit-ihc to WM.
;; T=8.20, adding #$imm-htr-ihc to WM.
;; T=8.30, adding #$fridge-icf to WM.
;; T=8.35, adding #$t-of-ihc-ih to WM.
;; T=8.35, adding #$t-of-icf-f to WM.
;; T=8.60, adding #$t-of-ihc-w to WM.
;; T=8.60, adding #$in-icf-gf to WM.
;; T=9.10, adding #$in-ihc-iw to WM.
;; T=9.20, adding #$goalst-icf to WM.
;; T=9.75, adding #$initst-ihc to WM.
;; T=10.80, adding #$in-icf-if to WM.
;; T=14.10, adding #$saucer-ihc to WM.
;; T=14.20, adding #$made-of-ihc to WM.
;; T=14.25, adding #$mchina-ihc to WM.
;; T=21.00, adding #$goalst-ihc to WM.
;; T=32.70, adding #$cause-ihc to WM.
;; T=42.50, adding #$to-reach-ihc to WM.
;; T=43.20, adding #$endst-ihc to WM.
;; T=43.30, adding #$follows-ihc to WM.
;; T=61.20, #<PR COKE-ICC> received in COKE-ICC<==>WATER-IHC.
;; T=64.70, #<PR SIT-ICC> received in SIT-ICC<==>SIT-IHC.
;; T=65.60, #<PR IN-ICC-CG> received in IN-ICC-CG<-->IN-IHC-WC.
;; T=68.00, #<PR GLASS-ICC> received in GLASS-ICC<==>CUP-IHC.
;; T=77.90, #<PR ON-ICC> received in ON-ICC<-->ON-IHC.
;; T=79.30, #<PR INITST-ICC> received in INITST-ICC<-->INITST-IHC.
;; T=81.10, #<PR IN-ICC-IC> received in IN-ICC-IC<-->IN-IHC-IW.
;; T=86.40, #<PR MADE-OF-ICC> received in MADE-OF-ICC<-->MADE-OF-IHC.
;; T=88.30, #<PR TABLE-ICC> received in TABLE-ICC<==>SAUCER-IHC.
;; T=91.40, #<PR ICE-CUBE-ICC> received in ICE-CUBE-ICC<==>IMM-HTR-IHC.
;; T=91.40, #<PR LOW-T-ICC> received in LOW-T-ICC<-->HIGH-T-IHC.
;; T=92.50, #<PR MGLASS-ICC> received in MGLASS-ICC<-->MCHINA-IHC.
;; T=141.80, #<PR T-OF-ICC> received in T-OF-ICC<-->T-OF-IHC-IH.
Finished working on d:\dual\ambr_exp\ice\ice_cube.dat  

> (mt)
 sit-icc         sit-ihc          1.93   winner
 ice-cube-icc    imm-htr-ihc      1.82   winner
 coke-icc        water-ihc        1.80   winner
 glass-icc       cup-ihc          1.83   winner
 table-icc       saucer-ihc       1.72   winner
 in-icc-ic       in-ihc-iw        1.81   winner
 in-icc-cg       in-ihc-wc        1.79   winner
 on-icc          on-ihc           1.79   winner
 t-of-icc        t-of-ihc-ih      1.78   winner
 low-t-icc       high-t-ihc       1.75   winner
 made-of-icc     made-of-ihc      1.78   winner
 mglass-icc      mchina-ihc       1.73   winner
 initst-icc      initst-ihc       1.80   winner
 endst-icc       goalst-ihc       1.24   mature
 follows-icc     follows-ihc      1.30   mature

> (hyps #$endst-icc)
 1.235  m   2    endst-icc<-->goalst-ihc
 0.976  m   1    endst-icc<==>t-of-ihc-w
 1.174  m   2    endst-icc<-->endst-ihc
 3 hyps (0 e + 3 m + 0 W)

> (hyps #$follows-icc)
 1.214  m   2    follows-icc<-->cause-ihc
 1.298  m   1    follows-icc<-->follows-ihc
 1.239  m   1    follows-icc<-->to-reach-ihc
 3 hyps (0 e + 3 m + 0 W)

> (rt #$follows-icc)
 0.630  1.298  m  follows-icc<-->follows-ihc 
 0.080  1.239  m  follows-icc<-->to-reach-ihc 
 0.287  1.214  m  follows-icc<-->cause-ihc 
#<RT FOLLOWS-ICC>
> (rt #$endst-icc)
 0.919  1.235  m  endst-icc<-->goalst-ihc 
 0.077  1.174  m  endst-icc<-->endst-ihc 
 0.000  0.976  m  endst-icc<==>t-of-ihc-w 
#<RT ENDST-ICC>


;;;;;;; *******************************************************

;;;; Log of the second run in the 'serial condition'.  June 26, 1998.

> (write-data-file "d:\\dual\\ambr_exp\\ice\\icf_icc.dat")
Now writing data file d:\dual\ambr_exp\ice\icf_icc.dat  
;; T=0.50, adding #$ice-cube-icf to WM.
;; T=0.62, adding #$in-ihc-wc to WM.
;; T=1.24, adding #$high-t-ihc to WM.
;; T=1.84, adding #$t-of-icf-i to WM.
;; T=1.86, adding #$low-t-icf to WM.
;; T=2.56, adding #$sit-icf to WM.
;; T=3.02, adding #$water-ihc to WM.
;; T=3.82, adding #$made-of-icf to WM.
;; T=4.06, adding #$mglass-icf to WM.
;; T=4.08, adding #$glass-icf to WM.
;; T=5.45, adding #$cup-ihc to WM.
;; T=5.50, adding #$on-ihc to WM.
;; T=5.70, adding #$on-icf-ig to WM.
;; T=6.70, adding #$initst-icf-1 to WM.
;; T=7.85, adding #$fridge-icf to WM.
;; T=7.90, adding #$t-of-icf-f to WM.
;; T=8.15, adding #$goalst-icf to WM.
;; T=8.15, adding #$in-icf-gf to WM.
;; T=9.35, adding #$in-icf-if to WM.
;; T=13.80, adding #$to-reach-icf to WM.
;; T=17.30, adding #$imm-htr-ihc to WM.
;; T=17.35, adding #$sit-ihc to WM.
;; T=17.45, adding #$t-of-ihc-ih to WM.
;; T=17.75, adding #$t-of-ihc-w to WM.
;; T=19.00, adding #$follows-icf to WM.
;; T=19.35, adding #$endst-icf to WM.
;; T=19.90, adding #$initst-ihc to WM.
;; T=19.95, adding #$in-ihc-iw to WM.
;; T=20.60, adding #$cause-icf-t to WM.
;; T=20.70, adding #$interst-icf to WM.
;; T=23.40, adding #$made-of-ihc to WM.
;; T=24.60, adding #$mchina-ihc to WM.
;; T=25.10, adding #$saucer-ihc to WM.
;; T=29.30, adding #$cause-icf-i to WM.
;; T=29.40, adding #$initst-icf-2 to WM.
;; T=42.70, #<PR SIT-ICC> received in SIT-ICC<-->SIT-ICF.
;; T=51.50, #<PR LOW-T-ICC> received in LOW-T-ICC<-->LOW-T-ICF.
;; T=53.60, #<PR MADE-OF-ICC> received in MADE-OF-ICC<-->MADE-OF-ICF.
;; T=55.00, #<PR ICE-CUBE-ICC> received in ICE-CUBE-ICC<-->ICE-CUBE-ICF.
;; T=59.60, #<PR MGLASS-ICC> received in MGLASS-ICC<-->MGLASS-ICF.
;; T=64.30, #<PR GLASS-ICC> received in GLASS-ICC<-->GLASS-ICF.
;; T=66.00, #<PR ON-ICC> received in ON-ICC<-->ON-ICF-IG.
;; T=72.70, #<PR COKE-ICC> received in COKE-ICC<==>FRIDGE-ICF.
;; T=98.70, #<PR INITST-ICC> received in INITST-ICC<-->INITST-ICF-1.
;; T=152.40, #<PR TABLE-ICC> received in TABLE-ICC<==>INTERST-ICF.
Finished working on d:\dual\ambr_exp\ice\icf_icc.dat
  
> (report)
 |  sit    |   ri  | ca |   mi  | cm |    |  sit    |   ri  | ca |   mi  | cm |   
 +---------+-------+----+-------+----+    +---------+-------+----+-------+----+
 | sit-WTP | 0.084 |  7 | -.999 |  0 |    | sit-BF  | 0.006 |  1 | -.999 |  0 |   
 | sit-IHC | 0.295 | 14 | -.999 |  0 |    | sit-GP  | 0.019 |  2 | -.999 |  0 |   
 | sit-MTF | 0.116 |  6 | -.999 |  0 |    | sit-BPF | 0.000 |  0 | -.999 |  0 |   
 | sit-ICF | 1.218 | 21 | 1.352 | 15 |    | sit-FDO | 0.057 |  4 | -.999 |  0 |   
 +---------+-------+----+-------+----+    +---------+-------+----+-------+----+
 | SIT-ICC | 2.835 | 15 | 1.352 | 15 |    | all WM  |192.09 |250 |       |  0 |


> (mt)
 sit-icc         sit-icf          1.93   winner
 ice-cube-icc    ice-cube-icf     1.74   winner
 coke-icc        fridge-icf       1.72   winner
 glass-icc       glass-icf        1.75   winner
 table-icc       interst-icf      1.60   winner
 in-icc-ic       in-icf-if        1.37   mature
 in-icc-cg       in-icf-gf        1.28   mature
 on-icc          on-icf-ig        1.62   winner
 t-of-icc        t-of-icf-i       1.48   mature
 low-t-icc       low-t-icf        1.74   winner
 made-of-icc     made-of-icf      1.78   winner
 mglass-icc      mglass-icf       1.73   winner
 initst-icc      initst-icf-1     1.73   winner
 endst-icc       goalst-icf       1.30   mature
 follows-icc     to-reach-icf     1.39   mature
> (g 100)
;; T=229.00, #<PR IN-ICC-CG> received in IN-ICC-CG<-->IN-ICF-GF.
;; T=249.40, #<PR IN-ICC-IC> received in IN-ICC-IC<-->IN-ICF-IF.
;; Time 300.00, 260 agents in WM: 114 c,  82 i,  64 h (0+52+12)
;;    3 clocks;   0 ag on agenda:   0 c,   0 i,   0 h (0+0+0), 0 nc

 |  sit    |   ri  | ca |   mi  | cm |    |  sit    |   ri  | ca |   mi  | cm |   
 +---------+-------+----+-------+----+    +---------+-------+----+-------+----+
 | sit-WTP | 0.085 |  7 | -.999 |  0 |    | sit-BF  | 0.006 |  1 | -.999 |  0 |   
 | sit-IHC | 0.301 | 14 | -.999 |  0 |    | sit-GP  | 0.019 |  2 | -.999 |  0 |   
 | sit-MTF | 0.116 |  6 | -.999 |  0 |    | sit-BPF | 0.000 |  0 | -.999 |  0 |   
 | sit-ICF | 1.285 | 21 | 1.447 | 15 |    | sit-FDO | 0.057 |  4 | -.999 |  0 |   
 +---------+-------+----+-------+----+    +---------+-------+----+-------+----+
 | SIT-ICC | 2.851 | 15 | 1.447 | 15 |    | all WM  |192.38 |248 |       |  0 |

> (hyps #$t-of-icc)
 1.487  m   1    t-of-icc<-->t-of-icf-i
 1.436  m   1    t-of-icc<-->t-of-icf-f
 0.683  m   1    t-of-icc<-->cause-icf-t
 0.687  m   1    t-of-icc<-->cause-icf-i
 4 hyps (0 e + 4 m + 0 W)

> (show-coa 'sit-ihc)
 0.921  imm-htr-ihc       0.835  0.035
 0.735  high-t-ihc        0.638  0.190
 0.575  t-of-ihc-ih       0.480  0.000
 0.501  cup-ihc           0.412  0.018
 0.484  sit-ihc           0.396  0.000
 0.450  in-ihc-wc         0.366  0.236
 0.390  water-ihc         0.313  0.061
 0.330  t-of-ihc-w        0.261  0.000
 0.318  on-ihc            0.252  0.093
 0.228  initst-ihc        0.177  0.000
 0.218  made-of-ihc       0.169  0.060
 0.213  in-ihc-iw         0.164  0.000
 0.187  mchina-ihc        0.144  0.025
 0.176  saucer-ihc        0.135  0.000
 0.000  goalst-ihc        0.059  0.000
 0.000  endst-ihc         0.023  0.000
 0.000  to-reach-ihc      0.015  0.000
 0.000  follows-ihc       0.009  0.000
 0.000  cause-ihc         0.054  0.000
14

|#

