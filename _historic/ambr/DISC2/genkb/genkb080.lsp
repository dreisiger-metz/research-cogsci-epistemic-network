
;; This file has been generated automatically.
;; It is a random sample of concept-to-instance links.
;; See AMBR/EXPERMT/GENKB.LSP for details.

;; Description of current GENKB parameters:
;;  *INSTANCE-link-GENKB-pattern* = (0.4 0.3 0.3 0.2 0.1)
;;  *A-LINK-GENKB-weight* = 0.2

;; Heralds of all GENKB templates underlying this file:
;;  Concepts from KB/SEMANTIC/ABSTRACT.LSP, ver. 3.0.0.
;;  Concepts from KB/SEMANTIC/TEMP_REL.LSP, ver. 3.0.0.
;;  Concepts from KB/SEMANTIC/SPAT_REL.LSP, ver. 3.0.0.
;;  Concepts from KB/SEMANTIC/PHYS_REL.LSP, ver. 3.0.0.
;;  Concepts from KB/SEMANTIC/PHYSPROP.LSP, ver. 3.0.0.
;;  Concepts from KB/SEMANTIC/LIQUID.LSP, ver. 3.0.0.
;;  Concepts from KB/SEMANTIC/CONTAIN.LSP, ver. 3.0.0.
;;  Concepts from KB/SEMANTIC/FOOD.LSP, ver. 3.0.0.
;;  Concepts from KB/SEMANTIC/ANIMALS.LSP, ver. 3.0.0.
;;  Concepts from KB/SEMANTIC/KITCHEN.LSP, ver. 3.0.0.
;;  Concepts from KB/SEMANTIC/FOREST.LSP, ver. 3.0.0.
;;  Concepts from KB/SEMANTIC/FURNIT.LSP, ver. 3.0.0.
;;  Base sit.WTP -- Water in a Teapot on a hot Plate, ver.3.0.0.
;;  Base sit.BF  -- Wooden Bowl on a Fire burns out, ver.3.0.0.
;;  Base sit.GP  -- Glass on a hot Plate breaks, ver.3.0.0.
;;  Base sit.IHC -- Imm.Heater in a Cup with water, ver.3.0.0.
;;  Base sit.MTF -- Milk in a Teapot in a Fridge, ver.3.0.0.
;;  Base sit.ICF -- Ice Cube in a glass in a Fridge, ver.3.0.0.
;;  Base sit.BPF -- Butter on a Plate in a Fridge, ver.3.0.0.
;;  Base sit.FDO -- Food on a Dish in an Oven, ver.3.0.0.
;;  Base sit.STC -- Sugar in Tea in a Cup, ver.3.0.0.
;;  Base sit.SFF -- Salt in Food on a plate in a Fridge, ver.3.0.0.
;;  Base sit.ERW -- Egg in Red Water, ver.3.0.0.
;;  Base sit.GWB -- Glass in a Wooden Box, ver.3.0.0.


(cl:in-package "AMBR")

(require "ltm")


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
  :instance  (*other* 1.300)
)

(with-agent  tv-set
  :instance  (*other* 1.300)
)

(with-agent  lamp
  :instance  (*other* 1.300)
)

(with-agent  immersion-heater
  :instance  ((*other* 1.200)
              (imm-htr-ihc 0.100) )
)

(with-agent  dish-washer
  :instance  (*other* 1.300)
)

(with-agent  oven
  :a-link    (t-of-fdo-o 0.200)
  :instance  ((oven-fdo 0.700)
              (*other* 0.600) )
)

(with-agent  fridge
  :instance  ((*other* 0.900)
              (fridge-mtf 0.300)
              (fridge-sff 0.100) )
)

(with-agent  hot-plate
  :instance  ((hplate-gp 0.600)
              (hplate-wtp 0.600)
              (*other* 0.100) )
)

(with-agent  baking-dish
  :instance  (*other* 1.300)
)

(with-agent  pan
  :instance  (*other* 1.300)
)

(with-agent  saucepan
  :instance  (*other* 1.300)
)

(with-agent  teapot
  :instance  ((*other* 1.100)
              (tpot-mtf 0.200) )
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

(with-agent  sugar
  :a-link    (taste-of-stc-s 0.200)
  :instance  ((*other* 1.000)
              (sugar-stc 0.300) )
)

(with-agent  salt
  :instance  ((salt-sff 0.700)
              (*other* 0.600) )
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
  :instance  ((egg-erw 0.400)
              (*other* 0.900) )
)

(with-agent  meat-soup
  :instance  (*other* 1.300)
)

(with-agent  butter
  :instance  ((butter-bpf 0.700)
              (*other* 0.600) )
)

(with-agent  meat
  :instance  (*other* 1.300)
)

(with-agent  bread
  :instance  (*other* 1.300)
)

(with-agent  food
  :instance  ((food-sff 0.400)
              (*other* 0.700)
              (food-fdo 0.200) )
)

(with-agent  cow
  :instance  (*other* 1.300)
)

(with-agent  chicken
  :instance  (*other* 1.300)
)

(with-agent  saucer
  :instance  ((saucer-stc 0.400)
              (*other* 0.900) )
)

(with-agent  bag
  :instance  (*other* 1.300)
)

(with-agent  box
  :instance  ((*other* 1.000)
              (box-gwb 0.300) )
)

(with-agent  pot
  :instance  (*other* 1.300)
)

(with-agent  plate
  :instance  ((*other* 0.800)
              (plate-sff 0.500) )
)

(with-agent  bowl
  :instance  ((*other* 1.200)
              (bowl-bf 0.100) )
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

(with-agent  ice-cube
  :instance  ((*other* 1.100)
              (ice-cube-icf 0.200) )
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
  :instance  ((*other* 1.000)
              (milk-mtf 0.300) )
)

(with-agent  water
  :instance  ((*other* 1.200)
              (water-wtp 0.100) )
)

(with-agent  bottle
  :instance  (*other* 1.300)
)

(with-agent  glass
  :instance  ((*other* 0.900)
              (glass-gwb 0.300)
              (glass-gp 0.100) )
)

(with-agent  cup
  :instance  ((*other* 1.200)
              (cup-stc 0.100) )
)

(with-agent  sour-taste
  :instance  (*other* 1.300)
)

(with-agent  salt-taste
  :instance  ((salty-sff 0.400)
              (*other* 0.900) )
)

(with-agent  sweet-taste
  :instance  ((*other* 1.100)
              (swt-jc-taste 0.200) )
)

(with-agent  taste-of
  :a-link    (sweet-stc 0.200)
  :instance  ((taste-of-sff-f 0.400)
              (salt-is-salty 0.300)
              (*other* 0.600) )
)

(with-agent  square-shape
  :instance  (*other* 1.300)
)

(with-agent  rectang-shape
  :instance  (*other* 1.300)
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
  :instance  ((*other* 1.000)
              (green-mtf 0.300) )
)

(with-agent  red
  :instance  (*other* 1.300)
)

(with-agent  black
  :instance  ((*other* 1.000)
              (black-wtp 0.300) )
)

(with-agent  white
  :instance  ((*other* 1.000)
              (wwine-is-white 0.300) )
)

(with-agent  low-temp
  :instance  ((*other* 0.800)
              (low-t-icf 0.500) )
)

(with-agent  medium-temp
  :instance  (*other* 1.300)
)

(with-agent  high-temp
  :a-link    (oven-fdo 0.200)
  :instance  ((high-t-wtp 0.400)
              (*other* 0.600)
              (high-t-gp 0.200)
              (high-t-ihc 0.100) )
)

(with-agent  shape-of
  :instance  (*other* 1.300)
)

(with-agent  color-of
  :instance  ((red-wine-color 0.400)
              (color-of-wtp 0.400)
              (color-of-erw-w 0.300)
              (*other* 0.200) )
)

(with-agent  temperature-of
  :instance  ((t-of-wtp-w 0.500)
              (t-of-bf-f 0.300)
              (fridge-is-cold 0.300)
              (t-of-bpf-f 0.200) )
)

(with-agent  is-dissipated
  :instance  (*other* 1.300)
)

(with-agent  is-burnt-out
  :instance  ((is-burnt-bf 0.400)
              (*other* 0.900) )
)

(with-agent  is-broken
  :instance  ((is-broken-gp 0.400)
              (*other* 0.900) )
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
  :instance  (*other* 1.300)
)

(with-agent  material-china
  :instance  ((*other* 1.200)
              (mchina-saucer 0.100) )
)

(with-agent  material-ice
  :instance  (*other* 1.300)
)

(with-agent  material-stone
  :instance  (*other* 1.300)
)

(with-agent  material-wood
  :instance  ((*other* 0.600)
              (mwood-bf 0.700) )
)

(with-agent  material-glass
  :instance  ((*other* 0.800)
              (mglass-icf 0.300)
              (mglass-gp 0.200) )
)

(with-agent  material-metal
  :instance  ((*other* 0.700)
              (mmetal-ckves 0.400)
              (mmetal-erw 0.200) )
)

(with-agent  made-of
  :instance  ((*other* 0.400)
              (made-of-ihc 0.300)
              (made-of-bf 0.300)
              (made-of-gwb-g 0.200)
              (ckves-md-metal 0.100) )
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

(with-agent  on
  :instance  ((on-bpf 0.400)
              (*other* 0.600)
              (on-gp 0.300) )
)

(with-agent  in
  :instance  ((*other* 1.100)
              (in-stc-tc 0.200) )
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

;; There are 152 WITH-AGENT forms in this file.
