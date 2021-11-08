
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
  :a-link    (t-of-bf-f 0.200)
  :instance  ((fire-bf 0.700)
              (*other* 0.600) )
)

(with-agent  tv-set
  :instance  (*other* 1.300)
)

(with-agent  lamp
  :instance  (*other* 1.300)
)

(with-agent  immersion-heater
  :a-link    (t-of-ihc-ih 0.200)
  :instance  ((*other* 1.200)
              (imm-htr-ihc 0.100) )
)

(with-agent  dish-washer
  :instance  (*other* 1.300)
)

(with-agent  oven
  :instance  ((*other* 1.000)
              (oven-fdo 0.300) )
)

(with-agent  fridge
  :a-link    (t-of-mtf-f 0.200)
  :instance  ((fridge-mtf 1.000)
              (*other* 0.300) )
)

(with-agent  hot-plate
  :instance  ((*other* 1.000)
              (hplate-wtp 0.300) )
)

(with-agent  baking-dish
  :instance  ((dish-fdo 0.400)
              (*other* 0.900) )
)

(with-agent  pan
  :instance  (*other* 1.300)
)

(with-agent  saucepan
  :instance  (*other* 1.300)
)

(with-agent  teapot
  :instance  (*other* 1.300)
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
  :instance  ((sugar-stc 0.900)
              (*other* 0.400) )
)

(with-agent  salt
  :instance  ((*other* 0.900)
              (salt-sff 0.400) )
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
  :instance  ((*other* 1.000)
              (egg-erw 0.300) )
)

(with-agent  meat-soup
  :instance  (*other* 1.300)
)

(with-agent  butter
  :instance  ((*other* 1.200)
              (butter-bpf 0.100) )
)

(with-agent  meat
  :instance  (*other* 1.300)
)

(with-agent  bread
  :instance  (*other* 1.300)
)

(with-agent  food
  :instance  ((*other* 1.000)
              (food-sff 0.300) )
)

(with-agent  cow
  :instance  (*other* 1.300)
)

(with-agent  chicken
  :instance  (*other* 1.300)
)

(with-agent  saucer
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
  :instance  ((*other* 1.200)
              (plate-sff 0.100) )
)

(with-agent  bowl
  :instance  ((bowl-bf 0.400)
              (*other* 0.900) )
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
  :instance  (ice-cube-icf 1.300)
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
  :instance  ((*other* 0.700)
              (tea-stc 0.600) )
)

(with-agent  milk
  :instance  ((*other* 1.000)
              (milk-mtf 0.300) )
)

(with-agent  water
  :instance  ((water-erw 0.400)
              (water-wtp 0.600)
              (*other* 0.200)
              (water-bf 0.100) )
)

(with-agent  bottle
  :instance  (*other* 1.300)
)

(with-agent  glass
  :instance  ((*other* 0.700)
              (glass-gp 0.300)
              (glass-gwb 0.300) )
)

(with-agent  cup
  :instance  ((*other* 1.200)
              (cup-ihc 0.100) )
)

(with-agent  sour-taste
  :instance  (*other* 1.300)
)

(with-agent  salt-taste
  :a-link    (salt-sff 0.200)
  :instance  (*other* 1.300)
)

(with-agent  sweet-taste
  :instance  ((*other* 0.900)
              (sweet-stc 0.300)
              (swt-fr-taste 0.100) )
)

(with-agent  taste-of
  :instance  ((juice-is-sweet 0.400)
              (*other* 0.300)
              (sugar-is-sweet 0.500)
              (taste-of-sff-f 0.100) )
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
  :instance  ((*other* 1.000)
              (circular-bpf 0.300) )
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
  :instance  ((red-tomato 0.500)
              (*other* 0.800) )
)

(with-agent  black
  :instance  (*other* 1.300)
)

(with-agent  white
  :instance  (*other* 1.300)
)

(with-agent  low-temp
  :a-link    (fridge-mtf 0.200)
  :instance  ((low-t-icf 0.700)
              (*other* 0.300)
              (low-t-mtf 0.200)
              (low-t-bpf 0.100) )
)

(with-agent  medium-temp
  :instance  (*other* 1.300)
)

(with-agent  high-temp
  :a-link    (hplate-wtp 0.200)
  :instance  ((*other* 0.400)
              (high-t-wtp 0.600)
              (high-t-bf 0.200)
              (high-t-ihc 0.100) )
)

(with-agent  shape-of
  :instance  (*other* 1.300)
)

(with-agent  color-of
  :a-link    (red-erw 0.200)
  :instance  ((color-of-erw-w 0.400)
              (*other* 0.600)
              (white-wine-color 0.200)
              (color-of-wtp 0.100) )
)

(with-agent  temperature-of
  :instance  ((t-of-ihc-w 0.600)
              (htsrc-is-hot 0.300)
              (t-of-icf-i 0.300)
              (*other* 0.100) )
)

(with-agent  is-dissipated
  :instance  ((*other* 0.800)
              (is-dissip-bf 0.500) )
)

(with-agent  is-burnt-out
  :instance  ((*other* 1.000)
              (is-burnt-bf 0.300) )
)

(with-agent  is-broken
  :instance  ((is-broken-gp 0.800)
              (*other* 0.500) )
)

(with-agent  burns-out
  :instance  (*other* 1.300)
)

(with-agent  breaks
  :instance  (*other* 1.300)
)

(with-agent  protects
  :instance  ((*other* 1.100)
              (protects-gwb 0.200) )
)

(with-agent  material-clay
  :instance  (*other* 1.300)
)

(with-agent  material-china
  :instance  ((mchina-cup 0.400)
              (*other* 0.800)
              (mchina-bpf 0.100) )
)

(with-agent  material-ice
  :instance  (*other* 1.300)
)

(with-agent  material-stone
  :instance  (*other* 1.300)
)

(with-agent  material-wood
  :instance  ((*other* 1.000)
              (mwood-gwb 0.300) )
)

(with-agent  material-glass
  :instance  ((*other* 0.500)
              (mglass-icf 0.300)
              (mglass-bottle 0.300)
              (mglass-gp 0.200) )
)

(with-agent  material-metal
  :instance  ((*other* 1.200)
              (mmetal-ckves 0.100) )
)

(with-agent  made-of
  :instance  ((*other* 0.700)
              (made-of-bf 0.300)
              (saucer-md-china 0.200)
              (bottle-md-gl 0.100) )
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
  :instance  ((on-fdo 0.600)
              (on-bpf 0.300)
              (*other* 0.400) )
)

(with-agent  in
  :instance  ((in-stc-st 0.400)
              (*other* 0.600)
              (in-stc-tc 0.200)
              (in-bf 0.100) )
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