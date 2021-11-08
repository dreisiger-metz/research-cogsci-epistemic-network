# This script file recreates the terms and sentences described by Rogers
# and McClelland (2004, p395) within an FGREP framework.
set learning-algorithm batch
set learning-rate 0.1
set print-errors 1
set print-sentence-activations 1


# Note that, here, relations are defined as fixed terms
declare-term is 1.0 0.0
declare-term can 0.0 1.0 0.0
declare-term has 0.0 0.0 1.0 0.0

declare-term #pine
declare-term #oak
declare-term #rose
declare-term #daisy
declare-term #robin
declare-term #canary
declare-term #sunfish
declare-term #salmon

declare-term pretty
declare-term big
declare-term living
declare-term green
declare-term red
declare-term yellow

declare-term grow
declare-term move
declare-term swim
declare-term fly
declare-term sing

declare-term skin
declare-term roots
declare-term leaves
declare-term bark
declare-term branch
declare-term petals
declare-term wings
declare-term feathers
declare-term gills
declare-term scales

# sentence declarations
add-sentence is #pine big
add-sentence is #pine living
add-sentence is #pine green
add-sentence is #oak big
add-sentence is #oak living
add-sentence is #rose pretty
add-sentence is #rose living
add-sentence is #rose red
add-sentence is #daisy pretty
add-sentence is #daisy living
add-sentence is #daisy yellow
add-sentence is #robin living
add-sentence is #robin red
add-sentence is #canary living
add-sentence is #canary yellow
add-sentence is #sunfish living
add-sentence is #sunfish yellow
add-sentence is #salmon living
add-sentence is #salmon red
add-sentence can #pine grow
add-sentence can #oak grow
add-sentence can #rose grow
add-sentence can #daisy grow
add-sentence can #robin grow
add-sentence can #robin move
add-sentence can #robin fly
add-sentence can #canary grow
add-sentence can #canary move
add-sentence can #canary fly
add-sentence can #canary sing
add-sentence can #sunfish grow
add-sentence can #sunfish move
add-sentence can #sunfish swim
add-sentence can #salmon grow
add-sentence can #salmon move
add-sentence can #salmon swim
add-sentence has #pine roots
add-sentence has #pine bark
add-sentence has #pine branch
add-sentence has #oak roots
add-sentence has #oak bark
add-sentence has #oak leaves
add-sentence has #oak branch
add-sentence has #rose roots
add-sentence has #rose leaves
add-sentence has #rose petals
add-sentence has #daisy roots
add-sentence has #daisy leaves
add-sentence has #daisy petals
add-sentence has #robin skin
add-sentence has #robin wings
add-sentence has #robin feathers
add-sentence has #canary skin
add-sentence has #canary wings
add-sentence has #canary feathers
add-sentence has #sunfish skin
add-sentence has #sunfish gills
add-sentence has #sunfish scales
add-sentence has #salmon skin
add-sentence has #salmon gills
add-sentence has #salmon scales


# and start the training process.
set output-file iteration--50000.meta
train-network iterations 50000
show cpu-time-total epochs seed
set output-file iteration--50000.expr
inspect-sentences
set output-file iteration--50000.term
inspect-terms
set output-file stdout
save iteration--50000.save
