# This script file recreates the terms and sentences described by Rogers
# and McClelland (2004, p395) within an FGREP framework.
#
# Note that, here, relations are defined as fixed terms
declare-term is        1.0 0.0
declare-term can       0.0 1.0 0.0
declare-term has       0.0 0.0 1 0.0
declare-term is-doing  0.0 0.0 0.0 1 0.0

declare-term episode-1
declare-term episode-2

declare-term #dog-1
declare-term #cat-1
declare-term #cat-2
declare-term #bird-1

declare-term big
declare-term small
declare-term pretty
declare-term living
declare-term brown
declare-term grey
declare-term black

declare-term move
declare-term run
declare-term fly
declare-term sing
declare-term bark

declare-term skin
declare-term fur
declare-term legs
declare-term wings
declare-term feathers

declare-term chasing
declare-term fleeing


# sentence declarations
add-sentence episode-1 is #dog-1 big
add-sentence episode-1 is #dog-1 living
add-sentence episode-1 is #dog-1 brown
add-sentence episode-1 can #dog-1 move
add-sentence episode-1 can #dog-1 run
add-sentence episode-1 can #dog-1 bark
add-sentence episode-1 has #dog-1 skin
add-sentence episode-1 has #dog-1 fur
add-sentence episode-1 has #dog-1 legs
add-sentence episode-1 is-doing #dog-1 chasing
add-sentence episode-1 is #cat-1 big
add-sentence episode-1 is #cat-1 living
add-sentence episode-1 is #cat-1 grey
add-sentence episode-1 can #cat-1 move
add-sentence episode-1 can #cat-1 run
add-sentence episode-1 has #cat-1 skin
add-sentence episode-1 has #cat-1 fur
add-sentence episode-1 has #cat-1 legs
add-sentence episode-1 is-doing #cat-1 fleeing

add-sentence episode-1 is #cat-2 big
add-sentence episode-1 is #cat-2 living
add-sentence episode-1 is #cat-2 grey
add-sentence episode-1 can #cat-2 move
add-sentence episode-1 can #cat-2 run
add-sentence episode-1 has #cat-2 skin
add-sentence episode-1 has #cat-2 fur
add-sentence episode-1 has #cat-2 legs
add-sentence episode-1 is-doing #cat-2 chasing
add-sentence episode-1 is #bird-1 little
add-sentence episode-1 is #bird-1 living
add-sentence episode-1 is #bird-1 black
add-sentence episode-1 can #bird-1 move
add-sentence episode-1 can #bird-1 run
add-sentence episode-1 can #bird-1 fly
add-sentence episode-1 has #bird-1 skin
add-sentence episode-1 has #bird-1 feathers
add-sentence episode-1 has #bird-1 wings
add-sentence episode-1 has #bird-1 legs
add-sentence episode-1 is-doing #bird-1 fleeing


# and train the network
train-network 0.00001
show cpu-time-last
save episode-1--0.00001.save
set quiet 1
set output-file episode-1--0.00001.data
inspect-term
set output-file stdout
set quiet 0

set learning-rate 0.05
train-network 0.000001
show cpu-time-last
save episode-1--0.000001.save
set quiet 1
set output-file episode-1--0.000001.data
inspect-term
set output-file stdout
set quiet 0
