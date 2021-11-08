# This script file recreates the terms and sentences described by Rogers
# and McClelland (2004, p395) within an FGREP framework.
set learning-algorithm batch
set learning-rate 0.1
set print-errors 1


# Note that, here, relations are defined as fixed terms
declare-term is 1.0 0.0
declare-term can 0.0 1.0 0.0
declare-term has 0.0 0.0 1.0 0.0

declare-term #object-1
declare-term #object-2
declare-term #object-3
declare-term #object-4
declare-term #object-5
declare-term #object-6
declare-term #object-7
declare-term #object-8

declare-term feature-1
declare-term feature-2
declare-term feature-3
declare-term feature-4
declare-term feature-5
declare-term feature-6
declare-term feature-7
declare-term feature-8

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


# save our untrained network and representations,
set quiet 1
set output-file iteration--0.meta
process-representations
show cpu-time-total epochs seed
set output-file iteration--0.expr
inspect-sentences
set output-file iteration--0.term
inspect-terms
set output-file stdout
save iteration--0.save


# and start the training process.
set output-file iteration--100.meta
train-network iterations 100
show cpu-time-total epochs seed
set output-file iteration--100.expr
inspect-sentences
set output-file iteration--100.term
inspect-terms
set output-file stdout
save iteration--100.save

set output-file iteration--200.meta
train-network iterations 100
show cpu-time-total epochs seed
set output-file iteration--200.expr
inspect-sentences
set output-file iteration--200.term
inspect-terms
set output-file stdout
save iteration--200.save

set output-file iteration--300.meta
train-network iterations 100
show cpu-time-total epochs seed
set output-file iteration--300.expr
inspect-sentences
set output-file iteration--300.term
inspect-terms
set output-file stdout
save iteration--300.save

set output-file iteration--400.meta
train-network iterations 100
show cpu-time-total epochs seed
set output-file iteration--400.expr
inspect-sentences
set output-file iteration--400.term
inspect-terms
set output-file stdout
save iteration--400.save

set output-file iteration--500.meta
train-network iterations 100
show cpu-time-total epochs seed
set output-file iteration--500.expr
inspect-sentences
set output-file iteration--500.term
inspect-terms
set output-file stdout
save iteration--500.save

set output-file iteration--600.meta
train-network iterations 100
show cpu-time-total epochs seed
set output-file iteration--600.expr
inspect-sentences
set output-file iteration--600.term
inspect-terms
set output-file stdout
save iteration--600.save

set output-file iteration--700.meta
train-network iterations 100
show cpu-time-total epochs seed
set output-file iteration--700.expr
inspect-sentences
set output-file iteration--700.term
inspect-terms
set output-file stdout
save iteration--700.save

set output-file iteration--800.meta
train-network iterations 100
show cpu-time-total epochs seed
set output-file iteration--800.expr
inspect-sentences
set output-file iteration--800.term
inspect-terms
set output-file stdout
save iteration--800.save

set output-file iteration--900.meta
train-network iterations 100
show cpu-time-total epochs seed
set output-file iteration--900.expr
inspect-sentences
set output-file iteration--900.term
inspect-terms
set output-file stdout
save iteration--900.save

set output-file iteration--1000.meta
train-network iterations 1000
show cpu-time-total epochs seed
set output-file iteration--1000.expr
inspect-sentences
set output-file iteration--1000.term
inspect-terms
set output-file stdout
save iteration--1000.save

set output-file iteration--2000.meta
train-network iterations 1000
show cpu-time-total epochs seed
set output-file iteration--2000.expr
inspect-sentences
set output-file iteration--2000.term
inspect-terms
set output-file stdout
save iteration--2000.save

set output-file iteration--3000.meta
train-network iterations 1000
show cpu-time-total epochs seed
set output-file iteration--3000.expr
inspect-sentences
set output-file iteration--3000.term
inspect-terms
set output-file stdout
save iteration--3000.save

set output-file iteration--4000.meta
train-network iterations 1000
show cpu-time-total epochs seed
set output-file iteration--4000.expr
inspect-sentences
set output-file iteration--4000.term
inspect-terms
set output-file stdout
save iteration--4000.save

set output-file iteration--5000.meta
train-network iterations 1000
show cpu-time-total epochs seed
set output-file iteration--5000.expr
inspect-sentences
set output-file iteration--5000.term
inspect-terms
set output-file stdout
save iteration--5000.save

set output-file iteration--6000.meta
train-network iterations 1000
show cpu-time-total epochs seed
set output-file iteration--6000.expr
inspect-sentences
set output-file iteration--6000.term
inspect-terms
set output-file stdout
save iteration--6000.save

set output-file iteration--7000.meta
train-network iterations 1000
show cpu-time-total epochs seed
set output-file iteration--7000.expr
inspect-sentences
set output-file iteration--7000.term
inspect-terms
set output-file stdout
save iteration--7000.save

set output-file iteration--8000.meta
train-network iterations 1000
show cpu-time-total epochs seed
set output-file iteration--8000.expr
inspect-sentences
set output-file iteration--8000.term
inspect-terms
set output-file stdout
save iteration--8000.save

set output-file iteration--9000.meta
train-network iterations 1000
show cpu-time-total epochs seed
set output-file iteration--9000.expr
inspect-sentences
set output-file iteration--9000.term
inspect-terms
set output-file stdout
save iteration--9000.save

set output-file iteration--10000.meta
train-network iterations 10000
show cpu-time-total epochs seed
set output-file iteration--10000.expr
inspect-sentences
set output-file iteration--10000.term
inspect-terms
set output-file stdout
save iteration--10000.save

set output-file iteration--20000.meta
train-network iterations 10000
show cpu-time-total epochs seed
set output-file iteration--20000.expr
inspect-sentences
set output-file iteration--20000.term
inspect-terms
set output-file stdout
save iteration--20000.save

set output-file iteration--30000.meta
train-network iterations 10000
show cpu-time-total epochs seed
set output-file iteration--30000.expr
inspect-sentences
set output-file iteration--30000.term
inspect-terms
set output-file stdout
save iteration--30000.save

set output-file iteration--40000.meta
train-network iterations 10000
show cpu-time-total epochs seed
set output-file iteration--40000.expr
inspect-sentences
set output-file iteration--40000.term
inspect-terms
set output-file stdout
save iteration--40000.save

set output-file iteration--50000.meta
train-network iterations 10000
show cpu-time-total epochs seed
set output-file iteration--50000.expr
inspect-sentences
set output-file iteration--50000.term
inspect-terms
set output-file stdout
save iteration--50000.save

set output-file iteration--60000.meta
train-network iterations 10000
show cpu-time-total epochs seed
set output-file iteration--60000.expr
inspect-sentences
set output-file iteration--60000.term
inspect-terms
set output-file stdout
save iteration--60000.save

set output-file iteration--70000.meta
train-network iterations 10000
show cpu-time-total epochs seed
set output-file iteration--70000.expr
inspect-sentences
set output-file iteration--70000.term
inspect-terms
set output-file stdout
save iteration--70000.save

set output-file iteration--80000.meta
train-network iterations 10000
show cpu-time-total epochs seed
set output-file iteration--80000.expr
inspect-sentences
set output-file iteration--80000.term
inspect-terms
set output-file stdout
save iteration--80000.save

set output-file iteration--90000.meta
train-network iterations 10000
show cpu-time-total epochs seed
set output-file iteration--90000.expr
inspect-sentences
set output-file iteration--90000.term
inspect-terms
set output-file stdout
save iteration--90000.save

set output-file iteration--100000.meta
train-network iterations 10000
show cpu-time-total epochs seed
set output-file iteration--100000.expr
inspect-sentences
set output-file iteration--100000.term
inspect-terms
set output-file stdout
save iteration--100000.save

set quiet 0
