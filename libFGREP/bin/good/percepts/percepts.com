# Note that, here, relations are defined as fixed terms
declare-term is-approx 0.0 0.0 0.0 0.0 1.0 0.0

# First, we define the basic size-related terms,
declare-term size--1.0cm
declare-term size--5.0cm
declare-term size--10cm
declare-term size--30cm
declare-term size--50cm
declare-term size--1.0m
declare-term size--1.5m
declare-term size--2.0m
declare-term size--3.0m
declare-term size--5.0m
declare-term size--10m
declare-term size--20m
declare-term size--infinite


# next, we define the motion-related terms,
declare-term speed--stationary
declare-term speed--slow
declare-term speed--medium
declare-term speed--fast
declare-term motion--swaying
declare-term motion--crawling
declare-term motion--walking
declare-term motion--running
declare-term motion--bouncing


# and finally, the colours...
declare-term colour--white
declare-term colour--grey
declare-term colour--black
declare-term colour--pink
declare-term colour--red
declare-term colour--dark-red
declare-term colour--light-orange
declare-term colour--orange
declare-term colour--brown
declare-term colour--light-yellow
declare-term colour--yellow
declare-term colour--dark-yellow
declare-term colour--light-green
declare-term colour--green
declare-term colour--dark-green
declare-term colour--light-cyan
declare-term colour--cyan
declare-term colour--dark-cyan
declare-term colour--light-blue
declare-term colour--blue
declare-term colour--dark-blue
declare-term colour--light-magenta
declare-term colour--magenta
declare-term colour--dark-magenta


# sentence declarations
add-sentence is-approx size--1.0cm size--5.0cm
add-sentence is-approx size--1.0cm size--10cm 0.5
add-sentence is-approx size--5.0cm size--1.0cm
add-sentence is-approx size--5.0cm size--10cm
add-sentence is-approx size--5.0cm size--30cm 0.5
add-sentence is-approx size--10cm size--1.0cm 0.5
add-sentence is-approx size--10cm size--5.0cm
add-sentence is-approx size--10cm size--30cm
add-sentence is-approx size--10cm size--50cm 0.5
add-sentence is-approx size--30cm size--5.0cm 0.5
add-sentence is-approx size--30cm size--10cm
add-sentence is-approx size--30cm size--50cm
add-sentence is-approx size--30cm size--1.0m 0.5
add-sentence is-approx size--50cm size--10cm 0.5
add-sentence is-approx size--50cm size--30cm
add-sentence is-approx size--50cm size--1.0m
add-sentence is-approx size--50cm size--1.5m 0.5
add-sentence is-approx size--1.0m size--30cm 0.5
add-sentence is-approx size--1.0m size--50cm
add-sentence is-approx size--1.0m size--1.5m
add-sentence is-approx size--1.0m size--2.0m 0.5
add-sentence is-approx size--1.5m size--50cm 0.5
add-sentence is-approx size--1.5m size--1.0m
add-sentence is-approx size--1.5m size--2.0m
add-sentence is-approx size--1.5m size--3.0m 0.5
add-sentence is-approx size--2.0m size--1.0m 0.5
add-sentence is-approx size--2.0m size--1.5m
add-sentence is-approx size--2.0m size--3.0m
add-sentence is-approx size--2.0m size--5.0m 0.5
add-sentence is-approx size--3.0m size--1.5m 0.5
add-sentence is-approx size--3.0m size--2.0m
add-sentence is-approx size--3.0m size--5.0m
add-sentence is-approx size--3.0m size--10m 0.5
add-sentence is-approx size--5.0m size--2.0m 0.5
add-sentence is-approx size--5.0m size--3.0m
add-sentence is-approx size--5.0m size--10m
add-sentence is-approx size--5.0m size--20m 0.5
add-sentence is-approx size--10m size--3.0m 0.5
add-sentence is-approx size--10m size--5.0m
add-sentence is-approx size--10m size--20m
add-sentence is-approx size--10m size--infinite 0.1
add-sentence is-approx size--20m size--5.0m 0.5
add-sentence is-approx size--20m size--10m
add-sentence is-approx size--20m size--infinite 0.25
add-sentence is-approx size--infinite size--10m 0.1
add-sentence is-approx size--infinite size--20m 0.25


add-sentence is-approx speed--stationary speed--slow 0.5
add-sentence is-approx speed--slow speed--stationary 0.5
add-sentence is-approx speed--slow speed--medium
add-sentence is-approx speed--medium speed--slow
add-sentence is-approx speed--medium speed--fast
add-sentence is-approx speed--fast speed--medium

add-sentence is-approx motion--swaying speed--stationary 0.5
add-sentence is-approx speed--stationary motion--swaying 0.5
add-sentence is-approx motion--crawling speed--slow
add-sentence is-approx motion--crawling motion--walking
add-sentence is-approx motion--crawling motion--running 0.25
add-sentence is-approx motion--walking motion--crawling
add-sentence is-approx motion--walking motion--running
add-sentence is-approx motion--running motion--walking
add-sentence is-approx motion--walking motion--bouncing 0.5
add-sentence is-approx motion--running motion--crawling 0.25
add-sentence is-approx motion--running motion--walking
add-sentence is-approx motion--running motion--bouncing


add-sentence is-approx colour--white colour--grey
add-sentence is-approx colour--grey colour--white
add-sentence is-approx colour--grey colour--black
add-sentence is-approx colour--black colour--grey

add-sentence is-approx colour--pink colour--white
add-sentence is-approx colour--pink colour--red
add-sentence is-approx colour--pink colour--light-magenta
add-sentence is-approx colour--pink colour--light-orange
add-sentence is-approx colour--red colour--pink
add-sentence is-approx colour--red colour--dark-red
add-sentence is-approx colour--red colour--magenta
add-sentence is-approx colour--red colour--orange
add-sentence is-approx colour--dark-red colour--red
add-sentence is-approx colour--dark-red colour--black
add-sentence is-approx colour--dark-red colour--dark-magenta
add-sentence is-approx colour--dark-red colour--brown

add-sentence is-approx colour--light-orange colour--white
add-sentence is-approx colour--light-orange colour--orange
add-sentence is-approx colour--light-orange colour--pink
add-sentence is-approx colour--light-orange colour--light-yellow
add-sentence is-approx colour--orange colour--light-orange
add-sentence is-approx colour--orange colour--brown
add-sentence is-approx colour--orange colour--red
add-sentence is-approx colour--orange colour--yellow
add-sentence is-approx colour--brown colour--orange
add-sentence is-approx colour--brown colour--black
add-sentence is-approx colour--brown colour--dark-red
add-sentence is-approx colour--brown colour--dark-yellow

add-sentence is-approx colour--light-yellow colour--white
add-sentence is-approx colour--light-yellow colour--yellow
add-sentence is-approx colour--light-yellow colour--light-orange
add-sentence is-approx colour--light-yellow colour--light-green
add-sentence is-approx colour--yellow colour--light-yellow
add-sentence is-approx colour--yellow colour--dark-yellow
add-sentence is-approx colour--yellow colour--orange
add-sentence is-approx colour--yellow colour--green
add-sentence is-approx colour--dark-yellow colour--yellow
add-sentence is-approx colour--dark-yellow colour--black
add-sentence is-approx colour--dark-yellow colour--brown
add-sentence is-approx colour--dark-yellow colour--dark-green

add-sentence is-approx colour--light-green colour--white
add-sentence is-approx colour--light-green colour--green
add-sentence is-approx colour--light-green colour--light-yellow
add-sentence is-approx colour--light-green colour--light-cyan
add-sentence is-approx colour--green colour--light-green
add-sentence is-approx colour--green colour--dark-green
add-sentence is-approx colour--green colour--yellow
add-sentence is-approx colour--green colour--cyan
add-sentence is-approx colour--dark-green colour--green
add-sentence is-approx colour--dark-green colour--black
add-sentence is-approx colour--dark-green colour--dark-yellow
add-sentence is-approx colour--dark-green colour--dark-cyan

add-sentence is-approx colour--light-cyan colour--white
add-sentence is-approx colour--light-cyan colour--cyan
add-sentence is-approx colour--light-cyan colour--light-green
add-sentence is-approx colour--light-cyan colour--light-blue
add-sentence is-approx colour--cyan colour--light-cyan
add-sentence is-approx colour--cyan colour--dark-cyan
add-sentence is-approx colour--cyan colour--green
add-sentence is-approx colour--cyan colour--blue
add-sentence is-approx colour--dark-cyan colour--cyan
add-sentence is-approx colour--dark-cyan colour--black
add-sentence is-approx colour--dark-cyan colour--dark-green
add-sentence is-approx colour--dark-cyan colour--dark-blue

add-sentence is-approx colour--light-blue colour--white
add-sentence is-approx colour--light-blue colour--blue
add-sentence is-approx colour--light-blue colour--light-cyan
add-sentence is-approx colour--light-blue colour--light-magenta
add-sentence is-approx colour--blue colour--light-blue
add-sentence is-approx colour--blue colour--dark-blue
add-sentence is-approx colour--blue colour--cyan
add-sentence is-approx colour--blue colour--magenta
add-sentence is-approx colour--dark-blue colour--blue
add-sentence is-approx colour--dark-blue colour--black
add-sentence is-approx colour--dark-blue colour--dark-cyan
add-sentence is-approx colour--dark-blue colour--dark-magenta

add-sentence is-approx colour--light-magenta colour--white
add-sentence is-approx colour--light-magenta colour--magenta
add-sentence is-approx colour--light-magenta colour--light-blue
add-sentence is-approx colour--light-magenta colour--pink
add-sentence is-approx colour--magenta colour--light-magenta
add-sentence is-approx colour--magenta colour--dark-magenta
add-sentence is-approx colour--magenta colour--blue
add-sentence is-approx colour--magenta colour--red
add-sentence is-approx colour--dark-magenta colour--magenta
add-sentence is-approx colour--dark-magenta colour--black
add-sentence is-approx colour--dark-magenta colour--dark-blue
add-sentence is-approx colour--dark-magenta colour--dark-red


save colours.save
set quiet 1
set output-file colours.data
inspect-term
set output-file stdout
set quiet 0

train-network error 0.001
show cpu-time-last
save colours--0.001.save
set quiet 1
set output-file colours--0.001.data
inspect-term
set output-file stdout
set quiet 0

set learning-rate 0.05
train-network error 0.00001
show cpu-time-last
save colours--0.00001.save
set quiet 1
set output-file colours--0.00001.data
inspect-term
set output-file stdout
set quiet 0

train-network error 0.000001
show cpu-time-last
save colours--0.000001.save
set quiet 1
set output-file colours--0.000001.data
inspect-term
set output-file stdout
set quiet 0
