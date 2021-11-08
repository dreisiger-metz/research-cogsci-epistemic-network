# Note that, here, relations are defined as fixed terms
declare-term is     1.0 0.0
declare-term can    0.0 1.0 0.0
declare-term has    0.0 0.0 1 0.0
declare-term action 0.0 0.0 0.0 1 0.0

declare-term colour-red
declare-term colour-orange
declare-term colour-yellow
declare-term colour-green
declare-term colour-cyan
declare-term colour-blue
declare-term colour-magenta

declare-term hue-000
declare-term hue-010
declare-term hue-020
declare-term hue-030
declare-term hue-040
declare-term hue-050
declare-term hue-060
declare-term hue-070
declare-term hue-080
declare-term hue-090
declare-term hue-100
declare-term hue-110
declare-term hue-120
declare-term hue-130
declare-term hue-140
declare-term hue-150
declare-term hue-160
declare-term hue-170
declare-term hue-180
declare-term hue-190
declare-term hue-200
declare-term hue-210
declare-term hue-220
declare-term hue-230
declare-term hue-240
declare-term hue-250
declare-term hue-260
declare-term hue-270
declare-term hue-280
declare-term hue-290
declare-term hue-300
declare-term hue-310
declare-term hue-320
declare-term hue-330
declare-term hue-340
declare-term hue-350
declare-term hue-360


declare-term hue-warm
declare-term hue-cool


# sentence declarations
add-sentence is colour-red hue-320 0.75
add-sentence is colour-red hue-330 0.90
add-sentence is colour-red hue-340
add-sentence is colour-red hue-350
add-sentence is colour-red hue-360
add-sentence is colour-red hue-000
add-sentence is colour-red hue-010
add-sentence is colour-red hue-020 0.9
add-sentence is colour-red hue-030 0.75
add-sentence is colour-orange hue-010 0.75
add-sentence is colour-orange hue-020 0.90
add-sentence is colour-orange hue-030
add-sentence is colour-orange hue-040
add-sentence is colour-orange hue-050 0.90
add-sentence is colour-orange hue-060 0.75
add-sentence is colour-yellow hue-040 0.75
add-sentence is colour-yellow hue-050 0.90
add-sentence is colour-yellow hue-060
add-sentence is colour-yellow hue-070
add-sentence is colour-yellow hue-080 0.90
add-sentence is colour-yellow hue-090 0.75
add-sentence is colour-green hue-080 0.75
add-sentence is colour-green hue-090 0.90
add-sentence is colour-green hue-100
add-sentence is colour-green hue-110
add-sentence is colour-green hue-120
add-sentence is colour-green hue-130
add-sentence is colour-green hue-140 0.90
add-sentence is colour-green hue-150 0.75
add-sentence is colour-cyan hue-140 0.75
add-sentence is colour-cyan hue-150 0.90
add-sentence is colour-cyan hue-160
add-sentence is colour-cyan hue-170
add-sentence is colour-cyan hue-180
add-sentence is colour-cyan hue-190 0.90
add-sentence is colour-cyan hue-200 0.75
add-sentence is colour-blue hue-190 0.75
add-sentence is colour-blue hue-200 0.90
add-sentence is colour-blue hue-210
add-sentence is colour-blue hue-220
add-sentence is colour-blue hue-230
add-sentence is colour-blue hue-240
add-sentence is colour-blue hue-250
add-sentence is colour-blue hue-260 0.90
add-sentence is colour-blue hue-270 0.75
add-sentence is colour-magenta hue-260 0.75
add-sentence is colour-magenta hue-270 0.90
add-sentence is colour-magenta hue-280
add-sentence is colour-magenta hue-290
add-sentence is colour-magenta hue-300
add-sentence is colour-magenta hue-310
add-sentence is colour-magenta hue-320 0.75
add-sentence is colour-magenta hue-330 0.90

train-network 0.0001
set learning-rate 0.05
train-network 0.00001
inspect-term
