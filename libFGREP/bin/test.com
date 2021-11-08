declare-term r1
declare-term t1
declare-term t2
declare-term t3
add-sentence r1 t1 t2
add-sentence r1 t1 t3
set learning-algorithm batch
