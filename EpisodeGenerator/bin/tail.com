# First we initialise the learning and output parameters,
set learning-algorithm batch
set learning-rate 0.1
set print-errors 1


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
set output-file iteration--500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--500.expr
inspect-sentences
set output-file iteration--500.term
inspect-terms
set output-file stdout
save iteration--500.save

set output-file iteration--1000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--1000.expr
inspect-sentences
set output-file iteration--1000.term
inspect-terms
set output-file stdout
save iteration--1000.save

set output-file iteration--1500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--1500.expr
inspect-sentences
set output-file iteration--1500.term
inspect-terms
set output-file stdout
save iteration--1500.save

set output-file iteration--2000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--2000.expr
inspect-sentences
set output-file iteration--2000.term
inspect-terms
set output-file stdout
save iteration--2000.save

set output-file iteration--2500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--2500.expr
inspect-sentences
set output-file iteration--2500.term
inspect-terms
set output-file stdout
save iteration--2500.save

set output-file iteration--3000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--3000.expr
inspect-sentences
set output-file iteration--3000.term
inspect-terms
set output-file stdout
save iteration--3000.save

set output-file iteration--3500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--3500.expr
inspect-sentences
set output-file iteration--3500.term
inspect-terms
set output-file stdout
save iteration--3500.save

set output-file iteration--4000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--4000.expr
inspect-sentences
set output-file iteration--4000.term
inspect-terms
set output-file stdout
save iteration--4000.save

set output-file iteration--4500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--4500.expr
inspect-sentences
set output-file iteration--4500.term
inspect-terms
set output-file stdout
save iteration--4500.save

set output-file iteration--5000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--5000.expr
inspect-sentences
set output-file iteration--5000.term
inspect-terms
set output-file stdout
save iteration--5000.save

set output-file iteration--5500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--5500.expr
inspect-sentences
set output-file iteration--5500.term
inspect-terms
set output-file stdout
save iteration--5500.save

set output-file iteration--6000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--6000.expr
inspect-sentences
set output-file iteration--6000.term
inspect-terms
set output-file stdout
save iteration--6000.save

set output-file iteration--6500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--6500.expr
inspect-sentences
set output-file iteration--6500.term
inspect-terms
set output-file stdout
save iteration--6500.save

set output-file iteration--7000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--7000.expr
inspect-sentences
set output-file iteration--7000.term
inspect-terms
set output-file stdout
save iteration--7000.save

set output-file iteration--7500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--7500.expr
inspect-sentences
set output-file iteration--7500.term
inspect-terms
set output-file stdout
save iteration--7500.save

set output-file iteration--8000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--8000.expr
inspect-sentences
set output-file iteration--8000.term
inspect-terms
set output-file stdout
save iteration--8000.save

set output-file iteration--8500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--8500.expr
inspect-sentences
set output-file iteration--8500.term
inspect-terms
set output-file stdout
save iteration--8500.save

set output-file iteration--9000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--9000.expr
inspect-sentences
set output-file iteration--9000.term
inspect-terms
set output-file stdout
save iteration--9000.save

set output-file iteration--9500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--9500.expr
inspect-sentences
set output-file iteration--9500.term
inspect-terms
set output-file stdout
save iteration--9500.save

set output-file iteration--10000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--10000.expr
inspect-sentences
set output-file iteration--10000.term
inspect-terms
set output-file stdout
save iteration--10000.save

set output-file iteration--10500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--10500.expr
inspect-sentences
set output-file iteration--10500.term
inspect-terms
set output-file stdout
save iteration--10500.save

set output-file iteration--11000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--11000.expr
inspect-sentences
set output-file iteration--11000.term
inspect-terms
set output-file stdout
save iteration--11000.save

set output-file iteration--11500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--11500.expr
inspect-sentences
set output-file iteration--11500.term
inspect-terms
set output-file stdout
save iteration--11500.save

set output-file iteration--12000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--12000.expr
inspect-sentences
set output-file iteration--12000.term
inspect-terms
set output-file stdout
save iteration--12000.save

set output-file iteration--12500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--12500.expr
inspect-sentences
set output-file iteration--12500.term
inspect-terms
set output-file stdout
save iteration--12500.save

set output-file iteration--13000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--13000.expr
inspect-sentences
set output-file iteration--13000.term
inspect-terms
set output-file stdout
save iteration--13000.save

set output-file iteration--13500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--13500.expr
inspect-sentences
set output-file iteration--13500.term
inspect-terms
set output-file stdout
save iteration--13500.save

set output-file iteration--14000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--14000.expr
inspect-sentences
set output-file iteration--14000.term
inspect-terms
set output-file stdout
save iteration--14000.save

set output-file iteration--14500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--14500.expr
inspect-sentences
set output-file iteration--14500.term
inspect-terms
set output-file stdout
save iteration--14500.save

set output-file iteration--15000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--15000.expr
inspect-sentences
set output-file iteration--15000.term
inspect-terms
set output-file stdout
save iteration--15000.save

set output-file iteration--15500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--15500.expr
inspect-sentences
set output-file iteration--15500.term
inspect-terms
set output-file stdout
save iteration--15500.save

set output-file iteration--16000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--16000.expr
inspect-sentences
set output-file iteration--16000.term
inspect-terms
set output-file stdout
save iteration--16000.save

set output-file iteration--16500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--16500.expr
inspect-sentences
set output-file iteration--16500.term
inspect-terms
set output-file stdout
save iteration--16500.save

set output-file iteration--17000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--17000.expr
inspect-sentences
set output-file iteration--17000.term
inspect-terms
set output-file stdout
save iteration--17000.save

set output-file iteration--17500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--17500.expr
inspect-sentences
set output-file iteration--17500.term
inspect-terms
set output-file stdout
save iteration--17500.save

set output-file iteration--18000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--18000.expr
inspect-sentences
set output-file iteration--18000.term
inspect-terms
set output-file stdout
save iteration--18000.save

set output-file iteration--18500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--18500.expr
inspect-sentences
set output-file iteration--18500.term
inspect-terms
set output-file stdout
save iteration--18500.save

set output-file iteration--19000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--19000.expr
inspect-sentences
set output-file iteration--19000.term
inspect-terms
set output-file stdout
save iteration--19000.save

set output-file iteration--19500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--19500.expr
inspect-sentences
set output-file iteration--19500.term
inspect-terms
set output-file stdout
save iteration--19500.save

set output-file iteration--20000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--20000.expr
inspect-sentences
set output-file iteration--20000.term
inspect-terms
set output-file stdout
save iteration--20000.save

set output-file iteration--20500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--20500.expr
inspect-sentences
set output-file iteration--20500.term
inspect-terms
set output-file stdout
save iteration--20500.save

set output-file iteration--21000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--21000.expr
inspect-sentences
set output-file iteration--21000.term
inspect-terms
set output-file stdout
save iteration--21000.save

set output-file iteration--21500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--21500.expr
inspect-sentences
set output-file iteration--21500.term
inspect-terms
set output-file stdout
save iteration--21500.save

set output-file iteration--22000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--22000.expr
inspect-sentences
set output-file iteration--22000.term
inspect-terms
set output-file stdout
save iteration--22000.save

set output-file iteration--22500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--22500.expr
inspect-sentences
set output-file iteration--22500.term
inspect-terms
set output-file stdout
save iteration--22500.save

set output-file iteration--23000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--23000.expr
inspect-sentences
set output-file iteration--23000.term
inspect-terms
set output-file stdout
save iteration--23000.save

set output-file iteration--23500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--23500.expr
inspect-sentences
set output-file iteration--23500.term
inspect-terms
set output-file stdout
save iteration--23500.save

set output-file iteration--24000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--24000.expr
inspect-sentences
set output-file iteration--24000.term
inspect-terms
set output-file stdout
save iteration--24000.save

set output-file iteration--24500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--24500.expr
inspect-sentences
set output-file iteration--24500.term
inspect-terms
set output-file stdout
save iteration--24500.save

set output-file iteration--25000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--25000.expr
inspect-sentences
set output-file iteration--25000.term
inspect-terms
set output-file stdout
save iteration--25000.save

set output-file iteration--25500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--25500.expr
inspect-sentences
set output-file iteration--25500.term
inspect-terms
set output-file stdout
save iteration--25500.save

set output-file iteration--26000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--26000.expr
inspect-sentences
set output-file iteration--26000.term
inspect-terms
set output-file stdout
save iteration--26000.save

set output-file iteration--26500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--26500.expr
inspect-sentences
set output-file iteration--26500.term
inspect-terms
set output-file stdout
save iteration--26500.save

set output-file iteration--27000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--27000.expr
inspect-sentences
set output-file iteration--27000.term
inspect-terms
set output-file stdout
save iteration--27000.save

set output-file iteration--27500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--27500.expr
inspect-sentences
set output-file iteration--27500.term
inspect-terms
set output-file stdout
save iteration--27500.save

set output-file iteration--28000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--28000.expr
inspect-sentences
set output-file iteration--28000.term
inspect-terms
set output-file stdout
save iteration--28000.save

set output-file iteration--28500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--28500.expr
inspect-sentences
set output-file iteration--28500.term
inspect-terms
set output-file stdout
save iteration--28500.save

set output-file iteration--29000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--29000.expr
inspect-sentences
set output-file iteration--29000.term
inspect-terms
set output-file stdout
save iteration--29000.save

set output-file iteration--29500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--29500.expr
inspect-sentences
set output-file iteration--29500.term
inspect-terms
set output-file stdout
save iteration--29500.save

set output-file iteration--30000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--30000.expr
inspect-sentences
set output-file iteration--30000.term
inspect-terms
set output-file stdout
save iteration--30000.save

set output-file iteration--30500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--30500.expr
inspect-sentences
set output-file iteration--30500.term
inspect-terms
set output-file stdout
save iteration--30500.save

set output-file iteration--31000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--31000.expr
inspect-sentences
set output-file iteration--31000.term
inspect-terms
set output-file stdout
save iteration--31000.save

set output-file iteration--31500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--31500.expr
inspect-sentences
set output-file iteration--31500.term
inspect-terms
set output-file stdout
save iteration--31500.save

set output-file iteration--32000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--32000.expr
inspect-sentences
set output-file iteration--32000.term
inspect-terms
set output-file stdout
save iteration--32000.save

set output-file iteration--32500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--32500.expr
inspect-sentences
set output-file iteration--32500.term
inspect-terms
set output-file stdout
save iteration--32500.save

set output-file iteration--33000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--33000.expr
inspect-sentences
set output-file iteration--33000.term
inspect-terms
set output-file stdout
save iteration--33000.save

set output-file iteration--33500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--33500.expr
inspect-sentences
set output-file iteration--33500.term
inspect-terms
set output-file stdout
save iteration--33500.save

set output-file iteration--34000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--34000.expr
inspect-sentences
set output-file iteration--34000.term
inspect-terms
set output-file stdout
save iteration--34000.save

set output-file iteration--34500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--34500.expr
inspect-sentences
set output-file iteration--34500.term
inspect-terms
set output-file stdout
save iteration--34500.save

set output-file iteration--35000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--35000.expr
inspect-sentences
set output-file iteration--35000.term
inspect-terms
set output-file stdout
save iteration--35000.save

set output-file iteration--35500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--35500.expr
inspect-sentences
set output-file iteration--35500.term
inspect-terms
set output-file stdout
save iteration--35500.save

set output-file iteration--36000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--36000.expr
inspect-sentences
set output-file iteration--36000.term
inspect-terms
set output-file stdout
save iteration--36000.save

set output-file iteration--36500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--36500.expr
inspect-sentences
set output-file iteration--36500.term
inspect-terms
set output-file stdout
save iteration--36500.save

set output-file iteration--37000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--37000.expr
inspect-sentences
set output-file iteration--37000.term
inspect-terms
set output-file stdout
save iteration--37000.save

set output-file iteration--37500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--37500.expr
inspect-sentences
set output-file iteration--37500.term
inspect-terms
set output-file stdout
save iteration--37500.save

set output-file iteration--38000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--38000.expr
inspect-sentences
set output-file iteration--38000.term
inspect-terms
set output-file stdout
save iteration--38000.save

set output-file iteration--38500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--38500.expr
inspect-sentences
set output-file iteration--38500.term
inspect-terms
set output-file stdout
save iteration--38500.save

set output-file iteration--39000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--39000.expr
inspect-sentences
set output-file iteration--39000.term
inspect-terms
set output-file stdout
save iteration--39000.save

set output-file iteration--39500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--39500.expr
inspect-sentences
set output-file iteration--39500.term
inspect-terms
set output-file stdout
save iteration--39500.save

set output-file iteration--40000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--40000.expr
inspect-sentences
set output-file iteration--40000.term
inspect-terms
set output-file stdout
save iteration--40000.save

set output-file iteration--40500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--40500.expr
inspect-sentences
set output-file iteration--40500.term
inspect-terms
set output-file stdout
save iteration--40500.save

set output-file iteration--41000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--41000.expr
inspect-sentences
set output-file iteration--41000.term
inspect-terms
set output-file stdout
save iteration--41000.save

set output-file iteration--41500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--41500.expr
inspect-sentences
set output-file iteration--41500.term
inspect-terms
set output-file stdout
save iteration--41500.save

set output-file iteration--42000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--42000.expr
inspect-sentences
set output-file iteration--42000.term
inspect-terms
set output-file stdout
save iteration--42000.save

set output-file iteration--42500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--42500.expr
inspect-sentences
set output-file iteration--42500.term
inspect-terms
set output-file stdout
save iteration--42500.save

set output-file iteration--43000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--43000.expr
inspect-sentences
set output-file iteration--43000.term
inspect-terms
set output-file stdout
save iteration--43000.save

set output-file iteration--43500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--43500.expr
inspect-sentences
set output-file iteration--43500.term
inspect-terms
set output-file stdout
save iteration--43500.save

set output-file iteration--44000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--44000.expr
inspect-sentences
set output-file iteration--44000.term
inspect-terms
set output-file stdout
save iteration--44000.save

set output-file iteration--44500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--44500.expr
inspect-sentences
set output-file iteration--44500.term
inspect-terms
set output-file stdout
save iteration--44500.save

set output-file iteration--45000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--45000.expr
inspect-sentences
set output-file iteration--45000.term
inspect-terms
set output-file stdout
save iteration--45000.save

set output-file iteration--45500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--45500.expr
inspect-sentences
set output-file iteration--45500.term
inspect-terms
set output-file stdout
save iteration--45500.save

set output-file iteration--46000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--46000.expr
inspect-sentences
set output-file iteration--46000.term
inspect-terms
set output-file stdout
save iteration--46000.save

set output-file iteration--46500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--46500.expr
inspect-sentences
set output-file iteration--46500.term
inspect-terms
set output-file stdout
save iteration--46500.save

set output-file iteration--47000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--47000.expr
inspect-sentences
set output-file iteration--47000.term
inspect-terms
set output-file stdout
save iteration--47000.save

set output-file iteration--47500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--47500.expr
inspect-sentences
set output-file iteration--47500.term
inspect-terms
set output-file stdout
save iteration--47500.save

set output-file iteration--48000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--48000.expr
inspect-sentences
set output-file iteration--48000.term
inspect-terms
set output-file stdout
save iteration--48000.save

set output-file iteration--48500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--48500.expr
inspect-sentences
set output-file iteration--48500.term
inspect-terms
set output-file stdout
save iteration--48500.save

set output-file iteration--49000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--49000.expr
inspect-sentences
set output-file iteration--49000.term
inspect-terms
set output-file stdout
save iteration--49000.save

set output-file iteration--49500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--49500.expr
inspect-sentences
set output-file iteration--49500.term
inspect-terms
set output-file stdout
save iteration--49500.save

set output-file iteration--50000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--50000.expr
inspect-sentences
set output-file iteration--50000.term
inspect-terms
set output-file stdout
save iteration--50000.save

set output-file iteration--50500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--50500.expr
inspect-sentences
set output-file iteration--50500.term
inspect-terms
set output-file stdout
save iteration--50500.save

set output-file iteration--51000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--51000.expr
inspect-sentences
set output-file iteration--51000.term
inspect-terms
set output-file stdout
save iteration--51000.save

set output-file iteration--51500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--51500.expr
inspect-sentences
set output-file iteration--51500.term
inspect-terms
set output-file stdout
save iteration--51500.save

set output-file iteration--52000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--52000.expr
inspect-sentences
set output-file iteration--52000.term
inspect-terms
set output-file stdout
save iteration--52000.save

set output-file iteration--52500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--52500.expr
inspect-sentences
set output-file iteration--52500.term
inspect-terms
set output-file stdout
save iteration--52500.save

set output-file iteration--53000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--53000.expr
inspect-sentences
set output-file iteration--53000.term
inspect-terms
set output-file stdout
save iteration--53000.save

set output-file iteration--53500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--53500.expr
inspect-sentences
set output-file iteration--53500.term
inspect-terms
set output-file stdout
save iteration--53500.save

set output-file iteration--54000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--54000.expr
inspect-sentences
set output-file iteration--54000.term
inspect-terms
set output-file stdout
save iteration--54000.save

set output-file iteration--54500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--54500.expr
inspect-sentences
set output-file iteration--54500.term
inspect-terms
set output-file stdout
save iteration--54500.save

set output-file iteration--55000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--55000.expr
inspect-sentences
set output-file iteration--55000.term
inspect-terms
set output-file stdout
save iteration--55000.save

set output-file iteration--55500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--55500.expr
inspect-sentences
set output-file iteration--55500.term
inspect-terms
set output-file stdout
save iteration--55500.save

set output-file iteration--56000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--56000.expr
inspect-sentences
set output-file iteration--56000.term
inspect-terms
set output-file stdout
save iteration--56000.save

set output-file iteration--56500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--56500.expr
inspect-sentences
set output-file iteration--56500.term
inspect-terms
set output-file stdout
save iteration--56500.save

set output-file iteration--57000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--57000.expr
inspect-sentences
set output-file iteration--57000.term
inspect-terms
set output-file stdout
save iteration--57000.save

set output-file iteration--57500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--57500.expr
inspect-sentences
set output-file iteration--57500.term
inspect-terms
set output-file stdout
save iteration--57500.save

set output-file iteration--58000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--58000.expr
inspect-sentences
set output-file iteration--58000.term
inspect-terms
set output-file stdout
save iteration--58000.save

set output-file iteration--58500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--58500.expr
inspect-sentences
set output-file iteration--58500.term
inspect-terms
set output-file stdout
save iteration--58500.save

set output-file iteration--59000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--59000.expr
inspect-sentences
set output-file iteration--59000.term
inspect-terms
set output-file stdout
save iteration--59000.save

set output-file iteration--59500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--59500.expr
inspect-sentences
set output-file iteration--59500.term
inspect-terms
set output-file stdout
save iteration--59500.save

set output-file iteration--60000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--60000.expr
inspect-sentences
set output-file iteration--60000.term
inspect-terms
set output-file stdout
save iteration--60000.save

set output-file iteration--60500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--60500.expr
inspect-sentences
set output-file iteration--60500.term
inspect-terms
set output-file stdout
save iteration--60500.save

set output-file iteration--61000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--61000.expr
inspect-sentences
set output-file iteration--61000.term
inspect-terms
set output-file stdout
save iteration--61000.save

set output-file iteration--61500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--61500.expr
inspect-sentences
set output-file iteration--61500.term
inspect-terms
set output-file stdout
save iteration--61500.save

set output-file iteration--62000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--62000.expr
inspect-sentences
set output-file iteration--62000.term
inspect-terms
set output-file stdout
save iteration--62000.save

set output-file iteration--62500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--62500.expr
inspect-sentences
set output-file iteration--62500.term
inspect-terms
set output-file stdout
save iteration--62500.save

set output-file iteration--63000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--63000.expr
inspect-sentences
set output-file iteration--63000.term
inspect-terms
set output-file stdout
save iteration--63000.save

set output-file iteration--63500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--63500.expr
inspect-sentences
set output-file iteration--63500.term
inspect-terms
set output-file stdout
save iteration--63500.save

set output-file iteration--64000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--64000.expr
inspect-sentences
set output-file iteration--64000.term
inspect-terms
set output-file stdout
save iteration--64000.save

set output-file iteration--64500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--64500.expr
inspect-sentences
set output-file iteration--64500.term
inspect-terms
set output-file stdout
save iteration--64500.save

set output-file iteration--65000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--65000.expr
inspect-sentences
set output-file iteration--65000.term
inspect-terms
set output-file stdout
save iteration--65000.save

set output-file iteration--65500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--65500.expr
inspect-sentences
set output-file iteration--65500.term
inspect-terms
set output-file stdout
save iteration--65500.save

set output-file iteration--66000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--66000.expr
inspect-sentences
set output-file iteration--66000.term
inspect-terms
set output-file stdout
save iteration--66000.save

set output-file iteration--66500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--66500.expr
inspect-sentences
set output-file iteration--66500.term
inspect-terms
set output-file stdout
save iteration--66500.save

set output-file iteration--67000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--67000.expr
inspect-sentences
set output-file iteration--67000.term
inspect-terms
set output-file stdout
save iteration--67000.save

set output-file iteration--67500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--67500.expr
inspect-sentences
set output-file iteration--67500.term
inspect-terms
set output-file stdout
save iteration--67500.save

set output-file iteration--68000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--68000.expr
inspect-sentences
set output-file iteration--68000.term
inspect-terms
set output-file stdout
save iteration--68000.save

set output-file iteration--68500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--68500.expr
inspect-sentences
set output-file iteration--68500.term
inspect-terms
set output-file stdout
save iteration--68500.save

set output-file iteration--69000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--69000.expr
inspect-sentences
set output-file iteration--69000.term
inspect-terms
set output-file stdout
save iteration--69000.save

set output-file iteration--69500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--69500.expr
inspect-sentences
set output-file iteration--69500.term
inspect-terms
set output-file stdout
save iteration--69500.save

set output-file iteration--70000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--70000.expr
inspect-sentences
set output-file iteration--70000.term
inspect-terms
set output-file stdout
save iteration--70000.save

set output-file iteration--70500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--70500.expr
inspect-sentences
set output-file iteration--70500.term
inspect-terms
set output-file stdout
save iteration--70500.save

set output-file iteration--71000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--71000.expr
inspect-sentences
set output-file iteration--71000.term
inspect-terms
set output-file stdout
save iteration--71000.save

set output-file iteration--71500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--71500.expr
inspect-sentences
set output-file iteration--71500.term
inspect-terms
set output-file stdout
save iteration--71500.save

set output-file iteration--72000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--72000.expr
inspect-sentences
set output-file iteration--72000.term
inspect-terms
set output-file stdout
save iteration--72000.save

set output-file iteration--72500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--72500.expr
inspect-sentences
set output-file iteration--72500.term
inspect-terms
set output-file stdout
save iteration--72500.save

set output-file iteration--73000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--73000.expr
inspect-sentences
set output-file iteration--73000.term
inspect-terms
set output-file stdout
save iteration--73000.save

set output-file iteration--73500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--73500.expr
inspect-sentences
set output-file iteration--73500.term
inspect-terms
set output-file stdout
save iteration--73500.save

set output-file iteration--74000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--74000.expr
inspect-sentences
set output-file iteration--74000.term
inspect-terms
set output-file stdout
save iteration--74000.save

set output-file iteration--74500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--74500.expr
inspect-sentences
set output-file iteration--74500.term
inspect-terms
set output-file stdout
save iteration--74500.save

set output-file iteration--75000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--75000.expr
inspect-sentences
set output-file iteration--75000.term
inspect-terms
set output-file stdout
save iteration--75000.save

set output-file iteration--75500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--75500.expr
inspect-sentences
set output-file iteration--75500.term
inspect-terms
set output-file stdout
save iteration--75500.save

set output-file iteration--76000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--76000.expr
inspect-sentences
set output-file iteration--76000.term
inspect-terms
set output-file stdout
save iteration--76000.save

set output-file iteration--76500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--76500.expr
inspect-sentences
set output-file iteration--76500.term
inspect-terms
set output-file stdout
save iteration--76500.save

set output-file iteration--77000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--77000.expr
inspect-sentences
set output-file iteration--77000.term
inspect-terms
set output-file stdout
save iteration--77000.save

set output-file iteration--77500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--77500.expr
inspect-sentences
set output-file iteration--77500.term
inspect-terms
set output-file stdout
save iteration--77500.save

set output-file iteration--78000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--78000.expr
inspect-sentences
set output-file iteration--78000.term
inspect-terms
set output-file stdout
save iteration--78000.save

set output-file iteration--78500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--78500.expr
inspect-sentences
set output-file iteration--78500.term
inspect-terms
set output-file stdout
save iteration--78500.save

set output-file iteration--79000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--79000.expr
inspect-sentences
set output-file iteration--79000.term
inspect-terms
set output-file stdout
save iteration--79000.save

set output-file iteration--79500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--79500.expr
inspect-sentences
set output-file iteration--79500.term
inspect-terms
set output-file stdout
save iteration--79500.save

set output-file iteration--80000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--80000.expr
inspect-sentences
set output-file iteration--80000.term
inspect-terms
set output-file stdout
save iteration--80000.save

set output-file iteration--80500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--80500.expr
inspect-sentences
set output-file iteration--80500.term
inspect-terms
set output-file stdout
save iteration--80500.save

set output-file iteration--81000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--81000.expr
inspect-sentences
set output-file iteration--81000.term
inspect-terms
set output-file stdout
save iteration--81000.save

set output-file iteration--81500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--81500.expr
inspect-sentences
set output-file iteration--81500.term
inspect-terms
set output-file stdout
save iteration--81500.save

set output-file iteration--82000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--82000.expr
inspect-sentences
set output-file iteration--82000.term
inspect-terms
set output-file stdout
save iteration--82000.save

set output-file iteration--82500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--82500.expr
inspect-sentences
set output-file iteration--82500.term
inspect-terms
set output-file stdout
save iteration--82500.save

set output-file iteration--83000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--83000.expr
inspect-sentences
set output-file iteration--83000.term
inspect-terms
set output-file stdout
save iteration--83000.save

set output-file iteration--83500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--83500.expr
inspect-sentences
set output-file iteration--83500.term
inspect-terms
set output-file stdout
save iteration--83500.save

set output-file iteration--84000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--84000.expr
inspect-sentences
set output-file iteration--84000.term
inspect-terms
set output-file stdout
save iteration--84000.save

set output-file iteration--84500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--84500.expr
inspect-sentences
set output-file iteration--84500.term
inspect-terms
set output-file stdout
save iteration--84500.save

set output-file iteration--85000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--85000.expr
inspect-sentences
set output-file iteration--85000.term
inspect-terms
set output-file stdout
save iteration--85000.save

set output-file iteration--85500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--85500.expr
inspect-sentences
set output-file iteration--85500.term
inspect-terms
set output-file stdout
save iteration--85500.save

set output-file iteration--86000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--86000.expr
inspect-sentences
set output-file iteration--86000.term
inspect-terms
set output-file stdout
save iteration--86000.save

set output-file iteration--86500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--86500.expr
inspect-sentences
set output-file iteration--86500.term
inspect-terms
set output-file stdout
save iteration--86500.save

set output-file iteration--87000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--87000.expr
inspect-sentences
set output-file iteration--87000.term
inspect-terms
set output-file stdout
save iteration--87000.save

set output-file iteration--87500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--87500.expr
inspect-sentences
set output-file iteration--87500.term
inspect-terms
set output-file stdout
save iteration--87500.save

set output-file iteration--88000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--88000.expr
inspect-sentences
set output-file iteration--88000.term
inspect-terms
set output-file stdout
save iteration--88000.save

set output-file iteration--88500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--88500.expr
inspect-sentences
set output-file iteration--88500.term
inspect-terms
set output-file stdout
save iteration--88500.save

set output-file iteration--89000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--89000.expr
inspect-sentences
set output-file iteration--89000.term
inspect-terms
set output-file stdout
save iteration--89000.save

set output-file iteration--89500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--89500.expr
inspect-sentences
set output-file iteration--89500.term
inspect-terms
set output-file stdout
save iteration--89500.save

set output-file iteration--90000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--90000.expr
inspect-sentences
set output-file iteration--90000.term
inspect-terms
set output-file stdout
save iteration--90000.save

set output-file iteration--90500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--90500.expr
inspect-sentences
set output-file iteration--90500.term
inspect-terms
set output-file stdout
save iteration--90500.save

set output-file iteration--91000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--91000.expr
inspect-sentences
set output-file iteration--91000.term
inspect-terms
set output-file stdout
save iteration--91000.save

set output-file iteration--91500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--91500.expr
inspect-sentences
set output-file iteration--91500.term
inspect-terms
set output-file stdout
save iteration--91500.save

set output-file iteration--92000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--92000.expr
inspect-sentences
set output-file iteration--92000.term
inspect-terms
set output-file stdout
save iteration--92000.save

set output-file iteration--92500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--92500.expr
inspect-sentences
set output-file iteration--92500.term
inspect-terms
set output-file stdout
save iteration--92500.save

set output-file iteration--93000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--93000.expr
inspect-sentences
set output-file iteration--93000.term
inspect-terms
set output-file stdout
save iteration--93000.save

set output-file iteration--93500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--93500.expr
inspect-sentences
set output-file iteration--93500.term
inspect-terms
set output-file stdout
save iteration--93500.save

set output-file iteration--94000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--94000.expr
inspect-sentences
set output-file iteration--94000.term
inspect-terms
set output-file stdout
save iteration--94000.save

set output-file iteration--94500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--94500.expr
inspect-sentences
set output-file iteration--94500.term
inspect-terms
set output-file stdout
save iteration--94500.save

set output-file iteration--95000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--95000.expr
inspect-sentences
set output-file iteration--95000.term
inspect-terms
set output-file stdout
save iteration--95000.save

set output-file iteration--95500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--95500.expr
inspect-sentences
set output-file iteration--95500.term
inspect-terms
set output-file stdout
save iteration--95500.save

set output-file iteration--96000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--96000.expr
inspect-sentences
set output-file iteration--96000.term
inspect-terms
set output-file stdout
save iteration--96000.save

set output-file iteration--96500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--96500.expr
inspect-sentences
set output-file iteration--96500.term
inspect-terms
set output-file stdout
save iteration--96500.save

set output-file iteration--97000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--97000.expr
inspect-sentences
set output-file iteration--97000.term
inspect-terms
set output-file stdout
save iteration--97000.save

set output-file iteration--97500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--97500.expr
inspect-sentences
set output-file iteration--97500.term
inspect-terms
set output-file stdout
save iteration--97500.save

set output-file iteration--98000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--98000.expr
inspect-sentences
set output-file iteration--98000.term
inspect-terms
set output-file stdout
save iteration--98000.save

set output-file iteration--98500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--98500.expr
inspect-sentences
set output-file iteration--98500.term
inspect-terms
set output-file stdout
save iteration--98500.save

set output-file iteration--99000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--99000.expr
inspect-sentences
set output-file iteration--99000.term
inspect-terms
set output-file stdout
save iteration--99000.save

set output-file iteration--99500.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--99500.expr
inspect-sentences
set output-file iteration--99500.term
inspect-terms
set output-file stdout
save iteration--99500.save

set output-file iteration--100000.meta
train-network iterations 500
show cpu-time-total epochs seed
set output-file iteration--100000.expr
inspect-sentences
set output-file iteration--100000.term
inspect-terms
set output-file stdout
save iteration--100000.save

set quiet 0
