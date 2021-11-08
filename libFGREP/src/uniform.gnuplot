bin_width = 0.01
bin_number(x) = floor(x/bin_width)
rounded(x) = bin_width * ( bin_number(x) + 0.5 )
plot 'uniform.dat' u (rounded($1)):(1.0) t 'data' smooth frequency w histeps
