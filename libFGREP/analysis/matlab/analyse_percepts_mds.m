function analyse_percepts_mds(filename)
%   Steps through the colour, motion, size and speed symbols generated by
%   the percept-reduced.com FGREP Console script
%   ANALYSE_PERCEPTS_MDS(filename) reads an m-by-n matrix of 
%       n-dimensional distributed representations from the specified
%       file, and for each of the four sets of terms, performs a multi-
%       dimensional scaling on them, and plots their resulting location
%       in two-dimensions.
dr_mds(filename, 3, 13)
pause
dr_mds(filename, 15, 19)
pause
dr_mds(filename, 20, 32)
pause
dr_mds(filename, 33, 36)
