function T = dr_hclust(filename, cutoff)
%   Plots the distributed representations based upon the results of an MDS
%   R = DR_MDS(filename) reads an m-by-n matrix of n-dimensional distributed
%           representations from the specified file, performs a multi-
%           dimensional scaling on them, and plots their resulting location
%           in two-dimensions.
%   R = DR_MDS(filename, first, last) also generates and plots a multi-
%           dimensionally-scaled transformation of the data in the input
%           file, but only does so for rows [first, last] inclusively.
X = importdata(filename, ' ', 0);
errcol = size(X.data);
errcol = errcol(2);

D = pdist(X.data(:,1:errcol - 1), 'euclidean');
Z = linkage(D, 'average');
T = cluster(Z, 'cutoff', cutoff);
