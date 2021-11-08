function r = dr_mds(filename, first, last)
%   Plots the distributed representations based upon the results of an MDS
%   R = DR_MDS(filename) reads an m-by-n matrix of n-dimensional distributed
%           representations from the specified file, performs a multi-
%           dimensional scaling on them, and plots their resulting location
%           in two-dimensions.
%   R = DR_MDS(filename, first, last) also generates and plots a multi-
%           dimensionally-scaled transformation of the data in the input
%           file, but only does so for rows [first, last] inclusively.
X = importdata(filename, ' ', 0);
if (nargin ~= 3)
  first = 1;
  last = size(X.data);
  last = last(1);
end

errcol = size(X.data);
errcol = errcol(2);

% now that we're including the errors in the last column, we need to modify
% both of X.data's dimensional bounds
D = pdist(X.data(first:last,1:errcol - 1), 'euclidean');
%D = pdist(X.data(first:last,:), 'euclidean');
%D = pdist(X.data(first:last,:), 'seuclidean');
[Y, e] = cmdscale(D);

subplot(1,1,1), plot(Y(:,1), Y(:,2), '.');
text(Y(:,1)+1e-9, Y(:,2), X.textdata(first:last));

i = 1;
while ((i <= max(size(e))) && (e(i) / max(abs(e)) > 0.05))
  i = i + 1;
end

r = i - 1;
