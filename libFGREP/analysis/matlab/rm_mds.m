function r = rm_mds(filename)
%   Plots the distributed representations based upon the results of an MDS;
%   this has been tailored for the training set of rogers and mcclelland
%   R = RM_MDS(filename) reads an m-by-n matrix of n-dimensional distributed
%           representations from the specified file, performs a multi-
%           dimensional scaling on them, and plots their resulting location
%           in two-dimensions.
X = importdata(filename, ' ', 0);
D = pdist(X.data(1:8,:), 'euclidean');
[Y, e] = cmdscale(D);

%e/max(abs(e))   % print the normalised eigen-values
subplot(1,1,1), plot(Y(:,1), Y(:,2), '.');
text(Y(:,1)+0.002, Y(:,2), X.textdata(1:8));

i = 1;
while ((i <= max(size(e))) && (e(i) / max(abs(e)) > 0.05))
  i = i + 1;
end
%fprintf('data is roughly %d-dimensional', i - 1);

r = i - 1;
