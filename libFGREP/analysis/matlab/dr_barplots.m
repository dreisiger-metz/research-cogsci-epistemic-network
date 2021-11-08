function r = dr_barplots(filename, first, last)
%   Generates a series of barplots that show the distributed representations
%   R = DR_BARPLOTS(filename) reads an m-by-n matrix of n-dimensional 
%           distributed representations from the specified file, and plots
%           their patterns of activation.
%   R = DR_BARPLOTS(filename, first, last) also plots the distributed 
%           representations in the file, but only does so for rows [first,
%           last] inclusively.
X = importdata(filename, ' ', 0);
if (nargin ~= 3)
  first = 1;
  last = max(size(X.data(:,1)));
end

subplot(1,1,1);
dataSize = last - first + 1;
xmax = floor(sqrt(dataSize)) + 1;
ymax = ceil(dataSize / xmax);

for i = first:last
  subplot(ymax, xmax, i - first + 1), bar(X.data(i,:));
  xlabel(X.textdata(i));
  axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);
end

r = dataSize;
