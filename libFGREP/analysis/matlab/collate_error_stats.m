function [means stds terms epochs drSizes] = collate_error_stats(minDRSize, maxDRSize)

if (nargin < 2)
  maxDRSize = minDRSize;
end
numberOfDRSizes = maxDRSize - minDRSize + 1;


% Create the [drSizes] vector
for i = 1:numberOfDRSizes
  drSizes(i) = minDRSize + i - 1;
end


% Calculate the first batch of error statistics so we can correctly pre-allocate
% the return arrays
pathbase = sprintf('%d', minDRSize);
[tmpMeans, tmpStds, terms, epochs] = calculate_error_stats(pathbase, 'iteration');
tmpMeanSize = size(tmpMeans);
means = zeros(tmpMeanSize(1), tmpMeanSize(2), numberOfDRSizes);
stds  = zeros(tmpMeanSize(1), tmpMeanSize(2), numberOfDRSizes);
means(:,:,1) = tmpMeans;
stds(:,:,1) = tmpStds;

for i = 2:numberOfDRSizes
  pathbase = sprintf('%d', i + minDRSize - 1);
  [tmpMeans, tmpStds, terms, epochs] = calculate_error_stats(pathbase, 'iteration');
  means(:,:,i) = tmpMeans;
  stds(:,:,i) = tmpStds;
end
