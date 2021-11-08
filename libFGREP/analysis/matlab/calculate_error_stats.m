function [means stds termLabels epochLabels] = calculate_error_stats(pathbasename, filebasename, numberOfTrials, numberOfSteps)

if (nargin < 3)
  numberOfTrials = 25;
end
if (nargin < 4)
  numberOfSteps = 200;
end

epochsPerStep = 500;

% Load trial zero, epoch zero, extract the terms' labels and pre-allocate [results].
% Note that, for now at least, we will ignore the first two rows as they correspond
% to the terms 'DontCare' and 'NULL', both of whose errors are NaNs.
filename = sprintf('%s--%d/%s--%d.term', pathbasename, 0, filebasename, 0);
datum = importdata(filename, ' ', 0);
datumSize = size(datum.data);
termLabels  = datum.textdata(3:datumSize(1));
epochLabels(1) = 0;
results = zeros(datumSize(1) - 2, datumSize(2), numberOfSteps + 1, numberOfTrials);
results(:,:,1,1) = datum.data(3:datumSize(1), :);

% Load the remaining epochs' data and place them in [results]
for trial = 1:numberOfTrials
  for step = 0:numberOfSteps
    epoch = step * epochsPerStep;
    epochLabels(step + 1) = epoch;
    filename = sprintf('%s--%d/%s--%d.term', pathbasename, trial - 1, filebasename, epoch);
    datum = importdata(filename, ' ', 0);
    results(:,:, step + 1, trial) = datum.data(3:datumSize(1), :);
  end
end


% Now calculate the mean and standard deviations of the errors for each term
means = squeeze(mean(results(:,datumSize(2),:,:), 4));
stds  = squeeze( std(results(:,datumSize(2),:,:), 0, 4));

%whos
%errorbarloglog(epochLabels, means, stds);
%ploterr(epochLabels, means, [], stds, 'logxy')
%subplot(1,1,1), plot(Y(:,1), Y(:,2), '.');
%text(Y(:,1)+1e-9, Y(:,2), X.textdata(first:last));
