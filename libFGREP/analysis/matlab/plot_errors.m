function [means stds terms epochs drSizes] = plot_errors(means, stds, epochs, drsizes)

% [means] and [stds] are ordered by (term, epochs, hidden-layer-size)
%drlegends = sprintf('%d', drsizes(1));
%for i = 2:max(size(drsizes))
%  drlegends(i) = sprintf('%d', drsizes(i));
%end

% Plot the average per-term error (itself averaged over all of the terms) as
% a function of epochs and hidden-layer sizes
loglog(epochs, squeeze(mean(means), 1);
axis([min(epochs) max(epochs) 1e-6 0.1]);
ylabel('average term errors');
xlabel('epochs');


% Plot the average per-category errors as a function of epochs and hidden layer
% sizes
