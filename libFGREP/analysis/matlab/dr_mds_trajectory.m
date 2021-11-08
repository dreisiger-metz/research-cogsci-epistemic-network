function dr_mds_trajectory(filename, first, last)
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
points = errcol(1) / 8 - 1;
errcol = errcol(2);

D = pdist(X.data(first:last,1:errcol - 1), 'euclidean');
[Y, e] = cmdscale(D);

% colour--blue
offset = 1;
subplot(1,1,1), plot(Y(offset:offset + points, 1), Y(offset:offset + points, 2), 'k-');
hold on;
plot(Y(offset + points,1), Y(offset + points,2), 'k*');
offset = offset + points + 1;
% colour--green
plot(Y(offset:offset + points,1), Y(offset:offset + points,2), 'k--');
plot(Y(offset + points,1), Y(offset + points,2), 'ko');
offset = offset + points + 1;

% height--10cm
plot(Y(offset:offset + points,1), Y(offset:offset + points,2), 'r-');
plot(Y(offset + points,1), Y(offset + points,2), 'r*');
offset = offset + points + 1;
% height--20cm
plot(Y(offset:offset + points,1), Y(offset:offset + points,2), 'r--');
plot(Y(offset + points,1), Y(offset + points,2), 'ro');
offset = offset + points + 1;

% width--10cm
plot(Y(offset:offset + points,1), Y(offset:offset + points,2), 'g-');
plot(Y(offset + points,1), Y(offset + points,2), 'g*');
offset = offset + points + 1;
% width--20cm
plot(Y(offset:offset + points,1), Y(offset:offset + points,2), 'g--');
plot(Y(offset + points,1), Y(offset + points,2), 'go');
offset = offset + points + 1;

% speed--slow
plot(Y(offset:offset + points,1), Y(offset:offset + points,2), 'b-');
plot(Y(offset + points,1), Y(offset + points,2), 'b*');
offset = offset + points + 1;
% speed--medium
plot(Y(offset:offset + points,1), Y(offset:offset + points,2), 'b--');
plot(Y(offset + points,1), Y(offset + points,2), 'bo');
offset = offset + points + 1;
hold off;
