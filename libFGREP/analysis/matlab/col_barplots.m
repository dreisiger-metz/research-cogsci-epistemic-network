function r = col_barplots(filename)
%   Generates a series of barplots that show the distributed representations,
%   tailored for the output of the colour training set
%   R = COL_BARPLOTS(filename) reads an m-by-n matrix of n-dimensional 
%           distributed representations from the specified file, and plots
%           their patterns of activation.
X = importdata(filename, ' ', 0);
subplot(1,1,1);

% just plot the colours 
subplot(7,1,1), bar(X.data(8,:));  % red
xlabel(X.textdata(8));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);

subplot(7,1,2), bar(X.data(7,:));  % orange
xlabel(X.textdata(7));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);

subplot(7,1,3), bar(X.data(9,:));  % yellow
xlabel(X.textdata(9));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);

subplot(7,1,4), bar(X.data(5,:));  % green
xlabel(X.textdata(5));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);

subplot(7,1,5), bar(X.data(4,:));  % cyan
xlabel(X.textdata(4));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);

subplot(7,1,6), bar(X.data(3,:));  % blue
xlabel(X.textdata(3));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);

subplot(7,1,7), bar(X.data(6,:));  % blue
xlabel(X.textdata(6));
axis([0 (max(size(X.data(1,:))) + 1) 0 1.0]);


r = max(size(X.data(1,:)));
